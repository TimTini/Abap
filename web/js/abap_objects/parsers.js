(function (ns) {
  "use strict";

  const utils = ns.utils;
  const { AbapAssignment, AbapIfStatement, AbapMessageStatement, AbapItabOperation } = ns.model || {};

  function asRegex(value, flags) {
    if (!value) return null;
    if (value instanceof RegExp) return value;
    if (typeof value === "string") return new RegExp(value, flags || "i");
    return null;
  }

  function regexExec(re, text) {
    if (!re) return null;
    re.lastIndex = 0;
    return re.exec(text);
  }

  function splitTokensOutsideStrings(text) {
    const s = String(text ?? "");
    const tokens = [];
    let buf = "";
    let inString = false;
    let inTemplate = false;

    for (let i = 0; i < s.length; i++) {
      const ch = s[i];

      if (ch === "'" && !inTemplate) {
        buf += ch;
        if (inString) {
          if (i + 1 < s.length && s[i + 1] === "'") {
            buf += s[i + 1];
            i++;
          } else {
            inString = false;
          }
        } else {
          inString = true;
        }
        continue;
      }

      if (!inString && ch === "|") {
        buf += ch;
        inTemplate = !inTemplate;
        continue;
      }

      if (!inString && !inTemplate && /\s/.test(ch)) {
        const t = buf.trim();
        if (t) tokens.push(t);
        buf = "";
        continue;
      }

      buf += ch;
    }

    const last = buf.trim();
    if (last) tokens.push(last);
    return tokens;
  }

  function normalizeStatement(stmtText) {
    return utils.normalizeSpaces(utils.stripTrailingPeriod(String(stmtText ?? "")));
  }

  function parseAssignmentStatement(stmtText, sourceRef, cfg) {
    if (!AbapAssignment) return null;
    const statement = normalizeStatement(stmtText);
    if (!statement) return null;

    const rules = Array.isArray(cfg?.statements?.assignments?.rules) ? cfg.statements.assignments.rules : [];
    for (const rule of rules) {
      const re = asRegex(rule?.regex, rule?.flags);
      const m = regexExec(re, statement);
      if (!m) continue;
      const lhs = String(m[1] || "").trim();
      const rhs = String(m[2] || "").trim();
      if (!lhs || !rhs) continue;
      return new AbapAssignment(lhs, rhs, statement, sourceRef);
    }
    return null;
  }

  function parseConditionalStatement(stmtText, sourceRef, cfg) {
    if (!AbapIfStatement) return null;
    const statement = normalizeStatement(stmtText);
    if (!statement) return null;

    const rules = Array.isArray(cfg?.statements?.conditionals?.rules) ? cfg.statements.conditionals.rules : [];
    for (const rule of rules) {
      const re = asRegex(rule?.regex, rule?.flags);
      const m = regexExec(re, statement);
      if (!m) continue;
      const kind = String(rule?.kind || "").trim().toUpperCase() || "IF";
      const condition = String(m[1] || "").trim();
      if (!condition) continue;
      return new AbapIfStatement(kind, condition, sourceRef);
    }
    return null;
  }

  function parseMessageStatement(stmtText, sourceRef, cfg) {
    if (!AbapMessageStatement) return null;
    const statement = normalizeStatement(stmtText);
    const tokens = splitTokensOutsideStrings(statement);
    if (tokens.length < 2) return null;
    if (String(tokens[0] || "").toUpperCase() !== "MESSAGE") return null;

    const opts = cfg?.statements?.messages;
    if (opts && opts.enabled === false) return null;

    let idx = 1;
    let msgType = "";
    let msgClass = "";
    let msgNo = "";
    let text = "";

    const t1 = String(tokens[idx] || "").trim();
    const t1u = t1.toUpperCase();

    if (t1u === "ID") {
      msgClass = String(tokens[idx + 1] || "").trim();
      idx += 2;

      while (idx < tokens.length) {
        const u = String(tokens[idx] || "").toUpperCase();
        if (u === "WITH" || u === "DISPLAY" || u === "RAISING" || u === "INTO") break;
        if (u === "TYPE" && idx + 1 < tokens.length) {
          msgType = String(tokens[idx + 1] || "").trim();
          idx += 2;
          continue;
        }
        if (u === "NUMBER" && idx + 1 < tokens.length) {
          msgNo = String(tokens[idx + 1] || "").trim();
          idx += 2;
          continue;
        }
        idx++;
      }
    } else {
      const m = /^([A-Za-z])(\d+)\(([^)]+)\)$/.exec(t1);
      if (m) {
        msgType = String(m[1] || "").trim();
        msgNo = String(m[2] || "").trim();
        msgClass = String(m[3] || "").trim();
        idx += 1;
      } else {
        const parts = [];
        while (idx < tokens.length) {
          const u = String(tokens[idx] || "").toUpperCase();
          if (u === "WITH" || u === "DISPLAY" || u === "RAISING" || u === "INTO") break;
          parts.push(tokens[idx]);
          idx++;
        }
        text = parts.join(" ").trim();
      }
    }

    let displayLike = "";
    let into = "";
    let raising = "";
    const withArgs = [];

    while (idx < tokens.length) {
      const u = String(tokens[idx] || "").toUpperCase();

      if (u === "DISPLAY" && String(tokens[idx + 1] || "").toUpperCase() === "LIKE") {
        displayLike = String(tokens[idx + 2] || "").trim();
        idx += 3;
        continue;
      }

      if (u === "WITH") {
        idx += 1;
        while (idx < tokens.length && withArgs.length < 4) {
          const peek = String(tokens[idx] || "").toUpperCase();
          if (peek === "DISPLAY" || peek === "RAISING" || peek === "INTO") break;
          withArgs.push(String(tokens[idx] || "").trim());
          idx++;
        }
        continue;
      }

      if (u === "INTO") {
        into = String(tokens[idx + 1] || "").trim();
        idx += 2;
        continue;
      }

      if (u === "RAISING") {
        raising = String(tokens[idx + 1] || "").trim();
        idx += 2;
        continue;
      }

      idx += 1;
    }

    return new AbapMessageStatement({
      msgType,
      msgClass,
      msgNo,
      text,
      displayLike,
      with: withArgs,
      into,
      raising,
      statement,
      sourceRef,
    });
  }

  function parseItabOperation(stmtText, sourceRef, cfg) {
    if (!AbapItabOperation) return null;
    const statement = normalizeStatement(stmtText);
    const tokens = splitTokensOutsideStrings(statement);
    if (tokens.length < 2) return null;

    const opts = cfg?.statements?.itabOps;
    if (opts && opts.enabled === false) return null;

    const head = String(tokens[0] || "").toUpperCase();

    function joinFrom(startIndex, stopSet) {
      const parts = [];
      let i = startIndex;
      for (; i < tokens.length; i++) {
        const u = String(tokens[i] || "").toUpperCase();
        if (stopSet.has(u)) break;
        parts.push(tokens[i]);
      }
      return { text: parts.join(" ").trim(), next: i };
    }

    if (head === "READ" && String(tokens[1] || "").toUpperCase() === "TABLE") {
      const table = String(tokens[2] || "").trim();
      if (!table) return null;

      let target = "";
      let conditionText = "";
      let conditionKind = "";
      let binarySearch = false;

      for (let i = 3; i < tokens.length; i++) {
        const u = String(tokens[i] || "").toUpperCase();

        if (u === "BINARY" && String(tokens[i + 1] || "").toUpperCase() === "SEARCH") {
          binarySearch = true;
          i += 1;
          continue;
        }

        if (u === "INTO" && !target) {
          target = String(tokens[i + 1] || "").trim();
          continue;
        }

        if (u === "ASSIGNING" && !target) {
          target = String(tokens[i + 1] || "").trim();
          continue;
        }

        if (u === "REFERENCE" && String(tokens[i + 1] || "").toUpperCase() === "INTO" && !target) {
          target = String(tokens[i + 2] || "").trim();
          continue;
        }

        if (u === "INDEX" && !conditionText) {
          conditionKind = "index";
          conditionText = String(tokens[i + 1] || "").trim();
          continue;
        }

        if (u === "WITH" && !conditionText) {
          const next = String(tokens[i + 1] || "").toUpperCase();
          const next2 = String(tokens[i + 2] || "").toUpperCase();
          let start = -1;
          if (next === "KEY") start = i + 2;
          else if (next === "TABLE" && next2 === "KEY") start = i + 3;

          if (start >= 0) {
            const stop = new Set(["BINARY", "INTO", "ASSIGNING", "TRANSPORTING", "REFERENCE", "COMPARING"]);
            const collected = joinFrom(start, stop);
            conditionKind = "key";
            conditionText = collected.text;
          }
          continue;
        }

        if (u === "FROM" && !conditionText) {
          conditionKind = "from";
          conditionText = String(tokens[i + 1] || "").trim();
          continue;
        }
      }

      return new AbapItabOperation("READ", { table, target, conditionText, conditionKind, binarySearch, statement }, sourceRef);
    }

    if (head === "COLLECT") {
      const intoIdx = tokens.findIndex((t) => String(t || "").toUpperCase() === "INTO");
      if (intoIdx < 0 || intoIdx + 1 >= tokens.length) return null;

      const target = tokens.slice(1, intoIdx).join(" ").trim();
      const table = String(tokens[intoIdx + 1] || "").trim();
      if (!table) return null;

      return new AbapItabOperation("COLLECT", { table, target, conditionText: "", conditionKind: "", binarySearch: false, statement }, sourceRef);
    }

    if (head === "MODIFY") {
      let idx = 1;
      if (String(tokens[idx] || "").toUpperCase() === "TABLE") idx++;
      const table = String(tokens[idx] || "").trim();
      if (!table) return null;

      let target = "";
      let conditionText = "";
      let conditionKind = "";

      for (let i = idx + 1; i < tokens.length; i++) {
        const u = String(tokens[i] || "").toUpperCase();
        if (u === "FROM" && !target) {
          const stop = new Set(["TRANSPORTING", "WHERE", "INDEX"]);
          target = joinFrom(i + 1, stop).text;
          continue;
        }
        if (u === "WHERE" && !conditionText) {
          conditionKind = "where";
          conditionText = tokens.slice(i + 1).join(" ").trim();
          break;
        }
        if (u === "INDEX" && !conditionText) {
          conditionKind = "index";
          conditionText = String(tokens[i + 1] || "").trim();
          continue;
        }
      }

      return new AbapItabOperation("MODIFY", { table, target, conditionText, conditionKind, binarySearch: false, statement }, sourceRef);
    }

    if (head === "DELETE") {
      let idx = 1;
      if (String(tokens[idx] || "").toUpperCase() === "TABLE") idx++;
      if (String(tokens[idx] || "").toUpperCase() === "ADJACENT") return null;
      const table = String(tokens[idx] || "").trim();
      if (!table) return null;

      let target = "";
      let conditionText = "";
      let conditionKind = "";

      for (let i = idx + 1; i < tokens.length; i++) {
        const u = String(tokens[i] || "").toUpperCase();
        if (u === "FROM" && !target) {
          const stop = new Set(["WHERE", "INDEX", "USING", "KEY"]);
          target = joinFrom(i + 1, stop).text;
          continue;
        }
        if (u === "WHERE" && !conditionText) {
          conditionKind = "where";
          conditionText = tokens.slice(i + 1).join(" ").trim();
          break;
        }
        if (u === "INDEX" && !conditionText) {
          conditionKind = "index";
          conditionText = String(tokens[i + 1] || "").trim();
          continue;
        }
        if (u === "WITH" && !conditionText) {
          const next = String(tokens[i + 1] || "").toUpperCase();
          const next2 = String(tokens[i + 2] || "").toUpperCase();
          let start = -1;
          if (next === "KEY") start = i + 2;
          else if (next === "TABLE" && next2 === "KEY") start = i + 3;
          if (start >= 0) {
            conditionKind = "key";
            conditionText = tokens.slice(start).join(" ").trim();
            break;
          }
        }
      }

      return new AbapItabOperation("DELETE", { table, target, conditionText, conditionKind, binarySearch: false, statement }, sourceRef);
    }

    return null;
  }

  function parseRegexStatement(stmtText, sourceRef, parseSpec) {
    const statement = normalizeStatement(stmtText);
    if (!statement) return null;

    const re = asRegex(parseSpec?.regex, parseSpec?.flags);
    const m = regexExec(re, statement);
    if (!m) return null;

    const fields = parseSpec?.fields && typeof parseSpec.fields === "object" ? parseSpec.fields : null;
    const out = { statement, sourceRef };
    if (!fields) return out;

    for (const [key, idx] of Object.entries(fields)) {
      const n = Number(idx);
      if (!Number.isFinite(n) || n < 0) continue;
      out[String(key)] = String(m[n] ?? "").trim();
    }
    return out;
  }

  ns.abapObjects = ns.abapObjects || {};
  ns.abapObjects.parsers = {
    splitTokensOutsideStrings,
    normalizeStatement,
    parseAssignmentStatement,
    parseConditionalStatement,
    parseMessageStatement,
    parseItabOperation,
    parseRegexStatement,
  };
})(window.AbapFlow);

