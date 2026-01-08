(function (ns) {
  "use strict";

  const utils = ns.utils;

  function isWordChar(ch) {
    return /[A-Za-z0-9_\/]/.test(String(ch || ""));
  }

  function matchKeywordAt(s, i, keywordUpper) {
    const kw = String(keywordUpper || "").toUpperCase();
    if (!kw) return false;
    const end = i + kw.length;
    if (end > s.length) return false;
    if (s.slice(i, end).toUpperCase() !== kw) return false;

    const prev = i > 0 ? s[i - 1] : "";
    const next = end < s.length ? s[end] : "";
    if (prev && isWordChar(prev)) return false;
    if (next && isWordChar(next)) return false;
    return true;
  }

  function stripOuterParens(text) {
    let s = String(text || "").trim();
    if (!s.startsWith("(") || !s.endsWith(")")) return s;

    function hasWrappingParens(input) {
      const t = String(input || "").trim();
      if (!t.startsWith("(") || !t.endsWith(")")) return false;

      let depth = 0;
      let inString = false;
      let inTemplate = false;

      for (let i = 0; i < t.length; i++) {
        const ch = t[i];

        if (ch === "'" && !inTemplate) {
          if (inString) {
            if (i + 1 < t.length && t[i + 1] === "'") {
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
          inTemplate = !inTemplate;
          continue;
        }

        if (inString || inTemplate) continue;

        if (ch === "(") depth++;
        else if (ch === ")") depth--;

        if (depth === 0 && i < t.length - 1) return false;
      }

      return depth === 0;
    }

    while (hasWrappingParens(s)) {
      s = s.slice(1, -1).trim();
    }

    return s;
  }

  function splitLogicalExpression(expr) {
    const s = String(expr || "").trim();
    const parts = [];
    const ops = [];

    let buf = "";
    let depth = 0;
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

      if (!inString && !inTemplate) {
        if (ch === "(") {
          depth++;
          buf += ch;
          continue;
        }
        if (ch === ")") {
          depth = Math.max(0, depth - 1);
          buf += ch;
          continue;
        }
      }

      if (!inString && !inTemplate && depth === 0) {
        if (matchKeywordAt(s, i, "AND")) {
          const t = buf.trim();
          if (t) parts.push(t);
          ops.push("AND");
          buf = "";
          i += 2;
          continue;
        }
        if (matchKeywordAt(s, i, "OR")) {
          const t = buf.trim();
          if (t) parts.push(t);
          ops.push("OR");
          buf = "";
          i += 1;
          continue;
        }
      }

      buf += ch;
    }

    const last = buf.trim();
    if (last) parts.push(last);
    if (ops.length >= parts.length) ops.length = Math.max(0, parts.length - 1);

    return { parts, ops };
  }

  function parseSimpleCondition(rawTerm) {
    const term = utils.normalizeSpaces(stripOuterParens(rawTerm));
    if (!term) return { item1: "", operator: "", item2: "", raw: "" };

    let m = /^(.+?)\s+IS\s+NOT\s+INITIAL$/i.exec(term);
    if (m) return { item1: m[1].trim(), operator: "IS NOT INITIAL", item2: "", raw: term };

    m = /^(.+?)\s+IS\s+INITIAL$/i.exec(term);
    if (m) return { item1: m[1].trim(), operator: "IS INITIAL", item2: "", raw: term };

    m = /^(.+?)\s*(<=|>=|<>|=|<|>)\s*(.+)$/i.exec(term);
    if (m) return { item1: m[1].trim(), operator: m[2].toUpperCase(), item2: m[3].trim(), raw: term };

    m = /^(.+?)\s+\b(EQ|NE|GT|LT|GE|LE)\b\s+(.+)$/i.exec(term);
    if (m) return { item1: m[1].trim(), operator: m[2].toUpperCase(), item2: m[3].trim(), raw: term };

    m = /^(.+?)\s+\bIN\b\s+(.+)$/i.exec(term);
    if (m) return { item1: m[1].trim(), operator: "IN", item2: m[2].trim(), raw: term };

    return { item1: term, operator: "", item2: "", raw: term };
  }

  function parseIfExpression(expr) {
    const { parts, ops } = splitLogicalExpression(expr);
    const out = [];

    for (let i = 0; i < parts.length; i++) {
      const parsed = parseSimpleCondition(parts[i]);
      out.push({ ...parsed, association: String(ops[i] || "").toUpperCase() });
    }

    return out;
  }

  ns.logic = { splitLogicalExpression, parseSimpleCondition, parseIfExpression };
})(window.AbapFlow);

