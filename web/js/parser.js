(function (ns) {
  "use strict";

  const utils = ns.utils;
  const doc = ns.doc;
  const {
    ProgramModel,
    SourceRef,
    AbapParameter,
    AbapDataDeclaration,
    AbapWrite,
    AbapAssignment,
    AbapIfStatement,
    AbapMessageStatement,
    AbapItabOperation,
    AbapCallEdge,
  } = ns.model;

  function defaultParserConfig() {
    const IDENT = String.raw`[A-Za-z_][A-Za-z0-9_\/]*`;
    const IDENT_PATH = String.raw`${IDENT}(?:(?:[-~]|->|=>)[A-Za-z0-9_\/]+)*`;

    return {
      version: 1,
      routineBlocks: [
        {
          kind: "FORM",
          start: /^FORM\b/i,
          end: /^ENDFORM\b/i,
          endRequiresPeriod: true,
          header: {
            name: new RegExp(String.raw`^FORM\s+(${IDENT})\b\s*(.*)$`, "i"),
            clauseOrder: ["TABLES", "USING", "CHANGING", "RAISING"],
            paramIgnoreTokens: ["OPTIONAL", "DEFAULT", "TYPE", "LIKE"],
          },
        },
      ],
      events: {
        exact: ["INITIALIZATION", "START-OF-SELECTION", "END-OF-SELECTION", "AT LINE-SELECTION", "AT USER-COMMAND"],
        prefixes: ["AT SELECTION-SCREEN", "TOP-OF-PAGE", "END-OF-PAGE"],
      },
      statements: {
        calls: [
          {
            kind: "PERFORM",
            pattern: new RegExp(String.raw`^PERFORM\s+(${IDENT})\b\s*(.*)$`, "i"),
            calleeKind: "FORM",
            clauseOrder: ["TABLES", "USING", "CHANGING"],
          },
        ],
        declarations: {
          globalKinds: ["DATA", "CONSTANTS", "PARAMETERS"],
          localKinds: ["DATA", "CONSTANTS"],
          ignorePatterns: {
            DATA: [/^DATA\(/i, /\bBEGIN\s+OF\b/i, /\bEND\s+OF\b/i],
            CONSTANTS: [/\bBEGIN\s+OF\b/i, /\bEND\s+OF\b/i],
          },
        },
        writes: {
          rules: [
            { regex: new RegExp(String.raw`^(${IDENT_PATH})\s*=\s*`) },
            { regex: new RegExp(String.raw`^CLEAR\s+(${IDENT_PATH})\b`, "i") },
            { whenStartsWith: "APPEND", regex: new RegExp(String.raw`\bTO\b\s+(${IDENT_PATH})\b`, "i") },
            { whenStartsWith: "CONCATENATE", regex: new RegExp(String.raw`\bINTO\b\s+(${IDENT_PATH})\b`, "i") },
          ],
        },
        assignments: {
          rules: [{ regex: new RegExp(String.raw`^(${IDENT_PATH})\s*=\s*(.+)$`) }],
        },
        conditionals: {
          rules: [
            { kind: "IF", regex: /^IF\s+(.+)$/i },
            { kind: "ELSEIF", regex: /^ELSEIF\s+(.+)$/i },
          ],
        },
        messages: { enabled: true },
        itabOps: { enabled: true },
      },
    };
  }

  function asRegex(value, flags) {
    if (!value) return null;
    if (value instanceof RegExp) return value;
    if (typeof value === "string") return new RegExp(value, flags || "i");
    return null;
  }

  function regexTest(re, text) {
    if (!re) return false;
    re.lastIndex = 0;
    return re.test(text);
  }

  function regexExec(re, text) {
    if (!re) return null;
    re.lastIndex = 0;
    return re.exec(text);
  }

  function getParserConfig() {
    const base = defaultParserConfig();
    const user = ns.parserConfig;
    if (!user || typeof user !== "object") return base;

    const statements = user.statements && typeof user.statements === "object" ? user.statements : {};
    const decls = statements.declarations && typeof statements.declarations === "object" ? statements.declarations : {};
    const writes = statements.writes && typeof statements.writes === "object" ? statements.writes : {};
    const assignments = statements.assignments && typeof statements.assignments === "object" ? statements.assignments : {};
    const conditionals = statements.conditionals && typeof statements.conditionals === "object" ? statements.conditionals : {};

    return {
      ...base,
      ...user,
      routineBlocks: Array.isArray(user.routineBlocks) ? user.routineBlocks : base.routineBlocks,
      events: { ...base.events, ...(user.events || {}) },
      statements: {
        ...base.statements,
        ...statements,
        calls: Array.isArray(statements.calls) ? statements.calls : base.statements.calls,
        declarations: {
          ...base.statements.declarations,
          ...decls,
          ignorePatterns: { ...base.statements.declarations.ignorePatterns, ...(decls.ignorePatterns || {}) },
        },
        writes: {
          ...base.statements.writes,
          ...writes,
          rules: Array.isArray(writes.rules) ? writes.rules : base.statements.writes.rules,
        },
        assignments: {
          ...base.statements.assignments,
          ...assignments,
          rules: Array.isArray(assignments.rules) ? assignments.rules : base.statements.assignments.rules,
        },
        conditionals: {
          ...base.statements.conditionals,
          ...conditionals,
          rules: Array.isArray(conditionals.rules) ? conditionals.rules : base.statements.conditionals.rules,
        },
      },
    };
  }

  function findRoutineBlocks(lines, cfg) {
    const defs = Array.isArray(cfg?.routineBlocks) ? cfg.routineBlocks : [];
    const blocks = [];
    let i = 0;

    while (i < lines.length) {
      const raw = lines[i] || "";
      if (utils.isFullLineComment(raw)) {
        i++;
        continue;
      }

      const code = utils.stripInlineComment(raw).trim();
      if (!code) {
        i++;
        continue;
      }

      let matchedDef = null;
      for (const def of defs) {
        const startRe = asRegex(def?.start);
        if (startRe && regexTest(startRe, code)) {
          matchedDef = def;
          break;
        }
      }

      if (!matchedDef) {
        i++;
        continue;
      }

      const startLine = i + 1;
      const endRe = asRegex(matchedDef.end);
      const endRequiresPeriod = matchedDef.endRequiresPeriod !== false;

      let j = i;
      while (j < lines.length) {
        const raw2 = lines[j] || "";
        if (utils.isFullLineComment(raw2)) {
          j++;
          continue;
        }
        const code2 = utils.stripInlineComment(raw2).trim();
        if (endRe && regexTest(endRe, code2) && (!endRequiresPeriod || code2.includes("."))) break;
        j++;
      }

      const endLine = Math.min(lines.length, j + 1);
      blocks.push({
        kind: String(matchedDef.kind || "ROUTINE").trim().toUpperCase(),
        startLine,
        endLine,
        def: matchedDef,
      });
      i = endLine;
    }

    return blocks;
  }

  function buildLineMask(lines, blocks) {
    const mask = new Array(lines.length).fill(false);
    for (const b of blocks) {
      for (let ln = b.startLine; ln <= b.endLine; ln++) mask[ln - 1] = true;
    }
    return mask;
  }

  function parseRoutineHeader(blockDef, stmtText, paramDescriptions, headerSourceRef) {
    const statement = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const header = blockDef?.header || {};
    const nameRe = asRegex(header.name);
    const m = regexExec(nameRe, statement);
    if (!m) return null;

    const name = m[1];
    const rest = m[2] || "";

    const clauseOrder = Array.isArray(header.clauseOrder) ? header.clauseOrder : [];
    if (clauseOrder.length === 0) return { name, params: [] };

    const u = rest.toUpperCase();
    const indices = clauseOrder
      .map((k) => ({ k, idx: u.search(new RegExp(`\\b${k}\\b`)) }))
      .filter((x) => x.idx >= 0)
      .sort((a, b) => a.idx - b.idx);

    const clauses = {};
    for (let i = 0; i < indices.length; i++) {
      const cur = indices[i];
      const next = indices[i + 1];
      const start = cur.idx;
      const end = next ? next.idx : rest.length;
      clauses[cur.k] = rest.slice(start + cur.k.length, end).trim();
      clauses[cur.k] = clauses[cur.k].replace(/\s+/g, " ").trim();
      if (clauses[cur.k].startsWith(cur.k)) clauses[cur.k] = clauses[cur.k].slice(cur.k.length).trim();
    }

    const ignore = new Set((header.paramIgnoreTokens || []).map((t) => String(t || "").trim().toUpperCase()).filter(Boolean));

    function parseParams(kind, text) {
      const out = [];
      if (!text) return out;

      if (kind === "RAISING") {
        const tokens = text.split(/\s+/).map(utils.cleanIdentifierToken).filter(Boolean);
        for (const t of tokens) {
          const desc = paramDescriptions[String(t).toLowerCase()] || "";
          out.push(new AbapParameter(kind, t, "", desc, headerSourceRef));
        }
        return out;
      }

      const tokens = text.split(/\s+/).filter(Boolean);
      let i = 0;
      while (i < tokens.length) {
        const raw0 = String(tokens[i] || "").trim();
        const rawUpper = utils.cleanIdentifierToken(raw0).toUpperCase();
        if (ignore.has(rawUpper)) {
          i++;
          continue;
        }

        const token = utils.cleanIdentifierToken(utils.unwrapValueToken(raw0));
        if (!utils.isIdentifier(token)) {
          i++;
          continue;
        }

        const pName = token;
        let dataType = "";
        if (i + 2 < tokens.length && /^(TYPE|LIKE)$/i.test(tokens[i + 1])) {
          dataType = utils.cleanIdentifierToken(tokens[i + 2]);
          i += 3;
        } else {
          i += 1;
        }

        const desc = paramDescriptions[String(pName).toLowerCase()] || "";
        out.push(new AbapParameter(kind, pName, dataType, desc, headerSourceRef));
      }
      return out;
    }

    const params = [];
    for (const clause of clauseOrder) {
      const k = String(clause || "").toUpperCase();
      params.push(...parseParams(k, clauses[k]));
    }

    return { name, params };
  }

  function extractDeclInlineDescriptionsByVar(lines, startLine, endLine, declKind) {
    const byLine = doc.extractInlineCommentsByLine ? doc.extractInlineCommentsByLine(lines, startLine, endLine) : {};
    const byVar = {};
    const kindUpper = String(declKind || "").trim().toUpperCase();

    const from = Math.max(1, startLine || 1);
    const to = Math.min(lines.length, endLine || from);

    for (let ln = from; ln <= to; ln++) {
      const c = String(byLine?.[ln] || "").trim();
      if (!c) continue;

      const raw = String(lines[ln - 1] || "");
      if (utils.isFullLineComment(raw)) continue;

      let code = utils.stripInlineComment(raw).trim();
      if (!code) continue;

      const dk = String(declKind || "").trim();
      if (dk) {
        const re = new RegExp(`^${dk}\\b`, "i");
        if (re.test(code)) {
          code = code.replace(re, "").trim();
          if (code.startsWith(":")) code = code.slice(1).trim();
        }
      }

      code = code.replace(/^[,]+/, "").trim();
      if (!code) continue;

      let firstToken = "";
      if (kindUpper === "PARAMETERS") {
        const m = /^([A-Za-z_][A-Za-z0-9_\/]*)/.exec(code);
        firstToken = m ? m[1] : "";
      } else {
        firstToken = utils.cleanIdentifierToken(code.split(/\s+/)[0] || "");
      }
      if (!utils.isIdentifier(firstToken)) continue;

      const key = String(firstToken).toLowerCase();
      if (!(key in byVar)) byVar[key] = c;
    }

    return byVar;
  }

  function parseDataOrConstants(stmt, declKind, descriptionsByVar, leadingDescription, sourceRef, options) {
    const s = utils.normalizeSpaces(utils.stripTrailingPeriod(stmt));
    const ignorePatterns = Array.isArray(options?.ignorePatterns) ? options.ignorePatterns : [];
    const kindUpper = String(declKind || "").trim().toUpperCase();
    for (const p of ignorePatterns) {
      const re = asRegex(p);
      if (re && regexTest(re, s)) return [];
    }

    let body = s.replace(new RegExp(`^${declKind}\\b`, "i"), "").trim();
    if (body.startsWith(":")) body = body.slice(1).trim();

    const parts = utils.splitByCommaOutsideQuotes(body).map((p) => p.trim()).filter(Boolean);
    const out = [];

    for (const p of parts) {
      let variableName = "";
      if (kindUpper === "PARAMETERS") {
        const m = /^([A-Za-z_][A-Za-z0-9_\/]*)/.exec(p);
        variableName = m ? m[1] : "";
      } else {
        const firstToken = utils.cleanIdentifierToken(p.split(/\s+/)[0] || "");
        variableName = firstToken;
      }
      if (!utils.isIdentifier(variableName)) continue;

      let dataType = "";
      if (kindUpper === "PARAMETERS") {
        const typeMatch = /\bTYPE\b\s+([^\s]+)/i.exec(p);
        dataType = typeMatch ? utils.cleanIdentifierToken(typeMatch[1]) : "";
        if (!dataType) {
          const likeMatch = /\bLIKE\b\s+([^\s]+)/i.exec(p);
          dataType = likeMatch ? utils.cleanIdentifierToken(likeMatch[1]) : "";
        }

        let len = "";
        const parenLen = new RegExp(`^${variableName}\\((\\d+)\\)`, "i").exec(p);
        if (parenLen && parenLen[1]) len = String(parenLen[1]);
        if (!len) {
          const lengthMatch = /\bLENGTH\b\s+(\d+)\b/i.exec(p);
          if (lengthMatch && lengthMatch[1]) len = String(lengthMatch[1]);
        }

        if (len) {
          if (dataType) dataType = `${dataType} LENGTH ${len}`;
          else dataType = `c LENGTH ${len}`;
        }
      } else {
        const typeMatch = /\bTYPE\b\s+([^\s]+)/i.exec(p);
        dataType = typeMatch ? utils.cleanIdentifierToken(typeMatch[1]) : "";
      }

      let value = "";
      if (kindUpper === "PARAMETERS") {
        const defaultMatch = /\bDEFAULT\b\s+(.+)$/i.exec(p);
        value = defaultMatch ? defaultMatch[1].trim() : "";
      } else {
        const valueMatch = /\bVALUE\b\s+(.+)$/i.exec(p);
        value = valueMatch ? valueMatch[1].trim() : "";
      }

      const inlineDesc = String(descriptionsByVar?.[String(variableName).toLowerCase()] || "").trim();
      const desc = inlineDesc || (out.length === 0 ? String(leadingDescription || "").trim() : "");

      out.push(new AbapDataDeclaration(declKind, variableName, dataType, value, desc, sourceRef));
    }

    return out;
  }

  function parseBeginOfDeclarationsFromStatement(statement, declKind, originalLines) {
    const out = [];
    const stack = [];
    let removedKindPrefix = false;

    const from = Math.max(1, Number(statement?.startLine || 1));
    const to = Math.min(originalLines.length, Number(statement?.endLine || from));

    for (let ln = from; ln <= to; ln++) {
      const raw = String(originalLines[ln - 1] || "");
      if (utils.isFullLineComment(raw)) continue;

      const parts = utils.splitInlineComment(raw);
      const inlineComment = String(parts.comment || "").trim();
      let codeLine = String(parts.code || "").trim();
      if (!codeLine) continue;

      if (!removedKindPrefix) {
        const re = new RegExp(`^${String(declKind || "").trim()}\\b`, "i");
        if (re.test(codeLine)) {
          codeLine = codeLine.replace(re, "").trim();
          if (codeLine.startsWith(":")) codeLine = codeLine.slice(1).trim();
        }
        removedKindPrefix = true;
      }

      const segments = utils.splitByCommaOutsideQuotes(codeLine);
      const parsed = [];

      for (let idx = 0; idx < segments.length; idx++) {
        let seg = String(segments[idx] || "").trim();
        if (!seg) continue;
        seg = seg.replace(/^[,]+/, "").trim();
        seg = seg.replace(/\.\s*$/, "").trim();
        if (!seg) continue;

        const normalized = utils.normalizeSpaces(seg);
        let m = /^BEGIN OF\s+([A-Za-z_][A-Za-z0-9_\/]*)\b/i.exec(normalized);
        if (m) {
          parsed.push({ kind: "begin", name: m[1], normalized, idx });
          continue;
        }

        m = /^END OF\s+([A-Za-z_][A-Za-z0-9_\/]*)\b/i.exec(normalized);
        if (m) {
          parsed.push({ kind: "end", name: m[1], normalized, idx });
          continue;
        }

        if (/^INCLUDE\b/i.test(normalized)) continue;

        const first = utils.cleanIdentifierToken(normalized.split(/\s+/)[0] || "");
        if (!utils.isIdentifier(first)) continue;
        if (/^(BEGIN|END|OF)$/i.test(first)) continue;

        const typeMatch = /\bTYPE\b\s+([^\s]+)/i.exec(normalized);
        let dataType = typeMatch ? utils.cleanIdentifierToken(typeMatch[1]) : "";
        if (!dataType) {
          const likeMatch = /\bLIKE\b\s+([^\s]+)/i.exec(normalized);
          dataType = likeMatch ? utils.cleanIdentifierToken(likeMatch[1]) : "";
        }

        const valueMatch = /\bVALUE\b\s+(.+)$/i.exec(normalized);
        const value = valueMatch ? String(valueMatch[1] || "").trim() : "";

        parsed.push({ kind: "field", name: first, dataType, value, normalized, idx });
      }

      if (inlineComment) {
        for (let i = parsed.length - 1; i >= 0; i--) {
          if (parsed[i].kind === "begin" || parsed[i].kind === "field") {
            parsed[i].inlineDesc = inlineComment;
            break;
          }
        }
      }

      for (const seg of parsed) {
        if (seg.kind === "begin") {
          const parent = stack.length ? stack[stack.length - 1] : null;
          const fullName = parent ? `${parent.fullName}-${seg.name}` : seg.name;
          const leading = !seg.inlineDesc ? doc.extractLeadingComment(originalLines, ln).description : "";
          const desc = String(seg.inlineDesc || leading || "").trim();
          out.push(new AbapDataDeclaration(declKind, fullName, "", "", desc, new SourceRef(ln, ln)));
          stack.push({ name: seg.name, fullName });
          continue;
        }

        if (seg.kind === "end") {
          const target = String(seg.name || "").toLowerCase();
          if (stack.length && String(stack[stack.length - 1].name || "").toLowerCase() === target) {
            stack.pop();
            continue;
          }

          let matchIdx = -1;
          for (let i = stack.length - 1; i >= 0; i--) {
            if (String(stack[i].name || "").toLowerCase() === target) {
              matchIdx = i;
              break;
            }
          }
          if (matchIdx >= 0) stack.splice(matchIdx);
          else if (stack.length) stack.pop();
          continue;
        }

        if (seg.kind === "field") {
          const parent = stack.length ? stack[stack.length - 1] : null;
          const fullName = parent ? `${parent.fullName}-${seg.name}` : seg.name;
          const leading = !seg.inlineDesc ? doc.extractLeadingComment(originalLines, ln).description : "";
          const desc = String(seg.inlineDesc || leading || "").trim();
          out.push(new AbapDataDeclaration(declKind, fullName, seg.dataType || "", seg.value || "", desc, new SourceRef(ln, ln)));
        }
      }
    }

    return out;
  }

  function makeTypeDefKey(scopeKey, typeName) {
    const sk = String(scopeKey || "").trim();
    const tn = String(typeName || "")
      .trim()
      .toLowerCase();
    if (!sk || !tn) return "";
    return `${sk}|${tn}`;
  }

  function parseTypesBeginOfFromStatement(statement, scopeKey, originalLines) {
    const stack = [];
    let removedKindPrefix = false;

    const from = Math.max(1, Number(statement?.startLine || 1));
    const to = Math.min(originalLines.length, Number(statement?.endLine || from));

    let typeName = "";
    let typeDesc = "";
    const fields = new Map();

    for (let ln = from; ln <= to; ln++) {
      const raw = String(originalLines[ln - 1] || "");
      if (utils.isFullLineComment(raw)) continue;

      const parts = utils.splitInlineComment(raw);
      const inlineComment = String(parts.comment || "").trim();
      let codeLine = String(parts.code || "").trim();
      if (!codeLine) continue;

      if (!removedKindPrefix) {
        const re = /^TYPES\b/i;
        if (re.test(codeLine)) {
          codeLine = codeLine.replace(re, "").trim();
          if (codeLine.startsWith(":")) codeLine = codeLine.slice(1).trim();
        }
        removedKindPrefix = true;
      }

      const segments = utils.splitByCommaOutsideQuotes(codeLine);
      const parsed = [];

      for (let idx = 0; idx < segments.length; idx++) {
        let seg = String(segments[idx] || "").trim();
        if (!seg) continue;
        seg = seg.replace(/^[,]+/, "").trim();
        seg = seg.replace(/\.\s*$/, "").trim();
        if (!seg) continue;

        const normalized = utils.normalizeSpaces(seg);
        let m = /^BEGIN OF\s+([A-Za-z_][A-Za-z0-9_\/]*)\b/i.exec(normalized);
        if (m) {
          parsed.push({ kind: "begin", name: m[1], normalized, idx });
          continue;
        }

        m = /^END OF\s+([A-Za-z_][A-Za-z0-9_\/]*)\b/i.exec(normalized);
        if (m) {
          parsed.push({ kind: "end", name: m[1], normalized, idx });
          continue;
        }

        if (/^INCLUDE\b/i.test(normalized)) continue;

        const first = utils.cleanIdentifierToken(normalized.split(/\s+/)[0] || "");
        if (!utils.isIdentifier(first)) continue;
        if (/^(BEGIN|END|OF)$/i.test(first)) continue;

        const typeMatch = /\bTYPE\b\s+([^\s]+)/i.exec(normalized);
        let dataType = typeMatch ? utils.cleanIdentifierToken(typeMatch[1]) : "";
        if (!dataType) {
          const likeMatch = /\bLIKE\b\s+([^\s]+)/i.exec(normalized);
          dataType = likeMatch ? utils.cleanIdentifierToken(likeMatch[1]) : "";
        }

        parsed.push({ kind: "field", name: first, dataType, normalized, idx });
      }

      if (inlineComment) {
        for (let i = parsed.length - 1; i >= 0; i--) {
          if (parsed[i].kind === "begin" || parsed[i].kind === "field") {
            parsed[i].inlineDesc = inlineComment;
            break;
          }
        }
      }

      for (const seg of parsed) {
        if (seg.kind === "begin") {
          if (!typeName) {
            typeName = seg.name;
            const leading = !seg.inlineDesc ? doc.extractLeadingComment(originalLines, ln).description : "";
            typeDesc = String(seg.inlineDesc || leading || "").trim();
            stack.push({ name: seg.name, path: "" });
            continue;
          }

          const parent = stack.length ? stack[stack.length - 1] : null;
          const parentPath = parent ? String(parent.path || "") : "";
          const path = parentPath ? `${parentPath}-${seg.name}` : seg.name;
          const leading = !seg.inlineDesc ? doc.extractLeadingComment(originalLines, ln).description : "";
          const desc = String(seg.inlineDesc || leading || "").trim();
          fields.set(path.toLowerCase(), new AbapDataDeclaration("TYPES", path, "", "", desc, new SourceRef(ln, ln)));
          stack.push({ name: seg.name, path });
          continue;
        }

        if (seg.kind === "end") {
          const target = String(seg.name || "").toLowerCase();
          if (stack.length && String(stack[stack.length - 1].name || "").toLowerCase() === target) {
            stack.pop();
            continue;
          }

          let matchIdx = -1;
          for (let i = stack.length - 1; i >= 0; i--) {
            if (String(stack[i].name || "").toLowerCase() === target) {
              matchIdx = i;
              break;
            }
          }
          if (matchIdx >= 0) stack.splice(matchIdx);
          else if (stack.length) stack.pop();
          continue;
        }

        if (seg.kind === "field") {
          const parent = stack.length ? stack[stack.length - 1] : null;
          const parentPath = parent ? String(parent.path || "") : "";
          const path = parentPath ? `${parentPath}-${seg.name}` : seg.name;
          const leading = !seg.inlineDesc ? doc.extractLeadingComment(originalLines, ln).description : "";
          const desc = String(seg.inlineDesc || leading || "").trim();
          fields.set(path.toLowerCase(), new AbapDataDeclaration("TYPES", path, seg.dataType || "", "", desc, new SourceRef(ln, ln)));
        }
      }
    }

    if (!typeName) return null;

    return {
      scopeKey: String(scopeKey || "").trim() || "PROGRAM",
      name: typeName,
      description: typeDesc,
      sourceRef: new SourceRef(from, to),
      fields,
    };
  }

  function parseClauseArgs(rest, clauseOrder) {
    const args = { tables: [], using: [], changing: [] };
    const keys = Array.isArray(clauseOrder) ? clauseOrder.map((k) => String(k || "").toUpperCase()) : [];
    if (!rest || keys.length === 0) return args;

    const u = rest.toUpperCase();
    const indices = keys
      .map((k) => ({ k, idx: u.search(new RegExp(`\\b${k}\\b`)) }))
      .filter((x) => x.idx >= 0)
      .sort((a, b) => a.idx - b.idx);

    const clauses = {};
    for (let i = 0; i < indices.length; i++) {
      const cur = indices[i];
      const next = indices[i + 1];
      const start = cur.idx;
      const end = next ? next.idx : rest.length;
      clauses[cur.k] = rest.slice(start + cur.k.length, end).trim();
      clauses[cur.k] = clauses[cur.k].replace(/\s+/g, " ").trim();
      if (clauses[cur.k].startsWith(cur.k)) clauses[cur.k] = clauses[cur.k].slice(cur.k.length).trim();
    }

    function tokensOf(text) {
      if (!text) return [];
      return text
        .split(/\s+/)
        .map((t) => utils.cleanIdentifierToken(t))
        .filter(Boolean);
    }

    if (clauses.TABLES != null) args.tables = tokensOf(clauses.TABLES);
    if (clauses.USING != null) args.using = tokensOf(clauses.USING);
    if (clauses.CHANGING != null) args.changing = tokensOf(clauses.CHANGING);
    return args;
  }

  function parseCallByRule(stmtText, rule) {
    const statement = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const pattern = asRegex(rule?.pattern);
    const m = regexExec(pattern, statement);
    if (!m) return null;

    const target = String(m[1] || "").trim();
    if (!target) return null;

    const rest = String(m[2] || "").trim();
    const args = parseClauseArgs(rest, rule?.clauseOrder);
    return { target, args };
  }

  function parseWriteTargets(stmtText, cfg) {
    const s = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const u = s.toUpperCase();

    const out = [];
    const rules = cfg?.statements?.writes?.rules || [];

    for (const rule of rules) {
      const when = String(rule?.whenStartsWith || "").trim();
      if (when) {
        const w = when.toUpperCase();
        if (!(u === w || u.startsWith(`${w} `))) continue;
      }

      const re = asRegex(rule?.regex);
      const m = regexExec(re, s);
      if (m && m[1]) out.push(m[1]);
    }

    return out;
  }

  function eventNameFromStatement(stmtText, cfg) {
    const t = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const u = t.toUpperCase();

    const exact = Array.isArray(cfg?.events?.exact) ? cfg.events.exact : [];
    for (const e of exact) {
      if (u === String(e || "").trim().toUpperCase()) return t;
    }

    const prefixes = Array.isArray(cfg?.events?.prefixes) ? cfg.events.prefixes : [];
    for (const p of prefixes) {
      const pu = String(p || "").trim().toUpperCase();
      if (pu && u.startsWith(pu)) return t;
    }

    return null;
  }

  function parseRoutineStatement(model, routine, statement, originalLines, cfg) {
    const text = statement.text;
    const normalized = utils.normalizeSpaces(utils.stripTrailingPeriod(text));
    if (!normalized) return;

    const src = new SourceRef(statement.startLine, statement.endLine);

    if (/^TYPES\b/i.test(normalized) && /\bBEGIN\s+OF\b/i.test(normalized)) {
      const scopeKey = routine?.key || "PROGRAM";
      const typeDef = parseTypesBeginOfFromStatement(statement, scopeKey, originalLines);
      if (typeDef && typeDef.name) {
        const key = makeTypeDefKey(typeDef.scopeKey, typeDef.name);
        if (key) model.typeDefs.set(key, typeDef);
      }
      return;
    }

    const callRules = Array.isArray(cfg?.statements?.calls) ? cfg.statements.calls : [];
    for (const rule of callRules) {
      const parsed = parseCallByRule(normalized, rule);
      if (!parsed) continue;

      const calleeKind = String(rule?.calleeKind || "FORM").trim().toUpperCase();
      const callee = model.ensureRoutine(calleeKind, parsed.target);
      const edge = new AbapCallEdge(routine.key, callee.key, parsed.target, parsed.args, src);
      model.addEdge(edge);
      return;
    }

    const declCfg = cfg?.statements?.declarations || {};
    const localKinds = Array.isArray(declCfg.localKinds) ? declCfg.localKinds : [];

    for (const kind of localKinds) {
      const dk = String(kind || "").trim().toUpperCase();
      if (!dk) continue;
      if (!new RegExp(`^${dk}\\b`, "i").test(normalized)) continue;

      if ((dk === "DATA" || dk === "CONSTANTS") && /\bBEGIN\s+OF\b/i.test(normalized)) {
        const decls = parseBeginOfDeclarationsFromStatement(statement, dk, originalLines);
        if (dk === "DATA") routine.localData.push(...decls);
        else if (dk === "CONSTANTS") routine.localConstants.push(...decls);
        return;
      }

      const leadingDoc = doc.extractLeadingComment(originalLines, statement.startLine);
      const inlineDescriptionsByVar = extractDeclInlineDescriptionsByVar(originalLines, statement.startLine, statement.endLine, dk);

      const decls = parseDataOrConstants(normalized, dk, inlineDescriptionsByVar, leadingDoc.description, src, {
        ignorePatterns: declCfg.ignorePatterns?.[dk] || [],
        supportsValue: dk === "CONSTANTS",
      });

      if (dk === "DATA") routine.localData.push(...decls);
      else if (dk === "CONSTANTS") routine.localConstants.push(...decls);
      return;
    }

    const registry = ns.abapObjects?.getRegistry?.() || null;
    if (registry && typeof registry.parseStatementItems === "function") {
      const items = registry.parseStatementItems(model, routine, normalized, src, cfg);
      if (items.length) {
        if (!Array.isArray(routine.statementItems)) routine.statementItems = [];
        for (const it of items) routine.statementItems.push(it);

        for (const it of items) {
          if (it.objectId === "assignment" && it.payload) routine.assignments.push(it.payload);
          if (it.objectId === "if" && it.payload) routine.ifStatements.push(it.payload);
          if (it.objectId === "message" && it.payload) routine.messages.push(it.payload);
          if (it.objectId === "itabOp" && it.payload) routine.itabOps.push(it.payload);
        }

        const shouldContinue = items.some((x) => x && x.continueAfterMatch);
        if (!shouldContinue) return;
      }
    }

    const writes = parseWriteTargets(normalized, cfg);
    for (const v of writes) routine.writes.push(new AbapWrite(v, normalized, src));
  }

  function parseRoutineBlocks(model, lines, blocks, cfg) {
    for (const block of blocks) {
      const statements = utils.collectStatements(lines, block.startLine, block.endLine);
      if (statements.length === 0) continue;

      const header = statements[0];
      const headerRef = new SourceRef(block.startLine, header.endLine);
      const leadingDoc = doc.extractLeadingComment(lines, block.startLine);

      const headerInfo = parseRoutineHeader(block.def, header.text, leadingDoc.paramDescriptions, headerRef);
      if (!headerInfo) continue;

      const routine = model.defineRoutine(block.kind, headerInfo.name, new SourceRef(block.startLine, block.endLine), leadingDoc.description, headerInfo.params);

      for (let i = 1; i < statements.length; i++) {
        const st = statements[i];
        const normalized = utils.normalizeSpaces(utils.stripTrailingPeriod(st.text));
        const endRe = asRegex(block.def?.end);
        if (endRe && regexTest(endRe, normalized)) break;
        parseRoutineStatement(model, routine, st, lines, cfg);
      }
    }
  }

  function parseTopLevel(model, lines, routineMask, cfg) {
    const masked = lines.map((l, idx) => (routineMask[idx] ? "" : l));
    const statements = utils.collectStatements(masked, 1, masked.length);

    let currentEvent = null;
    let lastStmt = null;

    for (const st of statements) {
      lastStmt = st;
      const normalized = utils.normalizeSpaces(utils.stripTrailingPeriod(st.text));
      if (!normalized) continue;

      const ev = eventNameFromStatement(normalized, cfg);
      if (ev) {
        currentEvent = model.ensureEvent(ev, new SourceRef(st.startLine, st.endLine));
        continue;
      }

      if (!currentEvent) {
        const src = new SourceRef(st.startLine, st.endLine);

        if (/^TYPES\b/i.test(normalized) && /\bBEGIN\s+OF\b/i.test(normalized)) {
          const typeDef = parseTypesBeginOfFromStatement(st, "PROGRAM", lines);
          if (typeDef && typeDef.name) {
            const key = makeTypeDefKey(typeDef.scopeKey, typeDef.name);
            if (key) model.typeDefs.set(key, typeDef);
          }
          continue;
        }

        const declCfg = cfg?.statements?.declarations || {};
        const globalKinds = Array.isArray(declCfg.globalKinds) ? declCfg.globalKinds : [];

        for (const kind of globalKinds) {
          const dk = String(kind || "").trim().toUpperCase();
          if (!dk) continue;
          if (!new RegExp(`^${dk}\\b`, "i").test(normalized)) continue;

          if ((dk === "DATA" || dk === "CONSTANTS") && /\bBEGIN\s+OF\b/i.test(normalized)) {
            const decls = parseBeginOfDeclarationsFromStatement(st, dk, lines);
            if (dk === "DATA") model.globalData.push(...decls);
            else if (dk === "CONSTANTS") model.globalConstants.push(...decls);
            break;
          }

          const leadingDoc = doc.extractLeadingComment(lines, st.startLine);
          const inlineDescriptionsByVar = extractDeclInlineDescriptionsByVar(lines, st.startLine, st.endLine, dk);

          const decls = parseDataOrConstants(normalized, dk, inlineDescriptionsByVar, leadingDoc.description, src, {
            ignorePatterns: declCfg.ignorePatterns?.[dk] || [],
            supportsValue: dk === "CONSTANTS",
          });

          if (dk === "DATA") model.globalData.push(...decls);
          else if (dk === "CONSTANTS") model.globalConstants.push(...decls);
          else if (dk === "PARAMETERS") model.globalData.push(...decls);
          break;
        }

        continue;
      }

      parseRoutineStatement(model, currentEvent, st, lines, cfg);
      if (currentEvent.sourceRef) currentEvent.sourceRef.endLine = st.endLine;
    }

    if (currentEvent && currentEvent.sourceRef && lastStmt) {
      currentEvent.sourceRef.endLine = Math.max(currentEvent.sourceRef.endLine || 0, lastStmt.endLine);
    }
  }

  function resolveTypeDef(model, scopeKey, typeName) {
    const sk = String(scopeKey || "").trim() || "PROGRAM";
    const tn = String(typeName || "").trim();
    if (!sk || !tn) return null;

    const localKey = makeTypeDefKey(sk, tn);
    if (localKey && model?.typeDefs?.has?.(localKey)) return model.typeDefs.get(localKey) || null;

    const globalKey = makeTypeDefKey("PROGRAM", tn);
    if (globalKey && model?.typeDefs?.has?.(globalKey)) return model.typeDefs.get(globalKey) || null;

    return null;
  }

  function expandTypedStructDeclsInList(model, declList, scopeKey) {
    const list = Array.isArray(declList) ? declList : [];
    if (!list.length) return;

    const existing = new Set(
      list
        .map((d) => String(d?.variableName || "").toLowerCase())
        .map((s) => s.trim())
        .filter(Boolean),
    );

    const queue = list.slice();
    for (let qi = 0; qi < queue.length; qi++) {
      const decl = queue[qi] || null;
      const typeName = String(decl?.dataType || "").trim();
      if (!typeName) continue;
      if (!/^[A-Za-z_][A-Za-z0-9_\/]*$/.test(typeName)) continue;

      const typeDef = resolveTypeDef(model, scopeKey, typeName);
      if (!typeDef || !typeDef.fields || typeof typeDef.fields.values !== "function") continue;

      for (const field of typeDef.fields.values()) {
        const fieldPath = String(field?.variableName || "").trim();
        if (!fieldPath) continue;

        const baseName = String(decl?.variableName || "").trim();
        if (!baseName) continue;

        const virtualName = `${baseName}-${fieldPath}`;
        const virtualLower = virtualName.toLowerCase();
        if (existing.has(virtualLower)) continue;

        const virtualDecl = new AbapDataDeclaration(
          decl.declKind,
          virtualName,
          String(field?.dataType || "").trim(),
          "",
          String(field?.description || "").trim(),
          field?.sourceRef || null,
        );
        virtualDecl.isVirtual = true;
        virtualDecl.virtualOrigin = {
          kind: "typeField",
          typeScopeKey: String(typeDef.scopeKey || "").trim() || "PROGRAM",
          typeName: String(typeDef.name || "").trim(),
          fieldPath,
        };

        list.push(virtualDecl);
        queue.push(virtualDecl);
        existing.add(virtualLower);
      }
    }
  }

  function expandTypedStructFields(model) {
    if (!model || !model.typeDefs || !model.nodes) return;

    expandTypedStructDeclsInList(model, model.globalData, "PROGRAM");
    expandTypedStructDeclsInList(model, model.globalConstants, "PROGRAM");

    for (const node of model.nodes.values()) {
      const scopeKey = String(node?.key || "").trim();
      if (!scopeKey) continue;
      expandTypedStructDeclsInList(model, node.localData, scopeKey);
      expandTypedStructDeclsInList(model, node.localConstants, scopeKey);
    }
  }

  function parseProgram(text) {
    const normalized = utils.normalizeNewlines(text);
    const lines = utils.splitLines(normalized);
    const offsets = utils.computeLineStartOffsets(normalized);
    const model = new ProgramModel(normalized, lines, offsets);

    const cfg = getParserConfig();
    const routineBlocks = findRoutineBlocks(lines, cfg);
    const routineMask = buildLineMask(lines, routineBlocks);

    parseRoutineBlocks(model, lines, routineBlocks, cfg);
    parseTopLevel(model, lines, routineMask, cfg);
    expandTypedStructFields(model);

    ns.graph.build(model);
    return model;
  }

  ns.parser = { parseProgram };
})(window.AbapFlow);
