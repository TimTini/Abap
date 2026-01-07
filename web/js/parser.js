(function (ns) {
  "use strict";

  const utils = ns.utils;
  const doc = ns.doc;
  const { ProgramModel, SourceRef, AbapParameter, AbapDataDeclaration, AbapWrite, AbapCallEdge } = ns.model;

  function findFormBlocks(lines) {
    const blocks = [];
    let i = 0;
    while (i < lines.length) {
      const raw = lines[i] || "";
      if (utils.isFullLineComment(raw)) {
        i++;
        continue;
      }
      const code = utils.stripInlineComment(raw).trim();
      if (!/^FORM\b/i.test(code)) {
        i++;
        continue;
      }

      const startLine = i + 1;
      let j = i;
      while (j < lines.length) {
        const raw2 = lines[j] || "";
        if (utils.isFullLineComment(raw2)) {
          j++;
          continue;
        }
        const code2 = utils.stripInlineComment(raw2).trim();
        if (/^ENDFORM\b/i.test(code2) && code2.includes(".")) break;
        j++;
      }
      const endLine = Math.min(lines.length, j + 1);
      blocks.push({ startLine, endLine });
      i = endLine;
    }
    return blocks;
  }

  function buildFormLineMask(lines, formBlocks) {
    const mask = new Array(lines.length).fill(false);
    for (const b of formBlocks) {
      for (let ln = b.startLine; ln <= b.endLine; ln++) mask[ln - 1] = true;
    }
    return mask;
  }

  function parseFormHeader(stmtText, paramDescriptions, headerSourceRef) {
    const statement = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const m = /^FORM\s+([A-Za-z_][A-Za-z0-9_\/]*)\b\s*(.*)$/i.exec(statement);
    if (!m) return null;

    const name = m[1];
    const rest = m[2] || "";

    const clauseOrder = ["TABLES", "USING", "CHANGING", "RAISING"];
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
        if (rawUpper === "OPTIONAL" || rawUpper === "DEFAULT" || rawUpper === "TYPE" || rawUpper === "LIKE") {
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
        if (i + 2 < tokens.length && /^TYPE$/i.test(tokens[i + 1])) {
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
    params.push(...parseParams("TABLES", clauses.TABLES));
    params.push(...parseParams("USING", clauses.USING));
    params.push(...parseParams("CHANGING", clauses.CHANGING));
    params.push(...parseParams("RAISING", clauses.RAISING));

    return { name, params };
  }

  function parseDataOrConstants(stmt, declKind, description, sourceRef) {
    const s = utils.normalizeSpaces(utils.stripTrailingPeriod(stmt));
    if (declKind === "DATA" && /^DATA\(/i.test(s)) return [];
    if (/\bBEGIN\s+OF\b/i.test(s)) return [];
    if (/\bEND\s+OF\b/i.test(s)) return [];

    let body = s.replace(new RegExp(`^${declKind}\\b`, "i"), "").trim();
    if (body.startsWith(":")) body = body.slice(1).trim();

    const parts = utils.splitByCommaOutsideQuotes(body).map((p) => p.trim()).filter(Boolean);
    const out = [];

    for (const p of parts) {
      const firstToken = utils.cleanIdentifierToken(p.split(/\s+/)[0] || "");
      if (!utils.isIdentifier(firstToken)) continue;
      const variableName = firstToken;
      const typeMatch = /\bTYPE\b\s+([^\s]+)/i.exec(p);
      const dataType = typeMatch ? utils.cleanIdentifierToken(typeMatch[1]) : "";
      let value = "";
      if (declKind === "CONSTANTS") {
        const valueMatch = /\bVALUE\b\s+(.+)$/i.exec(p);
        value = valueMatch ? valueMatch[1].trim() : "";
      }

      out.push(new AbapDataDeclaration(declKind, variableName, dataType, value, description, sourceRef));
    }

    return out;
  }

  function parsePerform(stmtText) {
    const statement = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const m = /^PERFORM\s+([A-Za-z_][A-Za-z0-9_\/]*)\b\s*(.*)$/i.exec(statement);
    if (!m) return null;
    const target = m[1];
    const rest = m[2] || "";

    const u = rest.toUpperCase();
    const keys = ["TABLES", "USING", "CHANGING"];
    const indices = keys
      .map((k) => ({ k, idx: u.search(new RegExp(`\\b${k}\\b`)) }))
      .filter((x) => x.idx >= 0)
      .sort((a, b) => a.idx - b.idx);

    const clauses = { TABLES: "", USING: "", CHANGING: "" };
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

    return {
      target,
      args: {
        tables: tokensOf(clauses.TABLES),
        using: tokensOf(clauses.USING),
        changing: tokensOf(clauses.CHANGING),
      },
    };
  }

  function parseWriteTargets(stmtText) {
    const s = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const u = s.toUpperCase();

    const out = [];

    let m = /^([A-Za-z_][A-Za-z0-9_\/]*(?:-[A-Za-z0-9_\/]+)*)\s*=\s*/.exec(s);
    if (m) out.push(m[1]);

    m = /^CLEAR\s+([A-Za-z_][A-Za-z0-9_\/]*(?:-[A-Za-z0-9_\/]+)*)\b/i.exec(s);
    if (m) out.push(m[1]);

    if (u.startsWith("APPEND ")) {
      m = /\bTO\b\s+([A-Za-z_][A-Za-z0-9_\/]*(?:-[A-Za-z0-9_\/]+)*)\b/i.exec(s);
      if (m) out.push(m[1]);
    }

    if (u.startsWith("CONCATENATE ")) {
      m = /\bINTO\b\s+([A-Za-z_][A-Za-z0-9_\/]*(?:-[A-Za-z0-9_\/]+)*)\b/i.exec(s);
      if (m) out.push(m[1]);
    }

    return out;
  }

  function eventNameFromStatement(stmtText) {
    const t = utils.normalizeSpaces(utils.stripTrailingPeriod(stmtText));
    const u = t.toUpperCase();

    const exact = ["INITIALIZATION", "START-OF-SELECTION", "END-OF-SELECTION", "AT LINE-SELECTION", "AT USER-COMMAND"];
    if (exact.includes(u)) return t;

    if (u.startsWith("AT SELECTION-SCREEN")) return t;
    if (u.startsWith("TOP-OF-PAGE")) return t;
    if (u.startsWith("END-OF-PAGE")) return t;

    return null;
  }

  function parseRoutineStatement(model, routine, statement, originalLines) {
    const text = statement.text;
    const normalized = utils.normalizeSpaces(text);
    if (!normalized) return;

    const inlineComment = doc.extractInlineCommentFromStatement(originalLines, statement.startLine, statement.endLine);
    const src = new SourceRef(statement.startLine, statement.endLine);

    if (/^PERFORM\b/i.test(normalized)) {
      const p = parsePerform(normalized);
      if (!p) return;
      const callee = model.ensureForm(p.target);
      const edge = new AbapCallEdge(routine.key, callee.key, p.target, p.args, src);
      model.addEdge(edge);
      return;
    }

    if (/^DATA\b/i.test(normalized)) {
      const decls = parseDataOrConstants(normalized, "DATA", inlineComment, src);
      routine.localData.push(...decls);
      return;
    }

    if (/^CONSTANTS\b/i.test(normalized)) {
      const decls = parseDataOrConstants(normalized, "CONSTANTS", inlineComment, src);
      routine.localConstants.push(...decls);
      return;
    }

    const writes = parseWriteTargets(normalized);
    for (const v of writes) routine.writes.push(new AbapWrite(v, normalized, src));
  }

  function parseForms(model, lines, formBlocks) {
    for (const block of formBlocks) {
      const statements = utils.collectStatements(lines, block.startLine, block.endLine);
      if (statements.length === 0) continue;

      const header = statements[0];
      const headerRef = new SourceRef(block.startLine, header.endLine);
      const formDoc = doc.extractLeadingComment(lines, block.startLine);

      const headerInfo = parseFormHeader(header.text, formDoc.paramDescriptions, headerRef);
      if (!headerInfo) continue;

      const routine = model.defineForm(headerInfo.name, new SourceRef(block.startLine, block.endLine), formDoc.description, headerInfo.params);

      for (let i = 1; i < statements.length; i++) {
        const st = statements[i];
        const normalized = utils.normalizeSpaces(st.text);
        if (/^ENDFORM\b/i.test(normalized)) break;
        parseRoutineStatement(model, routine, st, lines);
      }
    }
  }

  function parseTopLevel(model, lines, formMask) {
    const masked = lines.map((l, idx) => (formMask[idx] ? "" : l));
    const statements = utils.collectStatements(masked, 1, masked.length);

    let currentEvent = null;
    let lastStmt = null;

    for (const st of statements) {
      lastStmt = st;
      const normalized = utils.normalizeSpaces(st.text);
      if (!normalized) continue;

      const ev = eventNameFromStatement(normalized);
      if (ev) {
        currentEvent = model.ensureEvent(ev, new SourceRef(st.startLine, st.endLine));
        continue;
      }

      if (!currentEvent) {
        const inlineComment = doc.extractInlineCommentFromStatement(lines, st.startLine, st.endLine);
        const src = new SourceRef(st.startLine, st.endLine);

        if (/^DATA\b/i.test(normalized)) {
          const decls = parseDataOrConstants(normalized, "DATA", inlineComment, src);
          model.globalData.push(...decls);
          continue;
        }
        if (/^CONSTANTS\b/i.test(normalized)) {
          const decls = parseDataOrConstants(normalized, "CONSTANTS", inlineComment, src);
          model.globalConstants.push(...decls);
          continue;
        }
        continue;
      }

      parseRoutineStatement(model, currentEvent, st, lines);
      if (currentEvent.sourceRef) currentEvent.sourceRef.endLine = st.endLine;
    }

    if (currentEvent && currentEvent.sourceRef && lastStmt) {
      currentEvent.sourceRef.endLine = Math.max(currentEvent.sourceRef.endLine || 0, lastStmt.endLine);
    }
  }

  function parseProgram(text) {
    const normalized = utils.normalizeNewlines(text);
    const lines = utils.splitLines(normalized);
    const offsets = utils.computeLineStartOffsets(normalized);
    const model = new ProgramModel(normalized, lines, offsets);

    const formBlocks = findFormBlocks(lines);
    const formMask = buildFormLineMask(lines, formBlocks);

    parseForms(model, lines, formBlocks);
    parseTopLevel(model, lines, formMask);

    ns.graph.build(model);
    return model;
  }

  ns.parser = { parseProgram };
})(window.AbapFlow);
