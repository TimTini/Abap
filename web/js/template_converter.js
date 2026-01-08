(function (ns) {
  "use strict";

  const IDENT_PATH_RE = /[A-Za-z_][A-Za-z0-9_\/]*(?:(?:->|=>|[-~])[A-Za-z0-9_\/]+)*/g;
  const desc = ns.desc || null;

  function pickDescription(entity, fallbackName) {
    if (desc?.pickDescription) return desc.pickDescription(entity, fallbackName);

    if (!entity) return String(fallbackName || "").trim();
    const user = String(entity.userDescription || "").trim();
    if (user) return user;
    const code = String(entity.description || "").trim();
    if (code) return code;
    return String(fallbackName || entity.variableName || entity.name || "").trim();
  }

  function pickNote(entity) {
    if (!entity) return "";
    const user = String(entity.userNote || "").trim();
    if (user) return user;
    return String(entity.note || "").trim();
  }

  function stripStringsAndTemplates(s) {
    // ABAP strings: '...'; escaped by doubling: ''
    const noStrings = String(s || "").replace(/'(?:[^']|'')*'/g, " ");
    // ABAP string templates: |...|  (best-effort)
    return noStrings.replace(/\|[^|]*\|/g, " ");
  }

  function rootFromPath(path) {
    const s = String(path || "").trim();
    if (!s) return "";
    const root = s.split(/(?:->|=>|[-~])/)[0] || "";
    const m = /^[A-Za-z_][A-Za-z0-9_\/]*/.exec(root.trim());
    return m ? m[0] : "";
  }

  function resolveBestSymbol(model, routineKey, expr) {
    const resolver = ns.lineage?.resolveSymbol;
    const rk = String(routineKey || "").trim();
    if (!resolver || !model || !rk) return { root: "", scope: "unknown", decl: null };

    const sanitized = stripStringsAndTemplates(expr);
    IDENT_PATH_RE.lastIndex = 0;
    const tokens = [];
    let m = null;
    while ((m = IDENT_PATH_RE.exec(sanitized))) {
      const token = String(m[0] || "").trim();
      if (token) tokens.push(token);
    }

    for (const token of tokens) {
      const root = rootFromPath(token);
      if (!root) continue;
      const res = resolver(model, rk, root);
      if (res && String(res.scope || "").toLowerCase() !== "unknown") return { root, scope: res.scope, decl: res.decl || null };
    }

    const fallbackRoot = tokens.length ? rootFromPath(tokens[0]) : "";
    if (!fallbackRoot) return { root: "", scope: "unknown", decl: null };
    const fallback = resolver(model, rk, fallbackRoot);
    return { root: fallbackRoot, scope: fallback?.scope || "unknown", decl: fallback?.decl || null };
  }

  function extractSource(model, sourceRef) {
    if (!model || !Array.isArray(model.lines) || !sourceRef) return "";
    const start = Math.max(1, Math.floor(Number(sourceRef.startLine || 1)));
    const end = Math.min(model.lines.length, Math.floor(Number(sourceRef.endLine || start)));
    return model.lines.slice(start - 1, end).join("\n").trim();
  }

  function extractPerformEdges(model) {
    const edges = model && Array.isArray(model.edges) ? model.edges : [];
    return edges.filter((e) => String(e?.toKey || "").toUpperCase().startsWith("FORM:"));
  }

  function extractAssignments(model) {
    const out = [];
    const nodes = Array.from(model?.nodes?.values ? model.nodes.values() : []);
    for (const routine of nodes) {
      const assigns = Array.isArray(routine?.assignments) ? routine.assignments : [];
      for (const a of assigns) {
        if (!a || !a.sourceRef) continue;
        out.push({ routine, assignment: a });
      }
    }

    out.sort((a, b) => Number(a?.assignment?.sourceRef?.startLine || 0) - Number(b?.assignment?.sourceRef?.startLine || 0));
    return out;
  }

  function extractIfStatements(model) {
    const out = [];
    const nodes = Array.from(model?.nodes?.values ? model.nodes.values() : []);
    for (const routine of nodes) {
      const list = Array.isArray(routine?.ifStatements) ? routine.ifStatements : [];
      for (const st of list) {
        if (!st || !st.sourceRef) continue;
        out.push({ routine, ifStatement: st });
      }
    }

    out.sort((a, b) => Number(a?.ifStatement?.sourceRef?.startLine || 0) - Number(b?.ifStatement?.sourceRef?.startLine || 0));
    return out;
  }

  function buildArgMappings(model, callerKey, actuals, formals, options) {
    const a = Array.isArray(actuals) ? actuals : [];
    const f = Array.isArray(formals) ? formals : [];
    const len = a.length;
    if (len === 0) return [];

    const callPath = Array.isArray(options?.callPath) ? options.callPath : null;

    const out = [];
    for (let i = 0; i < len; i++) {
      const formal = f[i] || null;
      const actualText = a[i] != null ? String(a[i]) : "";
      const described = desc?.describeExpressionWithOrigin
        ? desc.describeExpressionWithOrigin(model, callerKey, actualText, { callPath })
        : { text: "", originKey: "" };
      out.push({
        actual: actualText,
        name: formal ? String(formal.name || "") : "",
        dataType: formal ? String(formal.dataType || "") : "",
        description: String(described.text || "").trim() || actualText.trim() || (formal ? String(formal.name || "") : ""),
        originKey: String(described.originKey || ""),
        note: formal ? pickNote(formal) : "",
      });
    }
    return out;
  }

  function buildPerformContext(model, edge, options) {
    const toKey = String(edge?.toKey || "");
    const fromKey = String(edge?.fromKey || "");

    const callee = model?.nodes?.get(toKey) || null;
    const caller = model?.nodes?.get(fromKey) || null;

    const params = Array.isArray(callee?.params) ? callee.params : [];
    const formalTables = params.filter((p) => String(p?.kind || "").toUpperCase() === "TABLES");
    const formalUsing = params.filter((p) => String(p?.kind || "").toUpperCase() === "USING");
    const formalChanging = params.filter((p) => String(p?.kind || "").toUpperCase() === "CHANGING");
    const formalRaising = params.filter((p) => String(p?.kind || "").toUpperCase() === "RAISING");

    const args = edge?.args || {};
    const callPath = Array.isArray(options?.callPath) ? options.callPath : null;
    const tables = buildArgMappings(model, fromKey, args.tables, formalTables, { callPath });
    const using = buildArgMappings(model, fromKey, args.using, formalUsing, { callPath });
    const changing = buildArgMappings(model, fromKey, args.changing, formalChanging, { callPath });
    const raising = formalRaising.map((p) => ({
      name: String(p?.name || ""),
      description: pickDescription(p, p?.name),
      originKey: ns.notes?.makeParamKey ? ns.notes.makeParamKey(toKey, String(p?.name || "")) : "",
      note: pickNote(p),
    }));

    const performName = String(edge?.targetName || callee?.name || "").trim();

    const presence = {
      tables: tables.length > 0,
      using: using.length > 0,
      changing: changing.length > 0,
      raising: raising.length > 0,
    };

    return {
      perform: {
        key: toKey,
        name: performName,
        description: pickDescription(callee, performName),
        note: pickNote(callee),
        originKey: toKey,
      },
      caller: {
        key: fromKey,
        name: String(caller?.name || "").trim(),
      },
      presence,
      tables,
      using,
      changing,
      raising,
    };
  }

  function buildAssignmentContext(model, routine, assignment, options) {
    const r = routine || null;
    const a = assignment || null;

    const lhsText = String(a?.lhs || "");
    const rhsText = String(a?.rhs || "");

    const routineKey = String(r?.key || "");
    const lhsResolved = resolveBestSymbol(model, routineKey, lhsText);
    const rhsResolved = resolveBestSymbol(model, routineKey, rhsText);
    const callPath = Array.isArray(options?.callPath) ? options.callPath : null;

    const lhsDesc = desc?.describeExpressionWithOrigin
      ? desc.describeExpressionWithOrigin(model, routineKey, lhsText, { callPath })
      : { text: pickDescription(lhsResolved.decl, lhsResolved.root || lhsText), originKey: "" };

    const rhsDesc = desc?.describeExpressionWithOrigin
      ? desc.describeExpressionWithOrigin(model, routineKey, rhsText, { callPath })
      : { text: pickDescription(rhsResolved.decl, rhsResolved.root || rhsText), originKey: "" };

    return {
      routine: {
        key: String(r?.key || ""),
        kind: String(r?.kind || ""),
        name: String(r?.name || ""),
      },
      assignment: {
        lhs: lhsText,
        rhs: rhsText,
        statement: String(a?.statement || ""),
      },
      item: {
        text: lhsText,
        root: lhsResolved.root,
        scope: String(lhsResolved.scope || "unknown"),
        description: String(lhsDesc.text || "").trim() || lhsText.trim() || lhsResolved.root,
        note: pickNote(lhsResolved.decl),
        originKey: String(lhsDesc.originKey || ""),
      },
      value: {
        text: rhsText,
        root: rhsResolved.root,
        scope: String(rhsResolved.scope || "unknown"),
        description: String(rhsDesc.text || "").trim() || rhsText.trim() || rhsResolved.root,
        note: pickNote(rhsResolved.decl),
        originKey: String(rhsDesc.originKey || ""),
      },
    };
  }

  function buildIfContext(model, routine, ifStatement, options) {
    const r = routine || null;
    const st = ifStatement || null;

    const routineKey = String(r?.key || "");
    const kind = String(st?.kind || "IF").trim().toUpperCase() || "IF";
    const condition = String(st?.condition || "").trim();
    const callPath = Array.isArray(options?.callPath) ? options.callPath : null;

    const parsed = typeof ns.logic?.parseIfExpression === "function" ? ns.logic.parseIfExpression(condition) : [{ item1: condition, operator: "", item2: "", association: "", raw: condition }];

    const conditions = (parsed || []).map((c) => {
      const item1Text = String(c?.item1 || "").trim();
      const item2Text = String(c?.item2 || "").trim();

      const item1Resolved = resolveBestSymbol(model, routineKey, item1Text);
      const item2Resolved = item2Text ? resolveBestSymbol(model, routineKey, item2Text) : { root: "", scope: "unknown", decl: null };

      const item1Desc = desc?.describeExpressionWithOrigin
        ? desc.describeExpressionWithOrigin(model, routineKey, item1Text, { callPath })
        : { text: pickDescription(item1Resolved.decl, item1Text) || item1Text, originKey: "" };

      const item2Desc =
        item2Text && desc?.describeExpressionWithOrigin
          ? desc.describeExpressionWithOrigin(model, routineKey, item2Text, { callPath })
          : { text: pickDescription(item2Resolved.decl, item2Text) || item2Text, originKey: "" };

      return {
        item1: {
          text: item1Text,
          root: item1Resolved.root,
          scope: String(item1Resolved.scope || "unknown"),
          description: String(item1Desc.text || "").trim() || item1Text,
          note: pickNote(item1Resolved.decl),
          originKey: String(item1Desc.originKey || ""),
        },
        operator: String(c?.operator || "").trim(),
        item2: {
          text: item2Text,
          root: item2Resolved.root,
          scope: String(item2Resolved.scope || "unknown"),
          description: String(item2Desc.text || "").trim() || item2Text,
          note: pickNote(item2Resolved.decl),
          originKey: String(item2Desc.originKey || ""),
        },
        association: String(c?.association || "").trim().toUpperCase(),
        raw: String(c?.raw || "").trim(),
      };
    });

    return {
      routine: {
        key: routineKey,
        kind: String(r?.kind || ""),
        name: String(r?.name || ""),
      },
      if: { kind, condition },
      conditions,
    };
  }

  function resolvePath(obj, path) {
    const raw = String(path || "").trim();
    if (!raw) return undefined;

    let cur = obj;
    const parts = raw.split(".").map((p) => p.trim()).filter(Boolean);

    for (const part of parts) {
      if (cur == null) return undefined;
      const m = /^([A-Za-z0-9_$]+)(.*)$/.exec(part);
      if (!m) return undefined;
      const prop = m[1];
      let rest = m[2] || "";
      cur = cur[prop];

      while (rest) {
        const m2 = /^\[(\d+)\](.*)$/.exec(rest);
        if (!m2) return undefined;
        const idx = Number(m2[1]);
        if (!Number.isFinite(idx)) return undefined;
        cur = cur == null ? undefined : cur[idx];
        rest = m2[2] || "";
      }
    }

    return cur;
  }

  function fillText(text, context) {
    const s = String(text ?? "");
    return s.replace(/\{([^}]+)\}/g, (_m, expr) => {
      const value = resolvePath(context, expr);
      if (value == null) return "";
      if (typeof value === "string") return value;
      if (typeof value === "number" || typeof value === "boolean") return String(value);
      return "";
    });
  }

  function replaceListIndexInText(text, listName, fromIndex, toIndex) {
    const s = String(text ?? "");
    const from = Number(fromIndex);
    const to = Number(toIndex);
    if (!Number.isFinite(from) || !Number.isFinite(to)) return s;
    const re = new RegExp(`\\{${listName}\\[${from}\\]`, "g");
    return s.replace(re, `{${listName}[${to}]`);
  }

  function fillTemplateConfig(templateConfig, context) {
    const cfg = JSON.parse(JSON.stringify(templateConfig || {}));
    if (!cfg || typeof cfg !== "object") return cfg;

    const sectionLabels =
      templateConfig?.sectionLabels && typeof templateConfig.sectionLabels === "object" ? templateConfig.sectionLabels : {};
    const presence = context?.presence && typeof context.presence === "object" ? context.presence : {};
    const labels = {
      ...(context?.labels && typeof context.labels === "object" ? context.labels : {}),
      tables: presence.tables ? String(sectionLabels.tables ?? "Table") : "",
      using: presence.using ? String(sectionLabels.using ?? "Using") : "",
      changing: presence.changing ? String(sectionLabels.changing ?? "Changing") : "",
      raising: presence.raising ? String(sectionLabels.raising ?? "Raising") : "",
    };
    const ctx = { ...(context || {}), labels };

    if (Array.isArray(cfg.cells)) {
      for (const cell of cfg.cells) {
        if (!cell || typeof cell !== "object") continue;
        if (cell.text == null) continue;

        const rawText = String(cell.text ?? "");
        const bindMatch = /^\{([^}]+)\}$/.exec(rawText.trim());
        if (bindMatch) cell.bind = String(bindMatch[1] || "").trim();

        cell.text = fillText(rawText, ctx);
      }
    }

    return cfg;
  }

  function parseCellAddr(addr) {
    const s = String(addr || "").trim().toUpperCase();
    const m = /^([A-Z]+)(\d+)$/.exec(s);
    if (!m) return null;
    return { col: m[1], row: Number(m[2]) };
  }

  function shiftAddrRow(addr, afterRow, delta) {
    const parsed = parseCellAddr(addr);
    if (!parsed) return addr;
    if (parsed.row > afterRow) return `${parsed.col}${parsed.row + delta}`;
    return `${parsed.col}${parsed.row}`;
  }

  function shiftRowHeights(rowHeights, afterRow, delta) {
    const src = rowHeights && typeof rowHeights === "object" ? rowHeights : {};
    const out = {};
    for (const [k, v] of Object.entries(src)) {
      const r = Number(k);
      if (!Number.isFinite(r) || r <= 0) continue;
      const nr = r > afterRow ? r + delta : r;
      out[String(nr)] = v;
    }
    return out;
  }

  function scanRepeatSections(cells) {
    const info = new Map();
    const re = /\{(tables|using|changing|raising|conditions)\[(\d+)\]\.[^}]*\}/g;

    for (const cell of cells) {
      const a = parseCellAddr(cell?.addr);
      if (!a) continue;
      const text = String(cell?.text ?? "");
      re.lastIndex = 0;
      let m;
      while ((m = re.exec(text))) {
        const list = m[1];
        const idx = Number(m[2]);
        if (!Number.isFinite(idx) || idx < 0) continue;

        if (!info.has(list)) {
          info.set(list, { maxIndex: -1, rowForIndex: new Map(), rowsUsed: new Set() });
        }
        const cur = info.get(list);
        cur.maxIndex = Math.max(cur.maxIndex, idx);
        cur.rowsUsed.add(a.row);
        if (!cur.rowForIndex.has(idx)) cur.rowForIndex.set(idx, a.row);
      }
    }

    return info;
  }

  function expandExcelLikeTableTemplate(templateConfig, context) {
    const cfg = JSON.parse(JSON.stringify(templateConfig || {}));
    if (!cfg || typeof cfg !== "object") return cfg;
    if (cfg.type !== "excel-like-table") return cfg;
    if (!cfg.grid || typeof cfg.grid !== "object") return cfg;
    if (!Array.isArray(cfg.cells)) return cfg;

    const merges = Array.isArray(cfg.merges) ? cfg.merges : [];
    cfg.merges = merges;

    const listLens = {
      tables: Array.isArray(context?.tables) ? context.tables.length : 0,
      using: Array.isArray(context?.using) ? context.using.length : 0,
      changing: Array.isArray(context?.changing) ? context.changing.length : 0,
      raising: Array.isArray(context?.raising) ? context.raising.length : 0,
      conditions: Array.isArray(context?.conditions) ? context.conditions.length : 0,
    };

    const info = scanRepeatSections(cfg.cells);
    const sections = [];

    for (const [list, data] of info.entries()) {
      const maxIndex = Number(data?.maxIndex);
      if (!Number.isFinite(maxIndex) || maxIndex < 0) continue;

      const rows = Array.from(data.rowsUsed || []).filter((r) => Number.isFinite(r) && r > 0).sort((a, b) => a - b);
      if (!rows.length) continue;

      sections.push({
        list,
        baseCount: maxIndex + 1,
        baseIndex: maxIndex,
        startRow: rows[0],
        endRow: rows[rows.length - 1],
        templateRow: data.rowForIndex?.get(maxIndex) || rows[rows.length - 1],
      });
    }

    sections.sort((a, b) => b.startRow - a.startRow);

    function insertRowsAfter(afterRow, count, section) {
      if (count <= 0) return;

      cfg.cells = cfg.cells.map((cell) => {
        const addr = shiftAddrRow(cell?.addr, afterRow, count);
        return { ...cell, addr };
      });

      cfg.merges = cfg.merges.map((m) => {
        const start = shiftAddrRow(m?.start, afterRow, count);
        return { ...m, start };
      });

      cfg.grid.rowHeights = shiftRowHeights(cfg.grid.rowHeights, afterRow, count);
      cfg.grid.rows = Math.max(1, Math.floor(Number(cfg.grid.rows || 1)) + count);

      const rowCells = cfg.cells
        .filter((cell) => parseCellAddr(cell?.addr)?.row === section.templateRow)
        .map((cell) => ({ ...cell }));
      const rowMerges = cfg.merges
        .filter((m) => parseCellAddr(m?.start)?.row === section.templateRow)
        .map((m) => ({ ...m }));
      const baseHeight =
        cfg.grid.rowHeights?.[String(section.templateRow)] ?? cfg.grid.rowHeights?.[Number(section.templateRow)] ?? null;

      for (let i = 1; i <= count; i++) {
        const newRow = afterRow + i;
        const newIndex = section.baseIndex + i;

        for (const baseCell of rowCells) {
          const parsed = parseCellAddr(baseCell?.addr);
          if (!parsed) continue;
          const newAddr = `${parsed.col}${newRow}`;
          const text = replaceListIndexInText(baseCell?.text, section.list, section.baseIndex, newIndex);
          let nextText = text;
          if (parsed.col === "A" && /\{labels\./.test(nextText)) nextText = "";
          cfg.cells.push({ ...baseCell, addr: newAddr, text: nextText });
        }

        for (const baseMerge of rowMerges) {
          const parsed = parseCellAddr(baseMerge?.start);
          if (!parsed) continue;
          cfg.merges.push({ ...baseMerge, start: `${parsed.col}${newRow}` });
        }

        if (baseHeight != null) cfg.grid.rowHeights[String(newRow)] = baseHeight;
      }
    }

    for (const section of sections) {
      const want = listLens[section.list] || 0;
      const baseCount = section.baseCount;
      if (want <= baseCount) continue;
      insertRowsAfter(section.endRow, want - baseCount, section);
    }

    return cfg;
  }

  function compactExcelLikeTableConfig(cfg) {
    if (!cfg || cfg.type !== "excel-like-table") return cfg;
    if (!cfg.grid || typeof cfg.grid !== "object") return cfg;
    if (!cfg.compact?.removeEmptyRows) return cfg;
    if (!Array.isArray(cfg.cells)) return cfg;

    const totalRows = Math.max(1, Math.floor(Number(cfg.grid.rows || 1)));
    const rowsWithText = new Set([1]);

    for (const cell of cfg.cells) {
      const parsed = parseCellAddr(cell?.addr);
      if (!parsed) continue;
      if (parsed.row < 1 || parsed.row > totalRows) continue;
      const text = String(cell?.text ?? "").trim();
      if (text) rowsWithText.add(parsed.row);
    }

    const keepRows = Array.from(rowsWithText).filter((r) => r >= 1 && r <= totalRows).sort((a, b) => a - b);
    if (keepRows.length === totalRows) return cfg;

    const rowMap = new Map();
    for (let i = 0; i < keepRows.length; i++) rowMap.set(keepRows[i], i + 1);

    cfg.cells = cfg.cells
      .map((cell) => {
        const parsed = parseCellAddr(cell?.addr);
        if (!parsed) return null;
        const newRow = rowMap.get(parsed.row);
        if (!newRow) return null;
        return { ...cell, addr: `${parsed.col}${newRow}` };
      })
      .filter(Boolean);

    if (Array.isArray(cfg.merges)) {
      cfg.merges = cfg.merges
        .map((merge) => {
          const parsed = parseCellAddr(merge?.start);
          if (!parsed) return null;
          const newRow = rowMap.get(parsed.row);
          if (!newRow) return null;
          return { ...merge, start: `${parsed.col}${newRow}` };
        })
        .filter(Boolean);
    }

    const oldHeights = cfg.grid.rowHeights && typeof cfg.grid.rowHeights === "object" ? cfg.grid.rowHeights : {};
    const newHeights = {};
    for (const oldRow of keepRows) {
      const newRow = rowMap.get(oldRow);
      const h = oldHeights[oldRow] ?? oldHeights[String(oldRow)];
      if (newRow && h != null) newHeights[String(newRow)] = h;
    }
    cfg.grid.rowHeights = newHeights;
    cfg.grid.rows = keepRows.length;
    return cfg;
  }

  function convertPerforms(model, templateConfig) {
    const edges = extractPerformEdges(model)
      .slice()
      .sort((a, b) => Number(a?.sourceRef?.startLine || 0) - Number(b?.sourceRef?.startLine || 0));
    return edges.map((edge) => {
      const context = buildPerformContext(model, edge);
      const expandedTemplate = expandExcelLikeTableTemplate(templateConfig, context);
      const filledConfig = compactExcelLikeTableConfig(fillTemplateConfig(expandedTemplate, context));
      const original = extractSource(model, edge.sourceRef) || `PERFORM ${context.perform.name}`;
      return { edge, context, filledConfig, original };
    });
  }

  function convertAssignments(model, templateConfig) {
    const items = extractAssignments(model);
    return items.map(({ routine, assignment }) => {
      const context = buildAssignmentContext(model, routine, assignment);
      const filledConfig = compactExcelLikeTableConfig(fillTemplateConfig(templateConfig, context));
      const original = extractSource(model, assignment.sourceRef) || `${context.assignment.lhs} = ${context.assignment.rhs}`;
      return {
        routineKey: String(routine?.key || ""),
        routineName: String(routine?.name || ""),
        routineKind: String(routine?.kind || ""),
        assignment,
        context,
        filledConfig,
        original,
        sourceRef: assignment?.sourceRef || null,
      };
    });
  }

  function convertIfStatements(model, templateConfig) {
    const items = extractIfStatements(model);
    return items.map(({ routine, ifStatement }) => {
      const context = buildIfContext(model, routine, ifStatement);
      const expanded = expandExcelLikeTableTemplate(templateConfig, context);
      const filledConfig = compactExcelLikeTableConfig(fillTemplateConfig(expanded, context));
      const original = extractSource(model, ifStatement.sourceRef) || `${context.if?.kind || "IF"} ${context.if?.condition || ""}`;
      return {
        routineKey: String(routine?.key || ""),
        routineName: String(routine?.name || ""),
        routineKind: String(routine?.kind || ""),
        ifStatement,
        context,
        filledConfig,
        original,
        sourceRef: ifStatement?.sourceRef || null,
      };
    });
  }

  ns.templateConverter = {
    extractPerformEdges,
    extractAssignments,
    extractIfStatements,
    buildPerformContext,
    buildAssignmentContext,
    buildIfContext,
    fillTemplateConfig,
    expandExcelLikeTableTemplate,
    compactExcelLikeTableConfig,
    convertPerforms,
    convertAssignments,
    convertIfStatements,
  };
})(window.AbapFlow);
