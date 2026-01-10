(function (ns) {
  "use strict";

  const utils = ns.utils;
  const IDENT_RE = /[A-Za-z_][A-Za-z0-9_\/]*(?:[-~][A-Za-z0-9_\/]+)*/g;

  function buildGlobalIndex(model) {
    const set = new Set();
    for (const d of model.globalData) set.add(String(d.variableName || "").toLowerCase());
    for (const c of model.globalConstants) set.add(String(c.variableName || "").toLowerCase());
    return set;
  }

  function buildGlobalDeclMap(model) {
    const map = new Map();
    for (const d of model.globalData || []) {
      const name = String(d.variableName || "").toLowerCase();
      if (name) map.set(name, { scope: "GLOBAL:DATA", decl: d });
    }
    for (const c of model.globalConstants || []) {
      const name = String(c.variableName || "").toLowerCase();
      if (name) map.set(name, { scope: "GLOBAL:CONSTANTS", decl: c });
    }
    return map;
  }

  function stripStrings(s) {
    // ABAP strings: '...'; escaped by doubling: ''
    return String(s || "").replace(/'(?:[^']|'')*'/g, " ");
  }

  function collectUsedGlobalNames(model, fromLine, toLine, globalDeclMap, stopAtNameLower) {
    const used = new Set();
    const lines = model?.lines || [];
    const from = Math.max(1, Number(fromLine || 1));
    const to = Math.min(lines.length, Number(toLine || from));
    const stop = String(stopAtNameLower || "").toLowerCase();

    for (let ln = from; ln <= to; ln++) {
      const raw = String(lines[ln - 1] || "");
      if (utils.isFullLineComment(raw)) continue;

      const code = utils.stripInlineComment(raw);
      if (!code.trim()) continue;

      const sanitized = stripStrings(code);
      IDENT_RE.lastIndex = 0;
      let m = null;
      while ((m = IDENT_RE.exec(sanitized))) {
        const token = String(m[0] || "").trim();
        if (!token) continue;
        const root = token.toLowerCase().split(/[-~]/)[0];
        if (!root) continue;
        if (!globalDeclMap.has(root)) continue;
        used.add(root);
        if (stop && root === stop) return used;
      }
    }

    return used;
  }

  function resolveSymbol(model, routineKey, varName) {
    const nameLower = String(varName || "").toLowerCase();
    const routine = model.nodes.get(routineKey);
    if (!routine) return { scope: "unknown" };

    for (const p of routine.params) {
      if (String(p.name || "").toLowerCase() === nameLower) return { scope: "parameter", decl: p };
    }

    for (const d of routine.localData) {
      if (String(d.variableName || "").toLowerCase() === nameLower) return { scope: "local", decl: d };
    }
    for (const c of routine.localConstants) {
      if (String(c.variableName || "").toLowerCase() === nameLower) return { scope: "local", decl: c };
    }

    for (const d of model.globalData) {
      if (String(d.variableName || "").toLowerCase() === nameLower) return { scope: "global", decl: d };
    }
    for (const c of model.globalConstants) {
      if (String(c.variableName || "").toLowerCase() === nameLower) return { scope: "global", decl: c };
    }

    return { scope: "unknown" };
  }

  function matchesVariableName(writtenName, tracedName) {
    const w = String(writtenName || "").toLowerCase();
    const t = String(tracedName || "").toLowerCase();
    return w === t || w.startsWith(`${t}-`) || w.startsWith(`${t}~`);
  }

  function formalParamsByKind(routine, kind) {
    return routine.params.filter((p) => String(p.kind).toUpperCase() === kind);
  }

  function mapThroughEdge(model, edge, callerVarName) {
    const callerToken = utils.cleanIdentifierToken(callerVarName);
    if (!utils.isIdentifier(callerToken)) return null;
    const callerNameLower = String(callerToken || "").toLowerCase();
    const callee = model.nodes.get(edge.toKey);
    if (!callee) return null;

    const rules = [
      { edgeKey: "changing", paramKind: "CHANGING" },
      { edgeKey: "tables", paramKind: "TABLES" },
      { edgeKey: "using", paramKind: "USING" },
    ];

    for (const rule of rules) {
      const actualArgs = (edge.args && edge.args[rule.edgeKey]) || [];
      const formalParams = formalParamsByKind(callee, rule.paramKind);
      const max = Math.min(actualArgs.length, formalParams.length);
      for (let i = 0; i < max; i++) {
        const actual = utils.cleanIdentifierToken(actualArgs[i]);
        if (!actual) continue;
        if (String(actual).toLowerCase() === callerNameLower) {
          return { mappedVarName: formalParams[i].name, kind: rule.paramKind };
        }
      }
    }

    return null;
  }

  function mapBackThroughEdge(model, edge, calleeVarName) {
    const calleeNameLower = String(calleeVarName || "").toLowerCase();
    const callee = model.nodes.get(edge.toKey);
    if (!callee) return null;

    const decl = callee.params.find((p) => String(p.name || "").toLowerCase() === calleeNameLower) || null;
    const kind = String(decl?.kind || "").toUpperCase();

    const rule = kind === "USING" ? { edgeKey: "using", paramKind: "USING" } : kind === "CHANGING" ? { edgeKey: "changing", paramKind: "CHANGING" } : kind === "TABLES" ? { edgeKey: "tables", paramKind: "TABLES" } : null;

    if (!rule) return null;

    const formalParams = formalParamsByKind(callee, rule.paramKind);
    const idx = formalParams.findIndex((p) => String(p.name || "").toLowerCase() === calleeNameLower);
    if (idx < 0) return null;

    const actualArgs = (edge.args && edge.args[rule.edgeKey]) || [];
    if (idx >= actualArgs.length) return null;

    const actual = utils.cleanIdentifierToken(actualArgs[idx]);
    if (!actual) return null;

    return { mappedVarName: actual, kind: rule.paramKind };
  }

  function traceVariable(model, startKey, varName, options) {
    const maxNodes = Math.max(50, Number(options?.maxNodes || 250));
    const globalIndex = buildGlobalIndex(model);
    const globalDeclMap = buildGlobalDeclMap(model);
    const usesGlobalCache = new Map();
    const usesGlobalSubtreeCache = new Map();
    const usesGlobalSubtreeVisiting = new Set();

    const startVar = String(varName || "").trim();
    if (!startVar) return { error: "Variable name is empty." };

    const startRoutine = model.nodes.get(startKey);
    if (!startRoutine) return { error: "Subroutine/event not found." };

    const stack = new Set();
    let visitedCount = 0;

    function routineUsesGlobal(routineKey, nameLower) {
      const key = `${String(routineKey || "")}|${String(nameLower || "").toLowerCase()}`;
      if (usesGlobalCache.has(key)) return usesGlobalCache.get(key);
      const routine = model.nodes.get(routineKey);
      if (!routine?.sourceRef) {
        usesGlobalCache.set(key, false);
        return false;
      }

      const used = collectUsedGlobalNames(
        model,
        routine.sourceRef.startLine,
        routine.sourceRef.endLine || routine.sourceRef.startLine,
        globalDeclMap,
        nameLower,
      );
      const out = used.has(String(nameLower || "").toLowerCase());
      usesGlobalCache.set(key, out);
      return out;
    }

    function routineUsesGlobalInSubtree(routineKey, nameLower) {
      const key = `${String(routineKey || "")}|${String(nameLower || "").toLowerCase()}`;
      if (usesGlobalSubtreeCache.has(key)) return usesGlobalSubtreeCache.get(key);
      if (usesGlobalSubtreeVisiting.has(key)) return false;
      usesGlobalSubtreeVisiting.add(key);

      let out = routineUsesGlobal(routineKey, nameLower);
      if (!out) {
        const routine = model.nodes.get(routineKey);
        for (const edge of routine?.calls || []) {
          if (routineUsesGlobalInSubtree(edge.toKey, nameLower)) {
            out = true;
            break;
          }
        }
      }

      usesGlobalSubtreeVisiting.delete(key);
      usesGlobalSubtreeCache.set(key, out);
      return out;
    }

    function walk(routineKey, currentVarName) {
      visitedCount++;
      const routine = model.nodes.get(routineKey);
      if (!routine) return null;

      const visitKey = `${routineKey}|${String(currentVarName || "").toLowerCase()}`;
      if (stack.has(visitKey)) {
        return {
          routineKey,
          routineName: routine.name,
          routineKind: routine.kind,
          varName: currentVarName,
          resolution: resolveSymbol(model, routineKey, currentVarName),
          sourceRef: routine.sourceRef,
          cycle: true,
          writes: [],
          calls: [],
        };
      }

      if (visitedCount > maxNodes) {
        return {
          routineKey,
          routineName: routine.name,
          routineKind: routine.kind,
          varName: currentVarName,
          resolution: resolveSymbol(model, routineKey, currentVarName),
          sourceRef: routine.sourceRef,
          truncated: true,
          writes: [],
          calls: [],
        };
      }

      stack.add(visitKey);

      const resolution = resolveSymbol(model, routineKey, currentVarName);
      const nameLower = String(currentVarName || "").toLowerCase();
      const isGlobal = resolution.scope === "global" && globalIndex.has(nameLower);

      const writes = routine.writes.filter((w) => matchesVariableName(w.variableName, currentVarName));
      const decl = resolution.decl;
      if (
        decl &&
        (resolution.scope === "local" || resolution.scope === "global") &&
        decl.declKind &&
        decl.variableName &&
        decl.value &&
        matchesVariableName(decl.variableName, currentVarName)
      ) {
        const dt = decl.dataType ? ` TYPE ${decl.dataType}` : "";
        const declUpper = String(decl.declKind || "").toUpperCase();
        const valueKeyword = declUpper === "PARAMETERS" ? "DEFAULT" : "VALUE";
        writes.unshift({
          variableName: decl.variableName,
          statement: `${decl.declKind} ${decl.variableName}${dt} ${valueKeyword} ${decl.value}`,
          sourceRef: decl.sourceRef || null,
        });
      }

      const calls = [];

      if (resolution.scope === "parameter") {
        const incoming = (routine.calledBy || []).slice().sort((a, b) => {
          const la = a.sourceRef?.startLine ?? 1e9;
          const lb = b.sourceRef?.startLine ?? 1e9;
          return la - lb;
        });

        for (const edge of incoming) {
          const mapping = mapBackThroughEdge(model, edge, currentVarName);
          if (!mapping) continue;
          const child = walk(edge.fromKey, mapping.mappedVarName);
          if (!child) continue;
          calls.push({
            direction: "fromCaller",
            edge,
            fromVar: mapping.mappedVarName,
            toVar: currentVarName,
            mappingKind: mapping.kind,
            child,
          });
        }
      }

      for (const edge of routine.calls) {
        const paramMapping = mapThroughEdge(model, edge, currentVarName);
        if (paramMapping) {
          const child = walk(edge.toKey, paramMapping.mappedVarName);
          if (!child) continue;
          calls.push({
            direction: "toCallee",
            edge,
            fromVar: currentVarName,
            toVar: paramMapping.mappedVarName,
            mappingKind: paramMapping.kind,
            child,
          });
          continue;
        }

        if (!isGlobal) continue;
        if (!routineUsesGlobalInSubtree(edge.toKey, nameLower)) continue;

        const child = walk(edge.toKey, currentVarName);
        if (!child) continue;
        calls.push({
          direction: "toCallee",
          edge,
          fromVar: currentVarName,
          toVar: currentVarName,
          mappingKind: "GLOBAL",
          child,
        });
      }

      stack.delete(visitKey);

      return {
        routineKey,
        routineName: routine.name,
        routineKind: routine.kind,
        varName: currentVarName,
        resolution,
        sourceRef: routine.sourceRef,
        writes,
        calls,
      };
    }

    const root = walk(startKey, startVar);
    return { startKey, varName: startVar, root };
  }

  function getVariablesForRoutine(model, routineKey, options) {
    const routine = model.nodes.get(routineKey);
    if (!routine) return [];

    const vars = [];
    for (const p of routine.params) vars.push({ name: p.name, scope: `PARAM:${p.kind}`, dataType: p.dataType, description: p.description });
    for (const d of routine.localData) vars.push({ name: d.variableName, scope: "LOCAL:DATA", dataType: d.dataType, description: d.description });
    for (const c of routine.localConstants)
      vars.push({ name: c.variableName, scope: "LOCAL:CONSTANTS", dataType: c.dataType, description: c.description });

    const globalMode = String(options?.globalMode || "all").toLowerCase();
    if (globalMode === "all") {
      for (const d of model.globalData) vars.push({ name: d.variableName, scope: "GLOBAL:DATA", dataType: d.dataType, description: d.description });
      for (const c of model.globalConstants) vars.push({ name: c.variableName, scope: "GLOBAL:CONSTANTS", dataType: c.dataType, description: c.description });
    } else if (globalMode === "used") {
      const globalDeclMap = buildGlobalDeclMap(model);
      const used = collectUsedGlobalNames(
        model,
        routine.sourceRef?.startLine,
        routine.sourceRef?.endLine || routine.sourceRef?.startLine,
        globalDeclMap,
      );

      const items = Array.from(used)
        .map((nameLower) => globalDeclMap.get(nameLower))
        .filter(Boolean)
        .map((entry) => ({
          name: entry.decl.variableName,
          scope: entry.scope,
          dataType: entry.decl.dataType,
          description: entry.decl.description,
        }))
        .sort((a, b) => String(a.name || "").localeCompare(String(b.name || "")));

      vars.push(...items);
    }

    return vars;
  }

  function getGlobalVariables(model) {
    const out = [];
    for (const d of model?.globalData || []) out.push({ name: d.variableName, scope: "GLOBAL:DATA", dataType: d.dataType, description: d.description });
    for (const c of model?.globalConstants || [])
      out.push({ name: c.variableName, scope: "GLOBAL:CONSTANTS", dataType: c.dataType, description: c.description });
    out.sort((a, b) => String(a.name || "").localeCompare(String(b.name || "")));
    return out;
  }

  function getProgramEntrypoints(model) {
    const nodes = Array.from(model?.nodes?.values?.() || []);
    const events = nodes
      .filter((n) => n.kind === "EVENT")
      .sort((a, b) => (a.sourceRef?.startLine ?? 1e9) - (b.sourceRef?.startLine ?? 1e9))
      .map((n) => n.key);

    if (events.length > 0) return events;

    const forms = nodes
      .filter((n) => n.kind === "FORM" && n.isDefined && (n.calledBy?.length || 0) === 0)
      .sort((a, b) => (a.sourceRef?.startLine ?? 1e9) - (b.sourceRef?.startLine ?? 1e9))
      .map((n) => n.key);

    return forms;
  }

  function traceGlobalVariable(model, varName, options) {
    const startVar = String(varName || "").trim();
    if (!startVar) return { error: "Variable name is empty." };

    const globalDeclMap = buildGlobalDeclMap(model);
    const startLower = startVar.toLowerCase();
    const globalEntry = globalDeclMap.get(startLower) || null;
    const decl = globalEntry?.decl || null;

    const entrypoints = getProgramEntrypoints(model);
    const calls = [];

    for (const key of entrypoints) {
      const routine = model.nodes.get(key);
      if (!routine?.sourceRef) continue;
      const childResult = traceVariable(model, key, startVar, options);
      if (!childResult?.root) continue;

      const root = childResult.root;
      const hasChildCalls = (root.calls || []).length > 0;
      const hasOwnWrites = (root.writes || []).some((w) => {
        const ln = w?.sourceRef?.startLine ?? null;
        if (!ln) return false;
        const from = root.sourceRef?.startLine ?? null;
        if (!from) return false;
        const to = root.sourceRef?.endLine ?? from;
        return ln >= from && ln <= to;
      });

      if (!hasChildCalls && !hasOwnWrites) continue;

      calls.push({
        direction: "toCallee",
        edge: { sourceRef: routine.sourceRef || null },
        fromVar: startVar,
        toVar: startVar,
        mappingKind: "ENTRYPOINT",
        label: `Entrypoint ${childResult.root.routineKind} ${childResult.root.routineName}: ${startVar}`,
        child: childResult.root,
      });
    }

    const resolution = decl ? { scope: "global", decl } : { scope: "unknown" };

    return {
      startKey: "PROGRAM",
      varName: startVar,
      root: {
        routineKey: "PROGRAM",
        routineName: "PROGRAM (Globals)",
        routineKind: "PROGRAM",
        varName: startVar,
        resolution,
        sourceRef: decl?.sourceRef || null,
        writes: [],
        calls,
      },
    };
  }

  ns.lineage = { traceVariable, traceGlobalVariable, getVariablesForRoutine, getGlobalVariables, resolveSymbol };
})(window.AbapFlow);
