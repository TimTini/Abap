(function (ns) {
  "use strict";

  const utils = ns.utils;

  function buildGlobalIndex(model) {
    const set = new Set();
    for (const d of model.globalData) set.add(String(d.variableName || "").toLowerCase());
    for (const c of model.globalConstants) set.add(String(c.variableName || "").toLowerCase());
    return set;
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
    const callerNameLower = String(callerVarName || "").toLowerCase();
    const callee = model.nodes.get(edge.toKey);
    if (!callee) return null;

    const rules = [
      { edgeKey: "changing", paramKind: "CHANGING" },
      { edgeKey: "tables", paramKind: "TABLES" },
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

    const startVar = String(varName || "").trim();
    if (!startVar) return { error: "Variable name is empty." };

    const startRoutine = model.nodes.get(startKey);
    if (!startRoutine) return { error: "Subroutine/event not found." };

    const stack = new Set();
    let visitedCount = 0;

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
        let mapping = null;
        if (isGlobal) {
          mapping = { mappedVarName: currentVarName, kind: "GLOBAL" };
        } else {
          mapping = mapThroughEdge(model, edge, currentVarName);
        }

        if (!mapping) continue;
        const child = walk(edge.toKey, mapping.mappedVarName);
        if (!child) continue;
        calls.push({
          direction: "toCallee",
          edge,
          fromVar: currentVarName,
          toVar: mapping.mappedVarName,
          mappingKind: mapping.kind,
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

  function getVariablesForRoutine(model, routineKey) {
    const routine = model.nodes.get(routineKey);
    if (!routine) return [];

    const vars = [];
    for (const p of routine.params) vars.push({ name: p.name, scope: `PARAM:${p.kind}`, dataType: p.dataType, description: p.description });
    for (const d of routine.localData) vars.push({ name: d.variableName, scope: "LOCAL:DATA", dataType: d.dataType, description: d.description });
    for (const c of routine.localConstants)
      vars.push({ name: c.variableName, scope: "LOCAL:CONSTANTS", dataType: c.dataType, description: c.description });

    for (const d of model.globalData) vars.push({ name: d.variableName, scope: "GLOBAL:DATA", dataType: d.dataType, description: d.description });
    for (const c of model.globalConstants) vars.push({ name: c.variableName, scope: "GLOBAL:CONSTANTS", dataType: c.dataType, description: c.description });

    return vars;
  }

  ns.lineage = { traceVariable, getVariablesForRoutine, resolveSymbol };
})(window.AbapFlow);
