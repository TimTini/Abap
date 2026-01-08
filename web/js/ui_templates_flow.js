(function (ns) {
  "use strict";

  const ui = ns.ui;
  const tpl = ui.templates;
  const utils = ns.utils;

  function computeIfIndentByLine(model, routine) {
    const out = new Map();
    if (!utils?.collectStatements || !utils?.normalizeSpaces || !utils?.stripTrailingPeriod) return out;

    const lines = model?.lines;
    const src = routine?.sourceRef;
    if (!Array.isArray(lines) || !src?.startLine || !src?.endLine) return out;

    const statements = utils.collectStatements(lines, src.startLine, src.endLine);

    let level = 0;
    for (const st of statements) {
      const normalized = utils.normalizeSpaces(utils.stripTrailingPeriod(st.text));
      if (!normalized) continue;

      const u = normalized.toUpperCase();

      if (u === "ENDIF" || u === "END IF") {
        level = Math.max(0, level - 1);
        out.set(st.startLine, level);
        continue;
      }

      if (u.startsWith("ELSEIF ")) {
        level = Math.max(0, level - 1);
        out.set(st.startLine, level);
        level += 1;
        continue;
      }

      if (u === "ELSE") {
        level = Math.max(0, level - 1);
        out.set(st.startLine, level);
        level += 1;
        continue;
      }

      if (u.startsWith("IF ")) {
        out.set(st.startLine, level);
        level += 1;
        continue;
      }

      out.set(st.startLine, level);
    }

    return out;
  }

  function listTemplateEntries() {
    const templates =
      ns.templateRegistry?.templates && typeof ns.templateRegistry.templates === "object" ? ns.templateRegistry.templates : {};
    const order = Array.isArray(ns.templateRegistry?.order)
      ? ns.templateRegistry.order
      : Object.keys(templates).sort((a, b) => a.localeCompare(b));

    const out = [];
    for (const id of order) {
      const t = templates[id];
      if (!t || typeof t !== "object") continue;
      const entry = { ...t, id: String(t.id || id) };
      out.push(entry);
    }

    return out;
  }

  function pickAutoTemplatesBySource(entries) {
    const out = new Map();
    for (const e of entries || []) {
      if (!e || typeof e !== "object") continue;
      if (e.auto === false) continue;
      const source = String(e.source || "").trim();
      if (!source) continue;
      if (!out.has(source)) out.set(source, e);
    }
    return out;
  }

  function getProgramEntrypoints(model) {
    const nodes = Array.from(model?.nodes?.values ? model.nodes.values() : []);
    const byLine = (a, b) => (a?.sourceRef?.startLine ?? 1e9) - (b?.sourceRef?.startLine ?? 1e9);

    const events = nodes
      .filter((n) => n && n.kind === "EVENT")
      .slice()
      .sort(byLine)
      .map((n) => n.key);
    if (events.length) return events;

    const roots = nodes
      .filter((n) => n && n.kind === "FORM" && n.isDefined && (n.calledBy?.length || 0) === 0)
      .slice()
      .sort(byLine)
      .map((n) => n.key);
    if (roots.length) return roots;

    return nodes
      .filter((n) => n && n.kind === "FORM" && n.isDefined)
      .slice()
      .sort(byLine)
      .map((n) => n.key);
  }

  function extractSource(model, sourceRef) {
    if (!model || !Array.isArray(model.lines) || !sourceRef) return "";
    const start = Math.max(1, Math.floor(Number(sourceRef.startLine || 1)));
    const end = Math.min(model.lines.length, Math.floor(Number(sourceRef.endLine || start)));
    return model.lines.slice(start - 1, end).join("\n").trim();
  }

  function edgeIdFrom(edge) {
    const fromKey = String(edge?.fromKey || "");
    const toKey = String(edge?.toKey || "");
    const start = Math.max(0, Math.floor(Number(edge?.sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(edge?.sourceRef?.endLine || start)));
    return `${fromKey}->${toKey}@${start}-${end}`;
  }

  function assignmentIdFrom(routineKey, sourceRef) {
    const rk = String(routineKey || "").trim() || "ASSIGNMENT";
    const start = Math.max(0, Math.floor(Number(sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(sourceRef?.endLine || start)));
    return `${rk}@${start}-${end}`;
  }

  function ifIdFrom(routineKey, sourceRef) {
    const rk = String(routineKey || "").trim() || "IF";
    const start = Math.max(0, Math.floor(Number(sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(sourceRef?.endLine || start)));
    return `IF:${rk}@${start}-${end}`;
  }

  function routineTemplateSteps(model, routineKey, templatesBySource) {
    const routine = model?.nodes?.get ? model.nodes.get(routineKey) : null;
    if (!routine) return [];

    const steps = [];

    if (templatesBySource.has("assignments") && Array.isArray(routine.assignments)) {
      for (const a of routine.assignments) {
        if (!a?.sourceRef) continue;
        steps.push({ kind: "assignment", sourceRef: a.sourceRef, routineKey, routine, assignment: a });
      }
    }

    if (templatesBySource.has("ifs") && Array.isArray(routine.ifStatements)) {
      for (const st of routine.ifStatements) {
        if (!st?.sourceRef) continue;
        steps.push({ kind: "if", sourceRef: st.sourceRef, routineKey, routine, ifStatement: st });
      }
    }

    if (templatesBySource.has("performCalls") && Array.isArray(routine.calls)) {
      for (const edge of routine.calls) {
        const toKey = String(edge?.toKey || "");
        if (!toKey.toUpperCase().startsWith("FORM:")) continue;
        if (!edge?.sourceRef) continue;
        steps.push({ kind: "perform", sourceRef: edge.sourceRef, routineKey, routine, edge });
      }
    }

    steps.sort((a, b) => Number(a?.sourceRef?.startLine || 0) - Number(b?.sourceRef?.startLine || 0));
    return steps;
  }

  function buildTemplatesFlow(model, templatesBySource, options) {
    const maxSteps = Math.max(50, Number(options?.maxSteps || 900));
    const items = [];
    let truncated = false;

    const converter = ns.templateConverter;
    if (!converter) return { items: [], truncated: false };

    function convertPerform(edge, callPath) {
      const entry = templatesBySource.get("performCalls");
      if (!entry?.config) return null;
      const context = converter.buildPerformContext(model, edge, { callPath });
      const expanded = converter.expandExcelLikeTableTemplate(entry.config, context);
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(expanded, context));
      const resultId = edgeIdFrom(edge);
      tpl.applyOverridesToConfig(filled, tpl.getLocalTemplateOverrides(entry.id, resultId), { model, context, callPath });
      return {
        kind: "perform",
        templateId: String(entry.id || ""),
        resultId,
        edge,
        context,
        sourceRef: edge?.sourceRef || null,
        original: extractSource(model, edge?.sourceRef) || `PERFORM ${context.perform?.name || ""}`,
        filledConfig: filled,
      };
    }

    function convertAssignment(routine, assignment, callPath) {
      const entry = templatesBySource.get("assignments");
      if (!entry?.config) return null;
      const context = converter.buildAssignmentContext(model, routine, assignment, { callPath });
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(entry.config, context));
      const resultId = assignmentIdFrom(routine?.key, assignment?.sourceRef);
      tpl.applyOverridesToConfig(filled, tpl.getLocalTemplateOverrides(entry.id, resultId), { model, context, callPath });
      return {
        kind: "assignment",
        templateId: String(entry.id || ""),
        resultId,
        assignment,
        routineKey: String(routine?.key || ""),
        context,
        sourceRef: assignment?.sourceRef || null,
        original: extractSource(model, assignment?.sourceRef) || `${context.assignment?.lhs || ""} = ${context.assignment?.rhs || ""}`,
        filledConfig: filled,
      };
    }

    function convertIf(routine, ifStatement, callPath) {
      const entry = templatesBySource.get("ifs");
      if (!entry?.config) return null;
      if (!converter.buildIfContext) return null;

      const context = converter.buildIfContext(model, routine, ifStatement, { callPath });
      const expanded = converter.expandExcelLikeTableTemplate(entry.config, context);
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(expanded, context));
      const resultId = ifIdFrom(routine?.key, ifStatement?.sourceRef);
      tpl.applyOverridesToConfig(filled, tpl.getLocalTemplateOverrides(entry.id, resultId), { model, context, callPath });

      const k = String(context?.if?.kind || "IF").trim().toUpperCase() || "IF";
      const cond = String(context?.if?.condition || "").trim();
      const fallback = cond ? `${k} ${cond}.` : `${k}.`;

      return {
        kind: "if",
        templateId: String(entry.id || ""),
        resultId,
        ifStatement,
        routineKey: String(routine?.key || ""),
        context,
        sourceRef: ifStatement?.sourceRef || null,
        original: extractSource(model, ifStatement?.sourceRef) || fallback,
        filledConfig: filled,
      };
    }

    function walk(routineKey, depth, stack, callPath) {
      const routine = model?.nodes?.get ? model.nodes.get(routineKey) : null;
      if (!routine) return;

      const ifIndentByLine = computeIfIndentByLine(model, routine);

      if (items.length >= maxSteps) {
        truncated = true;
        return;
      }

      stack.add(routineKey);

      const steps = routineTemplateSteps(model, routineKey, templatesBySource);
      for (const step of steps) {
        if (items.length >= maxSteps) {
          truncated = true;
          break;
        }

        const indentWithinIf = Number(ifIndentByLine.get(step?.sourceRef?.startLine) || 0);
        const stepDepth = depth + Math.max(0, indentWithinIf);

        if (step.kind === "if") {
          const r = convertIf(step.routine, step.ifStatement, callPath);
          if (r) items.push({ kind: "template", depth: stepDepth, isRecursion: false, result: r });
          continue;
        }

        if (step.kind === "assignment") {
          const r = convertAssignment(step.routine, step.assignment, callPath);
          if (r) items.push({ kind: "template", depth: stepDepth, isRecursion: false, result: r });
          continue;
        }

        if (step.kind === "perform") {
          const isRecursion = stack.has(step.edge?.toKey);
          const r = convertPerform(step.edge, callPath);
          if (r) items.push({ kind: "template", depth: stepDepth, isRecursion, result: r });
          if (!isRecursion) walk(step.edge.toKey, stepDepth + 1, stack, (callPath || []).concat([step.edge]));
        }
      }

      stack.delete(routineKey);
    }

    const roots = getProgramEntrypoints(model);
    for (const rootKey of roots) {
      const root = model.nodes.get(rootKey);
      items.push({ kind: "separator", label: root ? `${root.kind} ${root.name}` : rootKey, rootKey });
      walk(rootKey, 0, new Set(), []);
      if (truncated) break;
    }

    if (truncated) {
      items.push({ kind: "note", text: `Truncated at ${maxSteps} steps.` });
    }

    return { items, truncated, maxSteps };
  }

  tpl.listTemplateEntries = listTemplateEntries;
  tpl.pickAutoTemplatesBySource = pickAutoTemplatesBySource;
  tpl.buildTemplatesFlow = buildTemplatesFlow;
})(window.AbapFlow);
