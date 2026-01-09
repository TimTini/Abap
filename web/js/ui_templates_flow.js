(function (ns) {
  "use strict";

  const ui = ns.ui;
  const tpl = ui.templates;
  const utils = ns.utils;
  const extractSource =
    typeof utils?.extractSource === "function"
      ? utils.extractSource
      : (model, sourceRef) => {
          if (!model || !Array.isArray(model.lines) || !sourceRef) return "";
          const start = Math.max(1, Math.floor(Number(sourceRef.startLine || 1)));
          const end = Math.min(model.lines.length, Math.floor(Number(sourceRef.endLine || start)));
          return model.lines.slice(start - 1, end).join("\n").trim();
        };

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
      if (!out.has(source)) out.set(source, []);
      out.get(source).push(e);
    }
    return out;
  }

  function resolvePath(obj, path) {
    const raw = String(path || "").trim();
    if (!raw) return undefined;

    let cur = obj;
    const parts = raw
      .split(".")
      .map((p) => p.trim())
      .filter(Boolean);

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

  function templateWhenMatches(entry, context) {
    const when = entry?.when;
    if (!when) return true;
    if (typeof when !== "object") return true;

    const path = String(when.path || "").trim();
    if (!path) return true;

    const actual = resolvePath(context, path);
    const expected = when.equals;
    if (expected == null) return Boolean(actual);

    return String(actual ?? "")
      .trim()
      .toUpperCase() === String(expected ?? "")
      .trim()
      .toUpperCase();
  }

  function pickTemplateEntry(entries, context, sourceId) {
    const list = Array.isArray(entries) ? entries : [];
    if (!list.length) return null;

    const preferredId =
      ns.templateDefs && typeof ns.templateDefs.getPreferredTemplateId === "function" ? ns.templateDefs.getPreferredTemplateId(sourceId) : "";
    if (preferredId) {
      const preferred = list.find((e) => String(e?.id || "").trim() === preferredId) || null;
      if (preferred && templateWhenMatches(preferred, context)) return preferred;
    }

    for (const e of list) {
      if (templateWhenMatches(e, context)) return e;
    }
    return list[0] || null;
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

  function edgeIdFrom(edge) {
    const fromKey = String(edge?.fromKey || "");
    const toKey = String(edge?.toKey || "");
    const start = Math.max(0, Math.floor(Number(edge?.sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(edge?.sourceRef?.endLine || start)));
    return `${fromKey}->${toKey}@${start}-${end}`;
  }

  function statementIdFrom(objectId, routineKey, sourceRef) {
    const ok = String(objectId || "").trim() || "ST";
    const rk = String(routineKey || "").trim() || "ROUTINE";
    const start = Math.max(0, Math.floor(Number(sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(sourceRef?.endLine || start)));

    if (ok === "assignment") return `${rk}@${start}-${end}`;
    if (ok === "if") return `IF:${rk}@${start}-${end}`;
    if (ok === "message") return `MSG:${rk}@${start}-${end}`;
    if (ok === "itabOp") return `ITAB:${rk}@${start}-${end}`;

    return `ST:${ok}:${rk}@${start}-${end}`;
  }

  function routineTemplateSteps(model, routineKey, templatesBySource) {
    const routine = model?.nodes?.get ? model.nodes.get(routineKey) : null;
    if (!routine) return [];

    const steps = [];

    if (Array.isArray(routine.statementItems)) {
      for (const item of routine.statementItems) {
        const objectId = String(item?.objectId || "").trim();
        if (!objectId) continue;
        if (!templatesBySource.has(objectId)) continue;
        const src = item?.sourceRef || null;
        if (!src) continue;
        steps.push({ kind: "statement", objectId, sourceRef: src, routineKey, routine, item });
      }
    }

    const registry = ns.abapObjects?.getRegistry?.() || null;
    if (registry && Array.isArray(routine.calls)) {
      for (const edge of routine.calls) {
        if (!edge?.sourceRef) continue;
        const objDef = registry.matchCallEdge?.(edge) || null;
        const objectId = String(objDef?.id || "").trim();
        if (!objectId) continue;
        if (!templatesBySource.has(objectId)) continue;
        steps.push({ kind: "callEdge", objectId, sourceRef: edge.sourceRef, routineKey, routine, edge, objDef });
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
    const registry = ns.abapObjects?.getRegistry?.() || null;
    if (!registry) return { items: [], truncated: false };

    function convertCallEdge(step, callPath) {
      const objEntries = templatesBySource.get(step.objectId);
      if (!Array.isArray(objEntries) || objEntries.length === 0) return null;

      const ctx = registry.buildContext(step.objDef, model, step.routine, step.edge, { callPath });
      if (!ctx) return null;

      const entry = pickTemplateEntry(objEntries, ctx, step.objectId);
      if (!entry?.config) return null;

      const expanded = converter.expandExcelLikeTableTemplate(entry.config, ctx);
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(expanded, ctx));
      const resultId = edgeIdFrom(step.edge);
      tpl.applyOverridesToConfig(filled, tpl.getLocalTemplateOverrides(entry.id, resultId), { model, context: ctx, callPath });

      return {
        kind: String(step.objectId || "callEdge"),
        objectId: String(step.objectId || ""),
        templateId: String(entry.id || ""),
        resultId,
        routineKey: String(step.routineKey || ""),
        edge: step.edge,
        context: ctx,
        sourceRef: step.edge?.sourceRef || null,
        original: extractSource(model, step.edge?.sourceRef) || `CALL ${String(step.edge?.targetName || "").trim()}`,
        filledConfig: filled,
      };
    }

    function convertStatement(step, callPath) {
      const objEntries = templatesBySource.get(step.objectId);
      if (!Array.isArray(objEntries) || objEntries.length === 0) return null;

      const objDef = registry.objectsById?.get ? registry.objectsById.get(step.objectId) : null;
      if (!objDef) return null;

      const ctx = registry.buildContext(objDef, model, step.routine, step.item?.payload || null, { callPath });
      if (!ctx) return null;

      const entry = pickTemplateEntry(objEntries, ctx, step.objectId);
      if (!entry?.config) return null;

      const expanded = converter.expandExcelLikeTableTemplate(entry.config, ctx);
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(expanded, ctx));
      const resultId = statementIdFrom(step.objectId, step.routineKey, step.item?.sourceRef);
      tpl.applyOverridesToConfig(filled, tpl.getLocalTemplateOverrides(entry.id, resultId), { model, context: ctx, callPath });

      const fallback = String(step.item?.payload?.statement || "").trim() || String(objDef.label || step.objectId);

      return {
        kind: String(step.objectId || "statement"),
        objectId: String(step.objectId || ""),
        templateId: String(entry.id || ""),
        resultId,
        routineKey: String(step.routineKey || ""),
        item: step.item,
        context: ctx,
        sourceRef: step.item?.sourceRef || null,
        original: extractSource(model, step.item?.sourceRef) || fallback,
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

        if (step.kind === "statement") {
          const r = convertStatement(step, callPath);
          if (r) items.push({ kind: "template", depth: stepDepth, isRecursion: false, result: r });
          continue;
        }

        if (step.kind === "callEdge") {
          const isRecursion = stack.has(step.edge?.toKey);
          const r = convertCallEdge(step, callPath);
          if (r) items.push({ kind: "template", depth: stepDepth, isRecursion, result: r });
          if (!isRecursion && step.edge?.toKey) {
            walk(step.edge.toKey, stepDepth + 1, stack, (callPath || []).concat([step.edge]));
          }
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
