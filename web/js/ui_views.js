(function (ns) {
  "use strict";

  const ui = ns.ui;
  const state = ui.state;

  function renderJson() {
    const out = ui.$("jsonOutput");
    const model = state.model;
    if (!out) return;
    if (!model) {
      out.textContent = "Analyze to see JSON.";
      out.classList.add("empty");
      return;
    }
    out.classList.remove("empty");
    out.textContent = JSON.stringify(model.serialize(), null, 2);
  }

  function buildAllTemplatesObjectsExportXml(options) {
    const model = state.model;
    if (!model) return { ok: false, error: "Analyze first." };

    const tpl = ui.templates || null;
    if (!tpl?.listTemplateEntries || !tpl?.pickAutoTemplatesBySource || !tpl?.buildTemplatesFlow) {
      return { ok: false, error: "Template modules not loaded." };
    }

    const maxStepsRaw = Number(options?.maxSteps ?? ui.$("xmlMaxSteps")?.value ?? 5000);
    const maxSteps = Number.isFinite(maxStepsRaw) && maxStepsRaw > 0 ? Math.floor(maxStepsRaw) : 5000;

    const entries = tpl.listTemplateEntries();
    const templatesBySource = tpl.pickAutoTemplatesBySource(entries);
    if (!templatesBySource || typeof templatesBySource.size !== "number" || templatesBySource.size === 0) {
      return { ok: false, error: "No templates configured." };
    }

    const flow = tpl.buildTemplatesFlow(model, templatesBySource, { maxSteps });
    const items = [];

    for (const it of flow?.items || []) {
      if (!it || it.kind !== "template") continue;
      const r = it.result || null;
      if (!r) continue;

      const depthRaw = Number(it.depth ?? 0);
      const depth = Number.isFinite(depthRaw) ? Math.max(0, Math.floor(depthRaw)) : 0;
      items.push({
        kind: String(r.kind || ""),
        resultId: String(r.resultId || ""),
        objectId: String(r.objectId || ""),
        templateId: String(r.templateId || ""),
        routineKey: String(r.routineKey || ""),
        sourceRef: r.sourceRef || null,
        original: String(r.original || ""),
        context: r.context || null,
        depth,
      });
    }

    if (!items.length) return { ok: false, error: "No template-mapped statements found." };

    const build = typeof ui.buildAbapflowObjectsXml === "function" ? ui.buildAbapflowObjectsXml : null;
    if (!build) return { ok: false, error: "XML exporter not loaded." };

    const xml = build(items);
    return { ok: true, xml, count: items.length, truncated: Boolean(flow?.truncated), maxSteps };
  }

  function renderXml(options) {
    const out = ui.$("xmlOutput");
    if (!out) return;

    if (!state.model) {
      out.textContent = "Analyze to see XML.";
      out.classList.add("empty");
      return;
    }

    const res = buildAllTemplatesObjectsExportXml(options);
    if (!res.ok) {
      out.textContent = String(res.error || "Cannot build XML.");
      out.classList.add("empty");
      return;
    }

    out.classList.remove("empty");
    out.textContent = res.xml;
  }

  ui.renderJson = renderJson;
  ui.buildAllTemplatesObjectsExportXml = buildAllTemplatesObjectsExportXml;
  ui.renderXml = renderXml;
})(window.AbapFlow);
