(function (ns) {
  "use strict";

  const ui = ns.ui;
  const utils = ns.utils;

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function safeJsonStringify(value) {
    try {
      return JSON.stringify(value, null, 2);
    } catch (e) {
      return String(e?.message || e);
    }
  }

  function bindOnce() {
    const btn = ui?.$("abapObjCfgGenBtn");
    if (!btn || btn.dataset.bound) return;
    btn.dataset.bound = "1";

    btn.addEventListener("click", () => {
      const gen = ns.abapObjects?.syntaxGenerator;
      const store = ns.abapObjects?.customStore;
      if (!gen?.generateObjectDefFromSyntaxText) {
        ui.setStatus("Syntax generator module not loaded.", true);
        return;
      }
      if (!store?.upsertObjectDef) {
        ui.setStatus("ABAP Objects custom store module not loaded.", true);
        return;
      }

      const input = ui.$("abapObjCfgGenInput")?.value || "";
      const res = gen.generateObjectDefFromSyntaxText(input);
      const out = ui.$("abapObjCfgGenOutput");

      if (!res?.ok) {
        if (out) out.textContent = String(res?.error || "Generate failed.");
        ui.setStatus(String(res?.error || "Generate failed."), true);
        return;
      }

      const idOverride = asNonEmptyString(ui.$("abapObjCfgGenId")?.value);
      const objectDef = res.objectDef || {};
      const id = asNonEmptyString(idOverride) || asNonEmptyString(objectDef.id) || asNonEmptyString(res.keyword)?.toLowerCase();
      if (!id) {
        ui.setStatus("Object id is required.", true);
        return;
      }

      objectDef.id = id;
      if (!asNonEmptyString(objectDef.label)) objectDef.label = asNonEmptyString(res.keyword) || id;
      if (!asNonEmptyString(objectDef.kind)) objectDef.kind = "statement";

      const effective = store.getEffectiveConfig ? store.getEffectiveConfig(ns.abapObjectsMasterConfig)?.config : null;
      const existingObj = Array.isArray(effective?.objects)
        ? effective.objects.find((o) => String(o?.id || "").trim().toLowerCase() === id.toLowerCase()) || null
        : null;
      const existing = Boolean(existingObj);
      if (existing) {
        const ok = window.confirm(`"${id}" already exists. Override it with generated config?`);
        if (!ok) return;
      }

      if (existingObj && Array.isArray(existingObj.templates) && existingObj.templates.length && (!Array.isArray(objectDef.templates) || objectDef.templates.length === 0)) {
        objectDef.templates = JSON.parse(JSON.stringify(existingObj.templates));
      }

      if (!Array.isArray(objectDef.templates)) objectDef.templates = [];
      if (objectDef.templates.length === 0) {
        const genTpl = ns.abapObjects?.syntaxGenerator;
        const defs = ns.templateDefs;
        if (genTpl?.createDefaultKeyValueExcelLikeTableTemplate && defs?.upsertTemplate) {
          const templateId = `${id}.default.excel-like-table`;

          objectDef.templates.push({ id: templateId, label: "Default (key/value)", auto: true });

          if (!defs.getTemplateConfig || !defs.getTemplateConfig(templateId)) {
            const cfg = genTpl.createDefaultKeyValueExcelLikeTableTemplate("pairs");
            defs.upsertTemplate(templateId, cfg, { label: "Default (key/value)", source: id, auto: true });
          }
        }
      }

      const saved = store.upsertObjectDef(objectDef);
      if (!saved.ok) {
        const msg = saved.error || "Save failed.";
        if (out) out.textContent = msg;
        ui.setStatus(msg, true);
        return;
      }

      if (out) out.textContent = safeJsonStringify(objectDef);
      ui.setStatus(`Generated object: ${id}.`, false);

      if (ns.abapObjects?.reset) ns.abapObjects.reset();

      if (ui.selectAbapObjectConfig) {
        ui.selectAbapObjectConfig(id);
      } else {
        ui.setActiveTab?.("config");
      }

      const code = ui.$("abapInput")?.value || "";
      if (String(code).trim()) ui.$("btnAnalyze")?.click();
    });

    const input = ui.$("abapObjCfgGenInput");
    if (input) {
      input.addEventListener("input", () => {
        const gen = ns.abapObjects?.syntaxGenerator;
        const out = ui.$("abapObjCfgGenOutput");
        if (!gen?.extractFirstSyntaxStatement || !out) return;
        const stmt = gen.extractFirstSyntaxStatement(input.value);
        if (!stmt) {
          out.textContent = "";
          return;
        }
        const m = /^([A-Z][A-Z0-9_-]*)\\b/.exec(stmt);
        const keyword = asNonEmptyString(m?.[1]);
        if (keyword) {
          const idEl = ui.$("abapObjCfgGenId");
          if (idEl && !asNonEmptyString(idEl.value)) idEl.value = keyword.toLowerCase();
        }
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bindOnce);
  } else {
    bindOnce();
  }

  ns.ui = ns.ui || {};
  ns.ui.initAbapObjectsSyntaxGenerator = bindOnce;
})(window.AbapFlow);
