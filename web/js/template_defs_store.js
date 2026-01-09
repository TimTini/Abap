(function (ns) {
  "use strict";

  const utils = ns.utils;

  const STORAGE_KEY = "abapflow-template-defs";
  const SCHEMA = "abapflow-template-defs";
  const VERSION = 1;

  function nowIso() {
    if (utils && typeof utils.nowIso === "function") return utils.nowIso();
    return new Date().toISOString();
  }

  function safeParse(jsonText) {
    if (utils && typeof utils.safeJsonParse === "function") return utils.safeJsonParse(jsonText);
    try {
      return { ok: true, value: JSON.parse(String(jsonText || "")) };
    } catch (e) {
      return { ok: false, error: String(e?.message || e) };
    }
  }

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function isPlainObject(x) {
    return Boolean(x) && typeof x === "object" && !Array.isArray(x);
  }

  function normalizeWhen(when) {
    if (!isPlainObject(when)) return null;
    const path = asNonEmptyString(when.path);
    const equals = asNonEmptyString(when.equals);
    if (!path || !equals) return null;
    return { path, equals };
  }

  function normalizeTemplateMeta(meta) {
    if (!isPlainObject(meta)) return {};
    const label = asNonEmptyString(meta.label);
    const source = asNonEmptyString(meta.source);
    const auto = meta.auto != null ? Boolean(meta.auto) : true;
    const custom = Boolean(meta.custom);
    const when = normalizeWhen(meta.when);
    return {
      ...(label ? { label } : {}),
      ...(source ? { source } : {}),
      auto,
      custom,
      ...(when ? { when } : {}),
    };
  }

  function normalizeStore(value) {
    const out = {
      schema: SCHEMA,
      version: VERSION,
      updatedAt: nowIso(),
      templates: {},
      preferredBySource: {},
    };

    if (!isPlainObject(value)) return out;

    const schema = asNonEmptyString(value.schema);
    const version = Number(value.version);
    if (schema !== SCHEMA || !Number.isFinite(version) || version <= 0) return out;

    const templates = isPlainObject(value.templates) ? value.templates : {};
    for (const [idRaw, entry] of Object.entries(templates)) {
      const id = asNonEmptyString(idRaw);
      if (!id) continue;
      if (!isPlainObject(entry)) continue;
      const config = entry.config;
      if (!isPlainObject(config)) continue;
      const meta = normalizeTemplateMeta(entry.meta);
      out.templates[id] = { updatedAt: asNonEmptyString(entry.updatedAt) || nowIso(), config, meta };
    }

    const pref = isPlainObject(value.preferredBySource) ? value.preferredBySource : {};
    for (const [srcRaw, tidRaw] of Object.entries(pref)) {
      const src = asNonEmptyString(srcRaw);
      const tid = asNonEmptyString(tidRaw);
      if (!src || !tid) continue;
      out.preferredBySource[src] = tid;
    }

    out.updatedAt = asNonEmptyString(value.updatedAt) || out.updatedAt;
    return out;
  }

  function loadStore() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (!raw) return normalizeStore(null);
      const parsed = safeParse(raw);
      if (!parsed.ok) return normalizeStore(null);
      return normalizeStore(parsed.value);
    } catch (_) {
      return normalizeStore(null);
    }
  }

  function saveStore(store) {
    const normalized = normalizeStore(store);
    normalized.updatedAt = nowIso();
    localStorage.setItem(STORAGE_KEY, JSON.stringify(normalized));
    return normalized;
  }

  function getTemplateConfig(templateId) {
    const id = asNonEmptyString(templateId);
    if (!id) return null;
    const store = loadStore();
    return store.templates?.[id]?.config || null;
  }

  function upsertTemplate(templateId, config, meta) {
    const id = asNonEmptyString(templateId);
    if (!id) return { ok: false, error: "Template id is required." };
    if (!isPlainObject(config)) return { ok: false, error: "Template config must be an object." };

    const store = loadStore();
    const nextMeta = normalizeTemplateMeta(meta);
    const existing = store.templates?.[id];

    store.templates[id] = {
      updatedAt: nowIso(),
      config,
      meta: { ...(existing?.meta || {}), ...nextMeta },
    };

    const saved = saveStore(store);
    return { ok: true, store: saved };
  }

  function deleteTemplate(templateId) {
    const id = asNonEmptyString(templateId);
    if (!id) return { ok: false, error: "Template id is required." };

    const store = loadStore();
    const existed = Boolean(store.templates?.[id]);
    if (existed) delete store.templates[id];

    for (const [src, tid] of Object.entries(store.preferredBySource || {})) {
      if (String(tid || "") === id) delete store.preferredBySource[src];
    }

    const saved = saveStore(store);
    return { ok: true, existed, store: saved };
  }

  function listCustomTemplateEntries() {
    const store = loadStore();
    const out = [];

    for (const [id, entry] of Object.entries(store.templates || {})) {
      if (!entry || typeof entry !== "object") continue;
      const meta = entry.meta || {};
      if (!meta.custom) continue;
      const source = asNonEmptyString(meta.source);
      if (!source) continue;
      const label = asNonEmptyString(meta.label) || id;
      out.push({
        id,
        label,
        source,
        objectId: source,
        auto: meta.auto !== false,
        when: meta.when || null,
        config: entry.config,
      });
    }

    out.sort((a, b) => a.id.localeCompare(b.id));
    return out;
  }

  function getPreferredTemplateId(sourceId) {
    const src = asNonEmptyString(sourceId);
    if (!src) return "";
    const store = loadStore();
    return asNonEmptyString(store.preferredBySource?.[src]);
  }

  function setPreferredTemplateId(sourceId, templateId) {
    const src = asNonEmptyString(sourceId);
    const tid = asNonEmptyString(templateId);
    if (!src) return { ok: false, error: "Source id is required." };

    const store = loadStore();
    if (tid) store.preferredBySource[src] = tid;
    else delete store.preferredBySource[src];

    const saved = saveStore(store);
    return { ok: true, store: saved };
  }

  ns.templateDefs = {
    STORAGE_KEY,
    loadStore,
    saveStore,
    getTemplateConfig,
    upsertTemplate,
    deleteTemplate,
    listCustomTemplateEntries,
    getPreferredTemplateId,
    setPreferredTemplateId,
  };
})(window.AbapFlow);

