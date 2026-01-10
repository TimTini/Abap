(function (ns) {
  "use strict";

  const utils = ns.utils;

  const STORAGE_KEY = "abapflow-abap-objects-custom";
  const SCHEMA = "abapflow-abap-objects-custom";
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

  function deepClone(value) {
    if (value == null) return value;

    if (value instanceof RegExp) {
      return new RegExp(value.source, value.flags);
    }

    if (Array.isArray(value)) return value.map((x) => deepClone(x));

    if (typeof value === "object") {
      const out = {};
      for (const [k, v] of Object.entries(value)) out[k] = deepClone(v);
      return out;
    }

    return value;
  }

  function normalizeStore(value) {
    const out = {
      schema: SCHEMA,
      version: VERSION,
      updatedAt: nowIso(),
      objects: {},
      disabled: {},
    };

    if (!isPlainObject(value)) return out;
    if (asNonEmptyString(value.schema) !== SCHEMA) return out;
    const v = Number(value.version);
    if (!Number.isFinite(v) || v <= 0) return out;

    const objects = isPlainObject(value.objects) ? value.objects : {};
    for (const [keyRaw, obj] of Object.entries(objects)) {
      if (!isPlainObject(obj)) continue;
      const id = asNonEmptyString(obj.id) || asNonEmptyString(keyRaw);
      if (!id) continue;
      out.objects[id] = { ...obj, id };
    }

    const disabled = isPlainObject(value.disabled) ? value.disabled : {};
    for (const [keyRaw, flag] of Object.entries(disabled)) {
      const id = asNonEmptyString(keyRaw);
      if (!id) continue;
      if (flag) out.disabled[id] = true;
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

  function clearStore() {
    try {
      localStorage.removeItem(STORAGE_KEY);
    } catch (_) {}
  }

  function upsertObjectDef(objectDef) {
    const obj = isPlainObject(objectDef) ? objectDef : null;
    const id = asNonEmptyString(obj?.id);
    if (!id) return { ok: false, error: "Object id is required." };

    const store = loadStore();
    store.objects[id] = deepClone({ ...obj, id });
    const saved = saveStore(store);
    return { ok: true, store: saved };
  }

  function deleteObjectDef(objectId) {
    const id = asNonEmptyString(objectId);
    if (!id) return { ok: false, error: "Object id is required." };

    const store = loadStore();
    const existed = Boolean(store.objects?.[id]);
    if (existed) delete store.objects[id];
    const saved = saveStore(store);
    return { ok: true, existed, store: saved };
  }

  function setObjectDisabled(objectId, disabled) {
    const id = asNonEmptyString(objectId);
    if (!id) return { ok: false, error: "Object id is required." };

    const store = loadStore();
    if (disabled) store.disabled[id] = true;
    else delete store.disabled[id];
    const saved = saveStore(store);
    return { ok: true, store: saved };
  }

  function exportJson() {
    return JSON.stringify(loadStore(), null, 2);
  }

  function importJson(jsonText, options) {
    const mode = asNonEmptyString(options?.mode).toLowerCase() || "merge";
    const parsed = safeParse(jsonText);
    if (!parsed.ok) return { ok: false, error: parsed.error || "Invalid JSON." };
    const incoming = normalizeStore(parsed.value);

    if (mode === "replace") {
      const saved = saveStore(incoming);
      return { ok: true, store: saved };
    }

    const store = loadStore();
    for (const [id, obj] of Object.entries(incoming.objects || {})) {
      store.objects[id] = deepClone(obj);
    }
    for (const [id, flag] of Object.entries(incoming.disabled || {})) {
      if (flag) store.disabled[id] = true;
    }

    const saved = saveStore(store);
    return { ok: true, store: saved };
  }

  function getEffectiveConfig(masterConfig) {
    const base = isPlainObject(masterConfig) ? masterConfig : {};
    const store = loadStore();

    const out = deepClone(base);
    const baseObjects = Array.isArray(out.objects) ? out.objects : [];

    const byLower = new Map();
    for (let i = 0; i < baseObjects.length; i++) {
      const id = asNonEmptyString(baseObjects[i]?.id);
      if (!id) continue;
      const lower = id.toLowerCase();
      if (!byLower.has(lower)) byLower.set(lower, i);
    }

    const custom = [];
    for (const [id, obj] of Object.entries(store.objects || {})) {
      const oid = asNonEmptyString(obj?.id) || asNonEmptyString(id);
      if (!oid) continue;
      const lower = oid.toLowerCase();
      const cloned = deepClone({ ...(obj || {}), id: oid });
      if (byLower.has(lower)) baseObjects[byLower.get(lower)] = cloned;
      else custom.push(cloned);
    }

    custom.sort((a, b) => asNonEmptyString(a?.id).localeCompare(asNonEmptyString(b?.id)));

    const disabledSet = new Set(
      Object.entries(store.disabled || {})
        .filter(([, flag]) => Boolean(flag))
        .map(([id]) => String(id).trim().toLowerCase())
        .filter(Boolean),
    );

    out.objects = baseObjects
      .concat(custom)
      .filter((obj) => !disabledSet.has(asNonEmptyString(obj?.id).toLowerCase()));

    return { config: out, store };
  }

  ns.abapObjects = ns.abapObjects || {};
  ns.abapObjects.customStore = {
    STORAGE_KEY,
    loadStore,
    saveStore,
    clearStore,
    upsertObjectDef,
    deleteObjectDef,
    setObjectDisabled,
    exportJson,
    importJson,
    getEffectiveConfig,
  };
})(window.AbapFlow);

