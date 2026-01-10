(function (ns) {
  "use strict";

  const ui = ns.ui;
  ui.templates = ui.templates || {};
  const tpl = ui.templates;
  const utils = ns.utils;

  const TEMPLATE_OVERRIDES_STORAGE_KEY = "abapFlow.templateLocalOverrides.v1";
  const TEMPLATE_OVERRIDES_SCHEMA = "abapflow-template-local-overrides";
  const TEMPLATE_OVERRIDES_VERSION = 1;

  let templateLocalOverrides = loadTemplateLocalOverrides();

  const nowIso = typeof utils?.nowIso === "function" ? utils.nowIso : () => new Date().toISOString();
  const safeJsonParse =
    typeof utils?.safeJsonParse === "function"
      ? utils.safeJsonParse
      : (text) => {
          try {
            return { ok: true, value: JSON.parse(text) };
          } catch (err) {
            return { ok: false, error: String(err?.message || err) };
          }
        };

  function getActiveProgramId() {
    if (ns.notes?.getActiveProgramId) return ns.notes.getActiveProgramId();
    return "default";
  }

  function loadTemplateLocalOverrides() {
    try {
      const raw = localStorage.getItem(TEMPLATE_OVERRIDES_STORAGE_KEY);
      if (!raw) return new Map();
      const parsed = safeJsonParse(raw);
      if (!parsed.ok) return new Map();
      const obj = parsed.value;
      if (obj?.schema !== TEMPLATE_OVERRIDES_SCHEMA || obj?.version !== TEMPLATE_OVERRIDES_VERSION) return new Map();

      const out = new Map();
      const overrides = obj?.overrides && typeof obj.overrides === "object" ? obj.overrides : {};
      for (const [key, binds] of Object.entries(overrides)) {
        if (!binds || typeof binds !== "object") continue;
        const bindMap = new Map();
        for (const [bind, value] of Object.entries(binds)) {
          const b = String(bind || "").trim();
          if (!b) continue;

          if (typeof value === "string") {
            const v = String(value ?? "");
            if (!v.trim()) continue;
            bindMap.set(b, v);
            continue;
          }

          if (!value || typeof value !== "object" || Array.isArray(value)) continue;

          const rawByKey = value.byKey && typeof value.byKey === "object" && !Array.isArray(value.byKey) ? value.byKey : value;
          const byKey = new Map();
          for (const [originKeyRaw, ov] of Object.entries(rawByKey)) {
            const originKey = String(originKeyRaw || "").trim();
            if (!originKey) continue;
            const v = String(ov ?? "");
            if (!v.trim()) continue;
            byKey.set(originKey, v);
          }
          if (byKey.size) bindMap.set(b, byKey);
        }
        if (bindMap.size) out.set(String(key), bindMap);
      }
      return out;
    } catch (_) {
      return new Map();
    }
  }

  function saveTemplateLocalOverrides() {
    try {
      const obj = {
        schema: TEMPLATE_OVERRIDES_SCHEMA,
        version: TEMPLATE_OVERRIDES_VERSION,
        updatedAt: nowIso(),
        overrides: {},
      };

      for (const [key, bindMap] of templateLocalOverrides.entries()) {
        const bucket = {};
        for (const [bind, value] of bindMap.entries()) {
          const b = String(bind || "").trim();
          if (!b) continue;

          if (typeof value === "string") {
            const v = String(value ?? "");
            if (!v.trim()) continue;
            bucket[b] = v;
            continue;
          }

          if (value instanceof Map) {
            const byKey = {};
            for (const [originKey, ov] of value.entries()) {
              const ok = String(originKey || "").trim();
              if (!ok) continue;
              const v = String(ov ?? "");
              if (!v.trim()) continue;
              byKey[ok] = v;
            }
            if (Object.keys(byKey).length) bucket[b] = { byKey };
          }
        }
        if (Object.keys(bucket).length) obj.overrides[String(key)] = bucket;
      }

      localStorage.setItem(TEMPLATE_OVERRIDES_STORAGE_KEY, JSON.stringify(obj, null, 2));
    } catch (_) {
      // ignore
    }
  }

  function templateBucketKey(templateId, resultId) {
    const pid = String(getActiveProgramId() || "default");
    const tid = String(templateId || "").trim() || "default";
    const rid = String(resultId || "").trim();
    return `${pid}::${tid}::${rid}`;
  }

  function getLocalTemplateOverrides(templateId, resultId) {
    const key = templateBucketKey(templateId, resultId);
    return templateLocalOverrides.get(key) || null;
  }

  function setLocalTemplateOverride(templateId, resultId, bind, value) {
    const key = templateBucketKey(templateId, resultId);
    const b = String(bind || "").trim();
    if (!b) return;

    const v = String(value ?? "");
    const hasValue = Boolean(v.trim());

    let bucket = templateLocalOverrides.get(key);
    if (!bucket && hasValue) {
      bucket = new Map();
      templateLocalOverrides.set(key, bucket);
    }
    if (!bucket) return;

    bucket.set(b, v);
    if (!hasValue) bucket.delete(b);

    if (bucket.size === 0) templateLocalOverrides.delete(key);
    saveTemplateLocalOverrides();
  }

  function setLocalTemplateOverrideByOriginKey(templateId, resultId, bind, originKey, value, baseOriginKey) {
    const key = templateBucketKey(templateId, resultId);
    const b = String(bind || "").trim();
    const ok = String(originKey || "").trim();
    if (!b || !ok) return;

    const v = String(value ?? "");
    const hasValue = Boolean(v.trim());

    let bucket = templateLocalOverrides.get(key);
    if (!bucket && hasValue) {
      bucket = new Map();
      templateLocalOverrides.set(key, bucket);
    }
    if (!bucket) return;

    const baseKey = String(baseOriginKey || "").trim();

    const cur = bucket.get(b);
    let map = null;

    if (cur instanceof Map) {
      map = cur;
    } else if (typeof cur === "string") {
      map = new Map();
      const legacy = String(cur ?? "");
      if (legacy.trim()) {
        map.set(baseKey || ok, legacy);
      }
    } else {
      map = new Map();
    }

    if (hasValue) map.set(ok, v);
    else map.delete(ok);

    if (map.size) bucket.set(b, map);
    else bucket.delete(b);

    if (bucket.size === 0) templateLocalOverrides.delete(key);
    saveTemplateLocalOverrides();
  }

  function resolveBindPath(obj, path) {
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

  function applyOverridesToConfig(config, overridesMap, meta) {
    if (!config || !Array.isArray(config.cells) || !overridesMap) return;

    const model = meta?.model || null;
    const context = meta?.context || null;
    const callPath = Array.isArray(meta?.callPath) ? meta.callPath : null;

    const desc = ns.desc;

    for (const cell of config.cells) {
      const bind = String(cell?.bind || "").trim();
      if (!bind) continue;
      if (!overridesMap.has(bind)) continue;

      const overrideEntry = overridesMap.get(bind);
      const overrideValue = typeof overrideEntry === "string" ? String(overrideEntry ?? "") : "";

      if (
        model &&
        context &&
        desc &&
        typeof desc.describeExpression === "function" &&
        bind.endsWith(".description") &&
        bind.length > ".description".length
      ) {
        const basePath = bind.slice(0, -".description".length);
        const base = resolveBindPath(context, basePath);
        const originKey = String(base?.originKey || "").trim();
        const exprText = String((typeof base?.text === "string" && base.text.trim()) || (typeof base?.actual === "string" && base.actual.trim()) || "");

        const callerKey = String(context?.caller?.key || "").trim();
        const routineKey = String(context?.routine?.key || "").trim();
        const evalRoutineKey = callerKey && typeof base?.actual === "string" ? callerKey : routineKey || callerKey;

        if (exprText && evalRoutineKey) {
          let overrideByKey = null;
          if (overrideEntry instanceof Map) {
            overrideByKey = overrideEntry;
          } else if (originKey && overrideValue.trim()) {
            overrideByKey = new Map([[originKey, overrideValue]]);
          }

          if (overrideByKey && overrideByKey.size) {
            const resolved = desc.describeExpression(model, evalRoutineKey, exprText, { callPath, overrideByKey });
            const nextText = String(resolved?.text ?? "").trim();
            if (nextText) {
              cell.text = nextText;
              continue;
            }
          }
        }
      }

      if (typeof overrideEntry === "string") cell.text = overrideValue;
    }
  }

  tpl.getLocalTemplateOverrides = getLocalTemplateOverrides;
  tpl.setLocalTemplateOverride = setLocalTemplateOverride;
  tpl.setLocalTemplateOverrideByOriginKey = setLocalTemplateOverrideByOriginKey;
  tpl.resolveBindPath = resolveBindPath;
  tpl.applyOverridesToConfig = applyOverridesToConfig;
})(window.AbapFlow);
