(function (ns) {
  "use strict";

  function isPlainObject(x) {
    return Boolean(x) && typeof x === "object" && !Array.isArray(x);
  }

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function pushErr(errors, path, message) {
    errors.push({ path: String(path || ""), message: String(message || "Invalid value") });
  }

  function validateTemplateEntry(tpl, path, errors) {
    if (!isPlainObject(tpl)) {
      pushErr(errors, path, "Template must be an object.");
      return;
    }
    const id = asNonEmptyString(tpl.id);
    if (!id) pushErr(errors, `${path}.id`, "Template id is required.");

    const label = asNonEmptyString(tpl.label);
    if (!label) pushErr(errors, `${path}.label`, "Template label is required.");

    if (tpl.file != null && typeof tpl.file !== "string") {
      pushErr(errors, `${path}.file`, "Template file path must be a string.");
    }

    if (tpl.auto != null && typeof tpl.auto !== "boolean") {
      pushErr(errors, `${path}.auto`, "Template auto must be boolean.");
    }
  }

  function validateObjectDef(obj, index, errors) {
    const path = `objects[${index}]`;
    if (!isPlainObject(obj)) {
      pushErr(errors, path, "Object must be an object.");
      return;
    }

    const id = asNonEmptyString(obj.id);
    if (!id) pushErr(errors, `${path}.id`, "Object id is required.");

    const kind = asNonEmptyString(obj.kind).toLowerCase();
    if (!kind || !["statement", "calledge"].includes(kind)) {
      pushErr(errors, `${path}.kind`, "Object kind must be 'statement' or 'callEdge'.");
    }

    const builderKind = asNonEmptyString(obj?.builder?.kind);
    if (!builderKind) pushErr(errors, `${path}.builder.kind`, "Builder kind is required.");

    if (kind === "statement") {
      const parseKind = asNonEmptyString(obj?.parse?.kind).toLowerCase();
      if (!parseKind) pushErr(errors, `${path}.parse.kind`, "Parse kind is required for statement objects.");

      if (parseKind === "regex") {
        const rx = obj?.parse?.regex;
        const isRe = rx instanceof RegExp;
        const isStr = typeof rx === "string" && asNonEmptyString(rx);
        if (!isRe && !isStr) {
          pushErr(errors, `${path}.parse.regex`, "parse.regex must be a RegExp or a non-empty pattern string.");
        }

        if (obj?.parse?.flags != null && typeof obj.parse.flags !== "string") {
          pushErr(errors, `${path}.parse.flags`, "parse.flags must be a string.");
        }

        if (obj?.parse?.fields != null && !isPlainObject(obj.parse.fields)) {
          pushErr(errors, `${path}.parse.fields`, "parse.fields must be an object.");
        } else if (isPlainObject(obj?.parse?.fields)) {
          for (const [k, v] of Object.entries(obj.parse.fields)) {
            const n = Number(v);
            if (!asNonEmptyString(k)) continue;
            if (!Number.isFinite(n) || n < 0) pushErr(errors, `${path}.parse.fields.${k}`, "Field index must be a number >= 0.");
          }
        }
      }
    }

    if (kind === "calledge") {
      const toKeyPrefix = asNonEmptyString(obj?.match?.toKeyPrefix);
      if (!toKeyPrefix) pushErr(errors, `${path}.match.toKeyPrefix`, "match.toKeyPrefix is required for callEdge objects.");
    }

    if (obj.templates != null) {
      if (!Array.isArray(obj.templates)) {
        pushErr(errors, `${path}.templates`, "templates must be an array.");
      } else {
        for (let i = 0; i < obj.templates.length; i++) {
          validateTemplateEntry(obj.templates[i], `${path}.templates[${i}]`, errors);
        }
      }
    }
  }

  function validateMasterConfig(config) {
    const errors = [];

    if (!isPlainObject(config)) {
      pushErr(errors, "", "Config must be an object.");
      return { ok: false, errors };
    }

    const schema = asNonEmptyString(config.schema);
    if (!schema) pushErr(errors, "schema", "schema is required.");

    const version = Number(config.version);
    if (!Number.isFinite(version) || version <= 0) pushErr(errors, "version", "version must be a positive number.");

    if (config.parserConfig != null && !isPlainObject(config.parserConfig)) {
      pushErr(errors, "parserConfig", "parserConfig must be an object.");
    }

    if (!Array.isArray(config.objects)) {
      pushErr(errors, "objects", "objects must be an array.");
    } else {
      const seen = new Set();
      for (let i = 0; i < config.objects.length; i++) {
        const id = asNonEmptyString(config.objects?.[i]?.id);
        if (id) {
          const lower = id.toLowerCase();
          if (seen.has(lower)) pushErr(errors, `objects[${i}].id`, `Duplicate object id: ${id}`);
          else seen.add(lower);
        }
        validateObjectDef(config.objects[i], i, errors);
      }
    }

    return { ok: errors.length === 0, errors };
  }

  ns.abapObjects = ns.abapObjects || {};
  ns.abapObjects.schema = {
    validateMasterConfig,
  };
})(window.AbapFlow);
