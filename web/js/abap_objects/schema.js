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
    errors.push({ path: String(path || ""), message: String(message || "Giá trị không hợp lệ") });
  }

  function validateTemplateEntry(tpl, path, errors) {
    if (!isPlainObject(tpl)) {
      pushErr(errors, path, "Template phải là object.");
      return;
    }
    const id = asNonEmptyString(tpl.id);
    if (!id) pushErr(errors, `${path}.id`, "Cần template id.");

    const label = asNonEmptyString(tpl.label);
    if (!label) pushErr(errors, `${path}.label`, "Cần nhãn template.");

    if (tpl.file != null && typeof tpl.file !== "string") {
      pushErr(errors, `${path}.file`, "Đường dẫn file template phải là chuỗi.");
    }

    if (tpl.auto != null && typeof tpl.auto !== "boolean") {
      pushErr(errors, `${path}.auto`, "Thuộc tính auto phải là boolean.");
    }

    if (tpl.config != null && !isPlainObject(tpl.config)) {
      pushErr(errors, `${path}.config`, "Template config phải là object.");
    }
  }

  function validateObjectDef(obj, index, errors) {
    const path = `objects[${index}]`;
    if (!isPlainObject(obj)) {
      pushErr(errors, path, "Object phải là object.");
      return;
    }

    const id = asNonEmptyString(obj.id);
    if (!id) pushErr(errors, `${path}.id`, "Cần object id.");

    const kind = asNonEmptyString(obj.kind).toLowerCase();
    if (!kind || !["statement", "calledge"].includes(kind)) {
      pushErr(errors, `${path}.kind`, "Object kind phải là 'statement' hoặc 'callEdge'.");
    }

    const builderKind = asNonEmptyString(obj?.builder?.kind);
    if (!builderKind) pushErr(errors, `${path}.builder.kind`, "Cần builder kind.");

    if (kind === "statement") {
      const parseKind = asNonEmptyString(obj?.parse?.kind).toLowerCase();
      if (!parseKind) pushErr(errors, `${path}.parse.kind`, "Cần parse kind cho statement.");

      if (parseKind === "regex") {
        const rx = obj?.parse?.regex;
        const isRe = rx instanceof RegExp;
        const isStr = typeof rx === "string" && asNonEmptyString(rx);
        if (!isRe && !isStr) {
          pushErr(errors, `${path}.parse.regex`, "parse.regex phải là RegExp hoặc chuỗi pattern không rỗng.");
        }

        if (obj?.parse?.flags != null && typeof obj.parse.flags !== "string") {
          pushErr(errors, `${path}.parse.flags`, "parse.flags phải là chuỗi.");
        }

        if (obj?.parse?.fields != null && !isPlainObject(obj.parse.fields)) {
          pushErr(errors, `${path}.parse.fields`, "parse.fields phải là object.");
        } else if (isPlainObject(obj?.parse?.fields)) {
          for (const [k, v] of Object.entries(obj.parse.fields)) {
            const n = Number(v);
            if (!asNonEmptyString(k)) continue;
            if (!Number.isFinite(n) || n < 0) pushErr(errors, `${path}.parse.fields.${k}`, "Chỉ số field phải là số >= 0.");
          }
        }
      }
    }

    if (kind === "calledge") {
      const toKeyPrefix = asNonEmptyString(obj?.match?.toKeyPrefix);
      if (!toKeyPrefix) pushErr(errors, `${path}.match.toKeyPrefix`, "Cần match.toKeyPrefix cho callEdge.");
    }

    if (obj.templates != null) {
      if (!Array.isArray(obj.templates)) {
        pushErr(errors, `${path}.templates`, "templates phải là array.");
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
      pushErr(errors, "", "Config phải là object.");
      return { ok: false, errors };
    }

    const schema = asNonEmptyString(config.schema);
    if (!schema) pushErr(errors, "schema", "Cần schema.");

    const version = Number(config.version);
    if (!Number.isFinite(version) || version <= 0) pushErr(errors, "version", "version phải là số dương.");

    if (config.parserConfig != null && !isPlainObject(config.parserConfig)) {
      pushErr(errors, "parserConfig", "parserConfig phải là object.");
    }

    if (!Array.isArray(config.objects)) {
      pushErr(errors, "objects", "objects phải là array.");
    } else {
      const seen = new Set();
      for (let i = 0; i < config.objects.length; i++) {
        const id = asNonEmptyString(config.objects?.[i]?.id);
        if (id) {
          const lower = id.toLowerCase();
          if (seen.has(lower)) pushErr(errors, `objects[${i}].id`, `Trùng object id: ${id}`);
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
