(function (ns) {
  "use strict";

  const utils = ns.utils;

  ns.abapObjects = ns.abapObjects || {};
  const schema = ns.abapObjects.schema;
  const parsers = ns.abapObjects.parsers;

  const state = {
    readyPromise: null,
    config: null,
    templatesById: new Map(),
    registry: null,
  };

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function deepCloneJson(value) {
    return deepClone(value);
  }

  function deepClone(value) {
    if (value == null) return value;

    if (value instanceof RegExp) {
      return new RegExp(value.source, value.flags);
    }

    if (Array.isArray(value)) {
      return value.map((x) => deepClone(x));
    }

    if (typeof value === "object") {
      const out = {};
      for (const [k, v] of Object.entries(value)) out[k] = deepClone(v);
      return out;
    }

    return value;
  }

  function formatSchemaErrors(errors) {
    const list = Array.isArray(errors) ? errors : [];
    if (!list.length) return "Invalid config.";
    return list.map((e) => `${e.path || "(root)"}: ${e.message || "Invalid value"}`).join("\n");
  }

  function defaultLoadScript(url) {
    return new Promise((resolve, reject) => {
      if (typeof document === "undefined") {
        reject(new Error("No DOM available to load scripts."));
        return;
      }
      const src = String(url || "").trim();
      if (!src) {
        reject(new Error("Empty script url."));
        return;
      }

      const el = document.createElement("script");
      el.src = src;
      el.async = false;
      el.onload = () => resolve(true);
      el.onerror = () => reject(new Error(`Failed to load script: ${src}`));
      document.head.appendChild(el);
    });
  }

  function defineTemplate(id, config) {
    const tid = asNonEmptyString(id);
    if (!tid) return;
    state.templatesById.set(tid, config || {});
  }

  function getTemplateConfig(id) {
    const tid = asNonEmptyString(id);
    return tid ? state.templatesById.get(tid) || null : null;
  }

  function compileObjectDefs(config) {
    const objects = Array.isArray(config?.objects) ? config.objects : [];

    const objectsById = new Map();
    const statementObjects = [];
    const callEdgeObjects = [];
    const templates = [];

    for (const obj of objects) {
      const id = asNonEmptyString(obj?.id);
      if (!id) continue;
      const kind = asNonEmptyString(obj?.kind).toLowerCase();
      const label = asNonEmptyString(obj?.label) || id;

      const def = {
        id,
        kind,
        label,
        parse: obj?.parse || null,
        match: obj?.match || null,
        builder: obj?.builder || null,
        templates: Array.isArray(obj?.templates) ? obj.templates : [],
      };
      objectsById.set(id, def);

      if (kind === "statement") statementObjects.push(def);
      if (kind === "calledge") callEdgeObjects.push(def);

      for (const t of def.templates) {
        const tid = asNonEmptyString(t?.id);
        if (!tid) continue;
        templates.push({
          id: tid,
          label: asNonEmptyString(t?.label) || tid,
          objectId: id,
          source: id,
          auto: t?.auto !== false,
          when: t?.when || null,
          file: asNonEmptyString(t?.file),
        });
      }
    }

    return { objectsById, statementObjects, callEdgeObjects, templates };
  }

  function parseStatementByObject(def, stmtText, sourceRef, cfg) {
    const kind = asNonEmptyString(def?.parse?.kind).toLowerCase();
    if (!kind) return null;

    if (kind === "assignment") return parsers.parseAssignmentStatement(stmtText, sourceRef, cfg);
    if (kind === "conditional") return parsers.parseConditionalStatement(stmtText, sourceRef, cfg);
    if (kind === "message") return parsers.parseMessageStatement(stmtText, sourceRef, cfg);
    if (kind === "itabop") return parsers.parseItabOperation(stmtText, sourceRef, cfg);
    if (kind === "regex") return parsers.parseRegexStatement(stmtText, sourceRef, def.parse);

    return null;
  }

  function buildContext(def, model, routine, payload, options) {
    const kind = asNonEmptyString(def?.builder?.kind).toLowerCase();
    const tc = ns.templateConverter;
    if (!tc) return null;

    if (kind === "performcall") return tc.buildPerformContext(model, payload, options);
    if (kind === "assignment") return tc.buildAssignmentContext(model, routine, payload, options);
    if (kind === "if") return tc.buildIfContext(model, routine, payload, options);
    if (kind === "message") return tc.buildMessageContext(model, routine, payload, options);
    if (kind === "itabop") return tc.buildItabOpContext(model, routine, payload, options);
    if (kind === "append") return tc.buildAppendContext(model, routine, payload, options);

    if (kind === "mapping") return buildContextByMapping(model, routine, payload, def, options);
    return null;
  }

  function describeExprWithOrigin(model, routineKey, exprText, options) {
    const text = String(exprText ?? "").trim();
    if (!text) return { text: "", originKey: "" };
    if (ns.desc?.describeExpressionWithOrigin) return ns.desc.describeExpressionWithOrigin(model, routineKey, text, options);
    return { text, originKey: "" };
  }

  function buildExprNode(model, routineKey, exprText, options) {
    const text = String(exprText ?? "").trim();
    if (!text) return { text: "", description: "", originKey: "" };
    const described = describeExprWithOrigin(model, routineKey, text, options);
    return {
      text,
      description: String(described.text || "").trim() || text,
      originKey: String(described.originKey || ""),
    };
  }

  function buildContextByMapping(model, routine, payload, def, options) {
    const routineKey = String(routine?.key || "");
    const callPath = Array.isArray(options?.callPath) ? options.callPath : null;
    const mapping = def?.builder?.fields && typeof def.builder.fields === "object" ? def.builder.fields : {};

    const ctx = {
      routine: { key: routineKey, kind: String(routine?.kind || ""), name: String(routine?.name || "") },
      object: { id: String(def?.id || ""), label: String(def?.label || ""), statement: String(payload?.statement || payload?.raw || "") },
    };

    for (const [outKey, spec] of Object.entries(mapping)) {
      const t = String(spec?.type || "expr").trim().toLowerCase();
      const from = String(spec?.from || "").trim();
      const raw = from && payload && typeof payload === "object" ? payload[from] : "";

      if (t === "text") {
        ctx[outKey] = String(raw ?? "");
        continue;
      }

      if (t === "exprlist") {
        const list = Array.isArray(raw) ? raw : [];
        ctx[outKey] = list.map((x) => buildExprNode(model, routineKey, x, { callPath }));
        continue;
      }

      ctx[outKey] = buildExprNode(model, routineKey, raw, { callPath });
    }

    return ctx;
  }

  function installTemplateRegistry(templates) {
    ns.templateRegistry = ns.templateRegistry || { templates: {}, order: [] };
    ns.templateRegistry.templates = ns.templateRegistry.templates || {};
    ns.templateRegistry.order = Array.isArray(ns.templateRegistry.order) ? ns.templateRegistry.order : [];

    ns.templateRegistry.templates = {};
    ns.templateRegistry.order = [];

    for (const t of templates) {
      const cfg = getTemplateConfig(t.id);
      if (!cfg) continue;
      ns.templateRegistry.templates[t.id] = {
        id: t.id,
        label: t.label,
        source: t.source,
        auto: t.auto !== false,
        objectId: t.objectId,
        when: t.when || null,
        config: cfg,
      };
      ns.templateRegistry.order.push(t.id);
    }
  }

  function init(options) {
    if (state.readyPromise) return state.readyPromise;

    state.readyPromise = (async () => {
      if (!schema?.validateMasterConfig) throw new Error("ABAP Objects schema module not loaded.");
      if (!parsers) throw new Error("ABAP Objects parsers module not loaded.");

      let config = options?.config || ns.abapObjectsMasterConfig;
      if (!options?.config) {
        const customStore = ns.abapObjects?.customStore || null;
        if (customStore && typeof customStore.getEffectiveConfig === "function") {
          config = customStore.getEffectiveConfig(config)?.config || config;
        }
      }
      const validation = schema.validateMasterConfig(config);
      if (!validation.ok) {
        throw new Error(formatSchemaErrors(validation.errors));
      }

      state.config = config;

      if (config?.parserConfig && typeof config.parserConfig === "object") {
        ns.parserConfig = deepCloneJson(config.parserConfig);
      }

      const compiled = compileObjectDefs(config);
      state.registry = {
        ...compiled,
        parseStatementItems(model, routine, statementText, sourceRef, cfg) {
          const out = [];
          for (const objDef of compiled.statementObjects) {
            const parsed = parseStatementByObject(objDef, statementText, sourceRef, cfg);
            if (!parsed) continue;
            out.push({
              objectId: objDef.id,
              kind: "statement",
              payload: parsed,
              sourceRef: parsed?.sourceRef || sourceRef || null,
              continueAfterMatch: Boolean(objDef?.parse?.continueAfterMatch),
            });
            if (!objDef?.parse?.continueAfterMatch) break;
          }
          return out;
        },
        matchCallEdge(edge) {
          const toKey = String(edge?.toKey || "");
          for (const objDef of compiled.callEdgeObjects) {
            const prefix = asNonEmptyString(objDef?.match?.toKeyPrefix);
            if (prefix && toKey.toUpperCase().startsWith(prefix.toUpperCase())) return objDef;
          }
          return null;
        },
        buildContext,
      };

      const loadScript = typeof options?.loadScript === "function" ? options.loadScript : defaultLoadScript;

      const filesToLoad = compiled.templates.map((t) => t.file).filter(Boolean);
      const uniqueFiles = Array.from(new Set(filesToLoad));
      for (const file of uniqueFiles) await loadScript(file);

      const templateDefs = ns.templateDefs || null;
      if (templateDefs && typeof templateDefs.getTemplateConfig === "function") {
        for (const t of compiled.templates) {
          const override = templateDefs.getTemplateConfig(t.id);
          if (override) defineTemplate(t.id, override);
        }
      }

      for (const t of compiled.templates) {
        if (!getTemplateConfig(t.id)) throw new Error(`Template not registered: ${t.id} (${t.file})`);
      }

      let templatesForRegistry = compiled.templates;

      if (templateDefs && typeof templateDefs.listCustomTemplateEntries === "function") {
        const custom = templateDefs.listCustomTemplateEntries();
        const seen = new Set(compiled.templates.map((t) => String(t?.id || "").trim().toLowerCase()).filter(Boolean));

        const merged = [];
        for (const t of custom) {
          const id = asNonEmptyString(t?.id).toLowerCase();
          if (!id || seen.has(id)) continue;
          if (t?.config) defineTemplate(t.id, t.config);
          merged.push({
            id: asNonEmptyString(t.id),
            label: asNonEmptyString(t.label) || asNonEmptyString(t.id),
            objectId: asNonEmptyString(t.objectId) || asNonEmptyString(t.source),
            source: asNonEmptyString(t.source),
            auto: t.auto !== false,
            when: t.when || null,
            file: "",
          });
        }

        templatesForRegistry = merged.concat(compiled.templates);
      }

      installTemplateRegistry(templatesForRegistry);
      return state.registry;
    })();

    return state.readyPromise;
  }

  function whenReady() {
    return init();
  }

  function reset() {
    state.readyPromise = null;
    state.config = null;
    state.registry = null;
    state.templatesById.clear();
  }

  ns.abapObjects.defineTemplate = defineTemplate;
  ns.abapObjects.getTemplateConfig = getTemplateConfig;
  ns.abapObjects.init = init;
  ns.abapObjects.whenReady = whenReady;
  ns.abapObjects.getRegistry = () => state.registry;
  ns.abapObjects.getConfig = () => state.config;
  ns.abapObjects.reset = reset;
})(window.AbapFlow);
