"use strict";
(function registerTemplateService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before template loads.");
  }
  const state = runtime.state;
  const els = runtime.els;
  const constants = runtime.constants || {};
  const { DESC_STORAGE_KEY_V2, SETTINGS_STORAGE_KEY_V1, TEMPLATE_CONFIG_STORAGE_KEY_V1, THEME_STORAGE_KEY_V1, LAYOUT_SPLIT_STORAGE_KEY_V1, LAYOUT_SPLIT_DEFAULT, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX, MOBILE_LAYOUT_QUERY, RENDER_TREE_OPTIONS, DECL_TYPE_OPTIONS, NAME_CODE_OPTIONS, DEFAULT_SETTINGS, TEMPLATE_DEFAULT_CONFIG_V1, SAMPLE_ABAP } = constants;
  const setError = runtime.requireServiceMethod("runtimeState", "setError");
  const getKeywordEntries = runtime.requireServiceMethod("runtimeState", "getKeywordEntries");
  const getValueEntries = runtime.requireServiceMethod("runtimeState", "getValueEntries");
  const saveDescOverrides = runtime.requireServiceMethod("runtimeState", "saveDescOverrides");
  const normalizeSettings = runtime.requireServiceMethod("runtimeState", "normalizeSettings");
  const loadSettings = runtime.requireServiceMethod("runtimeState", "loadSettings");
  const setTemplateConfigError = runtime.requireServiceMethod("runtimeState", "setTemplateConfigError");
  const setTemplatePreviewMessage = runtime.requireServiceMethod("runtimeState", "setTemplatePreviewMessage");
  const cloneJsonValue = runtime.requireServiceMethod("runtimeState", "cloneJsonValue");
  const getDefaultTemplateConfig = runtime.requireServiceMethod("runtimeState", "getDefaultTemplateConfig");
  const mergeMissingDefaultTemplatesInPlace = runtime.requireServiceMethod("runtimeState", "mergeMissingDefaultTemplatesInPlace");
  const normalizeTemplateAliasToken = runtime.requireServiceMethod("runtimeState", "normalizeTemplateAliasToken");
  const parseRangeKey = runtime.requireServiceMethod("runtimeState", "parseRangeKey");
  const isTemplateOptionConfigKey = runtime.requireServiceMethod("runtimeState", "isTemplateOptionConfigKey");
  const parseTemplateLabelDirectives = runtime.requireServiceMethod("runtimeState", "parseTemplateLabelDirectives");
  const validateTemplateConfig = runtime.requireServiceMethod("runtimeState", "validateTemplateConfig");
  const saveTemplateConfig = runtime.requireServiceMethod("runtimeState", "saveTemplateConfig");
  const normalizeTheme = runtime.requireServiceMethod("runtimeState", "normalizeTheme");
  const applyTheme = runtime.requireServiceMethod("runtimeState", "applyTheme");
  const normalizeLayoutSplit = runtime.requireServiceMethod("runtimeState", "normalizeLayoutSplit");
  const applyLayoutSplit = runtime.requireServiceMethod("runtimeState", "applyLayoutSplit");
  const renderSettingsModalUi = runtime.requireServiceMethod("runtimeState", "renderSettingsModalUi");
  const normalizeEntryObjectForPath = runtime.requireServiceMethod("output", "normalizeEntryObjectForPath");
  const selectCodeLines = runtime.requireServiceMethod("output", "selectCodeLines");
  const getSelectedTemplateIndexSet = runtime.requireServiceMethod("output", "getSelectedTemplateIndexSet");
  const getSortedSelectedTemplateIndexes = runtime.requireServiceMethod("output", "getSortedSelectedTemplateIndexes");
  const syncRenderedTemplateSelection = runtime.requireServiceMethod("output", "syncRenderedTemplateSelection");
  const clearTemplateBlockSelection = runtime.requireServiceMethod("output", "clearTemplateBlockSelection");
  const pruneTemplateBlockSelection = runtime.requireServiceMethod("output", "pruneTemplateBlockSelection");
  const selectTemplateBlockFromInteraction = runtime.requireServiceMethod("output", "selectTemplateBlockFromInteraction");
  const refreshInputGutterTargets = runtime.requireServiceMethod("output", "refreshInputGutterTargets");
  const openTextModal = runtime.requireServiceMethod("output", "openTextModal");
  const getDeclTechName = runtime.requireServiceMethod("output", "getDeclTechName");
  const stripDeclCategoryPrefix = runtime.requireServiceMethod("output", "stripDeclCategoryPrefix");
  const isStructFieldDecl = runtime.requireServiceMethod("output", "isStructFieldDecl");
  const getDeclDisplayName = runtime.requireServiceMethod("output", "getDeclDisplayName");
  const el = runtime.requireServiceMethod("output", "el");
  const renderMeta = runtime.requireServiceMethod("output", "renderMeta");
  const getObjectLabel = runtime.requireServiceMethod("output", "getObjectLabel");
  const setSelectedTemplateBlock = runtime.requireServiceMethod("output", "setSelectedTemplateBlock");
  const renderDeclDescPanelUi = runtime.requireServiceMethod("descriptions", "renderDeclDescPanelUi");
  const getDeclKey = runtime.requireServiceMethod("descriptions", "getDeclKey");
  const getDeclOverrideLookupKeys = runtime.requireServiceMethod("descriptions", "getDeclOverrideLookupKeys");
  const cloneDeclWithPerformChainOverride = runtime.requireServiceMethod("descriptions", "cloneDeclWithPerformChainOverride");
  const getDeclOverrideStorageKey = runtime.requireServiceMethod("descriptions", "getDeclOverrideStorageKey");
  const applyDeclDescriptionOverride = runtime.requireServiceMethod("descriptions", "applyDeclDescriptionOverride");
  const normalizeDescOverrideEntry = runtime.requireServiceMethod("descriptions", "normalizeDescOverrideEntry");
  const getDeclOverrideEntry = runtime.requireServiceMethod("descriptions", "getDeclOverrideEntry");
  const normalizeDeclDescText = runtime.requireServiceMethod("descriptions", "normalizeDeclDescText");
  const getEffectiveDeclAtomicDescNormalized = runtime.requireServiceMethod("descriptions", "getEffectiveDeclAtomicDescNormalized");
  const buildStructDeclFromFieldDecl = runtime.requireServiceMethod("descriptions", "buildStructDeclFromFieldDecl");
  const getEffectiveDeclDesc = runtime.requireServiceMethod("descriptions", "getEffectiveDeclDesc");
  const getFinalDeclDesc = runtime.requireServiceMethod("descriptions", "getFinalDeclDesc");
  const safeJson = runtime.requireServiceMethod("descriptions", "safeJson");
  const getArrayItemTagName = runtime.requireServiceMethod("descriptions", "getArrayItemTagName");
  const isAbapStatementObject = runtime.requireServiceMethod("descriptions", "isAbapStatementObject");
  const getDeclSourceContextFromObject = runtime.requireServiceMethod("descriptions", "getDeclSourceContextFromObject");
  const buildObjectPathBase = runtime.requireServiceMethod("descriptions", "buildObjectPathBase");
  const ensureEntryDeclWithSynthetic = runtime.requireServiceMethod("descriptions", "ensureEntryDeclWithSynthetic");
  const ensureValueDeclWithSynthetic = runtime.requireServiceMethod("descriptions", "ensureValueDeclWithSynthetic");
  const ensureConditionClauseDeclsWithSynthetic = runtime.requireServiceMethod("descriptions", "ensureConditionClauseDeclsWithSynthetic");
  const isDeclLikeObject = runtime.requireServiceMethod("descriptions", "isDeclLikeObject");
  const hasValueLevelDescFields = runtime.requireServiceMethod("descriptions", "hasValueLevelDescFields");
  const resolveValueLevelFinalDesc = runtime.requireServiceMethod("descriptions", "resolveValueLevelFinalDesc");
  const createPerformSourceControl = runtime.requireServiceMethod("performSources", "createPerformSourceControl");
  const renderActiveRightPanel = runtime.requireServiceMethod("uiNavigation", "renderActiveRightPanel");
  const jumpInputToCodeRange = runtime.requireServiceMethod("uiNavigation", "jumpInputToCodeRange");
  const createSyntheticStructFieldDecl = runtime.requireServiceMethod("parserController", "createSyntheticStructFieldDecl");
  const start = runtime.requireServiceMethod("bootstrap", "start");
const TEMPLATE_GUI_FILTER_STORAGE_KEY_V1 = "abap-parser-viewer.templateGuiHiddenObjectTypes.v1";
  const TEMPLATE_FORM_EDITOR_PCT_STORAGE_KEY_V1 = "abap-parser-viewer.templateFormEditorPct.v1";
  const VIEWER_CONFIG_KIND_V1 = "abap-viewer-config";
  const VIEWER_CONFIG_VERSION_V1 = 1;
  const VIEWER_CONFIG_SECTION_DEFS_V1 = Object.freeze([
    { key: "templates", label: "Templates", fileToken: "templates" },
    { key: "descriptionSettings", label: "Description settings", fileToken: "description-settings" },
    { key: "descriptionOverrides", label: "Description overrides", fileToken: "description-overrides" },
    { key: "appearance", label: "Appearance", fileToken: "appearance" },
    { key: "templateUi", label: "Template UI", fileToken: "template-ui" }
  ]);
  let activeTemplateDynamicModal = null;
  let templateFilterModalControls = null;
  let activeTemplateFormSplitController = null;

var PERFORM_TRACE_META_KEY_TEMPLATE = "__abapPerformTraceBinding";
  var TEMPLATE_ROW_DECLS_META_KEY_TEMPLATE = "__abapTemplateRowDecls";
  var TEMPLATE_OBJECT_INDEX_META_KEY_TEMPLATE = "__abapTemplateObjectIndex";
  var TEMPLATE_CONTEXT_ERRORS_META_KEY_TEMPLATE = "__abapTemplateContextErrors";
  var TEMPLATE_PROVENANCE_WARNED_KEYS_TEMPLATE = new Set();

  function toInlineCssText(styleMap) {
    if (!styleMap || typeof styleMap !== "object") {
      return "";
    }
    const entries = [];
    for (const [key, value] of Object.entries(styleMap)) {
      const cssKey = String(key || "").trim();
      const cssValue = String(value || "").trim();
      if (!cssKey || !cssValue) {
        continue;
      }
      entries.push(`${cssKey}:${cssValue}`);
    }
    return entries.join(";");
  }

  function normalizeTemplateColorValue(value) {
    const raw = String(value || "").trim();
    if (!raw) {
      return "";
    }

    const alias = normalizeTemplateAliasToken(raw);
    if (!alias || alias === "default") {
      return "";
    }
    if (alias === "mau xanh nhat") {
      return "#dbeef4";
    }
    if (alias === "den") {
      return "#000000";
    }
    return raw;
  }

  function normalizeTemplateBorderValue(value) {
    const raw = String(value || "").trim();
    if (!raw) {
      return "";
    }

    const alias = normalizeTemplateAliasToken(raw);
    if (!alias || alias === "default") {
      return "";
    }
    if (alias === "outside line mong") {
      return "outside-thin";
    }
    return raw;
  }

  function normalizeTemplateAlignValue(value) {
    const alias = normalizeTemplateAliasToken(value);
    if (!alias || alias === "default") {
      return "";
    }
    if (alias === "left" || alias === "center" || alias === "right") {
      return alias;
    }
    return "";
  }

  function normalizeTemplateVAlignValue(value) {
    const alias = normalizeTemplateAliasToken(value);
    if (!alias || alias === "default") {
      return "";
    }
    if (alias === "top") {
      return "top";
    }
    if (alias === "middle" || alias === "center") {
      return "middle";
    }
    if (alias === "bottom") {
      return "bottom";
    }
    return "";
  }

  function parseTemplatePathSegments(pathExpression) {
    const raw = String(pathExpression || "").trim();
    if (!raw) {
      return [];
    }

    const segments = [];
    let token = "";
    let index = 0;

    const pushToken = () => {
      const trimmed = token.trim();
      if (trimmed) {
        segments.push(trimmed);
      }
      token = "";
    };

    while (index < raw.length) {
      const ch = raw[index];
      if (ch === ".") {
        pushToken();
        index += 1;
        continue;
      }

      if (ch === "[") {
        pushToken();
        const close = raw.indexOf("]", index + 1);
        if (close === -1) {
          return null;
        }
        const inside = raw.slice(index + 1, close).trim();
        if (!/^\d+$/.test(inside)) {
          return null;
        }
        segments.push(Number(inside));
        index = close + 1;
        continue;
      }

      token += ch;
      index += 1;
    }

    pushToken();
    return segments;
  }

  function isDeclLikePathSegment(segment) {
    const key = String(segment || "").replace(/\[\d+\]$/, "").trim();
    if (!key) {
      return false;
    }
    if (key.toLowerCase() === "decl") {
      return true;
    }
    return /decl$/i.test(key);
  }

  function isTemplateDeclLikeValue(value) {
    if (isDeclLikeObject(value)) {
      return true;
    }
    return Boolean(
      value
      && typeof value === "object"
      && typeof value.objectType === "string"
      && typeof value.name === "string"
    );
  }

  function resolveConditionOperandFinalDesc(clause, declKey, decl) {
    if (!clause || typeof clause !== "object" || !decl || typeof decl !== "object") {
      return undefined;
    }

    const normalizedDeclKey = String(declKey || "").trim().toLowerCase();
    const isLeft = normalizedDeclKey === "leftoperanddecl";
    const isRight = normalizedDeclKey === "rightoperanddecl";
    if (!isLeft && !isRight) {
      return undefined;
    }

    const operandKey = isLeft ? "leftOperand" : "rightOperand";
    const operandRefKey = isLeft ? "leftOperandRef" : "rightOperandRef";
    const operandText = String(clause[operandKey] || "");
    if (!operandText.trim()) {
      return undefined;
    }

    const operandRef = String(clause[operandRefKey] || "").trim();
    if (!operandRef && String(decl.objectType || "").trim().toUpperCase() === "SYSTEM") {
      return String(getFinalDeclDesc(decl) || operandText).trim() || undefined;
    }

    const resolved = resolveValueLevelFinalDesc({
      value: operandText,
      userDesc: "",
      codeDesc: "",
      declRef: operandRef,
      decl
    });
    return String(resolved || "").trim() || undefined;
  }

  function resolveTemplatePathValue(root, pathExpression) {
    const segments = parseTemplatePathSegments(pathExpression);
    if (!segments) {
      return undefined;
    }

    let current = root;
    let parent = null;
    let parentAccessKey = "";
    for (let segmentIndex = 0; segmentIndex < segments.length; segmentIndex += 1) {
      const segment = segments[segmentIndex];
      if (typeof segment === "number") {
        if (!Array.isArray(current)) {
          return undefined;
        }
        parent = current;
        parentAccessKey = `[${segment}]`;
        current = current[segment];
        continue;
      }

      const key = String(segment || "").trim();
      if (!key) {
        continue;
      }

      if (Array.isArray(current)) {
        const projected = [];
        const keyLower = key.toLowerCase();
        const nextKeyLower = String(segments[segmentIndex + 1] || "").trim().toLowerCase();
        for (const item of current) {
          if (!item || typeof item !== "object") {
            continue;
          }

          if (
            nextKeyLower === "finaldesc"
            && (keyLower === "leftoperanddecl" || keyLower === "rightoperanddecl")
          ) {
            const operandFinalDesc = resolveConditionOperandFinalDesc(item, key, item[key]);
            if (operandFinalDesc !== undefined) {
              projected.push(operandFinalDesc);
            }
            continue;
          }

          if (keyLower === "desc" && isDeclLikeObject(item)) {
            projected.push(getEffectiveDeclDesc(item));
            continue;
          }

          if (keyLower === "finaldesc") {
            if (isTemplateDeclLikeValue(item)) {
              projected.push(getFinalDeclDesc(item));
              continue;
            }
            if (hasValueLevelDescFields(item) || isDeclLikeObject(item.decl)) {
              projected.push(resolveValueLevelFinalDesc(item));
              continue;
            }
          }

          if (Object.prototype.hasOwnProperty.call(item, key)) {
            projected.push(item[key]);
          }
        }
        if (!projected.length) {
          return undefined;
        }
        parent = null;
        parentAccessKey = "";
        current = projected;
        if (
          nextKeyLower === "finaldesc"
          && (keyLower === "leftoperanddecl" || keyLower === "rightoperanddecl")
        ) {
          segmentIndex += 1;
        }
        continue;
      }

      if (!current || typeof current !== "object") {
        return undefined;
      }

      const keyLower = key.toLowerCase();
      if (keyLower === "desc" && isDeclLikeObject(current)) {
        return getEffectiveDeclDesc(current);
      }

      if (keyLower === "finaldesc") {
        const parentIsDecl = isDeclLikePathSegment(parentAccessKey);

        // If the path is explicitly '...decl.finalDesc', prefer value-level finalDesc
        // when the parent is a value-entry object (expression-aware output).
        if (parentIsDecl) {
          const conditionOperandFinalDesc = resolveConditionOperandFinalDesc(parent, parentAccessKey, current);
          if (conditionOperandFinalDesc !== undefined) {
            return conditionOperandFinalDesc;
          }
          if (parent && typeof parent === "object" && (hasValueLevelDescFields(parent) || isDeclLikeObject(parent.decl))) {
            return resolveValueLevelFinalDesc(parent);
          }
          return getFinalDeclDesc(current);
        }

        // Otherwise, apply the value-level or decl-level logic based on the object shape.
        if (isTemplateDeclLikeValue(current)) {
           return getFinalDeclDesc(current);
        }
        if (hasValueLevelDescFields(current) || isDeclLikeObject(current.decl)) {
          return resolveValueLevelFinalDesc(current);
        }
      }

      if (!Object.prototype.hasOwnProperty.call(current, key)) {
        return undefined;
      }
      parent = current;
      parentAccessKey = key;
      current = current[key];
    }

    return current;
  }

  function buildTemplatePathCandidates(tokenExpression) {
    const raw = String(tokenExpression || "").trim();
    if (!raw) {
      return [];
    }

    const candidates = new Set();
    candidates.add(raw);

    if (/^keyword\./i.test(raw)) {
      candidates.add(`keywords.${raw.slice("keyword.".length)}`);
    }

    return Array.from(candidates);
  }

  function getTemplateArrayItemTagName(keyHint) {
    if (typeof getArrayItemTagName === "function") {
      return getArrayItemTagName(keyHint);
    }
    const key = String(keyHint || "").trim().toLowerCase();
    if (key === "objects" || key === "children") {
      return "object";
    }
    return "item";
  }

  function normalizeTemplateEntryForPath(value, keyHint, pathParts, ownerContext, onError) {
    if (typeof normalizeEntryObjectForPath !== "function") {
      return value;
    }
    try {
      return normalizeEntryObjectForPath(value, keyHint, pathParts, ownerContext);
    } catch (err) {
      if (typeof onError === "function") {
        onError(err);
      }
      return value;
    }
  }

  function getTemplateDeclRenderKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    return (
      (typeof getDeclKey === "function" ? getDeclKey(decl) : "")
      || [
        decl.objectType || "",
        decl.scopeLabel || "",
        decl.name || "",
        decl.file || "",
        decl.lineStart || ""
      ].join("|")
    );
  }

  function dedupeTemplateDecls(list) {
    const out = [];
    const seen = new Set();
    for (const decl of Array.isArray(list) ? list : []) {
      if (!decl || typeof decl !== "object") {
        continue;
      }
      const key = getTemplateDeclRenderKey(decl);
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      out.push(decl);
    }
    return out;
  }

  function isTemplatePathDecl(decl) {
    return Boolean(
      decl
      && typeof decl === "object"
      && String(decl.objectType || "").trim().toUpperCase() === "PATH_DECL"
    );
  }

  function getTemplateDeclStorageKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const scope = String(decl.scopeLabel || "").trim().toUpperCase();
    const name = String(decl.name || "").trim().toUpperCase();
    return scope && name ? `${scope}:${name}` : "";
  }

  function attachTemplateSyntheticDeclAliases(decl, objectIndexOneBased) {
    if (!isTemplatePathDecl(decl)) {
      return decl;
    }
    const objectIndex = Math.max(1, Number(objectIndexOneBased) || 1);
    const canonicalKey = getTemplateDeclStorageKey(decl);
    const scopeLabel = String(decl.scopeLabel || "").trim().toUpperCase();
    const name = String(decl.name || "").trim().toUpperCase();
    const legacyScope = scopeLabel.replace(
      /^PATH:OBJECT:[^/]+\//,
      `PATH:OBJECTS/OBJECT[${objectIndex}]/`
    );
    const legacyScopes = [];
    const pushLegacyScope = (value) => {
      const scope = String(value || "").trim();
      if (scope && scope !== scopeLabel && !legacyScopes.includes(scope)) {
        legacyScopes.push(scope);
      }
    };
    pushLegacyScope(legacyScope);
    if (/\/CLAUSE\[/i.test(legacyScope)) {
      pushLegacyScope(legacyScope.replace(/\/CLAUSE\[/gi, "/ITEM["));
    }
    const lookupKeys = [];
    const pushKey = (value) => {
      const key = String(value || "").trim();
      if (key && !lookupKeys.includes(key)) {
        lookupKeys.push(key);
      }
    };
    pushKey(canonicalKey);
    if (name) {
      for (const scope of legacyScopes) {
        pushKey(`${scope}:${name}`);
      }
    }
    decl.overrideLookupKeys = lookupKeys;
    decl.pathAliases = legacyScopes;
    return decl;
  }

  function warnTemplateProvenanceOnce(reason, details) {
    const info = details && typeof details === "object" ? details : {};
    const errorMessage = info.error && info.error.message
      ? String(info.error.message)
      : String(info.error || "");
    const key = [
      String(reason || ""),
      String(info.objectId === undefined || info.objectId === null ? "" : info.objectId),
      String(info.line === undefined || info.line === null ? "" : info.line),
      String(info.template || ""),
      String(info.range || ""),
      String(info.token || ""),
      errorMessage
    ].join("|");
    if (TEMPLATE_PROVENANCE_WARNED_KEYS_TEMPLATE.has(key)) {
      return;
    }
    TEMPLATE_PROVENANCE_WARNED_KEYS_TEMPLATE.add(key);
    if (typeof console !== "undefined" && typeof console.warn === "function") {
      console.warn("Template description provenance resolution failed", {
        reason: String(reason || "RESOLUTION_ERROR"),
        objectId: info.objectId === undefined ? null : info.objectId,
        line: info.line === undefined ? null : info.line,
        template: String(info.template || ""),
        range: String(info.range || ""),
        token: String(info.token || ""),
        error: errorMessage
      });
    }
  }

  function isTemplateLiteralOrWildcard(value) {
    const text = String(value === undefined || value === null ? "" : value).trim();
    if (!text) {
      return false;
    }
    if (/^(?:\*|@\*|#|SPACE)$/i.test(text)) {
      return true;
    }
    if (/^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?$/.test(text)) {
      return true;
    }
    if (
      /^'(?:''|[^'])*'$/.test(text)
      || /^[A-Za-z_][A-Za-z0-9_]*'(?:''|[^'])*'$/.test(text)
      || /^`(?:``|[^`])*`$/.test(text)
      || /^\|[\s\S]*\|$/.test(text)
    ) {
      return true;
    }
    return /^(?:ABAP_TRUE|ABAP_FALSE|ABAP_UNDEFINED)$/i.test(text);
  }

  function isTemplateStaticOperandToken(value) {
    return /^(?:INITIAL|TABLE|LINE|LINES|ADJACENT|DUPLICATES)$/i.test(
      String(value === undefined || value === null ? "" : value).trim()
    );
  }

  function isTemplateIdentifierOperand(value) {
    const text = String(value === undefined || value === null ? "" : value).trim();
    if (!text || isTemplateLiteralOrWildcard(text) || isTemplateStaticOperandToken(text)) {
      return false;
    }
    return /^(?:@)?(?:<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)*$/.test(text)
      || /^(?:@)?(?:<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)/.test(text);
  }

  function isTemplateDataValueEntry(sourceObj, entry) {
    if (!sourceObj || !entry || typeof entry !== "object") {
      return false;
    }
    const objectType = String(sourceObj.objectType || "").trim().toUpperCase();
    const entryName = String(entry.name || entry.label || "").trim().toLowerCase();
    const allowedByType = {
      APPEND: ["what", "to", "target", "source", "assigning", "refinto"],
      ASSIGNMENT: ["target", "expr", "source", "value"],
      CLEAR: ["target", "with"],
      DELETE_ITAB: ["target", "itab", "from", "to", "index"],
      INSERT_ITAB: ["what", "to", "into", "target", "source", "intotable", "into-table", "index", "assigning", "refinto"],
      LOOP_AT_ITAB: ["itab", "into", "assigning", "refinto", "referenceinto", "reference-into", "from", "to"],
      MESSAGE: ["message", "id", "messagetype", "number", "displaylike", "into"],
      MODIFY_ITAB: ["itab", "itabordbtab", "target", "from", "assigning", "referenceinto", "reference-into", "index"],
      MOVE: ["source", "to", "target"],
      "MOVE-CORRESPONDING": ["source", "to", "target"],
      READ_TABLE: ["itab", "into", "assigning", "refinto", "referenceinto", "reference-into", "index"],
      SELECT: ["into", "intotable", "into-table", "appendingtable", "appending-table", "forallentries", "for-all-entries", "assigning", "refinto", "referenceinto", "reference-into"],
      SORT_ITAB: ["itab"],
      WRITE: ["output", "destination"],
      CONCATENATE: ["sources", "into", "separatedby", "separated-by", "linesof", "lines-of"]
    };
    const allowed = allowedByType[objectType];
    if (!Array.isArray(allowed) || !allowed.includes(entryName)) {
      return false;
    }
    if (
      objectType === "MESSAGE"
      && sourceObj.extras
      && sourceObj.extras.message
      && ["shorthand", "reference"].includes(String(sourceObj.extras.message.mode || ""))
      && ["message", "id", "messagetype", "number"].includes(entryName)
    ) {
      return false;
    }
    return true;
  }

  function getTemplateCanonicalObjectPathBase(obj) {
    if (typeof buildObjectPathBase === "function") {
      return buildObjectPathBase(obj);
    }
    const id = obj && obj.id !== undefined && obj.id !== null ? String(obj.id).trim() : "";
    if (id) {
      return `OBJECT:${id}`;
    }
    const objectType = String(obj && obj.objectType || "OBJECT").trim() || "OBJECT";
    const file = String(obj && obj.file || "NO_FILE").trim() || "NO_FILE";
    const line = Number(obj && obj.lineStart) || 0;
    return `OBJECT:${objectType}:${file}:${line}`;
  }

  function ensureTemplateCanonicalValueEntry(sourceObj, entry, objectIndexOneBased) {
    if (!entry || typeof entry !== "object" || isDeclLikeObject(entry.decl)) {
      return entry;
    }
    const valueText = String(entry.value === undefined || entry.value === null ? "" : entry.value).trim();
    if (!isTemplateDataValueEntry(sourceObj, entry) || !isTemplateIdentifierOperand(valueText)) {
      return entry;
    }
    if (typeof ensureEntryDeclWithSynthetic !== "function") {
      return entry;
    }
    const source = typeof getDeclSourceContextFromObject === "function"
      ? getDeclSourceContextFromObject(sourceObj)
      : {
          file: String(sourceObj && sourceObj.file || ""),
          lineStart: Number(sourceObj && sourceObj.lineStart) || null,
          raw: String(sourceObj && sourceObj.raw || "")
        };
    const entryName = String(entry.name || entry.label || "value").trim() || "value";
    const normalized = ensureEntryDeclWithSynthetic({ ...entry, decl: null }, {
      pathKey: [getTemplateCanonicalObjectPathBase(sourceObj), "values", entryName].join("/"),
      file: source.file,
      lineStart: source.lineStart,
      raw: source.raw,
      role: "value"
    });
    if (normalized && isTemplatePathDecl(normalized.decl)) {
      attachTemplateSyntheticDeclAliases(normalized.decl, objectIndexOneBased);
    }
    return normalized;
  }

  function attachTemplateEntrySyntheticAliases(entry, objectIndexOneBased) {
    if (!entry || typeof entry !== "object") {
      return entry;
    }
    for (const key of ["decl", "valueDecl", "leftOperandDecl", "rightOperandDecl"]) {
      if (isTemplatePathDecl(entry[key])) {
        attachTemplateSyntheticDeclAliases(entry[key], objectIndexOneBased);
      }
    }
    return entry;
  }

  function ensureTemplateCanonicalExtrasValueEntry(sourceObj, entry, options) {
    if (!entry || typeof entry !== "object") {
      return entry;
    }
    const opts = options && typeof options === "object" ? options : {};
    const valueText = String(entry.value === undefined || entry.value === null ? "" : entry.value).trim();
    if (!isTemplateIdentifierOperand(valueText)) {
      return entry;
    }
    const source = typeof getDeclSourceContextFromObject === "function"
      ? getDeclSourceContextFromObject(sourceObj)
      : {
          file: String(sourceObj && sourceObj.file || ""),
          lineStart: Number(sourceObj && sourceObj.lineStart) || null,
          raw: String(sourceObj && sourceObj.raw || "")
        };
    const basePath = [
      getTemplateCanonicalObjectPathBase(sourceObj),
      "extras",
      String(opts.extrasScope || "extras"),
      String(opts.sectionName || "section"),
      `item[${Math.max(1, Number(opts.indexOneBased) || 1)}]`
    ].join("/");
    let normalized = { ...entry };
    if (typeof ensureEntryDeclWithSynthetic === "function") {
      normalized = ensureEntryDeclWithSynthetic(normalized, {
        pathKey: basePath,
        file: source.file,
        lineStart: source.lineStart,
        raw: source.raw,
        role: `${opts.extrasScope || "extras"}:${opts.sectionName || "entry"}`
      });
    }
    if (typeof ensureValueDeclWithSynthetic === "function") {
      normalized = ensureValueDeclWithSynthetic(normalized, {
        pathKey: basePath,
        file: source.file,
        lineStart: source.lineStart,
        raw: source.raw,
        role: `${opts.extrasScope || "extras"}:${opts.sectionName || "entry"}:value`,
        nameHint: String(opts.sectionName || "value")
      });
    }
    return attachTemplateEntrySyntheticAliases(normalized, opts.objectIndexOneBased);
  }

  function ensureTemplateCanonicalConditionClause(sourceObj, clause, options) {
    if (!clause || typeof clause !== "object" || typeof ensureConditionClauseDeclsWithSynthetic !== "function") {
      return clause;
    }
    const opts = options && typeof options === "object" ? options : {};
    const source = typeof getDeclSourceContextFromObject === "function"
      ? getDeclSourceContextFromObject(sourceObj)
      : {
          file: String(sourceObj && sourceObj.file || ""),
          lineStart: Number(sourceObj && sourceObj.lineStart) || null,
          raw: String(sourceObj && sourceObj.raw || "")
        };
    const clausePath = [
      getTemplateCanonicalObjectPathBase(sourceObj),
      "extras",
      String(opts.extrasScope || "extras"),
      String(opts.sectionName || "conditions"),
      `clause[${Math.max(1, Number(opts.indexOneBased) || 1)}]`
    ].join("/");
    const normalized = ensureConditionClauseDeclsWithSynthetic(clause, {
      pathKey: clausePath,
      file: source.file,
      lineStart: source.lineStart,
      raw: source.raw
    });
    return attachTemplateEntrySyntheticAliases(normalized, opts.objectIndexOneBased);
  }

  function getTemplateNoDeclReason(value, isDataOperand, isSchemaValue) {
    if (isSchemaValue || isTemplateStaticOperandToken(value)) {
      return "NON_DECL_SCHEMA_VALUE";
    }
    if (isTemplateLiteralOrWildcard(value)) {
      return "LITERAL_NO_DECL";
    }
    if (isDataOperand && isTemplateIdentifierOperand(value)) {
      return "UNBOUND_IDENTIFIER";
    }
    return "MISSING_PROVENANCE";
  }

  function getPerformSourceBindingContextForTemplate(obj) {
    if (!obj || typeof obj !== "object") {
      return null;
    }
    const bindingContext = obj[PERFORM_TRACE_META_KEY_TEMPLATE];
    if (!bindingContext || typeof bindingContext !== "object") {
      return null;
    }
    if (!(bindingContext.byParamUpper instanceof Map)) {
      return null;
    }
    return bindingContext;
  }

  function isPerformSourceTemplateTraceableDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    const objectType = String(decl.objectType || "").toUpperCase();
    if (objectType === "FORM_PARAM" || objectType === "METHOD_PARAM") {
      return true;
    }
    return objectType === "STRUCT_FIELD"
      && ["FORM_PARAM", "METHOD_PARAM"].includes(String(decl.structObjectType || "").toUpperCase())
      && String(decl.structName || "").trim() !== ""
      && String(decl.fieldPath || "").trim() !== "";
  }

  function getPerformSourceTemplateParamUpper(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = String(decl.objectType || "").toUpperCase();
    if (objectType === "FORM_PARAM" || objectType === "METHOD_PARAM") {
      return String(decl.name || "").trim().toUpperCase();
    }
    if (objectType === "STRUCT_FIELD" && ["FORM_PARAM", "METHOD_PARAM"].includes(String(decl.structObjectType || "").toUpperCase())) {
      return String(decl.structName || "").trim().toUpperCase();
    }
    return "";
  }

  function buildPerformSourceTemplateTraceDecl(baseDecl, localDecl, ownerContext) {
    if (!baseDecl || typeof baseDecl !== "object") {
      return null;
    }
    if (!localDecl || typeof localDecl !== "object" || String(localDecl.objectType || "").toUpperCase() !== "STRUCT_FIELD") {
      return baseDecl;
    }

    const localFieldPath = String(localDecl.fieldPath || "").trim();
    if (!localFieldPath) {
      return baseDecl;
    }

    let rootBaseDecl = baseDecl;
    let rootStructName = String(baseDecl.name || "").trim();
    let prefixFieldPath = "";
    if (String(baseDecl.objectType || "").toUpperCase() === "STRUCT_FIELD") {
      rootStructName = String(baseDecl.structName || rootStructName).trim();
      prefixFieldPath = String(baseDecl.fieldPath || "").trim();
      rootBaseDecl = {
        ...baseDecl,
        id: baseDecl.structId || baseDecl.id || null,
        objectType: String(baseDecl.structObjectType || "STRUCT"),
        name: rootStructName,
        lineStart: Number(baseDecl.structLineStart || baseDecl.lineStart) || null,
        raw: String(baseDecl.structRaw || baseDecl.raw || ""),
        comment: String(baseDecl.structComment || baseDecl.comment || "")
      };
    }

    if (!rootStructName || !String(rootBaseDecl.scopeLabel || "").trim()) {
      return baseDecl;
    }

    const combinedFieldPath = prefixFieldPath ? (prefixFieldPath + "-" + localFieldPath) : localFieldPath;
    const candidate = {
      fullRef: rootStructName + "-" + combinedFieldPath,
      structName: rootStructName,
      fieldPath: combinedFieldPath
    };
    const traceContext = ownerContext && typeof ownerContext === "object"
      ? { file: ownerContext.file, lineStart: ownerContext.lineStart }
      : { file: rootBaseDecl.file, lineStart: rootBaseDecl.lineStart };
    if (typeof createSyntheticStructFieldDecl === "function") {
      const syntheticDecl = createSyntheticStructFieldDecl(rootBaseDecl, candidate, traceContext);
      if (syntheticDecl && typeof syntheticDecl === "object") {
        return syntheticDecl;
      }
    }

    return {
      id: rootBaseDecl.id || null,
      objectType: "STRUCT_FIELD",
      name: candidate.fullRef,
      file: String(traceContext.file || rootBaseDecl.file || ""),
      lineStart: Number(traceContext.lineStart || rootBaseDecl.lineStart) || null,
      raw: String(rootBaseDecl.raw || ""),
      comment: "",
      scopeId: Number(rootBaseDecl.scopeId || 0) || 0,
      scopeLabel: String(rootBaseDecl.scopeLabel || ""),
      scopeType: String(rootBaseDecl.scopeType || ""),
      scopeName: String(rootBaseDecl.scopeName || ""),
      structId: rootBaseDecl.id || null,
      structName: rootStructName,
      structObjectType: String(rootBaseDecl.objectType || "STRUCT"),
      structLineStart: Number(rootBaseDecl.lineStart || 0) || null,
      structRaw: String(rootBaseDecl.raw || ""),
      structComment: String(rootBaseDecl.comment || ""),
      traceFile: String(traceContext.file || ""),
      traceLineStart: Number(traceContext.lineStart || 0) || null,
      fieldPath: combinedFieldPath,
      synthetic: true
    };
  }

  function resolvePerformSourceTemplateTraceDecls(ownerContext, decl) {
    if (!isPerformSourceTemplateTraceableDecl(decl)) {
      return [];
    }
    const bindingContext = getPerformSourceBindingContextForTemplate(ownerContext);
    if (!bindingContext) {
      return [];
    }
    const paramUpper = getPerformSourceTemplateParamUpper(decl);
    if (!paramUpper) {
      return [];
    }
    const traceDecls = bindingContext.byParamUpper.get(paramUpper);
    if (!Array.isArray(traceDecls) || !traceDecls.length) {
      return [];
    }
    const localDeclType = String(decl.objectType || "").toUpperCase();
    const remappedTraceDecls = localDeclType === "STRUCT_FIELD"
      ? traceDecls.map((traceDecl) => buildPerformSourceTemplateTraceDecl(traceDecl, decl, ownerContext)).filter(Boolean)
      : traceDecls;
    const scopedTraceDecls = typeof cloneDeclWithPerformChainOverride === "function"
      ? remappedTraceDecls.map((traceDecl) => cloneDeclWithPerformChainOverride(traceDecl, ownerContext, decl))
      : remappedTraceDecls;
    return dedupeTemplateDecls(scopedTraceDecls);
  }

  function selectPerformSourceTemplateRootDecl(traceDecls) {
    const list = Array.isArray(traceDecls) ? traceDecls : [];
    for (let index = list.length - 1; index >= 0; index -= 1) {
      const decl = list[index];
      if (!decl || typeof decl !== "object") {
        continue;
      }
      if (!isPerformSourceTemplateTraceableDecl(decl)) {
        return decl;
      }
    }
    return null;
  }

  function isTemplateOriginDeclPath(pathParts) {
    return (Array.isArray(pathParts) ? pathParts : []).some((part) => (
      String(part || "").trim().toLowerCase() === "origindecls"
    ));
  }

  function isTemplateValueEntryLikeObject(value) {
    if (!value || typeof value !== "object" || Array.isArray(value)) {
      return false;
    }
    if (!Object.prototype.hasOwnProperty.call(value, "decl")) {
      return false;
    }
    return (
      Object.prototype.hasOwnProperty.call(value, "value")
      || Object.prototype.hasOwnProperty.call(value, "declRef")
      || Object.prototype.hasOwnProperty.call(value, "name")
      || Object.prototype.hasOwnProperty.call(value, "label")
      || hasValueLevelDescFields(value)
    );
  }

  function remapTemplateDeclForPerformSource(value, ownerContext) {
    if (isDeclLikeObject(value) && isPerformSourceTemplateTraceableDecl(value)) {
      const directTraceDecls = resolvePerformSourceTemplateTraceDecls(ownerContext, value);
      return selectPerformSourceTemplateRootDecl(directTraceDecls) || value;
    }

    if (!isTemplateValueEntryLikeObject(value)) {
      return value;
    }

    const localDecl = value.decl;
    if (!isDeclLikeObject(localDecl) || !isPerformSourceTemplateTraceableDecl(localDecl)) {
      return value;
    }

    const externalTraceDecls = resolvePerformSourceTemplateTraceDecls(ownerContext, localDecl);
    if (!externalTraceDecls.length) {
      return value;
    }

    const rootTraceDecl = selectPerformSourceTemplateRootDecl(externalTraceDecls);
    if (!rootTraceDecl) {
      return value;
    }

    const existingOrigins = Array.isArray(value.originDecls) ? value.originDecls : [];
    const scopedLocalDecl = typeof cloneDeclWithPerformChainOverride === "function"
      ? cloneDeclWithPerformChainOverride(localDecl, ownerContext, localDecl)
      : localDecl;
    const scopedExistingOrigins = typeof cloneDeclWithPerformChainOverride === "function"
      ? existingOrigins.map((originDecl) => cloneDeclWithPerformChainOverride(originDecl, ownerContext, localDecl))
      : existingOrigins;
    const originDecls = dedupeTemplateDecls([scopedLocalDecl, ...externalTraceDecls, ...scopedExistingOrigins]);
    return {
      ...value,
      decl: rootTraceDecl,
      valueDecl: scopedLocalDecl,
      originDecls
    };
  }

  function flattenTemplateValueEntries(obj) {
    if (typeof getValueEntries === "function") {
      return getValueEntries(obj);
    }
    if (!obj || typeof obj !== "object") {
      return [];
    }
    const values = obj.values;
    if (Array.isArray(values)) {
      return values.filter((entry) => entry && typeof entry === "object");
    }
    if (!values || typeof values !== "object") {
      return [];
    }
    const out = [];
    for (const key of Object.keys(values)) {
      const entryOrList = values[key];
      if (Array.isArray(entryOrList)) {
        for (const entry of entryOrList) {
          if (entry && typeof entry === "object") {
            out.push(entry);
          }
        }
        continue;
      }
      if (entryOrList && typeof entryOrList === "object") {
        out.push(entryOrList);
      }
    }
    return out;
  }

  function normalizeTemplatePairToken(value) {
    return String(value || "").trim().toLowerCase();
  }

  function labelToCamelName(label) {
    const parts = String(label || "").trim().split("-").filter(Boolean);
    if (!parts.length) {
      return "";
    }
    return parts[0] + parts.slice(1).map((part) => part.charAt(0).toUpperCase() + part.slice(1)).join("");
  }

  const KEYWORD_LABEL_VALUE_ALIASES = {
    "call-function": ["name", "function-name"],
    "call-method": ["target", "name"],
    "delete-adjacent-duplicates": ["target"],
    "in-program": ["program"],
    "loop-at": ["itab"],
    "modify-table": ["itab", "itabOrDbtab"],
    "read-table": ["itab"],
    "with-key": ["withKey", "with-key"],
    "with-table-key": ["withTableKey", "with-table-key"],
    "reference-into": ["refInto", "reference-into"],
    "ref-to": ["refTo", "ref-to"],
    "form-name": ["form"],
    "function-name": ["name"],
    "method-name": ["name"],
    "param-name": ["name"],
    "var-name": ["name"],
    "range-name": ["name"],
    "fs-name": ["name"],
    "struct-name": ["name"],
    "type-name": ["type"],
    "like-name": ["like"],
    "default-value": ["default"],
    "memory-id": ["memoryId", "memory-id"],
    "modif-id": ["modifId", "modif-id"],
    "group-name": ["group"],
    "assign": ["expr"],
    "at": ["position"],
    "if": ["ifCondition", "if"],
    "to": ["to", "target"],
    "lines-of": ["source", "source-itab"],
    "source-itab": ["source"],
    "to-itab": ["to"],
    "transporting-no-fields": ["transportingNoFields", "transporting-no-fields"],
    "binary-search": ["binarySearch", "binary-search"],
    "from-index": ["from"],
    "when-others": ["branch"]
  };

  const STMT_PRIMARY_VALUE_NAME = {
    APPEND: "what",
    CALL_FUNCTION: "name",
    CASE: "expr",
    CLEAR: "target",
    CONSTANTS: "name",
    DATA: "name",
    STATICS: "name",
    DELETE_ITAB: "target",
    DO: "times",
    PARAMETERS: "name",
    "SELECT-OPTIONS": "name",
    RANGES: "name",
    "FIELD-SYMBOLS": "name",
    TYPES: "name",
    READ_TABLE: "itab",
    LOOP_AT_ITAB: "itab",
    MODIFY_ITAB: "itabOrDbtab",
    "MOVE-CORRESPONDING": "source",
    INSERT_ITAB: "what",
    IF: "condition",
    ELSEIF: "condition",
    PERFORM: "form",
    FORM: "name",
    SELECT: "fields",
    SORT_ITAB: "itab",
    WHEN: "branch",
    CALL_METHOD: "target",
    CALL_TRANSACTION: "tcode",
    MESSAGE: "message",
    MOVE: "source",
    "MOVE-CORRESPONDING": "source",
    CLEAR: "target",
    METHOD: "name",
    WRITE: "output",
    CONCATENATE: "sources"
  };

  function keywordPositionInRaw(keyword, raw) {
    const text = String(keyword && keyword.text ? keyword.text : "").trim();
    if (!text) {
      return Number.MAX_SAFE_INTEGER;
    }
    const upperRaw = String(raw || "").toUpperCase();
    const upperText = text.toUpperCase();
    const idx = upperRaw.indexOf(upperText);
    return idx >= 0 ? idx : Number.MAX_SAFE_INTEGER;
  }

  function sortKeywordEntriesByRawPosition(keywords, raw) {
    const list = Array.isArray(keywords) ? keywords : [];
    return list
      .map((keyword, index) => ({
        keyword,
        index,
        position: keywordPositionInRaw(keyword, raw)
      }))
      .sort((left, right) => left.position - right.position || left.index - right.index)
      .map((item) => item.keyword);
  }

  function findValueEntryForKeyword(keyword, valueEntries, objectType) {
    const label = normalizeTemplatePairToken(keyword && keyword.label);
    if (!label) {
      return null;
    }

    const findByNameOrLabel = (token) => {
      const norm = normalizeTemplatePairToken(token);
      if (!norm) {
        return null;
      }
      return valueEntries.find((entry) =>
        normalizeTemplatePairToken(entry && entry.name) === norm
        || normalizeTemplatePairToken(entry && entry.label) === norm
      ) || null;
    };

    let match = findByNameOrLabel(label);
    if (match) {
      return match;
    }

    match = findByNameOrLabel(labelToCamelName(label));
    if (match) {
      return match;
    }

    const aliases = KEYWORD_LABEL_VALUE_ALIASES[label];
    if (Array.isArray(aliases)) {
      for (const alias of aliases) {
        match = findByNameOrLabel(alias);
        if (match) {
          return match;
        }
      }
    }

    if (label === "stmt") {
      const primaryName = STMT_PRIMARY_VALUE_NAME[String(objectType || "").trim().toUpperCase()] || "name";
      match = findByNameOrLabel(primaryName);
      if (match) {
        return match;
      }
      return valueEntries[0] || null;
    }

    return null;
  }

  function flattenTemplateKeywordEntries(obj) {
    if (typeof getKeywordEntries === "function") {
      return getKeywordEntries(obj);
    }
    if (!obj || typeof obj !== "object") {
      return [];
    }
    const keywords = obj.keywords;
    if (Array.isArray(keywords)) {
      return keywords.filter((entry) => entry && typeof entry === "object");
    }
    if (!keywords || typeof keywords !== "object") {
      return [];
    }
    const out = [];
    for (const key of Object.keys(keywords)) {
      const entryOrList = keywords[key];
      if (Array.isArray(entryOrList)) {
        for (const entry of entryOrList) {
          if (entry && typeof entry === "object") {
            out.push(entry);
          }
        }
        continue;
      }
      if (entryOrList && typeof entryOrList === "object") {
        out.push(entryOrList);
      }
    }
    return out;
  }

  function resolveTemplateValueRowFinalDesc(entry) {
    if (typeof resolveValueLevelFinalDesc === "function") {
      return String(resolveValueLevelFinalDesc(entry) || "").trim();
    }
    return "";
  }

  function createTemplateExpandedRow(text, declCandidates, provenance, keywordOverride) {
    const row = {
      text: String(text === undefined || text === null ? "" : text).trim(),
      declCandidates: dedupeTemplateDecls(declCandidates),
      provenance: provenance && typeof provenance === "object" ? { ...provenance } : null
    };
    const keyword = String(keywordOverride || "").trim();
    if (keyword) {
      row.keyword = keyword;
    }
    return row;
  }

  function createTemplateKeywordRow(keyword, finalDesc, declCandidates, provenance, semanticLabel) {
    const row = {
      keyword: String(keyword === undefined || keyword === null ? "" : keyword).trim(),
      label: normalizeTemplatePairToken(semanticLabel),
      finalDesc: String(finalDesc === undefined || finalDesc === null ? "" : finalDesc).trim()
    };
    Object.defineProperty(row, TEMPLATE_ROW_DECLS_META_KEY_TEMPLATE, {
      configurable: true,
      enumerable: false,
      value: {
        declCandidates: dedupeTemplateDecls(declCandidates),
        status: String(provenance && provenance.status || ""),
        reasonCode: String(provenance && provenance.reasonCode || "")
      }
    });
    return row;
  }

  function getTemplateKeywordRowProvenance(row) {
    if (!row || typeof row !== "object") {
      return { declCandidates: [], status: "unresolved", reasonCode: "MISSING_PROVENANCE" };
    }
    const meta = row[TEMPLATE_ROW_DECLS_META_KEY_TEMPLATE];
    if (Array.isArray(meta)) {
      return {
        declCandidates: dedupeTemplateDecls(meta),
        status: meta.length ? "editable" : "unresolved",
        reasonCode: meta.length ? "" : "MISSING_PROVENANCE"
      };
    }
    if (!meta || typeof meta !== "object") {
      return { declCandidates: [], status: "unresolved", reasonCode: "MISSING_PROVENANCE" };
    }
    const declCandidates = dedupeTemplateDecls(meta.declCandidates);
    return {
      declCandidates,
      status: String(meta.status || (declCandidates.length ? "editable" : "unresolved")),
      reasonCode: String(meta.reasonCode || (declCandidates.length ? "" : "MISSING_PROVENANCE"))
    };
  }

  function collectTemplateTraceAwareDeclCandidates(decl, ownerContext) {
    if (!decl || typeof decl !== "object") {
      return [];
    }
    const traceDecls = resolvePerformSourceTemplateTraceDecls(ownerContext, decl);
    const rootDecl = selectPerformSourceTemplateRootDecl(traceDecls) || decl;
    const scopedLocalDecl = typeof cloneDeclWithPerformChainOverride === "function"
      ? cloneDeclWithPerformChainOverride(decl, ownerContext, decl)
      : decl;
    return dedupeTemplateDecls([rootDecl, scopedLocalDecl, ...traceDecls]);
  }

  function buildTemplateSemanticValueEntry(entry, ownerContext) {
    if (!entry || typeof entry !== "object") {
      return null;
    }

    const valueDecl = entry.valueDecl && typeof entry.valueDecl === "object"
      ? entry.valueDecl
      : (entry.decl && typeof entry.decl === "object" ? entry.decl : null);
    const valueEntry = {
      ...entry,
      value: String(entry.value === undefined || entry.value === null ? "" : entry.value),
      userDesc: String(entry.userDesc || ""),
      codeDesc: String(entry.codeDesc || ""),
      declRef: String(entry.valueRef || entry.declRef || ""),
      decl: valueDecl
    };
    let traceAwareEntry = remapTemplateDeclForPerformSource(valueEntry, ownerContext);

    if (traceAwareEntry && isPerformSourceTemplateTraceableDecl(traceAwareEntry.decl)) {
      const rootFromOrigins = selectPerformSourceTemplateRootDecl(entry.originDecls);
      if (rootFromOrigins) {
        traceAwareEntry = {
          ...traceAwareEntry,
          decl: rootFromOrigins
        };
      }
    }

    return traceAwareEntry;
  }

  function buildTemplateSemanticValueRow(entry, ownerContext, provenanceOptions) {
    if (!entry || typeof entry !== "object") {
      return createTemplateExpandedRow("", []);
    }
    const sourceEntry = provenanceOptions
      ? ensureTemplateCanonicalExtrasValueEntry(ownerContext, entry, provenanceOptions)
      : entry;
    const rawValue = String(sourceEntry.value === undefined || sourceEntry.value === null ? "" : sourceEntry.value).trim();
    const valueEntry = buildTemplateSemanticValueEntry(sourceEntry, ownerContext);
    const declCandidates = valueEntry
      ? getTemplateEditableDeclCandidatesFromResolvedValue(valueEntry)
      : [];
    if (valueEntry && valueEntry.decl) {
      const resolved = resolveTemplateValueRowFinalDesc(valueEntry);
      if (resolved) {
        return createTemplateExpandedRow(resolved, declCandidates, {
          status: "editable",
          reasonCode: ""
        });
      }
    }
    return createTemplateExpandedRow(rawValue, declCandidates, {
      status: declCandidates.length ? "editable" : "not_applicable",
      reasonCode: declCandidates.length ? "" : getTemplateNoDeclReason(rawValue, true, false)
    });
  }

  function formatTemplateAssignmentRow(entry, ownerContext, provenanceOptions) {
    if (!entry || typeof entry !== "object") {
      return createTemplateExpandedRow("", []);
    }
    const name = String(entry.name || "").trim();
    const valueRow = buildTemplateSemanticValueRow(entry, ownerContext, provenanceOptions);
    const valueText = valueRow.text;
    if (name && valueText) {
      return createTemplateExpandedRow(`${name} = ${valueText}`, valueRow.declCandidates, valueRow.provenance);
    }
    return createTemplateExpandedRow(name || valueText, valueRow.declCandidates, valueRow.provenance);
  }

  function formatTemplateConditionRow(clause, ownerContext) {
    if (!clause || typeof clause !== "object") {
      return createTemplateExpandedRow("", []);
    }

    const resolveOperand = (operandKey, declKey) => {
      const rawOperand = String(clause[operandKey] || "").trim();
      const localDecl = clause[declKey];
      if (!localDecl || typeof localDecl !== "object") {
        return createTemplateExpandedRow(rawOperand, []);
      }
      const rootDecl = remapTemplateDeclForPerformSource(localDecl, ownerContext);
      const traceAwareClause = rootDecl === localDecl ? clause : { ...clause, [declKey]: rootDecl };
      const text = String(resolveConditionOperandFinalDesc(traceAwareClause, declKey, rootDecl) || rawOperand).trim();
      return createTemplateExpandedRow(text, collectTemplateTraceAwareDeclCandidates(localDecl, ownerContext));
    };

    const left = resolveOperand("leftOperand", "leftOperandDecl");
    const operator = String(clause.comparisonOperator || "").trim();
    const right = resolveOperand("rightOperand", "rightOperandDecl");
    const conditionText = [left.text, operator, right.text].filter(Boolean).join(" ").trim();
    const connector = String(clause.logicalConnector || "").trim();
    return createTemplateExpandedRow(
      connector && conditionText ? `${conditionText} ${connector}` : conditionText,
      [...left.declCandidates, ...right.declCandidates]
    );
  }

  function getTemplateConditionRows(sourceObj, keywordLabel, ownerContext, objectIndexOneBased) {
    const objectType = String(sourceObj && sourceObj.objectType || "").trim().toUpperCase();
    const extras = sourceObj && sourceObj.extras && typeof sourceObj.extras === "object"
      ? sourceObj.extras
      : {};
    let conditions = null;
    let extrasScope = "extras";
    let sectionName = "conditions";

    if (objectType === "PERFORM" && keywordLabel === "if" && extras.performCall) {
      conditions = extras.performCall.ifConditions;
      extrasScope = "performCall";
      sectionName = "ifConditions";
    } else if (objectType === "SELECT" && extras.select) {
      if (keywordLabel === "where") {
        conditions = extras.select.whereConditions;
        extrasScope = "select.where";
        sectionName = "whereConditions";
      } else if (keywordLabel === "having") {
        conditions = extras.select.havingConditions;
        extrasScope = "select.having";
        sectionName = "havingConditions";
      }
    } else if (objectType === "READ_TABLE" && ["with-key", "with-table-key"].includes(keywordLabel) && extras.readTable) {
      conditions = extras.readTable.conditions;
      extrasScope = "readTable";
    } else if (objectType === "LOOP_AT_ITAB" && keywordLabel === "where" && extras.loopAtItab) {
      conditions = extras.loopAtItab.conditions;
      extrasScope = "loopAtItab";
    } else if (objectType === "MODIFY_ITAB" && keywordLabel === "where" && extras.modifyItab) {
      conditions = extras.modifyItab.conditions;
      extrasScope = "modifyItab";
    } else if (objectType === "DELETE_ITAB" && keywordLabel === "where" && extras.deleteItab) {
      conditions = extras.deleteItab.conditions;
      extrasScope = "deleteItab";
    }

    const list = Array.isArray(conditions) ? conditions : [];
    return list.length ? list.map((clause, index) => formatTemplateConditionRow(
      ensureTemplateCanonicalConditionClause(sourceObj, clause, {
        extrasScope,
        sectionName,
        indexOneBased: index + 1,
        objectIndexOneBased
      }),
      ownerContext
    )) : null;
  }

  function buildTemplateWritePositionRows(sourceObj, write, ownerContext, objectIndexOneBased) {
    if (!write || typeof write !== "object") {
      return null;
    }
    const position = write.position && typeof write.position === "object" ? write.position : {};
    const buildPart = (name) => {
      const value = String(position[name] || "").trim();
      if (!value) {
        return createTemplateExpandedRow("", []);
      }
      return buildTemplateSemanticValueRow({
        value,
        valueRef: position[`${name}Ref`],
        valueDecl: position[`${name}Decl`]
      }, ownerContext, {
        extrasScope: "write",
        sectionName: `position.${name}`,
        indexOneBased: 1,
        objectIndexOneBased
      });
    };
    const column = buildPart("column");
    const length = buildPart("length");
    const hasColumn = Boolean(String(column.text || "").trim());
    const hasLength = Boolean(String(length.text || "").trim());
    const rows = [];

    // Newline-only WRITE / … → flag row with empty value (keyword text comes from synthetic "/").
    if (!hasColumn && !hasLength) {
      if (!write.newLine) {
        return null;
      }
      rows.push(createTemplateExpandedRow("", [], {
        status: "not_applicable",
        reasonCode: "NON_DECL_SCHEMA_VALUE"
      }));
      return rows;
    }

    // Do not prefix "/" into values — newline is already the keyword when synthetic at text is "/".
    if (hasColumn) {
      rows.push(column);
    }
    if (hasLength) {
      rows.push(createTemplateExpandedRow(
        length.text,
        length.declCandidates,
        length.provenance,
        "LENGTH"
      ));
    }
    return rows.length ? rows : null;
  }

  function getTemplateSemanticSectionRows(sourceObj, keywordLabel, ownerContext, objectIndexOneBased, keywordText) {
    const extras = sourceObj && sourceObj.extras && typeof sourceObj.extras === "object"
      ? sourceObj.extras
      : {};

    if (extras.message && keywordLabel === "with") {
      const withValues = Array.isArray(extras.message.with) ? extras.message.with : [];
      if (withValues.length) {
        return withValues.map((entry, index) => buildTemplateSemanticValueRow(entry, ownerContext, {
          extrasScope: "message",
          sectionName: "with",
          indexOneBased: index + 1,
          objectIndexOneBased
        }));
      }
    }

    if (extras.write) {
      if (keywordLabel === "at") {
        return buildTemplateWritePositionRows(sourceObj, extras.write, ownerContext, objectIndexOneBased);
      }
      const format = Array.isArray(extras.write.format) ? extras.write.format : [];
      const formatEntry = format.find((entry) => (
        normalizeTemplatePairToken(entry && entry.keyword).replace(/\s+/g, "-") === keywordLabel
      ));
      if (formatEntry) {
        if (!String(formatEntry.value || "").trim()) {
          return [createTemplateExpandedRow("", [], {
            status: "not_applicable",
            reasonCode: "NON_DECL_SCHEMA_VALUE"
          })];
        }
        return [buildTemplateSemanticValueRow(formatEntry, ownerContext, {
          extrasScope: "write",
          sectionName: "format",
          indexOneBased: format.indexOf(formatEntry) + 1,
          objectIndexOneBased
        })];
      }
    }

    const objectType = String(sourceObj && sourceObj.objectType || "").trim().toUpperCase();
    if (objectType === "SELECT" && extras.select) {
      const isModernSelect = isTemplateModernSelect(sourceObj);
      const normalizedKeywordText = String(keywordText || "").trim().toUpperCase();
      if (keywordLabel === "fields" || keywordLabel === "stmt") {
        const fields = Array.isArray(extras.select.fields) ? extras.select.fields : [];
        if (fields.length) {
          const isFieldListKeyword = (isModernSelect && normalizedKeywordText === "FIELDS")
            || (!isModernSelect && normalizedKeywordText === "SELECT");
          if (isFieldListKeyword) {
            return buildTemplateSemanticListRows(sourceObj, fields, ownerContext, {
              extrasScope: "select",
              sectionName: "fields",
              sqlSchema: true
            }, objectIndexOneBased);
          }
          // SELECT ... FIELDS has two keyword positions. Only the position that owns
          // the field list may render rows, otherwise every field is duplicated.
          return [];
        }
      }
      if (keywordLabel === "group-by" || keywordLabel === "group") {
        const groupBy = Array.isArray(extras.select.groupBy) ? extras.select.groupBy : [];
        if (groupBy.length) {
          return buildTemplateSemanticListRows(sourceObj, groupBy, ownerContext, {
            extrasScope: "select",
            sectionName: "groupBy",
            sqlSchema: true
          }, objectIndexOneBased);
        }
      }
      if (keywordLabel === "order-by" || keywordLabel === "order") {
        const orderBy = Array.isArray(extras.select.orderBy) ? extras.select.orderBy : [];
        if (orderBy.length) {
          return buildTemplateSemanticListRows(sourceObj, orderBy, ownerContext, {
            extrasScope: "select",
            sectionName: "orderBy",
            sqlSchema: true,
            formatItem: formatTemplateOrderByItem
          }, objectIndexOneBased);
        }
      }
    }

    if (objectType === "READ_TABLE" && extras.readTable) {
      const atomicReadOptionLabels = new Set([
        "comparing-no-fields",
        "comparing-all-fields",
        "transporting-all-fields"
      ]);
      if (atomicReadOptionLabels.has(keywordLabel)) {
        return [createTemplateExpandedRow("", [], {
          status: "not_applicable",
          reasonCode: "NON_DECL_SCHEMA_VALUE"
        })];
      }
      const atomicOption = keywordLabel === "comparing"
        ? String(extras.readTable.comparingRaw || "").trim().toUpperCase()
        : keywordLabel === "transporting"
          ? String(extras.readTable.transportingRaw || "").trim().toUpperCase()
          : "";
      if (["NO FIELDS", "ALL FIELDS"].includes(atomicOption)) {
        const phrase = `${keywordLabel.toUpperCase()} ${atomicOption}`;
        return [createTemplateExpandedRow("", [], {
          status: "not_applicable",
          reasonCode: "NON_DECL_SCHEMA_VALUE"
        }, phrase)];
      }
      if (
        ["no", "fields"].includes(keywordLabel)
        && /\b(?:COMPARING\s+(?:NO|ALL)\s+FIELDS|TRANSPORTING\s+ALL\s+FIELDS)\b/i.test(String(sourceObj.raw || ""))
      ) {
        return [];
      }
      if (keywordLabel === "comparing" || keywordLabel === "transporting") {
        const entries = Array.isArray(extras.readTable[keywordLabel]) ? extras.readTable[keywordLabel] : [];
        if (entries.length) {
          return buildTemplateSemanticListRows(sourceObj, entries, ownerContext, {
            extrasScope: "readTable",
            sectionName: keywordLabel,
            resolveItabComponent: true
          }, objectIndexOneBased);
        }
      }
    }

    if (objectType === "MODIFY_ITAB" && extras.modifyItab && keywordLabel === "transporting") {
      const transporting = Array.isArray(extras.modifyItab.transporting) ? extras.modifyItab.transporting : [];
      if (transporting.length) {
        return buildTemplateSemanticListRows(sourceObj, transporting, ownerContext, {
          extrasScope: "modifyItab",
          sectionName: "transporting",
          resolveItabComponent: true
        }, objectIndexOneBased);
      }
    }

    if (objectType === "DELETE_ITAB" && extras.deleteItab && keywordLabel === "comparing") {
      const comparing = Array.isArray(extras.deleteItab.comparing) ? extras.deleteItab.comparing : [];
      if (comparing.length) {
        return buildTemplateSemanticListRows(sourceObj, comparing, ownerContext, {
          extrasScope: "deleteItab",
          sectionName: "comparing",
          resolveItabComponent: true
        }, objectIndexOneBased);
      }
    }

    if (objectType === "SORT_ITAB" && extras.sortItab && keywordLabel === "stmt") {
      const globalDirection = String(extras.sortItab.direction || "").trim();
      const globalAsText = extras.sortItab.asText === true;
      if (globalDirection || globalAsText) {
        const itabEntry = flattenTemplateValueEntries(sourceObj).find((entry) => (
          normalizeTemplatePairToken(entry && entry.name) === "itab"
          || normalizeTemplatePairToken(entry && entry.label) === "itab"
        ));
        const itabRow = itabEntry
          ? buildTemplateSemanticValueRow(ensureTemplateCanonicalValueEntry(sourceObj, itabEntry, objectIndexOneBased), ownerContext)
          : createTemplateExpandedRow("", []);
        const rows = [createTemplateExpandedRow(
          itabRow.text || String(itabEntry && itabEntry.value || ""),
          itabRow.declCandidates || [],
          itabRow.provenance,
          "SORT"
        )];
        if (globalDirection) {
          rows.push(createTemplateExpandedRow("", [], {
            status: "not_applicable",
            reasonCode: "NON_DECL_SCHEMA_VALUE"
          }, globalDirection));
        }
        if (globalAsText) {
          rows.push(createTemplateExpandedRow("", [], {
            status: "not_applicable",
            reasonCode: "NON_DECL_SCHEMA_VALUE"
          }, "AS TEXT"));
        }
        return rows;
      }
    }

    if (objectType === "SORT_ITAB" && extras.sortItab && keywordLabel === "by") {
      const keys = Array.isArray(extras.sortItab.keys) ? extras.sortItab.keys : [];
      if (keys.length) {
        return buildTemplateSemanticListRows(sourceObj, keys, ownerContext, {
          extrasScope: "sortItab",
          sectionName: "keys",
          resolveItabComponent: true,
          formatItem: formatTemplateSortKeyItem
        }, objectIndexOneBased);
      }
      const dynamicBy = String(extras.sortItab.byRaw || "").trim();
      if (dynamicBy) {
        return [createTemplateExpandedRow(dynamicBy, [], {
          status: "not_applicable",
          reasonCode: "NON_DECL_SCHEMA_VALUE"
        })];
      }
    }
    if (objectType === "SORT_ITAB" && extras.sortItab && ["ascending", "descending", "as-text", "as", "text"].includes(keywordLabel)) {
      const globalDirection = String(extras.sortItab.direction || "").trim().toLowerCase();
      const globalAsText = extras.sortItab.asText === true;
      if (
        keywordLabel === globalDirection
        || (globalAsText && ["as-text", "as", "text"].includes(keywordLabel))
        || String(extras.sortItab.byRaw || "").trim()
      ) {
        // Global modifiers were emitted from SORT; per-key modifiers stay in BY rows.
        return [];
      }
    }

    if (objectType === "CONCATENATE" && extras.concatenate && (keywordLabel === "stmt" || keywordLabel === "sources")) {
      const sources = Array.isArray(extras.concatenate.sources) ? extras.concatenate.sources : [];
      if (sources.length) {
        return buildTemplateSemanticListRows(sourceObj, sources, ownerContext, {
          extrasScope: "concatenate",
          sectionName: "sources",
          legacyValueName: "sources"
        }, objectIndexOneBased);
      }
    }

    if (objectType === "WHEN" && extras.when && (keywordLabel === "stmt" || keywordLabel === "branch" || keywordLabel === "when-others")) {
      const branches = Array.isArray(extras.when.branches) ? extras.when.branches : [];
      if (branches.length) {
        return buildTemplateSemanticListRows(sourceObj, branches, ownerContext, {
          extrasScope: "when",
          sectionName: "branches",
          formatItem: formatTemplateWhenBranchItem
        }, objectIndexOneBased);
      }
    }

    if (objectType === "CATCH" && extras.catch && (keywordLabel === "stmt" || keywordLabel === "exception")) {
      const exceptions = Array.isArray(extras.catch.exceptions) ? extras.catch.exceptions : [];
      if (exceptions.length) {
        return buildTemplateSemanticListRows(sourceObj, exceptions, ownerContext, {
          extrasScope: "catch",
          sectionName: "exceptions",
          noDataBinding: true,
          formatItem: (entry, valueText, index) => [
            index === 0 ? String(extras.catch.modifier || "").trim() : "",
            valueText
          ].filter(Boolean).join(" ")
        }, objectIndexOneBased);
      }
    }

    const assignmentSection = extras.callFunction || extras.callMethod;
    if (assignmentSection && ["exporting", "importing", "changing", "tables", "receiving", "exceptions"].includes(keywordLabel)) {
      const assignments = Array.isArray(assignmentSection[keywordLabel]) ? assignmentSection[keywordLabel] : [];
      if (assignments.length) {
        const extrasScope = extras.callFunction ? "callFunction" : "callMethod";
        return assignments.map((entry, index) => formatTemplateAssignmentRow(entry, ownerContext, {
          extrasScope,
          sectionName: keywordLabel,
          indexOneBased: index + 1,
          objectIndexOneBased
        }));
      }
    }

    if (extras.performCall && ["using", "changing", "tables"].includes(keywordLabel)) {
      const values = Array.isArray(extras.performCall[keywordLabel]) ? extras.performCall[keywordLabel] : [];
      if (values.length) {
        return values.map((entry, index) => buildTemplateSemanticValueRow(entry, ownerContext, {
          extrasScope: "performCall",
          sectionName: keywordLabel,
          indexOneBased: index + 1,
          objectIndexOneBased
        }));
      }
    }

    const signature = extras.form || extras.methodSignature;
    if (signature && typeof signature === "object") {
      if (keywordLabel === "raising") {
        const exceptions = Array.isArray(signature.exceptions) ? signature.exceptions : [];
        if (exceptions.length) {
          return exceptions.map((entry) => createTemplateExpandedRow(
            String(entry && entry.name || "").trim(),
            getTemplateEditableDeclCandidatesFromResolvedValue(entry)
          ));
        }
      }

      const params = Array.isArray(signature.params) ? signature.params : [];
      const sectionParams = params.filter((param) => (
        String(param && param.section || "").trim().toLowerCase() === keywordLabel
      ));
      if (sectionParams.length) {
        return sectionParams.map((param) => {
          const docText = String(param && param.doc && param.doc.text || "").trim();
          return createTemplateExpandedRow(
            docText || String(param && param.name || "").trim(),
            getTemplateEditableDeclCandidatesFromResolvedValue(param)
          );
        });
      }
    }

    return null;
  }

  function splitTemplateTopLevelText(rawValue, mode) {
    const text = String(rawValue || "");
    const parts = [];
    let current = "";
    let quote = "";
    let roundDepth = 0;
    let squareDepth = 0;
    let curlyDepth = 0;

    const pushCurrent = () => {
      const value = current.replace(/\s+/g, " ").trim();
      if (value) {
        parts.push(value);
      }
      current = "";
    };

    for (let index = 0; index < text.length; index += 1) {
      const ch = text[index];
      const next = text[index + 1] || "";

      if (quote) {
        current += ch;
        if ((quote === "'" || quote === "`") && ch === quote && next === quote) {
          current += next;
          index += 1;
          continue;
        }
        if (quote === "|" && ch === "\\" && next) {
          current += next;
          index += 1;
          continue;
        }
        if (ch === quote) {
          quote = "";
        }
        continue;
      }

      if (ch === "'" || ch === "`" || ch === "|") {
        quote = ch;
        current += ch;
        continue;
      }
      if (ch === "(") {
        roundDepth += 1;
        current += ch;
        continue;
      }
      if (ch === ")") {
        roundDepth = Math.max(0, roundDepth - 1);
        current += ch;
        continue;
      }
      if (ch === "[") {
        squareDepth += 1;
        current += ch;
        continue;
      }
      if (ch === "]") {
        squareDepth = Math.max(0, squareDepth - 1);
        current += ch;
        continue;
      }
      if (ch === "{") {
        curlyDepth += 1;
        current += ch;
        continue;
      }
      if (ch === "}") {
        curlyDepth = Math.max(0, curlyDepth - 1);
        current += ch;
        continue;
      }

      const isTopLevel = roundDepth === 0 && squareDepth === 0 && curlyDepth === 0;
      if (isTopLevel && mode === "comma" && ch === ",") {
        pushCurrent();
        continue;
      }
      if (isTopLevel && mode === "space" && /\s/.test(ch)) {
        pushCurrent();
        continue;
      }
      current += ch;
    }

    pushCurrent();
    return parts;
  }

  function splitTemplateExpressionOperands(rawValue) {
    const text = String(rawValue || "").trim();
    if (!text) {
      return [];
    }

    const operands = [];
    let current = "";
    let quote = "";
    let roundDepth = 0;
    let squareDepth = 0;
    let curlyDepth = 0;

    const pushCurrent = () => {
      const part = current.trim();
      if (part) {
        operands.push(part);
      }
      current = "";
    };

    const isTopLevel = () => roundDepth === 0 && squareDepth === 0 && curlyDepth === 0;

    for (let index = 0; index < text.length; index += 1) {
      const ch = text[index];
      const next = text[index + 1] || "";

      if (quote) {
        current += ch;
        if ((quote === "'" || quote === "`") && ch === quote && next === quote) {
          current += next;
          index += 1;
          continue;
        }
        if (quote === "|" && ch === "\\" && next) {
          current += next;
          index += 1;
          continue;
        }
        if (ch === quote) {
          quote = "";
        }
        continue;
      }

      if (ch === "'" || ch === "`" || ch === "|") {
        quote = ch;
        current += ch;
        continue;
      }
      if (ch === "(") {
        roundDepth += 1;
        current += ch;
        continue;
      }
      if (ch === ")") {
        roundDepth = Math.max(0, roundDepth - 1);
        current += ch;
        continue;
      }
      if (ch === "[") {
        squareDepth += 1;
        current += ch;
        continue;
      }
      if (ch === "]") {
        squareDepth = Math.max(0, squareDepth - 1);
        current += ch;
        continue;
      }
      if (ch === "{") {
        curlyDepth += 1;
        current += ch;
        continue;
      }
      if (ch === "}") {
        curlyDepth = Math.max(0, curlyDepth - 1);
        current += ch;
        continue;
      }

      if (isTopLevel()) {
        if (ch === "&" && next === "&") {
          pushCurrent();
          index += 1;
          continue;
        }
        if (ch === "+" || ch === "*" || ch === "/") {
          pushCurrent();
          continue;
        }
        if (ch === "-") {
          const hasTrailingSpace = current.length > 0 && /\s$/.test(current);
          const trimmed = current.trim();
          const nextIsIdent = /[A-Za-z_<@]/.test(next);
          const endsWithIdent = /[A-Za-z0-9_>]$/.test(trimmed);
          // Keep structure/component paths like ids_inner-item or <fs>-field together.
          if (!hasTrailingSpace && endsWithIdent && nextIsIdent) {
            current += ch;
            continue;
          }
          const prev = trimmed.slice(-1);
          const unaryContext = !trimmed
            || /[+\-*/(&]$/.test(prev)
            || /\b(?:AND|OR|EQ|NE|LT|LE|GT|GE)$/i.test(trimmed);
          if (unaryContext) {
            current += ch;
            continue;
          }
          pushCurrent();
          continue;
        }
      }

      current += ch;
    }

    pushCurrent();
    return operands.length ? operands : [text];
  }

  function findTemplateTopLevelWord(rawValue, word, startIndex) {
    const text = String(rawValue || "");
    const upperWord = String(word || "").trim().toUpperCase();
    let quote = "";
    let roundDepth = 0;
    let squareDepth = 0;
    let curlyDepth = 0;

    for (let index = Math.max(0, Number(startIndex) || 0); index < text.length; index += 1) {
      const ch = text[index];
      const next = text[index + 1] || "";
      if (quote) {
        if ((quote === "'" || quote === "`") && ch === quote && next === quote) {
          index += 1;
          continue;
        }
        if (quote === "|" && ch === "\\" && next) {
          index += 1;
          continue;
        }
        if (ch === quote) {
          quote = "";
        }
        continue;
      }
      if (ch === "'" || ch === "`" || ch === "|") {
        quote = ch;
        continue;
      }
      if (ch === "(") {
        roundDepth += 1;
        continue;
      }
      if (ch === ")") {
        roundDepth = Math.max(0, roundDepth - 1);
        continue;
      }
      if (ch === "[") {
        squareDepth += 1;
        continue;
      }
      if (ch === "]") {
        squareDepth = Math.max(0, squareDepth - 1);
        continue;
      }
      if (ch === "{") {
        curlyDepth += 1;
        continue;
      }
      if (ch === "}") {
        curlyDepth = Math.max(0, curlyDepth - 1);
        continue;
      }
      if (roundDepth || squareDepth || curlyDepth) {
        continue;
      }

      const candidate = text.slice(index, index + upperWord.length).toUpperCase();
      const before = index > 0 ? text[index - 1] : "";
      const after = text[index + upperWord.length] || "";
      if (
        candidate === upperWord
        && !/[A-Za-z0-9_]/.test(before)
        && !/[A-Za-z0-9_]/.test(after)
      ) {
        return index;
      }
    }
    return -1;
  }

  function getTemplateSelectFieldSource(sourceObj, fallbackEntry) {
    const raw = String(sourceObj && sourceObj.raw || "");
    const selectMatch = raw.match(/^\s*SELECT\b/i);
    if (selectMatch) {
      const start = selectMatch[0].length;
      const fromIndex = findTemplateTopLevelWord(raw, "FROM", start);
      if (fromIndex > start) {
        const intoIndex = findTemplateTopLevelWord(raw, "INTO", start);
        const appendingIndex = findTemplateTopLevelWord(raw, "APPENDING", start);
        const clauseEnd = [fromIndex, intoIndex, appendingIndex]
          .filter((index) => index >= start)
          .reduce((earliest, index) => Math.min(earliest, index), fromIndex);
        return raw.slice(start, clauseEnd).replace(/^\s*(?:SINGLE|DISTINCT)\b\s*/i, "").trim();
      }
    }
    return String(fallbackEntry && fallbackEntry.value || "").trim();
  }

  function getTemplateModernSelectFieldsSource(sourceObj) {
    const raw = String(sourceObj && sourceObj.raw || "");
    const fieldsIndex = findTemplateTopLevelWord(raw, "FIELDS", 0);
    if (fieldsIndex < 0) {
      return "";
    }
    const start = fieldsIndex + "FIELDS".length;
    const stopWords = ["INTO", "APPENDING", "WHERE", "GROUP", "ORDER", "HAVING", "UP", "FOR", "UNION"];
    let end = raw.length;
    for (const word of stopWords) {
      const index = findTemplateTopLevelWord(raw, word, start);
      if (index >= 0 && index < end) {
        end = index;
      }
    }
    return raw.slice(start, end).replace(/^\s*DISTINCT\b\s*/i, "").trim();
  }

  function isTemplateSafeSimpleListItem(value) {
    const text = String(value || "").trim();
    if (!text || /^(?:AS|ASCENDING|CASE|CAST|DESCENDING|DISTINCT|END|FIELDS|NO|SINGLE|TEXT|THEN|WHEN)$/i.test(text)) {
      return false;
    }
    if (/^\(\s*[A-Za-z_][A-Za-z0-9_]*\s*\)$/.test(text)) {
      return true;
    }
    return /^(?:\*|@?(?:<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:(?:~|-)[A-Za-z_][A-Za-z0-9_]*|~\*)*)$/.test(text);
  }

  function splitTemplateSafeSimpleList(rawValue) {
    const items = splitTemplateTopLevelText(rawValue, "space");
    return items.length > 1 && items.every((item) => isTemplateSafeSimpleListItem(item)) ? items : null;
  }

  function splitTemplateSafeConcatenateList(rawValue) {
    const items = splitTemplateTopLevelText(rawValue, "space");
    const isSafeSource = (item) => isTemplateSafeSimpleListItem(item) || isTemplateLiteralOrWildcard(item);
    return items.length > 1 && items.every(isSafeSource) ? items : null;
  }

  function normalizeTemplateItabComponentToken(rawValue) {
    let text = String(rawValue || "").trim().replace(/^@+/, "");
    const parenMatch = text.match(/^\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)$/);
    if (parenMatch && parenMatch[1]) {
      text = parenMatch[1];
    }
    return text.trim();
  }

  function splitTemplateItabComponentList(rawValue) {
    const items = splitTemplateTopLevelText(rawValue, "space");
    const out = [];
    for (const item of items) {
      if (!isTemplateSafeSimpleListItem(item)) {
        continue;
      }
      const token = normalizeTemplateItabComponentToken(item);
      if (!token || /[-~>]/.test(token)) {
        continue;
      }
      if (!/^(?:table_line|[A-Za-z_][A-Za-z0-9_]*)$/i.test(token)) {
        continue;
      }
      out.push(token);
    }
    return out.length ? out : null;
  }

  function getTemplateStatementItabEntry(sourceObj) {
    if (!sourceObj || typeof sourceObj !== "object") {
      return null;
    }
    const objectType = String(sourceObj.objectType || "").trim().toUpperCase();
    const valueEntries = flattenTemplateValueEntries(sourceObj);
    const isDeleteAdjacentDuplicates = objectType === "DELETE_ITAB"
      && String(sourceObj && sourceObj.extras && sourceObj.extras.deleteItab && sourceObj.extras.deleteItab.variant || "") === "adjacentDuplicates";
    const wantedNames = objectType === "MODIFY_ITAB"
      ? ["itab", "itabOrDbtab"]
      : objectType === "DELETE_ITAB"
        ? (isDeleteAdjacentDuplicates ? ["from", "target", "itab"] : ["target", "itab", "from"])
        : ["itab"];
    for (const wanted of wantedNames) {
      const entry = valueEntries.find((item) => (
        normalizeTemplatePairToken(item && item.name) === normalizeTemplatePairToken(wanted)
        || normalizeTemplatePairToken(item && item.label) === normalizeTemplatePairToken(wanted)
      ));
      if (entry) {
        return entry;
      }
    }
    return null;
  }

  function getTemplateStatementItabName(sourceObj) {
    const entry = getTemplateStatementItabEntry(sourceObj);
    if (!entry) {
      return "";
    }
    const raw = String(entry.value || entry.name || "").trim().replace(/^@+/, "").split(/\s+/)[0] || "";
    if (/^<[^>]+>$/.test(raw) || /^[A-Za-z_][A-Za-z0-9_]*$/.test(raw)) {
      return raw;
    }
    return "";
  }

  function getTemplateStatementItabScopeHint(sourceObj, ownerContext) {
    const entry = getTemplateStatementItabEntry(sourceObj);
    const fromItabDecl = String(entry && entry.decl && entry.decl.scopeLabel || "").trim();
    if (fromItabDecl) {
      return fromItabDecl;
    }
    const fromOwner = String(ownerContext && ownerContext.scopeLabel || "").trim();
    if (fromOwner) {
      return fromOwner;
    }
    const hints = ownerContext && ownerContext.scopeHints;
    if (hints instanceof Set) {
      for (const hint of hints.values()) {
        const text = String(hint || "").trim();
        if (text) {
          return text;
        }
      }
    } else if (Array.isArray(hints) && hints.length) {
      const text = String(hints[0] || "").trim();
      if (text) {
        return text;
      }
    }
    return "";
  }

  function isTemplateModernSelect(sourceObj) {
    const raw = String(sourceObj && sourceObj.raw || "");
    const selectMatch = raw.match(/^\s*SELECT\b/i);
    if (!selectMatch) {
      return false;
    }
    const fromIndex = findTemplateTopLevelWord(raw, "FROM", selectMatch[0].length);
    const fieldsIndex = findTemplateTopLevelWord(raw, "FIELDS", selectMatch[0].length);
    return fromIndex >= 0 && fieldsIndex > fromIndex;
  }

  function formatTemplateOrderByItem(entry, valueText) {
    const direction = String(entry && entry.direction || "").trim();
    return [valueText, direction].filter(Boolean).join(" ");
  }

  function formatTemplateSortKeyItem(entry, valueText) {
    const direction = String(entry && entry.direction || "").trim();
    const asText = entry && (entry.asText === true || String(entry.asText || "").toUpperCase() === "AS TEXT");
    return [valueText, direction, asText ? "AS TEXT" : ""].filter(Boolean).join(" ");
  }

  function getTemplateItabComponentDisplayText(componentRow, componentDecl, rawValue) {
    const resolvedText = String(componentRow && componentRow.text || "").trim();
    const rawText = String(rawValue || "").trim();
    const technicalNames = new Set([rawText, String(componentDecl && componentDecl.name || "").trim()]
      .filter(Boolean)
      .map((value) => value.replace(/^@+/, "").toUpperCase()));
    for (const decl of Array.isArray(componentRow && componentRow.declCandidates) ? componentRow.declCandidates : []) {
      const name = String(decl && decl.name || "").trim().replace(/^@+/, "").toUpperCase();
      if (name) {
        technicalNames.add(name);
      }
    }
    return resolvedText && !technicalNames.has(resolvedText.replace(/^@+/, "").toUpperCase())
      ? resolvedText
      : rawText;
  }

  function formatTemplateWhenBranchItem(entry, valueText) {
    const kind = String(entry && entry.kind || "").trim().toUpperCase();
    if (kind === "OTHERS") {
      return "OTHERS";
    }
    const from = String(entry && entry.from || "").trim();
    const to = String(entry && (entry.to || entry.rangeEnd) || "").trim();
    const operator = String(entry && entry.operator || "").trim().toUpperCase();
    const connector = String(entry && entry.connector || "").trim();
    if ((kind === "THRU" || operator === "THRU") && (from || valueText) && to) {
      return [from || valueText, "THRU", to, connector].filter(Boolean).join(" ");
    }
    return [valueText, connector].filter(Boolean).join(" ");
  }

  function buildTemplateSemanticListRows(sourceObj, entries, ownerContext, options, objectIndexOneBased) {
    const list = Array.isArray(entries) ? entries : [];
    const opts = options && typeof options === "object" ? options : {};
    if (!list.length) {
      return null;
    }

    const itabName = opts.resolveItabComponent ? getTemplateStatementItabName(sourceObj) : "";
    const scopeHint = opts.resolveItabComponent
      ? getTemplateStatementItabScopeHint(sourceObj, ownerContext)
      : "";

    return list.map((item, index) => {
      const entry = item && typeof item === "object" ? item : { value: item };
      const rawValue = String(entry.value === undefined || entry.value === null ? "" : entry.value).trim();
      const canonicalEntry = (opts.sqlSchema || opts.noDataBinding) ? null : getTemplateCanonicalSemanticEntry(sourceObj, entry, rawValue);
      const semanticEntry = (opts.sqlSchema || opts.noDataBinding)
        ? { ...entry, decl: null, valueDecl: null, declRef: "" }
        : canonicalEntry || (opts.legacyValueName
        ? ensureTemplateCanonicalValueEntry(sourceObj, {
          ...entry,
          name: opts.legacyValueName,
          label: opts.legacyValueName,
          value: rawValue,
          decl: null
        }, objectIndexOneBased)
        : entry);
      const valueRow = buildTemplateSemanticValueRow(semanticEntry, ownerContext, canonicalEntry ? {
        extrasScope: String(opts.extrasScope || "extras"),
        sectionName: String(opts.sectionName || "items"),
        indexOneBased: index + 1,
        objectIndexOneBased
      } : null);
      const formatItem = typeof opts.formatItem === "function" ? opts.formatItem : null;
      const rangeEndRaw = String(entry.rangeEnd || entry.to || "").trim();
      const rangeEndDecl = rangeEndRaw
        ? lookupTemplateScopedValueDecl(rangeEndRaw, semanticEntry.decl || semanticEntry.valueDecl)
        : null;
      const rangeEndRow = rangeEndDecl
        ? buildTemplateSemanticValueRow({ value: rangeEndRaw, decl: rangeEndDecl, valueDecl: rangeEndDecl }, ownerContext)
        : null;
      const formattedEntry = rangeEndRow ? { ...entry, rangeEnd: rangeEndRow.text || rangeEndRaw, to: rangeEndRow.text || rangeEndRaw } : entry;
      const text = formatItem
        ? formatItem(formattedEntry, valueRow.text || rawValue, index)
        : (valueRow.text || rawValue);
      const componentDecl = itabName && rawValue
        ? lookupTemplateItabComponentDecl(itabName, rawValue, scopeHint)
        : null;
      if (componentDecl) {
        const componentRow = buildTemplateSemanticValueRow({
          ...semanticEntry,
          value: rawValue,
          decl: componentDecl,
          valueDecl: componentDecl,
          declRef: componentDecl.name
        }, ownerContext);
        const componentText = getTemplateItabComponentDisplayText(componentRow, componentDecl, rawValue);
        const formattedComponentText = formatItem
          ? formatItem(formattedEntry, componentText, index)
          : componentText;
        return createTemplateExpandedRow(
          formattedComponentText,
          collectTemplateTraceAwareDeclCandidates(componentDecl, ownerContext),
          { status: "editable", reasonCode: "" }
        );
      }
      const rangeCandidates = rangeEndRow ? rangeEndRow.declCandidates : [];
      return createTemplateExpandedRow(text, [...valueRow.declCandidates, ...rangeCandidates], {
        status: (valueRow.declCandidates.length || rangeCandidates.length) ? "editable" : String(valueRow.provenance && valueRow.provenance.status || "not_applicable"),
        reasonCode: (valueRow.declCandidates.length || rangeCandidates.length) ? "" : String(valueRow.provenance && valueRow.provenance.reasonCode || "NON_DECL_SCHEMA_VALUE")
      });
    });
  }

  function getTemplateCanonicalSemanticEntry(sourceObj, entry, rawValue) {
    if (entry && (isDeclLikeObject(entry.decl) || isDeclLikeObject(entry.valueDecl))) {
      return entry;
    }
    const wantedName = String(rawValue || "").trim().replace(/^@+/, "").toUpperCase();
    if (!wantedName) {
      return null;
    }
    const values = flattenTemplateValueEntries(sourceObj);
    let scopeDecl = null;
    for (const candidate of values) {
      const decl = candidate && (candidate.decl || candidate.valueDecl);
      if (!isDeclLikeObject(decl)) {
        continue;
      }
      scopeDecl = scopeDecl || decl;
      const valueTokens = String(candidate.value || "").trim().replace(/^@+/, "").split(/\s+/);
      if (String(candidate.declRef || decl.name || "").trim().replace(/^@+/, "").toUpperCase() === wantedName
        || String(valueTokens[0] || "").toUpperCase() === wantedName) {
        return { ...entry, decl, valueDecl: decl, declRef: decl.name };
      }
    }
    const scopedDecl = lookupTemplateScopedValueDecl(rawValue, scopeDecl);
    if (scopedDecl) {
      return { ...entry, decl: scopedDecl, valueDecl: scopedDecl, declRef: scopedDecl.name };
    }
    return null;
  }

  function lookupTemplateScopedValueDecl(rawValue, scopeDecl) {
    const wantedName = String(rawValue || "").trim().replace(/^@+/, "").toUpperCase();
    if (!wantedName || !/^(?:<[^>]+>|[A-Z_][A-Z0-9_]*)$/.test(wantedName)) {
      return null;
    }
    const preferredScope = String(scopeDecl && scopeDecl.scopeLabel || "GLOBAL").trim().toUpperCase();
    const decls = state.data && Array.isArray(state.data.decls) ? state.data.decls : [];
    const matches = [];
    for (const decl of decls) {
      if (!decl || String(decl.name || "").trim().toUpperCase() !== wantedName) {
        continue;
      }
      if (preferredScope && String(decl.scopeLabel || "").trim().toUpperCase() === preferredScope) {
        matches.push(decl);
      }
    }
    return matches.length === 1 ? matches[0] : null;
  }

  function lookupTemplateItabComponentDecl(itabName, componentToken, scopeHint) {
    const structName = String(itabName || "").trim();
    const fieldPath = normalizeTemplateItabComponentToken(componentToken);
    if (!structName || !fieldPath) {
      return null;
    }
    const fullRefUpper = `${structName}-${fieldPath}`.toUpperCase();
    const wantedScope = String(scopeHint || "").trim().toUpperCase();
    const decls = state.data && Array.isArray(state.data.decls) ? state.data.decls : [];

    let best = null;
    let bestScore = Number.NEGATIVE_INFINITY;
    for (const decl of decls) {
      if (!decl || typeof decl !== "object") {
        continue;
      }
      if (String(decl.objectType || "").trim().toUpperCase() !== "STRUCT_FIELD") {
        continue;
      }
      if (String(decl.name || "").trim().toUpperCase() !== fullRefUpper) {
        continue;
      }

      const declScope = String(decl.scopeLabel || "").trim().toUpperCase();
      let score = 0;
      if (wantedScope && declScope === wantedScope) {
        score += 1000;
      } else if (wantedScope && declScope) {
        score -= 100;
      }
      if (!decl.synthetic) {
        score += 10;
      }
      if (score > bestScore) {
        best = decl;
        bestScore = score;
      }
    }
    return best;
  }

  function getTemplateSafeRawListRows(sourceObj, keywordLabel, valueEntry) {
    const objectType = String(sourceObj && sourceObj.objectType || "").trim().toUpperCase();
    if (objectType === "SELECT" && (keywordLabel === "stmt" || keywordLabel === "fields")) {
      const fieldsRaw = getTemplateSelectFieldSource(sourceObj, valueEntry);
      const commaItems = splitTemplateTopLevelText(fieldsRaw, "comma");
      if (commaItems.length > 1) {
        return commaItems;
      }
      return splitTemplateSafeSimpleList(fieldsRaw);
    }
    if (objectType === "SORT_ITAB" && keywordLabel === "by") {
      return splitTemplateItabComponentList(valueEntry && valueEntry.value);
    }
    if (
      (objectType === "MODIFY_ITAB" || objectType === "READ_TABLE")
      && keywordLabel === "transporting"
    ) {
      return splitTemplateItabComponentList(valueEntry && valueEntry.value);
    }
    if (objectType === "READ_TABLE" && keywordLabel === "comparing") {
      const raw = String(sourceObj && sourceObj.extras && sourceObj.extras.readTable && sourceObj.extras.readTable.comparingRaw || valueEntry && valueEntry.value || "");
      return splitTemplateItabComponentList(raw);
    }
    if (objectType === "DELETE_ITAB" && keywordLabel === "comparing") {
      const raw = String(sourceObj && sourceObj.extras && sourceObj.extras.deleteItab && sourceObj.extras.deleteItab.comparingRaw || valueEntry && valueEntry.value || "");
      return splitTemplateItabComponentList(raw);
    }
    return null;
  }

  function shouldSkipConditionKeywordInTemplateRows(objectType, keywordLabel) {
    const type = String(objectType || "").trim().toUpperCase();
    const label = normalizeTemplatePairToken(keywordLabel);
    if (type === "SELECT" && (label === "where" || label === "having")) {
      return true;
    }
    if (type === "READ_TABLE" && (label === "with-key" || label === "with-table-key")) {
      return true;
    }
    if ((type === "LOOP_AT_ITAB" || type === "MODIFY_ITAB" || type === "DELETE_ITAB") && label === "where") {
      return true;
    }
    return false;
  }

  function getTemplateAssignmentOperandRows(sourceObj, ownerContext, objectIndexOneBased) {
    if (!sourceObj || typeof sourceObj !== "object") {
      return null;
    }
    if (String(sourceObj.objectType || "").trim().toUpperCase() !== "ASSIGNMENT") {
      return null;
    }

    const valueEntries = flattenTemplateValueEntries(sourceObj);
    const findEntry = (name) => valueEntries.find((entry) => (
      normalizeTemplatePairToken(entry && entry.name) === name
      || normalizeTemplatePairToken(entry && entry.label) === name
    )) || null;

    const targetEntry = findEntry("target");
    const exprEntry = findEntry("expr") || findEntry("source") || findEntry("value");
    const rows = [];

    if (targetEntry) {
      const targetRow = buildTemplateSemanticValueRow(targetEntry, ownerContext);
      rows.push(createTemplateExpandedRow(
        targetRow.text,
        targetRow.declCandidates,
        targetRow.provenance,
        "Đích"
      ));
    }

    const exprRaw = String(exprEntry && exprEntry.value !== undefined && exprEntry.value !== null
      ? exprEntry.value
      : "").trim();
    if (!exprRaw) {
      return rows.length ? rows : null;
    }

    const assignmentTokens = sourceObj.extras
      && sourceObj.extras.assignment
      && sourceObj.extras.assignment.expression
      && Array.isArray(sourceObj.extras.assignment.expression.tokens)
      ? sourceObj.extras.assignment.expression.tokens
      : null;

    const pushAssignmentOperandRow = (part, index, totalParts) => {
      const text = String(part || "").trim();
      if (!text) {
        return;
      }

      if (isTemplateLiteralOrWildcard(text) || /^[+-]?\d+(?:\.\d+)?$/i.test(text)) {
        rows.push(createTemplateExpandedRow(text, [], {
          status: "not_applicable",
          reasonCode: "LITERAL_NO_DECL"
        }, "Nguồn"));
        return;
      }

      const matchesWholeExpr = totalParts === 1
        && text.toUpperCase() === exprRaw.toUpperCase();
      const exprDeclName = String(
        (exprEntry && exprEntry.declRef)
        || (exprEntry && exprEntry.decl && exprEntry.decl.name)
        || ""
      ).trim().toUpperCase();
      const matchesBoundDecl = exprDeclName
        && text.replace(/^@/, "").toUpperCase() === exprDeclName.replace(/^@/, "");

      let partEntry;
      if ((matchesWholeExpr || matchesBoundDecl) && exprEntry) {
        partEntry = {
          ...exprEntry,
          value: text
        };
      } else {
        partEntry = ensureTemplateCanonicalValueEntry(sourceObj, {
          name: "expr",
          label: "expr",
          value: text,
          userDesc: "",
          codeDesc: "",
          decl: null
        }, objectIndexOneBased);
      }

      const valueRow = buildTemplateSemanticValueRow(partEntry, ownerContext);
      rows.push(createTemplateExpandedRow(
        valueRow.text || text,
        valueRow.declCandidates,
        valueRow.provenance,
        "Nguồn"
      ));
    };

    if (assignmentTokens && assignmentTokens.length) {
      for (let index = 0; index < assignmentTokens.length; index += 1) {
        const token = assignmentTokens[index];
        const kind = String(token && token.kind || "").trim().toLowerCase();
        const value = String(token && token.value !== undefined && token.value !== null ? token.value : "").trim();
        if (!value) {
          continue;
        }
        if (kind === "operator" || kind === "paren") {
          rows.push(createTemplateExpandedRow(value, [], {
            status: "not_applicable",
            reasonCode: "OPERATOR_TOKEN"
          }, "Toán tử"));
          continue;
        }
        if (kind === "literal") {
          rows.push(createTemplateExpandedRow(value, [], {
            status: "not_applicable",
            reasonCode: "LITERAL_NO_DECL"
          }, "Nguồn"));
          continue;
        }
        pushAssignmentOperandRow(value, index, assignmentTokens.length);
      }
      return rows.length ? rows : null;
    }

    const operands = splitTemplateExpressionOperands(exprRaw);
    for (let index = 0; index < operands.length; index += 1) {
      pushAssignmentOperandRow(operands[index], index, operands.length);
    }

    return rows.length ? rows : null;
  }

  function getTemplateConcatenateSourceRows(sourceObj, valueEntry, ownerContext, objectIndexOneBased) {
    if (!sourceObj || typeof sourceObj !== "object") {
      return null;
    }
    if (String(sourceObj.objectType || "").trim().toUpperCase() !== "CONCATENATE") {
      return null;
    }

    const semanticSources = sourceObj.extras
      && sourceObj.extras.concatenate
      && Array.isArray(sourceObj.extras.concatenate.sources)
      ? sourceObj.extras.concatenate.sources
      : [];
    if (semanticSources.length) {
      return buildTemplateSemanticListRows(sourceObj, semanticSources, ownerContext, {
        extrasScope: "concatenate",
        sectionName: "sources",
        legacyValueName: "sources"
      }, objectIndexOneBased);
    }

    const valueEntries = flattenTemplateValueEntries(sourceObj);
    const sourcesEntry = valueEntry && normalizeTemplatePairToken(valueEntry.name || valueEntry.label) === "sources"
      ? valueEntry
      : (valueEntries.find((entry) => normalizeTemplatePairToken(entry && entry.name) === "sources"
        || normalizeTemplatePairToken(entry && entry.label) === "sources") || null);
    const raw = String(sourcesEntry && sourcesEntry.value !== undefined && sourcesEntry.value !== null
      ? sourcesEntry.value
      : "").trim();
    if (!raw) {
      return null;
    }

    // Raw fallback must be conservative: a dynamic expression is one operand unless
    // the legacy text is provably a list of simple operands.
    const parts = splitTemplateSafeConcatenateList(raw);
    if (!Array.isArray(parts) || !parts.length) {
      return null;
    }

    const boundName = String(
      (sourcesEntry && sourcesEntry.declRef)
      || (sourcesEntry && sourcesEntry.decl && sourcesEntry.decl.name)
      || ""
    ).trim().toUpperCase();

    return parts.map((part) => {
      const text = String(part || "").trim();
      if (!text) {
        return createTemplateExpandedRow("", [], {
          status: "not_applicable",
          reasonCode: "MISSING_PROVENANCE"
        }, "CONCATENATE");
      }
      if (isTemplateLiteralOrWildcard(text) || /^[+-]?\d+(?:\.\d+)?$/i.test(text)) {
        return createTemplateExpandedRow(text, [], {
          status: "not_applicable",
          reasonCode: "LITERAL_NO_DECL"
        }, "CONCATENATE");
      }

      const matchesBound = boundName && text.replace(/^@/, "").toUpperCase() === boundName.replace(/^@/, "");
      const matchesWhole = parts.length === 1 && text.toUpperCase() === raw.toUpperCase();
      let partEntry;
      if ((matchesBound || matchesWhole) && sourcesEntry) {
        partEntry = { ...sourcesEntry, value: text };
      } else {
        partEntry = ensureTemplateCanonicalValueEntry(sourceObj, {
          name: "sources",
          label: "sources",
          value: text,
          userDesc: "",
          codeDesc: "",
          decl: null
        }, objectIndexOneBased);
      }
      const valueRow = buildTemplateSemanticValueRow(partEntry, ownerContext);
      return createTemplateExpandedRow(
        valueRow.text || text,
        valueRow.declCandidates,
        valueRow.provenance,
        "CONCATENATE"
      );
    });
  }

  function getTemplateExpandedKeywordRows(sourceObj, keyword, valueEntry, ownerContext, objectIndexOneBased) {
    const keywordLabel = normalizeTemplatePairToken(keyword && keyword.label);
    const objectType = String(sourceObj && sourceObj.objectType || "").trim().toUpperCase();
    if (shouldSkipConditionKeywordInTemplateRows(objectType, keywordLabel)) {
      return [];
    }
    if (
      objectType === "ASSIGNMENT"
      && ["assign", "add-assign", "sub-assign", "mul-assign", "div-assign", "cast"].includes(keywordLabel)
    ) {
      const assignmentRows = getTemplateAssignmentOperandRows(sourceObj, ownerContext, objectIndexOneBased);
      if (Array.isArray(assignmentRows) && assignmentRows.length) {
        return assignmentRows;
      }
    }
    if (objectType === "CONCATENATE" && (keywordLabel === "stmt" || keywordLabel === "sources")) {
      const sourceRows = getTemplateConcatenateSourceRows(sourceObj, valueEntry, ownerContext, objectIndexOneBased);
      if (Array.isArray(sourceRows) && sourceRows.length) {
        return sourceRows;
      }
    }
    const semanticRows = getTemplateSemanticSectionRows(
      sourceObj,
      keywordLabel,
      ownerContext,
      objectIndexOneBased,
      keyword && keyword.text
    );
    if (Array.isArray(semanticRows)) {
      return semanticRows;
    }
    if (
      objectType === "SELECT"
      && isTemplateModernSelect(sourceObj)
      && String(keyword && keyword.text || "").trim().toUpperCase() === "SELECT"
    ) {
      // In modern SELECT, only FIELDS owns the list, including legacy raw fallback.
      return [];
    }
    if (
      objectType === "SELECT"
      && isTemplateModernSelect(sourceObj)
      && String(keyword && keyword.text || "").trim().toUpperCase() === "FIELDS"
    ) {
      const fieldsRaw = String(
        sourceObj && sourceObj.extras && sourceObj.extras.select && sourceObj.extras.select.fieldsRaw
        || getTemplateModernSelectFieldsSource(sourceObj)
        || valueEntry && valueEntry.value
        || ""
      ).trim();
      const fields = splitTemplateTopLevelText(fieldsRaw, "comma");
      if (fields.length > 1) {
        return fields.map((text) => createTemplateExpandedRow(text, [], {
          status: "not_applicable",
          reasonCode: "NON_DECL_SCHEMA_VALUE"
        }));
      }
    }
    const conditionRows = getTemplateConditionRows(sourceObj, keywordLabel, ownerContext, objectIndexOneBased);
    if (Array.isArray(conditionRows) && conditionRows.length) {
      return conditionRows;
    }
    const rawListRows = getTemplateSafeRawListRows(sourceObj, keywordLabel, valueEntry);
    if (Array.isArray(rawListRows) && rawListRows.length) {
      const objectType = String(sourceObj && sourceObj.objectType || "").trim().toUpperCase();
      const itabName = (
        objectType === "SORT_ITAB"
        || objectType === "MODIFY_ITAB"
        || objectType === "READ_TABLE"
      )
        ? getTemplateStatementItabName(sourceObj)
        : "";
      return rawListRows.map((text) => {
        const scopeHint = getTemplateStatementItabScopeHint(sourceObj, ownerContext);
        const componentDecl = itabName
          ? lookupTemplateItabComponentDecl(itabName, text, scopeHint)
          : null;
        if (componentDecl) {
          const componentRow = buildTemplateSemanticValueRow({
            value: text,
            decl: componentDecl,
            valueDecl: componentDecl,
            declRef: componentDecl.name
          }, ownerContext);
          const componentText = getTemplateItabComponentDisplayText(componentRow, componentDecl, text);
          return createTemplateExpandedRow(
            componentText,
            collectTemplateTraceAwareDeclCandidates(componentDecl, ownerContext),
            {
              status: "editable",
              reasonCode: ""
            }
          );
        }
        return createTemplateExpandedRow(text, [], {
          status: "not_applicable",
          reasonCode: "NON_DECL_SCHEMA_VALUE"
        });
      });
    }
    return null;
  }

  function buildTemplateKeywordRows(sourceObj, ownerContext, objectIndexOneBased) {
    if (!sourceObj || typeof sourceObj !== "object") {
      return [];
    }

    const valueEntries = flattenTemplateValueEntries(sourceObj);
    const objectType = String(sourceObj.objectType || "").trim().toUpperCase();
    const raw = String(sourceObj.raw || "");
    const keywords = sortKeywordEntriesByRawPosition(
      flattenTemplateKeywordEntries(sourceObj),
      raw
    );
    const rows = [];

    for (const keyword of keywords) {
      const keywordText = String(keyword && keyword.text ? keyword.text : "").trim();
      const keywordLabel = normalizeTemplatePairToken(keyword && keyword.label);
      if (!keywordText) {
        continue;
      }
      if (shouldSkipConditionKeywordInTemplateRows(objectType, keyword && keyword.label)) {
        continue;
      }
      const valueEntry = findValueEntryForKeyword(keyword, valueEntries, objectType);
      const expandedRows = getTemplateExpandedKeywordRows(
        sourceObj,
        keyword,
        valueEntry,
        ownerContext || sourceObj,
        objectIndexOneBased
      );
      if (Array.isArray(expandedRows) && expandedRows.length) {
        for (const expandedRow of expandedRows) {
          const rowKeyword = expandedRow && typeof expandedRow === "object" && expandedRow.keyword
            ? expandedRow.keyword
            : keywordText;
          rows.push(createTemplateKeywordRow(
            rowKeyword,
            expandedRow && typeof expandedRow === "object" ? expandedRow.text : expandedRow,
            expandedRow && typeof expandedRow === "object" ? expandedRow.declCandidates : [],
            expandedRow && typeof expandedRow === "object" && expandedRow.provenance
              ? expandedRow.provenance
              : {
                  status: expandedRow && Array.isArray(expandedRow.declCandidates) && expandedRow.declCandidates.length
                    ? "editable"
                    : "not_applicable",
                  reasonCode: expandedRow && Array.isArray(expandedRow.declCandidates) && expandedRow.declCandidates.length
                    ? ""
                    : getTemplateNoDeclReason(
                        expandedRow && typeof expandedRow === "object" ? expandedRow.text : expandedRow,
                        false,
                        false
                      )
              },
            keywordLabel
          ));
        }
        continue;
      }
      if (expandedRows && Array.isArray(expandedRows) && expandedRows.length === 0) {
        continue;
      }
      const canonicalValueEntry = valueEntry
        ? ensureTemplateCanonicalValueEntry(sourceObj, valueEntry, objectIndexOneBased)
        : null;
      const traceAwareValueEntry = canonicalValueEntry
        ? remapTemplateDeclForPerformSource(canonicalValueEntry, ownerContext || sourceObj)
        : null;
      const declCandidates = traceAwareValueEntry
        ? getTemplateEditableDeclCandidatesFromResolvedValue(traceAwareValueEntry)
        : [];
      const rawValue = String(valueEntry && valueEntry.value !== undefined ? valueEntry.value : "").trim();
      const isDataOperand = Boolean(valueEntry && isTemplateDataValueEntry(sourceObj, valueEntry));
      const renderedValue = isTemplateLiteralOrWildcard(rawValue)
        ? rawValue
        : (traceAwareValueEntry ? resolveTemplateValueRowFinalDesc(traceAwareValueEntry) : "");
      rows.push(createTemplateKeywordRow(
        keywordText,
        renderedValue,
        declCandidates,
        {
          status: declCandidates.length ? "editable" : "not_applicable",
          reasonCode: declCandidates.length
            ? ""
            : getTemplateNoDeclReason(rawValue, isDataOperand, Boolean(valueEntry && !isDataOperand))
          },
        keywordLabel
      ));
    }

    return rows;
  }

  function orderTemplateDeclCandidates(list) {
    const deduped = dedupeTemplateDecls(list);
    return [
      ...deduped.filter((decl) => !isTemplatePathDecl(decl)),
      ...deduped.filter((decl) => isTemplatePathDecl(decl))
    ];
  }

  function getTemplateRowProvenanceByLine(contextObj, tokenExpression) {
    const token = String(tokenExpression || "").trim();
    const match = token.match(/^rows(?:\[(\d+)\])?\.(keyword|finalDesc)$/i);
    if (!match || !contextObj || !Array.isArray(contextObj.rows)) {
      return null;
    }

    const indexedRow = match[1] === undefined ? null : Number(match[1]);
    const propertyName = String(match[2] || "").toLowerCase() === "keyword" ? "keyword" : "finalDesc";
    const rows = indexedRow === null
      ? contextObj.rows.map((row, index) => ({ row, index }))
      : (contextObj.rows[indexedRow] ? [{ row: contextObj.rows[indexedRow], index: indexedRow }] : []);
    if (!rows.length) {
      return {
        declCandidatesByLine: [[]],
        statusByLine: ["unresolved"],
        reasonCodeByLine: ["UNRESOLVED_TEMPLATE_PATH"],
        sourcePathByLine: [token]
      };
    }

    const declCandidatesByLine = [];
    const statusByLine = [];
    const reasonCodeByLine = [];
    const sourcePathByLine = [];
    for (const item of rows) {
      const rowMeta = getTemplateKeywordRowProvenance(item.row);
      const rowText = String(item.row && item.row[propertyName] !== undefined ? item.row[propertyName] : "");
      const textLines = splitTemplateTextLines(rowText);
      for (let lineIndex = 0; lineIndex < textLines.length; lineIndex += 1) {
        declCandidatesByLine.push(orderTemplateDeclCandidates(rowMeta.declCandidates));
        statusByLine.push(rowMeta.declCandidates.length ? "editable" : String(rowMeta.status || "not_applicable"));
        reasonCodeByLine.push(rowMeta.declCandidates.length ? "" : String(rowMeta.reasonCode || "MISSING_PROVENANCE"));
        sourcePathByLine.push(`rows[${item.index}].${propertyName}`);
      }
    }
    return { declCandidatesByLine, statusByLine, reasonCodeByLine, sourcePathByLine };
  }

  function collectTemplateConcretePathRecords(root, pathExpression) {
    const segments = parseTemplatePathSegments(pathExpression);
    if (!segments) {
      return [];
    }
    const records = [];
    const walk = (current, segmentIndex, concretePath, lineage) => {
      if (segmentIndex >= segments.length) {
        records.push({
          value: current,
          owner: lineage.length ? lineage[lineage.length - 1] : null,
          lineage: lineage.slice(),
          sourcePath: concretePath
        });
        return;
      }

      const segment = segments[segmentIndex];
      if (Array.isArray(current)) {
        if (typeof segment === "number") {
          if (segment >= 0 && segment < current.length) {
            walk(current[segment], segmentIndex + 1, `${concretePath}[${segment}]`, lineage.concat(current[segment]));
          }
          return;
        }
        for (let index = 0; index < current.length; index += 1) {
          walk(current[index], segmentIndex, `${concretePath}[${index}]`, lineage.concat(current[index]));
        }
        return;
      }

      if (typeof segment === "number" || !current || typeof current !== "object") {
        return;
      }
      const key = String(segment || "").trim();
      const nextPath = concretePath ? `${concretePath}.${key}` : key;
      const keyLower = key.toLowerCase();
      if (
        segmentIndex === segments.length - 1
        && (keyLower === "finaldesc" || keyLower === "desc")
        && !Object.prototype.hasOwnProperty.call(current, key)
      ) {
        records.push({
          value: resolveTemplatePathValue(current, key),
          owner: current,
          lineage: lineage.concat(current),
          sourcePath: nextPath
        });
        return;
      }
      if (!Object.prototype.hasOwnProperty.call(current, key)) {
        return;
      }
      walk(current[key], segmentIndex + 1, nextPath, lineage.concat(current));
    };
    walk(root, 0, "", []);
    return records;
  }

  function findTemplateRecordSourceObject(record) {
    const lineage = record && Array.isArray(record.lineage) ? record.lineage : [];
    for (let index = lineage.length - 1; index >= 0; index -= 1) {
      const candidate = lineage[index];
      if (!candidate || typeof candidate !== "object" || Array.isArray(candidate)) {
        continue;
      }
      if (
        Object.prototype.hasOwnProperty.call(candidate, "value")
        || Object.prototype.hasOwnProperty.call(candidate, "leftOperand")
        || Object.prototype.hasOwnProperty.call(candidate, "rightOperand")
        || Object.prototype.hasOwnProperty.call(candidate, "decl")
        || Object.prototype.hasOwnProperty.call(candidate, "valueDecl")
      ) {
        return candidate;
      }
    }
    return record && record.owner && typeof record.owner === "object" ? record.owner : null;
  }

  function getTemplateRecordOperandText(record, tokenExpression) {
    const source = findTemplateRecordSourceObject(record);
    const token = String(tokenExpression || "");
    if (source && /leftOperand/i.test(token) && Object.prototype.hasOwnProperty.call(source, "leftOperand")) {
      return String(source.leftOperand || "");
    }
    if (source && /rightOperand/i.test(token) && Object.prototype.hasOwnProperty.call(source, "rightOperand")) {
      return String(source.rightOperand || "");
    }
    if (source && Object.prototype.hasOwnProperty.call(source, "value")) {
      return String(source.value === undefined || source.value === null ? "" : source.value);
    }
    return String(record && record.value !== undefined && record.value !== null ? record.value : "");
  }

  function isTemplateRecordDataOperand(contextObj, tokenExpression, record) {
    const token = String(tokenExpression || "").trim();
    const source = findTemplateRecordSourceObject(record);
    if (/\.[A-Za-z]*conditions(?:\[\d+\])?\./i.test(token) && /(?:left|right)Operand/i.test(token)) {
      return true;
    }
    if (/^values\./i.test(token) && source) {
      return isTemplateDataValueEntry(contextObj, source);
    }
    if (/^extras\.(?:performCall)\.(?:using|changing|tables)(?:\[\d+\])?/i.test(token)) {
      return true;
    }
    if (/^extras\.(?:callFunction|callMethod)\.(?:exporting|importing|changing|tables|receiving)(?:\[\d+\])?/i.test(token)) {
      return true;
    }
    return false;
  }

  function collectTemplateRecordDeclCandidates(contextObj, tokenExpression, record) {
    const token = String(tokenExpression || "").trim();
    const source = findTemplateRecordSourceObject(record);
    const candidates = [];
    const add = (value) => {
      if (isDeclLikeObject(value)) {
        candidates.push(value);
      }
    };

    add(record && record.value);
    add(record && record.owner);
    if (source && /leftOperand/i.test(token)) {
      add(source.leftOperandDecl);
    } else if (source && /rightOperand/i.test(token)) {
      add(source.rightOperandDecl);
    } else if (source) {
      add(source.valueDecl);
      add(source.decl);
      for (const originDecl of Array.isArray(source.originDecls) ? source.originDecls : []) {
        add(originDecl);
      }
    }

    const dataOperand = isTemplateRecordDataOperand(contextObj, token, record);
    const operandText = getTemplateRecordOperandText(record, token);
    const allowConditionSynthetic = /\.[A-Za-z]*conditions(?:\[\d+\])?\./i.test(token)
      && /(?:left|right)Operand/i.test(token);
    return orderTemplateDeclCandidates(candidates.filter((decl) => (
      !isTemplatePathDecl(decl)
      || allowConditionSynthetic
      || (dataOperand && isTemplateIdentifierOperand(operandText))
    )));
  }

  function resolveTemplateTokenProvenance(contextObj, tokenExpression, debugMeta) {
    const token = String(tokenExpression || "").trim();
    const rowMeta = getTemplateRowProvenanceByLine(contextObj, token);
    if (rowMeta) {
      return rowMeta;
    }

    const contextErrors = contextObj && Array.isArray(contextObj[TEMPLATE_CONTEXT_ERRORS_META_KEY_TEMPLATE])
      ? contextObj[TEMPLATE_CONTEXT_ERRORS_META_KEY_TEMPLATE]
      : [];
    if (contextErrors.length) {
      const contextError = contextErrors[0];
      warnTemplateProvenanceOnce("RESOLUTION_ERROR", {
        objectId: contextObj && contextObj.id,
        line: contextObj && contextObj.lineStart,
        template: debugMeta && debugMeta.templateKey,
        range: debugMeta && debugMeta.rangeKey,
        token,
        error: contextError && contextError.error
      });
      return {
        declCandidatesByLine: [[]],
        statusByLine: ["error"],
        reasonCodeByLine: ["RESOLUTION_ERROR"],
        sourcePathByLine: [String(contextError && contextError.path || token)]
      };
    }

    let resolvedValue;
    let sourcePath = "";
    let records = [];
    try {
      for (const candidate of buildTemplatePathCandidates(token)) {
        const candidateValue = resolveTemplatePathValue(contextObj, candidate);
        if (candidateValue === undefined) {
          continue;
        }
        resolvedValue = candidateValue;
        sourcePath = candidate;
        records = collectTemplateConcretePathRecords(contextObj, candidate);
        break;
      }
    } catch (err) {
      warnTemplateProvenanceOnce("RESOLUTION_ERROR", {
        objectId: contextObj && contextObj.id,
        line: contextObj && contextObj.lineStart,
        template: debugMeta && debugMeta.templateKey,
        range: debugMeta && debugMeta.rangeKey,
        token,
        error: err
      });
      return {
        declCandidatesByLine: [[]],
        statusByLine: ["error"],
        reasonCodeByLine: ["RESOLUTION_ERROR"],
        sourcePathByLine: [token]
      };
    }

    if (resolvedValue === undefined) {
      return {
        declCandidatesByLine: [[]],
        statusByLine: ["unresolved"],
        reasonCodeByLine: ["UNRESOLVED_TEMPLATE_PATH"],
        sourcePathByLine: [token]
      };
    }

    const resolvedLines = splitTemplateTextLines(stringifyTemplateResolvedValue(resolvedValue));
    const recordList = records.length ? records : [{ value: resolvedValue, owner: null, lineage: [], sourcePath }];
    const lineMeta = [];
    for (const record of recordList) {
      const declCandidates = collectTemplateRecordDeclCandidates(contextObj, sourcePath, record);
      const operandText = getTemplateRecordOperandText(record, sourcePath);
      const dataOperand = isTemplateRecordDataOperand(contextObj, sourcePath, record);
      const schemaValue = !dataOperand && (
        /(?:comparisonOperator|logicalConnector|\.type(?:\.|$)|\.form(?:\.|$)|\.program(?:\.|$)|\.name(?:\.|$)|\.fields(?:\.|$)|\.from(?:\.|$))/i.test(sourcePath)
      );
      const recordLines = splitTemplateTextLines(stringifyTemplateResolvedValue(record.value));
      const repeatCount = Math.max(1, recordLines.length);
      for (let lineIndex = 0; lineIndex < repeatCount; lineIndex += 1) {
        lineMeta.push({
          declCandidates,
          status: declCandidates.length ? "editable" : "not_applicable",
          reasonCode: declCandidates.length ? "" : getTemplateNoDeclReason(operandText, dataOperand, schemaValue),
          sourcePath: String(record.sourcePath || sourcePath || token)
        });
      }
    }
    while (lineMeta.length < resolvedLines.length) {
      lineMeta.push(lineMeta.length ? { ...lineMeta[lineMeta.length - 1] } : {
        declCandidates: [],
        status: "unresolved",
        reasonCode: "MISSING_PROVENANCE",
        sourcePath: sourcePath || token
      });
    }
    if (lineMeta.length > resolvedLines.length) {
      lineMeta.length = resolvedLines.length;
    }
    return {
      declCandidatesByLine: lineMeta.map((item) => item.declCandidates),
      statusByLine: lineMeta.map((item) => item.status),
      reasonCodeByLine: lineMeta.map((item) => item.reasonCode),
      sourcePathByLine: lineMeta.map((item) => item.sourcePath)
    };
  }

  function parseTemplatePlaceholderTokens(rawText) {
    const text = String(rawText === undefined || rawText === null ? "" : rawText);
    const tokens = [];
    const regex = /\{([^{}]+)\}/g;
    let match;
    while ((match = regex.exec(text)) !== null) {
      tokens.push({
        token: String(match[1] || "").trim(),
        start: match.index,
        end: regex.lastIndex,
        full: match[0]
      });
    }
    return tokens;
  }

  function combineTemplateLineProvenance(lineMeta) {
    const list = Array.isArray(lineMeta) ? lineMeta : [];
    const candidates = [];
    const paths = [];
    let status = "not_applicable";
    let reasonCode = "STATIC_TEXT";
    for (const item of list) {
      candidates.push(...(Array.isArray(item && item.declCandidates) ? item.declCandidates : []));
      const path = String(item && item.sourcePath || "").trim();
      if (path && !paths.includes(path)) {
        paths.push(path);
      }
      if (item && item.status === "editable") {
        status = "editable";
        reasonCode = "";
      } else if (status !== "editable" && item && item.status) {
        status = String(item.status);
        reasonCode = String(item.reasonCode || reasonCode);
      }
    }
    const orderedCandidates = orderTemplateDeclCandidates(candidates);
    if (orderedCandidates.length) {
      status = "editable";
      reasonCode = "";
    }
    return {
      declCandidates: orderedCandidates,
      status,
      reasonCode,
      sourcePath: paths.join(", ")
    };
  }

  function buildTemplateCellDeclMeta(contextObj, rawText, debugMeta) {
    const text = String(rawText === undefined || rawText === null ? "" : rawText);
    const placeholders = parseTemplatePlaceholderTokens(text);
    if (!placeholders.length) {
      return {
        declCandidates: [],
        declCandidatesByLine: [[]],
        status: "not_applicable",
        reasonCode: "STATIC_TEXT",
        sourcePath: "",
        statusByLine: ["not_applicable"],
        reasonCodeByLine: ["STATIC_TEXT"],
        sourcePathByLine: [""]
      };
    }

    const lines = [[]];
    let currentLine = 0;
    let cursor = 0;
    const advanceStaticLines = (staticText) => {
      const count = splitTemplateTextLines(staticText).length - 1;
      for (let index = 0; index < count; index += 1) {
        currentLine += 1;
        if (!lines[currentLine]) {
          lines[currentLine] = [];
        }
      }
    };

    for (const placeholder of placeholders) {
      advanceStaticLines(text.slice(cursor, placeholder.start));
      const tokenMeta = resolveTemplateTokenProvenance(contextObj, placeholder.token, debugMeta);
      let resolvedText = "";
      try {
        resolvedText = stringifyTemplateResolvedValue(resolveTemplatePlaceholderValue(contextObj, placeholder.token));
      } catch (err) {
        warnTemplateProvenanceOnce("RESOLUTION_ERROR", {
          objectId: contextObj && contextObj.id,
          line: contextObj && contextObj.lineStart,
          template: debugMeta && debugMeta.templateKey,
          range: debugMeta && debugMeta.rangeKey,
          token: placeholder.token,
          error: err
        });
      }
      const resolvedLineCount = Math.max(1, splitTemplateTextLines(resolvedText).length);
      for (let lineIndex = 0; lineIndex < resolvedLineCount; lineIndex += 1) {
        const candidatesByLine = tokenMeta.declCandidatesByLine || [];
        const statusByLine = tokenMeta.statusByLine || [];
        const reasonByLine = tokenMeta.reasonCodeByLine || [];
        const pathByLine = tokenMeta.sourcePathByLine || [];
        const selectedIndex = Math.max(0, Math.min(Math.max(0, candidatesByLine.length - 1), lineIndex));
        lines[currentLine].push({
          declCandidates: candidatesByLine[selectedIndex] || [],
          status: statusByLine[selectedIndex] || "unresolved",
          reasonCode: reasonByLine[selectedIndex] !== undefined
            ? String(reasonByLine[selectedIndex] || "")
            : "MISSING_PROVENANCE",
          sourcePath: pathByLine[selectedIndex] || placeholder.token
        });
        if (lineIndex < resolvedLineCount - 1) {
          currentLine += 1;
          if (!lines[currentLine]) {
            lines[currentLine] = [];
          }
        }
      }
      cursor = placeholder.end;
    }
    advanceStaticLines(text.slice(cursor));

    const combinedByLine = lines.map((line) => combineTemplateLineProvenance(line));
    const allCandidates = [];
    for (const item of combinedByLine) {
      allCandidates.push(...item.declCandidates);
    }
    const first = combinedByLine[0] || {
      status: "unresolved",
      reasonCode: "MISSING_PROVENANCE",
      sourcePath: ""
    };
    return {
      declCandidates: orderTemplateDeclCandidates(allCandidates),
      declCandidatesByLine: combinedByLine.map((item) => item.declCandidates),
      status: first.status,
      reasonCode: first.reasonCode,
      sourcePath: first.sourcePath,
      statusByLine: combinedByLine.map((item) => item.status),
      reasonCodeByLine: combinedByLine.map((item) => item.reasonCode),
      sourcePathByLine: combinedByLine.map((item) => item.sourcePath)
    };
  }

  function buildTemplateContextObject(obj, objectIndexOneBased) {
    const objectIndex = Number(objectIndexOneBased) || 1;
    const basePathParts = [getTemplateCanonicalObjectPathBase(obj)];
    const normalizationErrors = [];

    const cloneRecursive = (value, keyHint, pathParts, ownerContext) => {
      if (value === null || value === undefined) {
        return value;
      }
      if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
        return value;
      }

      if (Array.isArray(value)) {
        const itemTag = /conditions$/i.test(String(keyHint || ""))
          ? "clause"
          : getTemplateArrayItemTagName(keyHint);
        return value.map((item, index) => cloneRecursive(
          item,
          itemTag,
          pathParts.concat(`${itemTag}[${index + 1}]`),
          ownerContext
        ));
      }

      if (typeof value !== "object") {
        return value;
      }

      const nextOwnerContext = (typeof isAbapStatementObject === "function" && isAbapStatementObject(value))
        ? value
        : ownerContext;
      const normalized = normalizeTemplateEntryForPath(
        value,
        keyHint,
        pathParts,
        nextOwnerContext,
        (error) => normalizationErrors.push({
          path: pathParts.join("/"),
          error
        })
      );
      if (!normalized || typeof normalized !== "object") {
        return normalized;
      }
      const remappedForTrace = isTemplateOriginDeclPath(pathParts)
        ? normalized
        : remapTemplateDeclForPerformSource(normalized, nextOwnerContext);

      const out = {};
      for (const key of Object.keys(remappedForTrace)) {
        out[key] = cloneRecursive(
          remappedForTrace[key],
          key,
          pathParts.concat(key),
          nextOwnerContext
        );
      }
      return out;
    };

    const context = cloneRecursive(obj, "object", basePathParts, obj);
    if (context && typeof context === "object" && !Array.isArray(context)) {
      const stack = [context];
      const seen = new Set();
      while (stack.length) {
        const current = stack.pop();
        if (!current || typeof current !== "object" || seen.has(current)) {
          continue;
        }
        seen.add(current);
        if (isTemplatePathDecl(current)) {
          attachTemplateSyntheticDeclAliases(current, objectIndex);
        }
        if (Array.isArray(current)) {
          for (const item of current) {
            stack.push(item);
          }
          continue;
        }
        for (const key of Object.keys(current)) {
          stack.push(current[key]);
        }
      }
      Object.defineProperty(context, TEMPLATE_OBJECT_INDEX_META_KEY_TEMPLATE, {
        configurable: true,
        enumerable: false,
        value: objectIndex
      });
      Object.defineProperty(context, TEMPLATE_CONTEXT_ERRORS_META_KEY_TEMPLATE, {
        configurable: true,
        enumerable: false,
        value: normalizationErrors.slice()
      });
      context.rows = buildTemplateKeywordRows(obj, obj, objectIndex);
    }
    return context;
  }

  function stringifyTemplateResolvedValue(value) {
    if (value === undefined || value === null) {
      return "";
    }
    if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
      return String(value);
    }
    if (Array.isArray(value)) {
      return value
        .map((item) => stringifyTemplateResolvedValue(item))
        .join("\n");
    }
    if (isDeclLikeObject(value)) {
      return getDeclDisplayName(value) || getDeclTechName(value);
    }
    if (typeof value === "object") {
      if (hasValueLevelDescFields(value) || isDeclLikeObject(value.decl)) {
        const finalDesc = resolveValueLevelFinalDesc(value);
        if (finalDesc) {
          return finalDesc;
        }
      }
      return safeJson(value, false);
    }
    return String(value);
  }

  function collectTemplateDumpPaths(root) {
    const out = new Set();

    const walk = (value, path) => {
      if (path) {
        out.add(path);
      }
      if (value === null || value === undefined) {
        return;
      }

      if (Array.isArray(value)) {
        for (let i = 0; i < value.length; i += 1) {
          const nextPath = path ? `${path}[${i}]` : `[${i}]`;
          walk(value[i], nextPath);
        }
        return;
      }

      if (typeof value !== "object") {
        return;
      }

      if (path && isDeclLikeObject(value)) {
        out.add(`${path}.desc`);
        out.add(`${path}.finalDesc`);
      }
      if (path && (hasValueLevelDescFields(value) || isDeclLikeObject(value.decl))) {
        out.add(`${path}.finalDesc`);
      }

      for (const key of Object.keys(value)) {
        if (key === "children") {
          continue;
        }
        const nextPath = path ? `${path}.${key}` : key;
        walk(value[key], nextPath);
      }
    };

    walk(root, "");
    return Array.from(out).sort((a, b) => a.localeCompare(b));
  }

  function formatTemplateDumpValue(value) {
    if (value === undefined || value === null) {
      return "";
    }
    const text = String(value);
    return text
      .replace(/\r\n/g, "\\n")
      .replace(/\r/g, "\\n")

      .replace(/\n/g, "\\n");
  }

  function collectTemplateDumpPathValues(root) {
    const out = new Map();

    const addEntry = (path, value) => {
      const key = String(path || "").trim();
      if (!key || out.has(key)) {
        return;
      }
      out.set(key, formatTemplateDumpValue(value));
    };

    const walk = (value, path) => {
      const currentPath = String(path || "");
      if (value === undefined) {
        return;
      }

      if (value === null) {
        if (currentPath) {
          addEntry(currentPath, "");
        }
        return;
      }

      if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
        if (currentPath) {
          addEntry(currentPath, value);
        }
        return;
      }

      if (Array.isArray(value)) {
        for (let i = 0; i < value.length; i += 1) {
          const nextPath = currentPath ? `${currentPath}[${i}]` : `[${i}]`;
          walk(value[i], nextPath);
        }
        return;
      }

      if (typeof value !== "object") {
        if (currentPath) {
          addEntry(currentPath, String(value));
        }
        return;
      }

      if (currentPath && isDeclLikeObject(value)) {
        addEntry(`${currentPath}.desc`, getEffectiveDeclDesc(value));
        addEntry(`${currentPath}.finalDesc`, getFinalDeclDesc(value));
      }
      if (currentPath && (hasValueLevelDescFields(value) || isDeclLikeObject(value.decl))) {
        addEntry(`${currentPath}.finalDesc`, resolveValueLevelFinalDesc(value));
      }

      const keys = Object.keys(value);
      keys.sort((a, b) => a.localeCompare(b));
      for (const key of keys) {
        if (key === "children") {
          continue;
        }
        const nextPath = currentPath ? `${currentPath}.${key}` : key;
        walk(value[key], nextPath);
      }
    };

    walk(root, "");
    return Array.from(out.entries()).map(([path, value]) => `${path} = ${value}`);
  }

  function openTemplatePathDump(contextObj, index, obj) {
    const lines = collectTemplateDumpPathValues(contextObj);
    const fallback = collectTemplateDumpPaths(contextObj).map((path) => `${path} =`);
    const dumpText = (lines.length ? lines : fallback).join("\n");
    const objectType = obj && obj.objectType ? String(obj.objectType) : "OBJECT";
    const title = `Template Paths #${Number(index) + 1} ${objectType}`;

    if (typeof openTextModal === "function") {
      openTextModal(title, dumpText || "[No paths]");
      return;
    }

    if (typeof setError === "function") {
      setError("Path viewer is unavailable.");
    }
  }

  function resolveTemplatePlaceholderValue(obj, tokenExpression) {
    const token = String(tokenExpression || "").trim();
    if (!token) {
      return "";
    }

    if (token === "__DUMP__") {
      return collectTemplateDumpPaths(obj).join("\n");
    }
    if (token === "__DUMP_VALUES__" || token === "__DUMP_WITH_VALUES__") {
      return collectTemplateDumpPathValues(obj).join("\n");
    }

    const candidates = buildTemplatePathCandidates(token);

    for (const candidate of candidates) {
      const value = resolveTemplatePathValue(obj, candidate);
      if (value !== undefined) {
        return value;
      }
    }

    return "";
  }

  function resolveTemplateText(rawText, obj) {
    const templateText = String(rawText === undefined || rawText === null ? "" : rawText);
    const labelDirectives = parseTemplateLabelDirectives(templateText);
    if (!labelDirectives.errors.length && labelDirectives.directives.length) {
      const resolvedLabels = labelDirectives.directives.map((directive) => ({
        displayText: directive.displayText,
        semanticLabel: normalizeTemplatePairToken(resolveTemplatePlaceholderValue(obj, directive.path))
      }));
      const rows = obj && Array.isArray(obj.rows) ? obj.rows : [];
      const translatedRows = rows.map((row) => {
        const rowLabel = normalizeTemplatePairToken(row && row.label);
        const matched = resolvedLabels.find((entry) => entry.semanticLabel && entry.semanticLabel === rowLabel);
        return matched ? matched.displayText : String(row && row.keyword || "");
      });
      return {
        text: translatedRows.join("\n"),
        hasPlaceholder: true,
        hasTokenValue: translatedRows.some((value) => String(value || "") !== "")
      };
    }
    if (!templateText.includes("{")) {
      return {
        text: templateText,
        hasPlaceholder: false,
        hasTokenValue: false
      };
    }

    let hasPlaceholder = false;
    let hasTokenValue = false;
    const text = templateText.replace(/\{([^{}]+)\}/g, (full, token) => {
      hasPlaceholder = true;
      const resolved = resolveTemplatePlaceholderValue(obj, token);
      const resolvedText = stringifyTemplateResolvedValue(resolved);
      if (resolvedText !== "") {
        hasTokenValue = true;
      }
      return resolvedText;
    });

    return {
      text,
      hasPlaceholder,
      hasTokenValue
    };
  }

  function parseSingleTemplatePlaceholderToken(rawText) {
    const directiveResult = parseTemplateLabelDirectives(rawText);
    const text = String(
      directiveResult.directives.length && !directiveResult.errors.length
        ? directiveResult.body
        : (rawText === undefined || rawText === null ? "" : rawText)
    ).trim();
    if (!text) {
      return "";
    }
    const match = text.match(/^\{([^{}]+)\}$/);
    if (!match || !match[1]) {
      return "";
    }
    return String(match[1] || "").trim();
  }

  function buildTemplateDeclTokenCandidates(tokenExpression) {
    const token = String(tokenExpression || "").trim();
    if (!token) {
      return [];
    }

    const out = new Set();
    const add = (value) => {
      const next = String(value || "").trim();
      if (next) {
        out.add(next);
      }
    };

    add(token);

    if (/\.decl\.finaldesc$/i.test(token)) {
      add(token.replace(/\.finaldesc$/i, ""));
    }

    if (/\.finaldesc$/i.test(token)) {
      const base = token.replace(/\.finaldesc$/i, "");
      add(base);
      if (!/\.decl$/i.test(base)) {
        add(base + ".decl");
      }
      add(base + ".declRef");
    }

    if (/\.desc$/i.test(token)) {
      const base = token.replace(/\.desc$/i, "");
      add(base);
      if (!/\.decl$/i.test(base)) {
        add(base + ".decl");
      }
    }

    if (/^values\./i.test(token) && !/\.decl(\.|$)/i.test(token)) {
      add(token + ".decl");
    }

    const lowered = token.toLowerCase();
    if (lowered === "values.condition.finaldesc" || lowered === "values.condition") {
      add("values.condition");
      add("values.condition.leftOperand");
      add("values.condition.leftOperand.decl");
      add("values.condition.rightOperand");
      add("values.condition.rightOperand.decl");
    }

    if (lowered.includes(".conditions")) {
      const withoutFinal = token.replace(/\.finaldesc$/i, "");
      add(withoutFinal);
      add(withoutFinal + ".leftOperand");
      add(withoutFinal + ".leftOperand.decl");
      add(withoutFinal + ".rightOperand");
      add(withoutFinal + ".rightOperand.decl");
    }

    return Array.from(out);
  }

  function collectTemplateEditableDeclsFromResolvedValue(value, outList, depth, seenValues) {
    const out = Array.isArray(outList) ? outList : [];
    const currentDepth = Number(depth) || 0;
    const seen = seenValues instanceof Set ? seenValues : new Set();

    if (value === null || value === undefined) {
      return out;
    }
    if (currentDepth > 5) {
      return out;
    }

    if (Array.isArray(value)) {
      for (const item of value) {
        collectTemplateEditableDeclsFromResolvedValue(item, out, currentDepth + 1, seen);
      }
      return out;
    }

    if (isDeclLikeObject(value)) {
      out.push(value);
      return out;
    }

    if (typeof value !== "object") {
      return out;
    }

    if (seen.has(value)) {
      return out;
    }
    seen.add(value);

    if (isDeclLikeObject(value.decl)) {
      out.push(value.decl);
    }

    for (const key of Object.keys(value)) {
      if (key === "decl" || key === "desc" || key === "finalDesc" || key === "codeDesc" || key === "userDesc") {
        continue;
      }
      collectTemplateEditableDeclsFromResolvedValue(value[key], out, currentDepth + 1, seen);
    }
    return out;
  }

  function getTemplateEditableDeclCandidatesFromResolvedValue(value) {
    return dedupeTemplateDecls(collectTemplateEditableDeclsFromResolvedValue(value, []));
  }

  function resolveTemplateEditableDeclCandidatesFromToken(contextObj, token) {
    const out = [];
    const candidates = buildTemplateDeclTokenCandidates(token);
    for (const candidate of candidates) {
      const resolved = resolveTemplatePlaceholderValue(contextObj, candidate);
      const decls = getTemplateEditableDeclCandidatesFromResolvedValue(resolved);
      if (decls.length) {
        out.push(...decls);
      }
    }
    return dedupeTemplateDecls(out);
  }

  function createTemplateCellModel() {
    return {
      text: "",
      style: {},
      hidden: false,
      rowspan: 1,
      colspan: 1,
      hasPlaceholder: false,
      hasTokenValue: false,
      meta: null
    };
  }

  function buildTemplateCellStyle(rangeConfig, position) {
    const cfg = rangeConfig && typeof rangeConfig === "object" ? rangeConfig : {};
    const style = {};

    const background = normalizeTemplateColorValue(cfg.background);
    if (background) {
      style["background-color"] = background;
    }

    const fontColor = normalizeTemplateColorValue(cfg["font color"]);
    if (fontColor) {
      style.color = fontColor;
    }

    const fontSize = Number(cfg["font size"]);
    if (Number.isFinite(fontSize) && fontSize > 0) {
      style["font-size"] = `${fontSize}pt`;
    }

    const fontFamilyRaw = String(cfg["font family"] || "").trim();
    if (fontFamilyRaw && normalizeTemplateAliasToken(fontFamilyRaw) !== "default") {
      style["font-family"] = fontFamilyRaw;
    }

    if (cfg.bold === true) {
      style["font-weight"] = "700";
    }
    if (cfg.italic === true) {
      style["font-style"] = "italic";
    }
    if (cfg.underline === true) {
      style["text-decoration"] = "underline";
    }

    const align = normalizeTemplateAlignValue(cfg.align);
    if (align) {
      style["text-align"] = align;
    }

    const valign = normalizeTemplateVAlignValue(cfg.valign);
    if (valign) {
      style["vertical-align"] = valign;
    }

    if (cfg.wrap === true) {
      style["white-space"] = "pre-wrap";
    } else if (cfg.wrap === false) {
      style["white-space"] = "nowrap";
    }

    const border = normalizeTemplateBorderValue(cfg.border);
    if (border === "outside-thin") {
      const borderLine = "0.5pt solid #000000";
      if (position && position.isMergeAnchor) {
        style.border = borderLine;
      } else {
        style["border-top"] = position && position.isTop ? borderLine : "none";
        style["border-right"] = position && position.isRight ? borderLine : "none";
        style["border-bottom"] = position && position.isBottom ? borderLine : "none";
        style["border-left"] = position && position.isLeft ? borderLine : "none";
      }
    } else if (border) {
      style.border = border;
    }

    return style;
  }

  function parseTemplateOptionBoolean(value, fallback) {
    if (typeof value === "boolean") {
      return value;
    }
    if (typeof value === "number") {
      if (value === 1) {
        return true;
      }
      if (value === 0) {
        return false;
      }
      return fallback;
    }
    const token = String(value || "").trim().toLowerCase();
    if (!token) {
      return fallback;
    }
    if (token === "1" || token === "true" || token === "yes" || token === "y" || token === "on") {
      return true;
    }
    if (token === "0" || token === "false" || token === "no" || token === "n" || token === "off") {
      return false;
    }
    return fallback;
  }

  function parseTemplateOptionNumber(value, fallback, min, max) {
    const fallbackValue = Number.isFinite(Number(fallback)) ? Number(fallback) : 0;
    const minValue = Number.isFinite(Number(min)) ? Number(min) : fallbackValue;
    const maxValue = Number.isFinite(Number(max)) ? Number(max) : fallbackValue;

    let numeric = NaN;
    if (typeof value === "number") {
      numeric = value;
    } else if (typeof value === "string") {
      const token = value.trim();
      if (token) {
        numeric = Number(token);
      }
    }

    if (!Number.isFinite(numeric)) {
      return fallbackValue;
    }

    const normalized = Math.round(numeric);
    return Math.max(minValue, Math.min(maxValue, normalized));
  }

  function getTemplateOptionByPath(source, path) {
    if (!source || typeof source !== "object" || Array.isArray(source)) {
      return undefined;
    }
    const parts = String(path || "").split(".").map((item) => String(item || "").trim()).filter(Boolean);
    if (!parts.length) {
      return undefined;
    }

    let current = source;
    for (const part of parts) {
      if (!current || typeof current !== "object" || Array.isArray(current)) {
        return undefined;
      }
      if (!Object.prototype.hasOwnProperty.call(current, part)) {
        return undefined;
      }
      current = current[part];
    }
    return current;
  }

  function readTemplateOptionValue(sources, paths) {
    const sourceList = Array.isArray(sources) ? sources : [];
    const pathList = Array.isArray(paths) ? paths : [];
    for (const source of sourceList) {
      for (const path of pathList) {
        const value = getTemplateOptionByPath(source, path);
        if (value !== undefined) {
          return value;
        }
      }
    }
    return undefined;
  }

  function normalizeTemplatePreviewOptions(optionSource, templateDef, rangeSource) {
    const sources = [optionSource, templateDef, rangeSource];
    const hideEmptyRows = parseTemplateOptionBoolean(readTemplateOptionValue(sources, [
      "hideEmptyRows",
      "removeEmptyRows",
      "compact.removeEmptyRows"
    ]), true);
    const hideRowsWithoutValues = parseTemplateOptionBoolean(readTemplateOptionValue(sources, [
      "hideRowsWithoutValues",
      "removeEmptyRowsAdvanced",
      "removeEmptyRowsAdv",
      "compact.removeEmptyRowsAdvanced",
      "compact.removeEmptyRowsAdv"
    ]), true);
    const expandMultilineRows = parseTemplateOptionBoolean(readTemplateOptionValue(sources, [
      "expandMultilineRows",
      "expandArrayRows",
      "arrayToRows"
    ]), false);
    const squareCells = parseTemplateOptionBoolean(readTemplateOptionValue(sources, [
      "squareCells",
      "squareCellsEnabled",
      "fixedSquareCells"
    ]), true);
    const squareCellSize = parseTemplateOptionNumber(readTemplateOptionValue(sources, [
      "squareCellSize",
      "squareCellSizePx",
      "cellSize",
      "cellSizePx"
    ]), 18, 16, 240);
    const objectLabelValue = readTemplateOptionValue(sources, ["objectLabel"]);
    const objectLabel = typeof objectLabelValue === "string" ? objectLabelValue.trim() : "";

    return {
      hideEmptyRows,
      hideRowsWithoutValues,
      expandMultilineRows,
      squareCells,
      squareCellSize,
      objectLabel
    };
  }

  function isTemplateRangeMetaKey(rawKey) {
    const key = String(rawKey || "").trim().toLowerCase();
    if (!key) {
      return false;
    }
    return (
      key === "_options"
      || key === "options"
      || key === "ranges"
      || key === "compact"
      || key === "hideemptyrows"
      || key === "hiderowswithoutvalues"
      || key === "expandmultilinerows"
      || key === "removeemptyrows"
      || key === "removeemptyrowsadvanced"
      || key === "removeemptyrowsadv"
      || key === "expandarrayrows"
      || key === "arraytorows"
      || key === "squarecells"
      || key === "squarecellsenabled"
      || key === "fixedsquarecells"
      || key === "squarecellsize"
      || key === "squarecellsizepx"
      || key === "cellsize"
      || key === "cellsizepx"
    );
  }

  function resolveTemplateDefinitionForPreview(definition) {
    if (!definition || typeof definition !== "object" || Array.isArray(definition)) {
      return {
        map: null,
        options: normalizeTemplatePreviewOptions(null, null, null)
      };
    }

    const hasRanges = definition.ranges && typeof definition.ranges === "object" && !Array.isArray(definition.ranges);
    const rangeSource = hasRanges ? definition.ranges : definition;
    const optionSource = (definition._options && typeof definition._options === "object" && !Array.isArray(definition._options))
      ? definition._options
      : ((definition.options && typeof definition.options === "object" && !Array.isArray(definition.options))
        ? definition.options
        : null);
    const options = normalizeTemplatePreviewOptions(optionSource, definition, rangeSource);

    const map = {};
    for (const [key, value] of Object.entries(rangeSource)) {
      if (isTemplateRangeMetaKey(key)) {
        continue;
      }
      map[key] = value;
    }

    return { map, options };

  }

  function splitTemplateTextLines(text) {
    const normalized = String(text === undefined || text === null ? "" : text)
      .replace(/\r\n/g, "\n")
      .replace(/\r/g, "\n");
    if (normalized === "") {
      return [""];
    }
    return normalized.split("\n");
  }

  function getTemplateTextLine(lines, index) {
    const list = Array.isArray(lines) && lines.length ? lines : [""];
    const idx = Number(index) || 0;
    if (idx < list.length) {
      return String(list[idx] || "");
    }
    return String(list[list.length - 1] || "");
  }

  function cloneTemplateCellMeta(meta) {
    if (!meta || typeof meta !== "object") {
      return null;
    }
    const cloned = { ...meta };
    if (Array.isArray(meta.declCandidates)) {
      cloned.declCandidates = meta.declCandidates.slice();
    }
    if (Array.isArray(meta.declCandidatesByLine)) {
      // Per-line provenance is immutable; share it across the 20-cell template ranges.
      cloned.declCandidatesByLine = meta.declCandidatesByLine;
    }
    if (Array.isArray(meta.statusByLine)) {
      cloned.statusByLine = meta.statusByLine;
    }
    if (Array.isArray(meta.reasonCodeByLine)) {
      cloned.reasonCodeByLine = meta.reasonCodeByLine;
    }
    if (Array.isArray(meta.sourcePathByLine)) {
      cloned.sourcePathByLine = meta.sourcePathByLine;
    }
    return cloned;
  }

  function selectTemplateCellDeclCandidatesForLine(cell, lineIndex) {
    if (!cell || !cell.meta || !Array.isArray(cell.meta.declCandidatesByLine)) {
      return;
    }
    const candidatesByLine = cell.meta.declCandidatesByLine;
    const selectedIndex = candidatesByLine.length
      ? Math.max(0, Math.min(candidatesByLine.length - 1, Number(lineIndex) || 0))
      : 0;
    cell.meta.declCandidates = dedupeTemplateDecls(candidatesByLine[selectedIndex] || []);
    if (Array.isArray(cell.meta.statusByLine)) {
      cell.meta.status = String(cell.meta.statusByLine[selectedIndex] || "unresolved");
    }
    if (Array.isArray(cell.meta.reasonCodeByLine)) {
      cell.meta.reasonCode = cell.meta.reasonCodeByLine[selectedIndex] !== undefined
        ? String(cell.meta.reasonCodeByLine[selectedIndex] || "")
        : "MISSING_PROVENANCE";
    }
    if (Array.isArray(cell.meta.sourcePathByLine)) {
      cell.meta.sourcePath = String(cell.meta.sourcePathByLine[selectedIndex] || "");
    }
  }

  function cloneTemplateMatrixCell(cell) {
    if (!cell || typeof cell !== "object") {
      return createTemplateCellModel();
    }
    return {
      text: String(cell.text || ""),
      style: { ...(cell.style && typeof cell.style === "object" ? cell.style : {}) },
      hidden: Boolean(cell.hidden),
      rowspan: Number(cell.rowspan) || 1,
      colspan: Number(cell.colspan) || 1,
      hasPlaceholder: Boolean(cell.hasPlaceholder),
      hasTokenValue: Boolean(cell.hasTokenValue),
      meta: cloneTemplateCellMeta(cell.meta)
    };
  }

  function expandTemplateMatrixRows(matrix) {
    const rows = Array.isArray(matrix)
      ? matrix.map((row) => (Array.isArray(row) ? row.map((cell) => cloneTemplateMatrixCell(cell)) : []))
      : [];
    if (!rows.length) {
      return rows;
    }

    for (let rowIndex = rows.length - 1; rowIndex >= 0; rowIndex -= 1) {
      const row = rows[rowIndex];
      if (!Array.isArray(row) || !row.length) {
        continue;
      }

      const hasMergedCells = row.some((cell) => cell && (cell.hidden || cell.rowspan > 1 || cell.colspan > 1));
      if (hasMergedCells) {
        continue;
      }

      const lineByCol = new Map();
      let maxLines = 1;
      for (let colIndex = 0; colIndex < row.length; colIndex += 1) {
        const cell = row[colIndex];
        if (!cell || cell.hidden) {
          continue;
        }
        const lines = splitTemplateTextLines(cell.text);
        lineByCol.set(colIndex, lines);
        maxLines = Math.max(maxLines, lines.length);
      }

      if (maxLines <= 1) {
        continue;
      }

      for (let colIndex = 0; colIndex < row.length; colIndex += 1) {
        const cell = row[colIndex];
        if (!cell || cell.hidden) {
          continue;
        }
        const lines = lineByCol.get(colIndex) || [String(cell.text || "")];
        cell.text = getTemplateTextLine(lines, 0);
        selectTemplateCellDeclCandidatesForLine(cell, 0);
      }

      const extraRows = [];
      for (let lineIndex = 1; lineIndex < maxLines; lineIndex += 1) {
        const extraRow = row.map((cell) => cloneTemplateMatrixCell(cell));
        for (let colIndex = 0; colIndex < extraRow.length; colIndex += 1) {
          const cell = extraRow[colIndex];
          if (!cell || cell.hidden) {
            continue;
          }
          const lines = lineByCol.get(colIndex) || [String(cell.text || "")];
          cell.text = getTemplateTextLine(lines, lineIndex);
          selectTemplateCellDeclCandidatesForLine(cell, lineIndex);
        }
        extraRows.push(extraRow);
      }

      if (extraRows.length) {
        rows.splice(rowIndex + 1, 0, ...extraRows);
      }
    }

    return rows;
  }

  function isTemplateRowBlank(row) {
    const list = Array.isArray(row) ? row : [];
    for (const cell of list) {
      if (!cell || cell.hidden) {
        continue;
      }
      if (String(cell.text || "").trim() !== "") {
        return false;
      }
    }
    return true;
  }

  function getTemplateRowPlaceholderState(row) {
    const list = Array.isArray(row) ? row : [];
    let hasPlaceholder = false;
    let hasTokenValue = false;
    for (const cell of list) {
      if (!cell || cell.hidden) {
        continue;
      }
      if (cell.hasPlaceholder) {
        hasPlaceholder = true;
      }
      if (cell.hasTokenValue) {
        hasTokenValue = true;
      }
    }
    return { hasPlaceholder, hasTokenValue };
  }

  function compactTemplateMatrixRows(matrix, options) {
    const rows = Array.isArray(matrix) ? matrix : [];
    if (!rows.length) {
      return rows;
    }

    const removeAdvanced = options && options.hideRowsWithoutValues === true;
    const removeEmpty = removeAdvanced || (options && options.hideEmptyRows === true);
    if (!removeAdvanced && !removeEmpty) {
      return rows;
    }

    const out = [];
    for (const row of rows) {
      const rowBlank = isTemplateRowBlank(row);
      if (removeAdvanced) {
        const stateRow = getTemplateRowPlaceholderState(row);
        if (rowBlank || (stateRow.hasPlaceholder && !stateRow.hasTokenValue)) {
          continue;
        }
        out.push(row);
        continue;
      }

      if (removeEmpty && rowBlank) {
        continue;
      }
      out.push(row);
    }

    return out;
  }

  function applyTemplatePreviewOptions(matrix, options) {
    let next = Array.isArray(matrix) ? matrix : [];
    if (!next.length) {
      return next;
    }
    if (options && options.expandMultilineRows === true) {
      next = expandTemplateMatrixRows(next);
    }
    return compactTemplateMatrixRows(next, options);
  }

  function buildTemplateGridModel(obj, templateMap, templateOptions, metaOptions) {
    const map = templateMap && typeof templateMap === "object" ? templateMap : {};
    const options = templateOptions && typeof templateOptions === "object"
      ? templateOptions
      : normalizeTemplatePreviewOptions(null, null, null);
    const modelMeta = metaOptions && typeof metaOptions === "object" ? metaOptions : {};
    const modelTemplateKey = String(modelMeta.templateKey || "").trim();
    const modelObjectType = String(modelMeta.objectType || "").trim();
    const entries = [];
    const errors = [];
    let maxRow = 0;
    let maxCol = 0;

    for (const rangeKey of Object.keys(map)) {
      if (isTemplateRangeMetaKey(rangeKey)) {
        continue;
      }
      try {
        const parsedRange = parseRangeKey(rangeKey);
        entries.push({
          rangeKey,
          parsedRange,
          config: map[rangeKey] && typeof map[rangeKey] === "object" ? map[rangeKey] : {}
        });
        maxRow = Math.max(maxRow, parsedRange.r2);
        maxCol = Math.max(maxCol, parsedRange.c2);
      } catch (err) {
        errors.push(`${rangeKey}: ${err && err.message ? err.message : err}`);
      }
    }

    if (!entries.length) {
      return {
        matrix: [],
        maxRow: 0,
        maxCol: 0,
        errors
      };
    }

    const matrix = Array.from({ length: maxRow }, () =>
      Array.from({ length: maxCol }, () => createTemplateCellModel())
    );

    for (const entry of entries) {
      const cfg = entry.config;
      const range = entry.parsedRange;
      const hasText = Object.prototype.hasOwnProperty.call(cfg, "text");
      const rawText = hasText ? String(cfg.text === undefined || cfg.text === null ? "" : cfg.text) : "";
      const labelDirectiveResult = hasText ? parseTemplateLabelDirectives(rawText) : null;
      const provenanceText = labelDirectiveResult
        && labelDirectiveResult.directives.length
        && !labelDirectiveResult.errors.length
        ? labelDirectiveResult.body
        : rawText;
      const textMeta = hasText ? resolveTemplateText(rawText, obj) : null;
      const placeholderToken = hasText ? parseSingleTemplatePlaceholderToken(rawText) : "";
      const declMeta = hasText ? buildTemplateCellDeclMeta(obj, provenanceText, {
        templateKey: modelTemplateKey,
        rangeKey: String(entry.rangeKey || "")
      }) : null;
      const cellMeta = hasText
        ? {
            rangeKey: String(entry.rangeKey || ""),
            templateKey: modelTemplateKey,
            rawText,
            isSinglePlaceholder: Boolean(placeholderToken),
            placeholderToken,
            objectType: modelObjectType,
            ...(declMeta || {})
          }
        : null;
      const merge = cfg && cfg.merge === true;

      if (merge) {
        for (let row = range.r1; row <= range.r2; row += 1) {
          for (let col = range.c1; col <= range.c2; col += 1) {
            const cell = matrix[row - 1][col - 1];
            if (!cell) {
              continue;
            }
            const isAnchor = row === range.r1 && col === range.c1;
            if (isAnchor) {
              cell.hidden = false;
              cell.rowspan = range.r2 - range.r1 + 1;
              cell.colspan = range.c2 - range.c1 + 1;
              if (hasText) {
                cell.text = String(textMeta && textMeta.text ? textMeta.text : "");
                cell.hasPlaceholder = cell.hasPlaceholder || Boolean(textMeta && textMeta.hasPlaceholder);
                cell.hasTokenValue = cell.hasTokenValue || Boolean(textMeta && textMeta.hasTokenValue);
                cell.meta = cellMeta ? { ...cellMeta } : cell.meta;
              }
              const cellStyle = buildTemplateCellStyle(cfg, {
                isTop: true,
                isRight: true,
                isBottom: true,
                isLeft: true,
                isMergeAnchor: true
              });
              cell.style = { ...cell.style, ...cellStyle };
            } else {
              cell.hidden = true;
              cell.rowspan = 1;
              cell.colspan = 1;
            }
          }
        }
        continue;
      }

      for (let row = range.r1; row <= range.r2; row += 1) {
        for (let col = range.c1; col <= range.c2; col += 1) {
          const cell = matrix[row - 1][col - 1];
          if (!cell) {
            continue;
          }
          cell.hidden = false;
          cell.rowspan = 1;
          cell.colspan = 1;
          if (hasText) {
            cell.text = String(textMeta && textMeta.text ? textMeta.text : "");
            cell.hasPlaceholder = cell.hasPlaceholder || Boolean(textMeta && textMeta.hasPlaceholder);
            cell.hasTokenValue = cell.hasTokenValue || Boolean(textMeta && textMeta.hasTokenValue);
            cell.meta = cellMeta ? { ...cellMeta } : cell.meta;
          }
          const cellStyle = buildTemplateCellStyle(cfg, {
            isTop: row === range.r1,
            isRight: col === range.c2,
            isBottom: row === range.r2,
            isLeft: col === range.c1,
            isMergeAnchor: false
          });
          cell.style = { ...cell.style, ...cellStyle };
        }
      }
    }

    const compactedMatrix = applyTemplatePreviewOptions(matrix, options);
    return {
      matrix: compactedMatrix,
      maxRow: compactedMatrix.length,
      maxCol,
      errors,
      options
    };
  }

  function getTemplateCellCoordinate(rowIndex, colIndex) {
    let columnNumber = Math.max(1, Number(colIndex) + 1);
    let columnLabel = "";
    while (columnNumber > 0) {
      const remainder = (columnNumber - 1) % 26;
      columnLabel = String.fromCharCode(65 + remainder) + columnLabel;
      columnNumber = Math.floor((columnNumber - 1) / 26);
    }
    return `${columnLabel}${Math.max(1, Number(rowIndex) + 1)}`;
  }

  function moveTemplatePreviewCellFocus(table, currentCell, key) {
    const cells = Array.from(table.querySelectorAll("td.template-preview-editable"));
    const currentIndex = cells.indexOf(currentCell);
    if (currentIndex < 0) {
      return false;
    }

    let nextCell = null;
    if (key === "ArrowLeft" && currentIndex > 0) {
      nextCell = cells[currentIndex - 1];
    } else if (key === "ArrowRight" && currentIndex < cells.length - 1) {
      nextCell = cells[currentIndex + 1];
    } else if (key === "ArrowUp" || key === "ArrowDown") {
      const currentRow = Number(currentCell.getAttribute("data-template-grid-row"));
      const currentCol = Number(currentCell.getAttribute("data-template-grid-col"));
      const targetRow = currentRow + (key === "ArrowUp" ? -1 : 1);
      nextCell = cells.find((cell) => (
        Number(cell.getAttribute("data-template-grid-row")) === targetRow
        && Number(cell.getAttribute("data-template-grid-col")) === currentCol
      )) || cells.find((cell) => Number(cell.getAttribute("data-template-grid-row")) === targetRow) || null;
    }

    if (!nextCell) {
      return false;
    }

    for (const cell of cells) {
      cell.tabIndex = cell === nextCell ? 0 : -1;
    }
    nextCell.focus();
    return true;
  }

  function renderTemplateTable(model, handlers) {
    const matrix = model && Array.isArray(model.matrix) ? model.matrix : [];
    if (!matrix.length) {
      return null;
    }
    const options = model && model.options && typeof model.options === "object"
      ? model.options
      : normalizeTemplatePreviewOptions(null, null, null);
    const squareCells = options.squareCells !== false;
    const squareCellSize = parseTemplateOptionNumber(options.squareCellSize, 18, 16, 240);
    const handleCellDblClick = handlers && typeof handlers.onCellDblClick === "function"
      ? handlers.onCellDblClick
      : null;
    const invokeCellHandler = (handler, cellMeta, cellEl, ev) => {
      if (typeof handler !== "function") {
        return false;
      }
      try {
        handler(cellMeta, cellEl, ev);
        ev.__abapTemplateHandled = true;
        ev.preventDefault();
        ev.stopPropagation();
        return true;
      } catch (err) {
        if (typeof setError === "function") {
          setError("Template cell edit failed: " + (err && err.message ? err.message : String(err || "")));
        }
        return false;
      }
    };

    const table = el("table", {
      className: "template-preview-table",
      attrs: {
        style: squareCells
          ? "border-collapse:collapse;table-layout:fixed;width:max-content;min-width:max-content;"
          : "border-collapse:collapse;table-layout:auto;width:max-content;min-width:100%;"
      }
    });
    const tbody = el("tbody");
    let hasEditableTabStop = false;

    for (let rowIndex = 0; rowIndex < matrix.length; rowIndex += 1) {
      const row = matrix[rowIndex];
      const tr = el("tr");
      for (let colIndex = 0; colIndex < row.length; colIndex += 1) {
        const cell = row[colIndex];
        if (!cell || cell.hidden) {
          continue;
        }

        const td = document.createElement("td");
        if (cell.rowspan > 1) {
          td.rowSpan = cell.rowspan;
        }
        if (cell.colspan > 1) {
          td.colSpan = cell.colspan;
        }

        const cellText = String(cell.text || "");
        const textWrap = document.createElement("div");
        textWrap.textContent = cellText;
        const contentCssText = toInlineCssText(squareCells ? {
          display: "block",
          width: "100%",
          height: "100%",
          overflow: "visible",
          "white-space": "nowrap",
          "overflow-wrap": "normal",
          "word-break": "normal",
          "text-overflow": "clip",
          "line-height": "1",
          position: "relative",
          "z-index": "1",
          "pointer-events": "none"
        } : {
          display: "block",
          width: "100%",
          height: "100%"
        });
        if (contentCssText) {
          textWrap.setAttribute("style", contentCssText);
        }
        td.appendChild(textWrap);
        const colSpan = Math.max(1, Number(td.colSpan) || 1);
        const rowSpan = Math.max(1, Number(td.rowSpan) || 1);
        const baseMinWidth = squareCells ? `${squareCellSize}px` : "56px";
        const baseMaxWidth = squareCells ? `${squareCellSize}px` : "360px";
        const baseWidth = squareCells ? `${squareCellSize * colSpan}px` : "";
        const baseMinHeight = squareCells ? `${squareCellSize}px` : "";
        const baseHeight = squareCells ? `${squareCellSize * rowSpan}px` : "";
        const baseTextOverflow = "";
        const cssText = toInlineCssText({
          border: "none",
          "box-sizing": "border-box",
          "font-size": "10pt",
          "font-family": "\"MS PGothic\", \"MS UI Gothic\", Meiryo, sans-serif",
          color: "#111111",
          "background-color": "#ffffff",
          ...cell.style,
          "min-width": baseMinWidth,
          "max-width": baseMaxWidth,
          width: baseWidth,
          "min-height": baseMinHeight,
          height: baseHeight,
          padding: squareCells ? "0" : (cell.style && cell.style.padding ? cell.style.padding : "4px 6px"),
          "vertical-align": squareCells ? "middle" : (cell.style && cell.style["vertical-align"] ? cell.style["vertical-align"] : "top"),
          "white-space": squareCells ? "normal" : (cell.style && cell.style["white-space"] ? cell.style["white-space"] : "pre-wrap"),
          overflow: squareCells ? "visible" : (cell.style && cell.style.overflow ? cell.style.overflow : ""),
          "text-overflow": squareCells ? "" : (cell.style && cell.style["text-overflow"] ? cell.style["text-overflow"] : baseTextOverflow),
          "overflow-wrap": squareCells ? "normal" : (cell.style && cell.style["overflow-wrap"] ? cell.style["overflow-wrap"] : ""),
          "word-break": squareCells ? "normal" : (cell.style && cell.style["word-break"] ? cell.style["word-break"] : ""),
          "line-height": squareCells ? "1" : (cell.style && cell.style["line-height"] ? cell.style["line-height"] : ""),
          position: squareCells ? "relative" : (cell.style && cell.style.position ? cell.style.position : "")
        });
        if (cssText) {
          td.setAttribute("style", cssText);
        }

        const cellMeta = cell && cell.meta && typeof cell.meta === "object" ? cell.meta : null;
        if (cellMeta && cellMeta.rangeKey) {
          td.setAttribute("data-template-range-key", String(cellMeta.rangeKey));
        }
        if (handleCellDblClick) {
          const fallbackMeta = cellMeta || {
            rangeKey: String(td.getAttribute("data-template-range-key") || ""),
            templateKey: String(table.getAttribute("data-template-key") || ""),
            rawText: String(cell && cell.text ? cell.text : ""),
            isSinglePlaceholder: false,
            placeholderToken: "",
            objectType: String(table.getAttribute("data-object-type") || "")
          };
          td.__templateCellMeta = fallbackMeta;
          td.classList.add("template-preview-editable");
          td.tabIndex = hasEditableTabStop ? -1 : 0;
          hasEditableTabStop = true;
          td.setAttribute("role", "button");
          td.setAttribute("data-template-grid-row", String(rowIndex));
          td.setAttribute("data-template-grid-col", String(colIndex));
          td.setAttribute("title", "Double-click to edit this template cell");
          const coordinate = getTemplateCellCoordinate(rowIndex, colIndex);
          const accessibleText = cellText.trim();
          td.setAttribute(
            "aria-label",
            accessibleText
              ? `Edit template cell ${coordinate}: ${accessibleText}`
              : `Edit empty template cell ${coordinate}`
          );
          td.addEventListener("focus", () => {
            const editableCells = table.querySelectorAll("td.template-preview-editable");
            for (const editableCell of Array.from(editableCells)) {
              editableCell.tabIndex = editableCell === td ? 0 : -1;
            }
          });
          td.addEventListener("dblclick", (ev) => {
            if (ev.__abapTemplateHandled) {
              return;
            }
            invokeCellHandler(handleCellDblClick, fallbackMeta, td, ev);
          });
          td.addEventListener("keydown", (ev) => {
            if (ev.__abapTemplateHandled) {
              return;
            }
            if (["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"].includes(ev.key)) {
              if (moveTemplatePreviewCellFocus(table, td, ev.key)) {
                ev.preventDefault();
                ev.stopPropagation();
              }
              return;
            }
            if (ev.key !== "Enter" && ev.key !== " ") {
              return;
            }
            invokeCellHandler(handleCellDblClick, fallbackMeta, td, ev);
          });
        }

        tr.appendChild(td);
      }
      tbody.appendChild(tr);
    }

    if (handleCellDblClick) {
      table.addEventListener("dblclick", (ev) => {
        if (ev.__abapTemplateHandled) {
          return;
        }
        const target = ev.target && typeof ev.target.closest === "function"
          ? ev.target.closest("td")
          : null;
        if (!target || !table.contains(target)) {
          return;
        }
        const fallbackMeta = target.__templateCellMeta && typeof target.__templateCellMeta === "object"
          ? target.__templateCellMeta
          : {
              rangeKey: String(target.getAttribute("data-template-range-key") || ""),
              templateKey: String(table.getAttribute("data-template-key") || ""),
              rawText: String(target.textContent || ""),
              isSinglePlaceholder: false,
              placeholderToken: "",
              objectType: String(table.getAttribute("data-object-type") || "")
            };
        try {
          handleCellDblClick(fallbackMeta, target, ev);
          ev.__abapTemplateHandled = true;
          ev.preventDefault();
          ev.stopPropagation();
        } catch (err) {
          if (typeof setError === "function") {
            setError("Template cell edit failed: " + (err && err.message ? err.message : String(err || "")));
          }
        }
      });
    }

    table.appendChild(tbody);

    const scrollWrap = el("div", { className: "template-preview-table-scroll" });
    scrollWrap.appendChild(table);
    return scrollWrap;
  }

  async function copyHtmlWithFallback(html, plainText) {
    const safeHtml = String(html || "");
    const safeText = String(plainText || "");
    const clipboard = typeof navigator !== "undefined" && navigator ? navigator.clipboard : null;
    let lastClipboardError = null;

    if (
      safeHtml
      && clipboard
      && typeof clipboard.write === "function"
      && typeof window.ClipboardItem === "function"
    ) {
      try {
        const item = new window.ClipboardItem({
          "text/html": new Blob([safeHtml], { type: "text/html" }),
          "text/plain": new Blob([safeText], { type: "text/plain" })
        });
        await clipboard.write([item]);
        return;
      } catch (err) {
        lastClipboardError = err;
      }
    }

    if (clipboard && typeof clipboard.writeText === "function") {
      try {
        await clipboard.writeText(safeText);
        return;
      } catch (err) {
        lastClipboardError = err;
      }
    }

    const temp = document.createElement("div");
    temp.style.position = "fixed";
    temp.style.left = "-99999px";
    temp.style.top = "0";
    temp.setAttribute("contenteditable", "true");
    temp.innerHTML = safeHtml || safeText.replace(/\n/g, "<br>");
    document.body.appendChild(temp);

    const selection = window.getSelection();
    if (!selection) {
      document.body.removeChild(temp);
      if (lastClipboardError) {
        throw lastClipboardError;
      }
      throw new Error("Clipboard selection is unavailable.");
    }

    selection.removeAllRanges();
    const range = document.createRange();
    range.selectNodeContents(temp);
    selection.addRange(range);
    const copied = document.execCommand("copy");
    selection.removeAllRanges();
    document.body.removeChild(temp);

    if (!copied) {
      if (lastClipboardError) {
        throw lastClipboardError;
      }
      throw new Error("Copy failed in this browser.");
    }
  }

  function resolveTemplateMapForObject(obj, config) {
    const templates = config && typeof config === "object" && config.templates && typeof config.templates === "object"
      ? config.templates
      : {};
    const objectType = obj && obj.objectType ? String(obj.objectType) : "";
    const isAppendLinesOf = objectType === "APPEND"
      && obj.extras
      && obj.extras.append
      && String(obj.extras.append.variant || "") === "linesOf";
    const isInsertLinesOf = Boolean(objectType === "INSERT_ITAB"
      && obj.values
      && obj.values.source
      && String(obj.values.source.value || "").trim());
    const preferredKeys = isAppendLinesOf
      ? ["APPEND_LINES_OF", "APPEND"]
      : (isInsertLinesOf ? ["INSERT_LINES_OF", "INSERT_ITAB"] : [objectType]);
    for (const templateKey of preferredKeys) {
      if (templateKey && Object.prototype.hasOwnProperty.call(templates, templateKey)) {
        const resolved = resolveTemplateDefinitionForPreview(templates[templateKey]);
        return { key: templateKey, map: resolved.map, options: resolved.options };
      }
    }
    const defaultConfig = getDefaultTemplateConfig();
    const defaultTemplates = defaultConfig && defaultConfig.templates && typeof defaultConfig.templates === "object"
      ? defaultConfig.templates
      : {};
    for (const templateKey of preferredKeys) {
      if (templateKey && Object.prototype.hasOwnProperty.call(defaultTemplates, templateKey)) {
        const resolved = resolveTemplateDefinitionForPreview(defaultTemplates[templateKey]);
        return { key: templateKey, map: resolved.map, options: resolved.options };
      }
    }
    const resolved = resolveTemplateDefinitionForPreview(null);
    return { key: "", map: null, options: resolved.options };
  }

  function buildTemplatePlainTextFromBlock(block) {
    if (!block) {
      return "";
    }
    return String(block.innerText || block.textContent || "").trim();
  }

  function isTemplateCopyTableOnlyEnabled() {
    return Boolean(els.templateCopyTableOnly && els.templateCopyTableOnly.checked);
  }

  function buildTemplateCopyPayloadFromBlock(block) {
    if (!block || typeof block.cloneNode !== "function") {
      return { node: null, text: "" };
    }

    const clone = block.cloneNode(true);
    const actionButtons = clone.querySelectorAll("[data-template-action]");
    for (const actionBtn of Array.from(actionButtons)) {
      actionBtn.remove();
    }

    if (isTemplateCopyTableOnlyEnabled()) {
      const table = clone.querySelector(".template-preview-table");
      if (table) {
        return {
          node: table.cloneNode(true),
          text: buildTemplatePlainTextFromBlock(table)
        };
      }
    }

    return {
      node: clone,
      text: buildTemplatePlainTextFromBlock(clone)
    };
  }

  function syncTemplateEditorFromState() {
    if (!els.templateConfigJson) {
      return;
    }
    const pretty = safeJson(state.templateConfig || getDefaultTemplateConfig(), true);
    state.templateConfigDraft = pretty;
    els.templateConfigJson.value = pretty;
  }

  function applyTemplateConfigObject(config, options) {
    const opts = options && typeof options === "object" ? options : {};
    const check = validateTemplateConfig(config);
    if (!check.valid) {
      setTemplateConfigError(check.errors.join("\n"));
      return false;
    }

    const next = cloneJsonValue(config);
    if (!next || typeof next !== "object") {
      setTemplateConfigError("Cannot clone template config.");
      return false;
    }

    mergeMissingDefaultTemplatesInPlace(next);

    state.templateConfig = next;
    if (opts.save !== false) {
      saveTemplateConfig(next);
    }
    setTemplateConfigError("");
    syncTemplateEditorFromState();
    renderTemplatePreview();
    return true;
  }

  function applyTemplateConfigFromEditor() {
    const raw = els.templateConfigJson ? String(els.templateConfigJson.value || "").trim() : "";
    if (!raw) {
      setTemplateConfigError("Template config JSON is empty.");
      return;
    }

    let parsed = null;
    try {
      parsed = JSON.parse(raw);
    } catch (err) {
      setTemplateConfigError(`JSON parse error: ${err && err.message ? err.message : err}`);
      return;
    }

    applyTemplateConfigObject(parsed, { save: true });
  }

  function resetTemplateConfig() {
    const defaultConfig = getDefaultTemplateConfig();
    const currentConfig = state.templateConfig || defaultConfig;
    if (JSON.stringify(currentConfig) === JSON.stringify(defaultConfig)) {
      return true;
    }

    const confirmed = typeof window.confirm === "function"
      && window.confirm("Reset the saved template config to default? Your custom template changes will be replaced.");
    if (!confirmed) {
      return false;
    }

    return applyTemplateConfigObject(defaultConfig, { save: true });
  }

  function getTemplateVirtualState() {
    if (!state.templateVirtual || typeof state.templateVirtual !== "object") {
      state.templateVirtual = {
        items: [],
        itemCount: 0,
        start: 0,
        end: 0,
        lastScrollTop: 0,
        scrollDir: "down",
        pendingRaf: 0,
        isAdjustingScroll: false,
        avgItemHeight: 140,
        unknownItemHeight: 140,
        estimateCalibrated: false,
        adjustmentRaf: 0,
        adjustmentGeneration: 0,
        needsScrollSync: false,
        isRenderTransaction: false,
        geometryEpoch: 0,
        itemHeights: new Float64Array(0),
        prefixOffsets: new Float64Array(1),
        sourceRenderObjects: null,
        lineTargetMap: new Map(),
        isInitialized: false
      };
    }
    if (!(state.templateVirtual.lineTargetMap instanceof Map)) {
      state.templateVirtual.lineTargetMap = new Map();
    }
    if (!(state.templateVirtual.itemHeights instanceof Float64Array)) {
      state.templateVirtual.itemHeights = new Float64Array(0);
    }
    if (!(state.templateVirtual.prefixOffsets instanceof Float64Array)) {
      state.templateVirtual.prefixOffsets = new Float64Array(1);
    }
    if (!Number.isFinite(Number(state.templateVirtual.unknownItemHeight)) || Number(state.templateVirtual.unknownItemHeight) <= 0) {
      state.templateVirtual.unknownItemHeight = 140;
    }
    if (typeof state.templateVirtual.estimateCalibrated !== "boolean") {
      state.templateVirtual.estimateCalibrated = false;
    }
    if (!Number.isFinite(Number(state.templateVirtual.adjustmentRaf))) {
      state.templateVirtual.adjustmentRaf = 0;
    }
    if (!Number.isFinite(Number(state.templateVirtual.adjustmentGeneration))) {
      state.templateVirtual.adjustmentGeneration = 0;
    }
    if (typeof state.templateVirtual.needsScrollSync !== "boolean") {
      state.templateVirtual.needsScrollSync = false;
    }
    if (typeof state.templateVirtual.isRenderTransaction !== "boolean") {
      state.templateVirtual.isRenderTransaction = false;
    }
    if (!Number.isFinite(Number(state.templateVirtual.geometryEpoch))) {
      state.templateVirtual.geometryEpoch = 0;
    }
    return state.templateVirtual;
  }

  function getTemplateVirtualConfig(container, avgItemHeight) {
    const safeAvg = Math.max(1, Number(avgItemHeight) || 1);
    const clientHeight = Math.max(0, Number(container && container.clientHeight) || 0);
    const visibleEstimate = Math.max(1, Math.ceil(clientHeight / safeAvg));
    const overscanCount = Math.max(2, Math.ceil(visibleEstimate * 0.5));
    const batchCount = Math.max(4, Math.ceil(visibleEstimate * 0.5));
    const targetCount = visibleEstimate + (overscanCount * 2);
    const maxCount = targetCount + (batchCount * 2);
    const edgeThresholdPx = Math.max(40, Math.round(clientHeight * 0.2));
    return { visibleEstimate, overscanCount, batchCount, targetCount, maxCount, edgeThresholdPx };
  }

  function measureTemplateOuterHeight(node) {
    if (!node || typeof node.getBoundingClientRect !== "function") {
      return 0;
    }
    const rect = node.getBoundingClientRect();
    let marginTop = 0;
    let marginBottom = 0;
    try {
      const style = window.getComputedStyle(node);
      marginTop = Number.parseFloat(style && style.marginTop ? style.marginTop : "0") || 0;
      marginBottom = Number.parseFloat(style && style.marginBottom ? style.marginBottom : "0") || 0;
    } catch {
      // ignore
    }
    const container = els.templatePreviewOutput;
    const containerRect = container && typeof container.getBoundingClientRect === "function"
      ? container.getBoundingClientRect()
      : null;
    const layoutHeight = Math.max(0, Number(container && container.offsetHeight) || Number(container && container.clientHeight) || 0);
    const measuredScale = layoutHeight > 0 ? (Number(containerRect && containerRect.height) || 0) / layoutHeight : 1;
    const scrollScale = Number.isFinite(measuredScale) && measuredScale >= 0.5 && measuredScale <= 2
      ? measuredScale
      : 1;
    return Math.max(0, (rect.height / scrollScale) + marginTop + marginBottom);
  }

  function updateTemplateAverageHeight(virtual, heights) {
    const list = Array.isArray(heights) ? heights.filter((v) => Number(v) > 0) : [];
    if (!list.length) {
      return;
    }
    const sampleAvg = list.reduce((sum, value) => sum + Number(value || 0), 0) / list.length;
    const prev = Math.max(60, Number(virtual.avgItemHeight) || 140);
    const next = Math.max(60, ((prev * 0.8) + (sampleAvg * 0.2)));
    virtual.avgItemHeight = Math.round(next);
  }

  function calibrateTemplateUnknownItemHeight(virtual, heights) {
    if (!virtual || virtual.estimateCalibrated) {
      return;
    }
    const samples = Array.isArray(heights)
      ? heights
        .map((value) => Number(value))
        .filter((value) => Number.isFinite(value) && value >= 24)
        .sort((left, right) => left - right)
      : [];
    if (!samples.length) {
      return;
    }
    const middle = Math.floor(samples.length / 2);
    const median = samples.length % 2
      ? samples[middle]
      : ((samples[middle - 1] + samples[middle]) / 2);
    virtual.unknownItemHeight = Math.max(24, median);
    virtual.avgItemHeight = virtual.unknownItemHeight;
    virtual.estimateCalibrated = true;
    virtual.geometryEpoch = (Number(virtual.geometryEpoch) || 0) + 1;
  }

  function buildTemplateLineTargetMap(items) {
    const map = new Map();
    const list = Array.isArray(items) ? items : [];
    for (let index = 0; index < list.length; index += 1) {
      const item = list[index];
      const obj = item && item.obj ? item.obj : null;
      if (!obj) {
        continue;
      }
      const line = Number(obj.lineStart) || 0;
      if (!line || map.has(line)) {
        continue;
      }
      map.set(line, { kind: "template", index: String(index) });
    }
    return map;
  }

  function createTemplateBlockIconButton(action, title, svgMarkup) {
    const btn = el("button", {
      className: "template-block-icon-btn",
      attrs: {
        type: "button",
        title: String(title || ""),
        "aria-label": String(title || ""),
        "data-template-action": String(action || "")
      }
    });
    btn.innerHTML = String(svgMarkup || "");
    const svg = btn.querySelector("svg");
    if (svg) {
      svg.setAttribute("aria-hidden", "true");
      svg.setAttribute("focusable", "false");
    }
    return btn;
  }

  const TEMPLATE_BLOCK_ICON_SVG = {
    code: '<svg viewBox="0 0 16 16" width="14" height="14"><path fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" d="M5.5 3.5 2.5 8l3 4.5M10.5 3.5l3 4.5-3 4.5"/></svg>',
    paths: '<svg viewBox="0 0 16 16" width="14" height="14"><path fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" d="M3 4.5h10M3 8h10M3 11.5h7"/></svg>',
    copy: '<svg viewBox="0 0 16 16" width="14" height="14"><rect x="5.5" y="2.5" width="7.5" height="9.5" rx="1.2" fill="none" stroke="currentColor" stroke-width="1.5"/><path fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" d="M3.5 5.5v7.3c0 .7.5 1.2 1.2 1.2h6"/></svg>'
  };

  function buildTemplateBlockElement(item, absIndex, config, interactive) {
    const row = item && typeof item === "object" ? item : null;
    const obj = row && row.obj ? row.obj : null;
    if (!obj) {
      return null;
    }

    const isInteractive = interactive !== false;
    const depth = Math.max(0, Number(row.depth) || 0);
    const lineStart = Number(obj.lineStart) || 0;
    const indexText = String(absIndex);
    const templateContextObj = buildTemplateContextObject(obj, absIndex + 1);
    const resolved = resolveTemplateMapForObject(obj, config);
    const selectedIndexes = typeof getSelectedTemplateIndexSet === "function"
      ? getSelectedTemplateIndexSet()
      : new Set(state.selectedTemplateIndex !== "" ? [String(state.selectedTemplateIndex)] : []);
    const selected = selectedIndexes.has(indexText);
    const blockAttrs = {
      "data-template-index": indexText,
      "data-depth": String(depth),
      "aria-selected": selected ? "true" : "false"
    };
    if (lineStart > 0) {
      blockAttrs["data-line-start"] = String(lineStart);
    }
    const block = el("div", { className: `template-block${selected ? " selected" : ""}`, attrs: blockAttrs });
    const indentPx = Math.min(120, depth * 12);
    if (indentPx > 0) {
      block.style.marginLeft = `${indentPx}px`;
    } else {
      block.style.marginLeft = "";
    }

    const header = el("div", { className: "template-block-header" });
    const left = el("div");
    const technicalObjectType = String(obj.objectType || "OBJECT");
    const objectDisplayLabel = String(resolved.options && resolved.options.objectLabel || "").trim()
      || technicalObjectType;
    const label = getObjectLabel(obj);
    const titleText = `${absIndex + 1}. ${objectDisplayLabel}${label ? ` ${label}` : ""}`;
    const titleRow = el("div", { className: "template-block-title-row" });
    titleRow.appendChild(el("h4", {
      className: "template-block-title",
      text: titleText,
      attrs: { title: technicalObjectType }
    }));
    left.appendChild(titleRow);
    const meta = renderMeta(obj);
    left.appendChild(el("div", { className: "template-block-meta", text: meta || "" }));
    header.appendChild(left);

    if (isInteractive) {
      const titleActions = el("div", { className: "template-block-title-actions" });
      const codeBtn = createTemplateBlockIconButton("code", "Code", TEMPLATE_BLOCK_ICON_SVG.code);
      codeBtn.addEventListener("click", (ev) => {
        if (ev && typeof ev.stopPropagation === "function") {
          ev.stopPropagation();
        }
        if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(indexText);
        } else {
          state.selectedTemplateIndex = indexText;
        }
        if (lineStart > 0 && typeof selectCodeLines === "function") {
          const lineEnd = Number(obj && obj.block && obj.block.lineEnd) || lineStart;
          selectCodeLines(lineStart, lineEnd);
        }
      });
      titleActions.appendChild(codeBtn);

      const pathsBtn = createTemplateBlockIconButton("paths", "Paths", TEMPLATE_BLOCK_ICON_SVG.paths);
      pathsBtn.addEventListener("click", (ev) => {
        if (ev && typeof ev.stopPropagation === "function") {
          ev.stopPropagation();
        }
        openTemplatePathDump(templateContextObj, absIndex, obj);
      });
      titleActions.appendChild(pathsBtn);

      const copyBtn = createTemplateBlockIconButton("copy", "Copy", TEMPLATE_BLOCK_ICON_SVG.copy);
      copyBtn.addEventListener("click", async (ev) => {
        if (ev && typeof ev.stopPropagation === "function") {
          ev.stopPropagation();
        }
        try {
          const payload = buildTemplateCopyPayloadFromBlock(block);
          if (!payload.node) {
            setError("Nothing to copy.");
            return;
          }
          await copyHtmlWithFallback(payload.node.outerHTML, payload.text);
          setError("");
        } catch (err) {
          setError(`Copy failed: ${err && err.message ? err.message : err}`);
        }
      });
      titleActions.appendChild(copyBtn);
      titleRow.appendChild(titleActions);

      const performSourceControl = typeof createPerformSourceControl === "function"
        ? createPerformSourceControl(obj)
        : null;
      if (performSourceControl) {
        left.appendChild(performSourceControl);
      }

      block.addEventListener("click", (ev) => {
        if (typeof selectTemplateBlockFromInteraction === "function") {
          selectTemplateBlockFromInteraction(indexText, ev);
        } else if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(indexText, { scroll: false });
        } else {
          state.selectedTemplateIndex = indexText;
        }
      });
    }

    block.appendChild(header);

    if (!resolved.map || typeof resolved.map !== "object") {
      block.appendChild(el("div", { className: "template-empty", text: "[Missing template]" }));
      return block;
    }

    const model = buildTemplateGridModel(templateContextObj, resolved.map, resolved.options, {
      templateKey: resolved.key || "",
      objectType: String(obj.objectType || "")
    });
    if (model.errors.length) {
      block.appendChild(el("div", { className: "template-error", text: model.errors.join("\n") }));
    }

    const openCellUnifiedEditor = (cellMeta, cellElement) => {
      if (!cellMeta || typeof cellMeta !== "object") {
        return;
      }


      const templateKey = String(cellMeta.templateKey || resolved.key || "").trim();
      const rangeKey = String(cellMeta.rangeKey || "").trim();
      const currentText = cellMeta.rawText === undefined || cellMeta.rawText === null
        ? ""
        : String(cellMeta.rawText);
      const token = String(cellMeta.placeholderToken || "").trim();

      const getDeclLookupKeysSafe = (decl) => {
        const keys = [];
        const pushKey = (value) => {
          const key = String(value || "").trim();
          if (key && !keys.includes(key)) {
            keys.push(key);
          }
        };
        if (typeof getDeclOverrideLookupKeys === "function") {
          try {
            for (const key of getDeclOverrideLookupKeys(decl)) {
              pushKey(key);
            }
          } catch (err) {
            warnTemplateProvenanceOnce("decl-lookup-keys", {
              objectId: obj && obj.id,
              line: obj && obj.lineStart,
              template: templateKey,
              range: rangeKey,
              token,
              error: err
            });
          }
        }
        pushKey(typeof getDeclOverrideStorageKey === "function" ? getDeclOverrideStorageKey(decl) : "");
        for (const key of Array.isArray(decl && decl.overrideLookupKeys) ? decl.overrideLookupKeys : []) {
          pushKey(key);
        }
        return keys;
      };

      const getDescOverrideEntrySafe = (decl, lookupKeys) => {
        if (!decl || typeof decl !== "object") {
          return null;
        }
        if (typeof getDescOverrideEntry === "function") {
          try {
            return getDescOverrideEntry(decl);
          } catch (err) {
            warnTemplateProvenanceOnce("desc-override-read", {
              objectId: obj && obj.id,
              line: obj && obj.lineStart,
              template: templateKey,
              range: rangeKey,
              token,
              error: err
            });
          }
        }
        let rawOverride = null;
        for (const key of Array.isArray(lookupKeys) ? lookupKeys : []) {
          if (state && state.descOverrides && Object.prototype.hasOwnProperty.call(state.descOverrides, key)) {
            rawOverride = state.descOverrides[key];
            break;
          }
        }
        if (rawOverride === undefined || rawOverride === null) {
          return null;
        }
        if (typeof rawOverride === "string") {
          return { text: rawOverride, noNormalize: false };
        }
        if (typeof rawOverride === "object") {
          return {
            text: String(rawOverride.text || ""),
            noNormalize: rawOverride.noNormalize === true
          };
        }
        return null;
      };

      const hasCellDeclCandidates = Object.prototype.hasOwnProperty.call(cellMeta, "declCandidates")
        && Array.isArray(cellMeta.declCandidates);
      let targetDeclCandidates = hasCellDeclCandidates ? cellMeta.declCandidates.slice() : [];
      if (!hasCellDeclCandidates && token) {
        try {
          const resolvedValue = resolveTemplatePlaceholderValue(templateContextObj, token);
          targetDeclCandidates = getTemplateEditableDeclCandidatesFromResolvedValue(resolvedValue);
          if (!targetDeclCandidates.length) {
            targetDeclCandidates = resolveTemplateEditableDeclCandidatesFromToken(templateContextObj, token);
          }
        } catch (err) {
          warnTemplateProvenanceOnce("RESOLUTION_ERROR", {
            objectId: obj && obj.id,
            line: obj && obj.lineStart,
            template: templateKey,
            range: rangeKey,
            token,
            error: err
          });
          targetDeclCandidates = [];
        }
      }
      targetDeclCandidates = dedupeTemplateDecls(targetDeclCandidates);

      const modalDeclCandidates = [];
      for (let index = 0; index < targetDeclCandidates.length; index += 1) {
        const decl = targetDeclCandidates[index];
        if (!decl || typeof decl !== "object") {
          continue;
        }
        let declKey = "";
        let lookupKeys = [];
        try {
          lookupKeys = getDeclLookupKeysSafe(decl);
          declKey = lookupKeys.length
            ? lookupKeys[0]
            : (typeof getDeclOverrideStorageKey === "function" ? getDeclOverrideStorageKey(decl) : "");
        } catch (err) {
          warnTemplateProvenanceOnce("decl-key", {
            objectId: obj && obj.id,
            line: obj && obj.lineStart,
            template: templateKey,
            range: rangeKey,
            token,
            error: err
          });
          declKey = "";
        }
        const descEntry = getDescOverrideEntrySafe(decl, lookupKeys);
        const techName = typeof getDeclTechName === "function" ? getDeclTechName(decl) : String(decl && decl.name ? decl.name : "");
        const scopeName = String(decl && decl.scopeLabel ? decl.scopeLabel : "").trim();
        const label = scopeName ? (techName || "(unknown)") + " @ " + scopeName : (techName || "(unknown)");
        modalDeclCandidates.push({
          decl,
          declKey,
          lookupKeys,
          label,
          currentDesc: String(descEntry && descEntry.text ? descEntry.text : ""),
          skipNormalize: Boolean(descEntry && descEntry.noNormalize),
          selected: index === 0
        });
      }

      openTemplateCellUnifiedEditModal({
        metadata: {
          objectType: String(cellMeta.objectType || obj.objectType || ""),
          templateKey,
          rangeKey,
          token
        },
        textPart: {
          templateKey,
          rangeKey,
          objectType: String(cellMeta.objectType || obj.objectType || ""),
          currentText,
          onSaveText: ({ text: nextText }) => {
            if (!templateKey || !rangeKey) {
              return { ok: false, error: "Missing template key or range key." };
            }
            const baseConfig = state.templateConfig && typeof state.templateConfig === "object"
              ? state.templateConfig
              : getDefaultTemplateConfig();
            const nextConfig = {
              ...baseConfig,
              templates: { ...(baseConfig.templates && typeof baseConfig.templates === "object" ? baseConfig.templates : {}) }
            };
            const currentTemplate = nextConfig.templates[templateKey];
            const nextTemplate = currentTemplate && typeof currentTemplate === "object" && !Array.isArray(currentTemplate)
              ? { ...currentTemplate }
              : {};
            const existingCell = nextTemplate[rangeKey];
            const nextCell = existingCell && typeof existingCell === "object" && !Array.isArray(existingCell)
              ? { ...existingCell }
              : {};
            nextCell.text = String(nextText === undefined || nextText === null ? "" : nextText);
            nextTemplate[rangeKey] = nextCell;
            nextConfig.templates[templateKey] = nextTemplate;
            const applied = applyTemplateConfigObject(nextConfig, { save: true });
            if (!applied) {
              return { ok: false, error: "Failed to apply template config." };
            }
            setError("");
            return { ok: true };
          }
        },
        descPart: {
          token,
          status: String(cellMeta.status || "unresolved"),
          reasonCode: String(cellMeta.reasonCode || "MISSING_PROVENANCE"),
          sourcePath: String(cellMeta.sourcePath || token),
          declCandidates: modalDeclCandidates,
          onSaveDesc: ({ decl, decls, text: nextDesc, skipNormalize }) => {
            if (typeof applyDeclDescriptionOverride !== "function") {
              return { ok: false, error: "Description override helpers are unavailable." };
            }
            const targetDecls = Array.isArray(decls) && decls.length
              ? decls.filter((item) => item && typeof item === "object")
              : (decl && typeof decl === "object" ? [decl] : []);
            if (!targetDecls.length) {
              return { ok: false, error: "Decl target is unavailable." };
            }
            const raw = String(nextDesc === undefined || nextDesc === null ? "" : nextDesc);
            const clear = !raw.trim();
            state.pendingTemplateViewportAnchor = captureTemplateViewportAnchor({
              templateIndex: absIndex,
              gridRow: Number(cellElement && cellElement.getAttribute("data-template-grid-row")),
              gridCol: Number(cellElement && cellElement.getAttribute("data-template-grid-col"))
            });

            let wroteAnyKey = false;
            for (const targetDecl of targetDecls) {
              try {
                const applied = applyDeclDescriptionOverride({
                  decl: targetDecl,
                  text: raw,
                  skipNormalize: Boolean(skipNormalize),
                  clear
                });
                if (applied === false || (applied && applied.ok === false)) {
                  continue;
                }
                wroteAnyKey = true;
              } catch (err) {
                warnTemplateProvenanceOnce("decl-key", {
                  objectId: obj && obj.id,
                  line: obj && obj.lineStart,
                  template: templateKey,
                  range: rangeKey,
                  token,
                  error: err
                });
                continue;
              }
            }

            if (!wroteAnyKey) {
              state.pendingTemplateViewportAnchor = null;
              return { ok: false, error: "Decl key is unavailable." };
            }
            if (typeof renderTemplatePreview === "function") {
              renderTemplatePreview();
            }
            if (typeof renderDeclDescPanelUi === "function" && state.rightTab === "descriptions") {
              renderDeclDescPanelUi();
            }
            return { ok: true };
          }
        }
      });
    };

    const table = renderTemplateTable(model, isInteractive ? {
      onCellDblClick: (cellMeta, cellElement) => {
        if (!cellMeta || typeof cellMeta !== "object") {
          return;
        }
        openCellUnifiedEditor(cellMeta, cellElement);
      }
    } : null);
    if (table) {
      const previewTable = table.querySelector(".template-preview-table");
      if (previewTable) {
        previewTable.setAttribute("data-template-key", String(resolved.key || ""));
        previewTable.setAttribute("data-object-type", String(obj.objectType || ""));
        previewTable.setAttribute("data-template-index", indexText);
      }
      block.appendChild(table);
    } else {
      block.appendChild(el("div", { className: "template-empty", text: "[Missing template]" }));
    }

    return block;
  }

  function buildTemplateBlockCopyPayload(item, absIndex, config, tableOnly) {
    const block = buildTemplateBlockElement(item, absIndex, config, false);
    if (!block || typeof block.cloneNode !== "function") {
      return { node: null, text: "" };
    }

    const clone = block.cloneNode(true);
    const actionButtons = clone.querySelectorAll("[data-template-action]");
    for (const actionBtn of Array.from(actionButtons)) {
      actionBtn.remove();
    }

    if (tableOnly) {
      const table = clone.querySelector(".template-preview-table");
      if (table) {
        return {
          node: table.cloneNode(true),
          text: buildTemplatePlainTextFromBlock(table)
        };
      }
    }

    return {
      node: clone,
      text: buildTemplatePlainTextFromBlock(clone)
    };
  }

  function normalizeTemplateCopyIndexes(indexes, itemCount) {
    const total = Math.max(0, Number(itemCount) || 0);
    return Array.from(new Set((Array.isArray(indexes) ? indexes : [])
      .map((value) => Number(value))
      .filter((value) => Number.isInteger(value) && value >= 0 && value < total)))
      .sort((left, right) => left - right);
  }

  function buildTemplateCollectionCopyPayload(items, indexes, config, tableOnly) {
    const sourceItems = Array.isArray(items) ? items : [];
    const orderedIndexes = normalizeTemplateCopyIndexes(indexes, sourceItems.length);
    const payloads = [];
    for (const index of orderedIndexes) {
      const payload = buildTemplateBlockCopyPayload(sourceItems[index], index, config, Boolean(tableOnly));
      if (payload && payload.node) {
        payloads.push(payload);
      }
    }

    if (!payloads.length) {
      return { node: null, html: "", text: "", count: 0, spacerCount: 0 };
    }

    const plainText = payloads.map((payload) => String(payload.text || "").trim()).join("\n\n");
    if (tableOnly) {
      const combinedTable = payloads[0].node.cloneNode(false);
      const tbody = document.createElement("tbody");
      let maxColumns = 1;
      for (const payload of payloads) {
        for (const row of Array.from(payload.node.querySelectorAll("tr"))) {
          const columnCount = Array.from(row.children).reduce((sum, cell) => sum + Math.max(1, Number(cell.colSpan) || 1), 0);
          maxColumns = Math.max(maxColumns, columnCount);
        }
      }
      payloads.forEach((payload, payloadIndex) => {
        if (payloadIndex > 0) {
          const spacerRow = document.createElement("tr");
          spacerRow.setAttribute("data-template-spacer", "true");
          const spacerCell = document.createElement("td");
          spacerCell.colSpan = maxColumns;
          spacerCell.setAttribute("data-template-spacer-cell", "true");
          spacerRow.appendChild(spacerCell);
          tbody.appendChild(spacerRow);
        }
        for (const row of Array.from(payload.node.querySelectorAll("tr"))) {
          tbody.appendChild(row.cloneNode(true));
        }
      });
      combinedTable.appendChild(tbody);
      return {
        node: combinedTable,
        html: combinedTable.outerHTML,
        text: plainText,
        count: payloads.length,
        spacerCount: Math.max(0, payloads.length - 1)
      };
    }

    const wrapper = document.createElement("div");
    payloads.forEach((payload, payloadIndex) => {
      if (payloadIndex > 0) {
        const separator = document.createElement("div");
        separator.setAttribute("data-template-spacer", "true");
        separator.appendChild(document.createElement("br"));
        wrapper.appendChild(separator);
      }
      wrapper.appendChild(payload.node);
    });
    return {
      node: wrapper,
      html: wrapper.innerHTML,
      text: plainText,
      count: payloads.length,
      spacerCount: Math.max(0, payloads.length - 1)
    };
  }

  function getTemplateEstimatedItemHeight(virtual) {
    return Math.max(24, Number(virtual && virtual.avgItemHeight) || 140);
  }

  function getTemplateUnknownItemHeight(virtual) {
    return Math.max(24, Number(virtual && virtual.unknownItemHeight) || 140);
  }

  function ensureTemplateHeightCache(virtual, itemCount) {
    const total = Math.max(0, Number(itemCount) || 0);
    if (!(virtual.itemHeights instanceof Float64Array) || virtual.itemHeights.length !== total) {
      virtual.itemHeights = new Float64Array(total);
    }
    if (!(virtual.prefixOffsets instanceof Float64Array) || virtual.prefixOffsets.length !== (total + 1)) {
      virtual.prefixOffsets = new Float64Array(total + 1);
    }
  }

  function rebuildTemplatePrefixOffsets(virtual) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    ensureTemplateHeightCache(virtual, total);
    const estimate = getTemplateUnknownItemHeight(virtual);
    virtual.prefixOffsets[0] = 0;
    for (let index = 0; index < total; index += 1) {
      const measured = Number(virtual.itemHeights[index]) || 0;
      virtual.prefixOffsets[index + 1] = virtual.prefixOffsets[index] + (measured > 0 ? measured : estimate);
    }
  }

  function getTemplateOffsetAtIndex(virtual, index) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    const safeIndex = Math.max(0, Math.min(total, Number(index) || 0));
    if (!(virtual.prefixOffsets instanceof Float64Array) || virtual.prefixOffsets.length !== (total + 1)) {
      rebuildTemplatePrefixOffsets(virtual);
    }
    return Number(virtual.prefixOffsets[safeIndex]) || 0;
  }

  function findTemplateIndexAtOffset(virtual, scrollTop) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    if (!total) {
      return 0;
    }
    if (!(virtual.prefixOffsets instanceof Float64Array) || virtual.prefixOffsets.length !== (total + 1)) {
      rebuildTemplatePrefixOffsets(virtual);
    }
    const target = Math.max(0, Number(scrollTop) || 0);
    let low = 0;
    let high = total;
    while (low < high) {
      const mid = Math.floor((low + high) / 2);
      if (virtual.prefixOffsets[mid + 1] <= target) {
        low = mid + 1;
      } else {
        high = mid;
      }
    }
    return Math.max(0, Math.min(total - 1, low));
  }

  function measureRenderedTemplateItems(virtual, start, end) {
    if (!els.templatePreviewOutput) {
      return;
    }
    const heights = [];
    for (const node of Array.from(els.templatePreviewOutput.querySelectorAll(".template-block[data-template-index]"))) {
      const index = Number(node.getAttribute("data-template-index"));
      if (!Number.isFinite(index) || index < start || index >= end || index >= virtual.itemHeights.length) {
        continue;
      }
      const height = measureTemplateOuterHeight(node);
      if (height > 0) {
        virtual.itemHeights[index] = height;
        heights.push(height);
      }
    }
    calibrateTemplateUnknownItemHeight(virtual, heights);
    updateTemplateAverageHeight(virtual, heights);
    rebuildTemplatePrefixOffsets(virtual);
    const topSpacer = els.templatePreviewOutput.querySelector(".template-virtual-spacer-top");
    const bottomSpacer = els.templatePreviewOutput.querySelector(".template-virtual-spacer-bottom");
    if (topSpacer) {
      topSpacer.style.height = `${getTemplateOffsetAtIndex(virtual, start)}px`;
    }
    if (bottomSpacer) {
      const totalHeight = getTemplateOffsetAtIndex(virtual, virtual.itemCount);
      bottomSpacer.style.height = `${Math.max(0, totalHeight - getTemplateOffsetAtIndex(virtual, end))}px`;
    }
  }

  function computeTemplateVirtualRangeFromScroll(scrollTop) {
    const virtual = getTemplateVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!els.templatePreviewOutput || !total) {
      return { start: 0, end: 0 };
    }

    const estimatedHeight = getTemplateEstimatedItemHeight(virtual);
    const metrics = getTemplateVirtualConfig(els.templatePreviewOutput, estimatedHeight);
    if (total <= metrics.targetCount) {
      return { start: 0, end: total };
    }

    const top = Math.max(0, Number(scrollTop) || 0);
    const firstVisible = findTemplateIndexAtOffset(virtual, top);
    let start = Math.max(0, firstVisible - metrics.overscanCount);
    let end = Math.min(total, start + metrics.targetCount);
    if ((end - start) < metrics.targetCount) {
      start = Math.max(0, end - metrics.targetCount);
    }

    return { start, end };
  }

  function cancelTemplateVirtualAdjustment(virtual) {
    if (!virtual || typeof virtual !== "object") {
      return;
    }
    if (virtual.adjustmentRaf) {
      cancelAnimationFrame(virtual.adjustmentRaf);
      virtual.adjustmentRaf = 0;
    }
    virtual.adjustmentGeneration = (Number(virtual.adjustmentGeneration) || 0) + 1;
    virtual.isAdjustingScroll = false;
  }

  function beginTemplateVirtualAdjustment(virtual) {
    cancelTemplateVirtualAdjustment(virtual);
    virtual.isAdjustingScroll = true;
    return Number(virtual.adjustmentGeneration) || 0;
  }

  function finishTemplateVirtualAdjustment(virtual, generation) {
    if (!virtual || Number(virtual.adjustmentGeneration) !== Number(generation)) {
      return false;
    }
    virtual.adjustmentRaf = 0;
    virtual.isAdjustingScroll = false;
    if (virtual.needsScrollSync) {
      scheduleTemplateVirtualScroll();
    }
    return true;
  }

  function captureTemplateLogicalScrollAnchor(virtual, scrollTop) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    if (!total) {
      return null;
    }
    rebuildTemplatePrefixOffsets(virtual);
    const maxOffset = Math.max(0, getTemplateOffsetAtIndex(virtual, total));
    const top = Math.max(0, Math.min(maxOffset, Number(scrollTop) || 0));
    const index = findTemplateIndexAtOffset(virtual, top);
    return {
      index,
      intra: Math.max(0, top - getTemplateOffsetAtIndex(virtual, index))
    };
  }

  function ensureTemplateRangeContainsLogicalAnchor(virtual, anchor) {
    if (!virtual || !anchor) {
      return;
    }
    const total = Math.max(0, Number(virtual.itemCount) || 0);
    const index = Math.max(0, Math.min(total - 1, Number(anchor.index) || 0));
    if (!total || (index >= virtual.start && index < virtual.end)) {
      return;
    }
    const metrics = getTemplateVirtualConfig(els.templatePreviewOutput, virtual.avgItemHeight);
    const requestedCount = Math.max(1, Number(virtual.end) - Number(virtual.start), metrics.targetCount);
    let start = Math.max(0, index - Math.floor(requestedCount / 2));
    let end = Math.min(total, start + requestedCount);
    if ((end - start) < requestedCount) {
      start = Math.max(0, end - requestedCount);
    }
    virtual.start = start;
    virtual.end = end;
  }

  function restoreTemplateLogicalScrollAnchor(virtual, anchor, fallbackTop) {
    if (!els.templatePreviewOutput) {
      return;
    }
    const rawIndex = anchor && Object.prototype.hasOwnProperty.call(anchor, "itemIndex")
      ? anchor.itemIndex
      : (anchor && anchor.index);
    const rawIntra = anchor && Object.prototype.hasOwnProperty.call(anchor, "intraItemOffset")
      ? anchor.intraItemOffset
      : (anchor && anchor.intra);
    let nextTop = Math.max(0, Number(fallbackTop) || 0);
    if (Number.isFinite(Number(rawIndex))) {
      const index = Math.max(0, Math.min(virtual.itemCount - 1, Number(rawIndex) || 0));
      const anchorStart = getTemplateOffsetAtIndex(virtual, index);
      const anchorEnd = getTemplateOffsetAtIndex(virtual, index + 1);
      const maxIntra = Math.max(0, anchorEnd - anchorStart - 1);
      nextTop = anchorStart + Math.min(maxIntra, Math.max(0, Number(rawIntra) || 0));
    }
    const maxTop = Math.max(
      0,
      Number(els.templatePreviewOutput.scrollHeight || 0) - Number(els.templatePreviewOutput.clientHeight || 0)
    );
    els.templatePreviewOutput.scrollTop = Math.max(0, Math.min(maxTop, nextTop));
  }

  function renderTemplateVirtualRangeReplace(options) {
    if (!els.templatePreviewOutput) {
      return;
    }

    const opts = options && typeof options === "object" ? options : {};
    const virtual = getTemplateVirtualState();
    const items = Array.isArray(virtual.items) ? virtual.items : [];
    const total = items.length;
    rebuildTemplatePrefixOffsets(virtual);
    const requestedTop = Number.isFinite(Number(opts.scrollTop))
      ? Number(opts.scrollTop)
      : (Number(els.templatePreviewOutput.scrollTop) || 0);
    const logicalAnchor = opts.preserveScroll && opts.preserveLogicalAnchor !== false
      ? captureTemplateLogicalScrollAnchor(virtual, requestedTop)
      : null;
    ensureTemplateRangeContainsLogicalAnchor(virtual, logicalAnchor);
    const start = Math.max(0, Math.min(total, Number(virtual.start) || 0));
    const end = Math.max(start, Math.min(total, Number(virtual.end) || 0));
    const config = virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : getDefaultTemplateConfig();
    virtual.isRenderTransaction = true;
    try {
      const frag = document.createDocumentFragment();
      const topSpacer = document.createElement("div");
      topSpacer.className = "virtual-spacer template-virtual-spacer-top";
      topSpacer.style.height = `${getTemplateOffsetAtIndex(virtual, start)}px`;
      topSpacer.setAttribute("aria-hidden", "true");
      frag.appendChild(topSpacer);

      for (let index = start; index < end; index += 1) {
        const block = buildTemplateBlockElement(items[index], index, config, true);
        if (!block) {
          continue;
        }
        frag.appendChild(block);
      }

      const bottomSpacer = document.createElement("div");
      bottomSpacer.className = "virtual-spacer template-virtual-spacer-bottom";
      bottomSpacer.style.height = `${Math.max(0, getTemplateOffsetAtIndex(virtual, total) - getTemplateOffsetAtIndex(virtual, end))}px`;
      bottomSpacer.setAttribute("aria-hidden", "true");
      frag.appendChild(bottomSpacer);

      els.templatePreviewOutput.classList.remove("muted");
      els.templatePreviewOutput.replaceChildren(frag);
      measureRenderedTemplateItems(virtual, start, end);

      restoreTemplateLogicalScrollAnchor(virtual, logicalAnchor, requestedTop);
      virtual.lastScrollTop = Number(els.templatePreviewOutput.scrollTop || 0) || 0;
    } finally {
      virtual.isRenderTransaction = false;
    }
    if (virtual.needsScrollSync) {
      scheduleTemplateVirtualScroll();
    }
  }

  function initTemplateVirtualWindow(items, config, options) {
    const opts = options && typeof options === "object" ? options : {};
    const virtual = getTemplateVirtualState();
    const list = Array.isArray(items) ? items : [];

    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    cancelTemplateVirtualAdjustment(virtual);
    virtual.needsScrollSync = false;
    virtual.isRenderTransaction = false;
    virtual.geometryEpoch = (Number(virtual.geometryEpoch) || 0) + 1;
    virtual.avgItemHeight = 140;
    virtual.unknownItemHeight = 140;
    virtual.estimateCalibrated = false;
    virtual.itemHeights = new Float64Array(0);
    virtual.prefixOffsets = new Float64Array(1);
    virtual.sourceRenderObjects = state.renderObjects;
    virtual.items = list;
    virtual.itemCount = list.length;
    if (typeof pruneTemplateBlockSelection === "function") {
      pruneTemplateBlockSelection(virtual.itemCount);
    }
    ensureTemplateHeightCache(virtual, virtual.itemCount);
    rebuildTemplatePrefixOffsets(virtual);
    virtual.lineTargetMap = buildTemplateLineTargetMap(list);
    virtual.start = 0;
    virtual.end = 0;
    virtual.lastScrollTop = 0;
    virtual.scrollDir = "down";
    virtual.isAdjustingScroll = false;
    virtual.isInitialized = list.length > 0;
    virtual.config = config && typeof config === "object" ? config : getDefaultTemplateConfig();

    if (!list.length) {
      return;
    }

    const metrics = getTemplateVirtualConfig(els.templatePreviewOutput, virtual.avgItemHeight);
    const total = list.length;
    let start = 0;
    let end = Math.min(total, metrics.targetCount);

    const selectedIndex = state.selectedTemplateIndex !== ""
      ? Number(state.selectedTemplateIndex)
      : Number.NaN;
    const hasSelectedIndex = Number.isFinite(selectedIndex) && selectedIndex >= 0 && selectedIndex < total;
    if (hasSelectedIndex) {
      start = Math.max(0, selectedIndex - Math.floor(metrics.targetCount / 2));
      end = Math.min(total, start + metrics.targetCount);
      if ((end - start) < metrics.targetCount) {
        start = Math.max(0, end - metrics.targetCount);
      }
    } else if (opts.preserveScroll === true) {
      const range = computeTemplateVirtualRangeFromScroll(Number(opts.scrollTop) || 0);
      start = range.start;
      end = range.end;
    }

    virtual.start = start;
    virtual.end = end;
    renderTemplateVirtualRangeReplace({
      preserveScroll: true,
      preserveLogicalAnchor: !hasSelectedIndex,
      scrollTop: opts.preserveScroll === true ? (Number(opts.scrollTop) || 0) : 0
    });
  }

  function ensureTemplateWindowContainsIndex(index) {
    const absIndex = Number(index);
    if (!Number.isFinite(absIndex) || absIndex < 0 || !els.templatePreviewOutput) {
      return false;
    }
    const virtual = getTemplateVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!virtual.isInitialized || !total || absIndex >= total) {
      return false;
    }
    if (absIndex >= virtual.start && absIndex < virtual.end) {
      return true;
    }

    const metrics = getTemplateVirtualConfig(els.templatePreviewOutput, virtual.avgItemHeight);
    let start = Math.max(0, absIndex - Math.floor(metrics.targetCount / 2));
    let end = Math.min(total, start + metrics.targetCount);
    if ((end - start) < metrics.targetCount) {
      start = Math.max(0, end - metrics.targetCount);
    }

    virtual.start = start;
    virtual.end = end;
    const targetTop = Math.max(0, getTemplateOffsetAtIndex(virtual, start));
    renderTemplateVirtualRangeReplace({ preserveScroll: false, scrollTop: targetTop });
    return true;
  }

  function processTemplateVirtualScrollFrame() {
    if (!els.templatePreviewOutput) {
      return;
    }
    const virtual = getTemplateVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!virtual.isInitialized || !total) {
      return;
    }
    if (virtual.isAdjustingScroll || virtual.isRenderTransaction) {
      virtual.needsScrollSync = true;
      return;
    }

    const currentTop = Number(els.templatePreviewOutput.scrollTop || 0) || 0;
    const prevTop = Number(virtual.lastScrollTop || 0) || 0;
    virtual.scrollDir = currentTop >= prevTop ? "down" : "up";
    virtual.lastScrollTop = currentTop;

    const range = computeTemplateVirtualRangeFromScroll(currentTop);
    if (range.start === virtual.start && range.end === virtual.end) {
      return;
    }

    virtual.start = range.start;
    virtual.end = range.end;
    renderTemplateVirtualRangeReplace({ preserveScroll: true, scrollTop: currentTop });
  }

  function scheduleTemplateVirtualScroll() {
    const virtual = getTemplateVirtualState();
    virtual.needsScrollSync = true;
    if (virtual.pendingRaf || virtual.isAdjustingScroll || virtual.isRenderTransaction) {
      return;
    }
    virtual.pendingRaf = requestAnimationFrame(() => {
      virtual.pendingRaf = 0;
      if (virtual.isAdjustingScroll || virtual.isRenderTransaction) {
        virtual.needsScrollSync = true;
        return;
      }
      virtual.needsScrollSync = false;
      processTemplateVirtualScrollFrame();
      if (virtual.needsScrollSync) {
        scheduleTemplateVirtualScroll();
      }
    });
  }

  function handleTemplateVirtualScroll() {
    scheduleTemplateVirtualScroll();
  }

  function handleTemplateVirtualUserIntent() {
    const virtual = getTemplateVirtualState();
    cancelTemplateVirtualAdjustment(virtual);
    virtual.needsScrollSync = true;
    scheduleTemplateVirtualScroll();
  }

  function resetTemplateVirtualState() {
    const virtual = getTemplateVirtualState();
    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    cancelTemplateVirtualAdjustment(virtual);
    virtual.items = [];
    virtual.itemCount = 0;
    virtual.start = 0;
    virtual.end = 0;
    virtual.lastScrollTop = 0;
    virtual.scrollDir = "down";
    virtual.isAdjustingScroll = false;
    virtual.avgItemHeight = 140;
    virtual.unknownItemHeight = 140;
    virtual.estimateCalibrated = false;
    virtual.needsScrollSync = false;
    virtual.isRenderTransaction = false;
    virtual.geometryEpoch = (Number(virtual.geometryEpoch) || 0) + 1;
    virtual.itemHeights = new Float64Array(0);
    virtual.prefixOffsets = new Float64Array(1);
    virtual.sourceRenderObjects = null;
    virtual.lineTargetMap = new Map();
    virtual.isInitialized = false;
  }

  function getTemplateAnchorNode(anchor) {
    if (!els.templatePreviewOutput || !anchor || typeof anchor !== "object") {
      return null;
    }
    const index = Number(anchor.templateIndex);
    if (!Number.isFinite(index) || index < 0) {
      return null;
    }
    if (anchor.kind === "cell") {
      const row = Number(anchor.gridRow);
      const col = Number(anchor.gridCol);
      return els.templatePreviewOutput.querySelector(
        `.template-block[data-template-index="${index}"] td.template-preview-editable[data-template-grid-row="${row}"][data-template-grid-col="${col}"]`
      );
    }
    return els.templatePreviewOutput.querySelector(`.template-block[data-template-index="${index}"]`);
  }

  function captureTemplateViewportAnchor(preferred) {
    if (!els.templatePreviewOutput) {
      return null;
    }
    const container = els.templatePreviewOutput;
    const containerRect = container.getBoundingClientRect();
    const base = {
      scrollTop: Number(container.scrollTop) || 0,
      viewportOffset: 0
    };
    const preferredIndex = Number(preferred && preferred.templateIndex);
    const preferredRow = Number(preferred && preferred.gridRow);
    const preferredCol = Number(preferred && preferred.gridCol);
    if (Number.isFinite(preferredIndex) && Number.isFinite(preferredRow) && Number.isFinite(preferredCol)) {
      const anchor = {
        ...base,
        kind: "cell",
        templateIndex: preferredIndex,
        gridRow: preferredRow,
        gridCol: preferredCol
      };
      const cell = getTemplateAnchorNode(anchor);
      if (cell) {
        anchor.viewportOffset = cell.getBoundingClientRect().top - containerRect.top;
        return anchor;
      }
    }

    const selectedBlock = state.selectedTemplateIndex !== ""
      ? container.querySelector(`.template-block[data-template-index="${String(state.selectedTemplateIndex)}"]`)
      : null;
    const renderedBlocks = Array.from(container.querySelectorAll(".template-block[data-template-index]"));
    const isVisible = (block) => {
      if (!block) {
        return false;
      }
      const rect = block.getBoundingClientRect();
      return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
    };
    const firstVisible = renderedBlocks.find((block) => {
      return isVisible(block);
    }) || null;
    const block = isVisible(selectedBlock) ? selectedBlock : firstVisible;
    if (!block) {
      const logical = captureTemplateLogicalScrollAnchor(getTemplateVirtualState(), base.scrollTop);
      if (logical) {
        return {
          ...base,
          kind: "logical",
          itemIndex: logical.index,
          intraItemOffset: logical.intra
        };
      }
      return { ...base, kind: "scroll" };
    }
    return {
      ...base,
      kind: "item",
      templateIndex: Number(block.getAttribute("data-template-index")),
      viewportOffset: block.getBoundingClientRect().top - containerRect.top
    };
  }

  function restoreTemplateViewportAnchor(anchor) {
    if (!els.templatePreviewOutput || !anchor || typeof anchor !== "object") {
      return;
    }
    const container = els.templatePreviewOutput;
    if (anchor.kind === "logical") {
      const virtual = getTemplateVirtualState();
      const itemIndex = Number(anchor.itemIndex);
      cancelTemplateVirtualAdjustment(virtual);
      if (
        Number.isFinite(itemIndex)
        && itemIndex >= 0
        && ensureTemplateWindowContainsIndex(itemIndex)
      ) {
        restoreTemplateLogicalScrollAnchor(virtual, anchor, anchor.scrollTop);
        virtual.lastScrollTop = Number(container.scrollTop) || 0;
        return;
      }
    }
    if (anchor.kind === "scroll") {
      const virtual = getTemplateVirtualState();
      cancelTemplateVirtualAdjustment(virtual);
      container.scrollTop = Math.max(0, Number(anchor.scrollTop) || 0);
      virtual.lastScrollTop = Number(container.scrollTop) || 0;
      return;
    }
    const index = Number(anchor.templateIndex);
    if (!Number.isFinite(index) || index < 0 || !ensureTemplateWindowContainsIndex(index)) {
      const virtual = getTemplateVirtualState();
      cancelTemplateVirtualAdjustment(virtual);
      container.scrollTop = Math.max(0, Number(anchor.scrollTop) || 0);
      virtual.lastScrollTop = Number(container.scrollTop) || 0;
      return;
    }

    const virtual = getTemplateVirtualState();
    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    const generation = beginTemplateVirtualAdjustment(virtual);
    const apply = () => {
      if (Number(virtual.adjustmentGeneration) !== generation || !virtual.isAdjustingScroll) {
        return false;
      }
      const node = getTemplateAnchorNode(anchor);
      if (!node) {
        return false;
      }
      const containerRect = container.getBoundingClientRect();
      const currentOffset = node.getBoundingClientRect().top - containerRect.top;
      const maxTop = Math.max(0, Number(container.scrollHeight || 0) - Number(container.clientHeight || 0));
      const layoutHeight = Math.max(0, Number(container.offsetHeight) || Number(container.clientHeight) || 0);
      const measuredScale = layoutHeight > 0 ? (Number(containerRect.height) || 0) / layoutHeight : 1;
      const scrollScale = Number.isFinite(measuredScale) && measuredScale >= 0.5 && measuredScale <= 2
        ? measuredScale
        : 1;
      const nextTop = (Number(container.scrollTop) || 0)
        + ((currentOffset - (Number(anchor.viewportOffset) || 0)) / scrollScale);
      container.scrollTop = Math.max(0, Math.min(maxTop, nextTop));
      return true;
    };
    if (!apply()) {
      container.scrollTop = Math.max(0, Number(anchor.scrollTop) || 0);
      virtual.lastScrollTop = Number(container.scrollTop) || 0;
      finishTemplateVirtualAdjustment(virtual, generation);
      return;
    }
    virtual.adjustmentRaf = requestAnimationFrame(() => {
      if (Number(virtual.adjustmentGeneration) !== generation || !virtual.isAdjustingScroll) {
        return;
      }
      apply();
      virtual.lastScrollTop = Number(container.scrollTop) || 0;
      finishTemplateVirtualAdjustment(virtual, generation);
    });
  }

  function renderTemplatePreview() {
    if (!els.templatePreviewOutput) {
      return;
    }

    const preferredAnchor = state.pendingTemplateViewportAnchor;
    const viewportAnchor = preferredAnchor && preferredAnchor.kind
      ? preferredAnchor
      : captureTemplateViewportAnchor(preferredAnchor);
    state.pendingTemplateViewportAnchor = null;

    if (!state.data || !Array.isArray(state.renderObjects)) {
      resetTemplateVirtualState();
      setTemplatePreviewMessage("No data loaded.");
      if (typeof refreshInputGutterTargets === "function") {
        refreshInputGutterTargets();
      }
      return;
    }

    const config = state.templateConfig && typeof state.templateConfig === "object"
      ? state.templateConfig
      : getDefaultTemplateConfig();

    const check = validateTemplateConfig(config);
    if (!check.valid) {
      resetTemplateVirtualState();
      setTemplatePreviewMessage("Template config is invalid.");
      setTemplateConfigError(check.errors.join("\n"));
      if (typeof refreshInputGutterTargets === "function") {
        refreshInputGutterTargets();
      }
      return;
    }

    const items = getRenderableObjectListForTemplate();
    if (!items.length) {
      resetTemplateVirtualState();
      setTemplatePreviewMessage("No renderable objects.");
      if (typeof refreshInputGutterTargets === "function") {
        refreshInputGutterTargets();
      }
      return;
    }

    const scrollTop = Number(els.templatePreviewOutput.scrollTop || 0) || 0;
    initTemplateVirtualWindow(items, config, { preserveScroll: true, scrollTop });
    restoreTemplateViewportAnchor(viewportAnchor);
    state.templatePreviewCache = { count: items.length };

    if (typeof syncRenderedTemplateSelection === "function") {
      syncRenderedTemplateSelection();
    }
    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
  }

  function isTemplateDynamicModalOpen() {
    return Boolean(activeTemplateDynamicModal && activeTemplateDynamicModal.root && activeTemplateDynamicModal.root.isConnected);
  }



  function closeTemplateDynamicModal() {
    if (!activeTemplateDynamicModal) {
      return;
    }
    const current = activeTemplateDynamicModal;
    activeTemplateDynamicModal = null;

    if (typeof current.cleanup === "function") {
      try {
        current.cleanup();
      } catch {
        // ignore
      }
    }

    if (current.root && current.root.parentNode) {
      current.root.remove();
    }

    if (current.restoreMainLayout) {
      setMainLayoutVisible(true);
    }
    if (current.restoreMainChrome) {
      setTemplateFormChromeHidden(false);
    }
  }



  function openTemplateDynamicModal(titleText, options) {
    closeTemplateDynamicModal();
    const opts = options && typeof options === "object" ? options : {};

    const root = document.createElement("div");
    root.className = "modal";

    const content = document.createElement("div");
    content.className = `modal-content ${opts.contentClass || ""}`.trim();
    root.appendChild(content);

    const header = document.createElement("div");
    header.className = "modal-header";
    content.appendChild(header);

    const title = document.createElement("strong");
    title.textContent = String(titleText || "Template");
    header.appendChild(title);

    const actions = document.createElement("div");
    actions.className = "modal-actions";
    header.appendChild(actions);

    const closeBtn = document.createElement("button");
    closeBtn.type = "button";
    closeBtn.className = "secondary";
    closeBtn.textContent = "Close";
    closeBtn.addEventListener("click", closeTemplateDynamicModal);
    actions.appendChild(closeBtn);

    const body = document.createElement("div");
    body.className = "modal-body";
    content.appendChild(body);

    root.addEventListener("click", (ev) => {
      if (ev.target === root) {
        closeTemplateDynamicModal();
      }
    });

    document.body.appendChild(root);
    activeTemplateDynamicModal = { root, cleanup: null };

    return {
      root,
      content,
      header,
      actions,
      body,
      closeBtn,
      setCleanup(cleanup) {
        if (activeTemplateDynamicModal && activeTemplateDynamicModal.root === root) {
          activeTemplateDynamicModal.cleanup = typeof cleanup === "function" ? cleanup : null;
        }
      }
    };
  }



  function setMainLayoutVisible(isVisible) {
    const layout = document.getElementById("mainLayout");
    if (!layout) {
      return;
    }
    layout.hidden = !isVisible;
  }



  function setTemplateFormChromeHidden(isHidden) {
    const hidden = Boolean(isHidden);
    const headerEl = document.getElementById("appHeader");
    const controlsEl = document.getElementById("appControls");
    if (headerEl) {
      headerEl.hidden = hidden;
    }
    if (controlsEl) {
      controlsEl.hidden = hidden;
    }
    const container = document.querySelector(".container");
    if (container) {
      container.classList.toggle("is-template-form-full", hidden);
    }
    document.body.classList.toggle("is-template-form-active", hidden);
  }



  function openTemplateDynamicPage(titleText, options) {
    closeTemplateDynamicModal();
    const opts = options && typeof options === "object" ? options : {};

    const root = document.createElement("section");
    root.className = "template-dynamic-page";

    const content = document.createElement("div");
    content.className = `template-dynamic-page-content ${opts.contentClass || ""}`.trim();
    root.appendChild(content);

    const header = document.createElement("div");
    header.className = "modal-header";
    content.appendChild(header);

    const title = document.createElement("strong");
    title.textContent = String(titleText || "Template");
    header.appendChild(title);

    const actions = document.createElement("div");
    actions.className = "modal-actions";
    header.appendChild(actions);

    const closeBtn = document.createElement("button");
    closeBtn.type = "button";
    closeBtn.className = "secondary";
    closeBtn.textContent = "Back";
    closeBtn.addEventListener("click", closeTemplateDynamicModal);
    actions.appendChild(closeBtn);

    const body = document.createElement("div");
    body.className = "modal-body";
    content.appendChild(body);

    const parent = els.mainLayout && els.mainLayout.parentNode
      ? els.mainLayout.parentNode
      : document.querySelector(".container");
    if (parent && els.mainLayout && els.mainLayout.nextSibling) {
      parent.insertBefore(root, els.mainLayout.nextSibling);
    } else if (parent) {
      parent.appendChild(root);
    } else {
      document.body.appendChild(root);
    }

    setMainLayoutVisible(false);
    setTemplateFormChromeHidden(true);
    activeTemplateDynamicModal = {
      root,
      cleanup: null,
      restoreMainLayout: true,
      restoreMainChrome: true
    };

    return {
      root,
      content,
      header,
      actions,
      body,
      closeBtn,
      setCleanup(cleanup) {
        if (activeTemplateDynamicModal && activeTemplateDynamicModal.root === root) {
          activeTemplateDynamicModal.cleanup = typeof cleanup === "function" ? cleanup : null;
        }
      }
    };
  }



  function normalizeTemplateObjectTypeToken(value) {
    return String(value || "").trim().toUpperCase();
  }



  function ensureTemplateGuiFilterState() {
    if (!(state.templateGuiHiddenTypes instanceof Set)) {
      state.templateGuiHiddenTypes = new Set();
    }
    if (!Array.isArray(state.templateGuiObjectTypes)) {
      state.templateGuiObjectTypes = [];
    }
  }



  function loadTemplateGuiFilterState() {
    ensureTemplateGuiFilterState();
    let parsed = [];
    try {
      parsed = JSON.parse(localStorage.getItem(TEMPLATE_GUI_FILTER_STORAGE_KEY_V1) || "[]");
    } catch {
      parsed = [];
    }

    if (!Array.isArray(parsed)) {
      parsed = [];
    }

    const normalized = new Set();
    for (const value of parsed) {
      const token = normalizeTemplateObjectTypeToken(value);
      if (token) {
        normalized.add(token);
      }
    }
    state.templateGuiHiddenTypes = normalized;
  }



  function saveTemplateGuiFilterState() {
    ensureTemplateGuiFilterState();
    try {
      localStorage.setItem(
        TEMPLATE_GUI_FILTER_STORAGE_KEY_V1,
        JSON.stringify(Array.from(state.templateGuiHiddenTypes.values()).sort((a, b) => a.localeCompare(b)))
      );
    } catch {
      // ignore
    }
  }



  function getTemplateFilterControls() {
    if (templateFilterModalControls && templateFilterModalControls.options) {
      return templateFilterModalControls;
    }
    return { panel: null, allBtn: null, noneBtn: null, options: null };
  }



  function collectTemplateObjectTypesFromTree() {
    const tokens = new Set();
    const roots = Array.isArray(state.renderObjects) ? state.renderObjects : [];

    const walk = (obj) => {
      if (!obj || typeof obj !== "object") {
        return;
      }
      const objectType = normalizeTemplateObjectTypeToken(obj.objectType);
      if (objectType) {
        tokens.add(objectType);
      }
      const children = Array.isArray(obj.children) ? obj.children : [];
      for (const child of children) {
        walk(child);
      }
    };

    for (const root of roots) {
      walk(root);
    }

    return Array.from(tokens).sort((a, b) => a.localeCompare(b));
  }



  function refreshTemplateGuiFilterTypes() {
    ensureTemplateGuiFilterState();
    state.templateGuiObjectTypes = collectTemplateObjectTypesFromTree();
    renderTemplateGuiFilterControls();
  }



  function isTemplateObjectTypeVisibleForGui(objectType) {
    ensureTemplateGuiFilterState();
    const token = normalizeTemplateObjectTypeToken(objectType);
    if (!token) {
      return true;
    }
    return !state.templateGuiHiddenTypes.has(token);
  }



  function resetTemplateSelectionStateMain() {
    if (typeof clearTemplateBlockSelection === "function") {
      clearTemplateBlockSelection();
      return;
    }
    state.selectedTemplateIndex = "";
    state.selectedTemplateIndexes = new Set();
    state.templateSelectionAnchorIndex = "";
    if (els.templateCopySelectedBtn) {
      els.templateCopySelectedBtn.textContent = "Copy Selected (0)";
      els.templateCopySelectedBtn.disabled = true;
    }
  }



  function rerenderTemplateForGuiFilterChange() {
    resetTemplateSelectionStateMain();
    if (state.data && Array.isArray(state.renderObjects) && typeof renderTemplatePreview === "function") {
      renderTemplatePreview();
      return;
    }
    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
  }



  function applyTemplateGuiFilterSelection(mode) {
    ensureTemplateGuiFilterState();
    const objectTypes = Array.isArray(state.templateGuiObjectTypes) ? state.templateGuiObjectTypes : [];
    if (mode === "all") {
      for (const type of objectTypes) {
        state.templateGuiHiddenTypes.delete(type);
      }
    } else if (mode === "none") {
      for (const type of objectTypes) {
        state.templateGuiHiddenTypes.add(type);
      }
    }
    saveTemplateGuiFilterState();
    renderTemplateGuiFilterControls();
    rerenderTemplateForGuiFilterChange();
  }



  function renderTemplateGuiFilterControls() {
    const controls = getTemplateFilterControls();
    if (!controls.options) {
      return;
    }
    ensureTemplateGuiFilterState();

    const objectTypes = Array.isArray(state.templateGuiObjectTypes) ? state.templateGuiObjectTypes : [];
    controls.options.replaceChildren();

    if (!objectTypes.length) {
      controls.options.appendChild(el("span", { className: "muted", text: "No object types loaded." }));
      return;
    }

    const frag = document.createDocumentFragment();
    for (const objectType of objectTypes) {
      const label = document.createElement("label");
      label.className = "toggle";

      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.value = objectType;
      checkbox.checked = !state.templateGuiHiddenTypes.has(objectType);
      checkbox.addEventListener("change", () => {
        if (checkbox.checked) {
          state.templateGuiHiddenTypes.delete(objectType);
        } else {
          state.templateGuiHiddenTypes.add(objectType);
        }
        saveTemplateGuiFilterState();
        rerenderTemplateForGuiFilterChange();
      });

      label.appendChild(checkbox);
      label.appendChild(document.createTextNode(objectType));
      frag.appendChild(label);
    }

    controls.options.appendChild(frag);
  }



  function initTemplateGuiFilterControls() {
    loadTemplateGuiFilterState();
  }



  function buildTemplateFilterPanelElement() {
    const panel = document.createElement("div");
    panel.className = "template-type-filter";

    const head = document.createElement("div");
    head.className = "template-type-filter-head";
    panel.appendChild(head);

    const title = document.createElement("span");
    title.className = "template-type-filter-title";
    title.textContent = "Template Object Filter (GUI only)";
    head.appendChild(title);

    const actions = document.createElement("div");
    actions.className = "template-type-filter-actions";
    head.appendChild(actions);

    const allBtn = document.createElement("button");
    allBtn.type = "button";
    allBtn.className = "secondary";
    allBtn.textContent = "All";
    actions.appendChild(allBtn);

    const noneBtn = document.createElement("button");
    noneBtn.type = "button";
    noneBtn.className = "secondary";
    noneBtn.textContent = "None";
    actions.appendChild(noneBtn);

    const options = document.createElement("div");
    options.className = "template-type-filter-options";
    panel.appendChild(options);

    templateFilterModalControls = { panel, allBtn, noneBtn, options };
    renderTemplateGuiFilterControls();

    allBtn.addEventListener("click", () => applyTemplateGuiFilterSelection("all"));
    noneBtn.addEventListener("click", () => applyTemplateGuiFilterSelection("none"));
    return panel;
  }



  function openTemplateFilterModal() {
    const modal = openTemplateDynamicModal("Template Object Filter", { contentClass: "template-runtime-modal-content" });
    const panel = buildTemplateFilterPanelElement();
    modal.body.appendChild(panel);
    modal.setCleanup(() => {
      templateFilterModalControls = null;
    });
  }



  function isViewerConfigPlainObject(value) {
    return Boolean(value && typeof value === "object" && !Array.isArray(value));
  }



  function cloneViewerConfigValue(value) {
    const cloned = cloneJsonValue(value);
    if (cloned === null && value !== null) {
      throw new Error("Viewer config contains a value that cannot be cloned.");
    }
    return cloned;
  }



  function normalizeTemplateFormEditorPct(value) {
    const numeric = Number(value);
    if (!Number.isFinite(numeric) || numeric < 22 || numeric > 78) {
      return 58;
    }
    return Math.round(numeric);
  }



  function loadTemplateFormEditorPct() {
    const current = Number(state.templateFormEditorPct);
    if (Number.isFinite(current) && current >= 22 && current <= 78) {
      return Math.round(current);
    }

    let saved = null;
    try {
      saved = localStorage.getItem(TEMPLATE_FORM_EDITOR_PCT_STORAGE_KEY_V1);
    } catch {
      saved = null;
    }
    const normalized = normalizeTemplateFormEditorPct(saved);
    state.templateFormEditorPct = normalized;
    return normalized;
  }



  function applyTemplateFormEditorPct(value, options) {
    const opts = options && typeof options === "object" ? options : {};
    const normalized = normalizeTemplateFormEditorPct(value);
    state.templateFormEditorPct = normalized;

    if (activeTemplateFormSplitController && typeof activeTemplateFormSplitController.apply === "function") {
      activeTemplateFormSplitController.apply(normalized);
    }

    if (opts.save === false) {
      return normalized;
    }
    localStorage.setItem(TEMPLATE_FORM_EDITOR_PCT_STORAGE_KEY_V1, String(normalized));
    return normalized;
  }



  function getSelectedViewerConfigSectionDefs(sectionKeys) {
    const requested = new Set(Array.isArray(sectionKeys) ? sectionKeys.map((key) => String(key || "")) : []);
    return VIEWER_CONFIG_SECTION_DEFS_V1.filter((section) => requested.has(section.key));
  }



  function canonicalizeDescriptionOverridesForViewerConfig(value, options) {
    const opts = options && typeof options === "object" ? options : {};
    if (!isViewerConfigPlainObject(value)) {
      throw new Error("Description overrides must be an object.");
    }

    const canonical = {};
    for (const [key, rawEntry] of Object.entries(value)) {
      if (!String(key || "").trim()) {
        throw new Error("Description override keys must not be empty.");
      }
      if (typeof rawEntry === "string") {
        canonical[key] = rawEntry;
        continue;
      }
      if (!isViewerConfigPlainObject(rawEntry)) {
        throw new Error(`Description override ${key} must be a string or canonical object.`);
      }
      if (opts.strict && typeof rawEntry.text !== "string") {
        throw new Error(`Description override ${key}.text must be a string.`);
      }
      if (
        opts.strict
        && Object.prototype.hasOwnProperty.call(rawEntry, "noNormalize")
        && typeof rawEntry.noNormalize !== "boolean"
      ) {
        throw new Error(`Description override ${key}.noNormalize must be a boolean.`);
      }
      const entry = normalizeDescOverrideEntry(rawEntry);
      canonical[key] = entry.noNormalize
        ? { text: entry.text, noNormalize: true }
        : entry.text;
    }
    return canonical;
  }



  function getViewerConfigSectionValue(sectionKey) {
    if (sectionKey === "templates") {
      return cloneViewerConfigValue(state.templateConfig || getDefaultTemplateConfig());
    }
    if (sectionKey === "descriptionSettings") {
      return cloneViewerConfigValue(normalizeSettings(state.settings || {}));
    }
    if (sectionKey === "descriptionOverrides") {
      return canonicalizeDescriptionOverridesForViewerConfig(state.descOverrides || {});
    }
    if (sectionKey === "appearance") {
      return {
        theme: normalizeTheme(state.theme),
        layoutLeftPane: normalizeLayoutSplit(state.layoutLeftPane)
      };
    }
    if (sectionKey === "templateUi") {
      ensureTemplateGuiFilterState();
      return {
        hiddenObjectTypes: Array.from(state.templateGuiHiddenTypes.values())
          .map(normalizeTemplateObjectTypeToken)
          .filter(Boolean)
          .sort((left, right) => left.localeCompare(right)),
        formEditorPct: loadTemplateFormEditorPct()
      };
    }
    throw new Error(`Unknown Viewer config section: ${sectionKey}`);
  }



  function buildViewerConfigBundle(sectionKeys, exportedAt) {
    const selectedSections = getSelectedViewerConfigSectionDefs(sectionKeys);
    const sections = {};
    for (const section of selectedSections) {
      sections[section.key] = getViewerConfigSectionValue(section.key);
    }
    return {
      kind: VIEWER_CONFIG_KIND_V1,
      version: VIEWER_CONFIG_VERSION_V1,
      exportedAt: exportedAt || new Date().toISOString(),
      sections
    };
  }



  function getViewerConfigExportFileName(sectionKeys) {
    const selectedSections = getSelectedViewerConfigSectionDefs(sectionKeys);
    if (selectedSections.length === VIEWER_CONFIG_SECTION_DEFS_V1.length) {
      return "abap-viewer-config.json";
    }
    const suffix = selectedSections.map((section) => section.fileToken).join("-");
    return suffix ? `abap-viewer-config-${suffix}.json` : "abap-viewer-config.json";
  }



  function downloadViewerConfigBundle(bundle, fileName) {
    const content = safeJson(bundle, true);
    const blob = new Blob([content], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = fileName;
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
    URL.revokeObjectURL(url);
  }



  function openViewerConfigExportModal() {
    const modal = openTemplateDynamicModal("Export Viewer Config", { contentClass: "template-runtime-modal-content" });
    const error = document.createElement("div");
    error.className = "template-error";
    modal.body.appendChild(error);

    const selectAllLabel = document.createElement("label");
    selectAllLabel.className = "toggle";
    const selectAllInput = document.createElement("input");
    selectAllInput.type = "checkbox";
    selectAllInput.checked = true;
    selectAllInput.setAttribute("data-config-select-all", "true");
    selectAllLabel.appendChild(selectAllInput);
    selectAllLabel.appendChild(document.createTextNode("Select all"));
    modal.body.appendChild(selectAllLabel);

    const sectionInputs = [];
    const sectionList = document.createElement("div");
    sectionList.className = "controls";
    for (const section of VIEWER_CONFIG_SECTION_DEFS_V1) {
      const label = document.createElement("label");
      label.className = "toggle";
      const input = document.createElement("input");
      input.type = "checkbox";
      input.checked = true;
      input.value = section.key;
      input.setAttribute("data-config-section", section.key);
      label.appendChild(input);
      label.appendChild(document.createTextNode(section.label));
      sectionList.appendChild(label);
      sectionInputs.push(input);
    }
    modal.body.appendChild(sectionList);

    const syncSelectAll = () => {
      selectAllInput.checked = sectionInputs.every((input) => input.checked);
      selectAllInput.indeterminate = !selectAllInput.checked && sectionInputs.some((input) => input.checked);
    };
    selectAllInput.addEventListener("change", () => {
      for (const input of sectionInputs) {
        input.checked = selectAllInput.checked;
      }
      syncSelectAll();
    });
    for (const input of sectionInputs) {
      input.addEventListener("change", syncSelectAll);
    }

    const exportButton = document.createElement("button");
    exportButton.type = "button";
    exportButton.className = "secondary";
    exportButton.textContent = "Export selected";
    exportButton.addEventListener("click", () => {
      const selectedKeys = sectionInputs.filter((input) => input.checked).map((input) => input.value);
      if (!selectedKeys.length) {
        error.textContent = "Select at least one config section.";
        return;
      }
      try {
        const bundle = buildViewerConfigBundle(selectedKeys);
        downloadViewerConfigBundle(bundle, getViewerConfigExportFileName(selectedKeys));
        setError("");
        closeTemplateDynamicModal();
      } catch (err) {
        error.textContent = `Export failed: ${err && err.message ? err.message : err}`;
      }
    });
    modal.actions.prepend(exportButton);
  }



  function prepareViewerConfigDescriptionSettings(value) {
    if (!isViewerConfigPlainObject(value)) {
      throw new Error("Description settings must be an object.");
    }
    if (
      Object.prototype.hasOwnProperty.call(value, "normalizeDeclDesc")
      && typeof value.normalizeDeclDesc !== "boolean"
    ) {
      throw new Error("Description settings normalizeDeclDesc must be a boolean.");
    }
    if (
      Object.prototype.hasOwnProperty.call(value, "declFilterTypes")
      && !Array.isArray(value.declFilterTypes)
    ) {
      throw new Error("Description settings declFilterTypes must be an array.");
    }
    if (
      Array.isArray(value.declFilterTypes)
      && value.declFilterTypes.some((type) => !DECL_TYPE_OPTIONS.includes(String(type || "").trim().toUpperCase()))
    ) {
      throw new Error("Description settings declFilterTypes contains an unsupported type.");
    }
    if (
      Object.prototype.hasOwnProperty.call(value, "structDescTemplate")
      && (typeof value.structDescTemplate !== "string" || !value.structDescTemplate.trim())
    ) {
      throw new Error("Description settings structDescTemplate must be a non-empty string.");
    }
    if (
      Object.prototype.hasOwnProperty.call(value, "nameTemplatesByCode")
      && !isViewerConfigPlainObject(value.nameTemplatesByCode)
    ) {
      throw new Error("Description settings nameTemplatesByCode must be an object.");
    }
    if (isViewerConfigPlainObject(value.nameTemplatesByCode)) {
      for (const [code, template] of Object.entries(value.nameTemplatesByCode)) {
        if (!NAME_CODE_OPTIONS.some((option) => option.code === code) || typeof template !== "string" || !template.trim()) {
          throw new Error(`Description settings nameTemplatesByCode.${code} is invalid.`);
        }
      }
    }
    return normalizeSettings(value);
  }



  function prepareViewerConfigAppearance(value) {
    if (!isViewerConfigPlainObject(value)) {
      throw new Error("Appearance must be an object.");
    }
    if (value.theme !== "light" && value.theme !== "dark") {
      throw new Error("Appearance theme must be light or dark.");
    }
    const layoutLeftPane = Number(value.layoutLeftPane);
    if (!Number.isFinite(layoutLeftPane) || layoutLeftPane < LAYOUT_SPLIT_MIN || layoutLeftPane > LAYOUT_SPLIT_MAX) {
      throw new Error(`Appearance layoutLeftPane must be between ${LAYOUT_SPLIT_MIN} and ${LAYOUT_SPLIT_MAX}.`);
    }
    return { theme: normalizeTheme(value.theme), layoutLeftPane: normalizeLayoutSplit(layoutLeftPane) };
  }



  function prepareViewerConfigTemplateUi(value) {
    if (!isViewerConfigPlainObject(value)) {
      throw new Error("Template UI must be an object.");
    }
    if (!Array.isArray(value.hiddenObjectTypes)) {
      throw new Error("Template UI hiddenObjectTypes must be an array.");
    }
    if (value.hiddenObjectTypes.some((item) => typeof item !== "string" || !item.trim())) {
      throw new Error("Template UI hiddenObjectTypes must contain non-empty strings.");
    }
    const formEditorPct = Number(value.formEditorPct);
    if (!Number.isFinite(formEditorPct) || formEditorPct < 22 || formEditorPct > 78) {
      throw new Error("Template UI formEditorPct must be between 22 and 78.");
    }
    return {
      hiddenObjectTypes: Array.from(new Set(value.hiddenObjectTypes.map(normalizeTemplateObjectTypeToken)))
        .filter(Boolean)
        .sort((left, right) => left.localeCompare(right)),
      formEditorPct: normalizeTemplateFormEditorPct(formEditorPct)
    };
  }



  function prepareViewerConfigTemplates(value) {
    if (!isViewerConfigPlainObject(value)) {
      throw new Error("Templates must be a config object.");
    }
    const next = cloneViewerConfigValue(value);
    normalizeTemplateConfigLegacyFieldsInPlace(next);
    const check = validateTemplateConfig(next);
    if (!check.valid) {
      throw new Error(check.errors.join("\n"));
    }
    mergeMissingDefaultTemplatesInPlace(next);
    return next;
  }



  function validateAndPrepareViewerConfigBundle(value) {
    const errors = [];
    if (!isViewerConfigPlainObject(value)) {
      return { valid: false, errors: ["Viewer config must be a JSON object."], knownSections: [], unknownSections: [], prepared: {} };
    }
    if (value.kind !== VIEWER_CONFIG_KIND_V1) {
      errors.push(`Viewer config kind must be ${VIEWER_CONFIG_KIND_V1}.`);
    }
    if (Number(value.version) !== VIEWER_CONFIG_VERSION_V1) {
      errors.push(`Viewer config version must be ${VIEWER_CONFIG_VERSION_V1}.`);
    }
    if (!isViewerConfigPlainObject(value.sections)) {
      errors.push("Viewer config sections must be an object.");
      return { valid: false, errors, knownSections: [], unknownSections: [], prepared: {} };
    }

    const knownKeys = new Set(VIEWER_CONFIG_SECTION_DEFS_V1.map((section) => section.key));
    const unknownSections = Object.keys(value.sections).filter((key) => !knownKeys.has(key));
    const knownSections = VIEWER_CONFIG_SECTION_DEFS_V1.filter((section) => (
      Object.prototype.hasOwnProperty.call(value.sections, section.key)
    ));
    if (!knownSections.length) {
      errors.push("Viewer config has no known config section.");
    }

    const prepared = {};
    for (const section of knownSections) {
      try {
        const raw = value.sections[section.key];
        if (section.key === "templates") {
          prepared.templates = prepareViewerConfigTemplates(raw);
        } else if (section.key === "descriptionSettings") {
          prepared.descriptionSettings = prepareViewerConfigDescriptionSettings(raw);
        } else if (section.key === "descriptionOverrides") {
          prepared.descriptionOverrides = canonicalizeDescriptionOverridesForViewerConfig(raw, { strict: true });
        } else if (section.key === "appearance") {
          prepared.appearance = prepareViewerConfigAppearance(raw);
        } else if (section.key === "templateUi") {
          prepared.templateUi = prepareViewerConfigTemplateUi(raw);
        }
      } catch (err) {
        errors.push(`${section.label}: ${err && err.message ? err.message : err}`);
      }
    }

    return {
      valid: errors.length === 0,
      errors,
      knownSections,
      unknownSections,
      prepared
    };
  }



  function getViewerConfigStorageSnapshot() {
    const keys = [
      TEMPLATE_CONFIG_STORAGE_KEY_V1,
      SETTINGS_STORAGE_KEY_V1,
      DESC_STORAGE_KEY_V2,
      THEME_STORAGE_KEY_V1,
      LAYOUT_SPLIT_STORAGE_KEY_V1,
      TEMPLATE_GUI_FILTER_STORAGE_KEY_V1,
      TEMPLATE_FORM_EDITOR_PCT_STORAGE_KEY_V1
    ];
    const snapshot = {};
    for (const key of keys) {
      snapshot[key] = localStorage.getItem(key);
    }
    return snapshot;
  }



  function restoreViewerConfigStorageSnapshot(snapshot) {
    for (const [key, raw] of Object.entries(snapshot || {})) {
      if (raw === null || raw === undefined) {
        localStorage.removeItem(key);
      } else {
        localStorage.setItem(key, raw);
      }
    }
  }



  function getViewerConfigStateSnapshot() {
    ensureTemplateGuiFilterState();
    return {
      templateConfig: cloneViewerConfigValue(state.templateConfig || getDefaultTemplateConfig()),
      settings: cloneViewerConfigValue(state.settings || loadSettings()),
      descOverrides: cloneViewerConfigValue(state.descOverrides || {}),
      theme: state.theme,
      layoutLeftPane: state.layoutLeftPane,
      hiddenObjectTypes: Array.from(state.templateGuiHiddenTypes.values()),
      formEditorPct: loadTemplateFormEditorPct()
    };
  }



  function rerenderViewerAfterConfigImport() {
    if (typeof renderSettingsModalUi === "function") {
      renderSettingsModalUi();
    }
    if (typeof renderTemplateGuiFilterControls === "function") {
      renderTemplateGuiFilterControls();
    }
    state.templatePreviewCache = null;
    renderActiveRightPanel();
  }



  function restoreViewerConfigStateSnapshot(snapshot) {
    state.templateConfig = cloneViewerConfigValue(snapshot.templateConfig);
    state.settings = cloneViewerConfigValue(snapshot.settings);
    state.descOverrides = cloneViewerConfigValue(snapshot.descOverrides);
    state.templateGuiHiddenTypes = new Set(snapshot.hiddenObjectTypes || []);
    applyTheme(snapshot.theme, { save: false });
    applyLayoutSplit(snapshot.layoutLeftPane, { save: false });
    applyTemplateFormEditorPct(snapshot.formEditorPct, { save: false });
    syncTemplateEditorFromState();
    rerenderViewerAfterConfigImport();
  }



  function writePreparedViewerConfigSections(prepared, knownSections) {
    const selectedKeys = new Set(knownSections.map((section) => section.key));
    if (selectedKeys.has("templates")) {
      localStorage.setItem(TEMPLATE_CONFIG_STORAGE_KEY_V1, JSON.stringify(prepared.templates));
    }
    if (selectedKeys.has("descriptionSettings")) {
      localStorage.setItem(SETTINGS_STORAGE_KEY_V1, JSON.stringify(prepared.descriptionSettings));
    }
    if (selectedKeys.has("descriptionOverrides")) {
      localStorage.setItem(DESC_STORAGE_KEY_V2, JSON.stringify(prepared.descriptionOverrides));
    }
    if (selectedKeys.has("appearance")) {
      localStorage.setItem(THEME_STORAGE_KEY_V1, prepared.appearance.theme);
      localStorage.setItem(LAYOUT_SPLIT_STORAGE_KEY_V1, String(prepared.appearance.layoutLeftPane));
    }
    if (selectedKeys.has("templateUi")) {
      localStorage.setItem(
        TEMPLATE_GUI_FILTER_STORAGE_KEY_V1,
        JSON.stringify(prepared.templateUi.hiddenObjectTypes)
      );
      localStorage.setItem(
        TEMPLATE_FORM_EDITOR_PCT_STORAGE_KEY_V1,
        String(prepared.templateUi.formEditorPct)
      );
    }
  }



  function applyPreparedViewerConfigSections(prepared, knownSections) {
    const selectedKeys = new Set(knownSections.map((section) => section.key));
    if (selectedKeys.has("templates")) {
      const applied = applyTemplateConfigObject(prepared.templates, { save: false });
      if (!applied) {
        throw new Error("Templates could not be applied.");
      }
    }
    if (selectedKeys.has("descriptionSettings")) {
      state.settings = cloneViewerConfigValue(prepared.descriptionSettings);
    }
    if (selectedKeys.has("descriptionOverrides")) {
      state.descOverrides = cloneViewerConfigValue(prepared.descriptionOverrides);
    }
    if (selectedKeys.has("appearance")) {
      applyTheme(prepared.appearance.theme, { save: false });
      applyLayoutSplit(prepared.appearance.layoutLeftPane, { save: false });
    }
    if (selectedKeys.has("templateUi")) {
      state.templateGuiHiddenTypes = new Set(prepared.templateUi.hiddenObjectTypes);
      applyTemplateFormEditorPct(prepared.templateUi.formEditorPct, { save: false });
    }
    syncTemplateEditorFromState();
    rerenderViewerAfterConfigImport();
  }



  function importViewerConfigObject(value) {
    const validation = validateAndPrepareViewerConfigBundle(value);
    if (!validation.valid) {
      setTemplateConfigError(`Import failed: ${validation.errors.join("\n")}`);
      return false;
    }

    const confirmationLines = validation.knownSections.map((section) => `- ${section.label}`);
    const confirmed = typeof window.confirm !== "function"
      || window.confirm(`Import these Viewer config sections?\n${confirmationLines.join("\n")}`);
    if (!confirmed) {
      setTemplateConfigError("Import cancelled.");
      return false;
    }

    let storageSnapshot = null;
    let stateSnapshot = null;
    try {
      storageSnapshot = getViewerConfigStorageSnapshot();
      stateSnapshot = getViewerConfigStateSnapshot();
      writePreparedViewerConfigSections(validation.prepared, validation.knownSections);
      applyPreparedViewerConfigSections(validation.prepared, validation.knownSections);
    } catch (err) {
      let rollbackError = "";
      try {
        if (storageSnapshot) {
          restoreViewerConfigStorageSnapshot(storageSnapshot);
        }
        if (stateSnapshot) {
          restoreViewerConfigStateSnapshot(stateSnapshot);
        }
      } catch (restoreErr) {
        rollbackError = ` Rollback also failed: ${restoreErr && restoreErr.message ? restoreErr.message : restoreErr}`;
      }
      setTemplateConfigError(
        `Import failed and was rolled back: ${err && err.message ? err.message : err}.${rollbackError}`
      );
      return false;
    }

    if (validation.unknownSections.length) {
      setTemplateConfigError(`Import warning: ignored unknown sections: ${validation.unknownSections.join(", ")}.`);
    } else {
      setTemplateConfigError("");
    }
    setError("");
    return true;
  }



  function isLegacyTemplateConfig(value) {
    return Boolean(
      isViewerConfigPlainObject(value)
      && !Object.prototype.hasOwnProperty.call(value, "kind")
      && Object.prototype.hasOwnProperty.call(value, "version")
      && Object.prototype.hasOwnProperty.call(value, "templates")
    );
  }



  async function importViewerConfigFromFile(file) {
    if (!file) {
      return;
    }

    let text = "";
    try {
      text = await file.text();
    } catch (err) {
      setTemplateConfigError(`Import failed: ${err && err.message ? err.message : err}`);
      return;
    }

    try {
      const parsed = JSON.parse(text);
      if (isLegacyTemplateConfig(parsed)) {
        const applied = applyTemplateConfigObject(parsed, { save: true });
        if (applied) {
          setError("");
        }
        return;
      }
      importViewerConfigObject(parsed);
    } catch (err) {
      setTemplateConfigError(`Import JSON parse error: ${err && err.message ? err.message : err}`);
    }
  }



  function getRenderableObjectListForTemplate(options) {
    const opts = options && typeof options === "object" ? options : {};
    const includeHidden = opts.includeHidden === true;
    const out = [];
    const roots = Array.isArray(state.renderObjects) ? state.renderObjects : [];

    const appendNode = (obj, depth) => {
      if (!obj || typeof obj !== "object") {
        return;
      }

      if (includeHidden || isTemplateObjectTypeVisibleForGui(obj.objectType)) {
        out.push({ obj, depth: Math.max(0, Number(depth) || 0) });
      }

      const children = Array.isArray(obj.children) ? obj.children : [];
      for (const child of children) {
        appendNode(child, (Number(depth) || 0) + 1);
      }
    };

    for (const root of roots) {
      appendNode(root, 0);
    }

    return out;
  }



  function getTemplateCopyItemsAndConfig() {
    const virtual = typeof getTemplateVirtualState === "function" ? getTemplateVirtualState() : null;
    const items = virtual && Array.isArray(virtual.items) && virtual.items.length
      ? virtual.items
      : getRenderableObjectListForTemplate();
    const config = virtual && virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : (state.templateConfig && typeof state.templateConfig === "object"
        ? state.templateConfig
        : getDefaultTemplateConfig());
    return { items, config };
  }



  async function copyTemplateBlocksByIndexes(indexes) {
    const { items, config } = getTemplateCopyItemsAndConfig();
    if (!items.length) {
      setError("Nothing to copy.");
      return;
    }
    const tableOnly = typeof isTemplateCopyTableOnlyEnabled === "function"
      ? isTemplateCopyTableOnlyEnabled()
      : Boolean(els.templateCopyTableOnly && els.templateCopyTableOnly.checked);
    if (typeof buildTemplateCollectionCopyPayload !== "function") {
      throw new Error("Template clipboard builder is unavailable.");
    }
    const payload = buildTemplateCollectionCopyPayload(items, indexes, config, tableOnly);
    if (!payload || !payload.node) {
      setError("Nothing to copy.");
      return;
    }
    await copyHtmlWithFallback(payload.html, payload.text);
  }



  async function copySelectedTemplateBlocks() {
    const indexes = typeof getSortedSelectedTemplateIndexes === "function"
      ? getSortedSelectedTemplateIndexes().map(Number)
      : Array.from(state.selectedTemplateIndexes instanceof Set ? state.selectedTemplateIndexes : []).map(Number);
    if (!indexes.length) {
      setError("Select at least one template block first.");
      return;
    }
    await copyTemplateBlocksByIndexes(indexes);
  }



  function writeTemplateConfigDraftToTextarea(textarea) {
    if (!textarea) {
      return;
    }
    if (typeof syncTemplateEditorFromState === "function") {
      syncTemplateEditorFromState();
      return;
    }
    try {
      textarea.value = JSON.stringify(state.templateConfig || getDefaultTemplateConfig(), null, 2);
    } catch {
      textarea.value = "";
    }
  }



  function findRenderObjectById(id) {
    const targetId = String(id === undefined || id === null ? "" : id).trim();
    if (!targetId || !Array.isArray(state.renderObjects)) {
      return null;
    }
    const stack = state.renderObjects.slice();
    while (stack.length) {
      const current = stack.pop();
      if (!current || typeof current !== "object") {
        continue;
      }
      if (String(current.id === undefined || current.id === null ? "" : current.id) === targetId) {
        return current;
      }
      const children = Array.isArray(current.children) ? current.children : [];
      for (let index = children.length - 1; index >= 0; index -= 1) {
        stack.push(children[index]);
      }
    }
    return null;
  }



  function findTemplateObjectByIndex(index) {
    const absIndex = Number(index);
    if (!Number.isFinite(absIndex) || absIndex < 0) {
      return null;
    }
    if (typeof getTemplateVirtualState === "function") {
      const virtual = getTemplateVirtualState();
      const items = virtual && Array.isArray(virtual.items) ? virtual.items : [];
      if (absIndex < items.length) {
        const item = items[absIndex];
        if (item && typeof item === "object") {
          if (item.obj && typeof item.obj === "object") {
            return item.obj;
          }
          if (item.nodeInfo && item.nodeInfo.obj && typeof item.nodeInfo.obj === "object") {
            return item.nodeInfo.obj;
          }
        }
      }
    }
    return null;
  }



  function interceptTemplateCodeButtonClick(ev) {
    if (!els.templatePreviewOutput || !ev || !ev.target || !(ev.target instanceof Element)) {
      return;
    }
    const btn = ev.target.closest("button[data-template-action=\"code\"]");
    if (!btn || !els.templatePreviewOutput.contains(btn)) {
      return;
    }
    const block = btn.closest(".template-block");
    if (!block) {
      return;
    }
    const absIndex = Number(block.getAttribute("data-template-index"));
    const obj = findTemplateObjectByIndex(absIndex);
    const lineStart = Number(obj && obj.lineStart) || Number(block.getAttribute("data-line-start")) || 0;
    const lineEnd = Number(obj && obj.block && obj.block.lineEnd) || lineStart;
    const segmentIndex = Number.isFinite(Number(obj && obj.segmentIndex))
      ? Math.max(0, Math.floor(Number(obj.segmentIndex)))
      : null;
    if (lineStart <= 0) {
      return;
    }
    ev.preventDefault();
    ev.stopPropagation();
    if (typeof ev.stopImmediatePropagation === "function") {
      ev.stopImmediatePropagation();
    }
    jumpInputToCodeRange(lineStart, lineEnd, segmentIndex);
  }



  function normalizeTemplateConfigLegacyFieldsInPlace(config) {
    if (!config || typeof config !== "object" || Array.isArray(config)) {
      return false;
    }

    const normalizeColor = (value) => {
      const raw = String(value === undefined || value === null ? "" : value).trim();
      if (!raw) {
        return "";
      }
      if (typeof normalizeTemplateColorValue === "function") {
        return normalizeTemplateColorValue(raw);
      }
      if (/^#(?:[0-9a-f]{3}|[0-9a-f]{6})$/i.test(raw)) {
        return raw;
      }
      const lower = raw.toLowerCase();
      if (lower === "mau xanh nhat") {
        return "#dbeef4";
      }
      if (lower === "den") {
        return "#000000";
      }
      return raw;
    };

    const normalizeBorder = (value) => {
      const raw = String(value === undefined || value === null ? "" : value).trim();
      if (!raw) {
        return "";
      }
      if (typeof normalizeTemplateBorderValue === "function") {
        return normalizeTemplateBorderValue(raw);
      }
      if (raw.toLowerCase() === "outside line mong") {
        return "outside-thin";
      }
      return raw;
    };

    const normalizeAlign = (value) => {
      const raw = String(value === undefined || value === null ? "" : value).trim();
      if (!raw) {
        return "";
      }
      if (typeof normalizeTemplateAlignValue === "function") {
        return normalizeTemplateAlignValue(raw);
      }
      const lower = raw.toLowerCase();
      if (lower === "left" || lower === "center" || lower === "right") {
        return lower;
      }
      return "";
    };

    const normalizeVAlign = (value) => {
      const raw = String(value === undefined || value === null ? "" : value).trim();
      if (!raw) {
        return "";
      }
      if (typeof normalizeTemplateVAlignValue === "function") {
        return normalizeTemplateVAlignValue(raw);
      }
      const lower = raw.toLowerCase();
      if (lower === "top") {
        return "top";
      }
      if (lower === "middle" || lower === "center") {
        return "middle";
      }
      if (lower === "bottom") {
        return "bottom";
      }
      return "";
    };

    let changed = false;
    const walk = (node) => {
      if (!node || typeof node !== "object") {
        return;
      }
      if (Array.isArray(node)) {
        for (const item of node) {
          walk(item);
        }
        return;
      }

      for (const key of Object.keys(node)) {
        const value = node[key];
        const keyLower = String(key || "").trim().toLowerCase();
        if (keyLower === "background" || keyLower === "font color") {
          const normalized = normalizeColor(value);
          if (normalized !== value) {
            changed = true;
          }
          if (normalized) {
            node[key] = normalized;
          } else if (Object.prototype.hasOwnProperty.call(node, key)) {
            delete node[key];
            changed = true;
          }
          continue;
        }
        if (keyLower === "border") {
          const normalized = normalizeBorder(value);
          if (normalized !== value) {
            changed = true;
          }
          if (normalized) {
            node[key] = normalized;
          } else if (Object.prototype.hasOwnProperty.call(node, key)) {
            delete node[key];
            changed = true;
          }
          continue;
        }
        if (keyLower === "align") {
          const normalized = normalizeAlign(value);
          if (normalized !== value) {
            changed = true;
          }
          if (normalized) {
            node[key] = normalized;
          } else if (Object.prototype.hasOwnProperty.call(node, key)) {
            delete node[key];
            changed = true;
          }
          continue;
        }
        if (keyLower === "valign") {
          const normalized = normalizeVAlign(value);
          if (normalized !== value) {
            changed = true;
          }
          if (normalized) {
            node[key] = normalized;
          } else if (Object.prototype.hasOwnProperty.call(node, key)) {
            delete node[key];
            changed = true;
          }
          continue;
        }
        walk(value);
      }
    };

    walk(config);
    return changed;
  }



  function openTemplateConfigModal() {
    const modal = openTemplateDynamicPage("Template Form", { contentClass: "template-runtime-modal-content template-runtime-modal-wide" });
    modal.body.classList.add("template-config-modal-body");
    const prevJsonEl = els.templateConfigJson;
    const prevErrEl = els.templateConfigError;
    const PREVIEW_LIMIT = 4;
    let previewTimer = 0;
    const applyBtn = document.createElement("button");
    applyBtn.type = "button";
    applyBtn.className = "secondary";
    applyBtn.textContent = "Apply";
    modal.actions.prepend(applyBtn);

    const workspace = document.createElement("div");
    workspace.className = "template-config-workspace";
    modal.body.appendChild(workspace);
    const editorPane = document.createElement("div");
    editorPane.className = "template-config-editor-pane";
    const workspaceSplit = document.createElement("div");
    workspaceSplit.className = "template-config-workspace-split";
    workspaceSplit.setAttribute("role", "separator");
    workspaceSplit.setAttribute("tabindex", "0");
    workspaceSplit.setAttribute("aria-label", "Resize editor and live preview");

    const host = document.createElement("div");
    host.className = "template-config-editor-host";
    const excelStatusEl = document.createElement("div");
    excelStatusEl.className = "template-excel-status";
    excelStatusEl.hidden = true;
    const errEl = document.createElement("div");
    errEl.className = "template-error";
    editorPane.appendChild(host);
    editorPane.appendChild(excelStatusEl);
    editorPane.appendChild(errEl);

    const previewPane = document.createElement("section");
    previewPane.className = "template-config-live-preview";
    const previewHead = document.createElement("div");
    previewHead.className = "template-config-live-preview-head";
    const previewTitle = document.createElement("div");
    previewTitle.className = "template-config-live-preview-title";
    previewTitle.textContent = "Live preview";
    const previewMeta = document.createElement("div");
    previewMeta.className = "template-config-live-preview-meta";
    const previewNote = document.createElement("div");
    previewNote.className = "template-config-live-preview-note";
    previewNote.textContent = `Auto-update while editing • showing up to ${PREVIEW_LIMIT} matching objects`;
    previewHead.appendChild(previewTitle);
    previewHead.appendChild(previewMeta);
    previewHead.appendChild(previewNote);
    previewPane.appendChild(previewHead);
    const previewBody = document.createElement("div");
    previewBody.className = "template-config-live-preview-body muted";
    previewBody.textContent = "Preview unavailable.";
    previewPane.appendChild(previewBody);
    workspace.appendChild(editorPane);
    workspace.appendChild(workspaceSplit);
    workspace.appendChild(previewPane);

    let editorPct = loadTemplateFormEditorPct();
    const templateFormMaxWidthMq = window.matchMedia("(max-width: 1180px)");
    const applyTemplateWorkspaceSplit = () => {
      const stacked = templateFormMaxWidthMq.matches;
      workspaceSplit.setAttribute("aria-orientation", stacked ? "horizontal" : "vertical");
      editorPane.style.flex = `0 0 ${editorPct}%`;
      previewPane.style.flex = "1 1 0";
      editorPane.style.minWidth = stacked ? "0" : "220px";
      previewPane.style.minWidth = stacked ? "0" : "220px";
      editorPane.style.minHeight = stacked ? "120px" : "0";
      previewPane.style.minHeight = stacked ? "160px" : "0";
    };
    activeTemplateFormSplitController = {
      apply(nextPercent) {
        editorPct = normalizeTemplateFormEditorPct(nextPercent);
        applyTemplateWorkspaceSplit();
      }
    };
    applyTemplateWorkspaceSplit();
    const onTemplateFormSplitChanged = () => {
      applyTemplateWorkspaceSplit();
    };
    if (typeof templateFormMaxWidthMq.addEventListener === "function") {
      templateFormMaxWidthMq.addEventListener("change", onTemplateFormSplitChanged);
    } else if (typeof templateFormMaxWidthMq.addListener === "function") {
      templateFormMaxWidthMq.addListener(onTemplateFormSplitChanged);
    }
    let templateSplitDrag = null;
    const endTemplateSplitDrag = (ev) => {
      if (!templateSplitDrag) {
        return;
      }
      if (ev && templateSplitDrag.pointerId != null && ev.pointerId !== templateSplitDrag.pointerId) {
        return;
      }
      const pid = templateSplitDrag.pointerId;
      templateSplitDrag = null;
      workspace.classList.remove("is-resizing");
      try {
        applyTemplateFormEditorPct(editorPct);
      } catch {
        // ignore
      }
      if (typeof workspaceSplit.releasePointerCapture === "function" && pid != null) {
        try {
          workspaceSplit.releasePointerCapture(pid);
        } catch {
          // ignore
        }
      }
    };
    workspaceSplit.addEventListener("pointerdown", (ev) => {
      ev.preventDefault();
      templateSplitDrag = { pointerId: ev.pointerId };
      workspace.classList.add("is-resizing");
      if (typeof workspaceSplit.setPointerCapture === "function") {
        try {
          workspaceSplit.setPointerCapture(ev.pointerId);
        } catch {
          // ignore
        }
      }
    });
    workspaceSplit.addEventListener("pointermove", (ev) => {
      if (!templateSplitDrag || ev.pointerId !== templateSplitDrag.pointerId) {
        return;
      }
      const rect = workspace.getBoundingClientRect();
      const w = Math.max(1, rect.width);
      const h = Math.max(1, rect.height);
      const stacked = templateFormMaxWidthMq.matches;
      const ratio = stacked
        ? (ev.clientY - rect.top) / h
        : (ev.clientX - rect.left) / w;
      editorPct = Math.round(Math.min(78, Math.max(22, ratio * 100)));
      state.templateFormEditorPct = editorPct;
      applyTemplateWorkspaceSplit();
    });
    workspaceSplit.addEventListener("pointerup", endTemplateSplitDrag);
    workspaceSplit.addEventListener("pointercancel", endTemplateSplitDrag);
    workspaceSplit.addEventListener("keydown", (ev) => {
      const step = ev.shiftKey ? 5 : 2;
      const stacked = templateFormMaxWidthMq.matches;
      if (!stacked && (ev.key === "ArrowLeft" || ev.key === "ArrowRight")) {
        ev.preventDefault();
        editorPct = ev.key === "ArrowLeft"
          ? Math.max(22, editorPct - step)
          : Math.min(78, editorPct + step);
        state.templateFormEditorPct = editorPct;
        applyTemplateWorkspaceSplit();
        return;
      }
      if (stacked && (ev.key === "ArrowUp" || ev.key === "ArrowDown")) {
        ev.preventDefault();
        editorPct = ev.key === "ArrowUp"
          ? Math.max(22, editorPct - step)
          : Math.min(78, editorPct + step);
        state.templateFormEditorPct = editorPct;
        applyTemplateWorkspaceSplit();
      }
    });

    const OPTION_KEYS = new Set(["_options", "options", "ranges", "compact", "hideemptyrows", "hiderowswithoutvalues", "expandmultilinerows", "removeemptyrows", "removeemptyrowsadvanced", "removeemptyrowsadv", "expandarrayrows", "arraytorows"]);
    const FIELD_DEFS = [
      { key: "text", label: "Text", kind: "text" },
      { key: "background", label: "Background", kind: "color", fallback: "#ffffff" },
      { key: "border", label: "Border", kind: "suggest", opts: ["outside-thin"] },
      { key: "font", label: "Font", kind: "text" },
      { key: "font size", label: "Font Size", kind: "number" },
      { key: "font color", label: "Font Color", kind: "color", fallback: "#111111" },
      { key: "align", label: "Align", kind: "select", opts: ["", "left", "center", "right"] },
      { key: "valign", label: "VAlign", kind: "select", opts: ["", "top", "middle", "bottom"] },
      { key: "wrap", label: "Wrap", kind: "bool" },
      { key: "merge", label: "Merge", kind: "bool" },
      { key: "bold", label: "Bold", kind: "bool" },
      { key: "italic", label: "Italic", kind: "bool" },
      { key: "underline", label: "Underline", kind: "bool" }
    ];
    const isHexColorValue = (value) => /^#(?:[0-9a-f]{3}|[0-9a-f]{6})$/i.test(String(value || "").trim());
    const expandHexColorValue = (value) => {
      const raw = String(value || "").trim().toLowerCase();
      if (!/^#(?:[0-9a-f]{3}|[0-9a-f]{6})$/i.test(raw)) {
        return "";
      }
      if (raw.length === 4) {
        return "#" + raw.slice(1).split("").map((ch) => ch + ch).join("");
      }
      return raw;
    };
    const getColorPickerValue = (value, fallback) => {
      const expanded = expandHexColorValue(value);
      if (expanded) {
        return expanded;
      }
      return expandHexColorValue(fallback) || "#000000";
    };
    const buildTemplateConfigDomId = (...parts) => parts
      .map((part) => String(part || "").trim().replace(/[^a-z0-9_-]+/gi, "-").replace(/^-+|-+$/g, "").toLowerCase())
      .filter(Boolean)
      .join("-");
    const showErr = (m) => {
      const t = String(m || "").trim();
      errEl.textContent = t;
      if (typeof setTemplateConfigError === "function") {
        setTemplateConfigError(t);
      }
    };
    const showExcelStatus = (message, tone) => {
      const text = String(message || "").trim();
      excelStatusEl.textContent = text;
      excelStatusEl.hidden = !text;
      excelStatusEl.classList.toggle("is-success", tone === "success");
      excelStatusEl.classList.toggle("is-warning", tone === "warning");
    };
    const isOptionKey = (k) => (typeof isTemplateOptionConfigKey === "function")
      ? Boolean(isTemplateOptionConfigKey(k))
      : OPTION_KEYS.has(String(k || "").trim().toLowerCase());
    const base = state.templateConfig && typeof state.templateConfig === "object" ? state.templateConfig : getDefaultTemplateConfig();
    let draft = cloneJsonValue(base);
    if (!draft || typeof draft !== "object" || Array.isArray(draft)) {
      draft = getDefaultTemplateConfig();
    }
    draft.version = 1;
    if (!draft.templates || typeof draft.templates !== "object" || Array.isArray(draft.templates)) {
      draft.templates = {};
    }
    if (!Object.keys(draft.templates).length) {
      draft = getDefaultTemplateConfig();
    }
    let selKey = Object.keys(draft.templates)[0] || "";
    let selRange = "";
    let pendingExcelImport = null;
    let excelPasteOpen = false;
    els.templateConfigError = errEl;
    els.templateConfigJson = null;
    showErr("");

    const clearPreviewTimer = () => {
      if (!previewTimer) {
        return;
      }
      clearTimeout(previewTimer);
      previewTimer = 0;
    };
    const setPreviewState = (metaText, noteText, message, warning) => {
      previewTitle.textContent = "Live preview";
      previewMeta.textContent = String(metaText || "").trim();
      previewNote.textContent = String(noteText || "").trim();
      previewNote.classList.toggle("is-warning", warning === true);
      previewBody.classList.add("muted");
      previewBody.replaceChildren();
      previewBody.textContent = String(message || "").trim() || "Preview unavailable.";
    };
    const buildPreviewConfig = () => {
      const preCheck = validateDraft();
      if (!preCheck.ok) {
        return { config: null, message: preCheck.messages[0] || "Template draft is invalid." };
      }
      const nextConfig = serializeDraft();
      const chk = validateTemplateConfig(nextConfig);
      if (!chk.valid) {
        return { config: null, message: chk.errors[0] || "Template config is invalid." };
      }
      return { config: nextConfig, message: "" };
    };
    const renderLivePreview = () => {
      const activeKey = String(selKey || "").trim();
      if (!state.data || !Array.isArray(state.renderObjects) || !state.renderObjects.length) {
        setPreviewState(
          activeKey ? `Key: ${activeKey}` : "",
          "Render ABAP first to unlock live preview.",
          "No parsed data loaded.",
          false
        );
        return;
      }
      if (typeof getRenderableObjectListForTemplate !== "function" || typeof resolveTemplateMapForObject !== "function" || typeof buildTemplateBlockElement !== "function") {
        setPreviewState(
          activeKey ? `Key: ${activeKey}` : "",
          "Preview helpers are unavailable in this runtime.",
          "Live preview is unavailable.",
          true
        );
        return;
      }
      const previewConfig = buildPreviewConfig();
      if (!previewConfig.config) {
        setPreviewState(
          activeKey ? `Key: ${activeKey}` : "",
          "Fix validation issues to resume preview.",
          previewConfig.message || "Template config is invalid.",
          true
        );
        return;
      }
      const items = getRenderableObjectListForTemplate({ includeHidden: true });
      if (!Array.isArray(items) || !items.length) {
        setPreviewState(
          activeKey ? `Key: ${activeKey}` : "",
          "There are no renderable objects right now.",
          "Nothing to preview.",
          false
        );
        return;
      }
      const matches = [];
      for (let index = 0; index < items.length; index += 1) {
        const item = items[index];
        const obj = item && typeof item === "object" ? item.obj : null;
        if (!obj) {
          continue;
        }
        const resolved = resolveTemplateMapForObject(obj, previewConfig.config);
        if (String((resolved && resolved.key) || "") === activeKey) {
          matches.push({ item, index });
        }
      }
      if (!matches.length) {
        setPreviewState(
          activeKey ? `Key: ${activeKey}` : "No template key selected",
          "Preview shows the objects that currently resolve to the selected key.",
          activeKey
            ? `No objects currently resolve to template key "${activeKey}".`
            : "Select a template key to preview.",
          false
        );
        return;
      }
      const fragment = document.createDocumentFragment();
      let renderedCount = 0;
      for (const match of matches.slice(0, PREVIEW_LIMIT)) {
        const block = buildTemplateBlockElement(match.item, match.index, previewConfig.config, false);
        if (!block) {
          continue;
        }
        fragment.appendChild(block);
        renderedCount += 1;
      }
      if (!renderedCount) {
        setPreviewState(
          activeKey ? `Key: ${activeKey}` : "",
          "The selected template key has matches, but preview blocks failed to render.",
          "Preview rendering failed.",
          true
        );
        return;
      }
      previewTitle.textContent = "Live preview";
      previewMeta.textContent = activeKey
        ? `Key: ${activeKey} • showing ${renderedCount}/${matches.length} matching objects`
        : `Showing ${renderedCount}/${matches.length} objects`;
      previewNote.textContent = "Preview refreshes while you edit.";
      previewNote.classList.remove("is-warning");
      previewBody.classList.remove("muted");
      previewBody.replaceChildren(fragment);
    };
    const scheduleLivePreview = (delayMs) => {
      clearPreviewTimer();
      previewTimer = setTimeout(() => {
        previewTimer = 0;
        renderLivePreview();
      }, Math.max(0, Number(delayMs) || 0));
    };
    const TEMPLATE_TOKEN_SUGGESTION_LIMIT = 12;
    const TEMPLATE_TOKEN_SUGGESTION_OBJECT_SAMPLE = 16;
    const TEMPLATE_TOKEN_CHAR_RE = /[A-Za-z0-9_.\[\]]/;
    const templateTokenSuggestionCache = new Map();
    let activeTokenSuggest = null;

    const clearTemplateTokenSuggestionCache = () => {
      templateTokenSuggestionCache.clear();
    };
    const hideTemplateTokenSuggest = (stateObj) => {
      const nextState = stateObj || activeTokenSuggest;
      if (!nextState || !nextState.popup) {
        if (!stateObj) {
          activeTokenSuggest = null;
        }
        return;
      }
      nextState.popup.hidden = true;
      nextState.popup.replaceChildren();
      nextState.items = [];
      nextState.activeIndex = -1;
      nextState.meta = null;
      if (!stateObj || activeTokenSuggest === nextState) {
        activeTokenSuggest = null;
      }
    };
    const getTemplateAutocompleteConfig = () => {
      const out = cloneJsonValue(draft);
      if (!out || typeof out !== "object" || Array.isArray(out)) {
        return { version: 1, templates: {} };
      }
      out.version = 1;
      if (!out.templates || typeof out.templates !== "object" || Array.isArray(out.templates)) {
        out.templates = {};
      }
      return out;
    };
    const getTemplatePathSuggestionsForSelectedKey = () => {
      const activeKey = String(selKey || "").trim();
      if (!activeKey) {
        return [];
      }
      if (templateTokenSuggestionCache.has(activeKey)) {
        return templateTokenSuggestionCache.get(activeKey) || [];
      }
      if (
        !state.data
        || !Array.isArray(state.renderObjects)
        || !state.renderObjects.length
        || typeof collectTemplateDumpPaths !== "function"
        || typeof buildTemplateContextObject !== "function"
        || typeof resolveTemplateMapForObject !== "function"
      ) {
        templateTokenSuggestionCache.set(activeKey, []);
        return [];
      }

      const items = getRenderableObjectListForTemplate({ includeHidden: true });
      const config = getTemplateAutocompleteConfig();
      const out = new Set();
      let sampledCount = 0;

      for (let index = 0; index < items.length; index += 1) {
        const item = items[index];
        const obj = item && typeof item === "object" ? item.obj : null;
        if (!obj) {
          continue;
        }
        const resolved = resolveTemplateMapForObject(obj, config);
        if (String((resolved && resolved.key) || "") !== activeKey) {
          continue;
        }

        const contextObj = buildTemplateContextObject(obj, index + 1);
        const paths = collectTemplateDumpPaths(contextObj);
        for (const path of Array.isArray(paths) ? paths : []) {
          const normalized = String(path || "").trim();
          if (normalized) {
            out.add(normalized);
            if (normalized.startsWith("keywords.")) {
              out.add(`keyword.${normalized.slice("keywords.".length)}`);
            }
          }
        }

        sampledCount += 1;
        if (sampledCount >= TEMPLATE_TOKEN_SUGGESTION_OBJECT_SAMPLE) {
          break;
        }
      }

      const suggestions = Array.from(out).sort((left, right) => {
        const leftLower = String(left || "").toLowerCase();
        const rightLower = String(right || "").toLowerCase();
        const leftRank = leftLower.startsWith("values.") ? 0 : (leftLower.startsWith("extras.") ? 1 : 2);
        const rightRank = rightLower.startsWith("values.") ? 0 : (rightLower.startsWith("extras.") ? 1 : 2);
        if (leftRank !== rightRank) {
          return leftRank - rightRank;
        }
        const leftDepth = String(left || "").split(".").length;
        const rightDepth = String(right || "").split(".").length;
        if (leftDepth !== rightDepth) {
          return leftDepth - rightDepth;
        }
        if (String(left || "").length !== String(right || "").length) {
          return String(left || "").length - String(right || "").length;
        }
        return String(left || "").localeCompare(String(right || ""));
      });
      templateTokenSuggestionCache.set(activeKey, suggestions);
      return suggestions;
    };
    const getTemplateTokenQueryMeta = (input) => {
      if (!input || typeof input.value !== "string") {
        return null;
      }
      const value = String(input.value || "");
      const selectionStart = Number.isFinite(Number(input.selectionStart))
        ? Number(input.selectionStart)
        : value.length;
      const caret = Math.max(0, Math.min(value.length, selectionStart));
      const beforeCaret = value.slice(0, caret);
      const openIndex = beforeCaret.lastIndexOf("{");
      const closeIndex = beforeCaret.lastIndexOf("}");
      if (openIndex < 0 || closeIndex > openIndex) {
        return null;
      }
      const query = beforeCaret.slice(openIndex + 1);
      if (/[{}\r\n]/.test(query) || /\s/.test(query)) {
        return null;
      }

      let tokenTailEnd = caret;
      while (tokenTailEnd < value.length && TEMPLATE_TOKEN_CHAR_RE.test(value[tokenTailEnd])) {
        tokenTailEnd += 1;
      }
      const hasClosingBrace = value[tokenTailEnd] === "}";
      const replaceEnd = hasClosingBrace ? tokenTailEnd + 1 : tokenTailEnd;

      return {
        query,
        openIndex,
        replaceEnd,
        hasClosingBrace
      };
    };
    const filterTemplateTokenSuggestions = (query, allSuggestions) => {
      const rawQuery = String(query || "").trim().toLowerCase();
      const ranked = [];
      for (const suggestion of Array.isArray(allSuggestions) ? allSuggestions : []) {
        const value = String(suggestion || "").trim();
        if (!value) {
          continue;
        }
        const lowered = value.toLowerCase();
        let rank = 99;
        if (!rawQuery) {
          rank = lowered.startsWith("values.") ? 0 : (lowered.startsWith("extras.") ? 1 : 2);
        } else if (lowered.startsWith(rawQuery)) {
          rank = 0;
        } else if (lowered.includes(`.${rawQuery}`)) {
          rank = 1;
        } else if (lowered.includes(rawQuery)) {
          rank = 2;
        }
        if (rank === 99) {
          continue;
        }
        ranked.push({ value, rank });
      }
      ranked.sort((left, right) => {
        if (left.rank !== right.rank) {
          return left.rank - right.rank;
        }
        const leftDepth = left.value.split(".").length;
        const rightDepth = right.value.split(".").length;
        if (leftDepth !== rightDepth) {
          return leftDepth - rightDepth;
        }
        if (left.value.length !== right.value.length) {
          return left.value.length - right.value.length;
        }
        return left.value.localeCompare(right.value);
      });
      return ranked.slice(0, TEMPLATE_TOKEN_SUGGESTION_LIMIT).map((item) => item.value);
    };
    const renderTemplateTokenSuggestions = (stateObj) => {
      if (!stateObj || !stateObj.popup) {
        return;
      }
      const items = Array.isArray(stateObj.items) ? stateObj.items : [];
      if (!items.length) {
        hideTemplateTokenSuggest(stateObj);
        return;
      }
      const activeIndex = Math.max(0, Math.min(items.length - 1, Number(stateObj.activeIndex) || 0));
      stateObj.activeIndex = activeIndex;
      const fragment = document.createDocumentFragment();
      items.forEach((item, index) => {
        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = `template-config-token-option${index === activeIndex ? " is-active" : ""}`;
        btn.textContent = item;
        btn.addEventListener("mousedown", (ev) => {
          ev.preventDefault();
        });
        btn.addEventListener("click", () => {
          if (!stateObj.input || !stateObj.meta) {
            return;
          }
          const input = stateObj.input;
          const currentValue = String(input.value || "");
          const before = currentValue.slice(0, stateObj.meta.openIndex + 1);
          const after = currentValue.slice(stateObj.meta.replaceEnd);
          const nextValue = `${before}${item}${stateObj.meta.hasClosingBrace ? "" : "}"}${after}`;
          const nextCaret = before.length + item.length + (stateObj.meta.hasClosingBrace ? 0 : 1);
          input.value = nextValue;
          try {
            input.setSelectionRange(nextCaret, nextCaret);
          } catch {
            // ignore
          }
          hideTemplateTokenSuggest(stateObj);
          input.dispatchEvent(new Event("input", { bubbles: true }));
          input.focus();
          setTimeout(() => {
            refreshTemplateTokenSuggest(input, stateObj.popup);
          }, 0);
        });
        fragment.appendChild(btn);
      });
      stateObj.popup.hidden = false;
      stateObj.popup.replaceChildren(fragment);
    };
    const refreshTemplateTokenSuggest = (input, popup) => {
      if (!input || !popup) {
        hideTemplateTokenSuggest();
        return;
      }
      const meta = getTemplateTokenQueryMeta(input);
      if (!meta) {
        popup.hidden = true;
        popup.replaceChildren();
        if (activeTokenSuggest && activeTokenSuggest.input === input) {
          hideTemplateTokenSuggest(activeTokenSuggest);
        }
        return;
      }
      const suggestions = filterTemplateTokenSuggestions(meta.query, getTemplatePathSuggestionsForSelectedKey());
      if (!suggestions.length) {
        popup.hidden = true;
        popup.replaceChildren();
        if (activeTokenSuggest && activeTokenSuggest.input === input) {
          hideTemplateTokenSuggest(activeTokenSuggest);
        }
        return;
      }
      if (activeTokenSuggest && activeTokenSuggest.input !== input) {
        hideTemplateTokenSuggest(activeTokenSuggest);
      }
      activeTokenSuggest = {
        input,
        popup,
        meta,
        items: suggestions,
        activeIndex: 0
      };
      renderTemplateTokenSuggestions(activeTokenSuggest);
    };
    const bindTemplateTokenSuggestInput = (input, popup) => {
      if (!input || !popup) {
        return;
      }
      input.addEventListener("focus", () => {
        refreshTemplateTokenSuggest(input, popup);
      });
      input.addEventListener("input", () => {
        refreshTemplateTokenSuggest(input, popup);
      });
      input.addEventListener("click", () => {
        refreshTemplateTokenSuggest(input, popup);
      });
      input.addEventListener("keyup", (ev) => {
        if (ev.key === "ArrowDown" || ev.key === "ArrowUp") {
          return;
        }
        refreshTemplateTokenSuggest(input, popup);
      });
      input.addEventListener("keydown", (ev) => {
        if (!activeTokenSuggest || activeTokenSuggest.input !== input || activeTokenSuggest.popup !== popup || popup.hidden) {
          return;
        }
        const itemCount = Array.isArray(activeTokenSuggest.items) ? activeTokenSuggest.items.length : 0;
        if (!itemCount) {
          return;
        }
        if (ev.key === "ArrowDown") {
          ev.preventDefault();
          activeTokenSuggest.activeIndex = (activeTokenSuggest.activeIndex + 1) % itemCount;
          renderTemplateTokenSuggestions(activeTokenSuggest);
          return;
        }
        if (ev.key === "ArrowUp") {
          ev.preventDefault();
          activeTokenSuggest.activeIndex = (activeTokenSuggest.activeIndex - 1 + itemCount) % itemCount;
          renderTemplateTokenSuggestions(activeTokenSuggest);
          return;
        }
        if (ev.key === "Enter" || ev.key === "Tab") {
          const current = activeTokenSuggest.items[activeTokenSuggest.activeIndex];
          if (!current) {
            return;
          }
          ev.preventDefault();
          const button = popup.querySelector(".template-config-token-option.is-active");
          if (button instanceof HTMLElement) {
            button.click();
          }
          return;
        }
        if (ev.key === "Escape") {
          ev.preventDefault();
          hideTemplateTokenSuggest(activeTokenSuggest);
        }
      });
      input.addEventListener("blur", () => {
        setTimeout(() => {
          if (activeTokenSuggest && activeTokenSuggest.input === input) {
            hideTemplateTokenSuggest(activeTokenSuggest);
          }
        }, 0);
      });
    };

    const tdef = (k, create) => {
      const key = String(k || "").trim();
      if (!key) return null;
      let def = draft.templates[key];
      if ((!def || typeof def !== "object" || Array.isArray(def)) && create) {
        def = {};
        draft.templates[key] = def;
      }
      if (!def || typeof def !== "object" || Array.isArray(def)) return null;
      const hasRanges = Object.prototype.hasOwnProperty.call(def, "ranges") && def.ranges && typeof def.ranges === "object" && !Array.isArray(def.ranges);
      return { def, ranges: hasRanges ? def.ranges : def, hasRanges };
    };
    const listRanges = (k) => {
      const info = tdef(k, true);
      if (!info) return [];
      const out = [];
      for (const rk of Object.keys(info.ranges)) {
        if (isOptionKey(rk)) continue;
        const v = info.ranges[rk];
        out.push({ rangeKey: rk, cell: (v && typeof v === "object" && !Array.isArray(v)) ? v : { text: String(v === undefined || v === null ? "" : v) } });
      }
      return out;
    };
    const readOpts = (k) => {
      const info = tdef(k, true);
      const out = {
        hideEmptyRows: true,
        hideRowsWithoutValues: true,
        expandMultilineRows: false,
        squareCells: true,
        squareCellSize: 18,
        objectLabel: ""
      };
      if (!info) return out;
      const setB = (x, v) => { if (!(v === undefined || v === null || v === "")) out[x] = Boolean(v); };
      const setS = (x, v) => { if (typeof v === "string") out[x] = v.trim(); };
      const setN = (x, v, min, max) => {
        if (v === undefined || v === null || v === "") return;
        const num = Number(v);
        if (!Number.isFinite(num)) return;
        out[x] = Math.min(max, Math.max(min, Math.round(num)));
      };
      for (const src of [info.def.options, info.def._options]) {
        if (!src || typeof src !== "object" || Array.isArray(src)) continue;
        setB("hideEmptyRows", src.hideEmptyRows);
        setB("hideRowsWithoutValues", src.hideRowsWithoutValues);
        setB("expandMultilineRows", src.expandMultilineRows);
        setB("squareCells", src.squareCells ?? src.squareCellsEnabled ?? src.fixedSquareCells);
        setN("squareCellSize", src.squareCellSize ?? src.squareCellSizePx ?? src.cellSize ?? src.cellSizePx, 16, 240);
        setS("objectLabel", src.objectLabel);
      }
      setB("hideEmptyRows", info.def.compact);
      setB("hideEmptyRows", info.def.hideEmptyRows);
      setB("hideRowsWithoutValues", info.def.hideRowsWithoutValues);
      setB("hideRowsWithoutValues", info.def.removeEmptyRows || info.def.removeEmptyRowsAdvanced || info.def.removeEmptyRowsAdv);
      setB("expandMultilineRows", info.def.expandMultilineRows || info.def.expandArrayRows || info.def.arrayToRows);
      setB("squareCells", info.def.squareCells ?? info.def.squareCellsEnabled ?? info.def.fixedSquareCells);
      setN("squareCellSize", info.def.squareCellSize ?? info.def.squareCellSizePx ?? info.def.cellSize ?? info.def.cellSizePx, 16, 240);
      setB("squareCells", info.ranges.squareCells ?? info.ranges.squareCellsEnabled ?? info.ranges.fixedSquareCells);
      setN("squareCellSize", info.ranges.squareCellSize ?? info.ranges.squareCellSizePx ?? info.ranges.cellSize ?? info.ranges.cellSizePx, 16, 240);
      return out;
    };
    const setOpt = (k, name, v) => {
      const info = tdef(k, true);
      if (!info) return;
      const next = info.def._options && typeof info.def._options === "object" && !Array.isArray(info.def._options) ? info.def._options : {};
      next[name] = Boolean(v);
      info.def._options = next;
      scheduleLivePreview(80);
    };
    const setTextOpt = (k, name, value) => {
      const info = tdef(k, true);
      if (!info) return;
      const next = info.def._options && typeof info.def._options === "object" && !Array.isArray(info.def._options) ? info.def._options : {};
      next[name] = String(value === undefined || value === null ? "" : value).trim();
      info.def._options = next;
      scheduleLivePreview(80);
    };
    const nextRangeKey = (k) => {
      const info = tdef(k, true);
      if (!info) return "A1";
      const used = new Set(Object.keys(info.ranges).map((x) => String(x || "").trim().toUpperCase()));
      for (let i = 1; i <= 9999; i += 1) {
        const key = `A${i}`;
        if (!used.has(key)) return key;
      }
      return `A${Date.now()}`;
    };
    const setCell = (k, r, f, v) => {
      const info = tdef(k, true);
      if (!info) return;
      const cur = info.ranges[r];
      const cell = cur && typeof cur === "object" && !Array.isArray(cur) ? cur : {};
      if (f === "text") cell.text = String(v === undefined || v === null ? "" : v);
      else if (f === "font size") {
        const raw = String(v === undefined || v === null ? "" : v).trim();
        if (!raw) delete cell["font size"];
        else cell["font size"] = raw;
      } else if (f === "background" || f === "font color") {
        const raw = String(v === undefined || v === null ? "" : v).trim();
        if (!raw) delete cell[f];
        else cell[f] = isHexColorValue(raw) ? expandHexColorValue(raw) : raw;
      } else if (f === "wrap" || f === "merge" || f === "bold" || f === "italic" || f === "underline") cell[f] = Boolean(v);
      else {
        const txt = String(v === undefined || v === null ? "" : v).trim();
        if (!txt) delete cell[f];
        else cell[f] = txt;
      }
      info.ranges[r] = cell;
      scheduleLivePreview(80);
    };
    const validateDraft = () => {
      const msg = [];
      for (const key of Object.keys(draft.templates)) {
        for (const e of listRanges(key)) {
          try { parseRangeKey(e.rangeKey); } catch (er) { msg.push(`[${key}] ${e.rangeKey}: ${er && er.message ? er.message : "Invalid range."}`); }
          if (Object.prototype.hasOwnProperty.call(e.cell, "font size")) {
            const raw = String(e.cell["font size"] === undefined || e.cell["font size"] === null ? "" : e.cell["font size"]).trim();
            if (raw && !Number.isFinite(Number(raw))) msg.push(`[${key}] ${e.rangeKey}: Font size must be numeric.`);
          }
          for (const colorKey of ["background", "font color"]) {
            if (!Object.prototype.hasOwnProperty.call(e.cell, colorKey)) {
              continue;
            }
            const raw = String(e.cell[colorKey] === undefined || e.cell[colorKey] === null ? "" : e.cell[colorKey]).trim();
            if (raw && !isHexColorValue(raw)) {
              const label = colorKey === "font color" ? "Font color" : "Background";
              msg.push(`[${key}] ${e.rangeKey}: ${label} must be a hex color like #aabbcc.`);
            }
          }
        }
      }
      return { ok: msg.length === 0, messages: msg };
    };
    const serializeDraft = () => {
      const out = cloneJsonValue(draft) || getDefaultTemplateConfig();
      out.version = 1;
      if (!out.templates || typeof out.templates !== "object" || Array.isArray(out.templates)) out.templates = {};
      for (const key of Object.keys(out.templates)) {
        const def = out.templates[key];
        if (!def || typeof def !== "object" || Array.isArray(def)) continue;
        const hasRanges = Object.prototype.hasOwnProperty.call(def, "ranges") && def.ranges && typeof def.ranges === "object" && !Array.isArray(def.ranges);
        const ranges = hasRanges ? def.ranges : def;
        const opts = readOpts(key);
        def._options = {
          hideEmptyRows: Boolean(opts.hideEmptyRows),
          hideRowsWithoutValues: Boolean(opts.hideRowsWithoutValues),
          expandMultilineRows: Boolean(opts.expandMultilineRows),
          squareCells: opts.squareCells !== false,
          squareCellSize: Math.min(240, Math.max(16, Math.round(Number(opts.squareCellSize) || 18))),
          objectLabel: String(opts.objectLabel || "").trim()
        };
        delete def.options; delete def.compact; delete def.hideEmptyRows; delete def.hideRowsWithoutValues; delete def.expandMultilineRows;
        delete def.removeEmptyRows; delete def.removeEmptyRowsAdvanced; delete def.removeEmptyRowsAdv; delete def.expandArrayRows; delete def.arrayToRows;
        for (const rk of Object.keys(ranges)) {
          if (isOptionKey(rk)) continue;
          const cell = ranges[rk];
          if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;
          if (Object.prototype.hasOwnProperty.call(cell, "font size")) {
            const raw = String(cell["font size"] === undefined || cell["font size"] === null ? "" : cell["font size"]).trim();
            if (!raw) delete cell["font size"];
            else cell["font size"] = Number(raw);
          }
        }
      }
      return out;
    };
    const columnNumberToLabel = (col) => {
      let n = Math.max(1, Math.floor(Number(col) || 1));
      let label = "";
      while (n > 0) {
        const rem = (n - 1) % 26;
        label = String.fromCharCode(65 + rem) + label;
        n = Math.floor((n - 1) / 26);
      }
      return label;
    };
    const makeCellKey = (row, col) => `${columnNumberToLabel(col)}${Math.max(1, Math.floor(Number(row) || 1))}`;
    const normalizeRangeKeyForBuilder = (rangeKey) => {
      try {
        const parsed = parseRangeKey(rangeKey);
        if (!parsed) {
          return "A1";
        }
        const startKey = makeCellKey(parsed.r1, parsed.c1);
        const endKey = makeCellKey(parsed.r2, parsed.c2);
        return startKey === endKey ? startKey : `${startKey}:${endKey}`;
      } catch {
        return "A1";
      }
    };
    const makeRangeKeyFromBounds = (r1, c1, r2, c2) => {
      const top = Math.min(Number(r1) || 1, Number(r2) || 1);
      const left = Math.min(Number(c1) || 1, Number(c2) || 1);
      const bottom = Math.max(Number(r1) || 1, Number(r2) || 1);
      const right = Math.max(Number(c1) || 1, Number(c2) || 1);
      const startKey = makeCellKey(top, left);
      const endKey = makeCellKey(bottom, right);
      return startKey === endKey ? startKey : `${startKey}:${endKey}`;
    };
    const getBuilderSelection = () => {
      const rangeKey = normalizeRangeKeyForBuilder(selRange || "A1");
      try {
        const parsed = parseRangeKey(rangeKey);
        if (parsed) {
          return parsed;
        }
      } catch {
        // fallback below
      }
      return { r1: 1, c1: 1, r2: 1, c2: 1, key: "A1" };
    };
    const isSingleBuilderSelection = () => {
      const selection = getBuilderSelection();
      return selection.r1 === selection.r2 && selection.c1 === selection.c2;
    };
    const rangesOverlapForBuilder = (left, right) => Boolean(left && right)
      && !(left.r2 < right.r1 || left.r1 > right.r2 || left.c2 < right.c1 || left.c1 > right.c2);
    const rangeContainsCellForBuilder = (range, row, col) => Boolean(range)
      && row >= range.r1 && row <= range.r2 && col >= range.c1 && col <= range.c2;
    const rangeAreaForBuilder = (range) => {
      if (!range) return Number.POSITIVE_INFINITY;
      return Math.max(1, range.r2 - range.r1 + 1) * Math.max(1, range.c2 - range.c1 + 1);
    };
    const getCurrentCell = () => {
      const info = tdef(selKey, true);
      if (!info) return {};
      const normalized = normalizeRangeKeyForBuilder(selRange || "A1");
      const current = info.ranges[normalized];
      if (current && typeof current === "object" && !Array.isArray(current)) {
        return current;
      }
      return {};
    };
    const ensureSelectedCell = () => {
      const info = tdef(selKey, true);
      if (!info) return null;
      const normalized = normalizeRangeKeyForBuilder(selRange || "A1");
      selRange = normalized;
      const current = info.ranges[normalized];
      if (current && typeof current === "object" && !Array.isArray(current)) {
        return current;
      }
      info.ranges[normalized] = {};
      return info.ranges[normalized];
    };
    const setSelectedCellField = (field, value) => {
      const normalized = normalizeRangeKeyForBuilder(selRange || "A1");
      selRange = normalized;
      setCell(selKey, normalized, field, value);
    };
    const setSelectedRangeKey = (nextRangeKey) => {
      const normalized = normalizeRangeKeyForBuilder(nextRangeKey || "A1");
      const info = tdef(selKey, true);
      if (!info) {
        selRange = normalized;
        return true;
      }
      const oldKey = normalizeRangeKeyForBuilder(selRange || normalized);
      if (oldKey === normalized) {
        selRange = normalized;
        return true;
      }
      if (Object.prototype.hasOwnProperty.call(info.ranges, normalized)) {
        showErr(`Range ${normalized} already exists.`);
        return false;
      }
      const current = info.ranges[oldKey];
      if (current && typeof current === "object" && !Array.isArray(current)) {
        delete info.ranges[oldKey];
        info.ranges[normalized] = current;
      } else {
        info.ranges[normalized] = {};
      }
      selRange = normalized;
      showErr("");
      scheduleLivePreview(80);
      return true;
    };
    const getTemplateGridEntries = () => listRanges(selKey)
      .map((entry) => {
        try {
          const parsed = parseRangeKey(entry.rangeKey);
          return parsed ? { ...entry, parsed, area: rangeAreaForBuilder(parsed) } : null;
        } catch {
          return null;
        }
      })
      .filter(Boolean)
      .sort((left, right) => {
        if (left.area !== right.area) return right.area - left.area;
        return String(left.rangeKey || "").localeCompare(String(right.rangeKey || ""));
      });
    const getBuilderGridSize = () => {
      let rows = 8;
      let cols = 8;
      for (const entry of getTemplateGridEntries()) {
        rows = Math.max(rows, Number(entry.parsed.r2) || 1);
        cols = Math.max(cols, Number(entry.parsed.c2) || 1);
      }
      const selection = getBuilderSelection();
      rows = Math.max(rows, selection.r2);
      cols = Math.max(cols, selection.c2);
      return { rows: Math.min(Math.max(rows + 1, 8), 40), cols: Math.min(Math.max(cols + 1, 8), 26) };
    };
    const getEntriesForGridCell = (row, col) => getTemplateGridEntries()
      .filter((entry) => rangeContainsCellForBuilder(entry.parsed, row, col))
      .sort((left, right) => right.area - left.area);
    const applyStyleToGridCell = (td, cell, matches, row, col) => {
      const cfg = cell && typeof cell === "object" ? cell : {};
      if (cfg.background) td.style.backgroundColor = String(cfg.background);
      if (cfg["font color"]) td.style.color = String(cfg["font color"]);
      if (cfg["font size"]) td.style.fontSize = `${Number(cfg["font size"]) || 10}pt`;
      if (cfg["font family"] || cfg.font) td.style.fontFamily = String(cfg["font family"] || cfg.font);
      if (cfg.align) td.style.textAlign = String(cfg.align);
      if (cfg.valign) td.style.verticalAlign = String(cfg.valign);
      if (cfg.bold) td.style.fontWeight = "700";
      if (cfg.italic) td.style.fontStyle = "italic";
      if (cfg.underline) td.style.textDecoration = "underline";
      const borderLine = "2px solid #111111";
      for (const entry of Array.isArray(matches) ? matches : []) {
        if (!entry.cell || entry.cell.border !== "outside-thin") continue;
        if (row === entry.parsed.r1) td.style.borderTop = borderLine;
        if (col === entry.parsed.c2) td.style.borderRight = borderLine;
        if (row === entry.parsed.r2) td.style.borderBottom = borderLine;
        if (col === entry.parsed.c1) td.style.borderLeft = borderLine;
      }
    };
    const insertPlaceholderIntoText = (text, placeholder) => {
      const raw = String(placeholder || "").trim();
      if (!raw) return String(text || "");
      const token = raw.startsWith("{") && raw.endsWith("}") ? raw : `{${raw}}`;
      const current = String(text || "");
      return current ? `${current}${token}` : token;
    };
    const applyDetailedFormatToCell = (cell, backgroundColor) => {
      cell.background = backgroundColor;
      cell.border = "outside-thin";
      cell.font = "MS PGothic";
      cell["font color"] = "#111111";
      cell["font size"] = 10;
      cell["font family"] = "default";
      cell.bold = false;
      cell.italic = false;
      cell.underline = false;
      cell.merge = false;
      cell.align = "left";
      cell.valign = "top";
      cell.wrap = false;
    };
    const applyPaletteToolToSelection = (tool, options) => {
      const type = String(tool || "").trim().toLowerCase();
      const opts = options && typeof options === "object" ? options : {};
      const targetRange = normalizeRangeKeyForBuilder(opts.rangeKey || selRange || "A1");
      selRange = targetRange;
      const cell = ensureSelectedCell() || {};
      if (type === "text") {
        if (!Object.prototype.hasOwnProperty.call(cell, "text") || String(cell.text || "") === "") {
          setSelectedCellField("text", "Text");
        }
      } else if (type === "placeholder") {
        const selectedPath = String(opts.placeholder || "").trim() || String(getTemplatePathSuggestionsForSelectedKey()[0] || "values.name.finalDesc");
        setSelectedCellField("text", insertPlaceholderIntoText(cell.text, selectedPath));
      } else if (type === "format-blue") {
        applyDetailedFormatToCell(cell, "#dbeef4");
      } else if (type === "format-white") {
        applyDetailedFormatToCell(cell, "#ffffff");
      }
      showErr("");
      renderActive();
    };
    const makeBuilderButton = (label, onClick, extraClass) => {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = `secondary${extraClass ? ` ${extraClass}` : ""}`;
      btn.textContent = label;
      btn.addEventListener("click", onClick);
      return btn;
    };
    const renderOptionToggle = (parent, name) => {
      const opts = readOpts(selKey);
      const label = document.createElement("label");
      label.className = "toggle";
      const input = document.createElement("input");
      input.type = "checkbox";
      input.checked = Boolean(opts[name]);
      input.addEventListener("change", () => {
        setOpt(selKey, name, input.checked);
        showErr("");
        renderActive();
      });
      label.appendChild(input);
      label.appendChild(document.createTextNode(name));
      parent.appendChild(label);
    };
    const renderBuilderCellContent = (td, text) => {
      td.replaceChildren();
      const raw = String(text || "");
      if (!raw) {
        const empty = document.createElement("span");
        empty.className = "template-builder-empty-cell";
        empty.textContent = "＋";
        td.appendChild(empty);
        return;
      }
      const parts = raw.split(/(\{[^{}]+\})/g);
      for (const part of parts) {
        if (!part) {
          continue;
        }
        const span = document.createElement("span");
        if (/^\{[^{}]+\}$/.test(part)) {
          span.className = "template-builder-placeholder-token";
        }
        span.textContent = part;
        td.appendChild(span);
      }
    };
    let builderDragging = false;
    let builderDragAnchor = null;
    const stopBuilderDrag = () => {
      if (!builderDragging) {
        return;
      }
      builderDragging = false;
      builderDragAnchor = null;
      renderActive();
    };
    document.addEventListener("pointerup", stopBuilderDrag);
    document.addEventListener("pointercancel", stopBuilderDrag);



    function renderForm() {
      els.templateConfigJson = null;
      const root = document.createElement("div");
      root.className = "template-config-builder";
      const keys = Object.keys(draft.templates);
      if (!keys.includes(selKey)) selKey = keys[0] || "";
      if (!selRange) selRange = listRanges(selKey)[0]?.rangeKey || "A1";
      selRange = normalizeRangeKeyForBuilder(selRange);

      const topbar = document.createElement("div");
      topbar.className = "template-builder-topbar";
      root.appendChild(topbar);

      const keyField = document.createElement("label");
      keyField.className = "template-builder-field template-builder-key-field";
      const keyLabel = document.createElement("span");
      keyLabel.textContent = "Template Key";
      const keySelect = document.createElement("select");
      keySelect.className = "template-config-select";
      for (const key of keys) {
        const option = document.createElement("option");
        option.value = key;
        option.textContent = key;
        keySelect.appendChild(option);
      }
      keySelect.value = selKey;
      keySelect.addEventListener("change", () => {
        selKey = keySelect.value;
        selRange = listRanges(selKey)[0]?.rangeKey || "A1";
        pendingExcelImport = null;
        excelPasteOpen = false;
        showExcelStatus("");
        showErr("");
        renderActive();
      });
      keyField.appendChild(keyLabel);
      keyField.appendChild(keySelect);
      topbar.appendChild(keyField);

      const objectLabelField = document.createElement("label");
      objectLabelField.className = "template-builder-field";
      const objectLabelText = document.createElement("span");
      objectLabelText.textContent = "Object Label";
      const objectLabelInput = document.createElement("input");
      objectLabelInput.type = "text";
      objectLabelInput.className = "template-config-select template-builder-object-label";
      objectLabelInput.placeholder = selKey;
      objectLabelInput.value = String(readOpts(selKey).objectLabel || "");
      objectLabelInput.addEventListener("input", () => {
        setTextOpt(selKey, "objectLabel", objectLabelInput.value);
      });
      objectLabelField.appendChild(objectLabelText);
      objectLabelField.appendChild(objectLabelInput);
      topbar.appendChild(objectLabelField);

      const keyActions = document.createElement("div");
      keyActions.className = "template-builder-key-actions";
      keyActions.appendChild(makeBuilderButton("Copy to Excel", async () => {
        const excel = runtime.services.templateExcel;
        const info = tdef(selKey, false);
        if (!excel || !info) {
          showErr("Excel template service is unavailable.");
          return;
        }
        try {
          const payload = excel.buildClipboardPayload(selKey, info.def);
          const richCopy = await excel.writeClipboard(payload);
          showErr("");
          showExcelStatus(
            richCopy
              ? `Copied raw template "${selKey}" to Excel.`
              : `Copied "${selKey}" as TSV because rich clipboard is unavailable.`,
            richCopy ? "success" : "warning"
          );
        } catch (error) {
          showExcelStatus("");
          showErr(error && error.message ? error.message : "Copy to Excel failed.");
        }
      }));
      keyActions.appendChild(makeBuilderButton("Paste from Excel", () => {
        pendingExcelImport = null;
        excelPasteOpen = true;
        showExcelStatus("");
        showErr("");
        renderActive();
        host.querySelector(".template-excel-paste-zone")?.focus();
      }));
      keyActions.appendChild(makeBuilderButton("Add Key", () => {
        let i = 1;
        let key = "NEW_TEMPLATE";
        while (Object.prototype.hasOwnProperty.call(draft.templates, key)) {
          i += 1;
          key = `NEW_TEMPLATE_${i}`;
        }
        draft.templates[key] = {};
        selKey = key;
        selRange = "A1";
        showErr("");
        renderActive();
      }));
      keyActions.appendChild(makeBuilderButton("Clone Key", () => {
        const src = draft.templates[selKey];
        if (!src || typeof src !== "object") {
          showErr("Current template key is invalid.");
          return;
        }
        let i = 1;
        let key = `${selKey}_COPY`;
        while (Object.prototype.hasOwnProperty.call(draft.templates, key)) {
          i += 1;
          key = `${selKey}_COPY_${i}`;
        }
        draft.templates[key] = cloneJsonValue(src) || {};
        selKey = key;
        selRange = listRanges(selKey)[0]?.rangeKey || "A1";
        showErr("");
        renderActive();
      }));
      keyActions.appendChild(makeBuilderButton("Delete Key", () => {
        if (Object.keys(draft.templates).length <= 1) {
          showErr("At least one template key is required.");
          return;
        }
        if (!confirm(`Delete template key "${selKey}"?`)) return;
        delete draft.templates[selKey];
        selKey = Object.keys(draft.templates)[0] || "";
        selRange = listRanges(selKey)[0]?.rangeKey || "A1";
        showErr("");
        renderActive();
      }, "danger-lite"));
      topbar.appendChild(keyActions);

      if (excelPasteOpen) {
        const excelPanel = document.createElement("section");
        excelPanel.className = "template-excel-import-panel";
        const excelHead = document.createElement("div");
        excelHead.className = "template-excel-import-head";
        const excelTitle = document.createElement("strong");
        excelTitle.textContent = `Paste Excel into ${selKey}`;
        const excelCancel = makeBuilderButton("Cancel", () => {
          pendingExcelImport = null;
          excelPasteOpen = false;
          showExcelStatus("");
          showErr("");
          renderActive();
        });
        excelHead.appendChild(excelTitle);
        excelHead.appendChild(excelCancel);
        excelPanel.appendChild(excelHead);

        const pasteZone = document.createElement("div");
        pasteZone.className = "template-excel-paste-zone";
        pasteZone.tabIndex = 0;
        pasteZone.textContent = "Click here, then press Ctrl+V";
        pasteZone.addEventListener("paste", (event) => {
          event.preventDefault();
          const excel = runtime.services.templateExcel;
          const info = tdef(selKey, false);
          try {
            pendingExcelImport = excel.parseClipboardPayload({
              html: event.clipboardData ? event.clipboardData.getData("text/html") : "",
              text: event.clipboardData ? event.clipboardData.getData("text/plain") : "",
              currentOptions: info && info.def ? (info.def._options || info.def.options || {}) : {}
            });
            pendingExcelImport.targetKey = selKey;
            showExcelStatus(
              pendingExcelImport.warnings.length
                ? "Excel preview created with warnings."
                : "Excel preview created successfully.",
              pendingExcelImport.warnings.length ? "warning" : "success"
            );
            showErr("");
            renderActive();
          } catch (error) {
            pendingExcelImport = null;
            showExcelStatus("");
            showErr(error && error.message ? error.message : "Paste from Excel failed.");
          }
        });
        excelPanel.appendChild(pasteZone);

        if (pendingExcelImport && pendingExcelImport.targetKey === selKey) {
          const summary = document.createElement("div");
          summary.className = "template-excel-import-summary";
          summary.textContent = `${pendingExcelImport.source.toUpperCase()} • ${pendingExcelImport.stats.rows} × ${pendingExcelImport.stats.cols} • ${pendingExcelImport.stats.ranges} ranges • ${pendingExcelImport.stats.merged} merges`;
          excelPanel.appendChild(summary);
          if (pendingExcelImport.warnings.length) {
            const warnings = document.createElement("ul");
            warnings.className = "template-excel-import-warnings";
            pendingExcelImport.warnings.forEach((message) => {
              const item = document.createElement("li");
              item.textContent = message;
              warnings.appendChild(item);
            });
            excelPanel.appendChild(warnings);
          }
          const preview = document.createElement("div");
          preview.className = "template-excel-import-preview";
          const previewPayload = runtime.services.templateExcel.buildClipboardPayload(selKey, {
            ranges: pendingExcelImport.ranges
          });
          preview.innerHTML = previewPayload.html;
          excelPanel.appendChild(preview);
          const replaceButton = makeBuilderButton("Replace selected key", () => {
            const info = tdef(selKey, true);
            if (!info || !pendingExcelImport || pendingExcelImport.targetKey !== selKey) return;
            const nextRanges = cloneJsonValue(pendingExcelImport.ranges) || {};
            if (info.hasRanges) {
              info.def.ranges = nextRanges;
            } else {
              const keptOptions = {};
              Object.keys(info.def).forEach((key) => {
                if (isOptionKey(key)) keptOptions[key] = info.def[key];
              });
              Object.keys(info.def).forEach((key) => delete info.def[key]);
              Object.assign(info.def, keptOptions, nextRanges);
            }
            selRange = Object.keys(nextRanges)[0] || "A1";
            pendingExcelImport = null;
            excelPasteOpen = false;
            showExcelStatus(`Replaced draft template "${selKey}". Click Apply to save.`, "success");
            showErr("");
            renderActive();
          });
          replaceButton.classList.add("primary");
          excelPanel.appendChild(replaceButton);
        }
        root.appendChild(excelPanel);
      }

      const optionRow = document.createElement("div");
      optionRow.className = "template-builder-options-row";
      renderOptionToggle(optionRow, "hideEmptyRows");
      renderOptionToggle(optionRow, "hideRowsWithoutValues");
      renderOptionToggle(optionRow, "expandMultilineRows");
      root.appendChild(optionRow);

      const builderShell = document.createElement("div");
      builderShell.className = "template-builder-shell";
      root.appendChild(builderShell);

      const palette = document.createElement("aside");
      palette.className = "template-builder-palette";
      const paletteTitle = document.createElement("div");
      paletteTitle.className = "template-builder-section-title";
      paletteTitle.textContent = "Palette";
      palette.appendChild(paletteTitle);
      const paletteHint = document.createElement("div");
      paletteHint.className = "template-config-editor-hint";
      paletteHint.textContent = "Drag object vào grid, hoặc click để áp dụng vào vùng đang chọn.";
      palette.appendChild(paletteHint);
      const paletteItems = [
        { type: "text", title: "Text", desc: "Tạo text cell hoặc giữ text hiện có." },
        { type: "placeholder", title: "Placeholder", desc: "Chèn {path} vào text." },
        { type: "format-blue", title: "Border vùng, nền xanh", desc: "Nền #dbeef4, border outside-thin, font MS PGothic 10." },
        { type: "format-white", title: "Border vùng, nền trắng", desc: "Nền #ffffff, border outside-thin, font MS PGothic 10." }
      ];
      for (const item of paletteItems) {
        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = `template-builder-palette-item is-${item.type}`;
        btn.draggable = true;
        btn.setAttribute("data-template-builder-tool", item.type);
        btn.innerHTML = `<strong>${item.title}</strong><span>${item.desc}</span>`;
        btn.addEventListener("dragstart", (ev) => {
          ev.dataTransfer.setData("text/plain", item.type);
          ev.dataTransfer.effectAllowed = "copy";
        });
        btn.addEventListener("click", () => applyPaletteToolToSelection(item.type));
        palette.appendChild(btn);
      }
      builderShell.appendChild(palette);

      const gridPanel = document.createElement("section");
      gridPanel.className = "template-builder-grid-panel";
      const gridHead = document.createElement("div");
      gridHead.className = "template-builder-panel-head";
      const gridTitle = document.createElement("div");
      gridTitle.className = "template-builder-section-title";
      gridTitle.textContent = "Editable Grid";
      const selectionPill = document.createElement("span");
      selectionPill.className = "selection-pill template-builder-selection-pill";
      selectionPill.textContent = selRange ? `Selected ${selRange}` : "Selected (none)";
      gridHead.appendChild(gridTitle);
      gridHead.appendChild(selectionPill);
      gridPanel.appendChild(gridHead);

      const gridWrap = document.createElement("div");
      gridWrap.className = "template-builder-grid-wrap";
      const grid = document.createElement("table");
      grid.className = "template-builder-grid";
      const builderOpts = readOpts(selKey);
      const builderCellSize = Math.min(240, Math.max(16, Math.round(Number(builderOpts.squareCellSize) || 18)));
      grid.style.setProperty("--template-builder-cell-size", `${builderCellSize}px`);
      const refreshGridSelectionClasses = () => {
        const nextSelection = getBuilderSelection();
        for (const cell of grid.querySelectorAll("td[data-r1]")) {
          const cellRange = {
            r1: Number(cell.getAttribute("data-r1")) || 1,
            c1: Number(cell.getAttribute("data-c1")) || 1,
            r2: Number(cell.getAttribute("data-r2")) || 1,
            c2: Number(cell.getAttribute("data-c2")) || 1
          };
          cell.classList.toggle("in-selection", rangesOverlapForBuilder(nextSelection, cellRange));
          cell.classList.toggle("anchor", nextSelection.r1 === cellRange.r1 && nextSelection.c1 === cellRange.c1);
        }
        selectionPill.textContent = selRange ? `Selected ${selRange}` : "Selected (none)";
      };
      const size = getBuilderGridSize();
      const selection = getBuilderSelection();
      const headRow = document.createElement("tr");
      headRow.appendChild(document.createElement("th"));
      for (let col = 1; col <= size.cols; col += 1) {
        const th = document.createElement("th");
        th.textContent = columnNumberToLabel(col);
        headRow.appendChild(th);
      }
      grid.appendChild(headRow);
      const skipCells = new Set();
      for (let row = 1; row <= size.rows; row += 1) {
        const tr = document.createElement("tr");
        const rowHeader = document.createElement("th");
        rowHeader.textContent = String(row);
        tr.appendChild(rowHeader);
        for (let col = 1; col <= size.cols; col += 1) {
          const cellKey = makeCellKey(row, col);
          if (skipCells.has(cellKey)) {
            continue;
          }
          const matches = getEntriesForGridCell(row, col);
          const matched = matches.length ? matches[matches.length - 1] : null;
          const td = document.createElement("td");
          td.tabIndex = 0;
          td.setAttribute("data-row", String(row));
          td.setAttribute("data-col", String(col));
          let displayRange = { r1: row, c1: col, r2: row, c2: col };
          let cellConfig = null;
          if (matched) {
            const mergeMatch = matches.find((entry) => entry.cell && entry.cell.merge === true);
            const structuralMatch = mergeMatch || matched;
            displayRange = structuralMatch.parsed;
            cellConfig = Object.assign({}, ...matches.map((entry) => entry.cell || {}));
            td.setAttribute("data-range-key", normalizeRangeKeyForBuilder(matched.rangeKey));
            if (mergeMatch && cellConfig && cellConfig.merge === true) {
              td.rowSpan = Math.max(1, displayRange.r2 - displayRange.r1 + 1);
              td.colSpan = Math.max(1, displayRange.c2 - displayRange.c1 + 1);
              for (let rr = displayRange.r1; rr <= displayRange.r2; rr += 1) {
                for (let cc = displayRange.c1; cc <= displayRange.c2; cc += 1) {
                  if (rr !== row || cc !== col) skipCells.add(makeCellKey(rr, cc));
                }
              }
            }
          }
          td.setAttribute("data-r1", String(displayRange.r1));
          td.setAttribute("data-c1", String(displayRange.c1));
          td.setAttribute("data-r2", String(displayRange.r2));
          td.setAttribute("data-c2", String(displayRange.c2));
          if (rangesOverlapForBuilder(selection, displayRange)) {
            td.classList.add("in-selection");
          }
          if (selection.r1 === row && selection.c1 === col) {
            td.classList.add("anchor");
          }
          applyStyleToGridCell(td, cellConfig, matches, row, col);
          const text = cellConfig && Object.prototype.hasOwnProperty.call(cellConfig, "text") ? String(cellConfig.text || "") : "";
          renderBuilderCellContent(td, text);
          td.addEventListener("pointerdown", (ev) => {
            if (ev.button !== 0) return;
            const currentSelection = getBuilderSelection();
            const useShiftAnchor = Boolean(ev.shiftKey && currentSelection);
            const anchorRow = useShiftAnchor ? currentSelection.r1 : row;
            const anchorCol = useShiftAnchor ? currentSelection.c1 : col;
            builderDragging = true;
            builderDragAnchor = { row: anchorRow, col: anchorCol };
            selRange = makeRangeKeyFromBounds(anchorRow, anchorCol, row, col);
            showErr("");
            refreshGridSelectionClasses();
          });
          td.addEventListener("pointerenter", () => {
            if (!builderDragging || !builderDragAnchor) return;
            selRange = makeRangeKeyFromBounds(builderDragAnchor.row, builderDragAnchor.col, row, col);
            showErr("");
            refreshGridSelectionClasses();
          });
          td.addEventListener("keydown", (ev) => {
            if (ev.key !== "Enter" && ev.key !== " ") return;
            ev.preventDefault();
            const currentSelection = getBuilderSelection();
            const useShiftAnchor = Boolean(ev.shiftKey && currentSelection);
            const anchorRow = useShiftAnchor ? currentSelection.r1 : row;
            const anchorCol = useShiftAnchor ? currentSelection.c1 : col;
            selRange = makeRangeKeyFromBounds(anchorRow, anchorCol, row, col);
            renderActive();
          });
          td.addEventListener("dragover", (ev) => {
            ev.preventDefault();
            td.classList.add("is-drop-target");
            ev.dataTransfer.dropEffect = "copy";
          });
          td.addEventListener("dragleave", () => {
            td.classList.remove("is-drop-target");
          });
          td.addEventListener("drop", (ev) => {
            ev.preventDefault();
            td.classList.remove("is-drop-target");
            const tool = ev.dataTransfer.getData("text/plain") || ev.dataTransfer.getData("application/x-template-tool");
            const targetRange = rangesOverlapForBuilder(getBuilderSelection(), displayRange)
              ? selRange
              : makeRangeKeyFromBounds(row, col, row, col);
            applyPaletteToolToSelection(tool, { rangeKey: targetRange });
          });
          tr.appendChild(td);
        }
        grid.appendChild(tr);
      }
      gridWrap.appendChild(grid);
      gridPanel.appendChild(gridWrap);
      builderShell.appendChild(gridPanel);

      const inspector = document.createElement("aside");
      inspector.className = "template-builder-inspector";
      const inspectorTitle = document.createElement("div");
      inspectorTitle.className = "template-builder-section-title";
      inspectorTitle.textContent = "Inspector";
      inspector.appendChild(inspectorTitle);
      const selectedCell = getCurrentCell();
      const rangeField = document.createElement("label");
      rangeField.className = "template-builder-field";
      rangeField.innerHTML = "<span>Range</span>";
      const rangeInput = document.createElement("input");
      rangeInput.type = "text";
      rangeInput.className = "template-config-cell-input";
      rangeInput.value = selRange;
      rangeInput.addEventListener("blur", () => {
        if (setSelectedRangeKey(rangeInput.value)) {
          renderActive();
        } else {
          rangeInput.value = selRange;
        }
      });
      rangeInput.addEventListener("keydown", (ev) => {
        if (ev.key === "Enter") {
          ev.preventDefault();
          rangeInput.blur();
        }
      });
      rangeField.appendChild(rangeInput);
      inspector.appendChild(rangeField);

      const textField = document.createElement("label");
      textField.className = "template-builder-field";
      const textLabel = document.createElement("span");
      textLabel.textContent = "Text / Placeholder";
      const textWrap = document.createElement("div");
      textWrap.className = "template-config-text-field template-builder-text-field";
      const textArea = document.createElement("textarea");
      textArea.className = "template-config-json template-builder-textarea";
      textArea.placeholder = "Text or {values.name.finalDesc}";
      textArea.value = String(selectedCell.text === undefined || selectedCell.text === null ? "" : selectedCell.text);
      const popup = document.createElement("div");
      popup.className = "template-config-token-suggest";
      popup.hidden = true;
      textArea.addEventListener("input", () => {
        setSelectedCellField("text", textArea.value);
        showErr("");
      });
      bindTemplateTokenSuggestInput(textArea, popup);
      textWrap.appendChild(textArea);
      textWrap.appendChild(popup);
      textField.appendChild(textLabel);
      textField.appendChild(textWrap);
      inspector.appendChild(textField);

      const pathRow = document.createElement("div");
      pathRow.className = "template-builder-path-row";
      const pathSelect = document.createElement("select");
      pathSelect.className = "template-config-select";
      const pathSuggestions = getTemplatePathSuggestionsForSelectedKey();
      const fallbackPaths = pathSuggestions.length ? pathSuggestions : ["values.name.finalDesc", "values.target.decl.finalDesc", "keywords.stmt.text"];
      for (const path of fallbackPaths.slice(0, 80)) {
        const option = document.createElement("option");
        option.value = path;
        option.textContent = path;
        pathSelect.appendChild(option);
      }
      pathRow.appendChild(pathSelect);
      pathRow.appendChild(makeBuilderButton("Insert", () => {
        const token = pathSelect.value ? `{${pathSelect.value}}` : "";
        if (!token) return;
        textArea.value = insertPlaceholderIntoText(textArea.value, token);
        textArea.dispatchEvent(new Event("input", { bubbles: true }));
        textArea.focus();
      }));
      inspector.appendChild(pathRow);

      const styleGrid = document.createElement("div");
      styleGrid.className = "template-builder-style-grid";
      const addInputField = (labelText, input) => {
        const label = document.createElement("label");
        label.className = "template-builder-field";
        const span = document.createElement("span");
        span.textContent = labelText;
        label.appendChild(span);
        label.appendChild(input);
        styleGrid.appendChild(label);
      };
      const bgInput = document.createElement("input");
      bgInput.type = "text";
      bgInput.className = "template-config-cell-input";
      bgInput.placeholder = "#ffffff";
      bgInput.value = String(selectedCell.background || "");
      bgInput.addEventListener("input", () => {
        setSelectedCellField("background", bgInput.value);
        const raw = String(bgInput.value || "").trim();
        showErr(raw && !isHexColorValue(raw) ? `[${selKey}] ${selRange}: Background must be a hex color like #aabbcc.` : "");
      });
      addInputField("Background", bgInput);

      const fontColorInput = document.createElement("input");
      fontColorInput.type = "text";
      fontColorInput.className = "template-config-cell-input";
      fontColorInput.placeholder = "#111111";
      fontColorInput.value = String(selectedCell["font color"] || "");
      fontColorInput.addEventListener("input", () => {
        setSelectedCellField("font color", fontColorInput.value);
        const raw = String(fontColorInput.value || "").trim();
        showErr(raw && !isHexColorValue(raw) ? `[${selKey}] ${selRange}: Font color must be a hex color like #aabbcc.` : "");
      });
      addInputField("Font Color", fontColorInput);

      const borderInput = document.createElement("input");
      borderInput.type = "text";
      borderInput.className = "template-config-cell-input";
      borderInput.placeholder = "outside-thin";
      borderInput.value = String(selectedCell.border || "");
      borderInput.addEventListener("input", () => { setSelectedCellField("border", borderInput.value); showErr(""); });
      addInputField("Border", borderInput);

      const fontSizeInput = document.createElement("input");
      fontSizeInput.type = "text";
      fontSizeInput.inputMode = "decimal";
      fontSizeInput.className = "template-config-cell-input";
      fontSizeInput.value = String(selectedCell["font size"] || "");
      fontSizeInput.addEventListener("input", () => {
        setSelectedCellField("font size", fontSizeInput.value);
        showErr(fontSizeInput.value.trim() && !Number.isFinite(Number(fontSizeInput.value)) ? `[${selKey}] ${selRange}: Font size must be numeric.` : "");
      });
      addInputField("Font Size", fontSizeInput);

      const alignSelect = document.createElement("select");
      alignSelect.className = "template-config-select";
      for (const value of ["", "left", "center", "right"]) {
        const option = document.createElement("option");
        option.value = value;
        option.textContent = value || "(default)";
        alignSelect.appendChild(option);
      }
      alignSelect.value = String(selectedCell.align || "");
      alignSelect.addEventListener("change", () => { setSelectedCellField("align", alignSelect.value); showErr(""); renderActive(); });
      addInputField("Align", alignSelect);

      const valignSelect = document.createElement("select");
      valignSelect.className = "template-config-select";
      for (const value of ["", "top", "middle", "bottom"]) {
        const option = document.createElement("option");
        option.value = value;
        option.textContent = value || "(default)";
        valignSelect.appendChild(option);
      }
      valignSelect.value = String(selectedCell.valign || "");
      valignSelect.addEventListener("change", () => { setSelectedCellField("valign", valignSelect.value); showErr(""); renderActive(); });
      addInputField("VAlign", valignSelect);
      inspector.appendChild(styleGrid);

      const toggleGrid = document.createElement("div");
      toggleGrid.className = "template-builder-toggle-grid";
      const addToggle = (field, labelText, disabled) => {
        const label = document.createElement("label");
        label.className = "toggle";
        const input = document.createElement("input");
        input.type = "checkbox";
        input.checked = Boolean(selectedCell[field]);
        input.disabled = Boolean(disabled);
        input.addEventListener("change", () => {
          setSelectedCellField(field, input.checked);
          showErr("");
          renderActive();
        });
        label.appendChild(input);
        label.appendChild(document.createTextNode(labelText));
        toggleGrid.appendChild(label);
      };
      addToggle("wrap", "Wrap");
      addToggle("merge", "Merge", isSingleBuilderSelection());
      addToggle("bold", "Bold");
      addToggle("italic", "Italic");
      addToggle("underline", "Underline");
      inspector.appendChild(toggleGrid);

      const actions = document.createElement("div");
      actions.className = "template-builder-inspector-actions";
      actions.appendChild(makeBuilderButton("Duplicate", () => {
        const info = tdef(selKey, true);
        if (!info || !selRange) return;
        const sourceCell = info.ranges[selRange];
        const nextKey = nextRangeKey(selKey);
        info.ranges[nextKey] = cloneJsonValue(sourceCell && typeof sourceCell === "object" ? sourceCell : {}) || {};
        selRange = nextKey;
        showErr("");
        renderActive();
      }));
      actions.appendChild(makeBuilderButton("Delete", () => {
        const info = tdef(selKey, true);
        if (!info || !selRange || !Object.prototype.hasOwnProperty.call(info.ranges, selRange)) {
          showErr("Selected range not found.");
          return;
        }
        delete info.ranges[selRange];
        selRange = listRanges(selKey)[0]?.rangeKey || "A1";
        showErr("");
        renderActive();
      }, "danger-lite"));
      actions.appendChild(makeBuilderButton("Sort", () => {
        const info = tdef(selKey, true);
        if (!info) return;
        const entries = listRanges(selKey);
        const ok = [];
        const bad = [];
        for (const entry of entries) {
          try {
            const p = parseRangeKey(entry.rangeKey);
            ok.push({ ...entry, p });
          } catch {
            bad.push(entry);
          }
        }
        ok.sort((a, b) => (a.p.r1 - b.p.r1) || (a.p.c1 - b.p.c1) || (a.p.r2 - b.p.r2) || (a.p.c2 - b.p.c2));
        const next = {};
        for (const entry of ok) next[entry.rangeKey] = info.ranges[entry.rangeKey];
        for (const entry of bad) next[entry.rangeKey] = info.ranges[entry.rangeKey];
        if (info.hasRanges) info.def.ranges = next;
        else {
          const keep = {};
          for (const key of Object.keys(info.def)) if (isOptionKey(key)) keep[key] = info.def[key];
          for (const [key, value] of Object.entries(next)) keep[key] = value;
          for (const key of Object.keys(info.def)) delete info.def[key];
          for (const [key, value] of Object.entries(keep)) info.def[key] = value;
        }
        renderActive();
      }));
      inspector.appendChild(actions);

      const check = validateDraft();
      if (!check.ok) {
        const sum = document.createElement("div");
        sum.className = "template-error";
        sum.textContent = check.messages.join("\n");
        inspector.appendChild(sum);
      }
      builderShell.appendChild(inspector);
      host.replaceChildren(root);
    }



    function renderActive() {
      hideTemplateTokenSuggest();
      clearTemplateTokenSuggestionCache();
      renderForm();
      renderLivePreview();
    }



    function applyFromModal() {
      const preCheck = validateDraft();
      if (!preCheck.ok) { showErr(preCheck.messages.join("\n")); return; }
      const nextConfig = serializeDraft();
      const chk = validateTemplateConfig(nextConfig);
      if (!chk.valid) { showErr(chk.errors.join("\n")); return; }
      const ok = applyTemplateConfigObject(nextConfig, { save: true });
      if (!ok) { const fallback = String((els.templateConfigError && els.templateConfigError.textContent) || "").trim(); if (fallback) showErr(fallback); return; }
      showErr("");
      setError("");
      closeTemplateDynamicModal();
    }

    applyBtn.addEventListener("click", applyFromModal);
    modal.root.addEventListener("keydown", (ev) => { if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") { ev.preventDefault(); applyFromModal(); } });
    renderActive();
    modal.setCleanup(() => {
      activeTemplateFormSplitController = null;
      clearPreviewTimer();
      hideTemplateTokenSuggest();
      clearTemplateTokenSuggestionCache();
      document.removeEventListener("pointerup", stopBuilderDrag);
      document.removeEventListener("pointercancel", stopBuilderDrag);
      if (typeof templateFormMaxWidthMq.removeEventListener === "function") {
        templateFormMaxWidthMq.removeEventListener("change", onTemplateFormSplitChanged);
      } else if (typeof templateFormMaxWidthMq.removeListener === "function") {
        templateFormMaxWidthMq.removeListener(onTemplateFormSplitChanged);
      }
      els.templateConfigJson = prevJsonEl || null;
      els.templateConfigError = prevErrEl || null;
    });
  }



  function openTemplateCellTextEditModal(options) {
    const opts = options && typeof options === "object" ? options : {};
    const templateKey = String(opts.templateKey || "").trim();
    const rangeKey = String(opts.rangeKey || "").trim();
    const objectType = String(opts.objectType || "").trim();
    const currentText = String(opts.currentText === undefined || opts.currentText === null ? "" : opts.currentText);
    const onSave = typeof opts.onSave === "function" ? opts.onSave : null;

    const modal = openTemplateDynamicModal("Edit Template Cell Text", { contentClass: "template-runtime-modal-content template-runtime-modal-wide" });

    const saveBtn = document.createElement("button");
    saveBtn.type = "button";
    saveBtn.className = "secondary";
    saveBtn.textContent = "Save";
    modal.actions.prepend(saveBtn);

    const hint = document.createElement("div");
    hint.className = "muted";
    hint.style.marginBottom = "8px";
    hint.textContent = [
      objectType ? `Object: ${objectType}` : "",
      templateKey ? `Template: ${templateKey}` : "",
      rangeKey ? `Range: ${rangeKey}` : ""
    ].filter(Boolean).join(" • ");
    modal.body.appendChild(hint);

    const errorEl = document.createElement("div");
    errorEl.className = "template-error";
    errorEl.style.display = "none";
    modal.body.appendChild(errorEl);

    const textarea = document.createElement("textarea");
    textarea.className = "template-config-json";
    textarea.spellcheck = false;
    textarea.placeholder = "Cell text...";
    textarea.value = currentText;
    modal.body.appendChild(textarea);

    const showInlineError = (message) => {
      const text = String(message || "").trim();
      if (!text) {
        errorEl.textContent = "";
        errorEl.style.display = "none";
        return;
      }
      errorEl.textContent = text;
      errorEl.style.display = "block";
    };

    const submit = () => {
      showInlineError("");
      if (!onSave) {
        closeTemplateDynamicModal();
        return;
      }
      let ok = false;
      try {
        ok = onSave(String(textarea.value || "")) !== false;
      } catch (err) {
        showInlineError(err && err.message ? err.message : String(err));
        return;
      }
      if (!ok) {
        const fallback = String((els.templateConfigError && els.templateConfigError.textContent) || "").trim();
        if (fallback) {
          showInlineError(fallback);
        }
        return;
      }
      closeTemplateDynamicModal();
    };

    saveBtn.addEventListener("click", submit);
    textarea.addEventListener("keydown", (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        submit();
      }
    });

    setTimeout(() => {
      textarea.focus();
      textarea.setSelectionRange(0, textarea.value.length);
    }, 0);
  }



  function openTemplateCellUnifiedEditModal(options) {
    const opts = options && typeof options === "object" ? options : {};
    const metadata = opts.metadata && typeof opts.metadata === "object" ? opts.metadata : {};
    const textPart = opts.textPart && typeof opts.textPart === "object" ? opts.textPart : {};
    const descPart = opts.descPart && typeof opts.descPart === "object" ? opts.descPart : {};
    const provenance = descPart.provenance && typeof descPart.provenance === "object"
      ? descPart.provenance
      : {};

    const templateKey = String(textPart.templateKey || "").trim();
    const rangeKey = String(textPart.rangeKey || "").trim();
    const objectType = String(textPart.objectType || metadata.objectType || "").trim();
    const currentText = String(textPart.currentText === undefined || textPart.currentText === null ? "" : textPart.currentText);
    const onSaveText = typeof textPart.onSaveText === "function" ? textPart.onSaveText : null;

    const legacyDecl = descPart.decl && typeof descPart.decl === "object" ? descPart.decl : null;
    const token = String(descPart.token || "").trim();
    const currentDesc = String(descPart.currentDesc === undefined || descPart.currentDesc === null ? "" : descPart.currentDesc);
    const skipNormalize = Boolean(descPart.skipNormalize);
    const onSaveDesc = typeof descPart.onSaveDesc === "function" ? descPart.onSaveDesc : null;
    const normalizeEnabled = Boolean(state && state.settings && state.settings.normalizeDeclDesc);
    const descResolutionStatus = String(
      descPart.status || provenance.status || metadata.status || ""
    ).trim();
    const descReasonCode = String(
      descPart.reasonCode || provenance.reasonCode || metadata.reasonCode || ""
    ).trim().toUpperCase().replace(/[\s-]+/g, "_");
    const descSourcePath = String(
      descPart.sourcePath || provenance.sourcePath || metadata.sourcePath || ""
    ).trim();

    const getUnavailableDescriptionMessage = () => {
      const reasonCode = descReasonCode || (
        descResolutionStatus.toUpperCase() === "ERROR" ? "RESOLUTION_ERROR" : "MISSING_PROVENANCE"
      );
      const messages = {
        STATIC_TEXT: "Description không áp dụng cho static text.",
        LITERAL_NO_DECL: "Literal, số hoặc wildcard không có declaration.",
        NON_DECL_SCHEMA_VALUE: "Schema, type hoặc routine name không phải data operand.",
        UNRESOLVED_TEMPLATE_PATH: "Template path không resolve được.",
        UNBOUND_IDENTIFIER: "Data operand chưa được bind với declaration.",
        MISSING_PROVENANCE: "Cell có giá trị nhưng thiếu provenance.",
        RESOLUTION_ERROR: "Resolver phát sinh lỗi khi xác định declaration."
      };
      if (Object.prototype.hasOwnProperty.call(messages, reasonCode)) {
        return messages[reasonCode];
      }
      if (rawDeclCandidates.length && !onSaveDesc) {
        return "Không thể lưu Description vì chức năng lưu declaration không khả dụng.";
      }
      return messages.MISSING_PROVENANCE;
    };

    const normalizeDescValueForModal = (decl, value, noNormalize) => {
      const raw = String(value === undefined || value === null ? "" : value);
      if (!raw) {
        return "";
      }
      if (!normalizeEnabled || noNormalize) {
        return raw;
      }
      if (typeof normalizeDeclDescText === "function") {
        const normalized = String(normalizeDeclDescText(decl, raw) || "");
        return normalized || raw;
      }
      return raw;
    };

    const getDeclOverrideEntrySafeByDecl = (decl) => {
      if (!decl || typeof decl !== "object") {
        return { text: "", noNormalize: false };
      }
      if (typeof getDeclOverrideEntry === "function") {
        try {
          const entry = getDeclOverrideEntry(decl);
          if (entry && typeof entry === "object") {
            return { text: String(entry.text || ""), noNormalize: Boolean(entry.noNormalize) };
          }
        } catch {
          // fallback below
        }
      }
      let key = "";
      if (typeof getDeclOverrideStorageKey === "function") {
        try {
          key = String(getDeclOverrideStorageKey(decl) || "").trim();
        } catch {
          key = "";
        }
      }
      if (!key || !state || !state.descOverrides || !Object.prototype.hasOwnProperty.call(state.descOverrides, key)) {
        return { text: "", noNormalize: false };
      }
      const raw = state.descOverrides[key];
      if (typeof raw === "string") {
        return { text: raw, noNormalize: false };
      }
      if (raw && typeof raw === "object") {
        return { text: String(raw.text || ""), noNormalize: Boolean(raw.noNormalize) };
      }
      return { text: "", noNormalize: false };
    };

    const isStructFieldDeclForModal = (decl) => {
      if (!decl || typeof decl !== "object") {
        return false;
      }
      if (typeof isStructFieldDecl === "function") {
        try {
          return Boolean(isStructFieldDecl(decl));
        } catch {
          // fallback below
        }
      }
      return String(decl.objectType || "").trim().toUpperCase() === "STRUCT_FIELD";
    };

    const buildStructDeclFromFieldDeclSafe = (decl) => {
      if (!decl || typeof decl !== "object") {
        return null;
      }
      if (typeof buildStructDeclFromFieldDecl === "function") {
        try {
          const resolved = buildStructDeclFromFieldDecl(decl);
          if (resolved && typeof resolved === "object") {
            return resolved;
          }
        } catch {
          // fallback below
        }
      }
      if (!decl.scopeLabel || !decl.structName) {
        return null;
      }
      return {
        id: decl.structId || null,
        objectType: decl.structObjectType || decl.objectType || "STRUCT",
        name: String(decl.structName || ""),
        file: decl.file || "",
        lineStart: decl.structLineStart || null,
        raw: decl.structRaw || "",
        comment: decl.structComment || decl.structTypeComment || "",
        scopeId: decl.scopeId || 0,
        scopeLabel: decl.scopeLabel || "",
        scopeType: decl.scopeType || "",
        scopeName: decl.scopeName || ""
      };
    };

    const getEffectiveDescSafe = (decl) => {
      if (!decl || typeof decl !== "object") {
        return "";
      }
      if (typeof getEffectiveDeclDesc === "function") {
        try {
          return String(getEffectiveDeclDesc(decl) || "");
        } catch {
          return "";
        }
      }
      return "";
    };

    const getAtomicEffectiveDescSafe = (decl) => {
      if (!decl || typeof decl !== "object") {
        return "";
      }
      if (typeof getEffectiveDeclAtomicDescNormalized === "function") {
        try {
          return String(getEffectiveDeclAtomicDescNormalized(decl) || "");
        } catch {
          // fallback below
        }
      }
      return getEffectiveDescSafe(decl);
    };

    const stripStructPrefixForModalItemText = (itemText, structName) => {
      const raw = String(itemText || "").trim();
      if (!raw) {
        return "";
      }
      const struct = String(structName || "").trim();
      if (!struct) {
        return raw;
      }
      const prefix = `${struct.toUpperCase()}-`;
      if (raw.toUpperCase().startsWith(prefix)) {
        return raw.slice(struct.length + 1).trim();
      }
      return raw;
    };

    const isPerformTraceableDeclForModal = (decl) => {
      if (!decl || typeof decl !== "object") {
        return false;
      }
      const objectType = String(decl.objectType || "").toUpperCase();
      if (objectType === "FORM_PARAM" || objectType === "METHOD_PARAM") {
        return true;
      }
      return objectType === "STRUCT_FIELD"
        && ["FORM_PARAM", "METHOD_PARAM"].includes(String(decl.structObjectType || "").toUpperCase())
        && String(decl.structName || "").trim() !== ""
        && String(decl.fieldPath || "").trim() !== "";
    };

    // One PERFORM param chain (root + FORM_PARAM[+nested]) — not unrelated left/right operands.
    const isSinglePerformTraceFamilyCandidates = (candidates) => {
      const decls = (Array.isArray(candidates) ? candidates : [])
        .map((candidate) => (candidate && candidate.decl && typeof candidate.decl === "object" ? candidate.decl : null))
        .filter(Boolean);
      if (decls.length < 2) {
        return false;
      }
      const formParams = decls.filter(isPerformTraceableDeclForModal);
      if (!formParams.length) {
        return false;
      }
      const roots = decls.filter((decl) => !isPerformTraceableDeclForModal(decl));
      if (roots.length > 1) {
        return false;
      }
      return formParams.length + roots.length === decls.length;
    };

    // Never persist shared FORM_PARAM keys — they collide across multiple PERFORM sources.
    const getPerformTraceChainSaveDecls = (candidates) => {
      return (Array.isArray(candidates) ? candidates : [])
        .map((candidate) => (candidate && candidate.decl && typeof candidate.decl === "object" ? candidate.decl : null))
        .filter((decl) => decl && !isPerformTraceableDeclForModal(decl));
    };

    const normalizeDeclCandidate = (candidate, index) => {
      if (!candidate || typeof candidate !== "object") {
        return null;
      }
      const decl = candidate.decl && typeof candidate.decl === "object"
        ? candidate.decl
        : (candidate.objectType && candidate.name ? candidate : null);
      if (!decl) {
        return null;
      }
      let declKey = String(candidate.declKey || "").trim();
      if (typeof getDeclOverrideStorageKey === "function") {
        const storageKey = String(getDeclOverrideStorageKey(decl) || "").trim();
        declKey = storageKey || declKey;
      }
      const techName = typeof getDeclTechName === "function"
        ? getDeclTechName(decl)
        : String(decl.name || "");
      const scopeLabel = String(decl.scopeLabel || "").trim();
      const fallbackLabel = scopeLabel
        ? `${techName || "(unknown)"} @ ${scopeLabel}`
        : `${techName || "(unknown)"}`;
      return {
        decl,
        declKey: declKey || `idx:${index}`,
        label: String(candidate.label || fallbackLabel),
        currentDesc: String(candidate.currentDesc === undefined || candidate.currentDesc === null ? "" : candidate.currentDesc),
        skipNormalize: Boolean(candidate.skipNormalize),
        selected: candidate.selected === true
      };
    };

    const buildDescDraftForCandidate = (candidate) => {
      const decl = candidate && candidate.decl && typeof candidate.decl === "object" ? candidate.decl : null;
      if (!decl) {
        return {
          mode: "single",
          text: String(candidate && candidate.currentDesc ? candidate.currentDesc : ""),
          skipNormalize: Boolean(candidate && candidate.skipNormalize),
          initialText: String(candidate && candidate.currentDesc ? candidate.currentDesc : ""),
          initialSkipNormalize: Boolean(candidate && candidate.skipNormalize)
        };
      }

      const itemEntry = getDeclOverrideEntrySafeByDecl(decl);
      const itemDisplayRaw = itemEntry.text
        ? normalizeDescValueForModal(decl, itemEntry.text, itemEntry.noNormalize)
        : (String(candidate && candidate.currentDesc ? candidate.currentDesc : "") || getAtomicEffectiveDescSafe(decl));
      const itemDisplay = stripStructPrefixForModalItemText(itemDisplayRaw, String(decl.structName || ""));

      if (/^(?:PERFORM|METHOD)_CHAIN:/.test(String(candidate && candidate.declKey || ""))) {
        return {
          mode: "single",
          text: itemDisplay,
          skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize)),
          initialText: String(itemDisplay || ""),
          initialSkipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
        };
      }

      if (!isStructFieldDeclForModal(decl)) {
        return {
          mode: "single",
          text: itemDisplay,
          skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize)),
          initialText: String(itemDisplay || ""),
          initialSkipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
        };
      }

      const structDecl = buildStructDeclFromFieldDeclSafe(decl);
      const structKey = structDecl && typeof getDeclOverrideStorageKey === "function"
        ? String(getDeclOverrideStorageKey(structDecl) || "")
        : "";
      const itemKey = typeof getDeclOverrideStorageKey === "function"
        ? String(getDeclOverrideStorageKey(decl) || "")
        : String(candidate && candidate.declKey ? candidate.declKey : "");
      if (!structDecl || !structKey || !itemKey) {
        return {
          mode: "single",
          text: itemDisplay,
          skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize)),
          initialText: String(itemDisplay || ""),
          initialSkipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
        };
      }

      const structEntry = getDeclOverrideEntrySafeByDecl(structDecl);
      const structDisplay = structEntry.text
        ? normalizeDescValueForModal(structDecl, structEntry.text, false)
        : getEffectiveDescSafe(structDecl);

      return {
        mode: "structField",
        structDecl,
        itemDecl: decl,
        structKey,
        itemKey,
        structText: String(structDisplay || ""),
        itemText: String(itemDisplay || ""),
        skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize)),
        initialStructText: String(structDisplay || ""),
        initialItemText: String(itemDisplay || ""),
        initialSkipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
      };
    };

    const rawDeclCandidates = Array.isArray(descPart.declCandidates)
      ? descPart.declCandidates
      : [];
    let declCandidates = rawDeclCandidates
      .map((candidate, index) => normalizeDeclCandidate(candidate, index))
      .filter(Boolean);

    const uniqueDeclCandidates = [];
    const seenDeclCandidateKeys = new Set();
    for (const candidate of declCandidates) {
      const groupKey = String(candidate && candidate.declKey || "").trim();
      if (groupKey && seenDeclCandidateKeys.has(groupKey)) {
        continue;
      }
      if (groupKey) {
        seenDeclCandidateKeys.add(groupKey);
      }
      uniqueDeclCandidates.push(candidate);
    }
    declCandidates = uniqueDeclCandidates;

    if (!declCandidates.length && legacyDecl) {
      declCandidates = [{
        decl: legacyDecl,
        declKey: (typeof getDeclOverrideStorageKey === "function" ? String(getDeclOverrideStorageKey(legacyDecl) || "") : "") || "idx:0",
        label: (() => {
          const name = typeof getDeclTechName === "function" ? getDeclTechName(legacyDecl) : String(legacyDecl.name || "");
          const scope = String(legacyDecl.scopeLabel || "").trim();
          return scope ? `${name || "(unknown)"} @ ${scope}` : `${name || "(unknown)"}`;
        })(),
        currentDesc,
        skipNormalize,
        selected: true
      }];
    }

    const hasDecl = Boolean(declCandidates.length && onSaveDesc);
    const showDeclTargetSelect = hasDecl && declCandidates.length > 1;
    const modal = openTemplateDynamicModal("Edit Template Cell", { contentClass: "template-runtime-modal-content template-runtime-modal-wide" });

    const saveBtn = document.createElement("button");
    saveBtn.type = "button";
    saveBtn.className = "secondary";
    saveBtn.textContent = "Save";
    modal.actions.prepend(saveBtn);

    const hint = document.createElement("div");
    hint.className = "muted";
    hint.style.marginBottom = "8px";
    hint.textContent = [
      objectType ? `Object: ${objectType}` : "",
      templateKey ? `Template: ${templateKey}` : "",
      rangeKey ? `Range: ${rangeKey}` : "",
      token ? `Token: ${token}` : "",
      descResolutionStatus ? `Status: ${descResolutionStatus}` : "",
      descSourcePath ? `Source path: ${descSourcePath}` : ""
    ].filter(Boolean).join(" • ");
    modal.body.appendChild(hint);

    const errorEl = document.createElement("div");
    errorEl.className = "template-error";
    errorEl.style.display = "none";
    modal.body.appendChild(errorEl);

    const tabBar = document.createElement("div");
    tabBar.className = "modal-actions";
    tabBar.style.marginBottom = "8px";
    modal.body.appendChild(tabBar);

    const descTabBtn = document.createElement("button");
    descTabBtn.type = "button";
    descTabBtn.className = "secondary";
    descTabBtn.textContent = "Description";
    tabBar.appendChild(descTabBtn);

    const textTabBtn = document.createElement("button");
    textTabBtn.type = "button";
    textTabBtn.className = "secondary";
    textTabBtn.textContent = "Template Text";
    tabBar.appendChild(textTabBtn);

    const tabContent = document.createElement("div");
    modal.body.appendChild(tabContent);

    let activeTab = "desc";
    let textValue = currentText;
    let activeTextarea = null;
    let activeStructTextarea = null;
    let activeItemTextarea = null;
    let activeSkipCheckbox = null;
    const descDraftByKey = new Map();
    for (const candidate of declCandidates) {
      descDraftByKey.set(candidate.declKey, buildDescDraftForCandidate(candidate));
    }

    let selectedDeclIndex = declCandidates.findIndex((candidate) => candidate.selected === true);
    if (selectedDeclIndex < 0) {
      selectedDeclIndex = 0;
    }

    const getSelectedDeclCandidate = () => {
      if (!declCandidates.length) {
        return null;
      }
      const safeIndex = Math.max(0, Math.min(declCandidates.length - 1, selectedDeclIndex));
      return declCandidates[safeIndex] || null;
    };

    const getDescDraft = () => {
      const selected = getSelectedDeclCandidate();
      if (!selected) {
        return {
          mode: "single",
          text: "",
          skipNormalize: false,
          initialText: "",
          initialSkipNormalize: false
        };
      }
      const existing = descDraftByKey.get(selected.declKey);
      if (existing && typeof existing === "object") {
        if (existing.mode === "structField") {
          return {
            mode: "structField",
            structDecl: existing.structDecl || null,
            itemDecl: existing.itemDecl || selected.decl,
            structKey: String(existing.structKey || ""),
            itemKey: String(existing.itemKey || selected.declKey || ""),
            structText: String(existing.structText || ""),
            itemText: String(existing.itemText || ""),
            skipNormalize: Boolean(existing.skipNormalize),
            initialStructText: String(existing.initialStructText || ""),
            initialItemText: String(existing.initialItemText || ""),
            initialSkipNormalize: Boolean(existing.initialSkipNormalize)
          };
        }
        return {
          mode: "single",
          text: String(existing.text || ""),
          skipNormalize: Boolean(existing.skipNormalize),
          initialText: String(existing.initialText || ""),
          initialSkipNormalize: Boolean(existing.initialSkipNormalize)
        };
      }
      const draft = buildDescDraftForCandidate(selected);
      descDraftByKey.set(selected.declKey, draft);
      return draft;
    };

    const setDescDraft = (patch) => {
      const selected = getSelectedDeclCandidate();
      if (!selected) {
        return;
      }
      const current = getDescDraft();
      if (current.mode === "structField") {
        descDraftByKey.set(selected.declKey, {
          ...current,
          structText: patch && Object.prototype.hasOwnProperty.call(patch, "structText")
            ? String(patch.structText || "")
            : String(current.structText || ""),
          itemText: patch && Object.prototype.hasOwnProperty.call(patch, "itemText")
            ? String(patch.itemText || "")
            : String(current.itemText || ""),
          skipNormalize: patch && Object.prototype.hasOwnProperty.call(patch, "skipNormalize")
            ? Boolean(patch.skipNormalize)
            : Boolean(current.skipNormalize)
        });
        return;
      }
      descDraftByKey.set(selected.declKey, {
        mode: "single",
        text: patch && Object.prototype.hasOwnProperty.call(patch, "text") ? String(patch.text || "") : String(current.text || ""),
        skipNormalize: patch && Object.prototype.hasOwnProperty.call(patch, "skipNormalize") ? Boolean(patch.skipNormalize) : Boolean(current.skipNormalize),
        initialText: String(current.initialText || ""),
        initialSkipNormalize: Boolean(current.initialSkipNormalize)
      });
    };

    const syncActiveInputState = () => {
      if (activeTab === "text" && activeTextarea) {
        textValue = String(activeTextarea.value || "");
      }
      if (activeTab !== "desc") {
        return;
      }
      const draft = getDescDraft();
      if (draft.mode === "structField") {
        setDescDraft({
          structText: activeStructTextarea ? String(activeStructTextarea.value || "") : String(draft.structText || ""),
          itemText: activeItemTextarea ? String(activeItemTextarea.value || "") : String(draft.itemText || ""),
          skipNormalize: activeSkipCheckbox ? Boolean(activeSkipCheckbox.checked) : Boolean(draft.skipNormalize)
        });
        return;
      }
      if (activeTextarea) {
        setDescDraft({ text: String(activeTextarea.value || "") });
      }
    };

    const bindEditorHotkeys = (textarea, onEscape) => {
      textarea.addEventListener("keydown", (ev) => {
        if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
          ev.preventDefault();
          submit();
        }
        if (ev.key === "Escape") {
          ev.preventDefault();
          onEscape();
        }
      });
    };

    const setActiveTabStyles = () => {
      textTabBtn.style.fontWeight = activeTab === "text" ? "700" : "400";
      descTabBtn.style.fontWeight = activeTab === "desc" ? "700" : "400";
    };

    const renderActiveTab = () => {
      tabContent.replaceChildren();
      setActiveTabStyles();
      activeTextarea = null;
      activeStructTextarea = null;
      activeItemTextarea = null;
      activeSkipCheckbox = null;

      if (activeTab === "text") {
        const textLabel = document.createElement("div");
        textLabel.className = "muted";
        textLabel.style.marginBottom = "6px";
        textLabel.textContent = "Template Text";
        tabContent.appendChild(textLabel);

        const textArea = document.createElement("textarea");
        textArea.className = "template-config-json";
        textArea.spellcheck = false;
        textArea.placeholder = "Cell text...";
        textArea.value = textValue;
        tabContent.appendChild(textArea);
        activeTextarea = textArea;
        bindEditorHotkeys(textArea, closeTemplateDynamicModal);
        setTimeout(() => {
          textArea.focus();
          textArea.setSelectionRange(0, textArea.value.length);
        }, 0);
        return;
      }

      const descLabel = document.createElement("div");
      descLabel.className = "muted";
      descLabel.style.marginBottom = "6px";
      descLabel.textContent = "Description";
      tabContent.appendChild(descLabel);

      const descInfo = document.createElement("div");
      descInfo.className = "muted";
      descInfo.style.marginBottom = "6px";
      tabContent.appendChild(descInfo);

      if (showDeclTargetSelect) {
        const selectorLabel = document.createElement("div");
        selectorLabel.className = "muted";
        selectorLabel.style.marginBottom = "4px";
        selectorLabel.textContent = "Target";
        tabContent.appendChild(selectorLabel);

        const selector = document.createElement("select");
        selector.className = "template-config-json";
        selector.style.height = "32px";
        selector.style.marginBottom = "8px";
        for (let i = 0; i < declCandidates.length; i += 1) {
          const candidate = declCandidates[i];
          const option = document.createElement("option");
          option.value = String(i);
          option.textContent = candidate.label;
          if (i === selectedDeclIndex) {
            option.selected = true;
          }
          selector.appendChild(option);
        }
        selector.addEventListener("change", () => {
          syncActiveInputState();
          const nextIndex = Number(selector.value);
          if (Number.isFinite(nextIndex)) {
            selectedDeclIndex = Math.max(0, Math.min(declCandidates.length - 1, nextIndex));
          }
          renderActiveTab();
        });
        tabContent.appendChild(selector);
      }

      if (!hasDecl) {
        descInfo.textContent = getUnavailableDescriptionMessage();
        if (descReasonCode) {
          descInfo.dataset.reasonCode = descReasonCode;
        }
        if (descSourcePath) {
          descInfo.title = `Source path: ${descSourcePath}`;
        }
        const descArea = document.createElement("textarea");
        descArea.className = "template-config-json";
        descArea.spellcheck = false;
        descArea.placeholder = "Description override...";
        descArea.disabled = true;
        descArea.value = "";
        tabContent.appendChild(descArea);
        activeTextarea = descArea;
        bindEditorHotkeys(descArea, closeTemplateDynamicModal);
        setTimeout(() => {
          descArea.focus();
          descArea.setSelectionRange(0, descArea.value.length);
        }, 0);
        return;
      }

      const selected = getSelectedDeclCandidate();
      const draft = getDescDraft();

      if (draft.mode === "structField") {
        descInfo.textContent = selected ? `Decl: ${selected.label} (Struct + Item)` : "Decl: (unknown)";

        const structLabel = document.createElement("div");
        structLabel.className = "muted";
        structLabel.style.marginBottom = "4px";
        structLabel.textContent = "Struct";
        tabContent.appendChild(structLabel);

        const structArea = document.createElement("textarea");
        structArea.className = "template-config-json";
        structArea.spellcheck = false;
        structArea.placeholder = "Struct description override...";
        structArea.value = String(draft.structText || "");
        structArea.addEventListener("input", () => {
          setDescDraft({ structText: String(structArea.value || "") });
        });
        tabContent.appendChild(structArea);

        const itemLabel = document.createElement("div");
        itemLabel.className = "muted";
        itemLabel.style.marginTop = "8px";
        itemLabel.style.marginBottom = "4px";
        itemLabel.textContent = "Item";
        tabContent.appendChild(itemLabel);

        const itemArea = document.createElement("textarea");
        itemArea.className = "template-config-json";
        itemArea.spellcheck = false;
        itemArea.placeholder = "Item description override...";
        itemArea.value = String(draft.itemText || "");
        itemArea.addEventListener("input", () => {
          setDescDraft({ itemText: String(itemArea.value || "") });
        });
        tabContent.appendChild(itemArea);

        const skipWrap = document.createElement("label");
        skipWrap.className = "toggle";
        const skipInput = document.createElement("input");
        skipInput.type = "checkbox";
        skipInput.checked = Boolean(draft.skipNormalize);
        skipInput.addEventListener("change", () => {
          setDescDraft({ skipNormalize: Boolean(skipInput.checked) });
        });
        skipWrap.appendChild(skipInput);
        skipWrap.appendChild(document.createTextNode("Skip normalize (Item)"));
        tabContent.appendChild(skipWrap);

        activeStructTextarea = structArea;
        activeItemTextarea = itemArea;
        activeSkipCheckbox = skipInput;
        bindEditorHotkeys(structArea, closeTemplateDynamicModal);
        bindEditorHotkeys(itemArea, closeTemplateDynamicModal);
        setTimeout(() => {
          structArea.focus();
          structArea.setSelectionRange(0, structArea.value.length);
        }, 0);
        return;
      }

      descInfo.textContent = selected ? `Decl: ${selected.label}` : "Decl: (unknown)";
      const descArea = document.createElement("textarea");
      descArea.className = "template-config-json";
      descArea.spellcheck = false;
      descArea.placeholder = "Description override...";
      descArea.value = String(draft.text || "");
      descArea.addEventListener("input", () => {
        setDescDraft({ text: String(descArea.value || "") });
      });

      const skipWrap = document.createElement("label");
      skipWrap.className = "toggle";
      const skipInput = document.createElement("input");
      skipInput.type = "checkbox";
      skipInput.checked = Boolean(draft.skipNormalize);
      skipInput.addEventListener("change", () => {
        setDescDraft({ skipNormalize: Boolean(skipInput.checked) });
      });
      skipWrap.appendChild(skipInput);
      skipWrap.appendChild(document.createTextNode("Skip normalize"));

      tabContent.appendChild(descArea);
      tabContent.appendChild(skipWrap);
      activeTextarea = descArea;
      activeSkipCheckbox = skipInput;
      bindEditorHotkeys(descArea, closeTemplateDynamicModal);
      setTimeout(() => {
        descArea.focus();
        descArea.setSelectionRange(0, descArea.value.length);
      }, 0);
    };

    textTabBtn.addEventListener("click", () => {
      if (activeTab === "text") {
        return;
      }
      syncActiveInputState();
      activeTab = "text";
      renderActiveTab();
    });

    descTabBtn.addEventListener("click", () => {
      if (activeTab === "desc") {
        return;
      }
      syncActiveInputState();
      activeTab = "desc";
      renderActiveTab();
    });

    renderActiveTab();

    const showInlineError = (message) => {
      const text = String(message || "").trim();
      if (!text) {
        errorEl.textContent = "";
        errorEl.style.display = "none";
        return;
      }
      errorEl.textContent = text;
      errorEl.style.display = "block";
    };

    const normalizeSaveResult = (result, fallbackError) => {
      if (result === false || result === null || result === undefined) {
        return { ok: false, error: fallbackError || "Save failed." };
      }
      if (typeof result === "object") {
        const ok = result.ok !== false;
        return { ok, error: ok ? "" : String(result.error || fallbackError || "Save failed.") };
      }
      return { ok: true, error: "" };
    };

    const cloneModalStateValue = (value) => {
      if (typeof cloneJsonValue === "function") {
        try {
          return cloneJsonValue(value);
        } catch {
          // fallback below
        }
      }
      try {
        return JSON.parse(JSON.stringify(value));
      } catch {
        return value;
      }
    };

    const rerenderAfterDescOverrideChange = () => {
      state.templatePreviewCache = null;
      renderActiveRightPanel();
    };

    const restoreModalSnapshots = (snapshots) => {
      const original = snapshots && typeof snapshots === "object" ? snapshots : null;
      if (!original) {
        return true;
      }

      let restored = true;

      if (Object.prototype.hasOwnProperty.call(original, "templateConfig")) {
        const nextConfig = cloneModalStateValue(original.templateConfig);
        if (normalizeTemplateConfigLegacyFieldsInPlace(nextConfig) && typeof localStorage !== "undefined") {
          try {
            localStorage.setItem(
              TEMPLATE_CONFIG_STORAGE_KEY_V1,
              JSON.stringify(nextConfig && typeof nextConfig === "object" ? nextConfig : getDefaultTemplateConfig())
            );
          } catch {
            restored = false;
          }
        }
        if (typeof applyTemplateConfigObject === "function") {
          const applied = applyTemplateConfigObject(nextConfig, { save: true });
          if (!applied) {
            restored = false;
          }
        } else {
          state.templateConfig = nextConfig;
          try {
            localStorage.setItem(
              TEMPLATE_CONFIG_STORAGE_KEY_V1,
              JSON.stringify(nextConfig && typeof nextConfig === "object" ? nextConfig : getDefaultTemplateConfig())
            );
          } catch {
            restored = false;
          }
          if (typeof syncTemplateEditorFromState === "function") {
            syncTemplateEditorFromState();
          }
          if (typeof renderTemplatePreview === "function") {
            renderTemplatePreview();
          }
        }
      }

      if (Object.prototype.hasOwnProperty.call(original, "descOverrides")) {
        state.descOverrides = cloneModalStateValue(original.descOverrides) || {};
        if (typeof saveDescOverrides === "function") {
          try {
            saveDescOverrides();
          } catch {
            restored = false;
          }
        } else {
          try {
            localStorage.setItem(DESC_STORAGE_KEY_V2, JSON.stringify(state.descOverrides || {}));
          } catch {
            restored = false;
          }
        }
        rerenderAfterDescOverrideChange();
      }

      return restored;
    };

    const submit = () => {
      showInlineError("");
      if (!templateKey || !rangeKey || !onSaveText) {
        showInlineError("Cannot edit this template cell: missing template key/range.");
        return;
      }
      syncActiveInputState();

      const operations = [];
      const nextTemplateText = String(textValue || "");
      if (nextTemplateText !== currentText) {
        operations.push({
          label: "template text",
          run: () => onSaveText({ text: nextTemplateText }),
          fallbackError: "Save template text failed.",
          getError: (result) => {
            const fallback = String((els.templateConfigError && els.templateConfigError.textContent) || "").trim();
            return result.error || fallback || "Save template text failed.";
          }
        });
      }

      if (hasDecl) {
        const selected = getSelectedDeclCandidate();
        const draft = getDescDraft();
        if (draft.mode === "structField") {
          const structChanged = String(draft.structText || "") !== String(draft.initialStructText || "");
          if (structChanged) {
            operations.push({
              label: "struct description",
              run: () => onSaveDesc({
                decl: draft.structDecl || null,
                declKey: String(draft.structKey || ""),
                text: String(draft.structText || ""),
                skipNormalize: false
              }),
              fallbackError: "Save struct description failed.",
              getError: (result) => result.error || "Save struct description failed."
            });
          }

          const itemChanged = (
            String(draft.itemText || "") !== String(draft.initialItemText || "")
            || Boolean(draft.skipNormalize) !== Boolean(draft.initialSkipNormalize)
          );
          if (itemChanged) {
            operations.push({
              label: "item description",
              run: () => onSaveDesc({
                decl: selected ? selected.decl : (draft.itemDecl || null),
                declKey: selected ? selected.declKey : String(draft.itemKey || ""),
                text: String(draft.itemText || ""),
                skipNormalize: Boolean(draft.skipNormalize)
              }),
              fallbackError: "Save item description failed.",
              getError: (result) => result.error || "Save item description failed."
            });
          }
        } else {
          const descChanged = (
            String(draft.text || "") !== String(draft.initialText || "")
            || Boolean(draft.skipNormalize) !== Boolean(draft.initialSkipNormalize)
          );
          if (descChanged) {
            const saveDecl = selected ? selected.decl : null;
            operations.push({
              label: "description",
              run: () => onSaveDesc({
                decl: saveDecl,
                declKey: saveDecl && typeof getDeclOverrideStorageKey === "function"
                  ? String(getDeclOverrideStorageKey(saveDecl) || "")
                  : (selected ? selected.declKey : ""),
                text: String(draft.text || ""),
                skipNormalize: Boolean(draft.skipNormalize)
              }),
              fallbackError: "Save description failed.",
              getError: (result) => result.error || "Save description failed."
            });
          }
        }
      }

      if (!operations.length) {
        closeTemplateDynamicModal();
        return;
      }

      const snapshots = {
        templateConfig: cloneModalStateValue(state.templateConfig && typeof state.templateConfig === "object"
          ? state.templateConfig
          : getDefaultTemplateConfig()),
        descOverrides: cloneModalStateValue(state.descOverrides && typeof state.descOverrides === "object"
          ? state.descOverrides
          : {})
      };

      for (const operation of operations) {
        let opResult = null;
        try {
          opResult = normalizeSaveResult(operation.run(), operation.fallbackError);
        } catch (err) {
          const restored = restoreModalSnapshots(snapshots);
          const message = err && err.message ? err.message : String(err);
          showInlineError(restored ? message : `${message}\nRollback failed. Reload the viewer before saving again.`);
          return;
        }
        if (!opResult.ok) {
          const restored = restoreModalSnapshots(snapshots);
          const message = typeof operation.getError === "function"
            ? operation.getError(opResult)
            : (opResult.error || operation.fallbackError || `Save ${operation.label || "change"} failed.`);
          showInlineError(restored ? message : `${message}\nRollback failed. Reload the viewer before saving again.`);
          return;
        }
      }

      closeTemplateDynamicModal();
    };

    saveBtn.addEventListener("click", submit);
  }
  runtime.registerService("template", {
    attachTemplateSyntheticDeclAliases,
    syncTemplateEditorFromState,
    applyTemplateConfigFromEditor,
    resetTemplateConfig,
    buildTemplateCollectionCopyPayload,
    ensureTemplateWindowContainsIndex,
    scheduleTemplateVirtualScroll,
    handleTemplateVirtualScroll,
    handleTemplateVirtualUserIntent,
    resetTemplateVirtualState,
    captureTemplateViewportAnchor,
    restoreTemplateViewportAnchor,
    renderTemplatePreview,
    isTemplateDynamicModalOpen,
    closeTemplateDynamicModal,
    refreshTemplateGuiFilterTypes,
    resetTemplateSelectionStateMain,
    initTemplateGuiFilterControls,
    openTemplateFilterModal,
    buildViewerConfigBundle,
    getViewerConfigExportFileName,
    openViewerConfigExportModal,
    importViewerConfigObject,
    importViewerConfigFromFile,
    getRenderableObjectListForTemplate,
    copySelectedTemplateBlocks,
    interceptTemplateCodeButtonClick,
    normalizeTemplateConfigLegacyFieldsInPlace,
    openTemplateConfigModal,
  });
})(window);
