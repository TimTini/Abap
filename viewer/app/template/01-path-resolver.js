"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};

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
      SELECT: ["into", "intotable", "into-table", "appendingtable", "appending-table", "assigning", "refinto", "referenceinto", "reference-into"],
      SORT_ITAB: ["itab"],
      WRITE: ["output", "destination"]
    };
    const allowed = allowedByType[objectType];
    if (!Array.isArray(allowed) || !allowed.includes(entryName)) {
      return false;
    }
    if (
      objectType === "MESSAGE"
      && sourceObj.extras
      && sourceObj.extras.message
      && String(sourceObj.extras.message.mode || "") === "shorthand"
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

  function getExpandedPerformBindingContextForTemplate(obj) {
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

  function isExpandedPerformTemplateTraceableDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    const objectType = String(decl.objectType || "").toUpperCase();
    if (objectType === "FORM_PARAM") {
      return true;
    }
    return objectType === "STRUCT_FIELD"
      && String(decl.structObjectType || "").toUpperCase() === "FORM_PARAM"
      && String(decl.structName || "").trim() !== ""
      && String(decl.fieldPath || "").trim() !== "";
  }

  function getExpandedPerformTemplateParamUpper(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = String(decl.objectType || "").toUpperCase();
    if (objectType === "FORM_PARAM") {
      return String(decl.name || "").trim().toUpperCase();
    }
    if (objectType === "STRUCT_FIELD" && String(decl.structObjectType || "").toUpperCase() === "FORM_PARAM") {
      return String(decl.structName || "").trim().toUpperCase();
    }
    return "";
  }

  function buildExpandedPerformTemplateTraceDecl(baseDecl, localDecl, ownerContext) {
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

  function resolveExpandedPerformTemplateTraceDecls(ownerContext, decl) {
    if (!isExpandedPerformTemplateTraceableDecl(decl)) {
      return [];
    }
    const bindingContext = getExpandedPerformBindingContextForTemplate(ownerContext);
    if (!bindingContext) {
      return [];
    }
    const paramUpper = getExpandedPerformTemplateParamUpper(decl);
    if (!paramUpper) {
      return [];
    }
    const traceDecls = bindingContext.byParamUpper.get(paramUpper);
    if (!Array.isArray(traceDecls) || !traceDecls.length) {
      return [];
    }
    const localDeclType = String(decl.objectType || "").toUpperCase();
    const remappedTraceDecls = localDeclType === "STRUCT_FIELD"
      ? traceDecls.map((traceDecl) => buildExpandedPerformTemplateTraceDecl(traceDecl, decl, ownerContext)).filter(Boolean)
      : traceDecls;
    return dedupeTemplateDecls(remappedTraceDecls);
  }

  function selectExpandedPerformTemplateRootDecl(traceDecls) {
    const list = Array.isArray(traceDecls) ? traceDecls : [];
    for (let index = list.length - 1; index >= 0; index -= 1) {
      const decl = list[index];
      if (!decl || typeof decl !== "object") {
        continue;
      }
      if (!isExpandedPerformTemplateTraceableDecl(decl)) {
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

  function remapTemplateDeclForExpandedPerform(value, ownerContext) {
    if (isDeclLikeObject(value) && isExpandedPerformTemplateTraceableDecl(value)) {
      const directTraceDecls = resolveExpandedPerformTemplateTraceDecls(ownerContext, value);
      return selectExpandedPerformTemplateRootDecl(directTraceDecls) || value;
    }

    if (!isTemplateValueEntryLikeObject(value)) {
      return value;
    }

    const localDecl = value.decl;
    if (!isDeclLikeObject(localDecl) || !isExpandedPerformTemplateTraceableDecl(localDecl)) {
      return value;
    }

    const externalTraceDecls = resolveExpandedPerformTemplateTraceDecls(ownerContext, localDecl);
    if (!externalTraceDecls.length) {
      return value;
    }

    const rootTraceDecl = selectExpandedPerformTemplateRootDecl(externalTraceDecls);
    if (!rootTraceDecl) {
      return value;
    }

    const existingOrigins = Array.isArray(value.originDecls) ? value.originDecls : [];
    const originDecls = dedupeTemplateDecls([localDecl, ...externalTraceDecls, ...existingOrigins]);
    return {
      ...value,
      decl: rootTraceDecl,
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
    "transporting-no-fields": [],
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
    CALL_METHOD: "name",
    CALL_TRANSACTION: "name",
    MESSAGE: "message",
    MOVE: "source",
    "MOVE-CORRESPONDING": "source",
    CLEAR: "target",
    METHOD: "name",
    WRITE: "output"
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
    const api = window.AbapViewerRuntime && window.AbapViewerRuntime.api;
    if (api && typeof api.resolveValueLevelFinalDesc === "function") {
      return String(api.resolveValueLevelFinalDesc(entry) || "").trim();
    }
    return "";
  }

  function createTemplateExpandedRow(text, declCandidates, provenance) {
    return {
      text: String(text === undefined || text === null ? "" : text).trim(),
      declCandidates: dedupeTemplateDecls(declCandidates),
      provenance: provenance && typeof provenance === "object" ? { ...provenance } : null
    };
  }

  function createTemplateKeywordRow(keyword, finalDesc, declCandidates, provenance) {
    const row = {
      keyword: String(keyword === undefined || keyword === null ? "" : keyword).trim(),
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

  function getTemplateKeywordRowDecls(row) {
    return getTemplateKeywordRowProvenance(row).declCandidates;
  }

  function collectTemplateTraceAwareDeclCandidates(decl, ownerContext) {
    if (!decl || typeof decl !== "object") {
      return [];
    }
    const traceDecls = resolveExpandedPerformTemplateTraceDecls(ownerContext, decl);
    const rootDecl = selectExpandedPerformTemplateRootDecl(traceDecls) || decl;
    return dedupeTemplateDecls([rootDecl, decl, ...traceDecls]);
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
    let traceAwareEntry = remapTemplateDeclForExpandedPerform(valueEntry, ownerContext);

    if (traceAwareEntry && isExpandedPerformTemplateTraceableDecl(traceAwareEntry.decl)) {
      const rootFromOrigins = selectExpandedPerformTemplateRootDecl(entry.originDecls);
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
      const rootDecl = remapTemplateDeclForExpandedPerform(localDecl, ownerContext);
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

  function buildTemplateWritePositionRow(sourceObj, write, ownerContext, objectIndexOneBased) {
    if (!write || typeof write !== "object") {
      return null;
    }
    const position = write.position && typeof write.position === "object" ? write.position : {};
    const buildPart = (name) => {
      const value = String(position[name] || "").trim();
      if (!value) {
        return createTemplateExpandedRow("", []);
      }
      const capName = `${name.charAt(0).toUpperCase()}${name.slice(1)}`;
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
    const prefix = write.newLine ? "/" : "";
    const positionText = `${prefix}${column.text}${length.text ? `(${length.text})` : ""}`;
    const declCandidates = dedupeTemplateDecls([...column.declCandidates, ...length.declCandidates]);
    return createTemplateExpandedRow(positionText || prefix, declCandidates, {
      status: declCandidates.length ? "editable" : "not_applicable",
      reasonCode: declCandidates.length ? "" : getTemplateNoDeclReason(positionText || prefix, true, false)
    });
  }

  function getTemplateSemanticSectionRows(sourceObj, keywordLabel, ownerContext, objectIndexOneBased) {
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
        const positionRow = buildTemplateWritePositionRow(sourceObj, extras.write, ownerContext, objectIndexOneBased);
        return positionRow ? [positionRow] : null;
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
        return raw.slice(start, fromIndex).trim();
      }
    }
    return String(fallbackEntry && fallbackEntry.value || "").trim();
  }

  function isTemplateSafeSimpleListItem(value) {
    const text = String(value || "").trim();
    if (!text || /^(?:AS|CASE|CAST|DISTINCT|END|FIELDS|NO|SINGLE|THEN|WHEN)$/i.test(text)) {
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

  function getTemplateSafeRawListRows(sourceObj, keywordLabel, valueEntry) {
    const objectType = String(sourceObj && sourceObj.objectType || "").trim().toUpperCase();
    if (objectType === "SELECT" && keywordLabel === "stmt") {
      const fieldsRaw = getTemplateSelectFieldSource(sourceObj, valueEntry);
      const commaItems = splitTemplateTopLevelText(fieldsRaw, "comma");
      if (commaItems.length > 1) {
        return commaItems;
      }
      return splitTemplateSafeSimpleList(fieldsRaw);
    }
    if (objectType === "SORT_ITAB" && keywordLabel === "by") {
      return splitTemplateSafeSimpleList(valueEntry && valueEntry.value);
    }
    if (objectType === "MODIFY_ITAB" && keywordLabel === "transporting") {
      return splitTemplateSafeSimpleList(valueEntry && valueEntry.value);
    }
    return null;
  }

  function getTemplateExpandedKeywordRows(sourceObj, keyword, valueEntry, ownerContext, objectIndexOneBased) {
    const keywordLabel = normalizeTemplatePairToken(keyword && keyword.label);
    const semanticRows = getTemplateSemanticSectionRows(sourceObj, keywordLabel, ownerContext, objectIndexOneBased);
    if (Array.isArray(semanticRows) && semanticRows.length) {
      return semanticRows;
    }
    const conditionRows = getTemplateConditionRows(sourceObj, keywordLabel, ownerContext, objectIndexOneBased);
    if (Array.isArray(conditionRows) && conditionRows.length) {
      return conditionRows;
    }
    const rawListRows = getTemplateSafeRawListRows(sourceObj, keywordLabel, valueEntry);
    return Array.isArray(rawListRows) && rawListRows.length
      ? rawListRows.map((text) => createTemplateExpandedRow(text, [], {
          status: "not_applicable",
          reasonCode: "NON_DECL_SCHEMA_VALUE"
        }))
      : null;
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
      if (!keywordText) {
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
          rows.push(createTemplateKeywordRow(
            keywordText,
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
                }
          ));
        }
        continue;
      }
      const canonicalValueEntry = valueEntry
        ? ensureTemplateCanonicalValueEntry(sourceObj, valueEntry, objectIndexOneBased)
        : null;
      const traceAwareValueEntry = canonicalValueEntry
        ? remapTemplateDeclForExpandedPerform(canonicalValueEntry, ownerContext || sourceObj)
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
        }
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
        : remapTemplateDeclForExpandedPerform(normalized, nextOwnerContext);

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
    const text = String(rawText === undefined || rawText === null ? "" : rawText).trim();
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

  function getTemplateEditableDeclFromResolvedValue(value) {
    const candidates = getTemplateEditableDeclCandidatesFromResolvedValue(value);
    return candidates.length ? candidates[0] : null;
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

  function resolveTemplateEditableDeclFromToken(contextObj, token) {
    const candidates = resolveTemplateEditableDeclCandidatesFromToken(contextObj, token);
    return candidates.length ? candidates[0] : null;
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

    return {
      hideEmptyRows,
      hideRowsWithoutValues,
      expandMultilineRows,
      squareCells,
      squareCellSize
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
      const textMeta = hasText ? resolveTemplateText(rawText, obj) : null;
      const placeholderToken = hasText ? parseSingleTemplatePlaceholderToken(rawText) : "";
      const declMeta = hasText ? buildTemplateCellDeclMeta(obj, rawText, {
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

    if (
      safeHtml
      && navigator.clipboard
      && typeof navigator.clipboard.write === "function"
      && typeof window.ClipboardItem === "function"
    ) {
      const item = new window.ClipboardItem({
        "text/html": new Blob([safeHtml], { type: "text/html" }),
        "text/plain": new Blob([safeText], { type: "text/plain" })
      });
      await navigator.clipboard.write([item]);
      return;
    }

    if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
      await navigator.clipboard.writeText(safeText);
      return;
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
      throw new Error("Copy failed in this browser.");
    }
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
    if (objectType && Object.prototype.hasOwnProperty.call(templates, objectType)) {
      const resolved = resolveTemplateDefinitionForPreview(templates[objectType]);
      return { key: objectType, map: resolved.map, options: resolved.options };
    }
    const defaultConfig = getDefaultTemplateConfig();
    const defaultTemplates = defaultConfig && defaultConfig.templates && typeof defaultConfig.templates === "object"
      ? defaultConfig.templates
      : {};
    if (objectType && Object.prototype.hasOwnProperty.call(defaultTemplates, objectType)) {
      const resolved = resolveTemplateDefinitionForPreview(defaultTemplates[objectType]);
      return { key: objectType, map: resolved.map, options: resolved.options };
    }
    if (Object.prototype.hasOwnProperty.call(templates, "DEFAULT")) {
      const resolved = resolveTemplateDefinitionForPreview(templates.DEFAULT);
      return { key: "DEFAULT", map: resolved.map, options: resolved.options };
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

  function getRenderableObjectListForTemplate() {
    const out = [];
    const roots = Array.isArray(state.renderObjects) ? state.renderObjects : [];

    const appendNode = (obj, depth) => {
      if (!obj || typeof obj !== "object") {
        return;
      }
      out.push({ obj, depth: Math.max(0, Number(depth) || 0) });

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

  function renderTemplatePreview() {
    if (!els.templatePreviewOutput) {
      return;
    }

    if (!state.data || !Array.isArray(state.renderObjects)) {
      setTemplatePreviewMessage("No data loaded.");
      return;
    }

    const config = state.templateConfig && typeof state.templateConfig === "object"
      ? state.templateConfig
      : getDefaultTemplateConfig();

    const check = validateTemplateConfig(config);
    if (!check.valid) {
      setTemplatePreviewMessage("Template config is invalid.");
      setTemplateConfigError(check.errors.join("\n"));
      return;
    }

    const items = getRenderableObjectListForTemplate();
    if (!items.length) {
      setTemplatePreviewMessage("No renderable objects.");
      return;
    }

    const fragment = document.createDocumentFragment();
    for (let index = 0; index < items.length; index += 1) {
      const item = items[index];
      const obj = item.obj;
      const depth = Math.max(0, Number(item.depth) || 0);
      const templateContextObj = buildTemplateContextObject(obj, index + 1);
      const resolved = resolveTemplateMapForObject(obj, config);

      const blockAttrs = { "data-template-index": String(index), "data-depth": String(depth) };
      const lineStart = Number(obj && obj.lineStart) || 0;
      if (lineStart > 0) {
        blockAttrs["data-line-start"] = String(lineStart);
      }
      const block = el("div", { className: "template-block", attrs: blockAttrs });
      const indentPx = Math.min(120, depth * 12);
      if (indentPx > 0) {
        block.style.marginLeft = `${indentPx}px`;
      } else {
        block.style.marginLeft = "";
      }
      const header = el("div", { className: "template-block-header" });

      const left = el("div");
      const label = getObjectLabel(obj);
      const titleText = `${index + 1}. ${String(obj.objectType || "OBJECT")}${label ? ` ${label}` : ""}`;
      left.appendChild(el("h4", { className: "template-block-title", text: titleText }));
      const meta = renderMeta(obj);
      left.appendChild(el("div", { className: "template-block-meta", text: meta || "" }));
      header.appendChild(left);

      const actions = el("div", { className: "template-block-actions" });
      const codeBtn = el("button", {
        className: "secondary",
        text: "Code",
        attrs: { type: "button", "data-template-action": "code" }
      });
      codeBtn.addEventListener("click", () => {
        const selectedIndex = String(index);
        if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(selectedIndex);
        } else {
          state.selectedTemplateIndex = selectedIndex;
        }
        if (lineStart > 0 && typeof selectCodeLines === "function") {
          const lineEnd = Number(obj && obj.block && obj.block.lineEnd) || lineStart;
          selectCodeLines(lineStart, lineEnd);
        }
      });
      actions.appendChild(codeBtn);

      const pathsBtn = el("button", {
        className: "secondary",
        text: "Paths",
        attrs: { type: "button", "data-template-action": "paths" }
      });
      pathsBtn.addEventListener("click", (ev) => {
        if (ev && typeof ev.stopPropagation === "function") {
          ev.stopPropagation();
        }
        openTemplatePathDump(templateContextObj, index, obj);
      });
      actions.appendChild(pathsBtn);

      const copyBtn = el("button", {
        className: "secondary",
        text: "Copy",
        attrs: { type: "button", "data-template-action": "copy" }
      });
      copyBtn.addEventListener("click", async () => {
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
      actions.appendChild(copyBtn);
      header.appendChild(actions);
      block.appendChild(header);
      block.addEventListener("click", () => {
        const selectedIndex = String(index);
        if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(selectedIndex, { scroll: false });
        } else {
          state.selectedTemplateIndex = selectedIndex;
        }
      });

      if (!resolved.map || typeof resolved.map !== "object") {
        block.appendChild(el("div", { className: "template-empty", text: "[Missing template]" }));
        fragment.appendChild(block);
        continue;
      }

      const model = buildTemplateGridModel(templateContextObj, resolved.map, resolved.options, {
      templateKey: resolved.key || "",
      objectType: String(obj.objectType || "")
    });
      if (model.errors.length) {
        block.appendChild(el("div", { className: "template-error", text: model.errors.join("\n") }));
      }

      const table = renderTemplateTable(model, isInteractive ? {
      onCellDblClick: (cellMeta, cellEl) => {
        const safeMeta = cellMeta && typeof cellMeta === "object"
          ? cellMeta
          : {
              rangeKey: String(cellEl && typeof cellEl.getAttribute === "function" ? (cellEl.getAttribute("data-template-range-key") || "") : ""),
              templateKey: String(resolved.key || ""),
              rawText: String(cellEl && cellEl.textContent ? cellEl.textContent : ""),
              isSinglePlaceholder: false,
              placeholderToken: "",
              objectType: String(obj.objectType || "")
            };
        try {
          openCellUnifiedEditor(safeMeta);
        } catch (err) {
          if (typeof setError === "function") {
            setError("Template cell edit failed: " + (err && err.message ? err.message : String(err || "")));
          }
        }
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

      fragment.appendChild(block);
    }

    els.templatePreviewOutput.classList.remove("muted");
    els.templatePreviewOutput.replaceChildren(fragment);
    state.templatePreviewCache = { count: items.length };
    if (state.selectedTemplateIndex !== "" && typeof setSelectedTemplateBlock === "function") {
      setSelectedTemplateBlock(state.selectedTemplateIndex, { scroll: false });
    }
    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
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

  async function copyAllTemplateBlocks() {
    if (!els.templatePreviewOutput) {
      return;
    }
    const blocks = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"));
    if (!blocks.length) {
      setError("Nothing to copy.");
      return;
    }

    const wrapper = document.createElement("div");
    const plainLines = [];
    const tableOnly = isTemplateCopyTableOnlyEnabled();
    for (const block of blocks) {
      const payload = buildTemplateCopyPayloadFromBlock(block);
      if (!payload.node) {
        continue;
      }
      wrapper.appendChild(payload.node);
      if (tableOnly) {
        wrapper.appendChild(document.createElement("br"));
      }
      plainLines.push(payload.text);
    }

    if (!wrapper.childNodes.length) {
      setError("Nothing to copy.");
      return;
    }

    await copyHtmlWithFallback(wrapper.innerHTML, plainLines.filter(Boolean).join("\n\n"));
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
    const selected = state.selectedTemplateIndex !== "" && state.selectedTemplateIndex === indexText;
    const blockAttrs = { "data-template-index": indexText, "data-depth": String(depth) };
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
    const label = getObjectLabel(obj);
    const titleText = `${absIndex + 1}. ${String(obj.objectType || "OBJECT")}${label ? ` ${label}` : ""}`;
    left.appendChild(el("h4", { className: "template-block-title", text: titleText }));
    const meta = renderMeta(obj);
    left.appendChild(el("div", { className: "template-block-meta", text: meta || "" }));
    header.appendChild(left);

    if (isInteractive) {
      const actions = el("div", { className: "template-block-actions" });
      const performSourceControl = typeof createPerformSourceControl === "function"
        ? createPerformSourceControl(obj)
        : null;
      if (performSourceControl) {
        actions.appendChild(performSourceControl);
      }
      const codeBtn = el("button", {
        className: "secondary",
        text: "Code",
        attrs: { type: "button", "data-template-action": "code" }
      });
      codeBtn.addEventListener("click", () => {
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
      actions.appendChild(codeBtn);

      const pathsBtn = el("button", {
        className: "secondary",
        text: "Paths",
        attrs: { type: "button", "data-template-action": "paths" }
      });
      pathsBtn.addEventListener("click", (ev) => {
        if (ev && typeof ev.stopPropagation === "function") {
          ev.stopPropagation();
        }
        openTemplatePathDump(templateContextObj, absIndex, obj);
      });
      actions.appendChild(pathsBtn);

      const copyBtn = el("button", {
        className: "secondary",
        text: "Copy",
        attrs: { type: "button", "data-template-action": "copy" }
      });
      copyBtn.addEventListener("click", async () => {
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
      actions.appendChild(copyBtn);
      header.appendChild(actions);

      block.addEventListener("click", () => {
        if (typeof setSelectedTemplateBlock === "function") {
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
      const openUnifiedEditorModal = (typeof openTemplateCellUnifiedEditModal === "function")
        ? openTemplateCellUnifiedEditModal
        : ((typeof window !== "undefined" && typeof window.openTemplateCellUnifiedEditModal === "function")
          ? window.openTemplateCellUnifiedEditModal
          : null);
      if (!openUnifiedEditorModal) {
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

      openUnifiedEditorModal({
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
          onSaveDesc: ({ decl, text: nextDesc, skipNormalize }) => {
            if (!decl || typeof decl !== "object") {
              return { ok: false, error: "Decl target is unavailable." };
            }
            if (typeof getDeclOverrideStorageKey !== "function" || typeof saveDescOverrides !== "function") {
              return { ok: false, error: "Description override helpers are unavailable." };
            }
            const lookupKeys = getDeclLookupKeysSafe(decl);
            const declKey = lookupKeys.length ? lookupKeys[0] : getDeclOverrideStorageKey(decl);
            if (!declKey) {
              return { ok: false, error: "Decl key is unavailable." };
            }
            const raw = String(nextDesc === undefined || nextDesc === null ? "" : nextDesc);
            const trimmed = raw.trim();
            const stored = skipNormalize ? trimmed : stripDeclCategoryPrefix(trimmed);
            state.pendingTemplateViewportAnchor = captureTemplateViewportAnchor({
              templateIndex: absIndex,
              gridRow: Number(cellElement && cellElement.getAttribute("data-template-grid-row")),
              gridCol: Number(cellElement && cellElement.getAttribute("data-template-grid-col"))
            });
            for (const key of lookupKeys.length ? lookupKeys : [declKey]) {
              delete state.descOverrides[key];
              if (state.descOverridesLegacy && typeof state.descOverridesLegacy === "object") {
                delete state.descOverridesLegacy[key];
              }
            }
            if (stored) {
              state.descOverrides[declKey] = skipNormalize ? { text: stored, noNormalize: true } : stored;
            }
            const saved = saveDescOverrides();
            if (saved === false) {
              state.pendingTemplateViewportAnchor = null;
              return { ok: false, error: "Failed to persist description override." };
            }
            if (typeof renderOutput === "function") {
              renderOutput();
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

    if (state.selectedTemplateIndex !== "" && typeof setSelectedTemplateBlock === "function") {
      setSelectedTemplateBlock(state.selectedTemplateIndex, { scroll: false, ensure: false });
    }
    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
  }

  async function copyAllTemplateBlocks() {
    const virtual = getTemplateVirtualState();
    const items = Array.isArray(virtual.items) && virtual.items.length
      ? virtual.items
      : getRenderableObjectListForTemplate();
    if (!items.length) {
      setError("Nothing to copy.");
      return;
    }

    const config = virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : (state.templateConfig && typeof state.templateConfig === "object"
        ? state.templateConfig
        : getDefaultTemplateConfig());

    const wrapper = document.createElement("div");
    const plainLines = [];
    const tableOnly = isTemplateCopyTableOnlyEnabled();
    for (let index = 0; index < items.length; index += 1) {
      const payload = buildTemplateBlockCopyPayload(items[index], index, config, tableOnly);
      if (!payload.node) {
        continue;
      }
      wrapper.appendChild(payload.node);
      if (tableOnly) {
        wrapper.appendChild(document.createElement("br"));
      }
      plainLines.push(payload.text);
    }

    if (!wrapper.childNodes.length) {
      setError("Nothing to copy.");
      return;
    }

    await copyHtmlWithFallback(wrapper.innerHTML, plainLines.filter(Boolean).join("\n\n"));
  }

window.AbapViewerModules.factories = window.AbapViewerModules.factories || {};
window.AbapViewerModules.factories["03-template-preview"] = function registerTemplatePreview(runtime) {
  const targetRuntime = runtime || (window.AbapViewerRuntime = window.AbapViewerRuntime || {});
  targetRuntime.api = targetRuntime.api || {};
  targetRuntime.api.renderTemplatePreview = renderTemplatePreview;
  targetRuntime.api.applyTemplateConfigFromEditor = applyTemplateConfigFromEditor;
  window.AbapViewerModules.parts["03-template-preview"] = true;
};
window.AbapViewerModules.factories["03-template-preview"](window.AbapViewerRuntime);
