"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};

  var PERFORM_TRACE_META_KEY_TEMPLATE = "__abapPerformTraceBinding";

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

  function resolveTemplatePathValue(root, pathExpression) {
    const segments = parseTemplatePathSegments(pathExpression);
    if (!segments) {
      return undefined;
    }

    let current = root;
    let parent = null;
    let parentAccessKey = "";
    for (const segment of segments) {
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
        for (const item of current) {
          if (!item || typeof item !== "object") {
            continue;
          }

          const keyLower = key.toLowerCase();
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

  function normalizeTemplateEntryForPath(value, keyHint, pathParts, ownerContext) {
    if (typeof normalizeEntryObjectForXml !== "function") {
      return value;
    }
    try {
      return normalizeEntryObjectForXml(value, keyHint, pathParts, ownerContext);
    } catch {
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

    const existingOrigins = Array.isArray(value.originDecls) ? value.originDecls : [];
    const originDecls = dedupeTemplateDecls([localDecl, ...externalTraceDecls, ...existingOrigins]);
    return {
      ...value,
      decl: externalTraceDecls[0],
      originDecls
    };
  }

  function buildTemplateContextObject(obj, objectIndexOneBased) {
    const objectIndex = Number(objectIndexOneBased) || 1;
    const basePathParts = ["objects", `object[${objectIndex}]`];

    const cloneRecursive = (value, keyHint, pathParts, ownerContext) => {
      if (value === null || value === undefined) {
        return value;
      }
      if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
        return value;
      }

      if (Array.isArray(value)) {
        const itemTag = getTemplateArrayItemTagName(keyHint);
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
      const normalized = normalizeTemplateEntryForPath(value, keyHint, pathParts, nextOwnerContext);
      if (!normalized || typeof normalized !== "object") {
        return normalized;
      }
      const remappedForTrace = remapTemplateDeclForExpandedPerform(normalized, nextOwnerContext);

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

    return cloneRecursive(obj, "object", basePathParts, obj);
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
        .filter((text) => text !== "")
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
      meta: cell && cell.meta && typeof cell.meta === "object" ? { ...cell.meta } : null
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
      const cellMeta = hasText
        ? {
            rangeKey: String(entry.rangeKey || ""),
            templateKey: modelTemplateKey,
            rawText,
            isSinglePlaceholder: Boolean(placeholderToken),
            placeholderToken,
            objectType: modelObjectType
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
          : "border-collapse:collapse;table-layout:fixed;width:max-content;min-width:100%;"
      }
    });
    const tbody = el("tbody");

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
          td.tabIndex = 0;
          td.setAttribute("role", "button");
          td.setAttribute("title", "Double-click to edit this template cell");
          td.setAttribute("aria-label", "Edit template cell");
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
    return table;
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
      const keyText = resolved.key ? `template=${resolved.key}` : "template=missing";
      left.appendChild(el("div", { className: "template-block-meta", text: [meta, keyText].filter(Boolean).join(" • ") }));
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
      table.setAttribute("data-template-key", String(resolved.key || ""));
      table.setAttribute("data-object-type", String(obj.objectType || ""));
      table.setAttribute("data-template-index", indexText);
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
    applyTemplateConfigObject(getDefaultTemplateConfig(), { save: true });
  }

  function exportTemplateConfig() {
    const config = state.templateConfig || getDefaultTemplateConfig();
    const content = safeJson(config, true);
    const fileName = "abap-template-config.json";

    try {
      const blob = new Blob([content], { type: "application/json" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = fileName;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
      setError("");
    } catch (err) {
      setError(`Export failed: ${err && err.message ? err.message : err}`);
    }
  }

  async function importTemplateConfigFromFile(file) {
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
      const applied = applyTemplateConfigObject(parsed, { save: true });
      if (applied) {
        setError("");
      }
    } catch (err) {
      setTemplateConfigError(`Import JSON parse error: ${err && err.message ? err.message : err}`);
    }
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
        lineTargetMap: new Map(),
        isInitialized: false
      };
    }
    if (!(state.templateVirtual.lineTargetMap instanceof Map)) {
      state.templateVirtual.lineTargetMap = new Map();
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
    return Math.max(0, rect.height + marginTop + marginBottom);
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
    const keyText = resolved.key ? `template=${resolved.key}` : "template=missing";
    left.appendChild(el("div", { className: "template-block-meta", text: [meta, keyText].filter(Boolean).join(" • ") }));
    header.appendChild(left);

    if (isInteractive) {
      const actions = el("div", { className: "template-block-actions" });
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

    const openCellUnifiedEditor = (cellMeta) => {
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

      const getDescOverrideEntrySafe = (declKey) => {
        if (!declKey) {
          return null;
        }
        if (typeof getDescOverrideEntry === "function") {
          return getDescOverrideEntry(declKey);
        }
        const rawOverride = state && state.descOverrides ? state.descOverrides[declKey] : null;
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

      let targetDeclCandidates = [];
      if (token) {
        try {
          const resolvedValue = resolveTemplatePlaceholderValue(templateContextObj, token);
          targetDeclCandidates = getTemplateEditableDeclCandidatesFromResolvedValue(resolvedValue);
          if (!targetDeclCandidates.length) {
            targetDeclCandidates = resolveTemplateEditableDeclCandidatesFromToken(templateContextObj, token);
          }
        } catch {
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
        try {
          declKey = typeof getDeclOverrideStorageKey === "function" ? getDeclOverrideStorageKey(decl) : "";
        } catch {
          declKey = "";
        }
        const descEntry = getDescOverrideEntrySafe(declKey);
        const techName = typeof getDeclTechName === "function" ? getDeclTechName(decl) : String(decl && decl.name ? decl.name : "");
        const scopeName = String(decl && decl.scopeLabel ? decl.scopeLabel : "").trim();
        const label = scopeName ? (techName || "(unknown)") + " @ " + scopeName : (techName || "(unknown)");
        modalDeclCandidates.push({
          decl,
          declKey,
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
          declCandidates: modalDeclCandidates,
          onSaveDesc: ({ decl, text: nextDesc, skipNormalize }) => {
            if (!decl || typeof decl !== "object") {
              return { ok: false, error: "Decl target is unavailable." };
            }
            if (typeof getDeclOverrideStorageKey !== "function" || typeof saveDescOverrides !== "function") {
              return { ok: false, error: "Description override helpers are unavailable." };
            }
            const declKey = getDeclOverrideStorageKey(decl);
            if (!declKey) {
              return { ok: false, error: "Decl key is unavailable." };
            }
            const raw = String(nextDesc === undefined || nextDesc === null ? "" : nextDesc);
            const trimmed = raw.trim();
            const stored = skipNormalize ? trimmed : stripDeclCategoryPrefix(trimmed);
            if (!stored) {
              delete state.descOverrides[declKey];
            } else {
              state.descOverrides[declKey] = skipNormalize ? { text: stored, noNormalize: true } : stored;
            }
            const saved = saveDescOverrides();
            if (saved === false) {
              return { ok: false, error: "Failed to persist description override." };
            }
            state.haystackById = buildSearchIndex(state.renderObjects);
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
      onCellDblClick: (cellMeta) => {
        if (!cellMeta || typeof cellMeta !== "object") {
          return;
        }
        openCellUnifiedEditor(cellMeta);
      }
    } : null);
    if (table) {
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
    const firstVisible = Math.max(0, Math.floor(top / estimatedHeight));
    let start = Math.max(0, firstVisible - metrics.overscanCount);
    let end = Math.min(total, start + metrics.targetCount);
    if ((end - start) < metrics.targetCount) {
      start = Math.max(0, end - metrics.targetCount);
    }

    return { start, end };
  }

  function renderTemplateVirtualRangeReplace(options) {
    if (!els.templatePreviewOutput) {
      return;
    }

    const opts = options && typeof options === "object" ? options : {};
    const virtual = getTemplateVirtualState();
    const items = Array.isArray(virtual.items) ? virtual.items : [];
    const total = items.length;
    const start = Math.max(0, Math.min(total, Number(virtual.start) || 0));
    const end = Math.max(start, Math.min(total, Number(virtual.end) || 0));
    const estimatedHeight = getTemplateEstimatedItemHeight(virtual);
    const config = virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : getDefaultTemplateConfig();

    const frag = document.createDocumentFragment();
    const topSpacer = document.createElement("div");
    topSpacer.className = "virtual-spacer template-virtual-spacer-top";
    topSpacer.style.height = String(Math.max(0, Math.round(start * estimatedHeight))) + "px";
    topSpacer.setAttribute("aria-hidden", "true");
    frag.appendChild(topSpacer);

    const heights = [];
    for (let index = start; index < end; index += 1) {
      const block = buildTemplateBlockElement(items[index], index, config, true);
      if (!block) {
        continue;
      }
      frag.appendChild(block);
      heights.push(measureTemplateOuterHeight(block));
    }

    const bottomSpacer = document.createElement("div");
    bottomSpacer.className = "virtual-spacer template-virtual-spacer-bottom";
    bottomSpacer.style.height = String(Math.max(0, Math.round((total - end) * estimatedHeight))) + "px";
    bottomSpacer.setAttribute("aria-hidden", "true");
    frag.appendChild(bottomSpacer);

    els.templatePreviewOutput.classList.remove("muted");
    els.templatePreviewOutput.replaceChildren(frag);
    updateTemplateAverageHeight(virtual, heights);

    if (opts.preserveScroll) {
      const maxTop = Math.max(0, Number(els.templatePreviewOutput.scrollHeight || 0) - Number(els.templatePreviewOutput.clientHeight || 0));
      els.templatePreviewOutput.scrollTop = Math.max(0, Math.min(maxTop, Number(opts.scrollTop) || 0));
    } else {
      els.templatePreviewOutput.scrollTop = Math.max(0, Number(opts.scrollTop) || 0);
    }
    virtual.lastScrollTop = Number(els.templatePreviewOutput.scrollTop || 0) || 0;
  }

  function initTemplateVirtualWindow(items, config, options) {
    const opts = options && typeof options === "object" ? options : {};
    const virtual = getTemplateVirtualState();
    const list = Array.isArray(items) ? items : [];

    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }

    virtual.items = list;
    virtual.itemCount = list.length;
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
    if (Number.isFinite(selectedIndex) && selectedIndex >= 0 && selectedIndex < total) {
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
    const targetTop = Math.max(0, Math.round(start * getTemplateEstimatedItemHeight(virtual)));
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
    if (virtual.pendingRaf) {
      return;
    }
    virtual.pendingRaf = requestAnimationFrame(() => {
      virtual.pendingRaf = 0;
      processTemplateVirtualScrollFrame();
    });
  }

  function handleTemplateVirtualScroll() {
    scheduleTemplateVirtualScroll();
  }

function resetTemplateVirtualState() {
    const virtual = getTemplateVirtualState();
    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    virtual.items = [];
    virtual.itemCount = 0;
    virtual.start = 0;
    virtual.end = 0;
    virtual.lastScrollTop = 0;
    virtual.scrollDir = "down";
    virtual.isAdjustingScroll = false;
    virtual.lineTargetMap = new Map();
    virtual.isInitialized = false;
  }

  function renderTemplatePreview() {
    if (!els.templatePreviewOutput) {
      return;
    }

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
