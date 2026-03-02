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
        const parentLooksDecl = isDeclLikePathSegment(parentAccessKey);
        if (isTemplateDeclLikeValue(current) || parentLooksDecl) {
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

  function resolveExpandedPerformTemplateTraceDecls(ownerContext, decl) {
    if (!decl || typeof decl !== "object") {
      return [];
    }
    if (String(decl.objectType || "").toUpperCase() !== "FORM_PARAM") {
      return [];
    }
    const bindingContext = getExpandedPerformBindingContextForTemplate(ownerContext);
    if (!bindingContext) {
      return [];
    }
    const paramUpper = String(decl.name || "").trim().toUpperCase();
    if (!paramUpper) {
      return [];
    }
    const traceDecls = bindingContext.byParamUpper.get(paramUpper);
    if (!Array.isArray(traceDecls) || !traceDecls.length) {
      return [];
    }
    return dedupeTemplateDecls(traceDecls);
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
    if (!isDeclLikeObject(localDecl) || String(localDecl.objectType || "").toUpperCase() !== "FORM_PARAM") {
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

  function createTemplateCellModel() {
    return {
      text: "",
      style: {},
      hidden: false,
      rowspan: 1,
      colspan: 1,
      hasPlaceholder: false,
      hasTokenValue: false
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
      hasTokenValue: Boolean(cell.hasTokenValue)
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

  function buildTemplateGridModel(obj, templateMap, templateOptions) {
    const map = templateMap && typeof templateMap === "object" ? templateMap : {};
    const options = templateOptions && typeof templateOptions === "object"
      ? templateOptions
      : normalizeTemplatePreviewOptions(null, null, null);
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
      const textMeta = hasText ? resolveTemplateText(cfg.text, obj) : null;
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

  function renderTemplateTable(model) {
    const matrix = model && Array.isArray(model.matrix) ? model.matrix : [];
    if (!matrix.length) {
      return null;
    }
    const options = model && model.options && typeof model.options === "object"
      ? model.options
      : normalizeTemplatePreviewOptions(null, null, null);
    const squareCells = options.squareCells === true;
    const squareCellSize = parseTemplateOptionNumber(options.squareCellSize, 18, 16, 240);

    const table = el("table", {
      className: "template-preview-table",
      attrs: {
        style: "border-collapse:collapse;table-layout:fixed;width:max-content;min-width:100%;"
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

        td.textContent = String(cell.text || "");
        const colSpan = Math.max(1, Number(td.colSpan) || 1);
        const rowSpan = Math.max(1, Number(td.rowSpan) || 1);
        const baseMinWidth = squareCells ? `${squareCellSize}px` : "56px";
        const baseMaxWidth = squareCells ? `${squareCellSize}px` : "360px";
        const baseWidth = squareCells ? `${squareCellSize * colSpan}px` : "";
        const baseMinHeight = squareCells ? `${squareCellSize}px` : "";
        const baseHeight = squareCells ? `${squareCellSize * rowSpan}px` : "";
        const baseTextOverflow = squareCells ? "ellipsis" : "";
        const cssText = toInlineCssText({
          "min-width": baseMinWidth,
          "max-width": baseMaxWidth,
          width: baseWidth,
          "min-height": baseMinHeight,
          height: baseHeight,
          padding: "4px 6px",
          border: "none",
          "vertical-align": "top",
          "white-space": "pre-wrap",
          "text-overflow": baseTextOverflow,
          "box-sizing": "border-box",
          "font-size": "10pt",
          "font-family": "\"MS PGothic\", \"MS UI Gothic\", Meiryo, sans-serif",
          color: "#111111",
          "background-color": "#ffffff",
          ...cell.style
        });
        if (cssText) {
          td.setAttribute("style", cssText);
        }

        tr.appendChild(td);
      }
      tbody.appendChild(tr);
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

      const model = buildTemplateGridModel(templateContextObj, resolved.map, resolved.options);
      if (model.errors.length) {
        block.appendChild(el("div", { className: "template-error", text: model.errors.join("\n") }));
      }

      const table = renderTemplateTable(model);
      if (table) {
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

    const model = buildTemplateGridModel(templateContextObj, resolved.map, resolved.options);
    if (model.errors.length) {
      block.appendChild(el("div", { className: "template-error", text: model.errors.join("\n") }));
    }

    const table = renderTemplateTable(model);
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

  function applyTemplateScrollDelta(delta) {
    if (!els.templatePreviewOutput || Math.abs(Number(delta) || 0) < 1) {
      return;
    }
    const virtual = getTemplateVirtualState();
    virtual.isAdjustingScroll = true;
    const nextTop = Math.max(0, (Number(els.templatePreviewOutput.scrollTop) || 0) + Number(delta || 0));
    els.templatePreviewOutput.scrollTop = nextTop;
    virtual.lastScrollTop = nextTop;
    requestAnimationFrame(() => {
      virtual.isAdjustingScroll = false;
    });
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
    const config = virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : getDefaultTemplateConfig();

    const frag = document.createDocumentFragment();
    const heights = [];
    for (let index = start; index < end; index += 1) {
      const block = buildTemplateBlockElement(items[index], index, config, true);
      if (!block) {
        continue;
      }
      frag.appendChild(block);
      heights.push(measureTemplateOuterHeight(block));
    }

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
    }

    virtual.start = start;
    virtual.end = end;
    renderTemplateVirtualRangeReplace({
      preserveScroll: !(Number.isFinite(selectedIndex) && selectedIndex >= 0) && opts.preserveScroll === true,
      scrollTop: Number(opts.scrollTop) || 0
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
    renderTemplateVirtualRangeReplace({ preserveScroll: false, scrollTop: 0 });
    return true;
  }

  function appendTemplateBatch(batchCount) {
    if (!els.templatePreviewOutput) {
      return 0;
    }
    const virtual = getTemplateVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!total || virtual.end >= total) {
      return 0;
    }
    const count = Math.max(1, Number(batchCount) || 1);
    const oldEnd = virtual.end;
    const newEnd = Math.min(total, oldEnd + count);
    const config = virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : getDefaultTemplateConfig();

    const heights = [];
    for (let index = oldEnd; index < newEnd; index += 1) {
      const block = buildTemplateBlockElement(virtual.items[index], index, config, true);
      if (!block) {
        continue;
      }
      els.templatePreviewOutput.appendChild(block);
      heights.push(measureTemplateOuterHeight(block));
    }
    virtual.end = newEnd;
    updateTemplateAverageHeight(virtual, heights);
    return newEnd - oldEnd;
  }

  function prependTemplateBatch(batchCount) {
    if (!els.templatePreviewOutput) {
      return 0;
    }
    const virtual = getTemplateVirtualState();
    if (virtual.start <= 0) {
      return 0;
    }
    const count = Math.max(1, Number(batchCount) || 1);
    const newStart = Math.max(0, virtual.start - count);
    const config = virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : getDefaultTemplateConfig();

    const frag = document.createDocumentFragment();
    const nodes = [];
    for (let index = newStart; index < virtual.start; index += 1) {
      const block = buildTemplateBlockElement(virtual.items[index], index, config, true);
      if (!block) {
        continue;
      }
      nodes.push(block);
      frag.appendChild(block);
    }
    if (!nodes.length) {
      return 0;
    }

    els.templatePreviewOutput.prepend(frag);
    let addedHeight = 0;
    const heights = [];
    for (const block of nodes) {
      const height = measureTemplateOuterHeight(block);
      heights.push(height);
      addedHeight += height;
    }
    virtual.start = newStart;
    updateTemplateAverageHeight(virtual, heights);
    applyTemplateScrollDelta(addedHeight);
    return nodes.length;
  }

  function dropTemplateTop(dropCount) {
    if (!els.templatePreviewOutput) {
      return 0;
    }
    const virtual = getTemplateVirtualState();
    const count = Math.max(0, Number(dropCount) || 0);
    if (!count) {
      return 0;
    }

    let removed = 0;
    let removedHeight = 0;
    while (removed < count) {
      const first = els.templatePreviewOutput.firstElementChild;
      if (!first) {
        break;
      }
      removedHeight += measureTemplateOuterHeight(first);
      first.remove();
      removed += 1;
    }

    if (!removed) {
      return 0;
    }

    virtual.start = Math.min(virtual.end, virtual.start + removed);
    applyTemplateScrollDelta(-removedHeight);
    return removed;
  }

  function dropTemplateBottom(dropCount) {
    if (!els.templatePreviewOutput) {
      return 0;
    }
    const virtual = getTemplateVirtualState();
    const count = Math.max(0, Number(dropCount) || 0);
    if (!count) {
      return 0;
    }

    let removed = 0;
    while (removed < count) {
      const last = els.templatePreviewOutput.lastElementChild;
      if (!last) {
        break;
      }
      last.remove();
      removed += 1;
    }

    if (!removed) {
      return 0;
    }
    virtual.end = Math.max(virtual.start, virtual.end - removed);
    return removed;
  }

  function processTemplateVirtualScrollFrame() {
    if (!els.templatePreviewOutput) {
      return;
    }
    const virtual = getTemplateVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!virtual.isInitialized || !total || virtual.isAdjustingScroll) {
      return;
    }

    const scrollTop = Number(els.templatePreviewOutput.scrollTop || 0) || 0;
    const prevTop = Number(virtual.lastScrollTop || 0) || 0;
    virtual.scrollDir = scrollTop >= prevTop ? "down" : "up";
    virtual.lastScrollTop = scrollTop;

    const metrics = getTemplateVirtualConfig(els.templatePreviewOutput, virtual.avgItemHeight);
    const nearBottom = scrollTop + Number(els.templatePreviewOutput.clientHeight || 0) >= Number(els.templatePreviewOutput.scrollHeight || 0) - metrics.edgeThresholdPx;
    const nearTop = scrollTop <= metrics.edgeThresholdPx;

    if (virtual.scrollDir === "down") {
      if (nearBottom && virtual.end < total) {
        appendTemplateBatch(metrics.batchCount);
      }
      const rangeCount = virtual.end - virtual.start;
      if (rangeCount > metrics.maxCount) {
        const drop = Math.min(metrics.batchCount, rangeCount - metrics.targetCount);
        dropTemplateTop(drop);
      }
      return;
    }

    if (nearTop && virtual.start > 0) {
      prependTemplateBatch(metrics.batchCount);
    }
    const rangeCount = virtual.end - virtual.start;
    if (rangeCount > metrics.maxCount) {
      const drop = Math.min(metrics.batchCount, rangeCount - metrics.targetCount);
      dropTemplateBottom(drop);
    }
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




