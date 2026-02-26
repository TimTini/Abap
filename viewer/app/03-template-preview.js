"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};

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

  function resolveTemplatePathValue(root, pathExpression) {
    const segments = parseTemplatePathSegments(pathExpression);
    if (!segments) {
      return undefined;
    }

    let current = root;
    for (const segment of segments) {
      if (typeof segment === "number") {
        if (!Array.isArray(current)) {
          return undefined;
        }
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
            if (isDeclLikeObject(item)) {
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
        if (isDeclLikeObject(current)) {
          return getFinalDeclDesc(current);
        }
        if (hasValueLevelDescFields(current) || isDeclLikeObject(current.decl)) {
          return resolveValueLevelFinalDesc(current);
        }
      }

      if (!Object.prototype.hasOwnProperty.call(current, key)) {
        return undefined;
      }
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

      const out = {};
      for (const key of Object.keys(normalized)) {
        out[key] = cloneRecursive(
          normalized[key],
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
      const borderLine = "1px solid #c8c8c8";
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

    return {
      hideEmptyRows,
      hideRowsWithoutValues,
      expandMultilineRows
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
      errors
    };
  }

  function renderTemplateTable(model) {
    const matrix = model && Array.isArray(model.matrix) ? model.matrix : [];
    if (!matrix.length) {
      return null;
    }

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
        const cssText = toInlineCssText({
          "min-width": "56px",
          "max-width": "360px",
          padding: "4px 6px",
          border: "1px solid #d3d3d3",
          "vertical-align": "top",
          "white-space": "pre-wrap",
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
          const clone = block.cloneNode(true);
          const actionButtons = clone.querySelectorAll("[data-template-action]");
          for (const actionBtn of Array.from(actionButtons)) {
            actionBtn.remove();
          }
          await copyHtmlWithFallback(clone.outerHTML, buildTemplatePlainTextFromBlock(clone));
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
    for (const block of blocks) {
      const clone = block.cloneNode(true);
      const actionButtons = clone.querySelectorAll("[data-template-action]");
      for (const actionBtn of Array.from(actionButtons)) {
        actionBtn.remove();
      }
      wrapper.appendChild(clone);
      plainLines.push(buildTemplatePlainTextFromBlock(clone));
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


