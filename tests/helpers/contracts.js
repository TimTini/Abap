"use strict";

const fs = require("fs");
const path = require("path");

function readJson(filePath) {
  return JSON.parse(fs.readFileSync(filePath, "utf8"));
}

function writeJson(filePath, value) {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  fs.writeFileSync(filePath, JSON.stringify(value, null, 2) + "\n", "utf8");
}

function listFixtureFiles(dirPath) {
  if (!fs.existsSync(dirPath)) {
    return [];
  }
  return fs.readdirSync(dirPath)
    .filter((name) => name.toLowerCase().endsWith(".abap"))
    .sort();
}

function toPlainJson(value) {
  return JSON.parse(JSON.stringify(value));
}

function isDeclLike(value) {
  return Boolean(value)
    && typeof value === "object"
    && typeof value.objectType === "string"
    && typeof value.name === "string";
}

function normalizeDecl(value) {
  if (!isDeclLike(value)) {
    return value;
  }
  return {
    objectType: String(value.objectType || ""),
    name: String(value.name || ""),
    file: String(value.file || ""),
    lineStart: Number(value.lineStart || 0) || 0,
    raw: String(value.raw || ""),
    comment: String(value.comment || ""),
    scopeLabel: String(value.scopeLabel || ""),
    scopeType: String(value.scopeType || ""),
    scopeName: String(value.scopeName || ""),
    structName: String(value.structName || ""),
    structObjectType: String(value.structObjectType || ""),
    structLineStart: Number(value.structLineStart || 0) || 0,
    fieldPath: String(value.fieldPath || ""),
    traceFile: String(value.traceFile || ""),
    traceLineStart: Number(value.traceLineStart || 0) || 0,
    synthetic: value.synthetic === true
  };
}

function normalizeAny(value) {
  if (Array.isArray(value)) {
    return value.map((entry) => normalizeAny(entry));
  }
  if (!value || typeof value !== "object") {
    return value;
  }
  if (isDeclLike(value)) {
    return normalizeDecl(value);
  }
  const out = {};
  for (const key of Object.keys(value).sort()) {
    if (key === "id" || key === "parent") {
      continue;
    }
    out[key] = normalizeAny(value[key]);
  }
  return out;
}

function normalizeParserResult(result) {
  return {
    file: String(result && result.file ? result.file : ""),
    objects: normalizeAny(Array.isArray(result && result.objects) ? result.objects : []),
    decls: normalizeAny(Array.isArray(result && result.decls) ? result.decls : [])
  };
}

function mapEntriesToArray(mapLike) {
  if (!(mapLike instanceof Map)) {
    return [];
  }
  return Array.from(mapLike.entries())
    .map(([key, value]) => [key, normalizeAny(value)])
    .sort((left, right) => String(left[0]).localeCompare(String(right[0])));
}

function extractTextContentList(root, selector, limit) {
  const nodes = root ? Array.from(root.querySelectorAll(selector)) : [];
  return nodes.slice(0, limit).map((node) => String(node.textContent || "").trim());
}

function offsetToLineColumn(text, offset) {
  const value = String(text || "");
  const target = Math.max(0, Math.min(value.length, Number(offset) || 0));
  let line = 1;
  let column = 1;
  for (let index = 0; index < target; index += 1) {
    if (value[index] === "\n") {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }
  return { line, column };
}

function normalizeViewerState(window) {
  const runtime = window && window.AbapViewerRuntime ? window.AbapViewerRuntime : {};
  const state = runtime.state || {};
  const els = runtime.els || {};
  const inputText = els.inputText ? String(els.inputText.value || "") : "";
  const selectionStart = els.inputText ? Number(els.inputText.selectionStart || 0) || 0 : 0;
  const selectionEnd = els.inputText ? Number(els.inputText.selectionEnd || 0) || 0 : 0;

  const data = state.data && typeof state.data === "object" ? state.data : {};
  const renderObjects = Array.isArray(state.renderObjects) ? state.renderObjects : [];
  const declRows = els.declDescTable
    ? Array.from(els.declDescTable.querySelectorAll("tbody tr")).map((row) => {
        const cells = Array.from(row.querySelectorAll("td"));
        return {
          declKey: String(row.getAttribute("data-decl-key") || ""),
          lineStart: Number(row.getAttribute("data-line-start") || 0) || 0,
          selected: row.classList.contains("desc-selected"),
          type: cells[0] ? String(cells[0].textContent || "").trim() : "",
          scope: cells[1] ? String(cells[1].textContent || "").trim() : "",
          techId: cells[2] ? String(cells[2].textContent || "").trim() : "",
          codeDesc: cells[3] ? String(cells[3].textContent || "").trim() : "",
          userDesc: cells[4] ? String(cells[4].textContent || "").trim() : ""
        };
      })
    : [];

  const outputCards = els.output
    ? Array.from(els.output.querySelectorAll(".card[data-id]")).map((card) => ({
        id: String(card.getAttribute("data-id") || ""),
        lineStart: Number(card.getAttribute("data-line-start") || 0) || 0,
        selected: card.classList.contains("selected"),
        title: String((card.querySelector(".card-head") && card.querySelector(".card-head").textContent) || "").trim()
      }))
    : [];

  const templateBlocks = els.templatePreviewOutput
    ? Array.from(els.templatePreviewOutput.querySelectorAll(".template-block[data-template-index]")).map((block) => ({
        index: String(block.getAttribute("data-template-index") || ""),
        lineStart: Number(block.getAttribute("data-line-start") || 0) || 0,
        selected: block.classList.contains("selected"),
        textPreview: extractTextContentList(block, "td", 6).join(" | ")
      }))
    : [];

  return {
    errorText: els.error ? String(els.error.textContent || "").trim() : "",
    rightTab: String(state.rightTab || ""),
    dataSummary: {
      objectCount: Array.isArray(data.objects) ? data.objects.length : 0,
      declCount: Array.isArray(data.decls) ? data.decls.length : 0,
      renderObjectCount: renderObjects.length,
      firstObjectTypes: renderObjects.slice(0, 12).map((obj) => String(obj && obj.objectType ? obj.objectType : ""))
    },
    input: {
      lineCount: Number(state.inputLineCount || 0) || 0,
      selectionStart,
      selectionEnd,
      selectionStartLine: offsetToLineColumn(inputText, selectionStart).line,
      selectionEndLine: offsetToLineColumn(inputText, selectionEnd).line,
      selectedText: inputText.slice(selectionStart, selectionEnd)
    },
    selectedState: {
      selectedId: String(state.selectedId || ""),
      selectedTemplateIndex: String(state.selectedTemplateIndex || ""),
      selectedDeclKey: String(state.selectedDeclKey || "")
    },
    outputCards,
    templateBlocks,
    descriptionSummary: els.declDescSummary ? String(els.declDescSummary.textContent || "").trim() : "",
    descriptionRows: declRows.slice(0, 20),
    outputLineTargets: mapEntriesToArray(state.outputVirtual && state.outputVirtual.lineTargetMap),
    templateLineTargets: mapEntriesToArray(state.templateVirtual && state.templateVirtual.lineTargetMap),
    traceDecls: normalizeAny(
      (Array.isArray(data.decls) ? data.decls : []).filter((decl) => String(decl && decl.traceFile ? decl.traceFile : "").trim())
    )
  };
}

function escapeJsonPointerSegment(value) {
  return String(value).replace(/~/g, "~0").replace(/\//g, "~1");
}

function buildDiffs(expected, actual, basePath, diffs) {
  const pathPrefix = basePath || "";
  if (expected === actual) {
    return;
  }
  if (Array.isArray(expected) && Array.isArray(actual)) {
    const max = Math.max(expected.length, actual.length);
    for (let index = 0; index < max; index += 1) {
      buildDiffs(expected[index], actual[index], `${pathPrefix}/${index}`, diffs);
    }
    return;
  }
  const expectedIsObject = expected && typeof expected === "object";
  const actualIsObject = actual && typeof actual === "object";
  if (expectedIsObject && actualIsObject && !Array.isArray(expected) && !Array.isArray(actual)) {
    const keys = new Set([...Object.keys(expected), ...Object.keys(actual)]);
    for (const key of Array.from(keys).sort()) {
      buildDiffs(expected[key], actual[key], `${pathPrefix}/${escapeJsonPointerSegment(key)}`, diffs);
    }
    return;
  }
  diffs.push({
    path: pathPrefix || "/",
    expected: expected === undefined ? "__undefined__" : expected,
    actual: actual === undefined ? "__undefined__" : actual
  });
}

function diffJson(expected, actual) {
  const diffs = [];
  buildDiffs(expected, actual, "", diffs);
  return diffs;
}

function filterDiffsByAllowedPaths(diffs, allowedPaths) {
  const allow = new Set(Array.isArray(allowedPaths) ? allowedPaths : []);
  return diffs.filter((entry) => !allow.has(entry.path));
}

module.exports = {
  diffJson,
  filterDiffsByAllowedPaths,
  listFixtureFiles,
  normalizeParserResult,
  normalizeViewerState,
  readJson,
  toPlainJson,
  writeJson
};
