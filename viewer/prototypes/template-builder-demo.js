"use strict";

(function () {
  const STORAGE_KEY = "abap-template-builder-demo.v1";
  const STYLE_KEYS = [
    "background",
    "border",
    "font color",
    "font size",
    "align",
    "valign",
    "wrap",
    "merge",
    "bold"
  ];
  const DEFAULT_GRID = Object.freeze({ rows: 10, cols: 12 });
  const TOKEN_PRESETS = Object.freeze([
    "{keywords.stmt.text}",
    "{values.name.decl.name}",
    "{values.name.finalDesc}",
    "{values.form.finalDesc}",
    "{values.usingRaw}",
    "{values.changingRaw}",
    "{values.tablesRaw}",
    "{values.raisingRaw}",
    "{extras.form.params[0].name}",
    "{extras.form.params[0].type}",
    "{extras.form.exceptions[0].name}",
    "{extras.performCall.using[0].value}",
    "{extras.ifCondition.conditions[0].left.finalDesc}"
  ]);

  const els = {};
  const state = loadState();

  document.addEventListener("DOMContentLoaded", init);

  function init() {
    bindElements();
    bindEvents();
    ensureTemplate(state.activeTemplateKey);
    populateTokenPresets();
    renderAll();
    setStatus("Demo loaded. Main viewer is untouched.", "success");
  }

  function bindElements() {
    const ids = [
      "templateKeySelect",
      "starterPresetSelect",
      "newTemplateKeyInput",
      "createTemplateBtn",
      "resetCurrentTemplateBtn",
      "gridRowsInput",
      "gridColsInput",
      "applyGridBtn",
      "loadStarterBtn",
      "copyJsonBtn",
      "downloadJsonBtn",
      "openJsonFileBtn",
      "jsonFileInput",
      "selectionLabel",
      "previewGrid",
      "cellTextInput",
      "tokenPresetSelect",
      "insertTokenBtn",
      "backgroundInput",
      "fontColorInput",
      "borderSelect",
      "fontSizeInput",
      "alignSelect",
      "valignSelect",
      "wrapCheckbox",
      "boldCheckbox",
      "mergeCheckbox",
      "applyRangeBtn",
      "clearRangeStyleBtn",
      "clearAnchorTextBtn",
      "hideEmptyRowsCheckbox",
      "hideRowsWithoutValuesCheckbox",
      "expandMultilineRowsCheckbox",
      "jsonOutput",
      "applyJsonBtn",
      "formatJsonBtn",
      "statusBar"
    ];

    for (const id of ids) {
      els[id] = document.getElementById(id);
    }
  }

  function bindEvents() {
    els.templateKeySelect.addEventListener("change", () => {
      state.activeTemplateKey = String(els.templateKeySelect.value || "").trim().toUpperCase() || "FORM";
      ensureTemplate(state.activeTemplateKey);
      renderAll();
      persistState();
    });

    els.createTemplateBtn.addEventListener("click", handleCreateTemplate);
    els.resetCurrentTemplateBtn.addEventListener("click", handleResetCurrentTemplate);

    els.applyGridBtn.addEventListener("click", () => {
      const rows = clampInteger(els.gridRowsInput.value, 4, 40, DEFAULT_GRID.rows);
      const cols = clampInteger(els.gridColsInput.value, 4, 26, DEFAULT_GRID.cols);
      state.grid = { rows, cols };
      state.selection = createSingleCellSelection(1, 1);
      renderAll();
      persistState();
      setStatus("Grid size updated.", "success");
    });

    els.loadStarterBtn.addEventListener("click", () => {
      const preset = String(els.starterPresetSelect.value || "FORM").trim().toUpperCase();
      state.templates[state.activeTemplateKey] = buildStarterTemplate(preset);
      renderAll();
      persistState();
      setStatus(`Loaded ${preset} starter into ${state.activeTemplateKey}.`, "success");
    });

    els.insertTokenBtn.addEventListener("click", () => {
      const token = String(els.tokenPresetSelect.value || "").trim();
      if (token) {
        insertTextAtCursor(els.cellTextInput, token);
      }
    });

    els.applyRangeBtn.addEventListener("click", applyInspectorToSelection);
    els.clearRangeStyleBtn.addEventListener("click", clearSelectedRangeStyle);
    els.clearAnchorTextBtn.addEventListener("click", clearSelectedAnchorText);

    els.hideEmptyRowsCheckbox.addEventListener("change", applyTemplateOptionsFromUi);
    els.hideRowsWithoutValuesCheckbox.addEventListener("change", applyTemplateOptionsFromUi);
    els.expandMultilineRowsCheckbox.addEventListener("change", applyTemplateOptionsFromUi);

    els.copyJsonBtn.addEventListener("click", copyJsonToClipboard);
    els.downloadJsonBtn.addEventListener("click", downloadJsonFile);
    els.openJsonFileBtn.addEventListener("click", () => els.jsonFileInput.click());
    els.jsonFileInput.addEventListener("change", importJsonFile);
    els.applyJsonBtn.addEventListener("click", applyJsonFromEditor);
    els.formatJsonBtn.addEventListener("click", () => {
      els.jsonOutput.value = stringifyConfig(buildConfigObject());
      setStatus("JSON formatted.", "success");
    });

    document.addEventListener("mouseup", () => {
      state.dragging = false;
      state.dragOrigin = null;
    });
  }

  function handleCreateTemplate() {
    const rawKey = String(els.newTemplateKeyInput.value || "").trim();
    if (!rawKey) {
      setStatus("Template key is empty.", "error");
      return;
    }
    const key = rawKey.toUpperCase();
    const preset = String(els.starterPresetSelect.value || "FORM").trim().toUpperCase();
    state.templates[key] = buildStarterTemplate(preset);
    state.activeTemplateKey = key;
    els.newTemplateKeyInput.value = "";
    renderAll();
    persistState();
    setStatus(`Created ${key} from ${preset} starter.`, "success");
  }

  function handleResetCurrentTemplate() {
    const current = state.activeTemplateKey;
    if (!current) {
      return;
    }
    const preset = String(els.starterPresetSelect.value || current).trim().toUpperCase();
    state.templates[current] = buildStarterTemplate(preset);
    renderAll();
    persistState();
    setStatus(`Reset ${current} with ${preset} starter.`, "success");
  }

  function populateTokenPresets() {
    els.tokenPresetSelect.replaceChildren();
    for (const token of TOKEN_PRESETS) {
      const option = document.createElement("option");
      option.value = token;
      option.textContent = token;
      els.tokenPresetSelect.appendChild(option);
    }
  }

  function renderAll() {
    ensureTemplate(state.activeTemplateKey);
    syncToolbar();
    renderSelectionLabel();
    renderPreviewGrid();
    syncInspectorFromSelection();
    renderJsonOutput();
  }

  function syncToolbar() {
    const keys = Object.keys(state.templates).sort((a, b) => a.localeCompare(b));
    els.templateKeySelect.replaceChildren();
    for (const key of keys) {
      const option = document.createElement("option");
      option.value = key;
      option.textContent = key;
      option.selected = key === state.activeTemplateKey;
      els.templateKeySelect.appendChild(option);
    }
    if (!keys.includes(state.activeTemplateKey)) {
      state.activeTemplateKey = keys[0] || "FORM";
    }
    els.gridRowsInput.value = String(state.grid.rows);
    els.gridColsInput.value = String(state.grid.cols);
    els.starterPresetSelect.value = pickStarterPresetForTemplateKey(state.activeTemplateKey);
  }

  function renderSelectionLabel() {
    els.selectionLabel.textContent = selectionToRangeKey(state.selection);
  }

  function renderPreviewGrid() {
    els.previewGrid.replaceChildren();

    const headRow = document.createElement("tr");
    headRow.appendChild(makeHeaderCell(""));
    for (let col = 1; col <= state.grid.cols; col += 1) {
      headRow.appendChild(makeHeaderCell(columnNumberToLabel(col)));
    }
    els.previewGrid.appendChild(headRow);

    const mergeMap = buildMergeMap(getCurrentTemplate());
    for (let row = 1; row <= state.grid.rows; row += 1) {
      const tr = document.createElement("tr");
      tr.appendChild(makeHeaderCell(String(row)));

      for (let col = 1; col <= state.grid.cols; col += 1) {
        const key = makeCellKey(row, col);
        const coverInfo = mergeMap.covered.get(key);
        if (coverInfo && !coverInfo.isAnchor) {
          continue;
        }
        const displayRange = coverInfo && coverInfo.rangeKey
          ? parseRangeKey(coverInfo.rangeKey)
          : createSingleCellSelection(row, col);

        const td = document.createElement("td");
        td.dataset.startRow = String(displayRange.startRow);
        td.dataset.startCol = String(displayRange.startCol);
        td.dataset.endRow = String(displayRange.endRow);
        td.dataset.endCol = String(displayRange.endCol);
        td.addEventListener("mousedown", onPreviewCellMouseDown);
        td.addEventListener("mouseenter", onPreviewCellMouseEnter);
        if (coverInfo && coverInfo.rangeKey) {
          td.colSpan = coverInfo.colSpan;
          td.rowSpan = coverInfo.rowSpan;
        }
        if (rangesOverlap(state.selection, displayRange)) {
          td.classList.add("in-selection");
        }
        if (isSelectionAnchor(displayRange.startRow, displayRange.startCol)) {
          td.classList.add("anchor");
        }

        const cell = resolvePreviewCell(row, col);
        applyPreviewStyle(td, cell.style);
        td.innerHTML = formatPreviewText(cell.text);
        tr.appendChild(td);
      }

      els.previewGrid.appendChild(tr);
    }
  }

  function syncInspectorFromSelection() {
    const template = getCurrentTemplate();
    const rangeKey = selectionToRangeKey(state.selection);
    const anchorKey = makeCellKey(state.selection.startRow, state.selection.startCol);
    const rangeEntry = getTemplateEntry(template, rangeKey);
    const anchorEntry = getTemplateEntry(template, anchorKey);
    const styleSource = rangeKey === anchorKey ? anchorEntry : rangeEntry;

    els.cellTextInput.value = String(anchorEntry && anchorEntry.text ? anchorEntry.text : "");
    els.backgroundInput.value = normalizeColorInput(styleSource && styleSource.background, "#ffffff");
    els.fontColorInput.value = normalizeColorInput(styleSource && styleSource["font color"], "#111111");
    els.borderSelect.value = String(styleSource && styleSource.border ? styleSource.border : "");
    els.fontSizeInput.value = String(clampInteger(styleSource && styleSource["font size"], 8, 32, 10));
    els.alignSelect.value = String(styleSource && styleSource.align ? styleSource.align : "left");
    els.valignSelect.value = String(styleSource && styleSource.valign ? styleSource.valign : "top");
    els.wrapCheckbox.checked = Boolean(styleSource && styleSource.wrap);
    els.boldCheckbox.checked = Boolean(styleSource && styleSource.bold);
    els.mergeCheckbox.checked = Boolean(styleSource && styleSource.merge);

    const options = template._options || {};
    els.hideEmptyRowsCheckbox.checked = options.hideEmptyRows !== false;
    els.hideRowsWithoutValuesCheckbox.checked = options.hideRowsWithoutValues !== false;
    els.expandMultilineRowsCheckbox.checked = Boolean(options.expandMultilineRows);
  }

  function renderJsonOutput() {
    els.jsonOutput.value = stringifyConfig(buildConfigObject());
  }

  function onPreviewCellMouseDown(event) {
    startSelectionFromRange(readRangeFromDataset(event.currentTarget.dataset));
    renderAll();
  }

  function onPreviewCellMouseEnter(event) {
    if (!state.dragging) {
      return;
    }
    extendSelectionWithRange(readRangeFromDataset(event.currentTarget.dataset));
    renderAll();
  }

  function startSelectionFromRange(range) {
    const normalized = normalizeSelection(range);
    state.dragging = true;
    state.dragOrigin = normalized;
    state.selection = normalized;
  }

  function extendSelectionWithRange(range) {
    const normalized = normalizeSelection(range);
    const origin = state.dragOrigin || state.selection;
    state.selection = normalizeSelection({
      startRow: Math.min(origin.startRow, normalized.startRow),
      startCol: Math.min(origin.startCol, normalized.startCol),
      endRow: Math.max(origin.endRow, normalized.endRow),
      endCol: Math.max(origin.endCol, normalized.endCol)
    });
  }

  function readRangeFromDataset(dataset) {
    return normalizeSelection({
      startRow: Number(dataset && dataset.startRow) || 1,
      startCol: Number(dataset && dataset.startCol) || 1,
      endRow: Number(dataset && dataset.endRow) || Number(dataset && dataset.startRow) || 1,
      endCol: Number(dataset && dataset.endCol) || Number(dataset && dataset.startCol) || 1
    });
  }

  function applyInspectorToSelection() {
    const template = getCurrentTemplate();
    const rangeKey = selectionToRangeKey(state.selection);
    const anchorKey = makeCellKey(state.selection.startRow, state.selection.startCol);
    const styleTargetKey = rangeKey === anchorKey ? anchorKey : rangeKey;
    const styleEntry = { ...(getTemplateEntry(template, styleTargetKey) || {}) };

    styleEntry.background = els.backgroundInput.value;
    styleEntry["font color"] = els.fontColorInput.value;
    styleEntry.border = String(els.borderSelect.value || "");
    styleEntry["font size"] = clampInteger(els.fontSizeInput.value, 8, 32, 10);
    styleEntry.align = String(els.alignSelect.value || "left");
    styleEntry.valign = String(els.valignSelect.value || "top");
    styleEntry.wrap = Boolean(els.wrapCheckbox.checked);
    styleEntry.bold = Boolean(els.boldCheckbox.checked);
    styleEntry.merge = Boolean(els.mergeCheckbox.checked && rangeKey !== anchorKey);
    upsertTemplateEntry(template, styleTargetKey, styleEntry);

    const text = String(els.cellTextInput.value || "");
    const anchorEntry = { ...(getTemplateEntry(template, anchorKey) || {}) };
    if (text.trim()) {
      anchorEntry.text = text;
    } else {
      delete anchorEntry.text;
    }
    upsertTemplateEntry(template, anchorKey, anchorEntry);

    persistState();
    renderAll();
    setStatus(`Applied changes to ${rangeKey}.`, "success");
  }

  function clearSelectedRangeStyle() {
    const template = getCurrentTemplate();
    const rangeKey = selectionToRangeKey(state.selection);
    const anchorKey = makeCellKey(state.selection.startRow, state.selection.startCol);
    if (rangeKey === anchorKey) {
      const entry = { ...(getTemplateEntry(template, anchorKey) || {}) };
      for (const key of STYLE_KEYS) {
        delete entry[key];
      }
      upsertTemplateEntry(template, anchorKey, entry);
    } else {
      delete template[rangeKey];
    }
    persistState();
    renderAll();
    setStatus(`Cleared style for ${rangeKey}.`, "success");
  }

  function clearSelectedAnchorText() {
    const template = getCurrentTemplate();
    const anchorKey = makeCellKey(state.selection.startRow, state.selection.startCol);
    const entry = { ...(getTemplateEntry(template, anchorKey) || {}) };
    delete entry.text;
    upsertTemplateEntry(template, anchorKey, entry);
    persistState();
    renderAll();
    setStatus(`Cleared anchor text on ${anchorKey}.`, "success");
  }

  function applyTemplateOptionsFromUi() {
    const template = getCurrentTemplate();
    template._options = {
      hideEmptyRows: Boolean(els.hideEmptyRowsCheckbox.checked),
      hideRowsWithoutValues: Boolean(els.hideRowsWithoutValuesCheckbox.checked),
      expandMultilineRows: Boolean(els.expandMultilineRowsCheckbox.checked)
    };
    persistState();
    renderJsonOutput();
    setStatus("Template options updated.", "success");
  }

  async function copyJsonToClipboard() {
    const text = stringifyConfig(buildConfigObject());
    try {
      await navigator.clipboard.writeText(text);
      setStatus("JSON copied to clipboard.", "success");
    } catch (err) {
      setStatus(`Clipboard copy failed: ${toMessage(err)}`, "error");
    }
  }

  function downloadJsonFile() {
    const fileName = `${state.activeTemplateKey.toLowerCase()}-template-demo.json`;
    const blob = new Blob([stringifyConfig(buildConfigObject())], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = fileName;
    document.body.appendChild(anchor);
    anchor.click();
    anchor.remove();
    URL.revokeObjectURL(url);
    setStatus(`Downloaded ${fileName}.`, "success");
  }

  function importJsonFile(event) {
    const file = event.target.files && event.target.files[0];
    if (!file) {
      return;
    }
    const reader = new FileReader();
    reader.onload = () => {
      els.jsonOutput.value = String(reader.result || "");
      applyJsonFromEditor();
    };
    reader.onerror = () => {
      setStatus("Failed to read JSON file.", "error");
    };
    reader.readAsText(file, "utf8");
    event.target.value = "";
  }

  function applyJsonFromEditor() {
    const raw = String(els.jsonOutput.value || "").trim();
    if (!raw) {
      setStatus("JSON editor is empty.", "error");
      return;
    }

    try {
      const parsed = JSON.parse(raw);
      if (parsed && typeof parsed === "object" && parsed.templates && typeof parsed.templates === "object") {
        state.templates = normalizeTemplates(parsed.templates);
      } else if (parsed && typeof parsed === "object" && !Array.isArray(parsed)) {
        state.templates[state.activeTemplateKey] = normalizeSingleTemplate(parsed);
      } else {
        throw new Error("JSON must be an object.");
      }
      const keys = Object.keys(state.templates);
      state.activeTemplateKey = keys.includes(state.activeTemplateKey) ? state.activeTemplateKey : (keys[0] || "FORM");
      ensureTemplate(state.activeTemplateKey);
      persistState();
      renderAll();
      setStatus("JSON applied into demo state.", "success");
    } catch (err) {
      setStatus(`Invalid JSON: ${toMessage(err)}`, "error");
    }
  }

  function resolvePreviewCell(row, col) {
    const template = getCurrentTemplate();
    const cellKey = makeCellKey(row, col);
    const anchorEntry = getTemplateEntry(template, cellKey) || {};
    const styleEntries = getApplicableStyleEntries(template, row, col);
    const style = {};
    for (const item of styleEntries) {
      Object.assign(style, item.entry);
    }
    return { text: String(anchorEntry.text || ""), style };
  }

  function applyPreviewStyle(td, style) {
    if (!style || typeof style !== "object") {
      return;
    }
    if (style.background) {
      td.style.background = String(style.background);
    }
    if (style["font color"]) {
      td.style.color = String(style["font color"]);
    }
    if (style.border === "outside-medium") {
      td.style.borderWidth = "2px";
      td.style.borderStyle = "solid";
      td.style.borderColor = "#334155";
    } else if (style.border === "outside-thin") {
      td.style.borderWidth = "1px";
      td.style.borderStyle = "solid";
      td.style.borderColor = "#334155";
    }
    if (style["font size"]) {
      td.style.fontSize = `${Number(style["font size"]) || 10}px`;
    }
    if (style.align) {
      td.style.textAlign = String(style.align);
    }
    if (style.valign) {
      td.style.verticalAlign = { top: "top", middle: "middle", bottom: "bottom" }[String(style.valign)] || "top";
    }
    if (style.wrap) {
      td.style.whiteSpace = "pre-wrap";
      td.style.overflowWrap = "anywhere";
    } else {
      td.style.whiteSpace = "nowrap";
      td.style.textOverflow = "ellipsis";
    }
    if (style.bold) {
      td.style.fontWeight = "700";
    }
  }

  function buildMergeMap(template) {
    const covered = new Map();
    for (const [key, entry] of Object.entries(template)) {
      if (key === "_options" || !entry || typeof entry !== "object" || entry.merge !== true) {
        continue;
      }
      const range = parseRangeKey(key);
      if (!range || (range.startRow === range.endRow && range.startCol === range.endCol)) {
        continue;
      }
      for (let row = range.startRow; row <= range.endRow; row += 1) {
        for (let col = range.startCol; col <= range.endCol; col += 1) {
          covered.set(makeCellKey(row, col), {
            rangeKey: key,
            rowSpan: range.endRow - range.startRow + 1,
            colSpan: range.endCol - range.startCol + 1,
            isAnchor: row === range.startRow && col === range.startCol
          });
        }
      }
    }
    return { covered };
  }

  function getApplicableStyleEntries(template, row, col) {
    const entries = [];
    let order = 0;
    for (const [key, entry] of Object.entries(template)) {
      if (key === "_options" || !entry || typeof entry !== "object") {
        continue;
      }
      const range = parseRangeKey(key);
      if (!range || !rangeContains(range, row, col)) {
        continue;
      }
      const styleOnly = pickStyleFields(entry);
      if (!Object.keys(styleOnly).length) {
        continue;
      }
      entries.push({
        rangeKey: key,
        entry: styleOnly,
        area: (range.endRow - range.startRow + 1) * (range.endCol - range.startCol + 1),
        order
      });
      order += 1;
    }
    entries.sort((a, b) => {
      if (b.area !== a.area) {
        return b.area - a.area;
      }
      return a.order - b.order;
    });
    return entries;
  }

  function makeHeaderCell(text) {
    const th = document.createElement("th");
    th.textContent = text;
    return th;
  }

  function ensureTemplate(key) {
    const normalized = String(key || "").trim().toUpperCase() || "FORM";
    if (!state.templates[normalized]) {
      state.templates[normalized] = buildStarterTemplate(normalized);
    } else {
      state.templates[normalized] = normalizeSingleTemplate(state.templates[normalized]);
    }
    state.activeTemplateKey = normalized;
  }

  function getCurrentTemplate() {
    ensureTemplate(state.activeTemplateKey);
    return state.templates[state.activeTemplateKey];
  }

  function buildConfigObject() {
    return {
      version: 1,
      templates: normalizeTemplates(state.templates)
    };
  }

  function persistState() {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify({
        grid: state.grid,
        activeTemplateKey: state.activeTemplateKey,
        templates: state.templates
      }));
    } catch {
      // ignore
    }
  }

  function loadState() {
    const fallback = {
      grid: { ...DEFAULT_GRID },
      activeTemplateKey: "FORM",
      templates: { FORM: buildStarterTemplate("FORM") },
      selection: createSingleCellSelection(1, 1),
      dragging: false,
      dragOrigin: null
    };
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (!raw) {
        return fallback;
      }
      const parsed = JSON.parse(raw);
      return {
        grid: {
          rows: clampInteger(parsed && parsed.grid && parsed.grid.rows, 4, 40, DEFAULT_GRID.rows),
          cols: clampInteger(parsed && parsed.grid && parsed.grid.cols, 4, 26, DEFAULT_GRID.cols)
        },
        activeTemplateKey: String(parsed && parsed.activeTemplateKey ? parsed.activeTemplateKey : "FORM").trim().toUpperCase() || "FORM",
        templates: normalizeTemplates(parsed && parsed.templates ? parsed.templates : fallback.templates),
        selection: createSingleCellSelection(1, 1),
        dragging: false,
        dragOrigin: null
      };
    } catch {
      return fallback;
    }
  }

  function normalizeTemplates(input) {
    const templates = {};
    const source = input && typeof input === "object" && !Array.isArray(input) ? input : {};
    for (const [key, value] of Object.entries(source)) {
      const normalizedKey = String(key || "").trim().toUpperCase();
      if (normalizedKey) {
        templates[normalizedKey] = normalizeSingleTemplate(value);
      }
    }
    if (!Object.keys(templates).length) {
      templates.FORM = buildStarterTemplate("FORM");
    }
    return templates;
  }

  function normalizeSingleTemplate(input) {
    const out = { _options: normalizeTemplateOptions(input && input._options) };
    const source = input && typeof input === "object" && !Array.isArray(input) ? input : {};
    for (const [key, value] of Object.entries(source)) {
      if (key === "_options" || !value || typeof value !== "object" || Array.isArray(value)) {
        continue;
      }
      const normalizedEntry = {};
      if (Object.prototype.hasOwnProperty.call(value, "text")) {
        normalizedEntry.text = String(value.text || "");
      }
      for (const styleKey of STYLE_KEYS) {
        if (Object.prototype.hasOwnProperty.call(value, styleKey)) {
          normalizedEntry[styleKey] = value[styleKey];
        }
      }
      if (Object.keys(normalizedEntry).length) {
        out[key] = normalizedEntry;
      }
    }
    return out;
  }

  function normalizeTemplateOptions(input) {
    const options = input && typeof input === "object" && !Array.isArray(input) ? input : {};
    return {
      hideEmptyRows: options.hideEmptyRows !== false,
      hideRowsWithoutValues: options.hideRowsWithoutValues !== false,
      expandMultilineRows: Boolean(options.expandMultilineRows)
    };
  }

  function buildStarterTemplate(kind) {
    const key = String(kind || "FORM").trim().toUpperCase();
    const template = {
      _options: {
        hideEmptyRows: true,
        hideRowsWithoutValues: true,
        expandMultilineRows: false
      }
    };

    const labelStyle = {
      background: "#dbeef4",
      border: "outside-thin",
      "font color": "#111111",
      "font size": 10,
      align: "left",
      valign: "top",
      wrap: false,
      bold: false
    };
    const valueStyle = {
      background: "#ffffff",
      border: "outside-thin",
      "font color": "#111111",
      "font size": 10,
      align: "left",
      valign: "top",
      wrap: false,
      bold: false
    };

    if (key === "FORM") {
      template["A1:D1"] = { ...labelStyle };
      template.A1 = { text: "FORM" };
      template["E1:L1"] = { ...valueStyle };
      template.E1 = { text: "{values.name.finalDesc}" };
      template["A2:D2"] = { ...labelStyle };
      template.A2 = { text: "USING" };
      template["E2:L2"] = { ...valueStyle };
      template.E2 = { text: "{values.usingRaw}" };
      template["A3:D3"] = { ...labelStyle };
      template.A3 = { text: "CHANGING" };
      template["E3:L3"] = { ...valueStyle };
      template.E3 = { text: "{values.changingRaw}" };
      template["A4:D4"] = { ...labelStyle };
      template.A4 = { text: "TABLES" };
      template["E4:L4"] = { ...valueStyle };
      template.E4 = { text: "{values.tablesRaw}" };
      template["A5:D5"] = { ...labelStyle };
      template.A5 = { text: "RAISING" };
      template["E5:L5"] = { ...valueStyle };
      template.E5 = { text: "{values.raisingRaw}" };
      return template;
    }

    if (key === "PERFORM") {
      template["A1:D1"] = { ...labelStyle };
      template.A1 = { text: "PERFORM" };
      template["E1:L1"] = { ...valueStyle };
      template.E1 = { text: "{values.form.finalDesc}" };
      template["A2:D2"] = { ...labelStyle };
      template.A2 = { text: "USING" };
      template["E2:L2"] = { ...valueStyle };
      template.E2 = { text: "{extras.performCall.using[0].value}" };
      return template;
    }

    if (key === "IF") {
      template["A1:D1"] = { ...labelStyle };
      template.A1 = { text: "IF" };
      template["E1:L1"] = { ...valueStyle };
      template.E1 = { text: "{values.condition.finalDesc}" };
      return template;
    }

    template["A1:D1"] = { ...labelStyle };
    template.A1 = { text: "{keywords.stmt.text}" };
    template["E1:L1"] = { ...valueStyle };
    template.E1 = { text: "{values.name.finalDesc}" };
    return template;
  }

  function pickStarterPresetForTemplateKey(key) {
    const normalized = String(key || "FORM").trim().toUpperCase();
    return ["FORM", "PERFORM", "IF", "DEFAULT"].includes(normalized) ? normalized : "FORM";
  }

  function selectionToRangeKey(selection) {
    const normalized = normalizeSelection(selection);
    const start = makeCellKey(normalized.startRow, normalized.startCol);
    const end = makeCellKey(normalized.endRow, normalized.endCol);
    return start === end ? start : `${start}:${end}`;
  }

  function makeCellKey(row, col) {
    return `${columnNumberToLabel(col)}${row}`;
  }

  function columnNumberToLabel(col) {
    let current = Number(col) || 1;
    let label = "";
    while (current > 0) {
      const remainder = (current - 1) % 26;
      label = String.fromCharCode(65 + remainder) + label;
      current = Math.floor((current - 1) / 26);
    }
    return label;
  }

  function columnLabelToNumber(label) {
    let total = 0;
    const normalized = String(label || "").trim().toUpperCase();
    for (const ch of normalized) {
      total = (total * 26) + (ch.charCodeAt(0) - 64);
    }
    return total;
  }

  function parseRangeKey(key) {
    const raw = String(key || "").trim().toUpperCase();
    if (!raw) {
      return null;
    }
    const parts = raw.split(":");
    if (parts.length > 2) {
      return null;
    }
    const start = parseCellKey(parts[0]);
    const end = parseCellKey(parts[1] || parts[0]);
    if (!start || !end) {
      return null;
    }
    return normalizeSelection({
      startRow: start.row,
      startCol: start.col,
      endRow: end.row,
      endCol: end.col
    });
  }

  function parseCellKey(cellKey) {
    const match = /^([A-Z]+)(\d+)$/.exec(String(cellKey || "").trim().toUpperCase());
    if (!match) {
      return null;
    }
    return { col: columnLabelToNumber(match[1]), row: Number(match[2]) };
  }

  function createSingleCellSelection(row, col) {
    return { startRow: row, startCol: col, endRow: row, endCol: col };
  }

  function normalizeSelection(selection) {
    const startRow = Number(selection && selection.startRow) || 1;
    const startCol = Number(selection && selection.startCol) || 1;
    const endRow = Number(selection && selection.endRow) || startRow;
    const endCol = Number(selection && selection.endCol) || startCol;
    return {
      startRow: Math.min(startRow, endRow),
      startCol: Math.min(startCol, endCol),
      endRow: Math.max(startRow, endRow),
      endCol: Math.max(startCol, endCol)
    };
  }

  function rangeContains(range, row, col) {
    return row >= range.startRow && row <= range.endRow && col >= range.startCol && col <= range.endCol;
  }

  function rangesOverlap(left, right) {
    return !(left.endRow < right.startRow
      || left.startRow > right.endRow
      || left.endCol < right.startCol
      || left.startCol > right.endCol);
  }

  function isSelectionAnchor(row, col) {
    return row === state.selection.startRow && col === state.selection.startCol;
  }

  function getTemplateEntry(template, key) {
    const entry = template && Object.prototype.hasOwnProperty.call(template, key) ? template[key] : null;
    return entry && typeof entry === "object" && !Array.isArray(entry) ? entry : null;
  }

  function upsertTemplateEntry(template, key, entry) {
    const normalized = {};
    for (const [field, value] of Object.entries(entry || {})) {
      if (field === "text") {
        if (String(value || "").trim()) {
          normalized.text = String(value);
        }
        continue;
      }
      if (!STYLE_KEYS.includes(field)) {
        continue;
      }
      if (field === "merge" || field === "wrap" || field === "bold") {
        if (Boolean(value)) {
          normalized[field] = true;
        }
      } else if (field === "font size") {
        normalized[field] = clampInteger(value, 8, 32, 10);
      } else if (String(value || "").trim()) {
        normalized[field] = value;
      }
    }
    if (Object.keys(normalized).length) {
      template[key] = normalized;
    } else {
      delete template[key];
    }
  }

  function pickStyleFields(entry) {
    const style = {};
    for (const key of STYLE_KEYS) {
      if (Object.prototype.hasOwnProperty.call(entry || {}, key)) {
        style[key] = entry[key];
      }
    }
    return style;
  }

  function normalizeColorInput(value, fallback) {
    const raw = String(value || "").trim();
    return /^#[0-9a-fA-F]{6}$/.test(raw) ? raw.toLowerCase() : fallback;
  }

  function stringifyConfig(config) {
    return JSON.stringify(config, null, 2);
  }

  function insertTextAtCursor(textarea, text) {
    const start = textarea.selectionStart || 0;
    const end = textarea.selectionEnd || 0;
    const current = String(textarea.value || "");
    textarea.value = current.slice(0, start) + text + current.slice(end);
    textarea.selectionStart = textarea.selectionEnd = start + text.length;
    textarea.focus();
  }

  function clampInteger(value, min, max, fallback) {
    const num = Number.parseInt(String(value), 10);
    if (!Number.isFinite(num)) {
      return fallback;
    }
    return Math.max(min, Math.min(max, num));
  }

  function formatPreviewText(text) {
    const raw = String(text || "");
    if (!raw.trim()) {
      return "";
    }
    return raw.replace(/\{[^{}]+\}/g, (token) => `<span class="placeholder">${escapeHtml(token)}</span>`);
  }

  function escapeHtml(text) {
    return String(text || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function setStatus(message, tone) {
    els.statusBar.textContent = String(message || "").trim();
    els.statusBar.className = "status";
    if (tone === "error") {
      els.statusBar.classList.add("error");
    }
    if (tone === "success") {
      els.statusBar.classList.add("success");
    }
  }

  function toMessage(err) {
    return err && err.message ? err.message : String(err || "Unknown error");
  }
})();
