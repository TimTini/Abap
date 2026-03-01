"use strict";

(function () {
  const STORAGE_KEY = "abap-parser-viewer.templateConfigEditorOpen.v1";
  const DEFAULT_TEMPLATE_KEY = "DEFAULT";
  const TEMPLATE_KEY_REGEX = /^[A-Z][A-Z0-9_-]*$/;
  const ENTRY_CELL_REGEX = /^[A-Z]{1,3}[1-9][0-9]*$/;
  const ENTRY_RANGE_REGEX = /^[A-Z]{1,3}[1-9][0-9]*:[A-Z]{1,3}[1-9][0-9]*$/;

  const btn = document.getElementById("templateConfigToggleBtn");
  if (!btn) return;

  const state = {
    isOpen: false,
    mode: "form",
    dirty: false,
    draft: null,
    templateKey: DEFAULT_TEMPLATE_KEY,
    jsonText: "",
    advancedEntries: Object.create(null),
    modalEl: null,
    modalBodyEl: null,
    modeFormBtn: null,
    modeJsonBtn: null,
    statusEl: null,
    onKeyDown: null
  };

  const runtimeState = () => (window.AbapViewerRuntime && window.AbapViewerRuntime.state) || null;

  const clone = (value) => {
    if (typeof cloneJsonValue === "function") {
      const copied = cloneJsonValue(value);
      if (copied && typeof copied === "object") return copied;
    }
    try {
      return JSON.parse(JSON.stringify(value));
    } catch {
      return null;
    }
  };

  const pretty = (value) => {
    try {
      return JSON.stringify(value || {}, null, 2);
    } catch {
      return "{}";
    }
  };

  const setErr = (message) => {
    if (typeof setTemplateConfigError === "function") {
      setTemplateConfigError(message || "");
      return;
    }
    const el = document.getElementById("templateConfigError");
    if (el) el.textContent = String(message || "");
  };

  const defaultConfig = () => {
    if (typeof getDefaultTemplateConfig === "function") {
      const d = clone(getDefaultTemplateConfig());
      if (d && typeof d === "object") return d;
    }
    return { version: 1, templates: { DEFAULT: {} } };
  };

  const currentSavedConfig = () => {
    const rs = runtimeState();
    const src = rs && rs.templateConfig && typeof rs.templateConfig === "object"
      ? rs.templateConfig
      : defaultConfig();
    return clone(src) || defaultConfig();
  };

  const normTemplateKey = (text) => String(text || "").trim().toUpperCase();
  const normEntryKey = (text) => String(text || "").trim().toUpperCase();
  const isValidTemplateKey = (key) => TEMPLATE_KEY_REGEX.test(String(key || ""));
  const isValidEntryKey = (key) => {
    const n = normEntryKey(key);
    return ENTRY_CELL_REGEX.test(n) || ENTRY_RANGE_REGEX.test(n);
  };

  const templateMap = (config) => {
    if (!config || typeof config !== "object") return null;
    if (!config.templates || typeof config.templates !== "object" || Array.isArray(config.templates)) {
      config.templates = {};
    }
    return config.templates;
  };

  const getTemplate = (config, key) => {
    const map = templateMap(config);
    if (!map) return null;
    const k = normTemplateKey(key);
    const tmpl = map[k];
    return tmpl && typeof tmpl === "object" && !Array.isArray(tmpl) ? tmpl : null;
  };

  const styleEntryDefault = () => {
    if (typeof createTemplateBaseStyle === "function") {
      const style = clone(createTemplateBaseStyle("#ffffff"));
      if (style && typeof style === "object") return style;
    }
    return {
      background: "#ffffff",
      border: "outside-thin",
      font: "MS PGothic",
      "font color": "#111111",
      "font size": 10,
      "font family": "default",
      bold: false,
      italic: false,
      underline: false,
      merge: false,
      align: "left",
      valign: "top",
      wrap: false
    };
  };

  const templateDefault = (key) => {
    const k = normTemplateKey(key);
    if (typeof createGenericStatementTemplate === "function") {
      const t = clone(createGenericStatementTemplate(k));
      if (t && typeof t === "object") return t;
    }
    return {
      _options: {
        hideEmptyRows: true,
        hideRowsWithoutValues: true,
        expandMultilineRows: true,
        squareCells: true,
        squareCellSize: 18
      },
      A1: { text: k }
    };
  };

  const templateKeys = () => {
    const map = templateMap(state.draft);
    if (!map) return [];
    const keys = Object.keys(map)
      .map((key) => normTemplateKey(key))
      .filter((key) => key && map[key] && typeof map[key] === "object" && !Array.isArray(map[key]));
    if (!keys.includes(DEFAULT_TEMPLATE_KEY)) {
      keys.unshift(DEFAULT_TEMPLATE_KEY);
      map[DEFAULT_TEMPLATE_KEY] = templateDefault(DEFAULT_TEMPLATE_KEY);
    }
    const uniq = Array.from(new Set(keys));
    const tail = uniq.filter((key) => key !== DEFAULT_TEMPLATE_KEY).sort((a, b) => a.localeCompare(b));
    return [DEFAULT_TEMPLATE_KEY].concat(tail);
  };

  const entryKeys = (tmpl) => (!tmpl || typeof tmpl !== "object")
    ? []
    : Object.keys(tmpl).filter((key) => key !== "_options").sort((a, b) => a.localeCompare(b));

  const isTextEntry = (entry) => Boolean(
    entry
    && typeof entry === "object"
    && !Array.isArray(entry)
    && Object.prototype.hasOwnProperty.call(entry, "text")
  );

  const isStyleEntry = (entry) => Boolean(
    entry
    && typeof entry === "object"
    && !Array.isArray(entry)
    && !Object.prototype.hasOwnProperty.call(entry, "text")
  );

  const selectedTemplate = () => getTemplate(state.draft, state.templateKey);

  const ensureOptions = (tmpl) => {
    const base = tmpl && typeof tmpl._options === "object" && !Array.isArray(tmpl._options)
      ? tmpl._options
      : {};
    let normalized = base;
    if (typeof normalizeTemplatePreviewOptions === "function") {
      normalized = normalizeTemplatePreviewOptions(base);
    }
    tmpl._options = {
      hideEmptyRows: Boolean(normalized.hideEmptyRows),
      hideRowsWithoutValues: Boolean(normalized.hideRowsWithoutValues),
      expandMultilineRows: Boolean(normalized.expandMultilineRows),
      squareCells: normalized.squareCells === undefined ? true : Boolean(normalized.squareCells),
      squareCellSize: Math.max(1, Number(normalized.squareCellSize) || 18)
    };
    return tmpl._options;
  };

  const syncSelection = () => {
    if (!state.draft || typeof state.draft !== "object") state.draft = defaultConfig();
    const keys = templateKeys();
    if (!keys.length) {
      state.templateKey = DEFAULT_TEMPLATE_KEY;
      return;
    }
    if (!keys.includes(state.templateKey)) {
      state.templateKey = keys.includes(DEFAULT_TEMPLATE_KEY) ? DEFAULT_TEMPLATE_KEY : keys[0];
    }
  };

  const markDirty = () => {
    state.dirty = true;
    refreshStatus();
  };

  const loadOpenState = () => {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      return raw === "1" || raw === "true";
    } catch {
      return false;
    }
  };

  const saveOpenState = (open) => {
    try {
      localStorage.setItem(STORAGE_KEY, open ? "1" : "0");
    } catch {
      // ignore localStorage failure
    }
  };

  const updateButton = () => {
    btn.textContent = state.isOpen ? "Close Template Editor" : "Open Template Editor";
    btn.setAttribute("aria-expanded", String(state.isOpen));
    btn.setAttribute("aria-controls", "templateConfigModal");
  };

  const refreshStatus = () => {
    if (state.statusEl) {
      state.statusEl.textContent = state.dirty ? "Draft has unsaved changes" : "Draft is synced";
    }
    if (state.modeFormBtn) {
      const active = state.mode === "form";
      state.modeFormBtn.classList.toggle("active", active);
      state.modeFormBtn.setAttribute("aria-pressed", active ? "true" : "false");
    }
    if (state.modeJsonBtn) {
      const active = state.mode === "json";
      state.modeJsonBtn.classList.toggle("active", active);
      state.modeJsonBtn.setAttribute("aria-pressed", active ? "true" : "false");
    }
  };

  const initDraft = () => {
    state.draft = currentSavedConfig();
    state.mode = "form";
    state.dirty = false;
    state.templateKey = DEFAULT_TEMPLATE_KEY;
    state.jsonText = "";
    state.advancedEntries = Object.create(null);
    syncSelection();
  };

  const saveDraft = () => {
    const draft = clone(state.draft);
    if (!draft || typeof draft !== "object") {
      setErr("Cannot save: invalid draft config.");
      return false;
    }

    if (typeof validateTemplateConfig === "function") {
      const check = validateTemplateConfig(draft);
      if (!check.valid) {
        setErr((check.errors || []).join("\n"));
        return false;
      }
    }

    let ok = false;
    if (typeof applyTemplateConfigObject === "function") {
      ok = applyTemplateConfigObject(draft, { save: true });
    } else {
      const rs = runtimeState();
      if (!rs) {
        setErr("Cannot save: runtime state unavailable.");
        return false;
      }
      rs.templateConfig = draft;
      if (typeof saveTemplateConfig === "function") saveTemplateConfig(draft);
      if (typeof renderTemplatePreview === "function") renderTemplatePreview();
      ok = true;
    }
    if (!ok) return false;

    const prevTemplate = state.templateKey;
    state.draft = currentSavedConfig();
    state.templateKey = prevTemplate;
    state.dirty = false;
    if (state.mode === "json") state.jsonText = pretty(state.draft);
    setErr("");
    syncSelection();
    renderModalBody();
    return true;
  };

  const discardDraft = () => {
    if (state.dirty && !window.confirm("Discard unsaved changes?")) return;
    const prevTemplate = state.templateKey;
    state.draft = currentSavedConfig();
    state.templateKey = prevTemplate;
    state.dirty = false;
    state.advancedEntries = Object.create(null);
    if (state.mode === "json") state.jsonText = pretty(state.draft);
    setErr("");
    syncSelection();
    renderModalBody();
  };

  const resetTemplate = () => {
    const key = normTemplateKey(state.templateKey);
    if (!key) return setErr("Select a template first.");

    const map = templateMap(state.draft);
    if (!map) return setErr("Cannot reset template.");

    const defaults = defaultConfig();
    const defaultMap = templateMap(defaults) || {};
    const resetObj = defaultMap[key] ? clone(defaultMap[key]) : templateDefault(key);
    map[key] = resetObj && typeof resetObj === "object" ? resetObj : templateDefault(key);
    state.advancedEntries = Object.create(null);
    markDirty();
    if (state.mode === "json") state.jsonText = pretty(state.draft);
    setErr("");
    renderModalBody();
  };

  const parseJsonDraft = () => {
    const raw = String(state.jsonText || "").trim();
    if (!raw) {
      setErr("JSON editor is empty.");
      return null;
    }

    let parsed = null;
    try {
      parsed = JSON.parse(raw);
    } catch (err) {
      setErr(`JSON parse error: ${err && err.message ? err.message : err}`);
      return null;
    }

    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
      setErr("Template config must be an object.");
      return null;
    }

    if (typeof validateTemplateConfig === "function") {
      const check = validateTemplateConfig(parsed);
      if (!check.valid) {
        setErr((check.errors || []).join("\n"));
        return null;
      }
    }

    return parsed;
  };

  const applyJsonToDraft = () => {
    const parsed = parseJsonDraft();
    if (!parsed) return false;

    const prevTemplate = state.templateKey;
    state.draft = clone(parsed) || parsed;
    state.templateKey = prevTemplate;
    state.advancedEntries = Object.create(null);
    markDirty();
    setErr("");
    syncSelection();
    renderModalBody();
    return true;
  };

  const addTemplate = (rawKey) => {
    const key = normTemplateKey(rawKey);
    if (!key) return setErr("Enter a template key.");
    if (!isValidTemplateKey(key)) return setErr("Template key must match [A-Z][A-Z0-9_-]*.");

    const map = templateMap(state.draft);
    if (!map) return setErr("Cannot add template.");
    if (Object.prototype.hasOwnProperty.call(map, key)) return setErr(`Template '${key}' already exists.`);

    map[key] = templateDefault(key);
    state.templateKey = key;
    state.advancedEntries = Object.create(null);
    markDirty();
    setErr("");
    renderModalBody();
    return true;
  };

  const deleteTemplate = () => {
    const key = normTemplateKey(state.templateKey);
    if (!key) return setErr("Select a template first.");
    if (key === DEFAULT_TEMPLATE_KEY) return setErr("DEFAULT template cannot be deleted.");

    const map = templateMap(state.draft);
    if (!map || !Object.prototype.hasOwnProperty.call(map, key)) return setErr("Selected template does not exist.");
    if (!window.confirm(`Delete template '${key}'?`)) return;

    delete map[key];
    state.templateKey = DEFAULT_TEMPLATE_KEY;
    state.advancedEntries = Object.create(null);
    markDirty();
    setErr("");
    syncSelection();
    renderModalBody();
  };

  const addEntry = (kind, rawKey) => {
    const key = normEntryKey(rawKey);
    if (!key) return setErr("Enter an entry key.");
    if (!isValidEntryKey(key)) return setErr("Entry key must be like A1 or A1:F1.");

    const tmpl = selectedTemplate();
    if (!tmpl) return setErr("Select a template first.");
    if (Object.prototype.hasOwnProperty.call(tmpl, key)) return setErr(`Entry '${key}' already exists.`);

    tmpl[key] = kind === "text" ? { text: "" } : styleEntryDefault();
    if (kind === "style") state.advancedEntries[key] = false;
    markDirty();
    setErr("");
    renderModalBody();
    return true;
  };

  const renameEntry = (oldKeyRaw, newKeyRaw) => {
    const oldKey = normEntryKey(oldKeyRaw);
    const newKey = normEntryKey(newKeyRaw);
    const tmpl = selectedTemplate();

    if (!tmpl || !Object.prototype.hasOwnProperty.call(tmpl, oldKey)) {
      setErr("Selected entry does not exist.");
      renderModalBody();
      return false;
    }
    if (!newKey) {
      setErr("Entry key cannot be empty.");
      renderModalBody();
      return false;
    }
    if (!isValidEntryKey(newKey)) {
      setErr("Entry key must be like A1 or A1:F1.");
      renderModalBody();
      return false;
    }

    if (newKey === oldKey) {
      setErr("");
      if (String(newKeyRaw || "").trim() !== oldKey) renderModalBody();
      return true;
    }

    if (Object.prototype.hasOwnProperty.call(tmpl, newKey)) {
      setErr(`Entry '${newKey}' already exists.`);
      renderModalBody();
      return false;
    }

    const entry = tmpl[oldKey];
    delete tmpl[oldKey];
    tmpl[newKey] = entry;

    if (Object.prototype.hasOwnProperty.call(state.advancedEntries, oldKey)) {
      state.advancedEntries[newKey] = Boolean(state.advancedEntries[oldKey]);
      delete state.advancedEntries[oldKey];
    }

    markDirty();
    setErr("");
    renderModalBody();
    return true;
  };

  const deleteEntry = (keyRaw) => {
    const key = normEntryKey(keyRaw);
    if (!key) return setErr("Entry key is empty.");

    const tmpl = selectedTemplate();
    if (!tmpl || !Object.prototype.hasOwnProperty.call(tmpl, key)) return setErr("Selected entry does not exist.");
    if (!window.confirm(`Delete entry '${key}'?`)) return;

    delete tmpl[key];
    if (Object.prototype.hasOwnProperty.call(state.advancedEntries, key)) {
      delete state.advancedEntries[key];
    }
    markDirty();
    setErr("");
    renderModalBody();
  };

  const switchMode = (mode) => {
    if (mode !== "form" && mode !== "json") return;
    if (mode === state.mode) return;
    if (mode === "json") state.jsonText = pretty(state.draft);
    state.mode = mode;
    setErr("");
    renderModalBody();
  };

  const closeEditor = () => {
    if (!state.isOpen) return;
    if (state.dirty && !window.confirm("Discard unsaved changes?")) return;

    unmountModal();
    state.isOpen = false;
    state.mode = "form";
    state.dirty = false;
    state.draft = null;
    state.jsonText = "";
    state.advancedEntries = Object.create(null);
    saveOpenState(false);
    updateButton();
    setErr("");
  };

  const openEditor = () => {
    if (state.isOpen) return;
    initDraft();
    state.isOpen = true;
    mountModal();
    renderModalBody();
    saveOpenState(true);
    updateButton();
    setErr("");
  };

  const mkHeading = (text) => {
    const el = document.createElement("h3");
    el.textContent = text;
    return el;
  };

  const mkButton = (text, className) => {
    const b = document.createElement("button");
    b.type = "button";
    b.className = className || "secondary";
    b.textContent = text;
    return b;
  };

  const mkInputField = (labelText, inputEl, options) => {
    const opts = options && typeof options === "object" ? options : {};
    const wrap = document.createElement("div");
    wrap.className = "template-form-field";

    if (opts.checkbox) {
      wrap.classList.add("checkbox-field");
      const label = document.createElement("label");
      label.textContent = labelText;
      wrap.appendChild(inputEl);
      wrap.appendChild(label);
      return wrap;
    }

    const label = document.createElement("label");
    label.textContent = labelText;
    wrap.appendChild(label);
    wrap.appendChild(inputEl);
    return wrap;
  };

  const mkSelect = (items, value) => {
    const sel = document.createElement("select");
    sel.className = "template-form-input";
    for (const item of items) {
      const opt = document.createElement("option");
      opt.value = String(item.value);
      opt.textContent = String(item.label);
      if (String(item.value) === String(value)) opt.selected = true;
      sel.appendChild(opt);
    }
    return sel;
  };

  const mkText = (value, placeholder) => {
    const el = document.createElement("input");
    el.type = "text";
    el.className = "template-form-input";
    el.value = value || "";
    if (placeholder) el.placeholder = placeholder;
    return el;
  };

  const mkNumber = (value, min, max) => {
    const el = document.createElement("input");
    el.type = "number";
    el.className = "template-form-input";
    el.step = "1";
    el.value = value === undefined || value === null ? "" : String(value);
    if (min !== undefined) el.min = String(min);
    if (max !== undefined) el.max = String(max);
    return el;
  };

  const mkCheck = (checked) => {
    const el = document.createElement("input");
    el.type = "checkbox";
    el.checked = Boolean(checked);
    return el;
  };

  const sectionTemplate = () => {
    const section = document.createElement("section");
    section.className = "template-form-section";
    section.appendChild(mkHeading("Template"));

    const row1 = document.createElement("div");
    row1.className = "template-form-row";
    const keys = templateKeys();
    const sel = mkSelect(keys.map((k) => ({ value: k, label: k })), state.templateKey);
    sel.addEventListener("change", () => {
      state.templateKey = normTemplateKey(sel.value);
      state.advancedEntries = Object.create(null);
      setErr("");
      syncSelection();
      renderModalBody();
    });
    row1.appendChild(mkInputField("Current template", sel));
    section.appendChild(row1);

    const row2 = document.createElement("div");
    row2.className = "template-form-row";
    const input = mkText("", "e.g. ASSIGNMENT_COPY");
    input.addEventListener("keydown", (ev) => {
      if (ev.key === "Enter") {
        ev.preventDefault();
        addTemplate(input.value);
      }
    });
    row2.appendChild(mkInputField("New template key", input));

    const addBtn = mkButton("Add Template");
    addBtn.addEventListener("click", () => addTemplate(input.value));
    row2.appendChild(addBtn);

    const delBtn = mkButton("Delete Template");
    delBtn.disabled = state.templateKey === DEFAULT_TEMPLATE_KEY;
    delBtn.addEventListener("click", deleteTemplate);
    row2.appendChild(delBtn);

    section.appendChild(row2);
    return section;
  };

  const sectionOptions = () => {
    const section = document.createElement("section");
    section.className = "template-form-section";
    section.appendChild(mkHeading("Template Options"));

    const tmpl = selectedTemplate();
    if (!tmpl) {
      const p = document.createElement("p");
      p.className = "template-form-help";
      p.textContent = "No template selected.";
      section.appendChild(p);
      return section;
    }

    const opts = ensureOptions(tmpl);
    const row1 = document.createElement("div");
    row1.className = "template-form-row";

    const appendToggle = (label, key) => {
      const c = mkCheck(opts[key]);
      c.addEventListener("change", () => {
        opts[key] = c.checked;
        markDirty();
      });
      row1.appendChild(mkInputField(label, c, { checkbox: true }));
    };

    appendToggle("Hide empty rows", "hideEmptyRows");
    appendToggle("Hide rows without values", "hideRowsWithoutValues");
    appendToggle("Expand multiline rows", "expandMultilineRows");
    appendToggle("Square cells", "squareCells");
    section.appendChild(row1);

    const row2 = document.createElement("div");
    row2.className = "template-form-row";
    const size = mkNumber(opts.squareCellSize, 1, 500);
    size.addEventListener("change", () => {
      const value = Math.max(1, Number(size.value) || 18);
      opts.squareCellSize = value;
      size.value = String(value);
      markDirty();
    });
    row2.appendChild(mkInputField("Square cell size", size));
    section.appendChild(row2);

    return section;
  };

  const renderTextEntryCard = (card, entry) => {
    const ta = document.createElement("textarea");
    ta.className = "template-form-textarea";
    ta.placeholder = "Template text";
    ta.value = String(entry.text || "");
    ta.addEventListener("input", () => {
      entry.text = ta.value;
      markDirty();
    });

    const field = mkInputField("Text", ta);
    field.style.width = "100%";
    card.appendChild(field);
  };

  const appendStyleBasics = (card, entry) => {
    const grid = document.createElement("div");
    grid.className = "template-form-grid";

    const bg = mkText(entry.background, "");
    bg.addEventListener("change", () => {
      entry.background = String(bg.value || "");
      markDirty();
    });
    grid.appendChild(mkInputField("Background", bg));

    const color = mkText(entry["font color"], "");
    color.addEventListener("change", () => {
      entry["font color"] = String(color.value || "");
      markDirty();
    });
    grid.appendChild(mkInputField("Font color", color));

    const size = mkNumber(entry["font size"], 1, 200);
    size.addEventListener("change", () => {
      entry["font size"] = Math.max(1, Number(size.value) || 10);
      size.value = String(entry["font size"]);
      markDirty();
    });
    grid.appendChild(mkInputField("Font size", size));

    const align = mkSelect([
      { value: "left", label: "left" },
      { value: "center", label: "center" },
      { value: "right", label: "right" }
    ], entry.align || "left");
    align.addEventListener("change", () => {
      entry.align = String(align.value || "left");
      markDirty();
    });
    grid.appendChild(mkInputField("Align", align));

    card.appendChild(grid);

    const row = document.createElement("div");
    row.className = "template-form-row";
    const appendFlag = (label, key) => {
      const c = mkCheck(entry[key]);
      c.addEventListener("change", () => {
        entry[key] = c.checked;
        markDirty();
      });
      row.appendChild(mkInputField(label, c, { checkbox: true }));
    };
    appendFlag("Wrap", "wrap");
    appendFlag("Bold", "bold");
    appendFlag("Italic", "italic");
    card.appendChild(row);
  };

  const appendStyleAdvanced = (card, entry) => {
    const grid = document.createElement("div");
    grid.className = "template-form-grid";

    const border = mkText(entry.border, "");
    border.addEventListener("change", () => {
      entry.border = String(border.value || "");
      markDirty();
    });
    grid.appendChild(mkInputField("Border", border));

    const font = mkText(entry.font, "");
    font.addEventListener("change", () => {
      entry.font = String(font.value || "");
      markDirty();
    });
    grid.appendChild(mkInputField("Font", font));

    const family = mkText(entry["font family"], "");
    family.addEventListener("change", () => {
      entry["font family"] = String(family.value || "");
      markDirty();
    });
    grid.appendChild(mkInputField("Font family", family));

    const valign = mkSelect([
      { value: "top", label: "top" },
      { value: "middle", label: "middle" },
      { value: "bottom", label: "bottom" }
    ], entry.valign || "top");
    valign.addEventListener("change", () => {
      entry.valign = String(valign.value || "top");
      markDirty();
    });
    grid.appendChild(mkInputField("VAlign", valign));

    card.appendChild(grid);

    const row = document.createElement("div");
    row.className = "template-form-row";
    const appendFlag = (label, key) => {
      const c = mkCheck(entry[key]);
      c.addEventListener("change", () => {
        entry[key] = c.checked;
        markDirty();
      });
      row.appendChild(mkInputField(label, c, { checkbox: true }));
    };
    appendFlag("Underline", "underline");
    appendFlag("Merge", "merge");
    card.appendChild(row);
  };

  const buildEntryCard = (entryKey) => {
    const tmpl = selectedTemplate();
    if (!tmpl) return null;

    const key = normEntryKey(entryKey);
    const entry = tmpl[key];
    if (!entry || typeof entry !== "object" || Array.isArray(entry)) return null;

    const card = document.createElement("div");
    card.className = "template-entry-card";

    const header = document.createElement("div");
    header.className = "template-entry-header";

    const title = document.createElement("div");
    title.className = "template-entry-title";

    const keyInput = mkText(key, "A1 or A1:F1");
    keyInput.addEventListener("change", () => {
      renameEntry(key, keyInput.value);
    });
    title.appendChild(mkInputField("Entry key", keyInput));

    const kind = document.createElement("span");
    kind.className = "template-entry-kind";
    kind.textContent = isTextEntry(entry) ? "TEXT" : (isStyleEntry(entry) ? "STYLE" : "UNKNOWN");
    title.appendChild(kind);

    header.appendChild(title);

    const actions = document.createElement("div");
    actions.className = "template-entry-actions";

    if (isStyleEntry(entry)) {
      const isAdvancedOpen = Boolean(state.advancedEntries[key]);
      const adv = mkButton(isAdvancedOpen ? "Close Advanced" : "Open Advanced");
      adv.addEventListener("click", () => {
        state.advancedEntries[key] = !isAdvancedOpen;
        renderModalBody();
      });
      actions.appendChild(adv);
    }

    const del = mkButton("Delete Entry");
    del.addEventListener("click", () => deleteEntry(key));
    actions.appendChild(del);

    header.appendChild(actions);
    card.appendChild(header);

    if (isTextEntry(entry)) {
      renderTextEntryCard(card, entry);
      return card;
    }

    if (isStyleEntry(entry)) {
      appendStyleBasics(card, entry);
      if (Boolean(state.advancedEntries[key])) {
        appendStyleAdvanced(card, entry);
      }
      return card;
    }

    const unsupported = document.createElement("p");
    unsupported.className = "template-form-help";
    unsupported.textContent = "Unsupported entry format.";
    card.appendChild(unsupported);
    return card;
  };

  const sectionEntries = () => {
    const section = document.createElement("section");
    section.className = "template-form-section";
    section.appendChild(mkHeading("Entries"));

    const tmpl = selectedTemplate();
    if (!tmpl) {
      const p = document.createElement("p");
      p.className = "template-form-help";
      p.textContent = "No template selected.";
      section.appendChild(p);
      return section;
    }

    const addRow = document.createElement("div");
    addRow.className = "template-form-row";
    const input = mkText("", "e.g. A1 or A1:F1");
    input.addEventListener("keydown", (ev) => {
      if (ev.key === "Enter") {
        ev.preventDefault();
        addEntry("style", input.value);
      }
    });
    addRow.appendChild(mkInputField("New entry key", input));

    const addStyle = mkButton("Add Style Range");
    addStyle.addEventListener("click", () => addEntry("style", input.value));
    addRow.appendChild(addStyle);

    const addText = mkButton("Add Text Cell");
    addText.addEventListener("click", () => addEntry("text", input.value));
    addRow.appendChild(addText);

    section.appendChild(addRow);

    const keys = entryKeys(tmpl);
    if (!keys.length) {
      const p = document.createElement("p");
      p.className = "template-form-help";
      p.textContent = "No entries in this template.";
      section.appendChild(p);
      return section;
    }

    const list = document.createElement("div");
    list.className = "template-entry-list";
    for (const key of keys) {
      const card = buildEntryCard(key);
      if (card) list.appendChild(card);
    }
    section.appendChild(list);
    return section;
  };

  const sectionActions = () => {
    const section = document.createElement("section");
    section.className = "template-form-section";
    section.appendChild(mkHeading("Actions"));

    const row = document.createElement("div");
    row.className = "template-form-actions";

    const save = mkButton("Save Config");
    save.addEventListener("click", saveDraft);
    row.appendChild(save);

    const discard = mkButton("Discard Draft");
    discard.addEventListener("click", discardDraft);
    row.appendChild(discard);

    const reset = mkButton("Reset Template");
    reset.addEventListener("click", resetTemplate);
    row.appendChild(reset);

    const jsonMode = mkButton("Open JSON Editor");
    jsonMode.addEventListener("click", () => switchMode("json"));
    row.appendChild(jsonMode);

    section.appendChild(row);

    const help = document.createElement("p");
    help.className = "template-form-help";
    help.textContent = "All entries are editable in this form. Save applies to preview and local storage.";
    section.appendChild(help);

    return section;
  };

  const renderFormMode = () => {
    const root = document.createElement("div");
    root.className = "template-form";
    root.appendChild(sectionTemplate());
    root.appendChild(sectionOptions());
    root.appendChild(sectionEntries());
    root.appendChild(sectionActions());
    return root;
  };

  const renderJsonMode = () => {
    const section = document.createElement("section");
    section.className = "template-json-panel";
    section.appendChild(mkHeading("JSON Editor"));

    const help = document.createElement("p");
    help.className = "template-form-help";
    help.textContent = "Use JSON for advanced edits. Changes apply only when you click Apply or Save.";
    section.appendChild(help);

    const ta = document.createElement("textarea");
    ta.className = "template-form-textarea";
    ta.style.minHeight = "340px";
    ta.placeholder = "Template config JSON...";
    ta.value = String(state.jsonText || "");
    ta.addEventListener("input", () => {
      state.jsonText = ta.value;
    });
    section.appendChild(ta);

    const row = document.createElement("div");
    row.className = "template-form-actions";

    const validate = mkButton("Validate JSON");
    validate.addEventListener("click", () => {
      const parsed = parseJsonDraft();
      if (parsed) setErr("");
    });
    row.appendChild(validate);

    const apply = mkButton("Apply JSON To Draft");
    apply.addEventListener("click", applyJsonToDraft);
    row.appendChild(apply);

    const save = mkButton("Save JSON");
    save.addEventListener("click", () => {
      if (applyJsonToDraft()) saveDraft();
    });
    row.appendChild(save);

    const reload = mkButton("Reload From Draft");
    reload.addEventListener("click", () => {
      state.jsonText = pretty(state.draft);
      renderModalBody();
    });
    row.appendChild(reload);

    const back = mkButton("Back To Form Editor");
    back.addEventListener("click", () => switchMode("form"));
    row.appendChild(back);

    section.appendChild(row);
    return section;
  };

  function renderModalBody() {
    if (!state.modalBodyEl || !state.isOpen) return;

    syncSelection();
    state.modalBodyEl.replaceChildren();
    if (state.mode === "json") state.modalBodyEl.appendChild(renderJsonMode());
    else state.modalBodyEl.appendChild(renderFormMode());
    refreshStatus();
  }

  function mountModal() {
    if (state.modalEl) return;

    const modal = document.createElement("div");
    modal.id = "templateConfigModal";
    modal.className = "modal template-editor-modal";

    const content = document.createElement("div");
    content.className = "modal-content template-editor-modal-content";

    const header = document.createElement("div");
    header.className = "modal-header";

    const titleWrap = document.createElement("div");
    titleWrap.className = "template-editor-title-wrap";

    const title = document.createElement("strong");
    title.textContent = "Template Editor";
    titleWrap.appendChild(title);

    const status = document.createElement("span");
    status.className = "template-editor-status";
    titleWrap.appendChild(status);

    header.appendChild(titleWrap);

    const actions = document.createElement("div");
    actions.className = "modal-actions template-editor-header-actions";

    const formBtn = mkButton("Form Editor", "secondary template-mode-btn");
    formBtn.addEventListener("click", () => switchMode("form"));
    actions.appendChild(formBtn);

    const jsonBtn = mkButton("JSON Editor", "secondary template-mode-btn");
    jsonBtn.addEventListener("click", () => switchMode("json"));
    actions.appendChild(jsonBtn);

    const closeBtn = mkButton("Close");
    closeBtn.addEventListener("click", closeEditor);
    actions.appendChild(closeBtn);

    header.appendChild(actions);

    const body = document.createElement("div");
    body.className = "modal-body template-editor-modal-body";

    content.appendChild(header);
    content.appendChild(body);
    modal.appendChild(content);

    modal.addEventListener("click", (ev) => {
      if (ev.target === modal) closeEditor();
    });

    state.onKeyDown = (ev) => {
      if (!state.isOpen) return;
      if (ev.key !== "Escape") return;
      ev.preventDefault();
      closeEditor();
    };
    document.addEventListener("keydown", state.onKeyDown);

    document.body.appendChild(modal);
    state.modalEl = modal;
    state.modalBodyEl = body;
    state.modeFormBtn = formBtn;
    state.modeJsonBtn = jsonBtn;
    state.statusEl = status;
    refreshStatus();
  }

  function unmountModal() {
    if (state.onKeyDown) {
      document.removeEventListener("keydown", state.onKeyDown);
      state.onKeyDown = null;
    }

    if (state.modalEl && state.modalEl.parentNode) {
      state.modalEl.parentNode.removeChild(state.modalEl);
    }

    state.modalEl = null;
    state.modalBodyEl = null;
    state.modeFormBtn = null;
    state.modeJsonBtn = null;
    state.statusEl = null;
  }

  btn.addEventListener("click", () => {
    if (state.isOpen) closeEditor();
    else openEditor();
  });

  const shouldOpen = loadOpenState();
  state.isOpen = false;
  updateButton();
  if (shouldOpen) openEditor();
})();
