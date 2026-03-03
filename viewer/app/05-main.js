"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};

  const TEMPLATE_GUI_FILTER_STORAGE_KEY_V1 = "abap-parser-viewer.templateGuiHiddenObjectTypes.v1";
  let activeTemplateDynamicModal = null;
  let templateFilterModalControls = null;

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

  function rerenderTemplateForGuiFilterChange() {
    if (state.selectedTemplateIndex !== "") {
      state.selectedTemplateIndex = "";
    }
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

  async function copyAllTemplateBlocks() {
    const items = getRenderableObjectListForTemplate({ includeHidden: true });
    if (!items.length) {
      setError("Nothing to copy.");
      return;
    }

    const virtual = typeof getTemplateVirtualState === "function" ? getTemplateVirtualState() : null;
    const config = virtual && virtual.config && typeof virtual.config === "object"
      ? virtual.config
      : (state.templateConfig && typeof state.templateConfig === "object"
        ? state.templateConfig
        : getDefaultTemplateConfig());

    const wrapper = document.createElement("div");
    const plainLines = [];
    const tableOnly = typeof isTemplateCopyTableOnlyEnabled === "function"
      ? isTemplateCopyTableOnlyEnabled()
      : Boolean(els.templateCopyTableOnly && els.templateCopyTableOnly.checked);

    for (let index = 0; index < items.length; index += 1) {
      const payload = buildTemplateBlockCopyPayload(items[index], index, config, tableOnly);
      if (!payload || !payload.node) {
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

  function openTemplateConfigModal() {
    const modal = openTemplateDynamicModal("Template Form", { contentClass: "template-runtime-modal-content template-runtime-modal-wide" });
    modal.closeBtn.textContent = "Cancel";
    const prevJsonEl = els.templateConfigJson;
    const prevErrEl = els.templateConfigError;
    const applyBtn = document.createElement("button");
    applyBtn.type = "button";
    applyBtn.className = "secondary";
    applyBtn.textContent = "Apply";
    modal.actions.prepend(applyBtn);

    const tabs = document.createElement("div");
    tabs.className = "template-config-editor-tabs";
    const formBtn = document.createElement("button");
    formBtn.type = "button";
    formBtn.className = "secondary";
    formBtn.textContent = "Form";
    const jsonBtn = document.createElement("button");
    jsonBtn.type = "button";
    jsonBtn.className = "secondary";
    jsonBtn.textContent = "JSON (Advanced)";
    tabs.appendChild(formBtn);
    tabs.appendChild(jsonBtn);
    modal.body.appendChild(tabs);
    const host = document.createElement("div");
    host.className = "template-config-editor-host";
    modal.body.appendChild(host);
    const errEl = document.createElement("div");
    errEl.className = "template-error";
    modal.body.appendChild(errEl);

    const OPTION_KEYS = new Set(["_options", "options", "ranges", "compact", "hideemptyrows", "hiderowswithoutvalues", "expandmultilinerows", "removeemptyrows", "removeemptyrowsadvanced", "removeemptyrowsadv", "expandarrayrows", "arraytorows"]);
    const FIELD_DEFS = [
      { key: "text", label: "Text", kind: "text" },
      { key: "background", label: "Background", kind: "text" },
      { key: "border", label: "Border", kind: "text" },
      { key: "font", label: "Font", kind: "text" },
      { key: "font size", label: "Font Size", kind: "number" },
      { key: "font color", label: "Font Color", kind: "text" },
      { key: "align", label: "Align", kind: "select", opts: ["", "left", "center", "right"] },
      { key: "valign", label: "VAlign", kind: "select", opts: ["", "top", "middle", "bottom"] },
      { key: "wrap", label: "Wrap", kind: "bool" },
      { key: "merge", label: "Merge", kind: "bool" },
      { key: "bold", label: "Bold", kind: "bool" },
      { key: "italic", label: "Italic", kind: "bool" },
      { key: "underline", label: "Underline", kind: "bool" }
    ];
    const showErr = (m) => {
      const t = String(m || "").trim();
      errEl.textContent = t;
      if (typeof setTemplateConfigError === "function") {
        setTemplateConfigError(t);
      }
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
      draft.templates.DEFAULT = {};
    }
    let activeTab = "form";
    let jsonArea = null;
    let selKey = Object.keys(draft.templates)[0] || "DEFAULT";
    let selRange = "";
    els.templateConfigError = errEl;
    els.templateConfigJson = null;
    showErr("");

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
      const out = { hideEmptyRows: true, hideRowsWithoutValues: true, expandMultilineRows: false };
      if (!info) return out;
      const setB = (x, v) => { if (!(v === undefined || v === null || v === "")) out[x] = Boolean(v); };
      for (const src of [info.def.options, info.def._options]) {
        if (!src || typeof src !== "object" || Array.isArray(src)) continue;
        setB("hideEmptyRows", src.hideEmptyRows);
        setB("hideRowsWithoutValues", src.hideRowsWithoutValues);
        setB("expandMultilineRows", src.expandMultilineRows);
      }
      setB("hideEmptyRows", info.def.compact);
      setB("hideEmptyRows", info.def.hideEmptyRows);
      setB("hideRowsWithoutValues", info.def.hideRowsWithoutValues);
      setB("hideRowsWithoutValues", info.def.removeEmptyRows || info.def.removeEmptyRowsAdvanced || info.def.removeEmptyRowsAdv);
      setB("expandMultilineRows", info.def.expandMultilineRows || info.def.expandArrayRows || info.def.arrayToRows);
      return out;
    };
    const setOpt = (k, name, v) => {
      const info = tdef(k, true);
      if (!info) return;
      const next = info.def._options && typeof info.def._options === "object" && !Array.isArray(info.def._options) ? info.def._options : {};
      next[name] = Boolean(v);
      info.def._options = next;
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
      } else if (f === "wrap" || f === "merge" || f === "bold" || f === "italic" || f === "underline") cell[f] = Boolean(v);
      else {
        const txt = String(v === undefined || v === null ? "" : v).trim();
        if (!txt) delete cell[f];
        else cell[f] = txt;
      }
      info.ranges[r] = cell;
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
        def._options = { hideEmptyRows: Boolean(opts.hideEmptyRows), hideRowsWithoutValues: Boolean(opts.hideRowsWithoutValues), expandMultilineRows: Boolean(opts.expandMultilineRows) };
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
    const syncJsonToDraft = () => {
      if (!jsonArea) return true;
      const raw = String(jsonArea.value || "").trim();
      if (!raw) { showErr("Template config JSON is empty."); return false; }
      let parsed = null;
      try { parsed = JSON.parse(raw); } catch (er) { showErr(`JSON parse error: ${er && er.message ? er.message : er}`); return false; }
      const chk = validateTemplateConfig(parsed);
      if (!chk.valid) { showErr(chk.errors.join("\n")); return false; }
      draft = cloneJsonValue(parsed);
      if (!draft || typeof draft !== "object" || Array.isArray(draft)) { showErr("Cannot load JSON draft."); return false; }
      if (!draft.templates || typeof draft.templates !== "object" || Array.isArray(draft.templates)) draft.templates = {};
      if (!Object.keys(draft.templates).length) draft.templates.DEFAULT = {};
      if (!Object.prototype.hasOwnProperty.call(draft.templates, selKey)) selKey = Object.keys(draft.templates)[0] || "DEFAULT";
      selRange = "";
      showErr("");
      return true;
    };

    function renderForm() {
      els.templateConfigJson = null;
      const root = document.createElement("div");
      root.className = "template-config-form";
      const keys = Object.keys(draft.templates);
      if (!keys.includes(selKey)) selKey = keys[0] || "DEFAULT";
      const keyRow = document.createElement("div");
      keyRow.className = "template-config-key-row";
      root.appendChild(keyRow);
      const lbl = document.createElement("label");
      lbl.className = "muted";
      lbl.textContent = "Template Key";
      keyRow.appendChild(lbl);
      const sel = document.createElement("select");
      sel.className = "template-config-select";
      for (const k of keys) { const o = document.createElement("option"); o.value = k; o.textContent = k; sel.appendChild(o); }
      sel.value = selKey;
      sel.addEventListener("change", () => { selKey = sel.value; selRange = ""; renderActive(); });
      keyRow.appendChild(sel);
      const mkBtn = (txt, fn) => { const b = document.createElement("button"); b.type = "button"; b.className = "secondary"; b.textContent = txt; b.addEventListener("click", fn); keyRow.appendChild(b); };
      mkBtn("Add Key", () => { let i = 1; let k = "NEW_TEMPLATE"; while (Object.prototype.hasOwnProperty.call(draft.templates, k)) { i += 1; k = `NEW_TEMPLATE_${i}`; } draft.templates[k] = {}; selKey = k; selRange = ""; showErr(""); renderActive(); });
      mkBtn("Clone Key", () => { const src = draft.templates[selKey]; if (!src || typeof src !== "object") { showErr("Current template key is invalid."); return; } let i = 1; let k = `${selKey}_COPY`; while (Object.prototype.hasOwnProperty.call(draft.templates, k)) { i += 1; k = `${selKey}_COPY_${i}`; } draft.templates[k] = cloneJsonValue(src); selKey = k; selRange = ""; showErr(""); renderActive(); });
      mkBtn("Delete Key", () => { if (Object.keys(draft.templates).length <= 1) { showErr("At least one template key is required."); return; } if (!confirm(`Delete template key \"${selKey}\"?`)) return; delete draft.templates[selKey]; selKey = Object.keys(draft.templates)[0] || "DEFAULT"; selRange = ""; showErr(""); renderActive(); });
      const opts = readOpts(selKey);
      const optRow = document.createElement("div");
      optRow.className = "template-config-options-row";
      root.appendChild(optRow);
      const optToggle = (name) => { const l = document.createElement("label"); l.className = "toggle"; const c = document.createElement("input"); c.type = "checkbox"; c.checked = Boolean(opts[name]); c.addEventListener("change", () => { setOpt(selKey, name, c.checked); showErr(""); }); l.appendChild(c); l.appendChild(document.createTextNode(name)); optRow.appendChild(l); };
      optToggle("hideEmptyRows"); optToggle("hideRowsWithoutValues"); optToggle("expandMultilineRows");
      const tool = document.createElement("div");
      tool.className = "template-config-ranges-toolbar";
      root.appendChild(tool);
      const mkToolBtn = (txt, fn) => { const b = document.createElement("button"); b.type = "button"; b.className = "secondary"; b.textContent = txt; b.addEventListener("click", fn); tool.appendChild(b); };
      mkToolBtn("Add Range", () => { const info = tdef(selKey, true); if (!info) return; const k = nextRangeKey(selKey); info.ranges[k] = { text: "" }; selRange = k; showErr(""); renderActive(); });
      mkToolBtn("Duplicate", () => { if (!selRange) { showErr("Select a range to duplicate."); return; } const info = tdef(selKey, true); if (!info || !Object.prototype.hasOwnProperty.call(info.ranges, selRange)) { showErr("Selected range not found."); return; } const k = nextRangeKey(selKey); info.ranges[k] = cloneJsonValue(info.ranges[selRange]) || {}; selRange = k; showErr(""); renderActive(); });
      mkToolBtn("Delete", () => { if (!selRange) { showErr("Select a range to delete."); return; } const info = tdef(selKey, true); if (!info || !Object.prototype.hasOwnProperty.call(info.ranges, selRange)) { showErr("Selected range not found."); return; } delete info.ranges[selRange]; selRange = ""; showErr(""); renderActive(); });
      mkToolBtn("Sort by position", () => { const info = tdef(selKey, true); if (!info) return; const entries = listRanges(selKey); const ok = []; const bad = []; for (const e of entries) { try { const p = parseRangeKey(e.rangeKey); ok.push({ ...e, p }); } catch { bad.push(e); } } ok.sort((a, b) => (a.p.r1 - b.p.r1) || (a.p.c1 - b.p.c1) || (a.p.r2 - b.p.r2) || (a.p.c2 - b.p.c2)); const next = {}; for (const e of ok) next[e.rangeKey] = info.ranges[e.rangeKey]; for (const e of bad) next[e.rangeKey] = info.ranges[e.rangeKey]; if (info.hasRanges) info.def.ranges = next; else { const keep = {}; for (const k of Object.keys(info.def)) if (isOptionKey(k)) keep[k] = info.def[k]; for (const [k, v] of Object.entries(next)) keep[k] = v; for (const k of Object.keys(info.def)) delete info.def[k]; for (const [k, v] of Object.entries(keep)) info.def[k] = v; } renderActive(); });
      const sLbl = document.createElement("span");
      sLbl.className = "muted";
      sLbl.textContent = selRange ? `Selected: ${selRange}` : "Selected: (none)";
      tool.appendChild(sLbl);
      const wrap = document.createElement("div");
      wrap.className = "template-config-ranges-wrap";
      root.appendChild(wrap);
      const table = document.createElement("table");
      table.className = "template-config-ranges-table";
      const th = document.createElement("thead");
      const trh = document.createElement("tr");
      for (const t of ["Sel", "Range", ...FIELD_DEFS.map((f) => f.label)]) { const c = document.createElement("th"); c.textContent = t; trh.appendChild(c); }
      th.appendChild(trh);
      table.appendChild(th);
      const tb = document.createElement("tbody");
      const entries = listRanges(selKey);
      if (!selRange && entries.length) selRange = entries[0].rangeKey;
      for (const e of entries) {
        const tr = document.createElement("tr");
        if (e.rangeKey === selRange) tr.classList.add("is-selected");
        const tdSel = document.createElement("td");
        const rd = document.createElement("input");
        rd.type = "radio";
        rd.name = "templateRangeSelect";
        rd.checked = e.rangeKey === selRange;
        rd.addEventListener("change", () => { selRange = e.rangeKey; renderActive(); });
        tdSel.appendChild(rd);
        tr.appendChild(tdSel);
        const tdKey = document.createElement("td");
        const inpKey = document.createElement("input");
        inpKey.type = "text";
        inpKey.className = "template-config-cell-input";
        inpKey.value = e.rangeKey;
        inpKey.addEventListener("focus", () => { selRange = e.rangeKey; });
        inpKey.addEventListener("blur", () => {
          const oldK = String(e.rangeKey || "").trim();
          const newK = String(inpKey.value || "").trim().toUpperCase();
          if (!newK || oldK === newK) return;
          const info = tdef(selKey, true);
          if (!info || !Object.prototype.hasOwnProperty.call(info.ranges, oldK)) return;
          if (Object.prototype.hasOwnProperty.call(info.ranges, newK)) { showErr(`Range ${newK} already exists.`); renderActive(); return; }
          const v = info.ranges[oldK]; delete info.ranges[oldK]; info.ranges[newK] = v; if (selRange === oldK) selRange = newK; showErr(""); renderActive();
        });
        tdKey.appendChild(inpKey);
        tr.appendChild(tdKey);
        for (const f of FIELD_DEFS) {
          const td = document.createElement("td");
          if (f.kind === "bool") {
            const c = document.createElement("input");
            c.type = "checkbox";
            c.checked = Boolean(e.cell[f.key]);
            c.addEventListener("change", () => { selRange = e.rangeKey; setCell(selKey, e.rangeKey, f.key, c.checked); showErr(""); });
            td.appendChild(c);
          } else if (f.kind === "select") {
            const s = document.createElement("select");
            s.className = "template-config-select";
            for (const ov of f.opts || []) { const o = document.createElement("option"); o.value = ov; o.textContent = ov || "(default)"; s.appendChild(o); }
            s.value = String(e.cell[f.key] === undefined || e.cell[f.key] === null ? "" : e.cell[f.key]);
            s.addEventListener("change", () => { selRange = e.rangeKey; setCell(selKey, e.rangeKey, f.key, s.value); showErr(""); });
            td.appendChild(s);
          } else {
            const i = document.createElement("input");
            i.type = "text";
            i.className = "template-config-cell-input";
            i.value = String(e.cell[f.key] === undefined || e.cell[f.key] === null ? "" : e.cell[f.key]);
            i.addEventListener("input", () => { selRange = e.rangeKey; setCell(selKey, e.rangeKey, f.key, i.value); if (f.key === "font size" && i.value.trim() && !Number.isFinite(Number(i.value))) showErr(`[${selKey}] ${e.rangeKey}: Font size must be numeric.`); else showErr(""); });
            td.appendChild(i);
          }
          tr.appendChild(td);
        }
        tb.appendChild(tr);
      }
      table.appendChild(tb);
      wrap.appendChild(table);
      const check = validateDraft();
      if (!check.ok) { const sum = document.createElement("div"); sum.className = "template-error"; sum.textContent = check.messages.join("\n"); root.appendChild(sum); }
      host.replaceChildren(root);
    }

    function renderJson() {
      const root = document.createElement("div");
      root.className = "template-config-json-tab";
      const hint = document.createElement("div");
      hint.className = "muted";
      hint.textContent = "Advanced mode. Apply validates schema and keeps compatibility.";
      root.appendChild(hint);
      const ta = document.createElement("textarea");
      ta.className = "template-config-json";
      ta.spellcheck = false;
      ta.placeholder = "Template config JSON...";
      ta.value = safeJson(draft, true);
      ta.addEventListener("keydown", (ev) => { if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") { ev.preventDefault(); applyFromModal(); } });
      root.appendChild(ta);
      host.replaceChildren(root);
      jsonArea = ta;
      els.templateConfigJson = ta;
      setTimeout(() => { ta.focus(); ta.setSelectionRange(0, 0); }, 0);
    }

    function renderActive() {
      formBtn.classList.toggle("active", activeTab === "form");
      jsonBtn.classList.toggle("active", activeTab === "json");
      if (activeTab === "json") renderJson();
      else { jsonArea = null; renderForm(); }
    }

    function switchTab(next) {
      const tab = next === "json" ? "json" : "form";
      if (tab === activeTab) return;
      if (activeTab === "json" && !syncJsonToDraft()) return;
      activeTab = tab;
      showErr("");
      renderActive();
    }

    function applyFromModal() {
      if (activeTab === "json" && !syncJsonToDraft()) return;
      const preCheck = validateDraft();
      if (!preCheck.ok) { showErr(preCheck.messages.join("\n")); if (activeTab !== "form") { activeTab = "form"; renderActive(); } return; }
      const nextConfig = serializeDraft();
      const chk = validateTemplateConfig(nextConfig);
      if (!chk.valid) { showErr(chk.errors.join("\n")); return; }
      const ok = applyTemplateConfigObject(nextConfig, { save: true });
      if (!ok) { const fallback = String((els.templateConfigError && els.templateConfigError.textContent) || "").trim(); if (fallback) showErr(fallback); return; }
      showErr("");
      setError("");
      closeTemplateDynamicModal();
    }

    formBtn.addEventListener("click", () => switchTab("form"));
    jsonBtn.addEventListener("click", () => switchTab("json"));
    applyBtn.addEventListener("click", applyFromModal);
    modal.root.addEventListener("keydown", (ev) => { if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") { ev.preventDefault(); applyFromModal(); } });
    renderActive();
    modal.setCleanup(() => {
      els.templateConfigJson = prevJsonEl || null;
      els.templateConfigError = prevErrEl || null;
      jsonArea = null;
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
    const textPart = opts.textPart && typeof opts.textPart === "object" ? opts.textPart : {};
    const descPart = opts.descPart && typeof opts.descPart === "object" ? opts.descPart : {};

    const templateKey = String(textPart.templateKey || "").trim();
    const rangeKey = String(textPart.rangeKey || "").trim();
    const objectType = String(textPart.objectType || "").trim();
    const currentText = String(textPart.currentText === undefined || textPart.currentText === null ? "" : textPart.currentText);
    const onSaveText = typeof textPart.onSaveText === "function" ? textPart.onSaveText : null;

    const legacyDecl = descPart.decl && typeof descPart.decl === "object" ? descPart.decl : null;
    const token = String(descPart.token || "").trim();
    const currentDesc = String(descPart.currentDesc === undefined || descPart.currentDesc === null ? "" : descPart.currentDesc);
    const skipNormalize = Boolean(descPart.skipNormalize);
    const onSaveDesc = typeof descPart.onSaveDesc === "function" ? descPart.onSaveDesc : null;

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
      if (!declKey && typeof getDeclOverrideStorageKey === "function") {
        declKey = String(getDeclOverrideStorageKey(decl) || "").trim();
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

    const rawDeclCandidates = Array.isArray(descPart.declCandidates)
      ? descPart.declCandidates
      : [];
    let declCandidates = rawDeclCandidates
      .map((candidate, index) => normalizeDeclCandidate(candidate, index))
      .filter(Boolean);

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
      token ? `Token: ${token}` : ""
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

    const textTabBtn = document.createElement("button");
    textTabBtn.type = "button";
    textTabBtn.className = "secondary";
    textTabBtn.textContent = "Template Text";
    tabBar.appendChild(textTabBtn);

    const descTabBtn = document.createElement("button");
    descTabBtn.type = "button";
    descTabBtn.className = "secondary";
    descTabBtn.textContent = "Description";
    tabBar.appendChild(descTabBtn);

    const tabContent = document.createElement("div");
    modal.body.appendChild(tabContent);

    let activeTab = "text";
    let textValue = currentText;
    let activeTextarea = null;
    const descDraftByKey = new Map();
    for (const candidate of declCandidates) {
      descDraftByKey.set(candidate.declKey, {
        text: String(candidate.currentDesc || ""),
        skipNormalize: Boolean(candidate.skipNormalize)
      });
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
        return { text: "", skipNormalize: false };
      }
      const existing = descDraftByKey.get(selected.declKey);
      if (existing && typeof existing === "object") {
        return {
          text: String(existing.text || ""),
          skipNormalize: Boolean(existing.skipNormalize)
        };
      }
      return { text: "", skipNormalize: false };
    };

    const setDescDraft = (patch) => {
      const selected = getSelectedDeclCandidate();
      if (!selected) {
        return;
      }
      const current = getDescDraft();
      const next = {
        text: patch && Object.prototype.hasOwnProperty.call(patch, "text") ? String(patch.text || "") : current.text,
        skipNormalize: patch && Object.prototype.hasOwnProperty.call(patch, "skipNormalize") ? Boolean(patch.skipNormalize) : current.skipNormalize
      };
      descDraftByKey.set(selected.declKey, next);
    };

    const syncActiveInputState = () => {
      if (activeTab === "text" && activeTextarea) {
        textValue = String(activeTextarea.value || "");
      }
      if (activeTab === "desc" && activeTextarea) {
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

      if (hasDecl && declCandidates.length > 1) {
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

      const descArea = document.createElement("textarea");
      descArea.className = "template-config-json";
      descArea.spellcheck = false;
      descArea.placeholder = "Description override...";

      const skipWrap = document.createElement("label");
      skipWrap.className = "toggle";
      const skipInput = document.createElement("input");
      skipInput.type = "checkbox";
      skipWrap.appendChild(skipInput);
      skipWrap.appendChild(document.createTextNode("Skip normalize"));

      if (!hasDecl) {
        descInfo.textContent = "Khong tim thay decl cho o nay.";
        descArea.disabled = true;
        skipInput.disabled = true;
        descArea.value = "";
        skipInput.checked = false;
      } else {
        const selected = getSelectedDeclCandidate();
        const draft = getDescDraft();
        descArea.value = draft.text;
        skipInput.checked = draft.skipNormalize;
        descInfo.textContent = selected ? `Decl: ${selected.label}` : "Decl: (unknown)";
        skipInput.addEventListener("change", () => {
          setDescDraft({ skipNormalize: Boolean(skipInput.checked) });
        });
      }

      tabContent.appendChild(descArea);
      tabContent.appendChild(skipWrap);
      activeTextarea = descArea;
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

    const submit = () => {
      showInlineError("");
      if (!templateKey || !rangeKey || !onSaveText) {
        showInlineError("Cannot edit this template cell: missing template key/range.");
        return;
      }
      syncActiveInputState();

      let textResult = null;
      try {
        textResult = normalizeSaveResult(
          onSaveText({ text: String(textValue || "") }),
          "Save template text failed."
        );
      } catch (err) {
        showInlineError(err && err.message ? err.message : String(err));
        return;
      }
      if (!textResult.ok) {
        const fallback = String((els.templateConfigError && els.templateConfigError.textContent) || "").trim();
        showInlineError(textResult.error || fallback || "Save template text failed.");
        return;
      }

      if (hasDecl) {
        const selected = getSelectedDeclCandidate();
        const draft = getDescDraft();
        let descResult = null;
        try {
          descResult = normalizeSaveResult(onSaveDesc({
            decl: selected ? selected.decl : null,
            declKey: selected ? selected.declKey : "",
            text: String(draft.text || ""),
            skipNormalize: Boolean(draft.skipNormalize)
          }), "Save description failed.");
        } catch (err) {
          showInlineError(err && err.message ? err.message : String(err));
          return;
        }
        if (!descResult.ok) {
          showInlineError(descResult.error || "Save description failed.");
          return;
        }
      }

      closeTemplateDynamicModal();
    };

    saveBtn.addEventListener("click", submit);
  }
  window.openTemplateCellUnifiedEditModal = openTemplateCellUnifiedEditModal;

  function getInputGotoControls() {
    return {
      input: document.getElementById("inputGotoLine"),
      button: document.getElementById("inputGotoLineBtn")
    };
  }

  function getCurrentInputLineCount() {
    if (Number.isFinite(state.inputLineCount) && state.inputLineCount > 0) {
      return Math.max(1, Number(state.inputLineCount));
    }
    if (typeof countInputLines === "function") {
      return Math.max(1, Number(countInputLines((els.inputText && els.inputText.value) || "")) || 1);
    }
    const text = String((els.inputText && els.inputText.value) || "");
    return Math.max(1, text.split(/\r\n|\r|\n/).length);
  }

  function goToInputLine(lineNumber) {
    if (!els.inputText) {
      return { line: 1, total: 1 };
    }

    const totalLines = getCurrentInputLineCount();
    const next = Number.isFinite(Number(lineNumber)) ? Number(lineNumber) : 1;
    const targetLine = Math.max(1, Math.min(totalLines, Math.floor(next)));

    if (typeof selectCodeLines === "function") {
      selectCodeLines(targetLine, targetLine);
      return { line: targetLine, total: totalLines };
    }

    const text = els.inputText.value || "";
    const offsets = Array.isArray(state.inputLineOffsets) && state.inputLineOffsets.length
      ? state.inputLineOffsets
      : computeLineOffsets(text);
    const startOffset = Number(offsets[Math.max(0, targetLine - 1)]) || 0;
    const nextOffsetIndex = Math.min(offsets.length - 1, targetLine);
    const nextOffset = Number(offsets[nextOffsetIndex]);
    const endOffset = Number.isFinite(nextOffset) && nextOffset > startOffset
      ? Math.max(startOffset, nextOffset - 1)
      : startOffset;

    els.inputText.focus();
    els.inputText.selectionStart = startOffset;
    els.inputText.selectionEnd = endOffset;

    let lineHeight = 18;
    try {
      lineHeight = Number.parseFloat(window.getComputedStyle(els.inputText).lineHeight || "18") || 18;
    } catch {
      lineHeight = 18;
    }
    const targetScrollTop = Math.max(
      0,
      Math.round(((targetLine - 1) * lineHeight) - ((Number(els.inputText.clientHeight) || 0) * 0.35))
    );
    els.inputText.scrollTop = targetScrollTop;
    if (typeof syncInputGutterScroll === "function") {
      syncInputGutterScroll();
    }

    return { line: targetLine, total: totalLines };
  }

  function submitInputGotoLine() {
    const controls = getInputGotoControls();
    if (!controls.input) {
      return;
    }

    const raw = String(controls.input.value || "").trim();
    if (!raw) {
      setError("Enter a line number.");
      controls.input.focus();
      return;
    }

    const parsed = Number(raw);
    if (!Number.isFinite(parsed)) {
      setError("Invalid line number.");
      controls.input.focus();
      controls.input.select();
      return;
    }

    const result = goToInputLine(parsed);
    controls.input.value = String(result.line);
    setError("");
  }

  function initInputGotoLineControls() {
    const controls = getInputGotoControls();
    if (controls.button) {
      controls.button.addEventListener("click", submitInputGotoLine);
    }
    if (controls.input) {
      controls.input.addEventListener("keydown", (ev) => {
        if (ev.key === "Enter") {
          ev.preventDefault();
          submitInputGotoLine();
        }
      });
    }
  }

  function parseFromTextarea(fileName) {
    const content = els.inputText.value || "";
    const trimmed = content.trim();
    const isJsonInput = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonInput ? "json" : "abap";
    rebuildInputGutter();
    if (!trimmed) {
      setError("Input is empty.");
      setOutputMessage("No data loaded.");
      return;
    }

    state.inputLineOffsets = computeLineOffsets(content);

    if (isJsonInput) {
      try {
        const json = JSON.parse(trimmed);
        const parsed = normalizeParsedJson(json);
        if (!parsed) {
          throw new Error("JSON parsed, but shape is not { file, objects[] } or objects[].");
        }
        state.data = parsed;
      } catch (err) {
        setError(`JSON parse error: ${err && err.message ? err.message : err}`);
        setOutputMessage("No data loaded.");
        return;
      }
    } else {
      if (!window.AbapParser || typeof window.AbapParser.parseAbapText !== "function") {
        setError("AbapParser not loaded.");
        setOutputMessage("No data loaded.");
        return;
      }

      try {
        const builtInConfigs = typeof window.AbapParser.getConfigs === "function" ? window.AbapParser.getConfigs() : [];
        const customConfigs = getCustomConfigs();
        const configs = [...customConfigs, ...builtInConfigs];
        state.data = window.AbapParser.parseAbapText(content, configs, fileName || "");
      } catch (err) {
        setError(`Parse error: ${err && err.message ? err.message : err}`);
        setOutputMessage("No data loaded.");
        return;
      }
    }

    state.collapsedIds.clear();
    state.selectedId = "";
    state.selectedTemplateIndex = "";
    state.renderObjects = buildRenderableObjects(state.data && state.data.objects, RENDER_TREE_OPTIONS);
    state.haystackById = buildSearchIndex(state.renderObjects);
    populateTypeFilter(state.renderObjects);
    refreshTemplateGuiFilterTypes();
    if (state.rightTab === "output") {
      renderOutput();
    } else if (state.rightTab === "descriptions") {
      renderDeclDescPanelUi();
    } else if (state.rightTab === "template") {
      renderTemplatePreview();
    }
  }

  function resetUi() {
    els.searchInput.value = "";
    els.typeFilter.value = "";
    els.showRaw.checked = true;
    els.showKeywords.checked = true;
    els.showValues.checked = true;
    els.showExtras.checked = false;
    state.collapsedIds.clear();
    state.selectedId = "";
  }

  function collapseAll() {
    state.collapsedIds.clear();
    walkObjects(state.renderObjects, (obj) => {
      const id = normalizeId(obj && obj.id);
      const children = Array.isArray(obj && obj.children) ? obj.children : [];
      if (id && children.length) {
        state.collapsedIds.add(id);
      }
    });
  }

  function init() {
    renderBuildInfo();
    state.descOverrides = loadDescOverrides();
    state.descOverridesLegacy = loadLegacyDescOverrides();
    state.customRules = loadCustomRules();
    state.settings = loadSettings();
    state.templateConfig = loadTemplateConfig();
    state.templatePreviewCache = null;
    applyTheme(loadTheme(), { save: false });
    initLayoutSplitter();

    if (els.templateKeyMode) {
      els.templateKeyMode.textContent = "AUTO: objectType -> DEFAULT";
    }
    syncTemplateEditorFromState();
    setTemplateConfigError("");

    if (els.inputText && !els.inputText.value.trim()) {
      els.inputText.value = SAMPLE_ABAP;
    }

    rebuildInputGutter();
    initInputGotoLineControls();
    initTemplateGuiFilterControls();

    if (els.inputText) {
      els.inputText.addEventListener("input", rebuildInputGutter);
      els.inputText.addEventListener("scroll", syncInputGutterScroll);
    }
    if (els.output && typeof handleOutputVirtualScroll === "function") {
      els.output.addEventListener("scroll", handleOutputVirtualScroll, { passive: true });
    }
    if (els.templatePreviewOutput && typeof handleTemplateVirtualScroll === "function") {
      els.templatePreviewOutput.addEventListener("scroll", handleTemplateVirtualScroll, { passive: true });
    }

    if (els.inputGutter) {
      els.inputGutter.addEventListener("click", onInputGutterClick);
      els.inputGutter.addEventListener("wheel", (ev) => {
        if (!els.inputText) {
          return;
        }
        els.inputText.scrollTop += ev.deltaY;
        syncInputGutterScroll();
        ev.preventDefault();
      }, { passive: false });
    }

    if (els.themeToggle) {
      els.themeToggle.addEventListener("change", () => {
        applyTheme(els.themeToggle.checked ? "dark" : "light");
      });
    }

    els.parseBtn.addEventListener("click", () => parseFromTextarea("input.abap"));
    els.searchInput.addEventListener("input", renderOutput);
    els.typeFilter.addEventListener("change", renderOutput);
    els.showRaw.addEventListener("change", renderOutput);
    els.showKeywords.addEventListener("change", renderOutput);
    els.showValues.addEventListener("change", renderOutput);
    els.showExtras.addEventListener("change", renderOutput);

    els.expandAllBtn.addEventListener("click", () => {
      state.collapsedIds.clear();
      renderOutput();
    });

    els.collapseAllBtn.addEventListener("click", () => {
      if (!state.data) {
        return;
      }
      collapseAll();
      renderOutput();
    });

    els.clearFiltersBtn.addEventListener("click", () => {
      resetUi();
      renderOutput();
    });

    els.descBtn.addEventListener("click", () => {
      setRightTab("descriptions");
    });

    if (els.rightTabOutputBtn) {
      els.rightTabOutputBtn.addEventListener("click", () => setRightTab("output"));
    }

    if (els.rightTabTemplateBtn) {
      els.rightTabTemplateBtn.addEventListener("click", () => setRightTab("template"));
    }

    if (els.rightTabDescBtn) {
      els.rightTabDescBtn.addEventListener("click", () => setRightTab("descriptions"));
    }

    if (els.templateCopyAllBtn) {
      els.templateCopyAllBtn.addEventListener("click", () => {
        copyAllTemplateBlocks()
          .then(() => setError(""))
          .catch((err) => setError(`Copy failed: ${err && err.message ? err.message : err}`));
      });
    }

    if (els.templateResetBtn) {
      els.templateResetBtn.addEventListener("click", resetTemplateConfig);
    }

    if (els.templateExportBtn) {
      els.templateExportBtn.addEventListener("click", exportTemplateConfig);
    }

    if (els.templateImportBtn && els.templateImportInput) {
      els.templateImportBtn.addEventListener("click", () => {
        els.templateImportInput.click();
      });
    }

    if (els.templateImportInput) {
      els.templateImportInput.addEventListener("change", async (ev) => {
        const file = ev && ev.target && ev.target.files ? ev.target.files[0] : null;
        if (!file) {
          return;
        }
        await importTemplateConfigFromFile(file);
        els.templateImportInput.value = "";
      });
    }

    if (els.templateApplyBtn) {
      els.templateApplyBtn.addEventListener("click", openTemplateConfigModal);
    }

    const templateFilterModalBtn = document.getElementById("templateFilterModalBtn");
    if (templateFilterModalBtn) {
      templateFilterModalBtn.addEventListener("click", openTemplateFilterModal);
    }

    if (els.declDescJsonBtn) {
      els.declDescJsonBtn.addEventListener("click", () => {
        openJsonModal({
          storageKey: DESC_STORAGE_KEY_V2,
          overrides: state.descOverrides,
          legacyStorageKey: DESC_STORAGE_KEY_LEGACY_V1,
          legacyOverrides: state.descOverridesLegacy,
          registry: window.AbapVarDescriptions || {}
        });
      });
    }

    if (els.declDescSearch) {
      els.declDescSearch.addEventListener("input", renderDeclDescPanelUi);
    }

    if (els.declDescMissingOnly) {
      els.declDescMissingOnly.addEventListener("change", renderDeclDescPanelUi);
    }

    if (els.rulesBtn) {
      els.rulesBtn.addEventListener("click", openRulesModal);
    }

    if (els.settingsBtn) {
      els.settingsBtn.addEventListener("click", openSettingsModal);
    }

    if (els.exportXmlBtn) {
      els.exportXmlBtn.addEventListener("click", () => {
        if (!state.data || !Array.isArray(state.data.objects)) {
          setError("No parsed data to export. Click Render first.");
          return;
        }
        setError("");
        openTextModal("Export XML", buildAbapFlowXml(state.data));
      });
    }

    els.fileInput.addEventListener("change", async (ev) => {
      const file = ev.target && ev.target.files ? ev.target.files[0] : null;
      if (!file) {
        return;
      }
      const text = await file.text();
      els.inputText.value = text;
      parseFromTextarea(file.name || "");
    });

    if (els.rulesCloseBtn) {
      els.rulesCloseBtn.addEventListener("click", closeRulesModal);
    }
    if (els.rulesModal) {
      els.rulesModal.addEventListener("click", (ev) => {
        if (ev.target === els.rulesModal) {
          closeRulesModal();
        }
      });
    }

    if (els.settingsCloseBtn) {
      els.settingsCloseBtn.addEventListener("click", closeSettingsModal);
    }
    if (els.settingsSaveBtn) {
      els.settingsSaveBtn.addEventListener("click", () => {
        applySettingsFromModal();
        closeSettingsModal();
      });
    }
    if (els.settingsResetBtn) {
      els.settingsResetBtn.addEventListener("click", resetSettingsToDefault);
    }
    if (els.settingsModal) {
      els.settingsModal.addEventListener("click", (ev) => {
        if (ev.target === els.settingsModal) {
          closeSettingsModal();
        }
      });
    }
    if (els.rulesNewBtn) {
      els.rulesNewBtn.addEventListener("click", startNewRule);
    }
    if (els.rulesSaveBtn) {
      els.rulesSaveBtn.addEventListener("click", saveRuleFromEditor);
    }
    if (els.rulesDeleteBtn) {
      els.rulesDeleteBtn.addEventListener("click", deleteActiveRule);
    }
    if (els.rulesDownloadBtn) {
      els.rulesDownloadBtn.addEventListener("click", downloadRuleFromEditor);
    }
    if (els.rulesSelect) {
      els.rulesSelect.addEventListener("change", () => {
        const id = els.rulesSelect.value || "";
        if (!id) {
          startNewRule();
          return;
        }
        selectRule(id);
      });
    }
    if (els.rulesTemplate) {
      els.rulesTemplate.addEventListener("change", () => {
        if (!state.activeRuleId) {
          startNewRule();
        }
      });
    }
    if (els.rulesJson) {
      els.rulesJson.addEventListener("keydown", (ev) => {
        if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
          ev.preventDefault();
          saveRuleFromEditor();
        }
      });
    }

    els.jsonCloseBtn.addEventListener("click", closeJsonModal);
    els.jsonModal.addEventListener("click", (ev) => {
      if (ev.target === els.jsonModal) {
        closeJsonModal();
      }
    });
    els.jsonCopyBtn.addEventListener("click", () => {
      copyJsonToClipboard().catch((err) => setError(err && err.message ? err.message : err));
    });

    els.editCancelBtn.addEventListener("click", closeEditModal);
    els.editModal.addEventListener("click", (ev) => {
      if (ev.target === els.editModal) {
        closeEditModal();
      }
    });
    els.editSaveBtn.addEventListener("click", () => {
      applyEditModal("save");
      closeEditModal();
    });
    els.editClearBtn.addEventListener("click", () => {
      applyEditModal("clear");
      closeEditModal();
    });

    const onEditKeydown = (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        applyEditModal("save");
        closeEditModal();
      }
    };

    els.editDesc.addEventListener("keydown", onEditKeydown);
    if (els.editStructDesc) {
      els.editStructDesc.addEventListener("keydown", onEditKeydown);
    }
    if (els.editItemDesc) {
      els.editItemDesc.addEventListener("keydown", onEditKeydown);
    }

    window.addEventListener("keydown", (ev) => {
      if (ev.key !== "Escape") {
        return;
      }

      if (isTemplateDynamicModalOpen()) {
        closeTemplateDynamicModal();
        return;
      }

      if (!els.editModal.hidden) {
        closeEditModal();
        return;
      }

      if (els.rulesModal && !els.rulesModal.hidden) {
        closeRulesModal();
        return;
      }

      if (!els.jsonModal.hidden) {
        closeJsonModal();
        return;
      }

      if (state.rightTab === "descriptions" || state.rightTab === "template") {
        setRightTab("output");
        return;
      }
    });

    setRightTab(state.rightTab);
    parseFromTextarea("sample.abap");
  }

window.AbapViewerModules.factories = window.AbapViewerModules.factories || {};
window.AbapViewerModules.factories["05-main"] = function registerMain(runtime) {
  const targetRuntime = runtime || (window.AbapViewerRuntime = window.AbapViewerRuntime || {});
  targetRuntime.api = targetRuntime.api || {};
  targetRuntime.api.parseFromTextarea = parseFromTextarea;
  targetRuntime.api.init = init;

  window.AbapViewerModules.start = function startAbapViewer() {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", init, { once: true });
    } else {
      init();
    }
  };
  window.AbapViewerModules.parts["05-main"] = true;
};
window.AbapViewerModules.factories["05-main"](window.AbapViewerRuntime);

