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

  function jumpInputToCodeRange(lineStart, lineEnd) {
    const start = Math.max(1, Number(lineStart) || 1);
    const end = Math.max(start, Number(lineEnd) || start);
    if (typeof selectCodeLines === "function") {
      selectCodeLines(start, end);
    } else if (els.inputText) {
      const text = String(els.inputText.value || "");
      const lines = text.split(/\r\n|\r|\n/);
      const toOffset = (lineNo) => {
        const line = Math.max(1, Math.min(lines.length || 1, Number(lineNo) || 1));
        let offset = 0;
        for (let i = 0; i < line - 1; i += 1) {
          offset += lines[i].length + 1;
        }
        return offset;
      };
      const startOffset = toOffset(start);
      const endOffset = toOffset(end + 1);
      els.inputText.focus();
      els.inputText.setSelectionRange(startOffset, Math.max(startOffset, endOffset));
    }

    if (!els.inputText) {
      return;
    }
    const lineHeight = typeof getInputGutterLineHeightPx === "function"
      ? Math.max(12, Number(getInputGutterLineHeightPx()) || 18)
      : 18;
    const viewportHeight = Math.max(0, Number(els.inputText.clientHeight) || 0);
    const offsetTop = Math.max(0, Math.round(viewportHeight * 0.28));
    const targetTop = Math.max(0, Math.round(((start - 1) * lineHeight) - offsetTop));
    els.inputText.scrollTop = targetTop;
    if (typeof syncInputGutterScroll === "function") {
      syncInputGutterScroll();
    }
  }

  function interceptOutputCodeButtonClick(ev) {
    if (!els.output || !ev || !ev.target || !(ev.target instanceof Element)) {
      return;
    }
    const btn = ev.target.closest("button.btn-ghost");
    if (!btn || !els.output.contains(btn)) {
      return;
    }
    if (String(btn.textContent || "").trim().toLowerCase() !== "code") {
      return;
    }
    const card = btn.closest(".card[data-id]");
    if (!card) {
      return;
    }
    const objId = String(card.getAttribute("data-id") || "").trim();
    const obj = findRenderObjectById(objId);
    const lineStart = Number(obj && obj.lineStart) || Number(card.getAttribute("data-line-start")) || 0;
    const lineEnd = Number(obj && obj.block && obj.block.lineEnd) || lineStart;
    if (lineStart <= 0) {
      return;
    }
    ev.preventDefault();
    ev.stopPropagation();
    if (typeof ev.stopImmediatePropagation === "function") {
      ev.stopImmediatePropagation();
    }
    jumpInputToCodeRange(lineStart, lineEnd);
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
    if (lineStart <= 0) {
      return;
    }
    ev.preventDefault();
    ev.stopPropagation();
    if (typeof ev.stopImmediatePropagation === "function") {
      ev.stopImmediatePropagation();
    }
    jumpInputToCodeRange(lineStart, lineEnd);
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
      ta.className = "template-config-json template-config-json-large";
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
    const normalizeEnabled = Boolean(state && state.settings && state.settings.normalizeDeclDesc);

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

    const buildDescDraftForCandidate = (candidate) => {
      const decl = candidate && candidate.decl && typeof candidate.decl === "object" ? candidate.decl : null;
      if (!decl) {
        return {
          mode: "single",
          text: String(candidate && candidate.currentDesc ? candidate.currentDesc : ""),
          skipNormalize: Boolean(candidate && candidate.skipNormalize)
        };
      }

      const itemEntry = getDeclOverrideEntrySafeByDecl(decl);
      const itemDisplayRaw = itemEntry.text
        ? normalizeDescValueForModal(decl, itemEntry.text, itemEntry.noNormalize)
        : (String(candidate && candidate.currentDesc ? candidate.currentDesc : "") || getAtomicEffectiveDescSafe(decl));
      const itemDisplay = stripStructPrefixForModalItemText(itemDisplayRaw, String(decl.structName || ""));

      if (!isStructFieldDeclForModal(decl)) {
        return {
          mode: "single",
          text: itemDisplay,
          skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
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
          skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
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
        skipNormalize: Boolean(itemEntry.noNormalize || (candidate && candidate.skipNormalize))
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
        return { mode: "single", text: "", skipNormalize: false };
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
            skipNormalize: Boolean(existing.skipNormalize)
          };
        }
        return {
          mode: "single",
          text: String(existing.text || ""),
          skipNormalize: Boolean(existing.skipNormalize)
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
        skipNormalize: patch && Object.prototype.hasOwnProperty.call(patch, "skipNormalize") ? Boolean(patch.skipNormalize) : Boolean(current.skipNormalize)
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

      if (!hasDecl) {
        descInfo.textContent = "Khong tim thay decl cho o nay.";
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
        if (draft.mode === "structField") {
          let structResult = null;
          try {
            structResult = normalizeSaveResult(onSaveDesc({
              decl: draft.structDecl || null,
              declKey: String(draft.structKey || ""),
              text: String(draft.structText || ""),
              skipNormalize: false
            }), "Save struct description failed.");
          } catch (err) {
            showInlineError(err && err.message ? err.message : String(err));
            return;
          }
          if (!structResult.ok) {
            showInlineError(structResult.error || "Save struct description failed.");
            return;
          }

          let itemResult = null;
          try {
            itemResult = normalizeSaveResult(onSaveDesc({
              decl: selected ? selected.decl : (draft.itemDecl || null),
              declKey: selected ? selected.declKey : String(draft.itemKey || ""),
              text: String(draft.itemText || ""),
              skipNormalize: Boolean(draft.skipNormalize)
            }), "Save item description failed.");
          } catch (err) {
            showInlineError(err && err.message ? err.message : String(err));
            return;
          }
          if (!itemResult.ok) {
            showInlineError(itemResult.error || "Save item description failed.");
            return;
          }
        } else {
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

  function isDeclLikeRecordForSynthetic(decl) {
    return Boolean(decl)
      && typeof decl === "object"
      && typeof decl.objectType === "string"
      && typeof decl.name === "string";
  }

  function normalizeDeclKeyTokenForSynthetic(value) {
    return String(value || "").trim().toUpperCase();
  }

  function makeDeclScopeNameKeyForSynthetic(scopeLabel, name) {
    const scope = normalizeDeclKeyTokenForSynthetic(scopeLabel);
    const declName = normalizeDeclKeyTokenForSynthetic(name);
    if (!scope || !declName) {
      return "";
    }
    return `${scope}:${declName}`;
  }

  function extractStructFieldRefForSynthetic(rawValue) {
    const text = String(rawValue || "").trim();
    if (!text) {
      return null;
    }

    const match = text.match(/(^|[^A-Za-z0-9_<>])([A-Za-z_][A-Za-z0-9_]*-[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z_][A-Za-z0-9_]*)*)/);
    if (!match || !match[2]) {
      return null;
    }

    const fullRef = String(match[2] || "").trim();
    const dash = fullRef.indexOf("-");
    if (dash <= 0 || dash >= fullRef.length - 1) {
      return null;
    }

    const structName = fullRef.slice(0, dash).trim();
    const fieldPath = fullRef.slice(dash + 1).trim();
    if (!structName || !fieldPath) {
      return null;
    }

    const structUpper = normalizeDeclKeyTokenForSynthetic(structName);
    // Skip ABAP system fields (SY-*) and field symbols (<fs>-*) in this synthetic flow.
    if (structUpper === "SY" || structName.startsWith("<")) {
      return null;
    }

    return { fullRef, structName, fieldPath, structUpper };
  }

  function collectScopeHintsFromObjectForSynthetic(obj) {
    const hints = new Set();
    if (!obj || typeof obj !== "object") {
      return hints;
    }

    const addScope = (decl) => {
      if (!isDeclLikeRecordForSynthetic(decl)) {
        return;
      }
      const scope = String(decl.scopeLabel || "").trim();
      if (scope) {
        hints.add(scope);
      }
    };

    const values = obj.values && typeof obj.values === "object" ? obj.values : null;
    if (values) {
      for (const entryOrList of Object.values(values)) {
        const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          addScope(entry.decl);
        }
      }
    }

    const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
    if (!extras) {
      return hints;
    }

    const addFromAssignSections = (container, sections) => {
      for (const sectionName of sections) {
        const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          addScope(entry.valueDecl);
          const origins = Array.isArray(entry.originDecls) ? entry.originDecls : [];
          for (const origin of origins) {
            addScope(origin);
          }
        }
      }
    };

    if (extras.callFunction) {
      addFromAssignSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"]);
    }
    if (extras.callMethod) {
      addFromAssignSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"]);
    }
    if (extras.performCall) {
      addFromAssignSections(extras.performCall, ["using", "changing", "tables"]);
    }

    if (extras.form && Array.isArray(extras.form.params)) {
      for (const param of extras.form.params) {
        const origins = param && Array.isArray(param.originDecls) ? param.originDecls : [];
        for (const origin of origins) {
          addScope(origin);
        }
      }
    }

    const conditionContainers = [];
    if (extras.ifCondition && Array.isArray(extras.ifCondition.conditions)) {
      conditionContainers.push(extras.ifCondition.conditions);
    }
    if (extras.performCall && Array.isArray(extras.performCall.ifConditions)) {
      conditionContainers.push(extras.performCall.ifConditions);
    }
    if (extras.select) {
      if (Array.isArray(extras.select.whereConditions)) {
        conditionContainers.push(extras.select.whereConditions);
      }
      if (Array.isArray(extras.select.havingConditions)) {
        conditionContainers.push(extras.select.havingConditions);
      }
    }
    for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
      if (extras[key] && Array.isArray(extras[key].conditions)) {
        conditionContainers.push(extras[key].conditions);
      }
    }

    for (const conditions of conditionContainers) {
      for (const clause of conditions) {
        if (!clause || typeof clause !== "object") {
          continue;
        }
        addScope(clause.leftOperandDecl);
        addScope(clause.rightOperandDecl);
      }
    }

    return hints;
  }

  function sanitizeDeclSyntheticIdToken(value) {
    return String(value || "")
      .trim()
      .replace(/\s+/g, "_")
      .replace(/[^A-Za-z0-9_:\-./[\]#]/g, "_")
      .toUpperCase();
  }

  function pickStructBaseDeclForSynthetic(candidate, context, index) {
    if (!candidate || !candidate.structUpper || !index || !(index.byNameUpper instanceof Map)) {
      return null;
    }

    const candidates = index.byNameUpper.get(candidate.structUpper) || [];
    const usableCandidates = candidates.filter((decl) => {
      const objectType = normalizeDeclKeyTokenForSynthetic(decl && decl.objectType);
      return objectType !== "STRUCT_FIELD" && objectType !== "PATH_DECL" && objectType !== "SYSTEM";
    });
    if (!usableCandidates.length) {
      return null;
    }

    const preferredScopes = context && context.scopeHints instanceof Set
      ? Array.from(context.scopeHints.values()).map((value) => String(value || "").trim()).filter(Boolean)
      : [];
    const preferredScopeSet = new Set(preferredScopes.map((value) => normalizeDeclKeyTokenForSynthetic(value)));
    const usageFile = context ? String(context.file || "").trim() : "";
    const usageLine = context ? (Number(context.lineStart) || 0) : 0;

    const scoreCandidate = (decl) => {
      const declScope = normalizeDeclKeyTokenForSynthetic(decl.scopeLabel);
      const declFile = String(decl.file || "").trim();
      const declLine = Number(decl.lineStart || 0) || 0;

      let score = 0;
      if (preferredScopeSet.size && preferredScopeSet.has(declScope)) {
        score += 1000000;
      }
      if (usageFile && declFile && usageFile === declFile) {
        score += 100000;
      }
      if (usageLine > 0 && declLine > 0) {
        if (declLine <= usageLine) {
          score += 10000;
          score += Math.max(0, 5000 - Math.abs(usageLine - declLine));
        } else {
          score += Math.max(0, 1000 - Math.abs(usageLine - declLine));
        }
      }
      return score;
    };

    let best = null;
    let bestScore = Number.NEGATIVE_INFINITY;
    for (const decl of usableCandidates) {
      const score = scoreCandidate(decl);
      if (score > bestScore) {
        best = decl;
        bestScore = score;
      }
    }
    return best;
  }

  function createSyntheticStructFieldDecl(baseDecl, candidate, context) {
    if (!isDeclLikeRecordForSynthetic(baseDecl) || !candidate) {
      return null;
    }

    const scopeLabel = String(baseDecl.scopeLabel || "").trim();
    if (!scopeLabel) {
      return null;
    }

    const fullRef = String(candidate.fullRef || "").trim();
    const fieldPath = String(candidate.fieldPath || "").trim();
    if (!fullRef || !fieldPath) {
      return null;
    }

    const idScope = sanitizeDeclSyntheticIdToken(scopeLabel) || "NO_SCOPE";
    const idStruct = sanitizeDeclSyntheticIdToken(baseDecl.name || candidate.structName || "") || "STRUCT";
    const idField = sanitizeDeclSyntheticIdToken(fieldPath) || "FIELD";
    const usageFile = context ? String(context.file || "").trim() : "";
    const usageLine = context ? (Number(context.lineStart) || 0) : 0;

    return {
      id: `SYNTH:STRUCT_FIELD:${idScope}:${idStruct}:${idField}`,
      objectType: "STRUCT_FIELD",
      name: fullRef,
      file: String(baseDecl.file || usageFile || ""),
      lineStart: Number(baseDecl.lineStart || usageLine) || null,
      raw: String(baseDecl.raw || ""),
      comment: "",
      scopeId: Number(baseDecl.scopeId || 0) || 0,
      scopeLabel,
      scopeType: String(baseDecl.scopeType || ""),
      scopeName: String(baseDecl.scopeName || ""),
      structId: baseDecl.id || null,
      structName: String(baseDecl.name || candidate.structName || ""),
      structObjectType: String(baseDecl.objectType || "STRUCT"),
      structLineStart: Number(baseDecl.lineStart || 0) || null,
      structRaw: String(baseDecl.raw || ""),
      structComment: String(baseDecl.comment || ""),
      fieldPath,
      synthetic: true
    };
  }

  function buildSyntheticDeclIndex(data) {
    const byNameUpper = new Map();
    const byScopeName = new Map();
    const sourceDecls = [];

    const pushDecl = (decl) => {
      if (!isDeclLikeRecordForSynthetic(decl)) {
        return;
      }
      sourceDecls.push(decl);
    };

    if (data && Array.isArray(data.decls)) {
      for (const decl of data.decls) {
        pushDecl(decl);
      }
    }

    if (data && Array.isArray(data.objects) && typeof walkObjects === "function") {
      walkObjects(data.objects, (obj) => {
        if (!obj || typeof obj !== "object") {
          return;
        }
        const values = obj.values && typeof obj.values === "object" ? obj.values : null;
        if (values) {
          for (const entryOrList of Object.values(values)) {
            const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
            for (const entry of list) {
              if (!entry || typeof entry !== "object") {
                continue;
              }
              pushDecl(entry.decl);
            }
          }
        }
      });
    }

    const addToMaps = (decl) => {
      const nameUpper = normalizeDeclKeyTokenForSynthetic(decl.name);
      if (nameUpper) {
        if (!byNameUpper.has(nameUpper)) {
          byNameUpper.set(nameUpper, []);
        }
        const list = byNameUpper.get(nameUpper);
        if (!list.includes(decl)) {
          list.push(decl);
        }
      }

      const key = makeDeclScopeNameKeyForSynthetic(decl.scopeLabel, decl.name);
      if (key && !byScopeName.has(key)) {
        byScopeName.set(key, decl);
      }
    };

    for (const decl of sourceDecls) {
      addToMaps(decl);
    }

    return { byNameUpper, byScopeName };
  }

  function ensureSyntheticStructFieldDeclForEntry(entry, options, index, createdDecls) {
    if (!entry || typeof entry !== "object" || !options || !index) {
      return false;
    }

    const targetProp = String(options.targetProp || "decl");
    const currentDecl = entry[targetProp];
    if (isDeclLikeRecordForSynthetic(currentDecl)) {
      return false;
    }

    const sourceKeys = Array.isArray(options.sourceKeys) && options.sourceKeys.length
      ? options.sourceKeys
      : ["declRef", "value", "name"];
    let candidate = null;
    for (const key of sourceKeys) {
      candidate = extractStructFieldRefForSynthetic(entry[key]);
      if (candidate) {
        break;
      }
    }
    if (!candidate) {
      return false;
    }

    const baseDecl = pickStructBaseDeclForSynthetic(candidate, options.context, index);
    if (!baseDecl) {
      return false;
    }

    const scopeNameKey = makeDeclScopeNameKeyForSynthetic(baseDecl.scopeLabel, candidate.fullRef);
    if (!scopeNameKey) {
      return false;
    }

    let fieldDecl = index.byScopeName.get(scopeNameKey) || null;
    if (!fieldDecl) {
      fieldDecl = createSyntheticStructFieldDecl(baseDecl, candidate, options.context);
      if (!fieldDecl) {
        return false;
      }
      index.byScopeName.set(scopeNameKey, fieldDecl);

      const nameUpper = normalizeDeclKeyTokenForSynthetic(fieldDecl.name);
      if (nameUpper) {
        if (!index.byNameUpper.has(nameUpper)) {
          index.byNameUpper.set(nameUpper, []);
        }
        index.byNameUpper.get(nameUpper).push(fieldDecl);
      }
      if (Array.isArray(createdDecls)) {
        createdDecls.push(fieldDecl);
      }
    }

    entry[targetProp] = fieldDecl;

    if (targetProp === "decl" && !String(entry.declRef || "").trim()) {
      entry.declRef = candidate.fullRef;
    }
    if (targetProp === "valueDecl" && !String(entry.valueRef || "").trim()) {
      entry.valueRef = candidate.fullRef;
    }
    if (targetProp === "leftOperandDecl" && !String(entry.leftOperandRef || "").trim()) {
      entry.leftOperandRef = candidate.fullRef;
    }
    if (targetProp === "rightOperandDecl" && !String(entry.rightOperandRef || "").trim()) {
      entry.rightOperandRef = candidate.fullRef;
    }

    return true;
  }

  function augmentSyntheticStructFieldDecls(data) {
    if (!data || typeof data !== "object" || !Array.isArray(data.objects)) {
      return 0;
    }

    const index = buildSyntheticDeclIndex(data);
    const createdDecls = [];

    const processAssignSections = (container, sections, context) => {
      for (const sectionName of sections) {
        const list = container && Array.isArray(container[sectionName]) ? container[sectionName] : [];
        for (const entry of list) {
          if (!entry || typeof entry !== "object") {
            continue;
          }
          ensureSyntheticStructFieldDeclForEntry(entry, {
            targetProp: "valueDecl",
            sourceKeys: ["valueRef", "declRef", "value", "name"],
            context
          }, index, createdDecls);
        }
      }
    };

    const processConditionList = (conditions, context) => {
      const list = Array.isArray(conditions) ? conditions : [];
      for (const clause of list) {
        if (!clause || typeof clause !== "object") {
          continue;
        }
        ensureSyntheticStructFieldDeclForEntry(clause, {
          targetProp: "leftOperandDecl",
          sourceKeys: ["leftOperandRef", "leftOperand"],
          context
        }, index, createdDecls);
        ensureSyntheticStructFieldDeclForEntry(clause, {
          targetProp: "rightOperandDecl",
          sourceKeys: ["rightOperandRef", "rightOperand"],
          context
        }, index, createdDecls);
      }
    };

    if (typeof walkObjects === "function") {
      walkObjects(data.objects, (obj) => {
        if (!obj || typeof obj !== "object") {
          return;
        }

        const context = {
          file: String(obj.file || ""),
          lineStart: Number(obj.lineStart || 0) || 0,
          scopeHints: collectScopeHintsFromObjectForSynthetic(obj)
        };

        const values = obj.values && typeof obj.values === "object" ? obj.values : null;
        if (values) {
          for (const entryOrList of Object.values(values)) {
            const list = Array.isArray(entryOrList) ? entryOrList : [entryOrList];
            for (const entry of list) {
              if (!entry || typeof entry !== "object") {
                continue;
              }
              ensureSyntheticStructFieldDeclForEntry(entry, {
                targetProp: "decl",
                sourceKeys: ["declRef", "value", "name"],
                context
              }, index, createdDecls);
            }
          }
        }

        const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
        if (!extras) {
          return;
        }

        if (extras.callFunction) {
          processAssignSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"], context);
        }
        if (extras.callMethod) {
          processAssignSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"], context);
        }
        if (extras.performCall) {
          processAssignSections(extras.performCall, ["using", "changing", "tables"], context);
          processConditionList(extras.performCall.ifConditions, context);
        }

        if (extras.ifCondition) {
          processConditionList(extras.ifCondition.conditions, context);
        }

        if (extras.select) {
          processConditionList(extras.select.whereConditions, context);
          processConditionList(extras.select.havingConditions, context);
        }

        for (const key of ["readTable", "loopAtItab", "modifyItab", "deleteItab"]) {
          if (extras[key]) {
            processConditionList(extras[key].conditions, context);
          }
        }
      });
    }

    if (!Array.isArray(data.decls)) {
      data.decls = [];
    }
    for (const decl of createdDecls) {
      data.decls.push(decl);
    }

    return createdDecls.length;
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

    augmentSyntheticStructFieldDecls(state.data);

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
    if (els.output) {
      els.output.addEventListener("click", interceptOutputCodeButtonClick, true);
    }
    if (els.templatePreviewOutput && typeof handleTemplateVirtualScroll === "function") {
      els.templatePreviewOutput.addEventListener("scroll", handleTemplateVirtualScroll, { passive: true });
    }
    if (els.templatePreviewOutput) {
      els.templatePreviewOutput.addEventListener("click", interceptTemplateCodeButtonClick, true);
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

      if (els.settingsModal && !els.settingsModal.hidden) {
        closeSettingsModal();
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

