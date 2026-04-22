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

  function jumpInputToCodeRange(lineStart, lineEnd, segmentIndex) {
    const start = Math.max(1, Number(lineStart) || 1);
    const end = Math.max(start, Number(lineEnd) || start);
    const targetSegmentIndex = Number.isFinite(Number(segmentIndex))
      ? Math.max(0, Math.floor(Number(segmentIndex)))
      : null;

    if (targetSegmentIndex !== null && typeof goToInputLine === "function") {
      goToInputLine({ line: start, segmentIndex: targetSegmentIndex });
      return;
    }

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
    const modal = openTemplateDynamicModal("Template Form", { contentClass: "template-runtime-modal-content template-runtime-modal-wide" });
    modal.closeBtn.textContent = "Cancel";
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
    workspace.appendChild(editorPane);

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
    editorPane.appendChild(tabs);
    const host = document.createElement("div");
    host.className = "template-config-editor-host";
    editorPane.appendChild(host);
    const errEl = document.createElement("div");
    errEl.className = "template-error";
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
    workspace.appendChild(previewPane);

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
      if (activeTab === "json" && jsonArea) {
        const raw = String(jsonArea.value || "").trim();
        if (!raw) {
          return { config: null, message: "JSON draft is empty." };
        }
        let parsed = null;
        try {
          parsed = JSON.parse(raw);
        } catch (err) {
          return { config: null, message: `Preview paused: JSON parse error (${err && err.message ? err.message : err}).` };
        }
        const normalized = cloneJsonValue(parsed);
        if (!normalized || typeof normalized !== "object" || Array.isArray(normalized)) {
          return { config: null, message: "Preview paused: JSON root must be an object." };
        }
        normalizeTemplateConfigLegacyFieldsInPlace(normalized);
        normalized.version = 1;
        if (!normalized.templates || typeof normalized.templates !== "object" || Array.isArray(normalized.templates)) {
          normalized.templates = {};
        }
        if (!Object.keys(normalized.templates).length) {
          normalized.templates.DEFAULT = {};
        }
        const chk = validateTemplateConfig(normalized);
        if (!chk.valid) {
          return { config: null, message: `Preview paused: ${chk.errors[0] || "Template config is invalid."}` };
        }
        return { config: normalized, message: "" };
      }
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
          activeTab === "json"
            ? "JSON mode updates preview only when the draft is valid."
            : "Fix validation issues to resume preview.",
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
      previewNote.textContent = activeTab === "json"
        ? "JSON mode: preview refreshes whenever the draft becomes valid."
        : "Form mode: preview refreshes while you type.";
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
      const out = { hideEmptyRows: true, hideRowsWithoutValues: true, expandMultilineRows: false, squareCells: true, squareCellSize: 18 };
      if (!info) return out;
      const setB = (x, v) => { if (!(v === undefined || v === null || v === "")) out[x] = Boolean(v); };
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
          squareCellSize: Math.min(240, Math.max(16, Math.round(Number(opts.squareCellSize) || 18)))
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
    const syncJsonToDraft = () => {
      if (!jsonArea) return true;
      const raw = String(jsonArea.value || "").trim();
      if (!raw) { showErr("Template config JSON is empty."); return false; }
      let parsed = null;
      try { parsed = JSON.parse(raw); } catch (er) { showErr(`JSON parse error: ${er && er.message ? er.message : er}`); return false; }
      const normalized = cloneJsonValue(parsed);
      if (!normalized || typeof normalized !== "object" || Array.isArray(normalized)) { showErr("Cannot load JSON draft."); return false; }
      normalizeTemplateConfigLegacyFieldsInPlace(normalized);
      const chk = validateTemplateConfig(normalized);
      if (!chk.valid) { showErr(chk.errors.join("\n")); return false; }
      draft = normalized;
      if (!draft || typeof draft !== "object" || Array.isArray(draft)) { showErr("Cannot load JSON draft."); return false; }
      if (!draft.templates || typeof draft.templates !== "object" || Array.isArray(draft.templates)) draft.templates = {};
      if (!Object.keys(draft.templates).length) draft.templates.DEFAULT = {};
      if (!Object.prototype.hasOwnProperty.call(draft.templates, selKey)) selKey = Object.keys(draft.templates)[0] || "DEFAULT";
      selRange = "";
      try { jsonArea.value = JSON.stringify(draft, null, 2); } catch { /* ignore */ }
      showErr("");
      return true;
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
    const getEntryForGridCell = (row, col) => {
      const matches = getTemplateGridEntries()
        .filter((entry) => rangeContainsCellForBuilder(entry.parsed, row, col))
        .sort((left, right) => left.area - right.area);
      return matches[0] || null;
    };
    const applyStyleToGridCell = (td, cell) => {
      const cfg = cell && typeof cell === "object" ? cell : {};
      if (cfg.background) td.style.backgroundColor = String(cfg.background);
      if (cfg["font color"]) td.style.color = String(cfg["font color"]);
      if (cfg["font size"]) td.style.fontSize = `${Number(cfg["font size"]) || 10}pt`;
      if (cfg.font) td.style.fontFamily = String(cfg.font);
      if (cfg.align) td.style.textAlign = String(cfg.align);
      if (cfg.valign) td.style.verticalAlign = String(cfg.valign);
      if (cfg.bold) td.style.fontWeight = "700";
      if (cfg.italic) td.style.fontStyle = "italic";
      if (cfg.underline) td.style.textDecoration = "underline";
      if (cfg.border) td.classList.add("has-border-token");
    };
    const insertPlaceholderIntoText = (text, placeholder) => {
      const raw = String(placeholder || "").trim();
      if (!raw) return String(text || "");
      const token = raw.startsWith("{") && raw.endsWith("}") ? raw : `{${raw}}`;
      const current = String(text || "");
      return current ? `${current}${token}` : token;
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
      } else if (type === "format") {
        if (!cell.background) setSelectedCellField("background", "#dbeef4");
        if (!cell.border) setSelectedCellField("border", "outside-thin");
        if (!cell["font color"]) setSelectedCellField("font color", "#111111");
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
      if (!keys.includes(selKey)) selKey = keys[0] || "DEFAULT";
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
        showErr("");
        renderActive();
      });
      keyField.appendChild(keyLabel);
      keyField.appendChild(keySelect);
      topbar.appendChild(keyField);

      const keyActions = document.createElement("div");
      keyActions.className = "template-builder-key-actions";
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
        selKey = Object.keys(draft.templates)[0] || "DEFAULT";
        selRange = listRanges(selKey)[0]?.rangeKey || "A1";
        showErr("");
        renderActive();
      }, "danger-lite"));
      topbar.appendChild(keyActions);

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
        { type: "format", title: "Format", desc: "Áp dụng background/border/font." }
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
          const matched = getEntryForGridCell(row, col);
          const td = document.createElement("td");
          td.tabIndex = 0;
          td.setAttribute("data-row", String(row));
          td.setAttribute("data-col", String(col));
          let displayRange = { r1: row, c1: col, r2: row, c2: col };
          let cellConfig = null;
          if (matched) {
            displayRange = matched.parsed;
            cellConfig = matched.cell;
            td.setAttribute("data-range-key", normalizeRangeKeyForBuilder(matched.rangeKey));
            if (cellConfig && cellConfig.merge === true) {
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
          applyStyleToGridCell(td, cellConfig);
          const text = cellConfig && Object.prototype.hasOwnProperty.call(cellConfig, "text") ? String(cellConfig.text || "") : "";
          renderBuilderCellContent(td, text);
          td.addEventListener("pointerdown", (ev) => {
            if (ev.button !== 0) return;
            builderDragging = true;
            builderDragAnchor = { row, col };
            selRange = makeRangeKeyFromBounds(row, col, row, col);
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
            selRange = makeRangeKeyFromBounds(row, col, row, col);
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
      ta.addEventListener("input", () => {
        scheduleLivePreview(180);
      });
      ta.addEventListener("keydown", (ev) => { if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") { ev.preventDefault(); applyFromModal(); } });
      root.appendChild(ta);
      host.replaceChildren(root);
      jsonArea = ta;
      els.templateConfigJson = ta;
      setTimeout(() => { ta.focus(); ta.setSelectionRange(0, 0); }, 0);
    }

    function renderActive() {
      hideTemplateTokenSuggest();
      clearTemplateTokenSuggestionCache();
      formBtn.classList.toggle("active", activeTab === "form");
      jsonBtn.classList.toggle("active", activeTab === "json");
      if (activeTab === "json") renderJson();
      else { jsonArea = null; renderForm(); }
      renderLivePreview();
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
      clearPreviewTimer();
      hideTemplateTokenSuggest();
      clearTemplateTokenSuggestionCache();
      document.removeEventListener("pointerup", stopBuilderDrag);
      document.removeEventListener("pointercancel", stopBuilderDrag);
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
            operations.push({
              label: "description",
              run: () => onSaveDesc({
                decl: selected ? selected.decl : null,
                declKey: selected ? selected.declKey : "",
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

  function getInputLineText(lineNumber) {
    if (!els.inputText) {
      return "";
    }

    const lines = String(els.inputText.value || "").split(/\r\n|\r|\n/);
    const lineIndex = Math.max(0, Math.min(lines.length - 1, Math.floor(Math.max(1, Number(lineNumber) || 1)) - 1));
    return String(lines[lineIndex] || "");
  }

  function getSegmentRangesForLineText(lineText) {
    const source = String(lineText || "");
    const segments = [];
    let inSingleQuote = false;
    let inPipe = false;
    let segmentStart = 0;

    for (let index = 0; index < source.length; index += 1) {
      const char = source[index];
      const nextChar = index + 1 < source.length ? source[index + 1] : "";
      const prevChar = index > 0 ? source[index - 1] : "";

      if (char === "'" && !inPipe) {
        if (inSingleQuote && nextChar === "'") {
          index += 1;
          continue;
        }
        inSingleQuote = !inSingleQuote;
        continue;
      }

      if (char === "|" && !inSingleQuote) {
        if (inPipe && nextChar === "|") {
          index += 1;
          continue;
        }
        inPipe = !inPipe;
        continue;
      }

      if (char !== "." || inSingleQuote || inPipe) {
        continue;
      }

      if (/\d/.test(prevChar) && /\d/.test(nextChar)) {
        continue;
      }

      const piece = source.slice(segmentStart, index + 1).trim();
      if (piece) {
        segments.push({ start: segmentStart, end: index + 1, text: piece });
      }
      segmentStart = index + 1;
    }

    const trailing = source.slice(segmentStart).trim();
    if (trailing) {
      segments.push({ start: segmentStart, end: source.length, text: trailing });
    }

    return segments;
  }

  function getSegmentRangeForLine(lineText, segmentIndex) {
    const targetIndex = Math.max(0, Number(segmentIndex) || 0);
    return getSegmentRangesForLineText(lineText)[targetIndex] || null;
  }

  function findDeclSegmentIndex(decl) {
    if (!decl || typeof decl !== "object") {
      return null;
    }

    const declaredSegmentIndex = Number.isFinite(Number(decl.segmentIndex))
      ? Math.max(0, Math.floor(Number(decl.segmentIndex)))
      : null;
    if (declaredSegmentIndex !== null) {
      return declaredSegmentIndex;
    }

    const lineStart = Number(decl.lineStart || 0) || 0;
    if (lineStart <= 0) {
      return null;
    }

    const lineText = getInputLineText(lineStart);
    if (!lineText) {
      return null;
    }

    const segments = getSegmentRangesForLineText(lineText);
    if (!segments.length) {
      return null;
    }

    const rawText = String(decl.raw || "").trim().toLowerCase();
    if (rawText) {
      const exactIndex = segments.findIndex((segment) => String(segment.text || "").trim().toLowerCase() === rawText);
      if (exactIndex >= 0) {
        return exactIndex;
      }
    }

    const nameText = String(decl.name || "").trim().toLowerCase();
    if (nameText) {
      const nameIndex = segments.findIndex((segment) => String(segment.text || "").toLowerCase().includes(nameText));
      if (nameIndex >= 0) {
        return nameIndex;
      }
    }

    return null;
  }

  function goToInputLine(lineNumber) {
    if (!els.inputText) {
      return { line: 1, total: 1 };
    }

    const inputLine = lineNumber && typeof lineNumber === "object"
      ? lineNumber
      : { line: lineNumber, segmentIndex: null };
    const totalLines = getCurrentInputLineCount();
    const next = Number.isFinite(Number(inputLine.line)) ? Number(inputLine.line) : 1;
    const targetLine = Math.max(1, Math.min(totalLines, Math.floor(next)));
    const targetSegmentIndex = Number.isFinite(Number(inputLine.segmentIndex))
      ? Math.max(0, Math.floor(Number(inputLine.segmentIndex)))
      : null;

    if (typeof selectCodeLines === "function" && targetSegmentIndex === null) {
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
    if (targetSegmentIndex !== null) {
      const lineStartOffset = Number(offsets[Math.max(0, targetLine - 1)]) || 0;
      const lineEndOffset = Number(offsets[Math.min(offsets.length - 1, targetLine)]) || text.length;
      const lineText = text.slice(lineStartOffset, lineEndOffset);
      const segmentRange = getSegmentRangeForLine(lineText, targetSegmentIndex);
      if (segmentRange) {
        els.inputText.selectionStart = lineStartOffset + segmentRange.start;
        els.inputText.selectionEnd = lineStartOffset + segmentRange.end;
      } else {
        els.inputText.selectionStart = startOffset;
        els.inputText.selectionEnd = endOffset;
      }
    } else {
      els.inputText.selectionStart = startOffset;
      els.inputText.selectionEnd = endOffset;
    }

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
      file: String(usageFile || baseDecl.file || ""),
      lineStart: Number(usageLine || baseDecl.lineStart) || null,
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
      traceFile: usageFile || "",
      traceLineStart: usageLine || null,
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
    if (!state.templateConfig || typeof state.templateConfig !== "object" || Array.isArray(state.templateConfig)) {
      state.templateConfig = getDefaultTemplateConfig();
    }
    if (normalizeTemplateConfigLegacyFieldsInPlace(state.templateConfig)) {
      try {
        localStorage.setItem(
          TEMPLATE_CONFIG_STORAGE_KEY_V1,
          JSON.stringify(state.templateConfig)
        );
      } catch {
        // ignore
      }
    }
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
  targetRuntime.api.goToInputLine = goToInputLine;
  targetRuntime.api.jumpInputToCodeRange = jumpInputToCodeRange;
  targetRuntime.api.findDeclSegmentIndex = findDeclSegmentIndex;
  targetRuntime.api.getSegmentRangesForLineText = getSegmentRangesForLineText;

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

