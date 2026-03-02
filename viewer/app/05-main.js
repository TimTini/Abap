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
    const modal = openTemplateDynamicModal("Template JSON", { contentClass: "template-runtime-modal-content template-runtime-modal-wide" });

    const previousConfigJsonEl = els.templateConfigJson;
    const previousConfigErrorEl = els.templateConfigError;

    const applyBtn = document.createElement("button");
    applyBtn.type = "button";
    applyBtn.className = "secondary";
    applyBtn.textContent = "Apply";
    modal.actions.prepend(applyBtn);

    const errorEl = document.createElement("div");
    errorEl.className = "template-error";
    modal.body.appendChild(errorEl);

    const textarea = document.createElement("textarea");
    textarea.className = "template-config-json";
    textarea.spellcheck = false;
    textarea.placeholder = "Template config JSON...";
    modal.body.appendChild(textarea);

    els.templateConfigJson = textarea;
    els.templateConfigError = errorEl;
    writeTemplateConfigDraftToTextarea(textarea);
    if (typeof setTemplateConfigError === "function") {
      setTemplateConfigError("");
    }

    const applyFromModal = () => {
      try {
        applyTemplateConfigFromEditor();
      } catch (err) {
        setError(`Apply failed: ${err && err.message ? err.message : err}`);
        return;
      }
      if (typeof setError === "function") {
        setError("");
      }
      const hasInlineError = String(errorEl.textContent || "").trim();
      if (!hasInlineError) {
        closeTemplateDynamicModal();
      }
    };

    applyBtn.addEventListener("click", applyFromModal);
    textarea.addEventListener("keydown", (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        applyFromModal();
      }
    });

    setTimeout(() => {
      textarea.focus();
      textarea.setSelectionRange(0, 0);
    }, 0);

    modal.setCleanup(() => {
      els.templateConfigJson = previousConfigJsonEl || null;
      els.templateConfigError = previousConfigErrorEl || null;
    });
  }

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

