"use strict";
(function registerBootstrapService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before bootstrap loads.");
  }
  const state = runtime.state;
  const els = runtime.els;
  const constants = runtime.constants || {};
  const { DESC_STORAGE_KEY_V2, SETTINGS_STORAGE_KEY_V1, TEMPLATE_CONFIG_STORAGE_KEY_V1, THEME_STORAGE_KEY_V1, LAYOUT_SPLIT_STORAGE_KEY_V1, LAYOUT_SPLIT_DEFAULT, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX, MOBILE_LAYOUT_QUERY, RENDER_TREE_OPTIONS, DECL_TYPE_OPTIONS, NAME_CODE_OPTIONS, DEFAULT_SETTINGS, TEMPLATE_DEFAULT_CONFIG_V1, SAMPLE_ABAP } = constants;
  const setError = runtime.requireServiceMethod("runtimeState", "setError");
  const renderBuildInfo = runtime.requireServiceMethod("runtimeState", "renderBuildInfo");
  const loadDescOverrides = runtime.requireServiceMethod("runtimeState", "loadDescOverrides");
  const loadSettings = runtime.requireServiceMethod("runtimeState", "loadSettings");
  const setTemplateConfigError = runtime.requireServiceMethod("runtimeState", "setTemplateConfigError");
  const getDefaultTemplateConfig = runtime.requireServiceMethod("runtimeState", "getDefaultTemplateConfig");
  const mergeMissingDefaultTemplatesInPlace = runtime.requireServiceMethod("runtimeState", "mergeMissingDefaultTemplatesInPlace");
  const loadTemplateConfig = runtime.requireServiceMethod("runtimeState", "loadTemplateConfig");
  const loadTheme = runtime.requireServiceMethod("runtimeState", "loadTheme");
  const applyTheme = runtime.requireServiceMethod("runtimeState", "applyTheme");
  const initLayoutSplitter = runtime.requireServiceMethod("runtimeState", "initLayoutSplitter");
  const openSettingsModal = runtime.requireServiceMethod("runtimeState", "openSettingsModal");
  const closeSettingsModal = runtime.requireServiceMethod("runtimeState", "closeSettingsModal");
  const clearTemplateBlockSelection = runtime.requireServiceMethod("output", "clearTemplateBlockSelection");
  const selectTemplateBlockFromInteraction = runtime.requireServiceMethod("output", "selectTemplateBlockFromInteraction");
  const onInputGutterClick = runtime.requireServiceMethod("output", "onInputGutterClick");
  const openJsonModal = runtime.requireServiceMethod("output", "openJsonModal");
  const closeJsonModal = runtime.requireServiceMethod("output", "closeJsonModal");
  const copyJsonToClipboard = runtime.requireServiceMethod("output", "copyJsonToClipboard");
  const syncInputGutterScroll = runtime.requireServiceMethod("output", "syncInputGutterScroll");
  const rebuildInputGutter = runtime.requireServiceMethod("output", "rebuildInputGutter");
  const renderDeclDescPanelUi = runtime.requireServiceMethod("descriptions", "renderDeclDescPanelUi");
  const getEffectiveDeclDesc = runtime.requireServiceMethod("descriptions", "getEffectiveDeclDesc");
  const getFinalDeclDesc = runtime.requireServiceMethod("descriptions", "getFinalDeclDesc");
  const closeEditModal = runtime.requireServiceMethod("descriptions", "closeEditModal");
  const applyEditModal = runtime.requireServiceMethod("descriptions", "applyEditModal");
  const resolveValueLevelFinalDesc = runtime.requireServiceMethod("descriptions", "resolveValueLevelFinalDesc");
  const buildPerformCallPathRegistry = runtime.requireServiceMethod("performSources", "buildPerformCallPathRegistry");
  const selectPerformSourceCandidate = runtime.requireServiceMethod("performSources", "selectPerformSourceCandidate");
  const syncTemplateEditorFromState = runtime.requireServiceMethod("template", "syncTemplateEditorFromState");
  const applyTemplateConfigFromEditor = runtime.requireServiceMethod("template", "applyTemplateConfigFromEditor");
  const resetTemplateConfig = runtime.requireServiceMethod("template", "resetTemplateConfig");
  const buildTemplateCollectionCopyPayload = runtime.requireServiceMethod("template", "buildTemplateCollectionCopyPayload");
  const ensureTemplateWindowContainsIndex = runtime.requireServiceMethod("template", "ensureTemplateWindowContainsIndex");
  const handleTemplateVirtualScroll = runtime.requireServiceMethod("template", "handleTemplateVirtualScroll");
  const handleTemplateVirtualUserIntent = runtime.requireServiceMethod("template", "handleTemplateVirtualUserIntent");
  const renderTemplatePreview = runtime.requireServiceMethod("template", "renderTemplatePreview");
  const isTemplateDynamicModalOpen = runtime.requireServiceMethod("template", "isTemplateDynamicModalOpen");
  const closeTemplateDynamicModal = runtime.requireServiceMethod("template", "closeTemplateDynamicModal");
  const initTemplateGuiFilterControls = runtime.requireServiceMethod("template", "initTemplateGuiFilterControls");
  const openTemplateFilterModal = runtime.requireServiceMethod("template", "openTemplateFilterModal");
  const buildViewerConfigBundle = runtime.requireServiceMethod("template", "buildViewerConfigBundle");
  const getViewerConfigExportFileName = runtime.requireServiceMethod("template", "getViewerConfigExportFileName");
  const openViewerConfigExportModal = runtime.requireServiceMethod("template", "openViewerConfigExportModal");
  const importViewerConfigObject = runtime.requireServiceMethod("template", "importViewerConfigObject");
  const importViewerConfigFromFile = runtime.requireServiceMethod("template", "importViewerConfigFromFile");
  const copySelectedTemplateBlocks = runtime.requireServiceMethod("template", "copySelectedTemplateBlocks");
  const interceptTemplateCodeButtonClick = runtime.requireServiceMethod("template", "interceptTemplateCodeButtonClick");
  const normalizeTemplateConfigLegacyFieldsInPlace = runtime.requireServiceMethod("template", "normalizeTemplateConfigLegacyFieldsInPlace");
  const openTemplateConfigModal = runtime.requireServiceMethod("template", "openTemplateConfigModal");
  const setRightTab = runtime.requireServiceMethod("uiNavigation", "setRightTab");
  const applySettingsFromModal = runtime.requireServiceMethod("uiNavigation", "applySettingsFromModal");
  const resetSettingsToDefault = runtime.requireServiceMethod("uiNavigation", "resetSettingsToDefault");
  const jumpInputToCodeRange = runtime.requireServiceMethod("uiNavigation", "jumpInputToCodeRange");
  const getSegmentRangesForLineText = runtime.requireServiceMethod("uiNavigation", "getSegmentRangesForLineText");
  const findDeclSegmentIndex = runtime.requireServiceMethod("uiNavigation", "findDeclSegmentIndex");
  const goToInputLine = runtime.requireServiceMethod("uiNavigation", "goToInputLine");
  const initInputGotoLineControls = runtime.requireServiceMethod("uiNavigation", "initInputGotoLineControls");
  const parseFromTextarea = runtime.requireServiceMethod("parserController", "parseFromTextarea");
let virtualGeometryRefreshFrameMain = 0;

function start() {
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init, { once: true });
  } else {
    init();
  }
}


  function isVirtualScrollKeyMain(ev) {
    if (!ev) {
      return false;
    }
    return [
      "ArrowUp",
      "ArrowDown",
      "PageUp",
      "PageDown",
      "Home",
      "End",
      " ",
      "Space",
      "Spacebar"
    ].includes(String(ev.key || ""));
  }



  function isEditableVirtualScrollTargetMain(target) {
    if (!(target instanceof Element)) {
      return false;
    }
    if (target.closest("input, textarea, select")) {
      return true;
    }
    if (target.isContentEditable) {
      return true;
    }
    const editableAncestor = target.closest("[contenteditable]");
    return Boolean(
      editableAncestor
      && String(editableAncestor.getAttribute("contenteditable") || "").toLowerCase() !== "false"
    );
  }



  function addVirtualUserIntentListenersMain(container, handler) {
    if (!container || typeof handler !== "function") {
      return;
    }
    const passiveCapture = { passive: true, capture: true };
    for (const eventName of ["wheel", "pointerdown", "touchstart"]) {
      container.addEventListener(eventName, handler, passiveCapture);
    }
    container.addEventListener("keydown", (ev) => {
      if (!isVirtualScrollKeyMain(ev) || isEditableVirtualScrollTargetMain(ev.target)) {
        return;
      }
      handler(ev);
    }, { capture: true });
  }



  function scheduleVirtualGeometryRefreshMain() {
    if (virtualGeometryRefreshFrameMain || typeof requestAnimationFrame !== "function") {
      return;
    }
    virtualGeometryRefreshFrameMain = requestAnimationFrame(() => {
      virtualGeometryRefreshFrameMain = 0;
      if (!state.data || !Array.isArray(state.renderObjects) || !state.renderObjects.length) {
        return;
      }
      if (
        state.rightTab === "template"
        && els.templatePreviewPanel
        && !els.templatePreviewPanel.hidden
        && typeof renderTemplatePreview === "function"
      ) {
        renderTemplatePreview();
      }
    });
  }



  function init() {
    renderBuildInfo();
    state.descOverrides = loadDescOverrides();
    state.settings = loadSettings();
    state.templateConfig = loadTemplateConfig();
    if (!state.templateConfig || typeof state.templateConfig !== "object" || Array.isArray(state.templateConfig)) {
      state.templateConfig = getDefaultTemplateConfig();
    }
    const templateLegacyFieldsChanged = normalizeTemplateConfigLegacyFieldsInPlace(state.templateConfig);
    const templateDefaultsAdded = mergeMissingDefaultTemplatesInPlace(state.templateConfig);
    const templateConfigChanged = templateLegacyFieldsChanged || templateDefaultsAdded;
    if (templateConfigChanged) {
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
    if (typeof window !== "undefined") {
      if (typeof syncInputGutterScroll === "function") {
        window.addEventListener("resize", syncInputGutterScroll, { passive: true });
      }
      window.addEventListener("resize", scheduleVirtualGeometryRefreshMain, { passive: true });
      window.addEventListener("abap-viewer-layout-resize", scheduleVirtualGeometryRefreshMain);
      if (window.visualViewport && typeof window.visualViewport.addEventListener === "function") {
        if (typeof syncInputGutterScroll === "function") {
          window.visualViewport.addEventListener("resize", syncInputGutterScroll, { passive: true });
        }
        window.visualViewport.addEventListener("resize", scheduleVirtualGeometryRefreshMain, { passive: true });
      }
    }
    if (els.templatePreviewOutput && typeof handleTemplateVirtualScroll === "function") {
      els.templatePreviewOutput.addEventListener("scroll", handleTemplateVirtualScroll, { passive: true });
    }
    if (els.templatePreviewOutput && typeof handleTemplateVirtualUserIntent === "function") {
      addVirtualUserIntentListenersMain(els.templatePreviewOutput, handleTemplateVirtualUserIntent);
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

    els.descBtn.addEventListener("click", () => {
      setRightTab("descriptions");
    });

    if (els.rightTabTemplateBtn) {
      els.rightTabTemplateBtn.addEventListener("click", () => setRightTab("template"));
    }

    if (els.rightTabDescBtn) {
      els.rightTabDescBtn.addEventListener("click", () => setRightTab("descriptions"));
    }

    if (els.templateCopySelectedBtn) {
      els.templateCopySelectedBtn.addEventListener("click", (ev) => {
        if (ev && typeof ev.stopPropagation === "function") {
          ev.stopPropagation();
        }
        copySelectedTemplateBlocks()
          .then(() => setError(""))
          .catch((err) => setError(`Copy failed: ${err && err.message ? err.message : err}`));
      });
    }

    if (els.templateResetBtn) {
      els.templateResetBtn.addEventListener("click", resetTemplateConfig);
    }

    if (els.templateExportBtn) {
      els.templateExportBtn.addEventListener("click", openViewerConfigExportModal);
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
        await importViewerConfigFromFile(file);
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
          registry: constants.VARIABLE_DESCRIPTIONS || {}
        });
      });
    }

    if (els.declDescSearch) {
      els.declDescSearch.addEventListener("input", renderDeclDescPanelUi);
    }

    if (els.declDescMissingOnly) {
      els.declDescMissingOnly.addEventListener("change", renderDeclDescPanelUi);
    }

    if (els.settingsBtn) {
      els.settingsBtn.addEventListener("click", openSettingsModal);
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
  const output = runtime.getService("output");
  const descriptions = runtime.getService("descriptions");
  const performSources = runtime.getService("performSources");
  const template = runtime.getService("template");
  const uiNavigation = runtime.getService("uiNavigation");
  const parserController = runtime.getService("parserController");
  runtime.api = {
    applyTemplateConfigFromEditor: template.applyTemplateConfigFromEditor,
    buildPerformCallPathRegistry: performSources.buildPerformCallPathRegistry,
    buildTemplateCollectionCopyPayload: template.buildTemplateCollectionCopyPayload,
    buildViewerConfigBundle: template.buildViewerConfigBundle,
    clearTemplateBlockSelection: output.clearTemplateBlockSelection,
    ensureTemplateWindowContainsIndex: template.ensureTemplateWindowContainsIndex,
    findDeclSegmentIndex: uiNavigation.findDeclSegmentIndex,
    getEffectiveDeclDesc: descriptions.getEffectiveDeclDesc,
    getFinalDeclDesc: descriptions.getFinalDeclDesc,
    getSegmentRangesForLineText: uiNavigation.getSegmentRangesForLineText,
    getSelectedTemplateIndexes: output.getSortedSelectedTemplateIndexes,
    getViewerConfigExportFileName: template.getViewerConfigExportFileName,
    goToInputLine: uiNavigation.goToInputLine,
    importViewerConfigObject: template.importViewerConfigObject,
    init: init,
    jumpInputToCodeRange: uiNavigation.jumpInputToCodeRange,
    parseFromTextarea: parserController.parseFromTextarea,
    renderDeclDescPanelUi: descriptions.renderDeclDescPanelUi,
    renderTemplatePreview: template.renderTemplatePreview,
    resolveValueLevelFinalDesc: descriptions.resolveValueLevelFinalDesc,
    selectPerformSourceCandidate: performSources.selectPerformSourceCandidate,
    selectTemplateBlockFromInteraction: output.selectTemplateBlockFromInteraction,
    setRightTab: uiNavigation.setRightTab
  };

  runtime.registerService("bootstrap", {
    start,
  });
})(window);
