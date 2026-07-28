"use strict";
(function registerUiNavigationService(global) {
  const runtime = global.AbapViewerRuntime;
  if (!runtime || typeof runtime.registerService !== "function") {
    throw new Error("ABAP Viewer service registry missing before uiNavigation loads.");
  }
  const state = runtime.state;
  const els = runtime.els;
  const constants = runtime.constants || {};
  const { DESC_STORAGE_KEY_V2, SETTINGS_STORAGE_KEY_V1, TEMPLATE_CONFIG_STORAGE_KEY_V1, THEME_STORAGE_KEY_V1, LAYOUT_SPLIT_STORAGE_KEY_V1, LAYOUT_SPLIT_DEFAULT, LAYOUT_SPLIT_MIN, LAYOUT_SPLIT_MAX, MOBILE_LAYOUT_QUERY, RENDER_TREE_OPTIONS, DECL_TYPE_OPTIONS, NAME_CODE_OPTIONS, DEFAULT_SETTINGS, TEMPLATE_DEFAULT_CONFIG_V1, SAMPLE_ABAP } = constants;
  const setError = runtime.requireServiceMethod("runtimeState", "setError");
  const normalizeSettings = runtime.requireServiceMethod("runtimeState", "normalizeSettings");
  const saveSettings = runtime.requireServiceMethod("runtimeState", "saveSettings");
  const renderSettingsModalUi = runtime.requireServiceMethod("runtimeState", "renderSettingsModalUi");
  const computeLineOffsets = runtime.requireServiceMethod("output", "computeLineOffsets");
  const countInputLines = runtime.requireServiceMethod("output", "countInputLines");
  const refreshInputGutterTargets = runtime.requireServiceMethod("output", "refreshInputGutterTargets");
  const measureInputLineMetrics = runtime.requireServiceMethod("output", "measureInputLineMetrics");
  const syncInputGutterScroll = runtime.requireServiceMethod("output", "syncInputGutterScroll");
  const renderDeclDescPanelUi = runtime.requireServiceMethod("descriptions", "renderDeclDescPanelUi");
  const renderTemplatePreview = runtime.requireServiceMethod("template", "renderTemplatePreview");
  const start = runtime.requireServiceMethod("bootstrap", "start");


  function renderActiveRightPanel() {
    if (state.rightTab === "descriptions") {
      renderDeclDescPanelUi();
      return;
    }
    if (typeof renderTemplatePreview === "function") {
      renderTemplatePreview();
    }
  }



  function setRightTab(nextTab) {
    const tab = nextTab === "descriptions" ? "descriptions" : "template";
    state.rightTab = tab;

    const showDescriptions = tab === "descriptions";
    const showTemplate = tab === "template";
    if (els.templatePreviewPanel) {
      els.templatePreviewPanel.hidden = !showTemplate;
    }
    if (els.declDescPanel) {
      els.declDescPanel.hidden = !showDescriptions;
    }

    if (els.rightPanelTitle) {
      els.rightPanelTitle.textContent = showDescriptions
        ? "Data"
        : "Template Preview";
    }
    if (els.rightTabTemplateBtn) {
      els.rightTabTemplateBtn.classList.toggle("active", showTemplate);
      els.rightTabTemplateBtn.setAttribute("aria-selected", String(showTemplate));
    }
    if (els.rightTabDescBtn) {
      els.rightTabDescBtn.classList.toggle("active", showDescriptions);
      els.rightTabDescBtn.setAttribute("aria-selected", String(showDescriptions));
    }
    if (els.declDescJsonBtn) {
      els.declDescJsonBtn.hidden = !showDescriptions;
    }

    if (showDescriptions) {
      renderDeclDescPanelUi();
      setTimeout(() => {
        if (els.declDescSearch) {
          els.declDescSearch.focus();
        }
      }, 0);
    } else if (showTemplate) {
      renderTemplatePreview();
    }

    refreshInputGutterTargets();
  }



  function applySettingsFromModal() {
    if (!els.settingsModal) {
      return;
    }

    const next = {
      normalizeDeclDesc: Boolean(els.settingsNormalizeDesc && els.settingsNormalizeDesc.checked),
      declFilterTypes: [],
      structDescTemplate: (els.settingsStructTemplate && els.settingsStructTemplate.value)
        ? String(els.settingsStructTemplate.value || "")
        : DEFAULT_SETTINGS.structDescTemplate,
      nameTemplatesByCode: {}
    };

    if (els.settingsDeclTypes) {
      const inputs = els.settingsDeclTypes.querySelectorAll("input[type=checkbox]");
      for (const input of Array.from(inputs)) {
        if (input.checked) {
          next.declFilterTypes.push(String(input.value || "").trim().toUpperCase());
        }
      }
    }


    const nameInputs = els.settingsNameTemplates
      ? els.settingsNameTemplates.querySelectorAll("input[data-code]")
      : [];

    for (const input of Array.from(nameInputs)) {
      const code = String(input.getAttribute("data-code") || "").trim().toUpperCase();
      if (!code) {
        continue;
      }
      next.nameTemplatesByCode[code] = String(input.value || "");
    }

    state.settings = normalizeSettings(next);
    saveSettings(state.settings);
    state.templatePreviewCache = null;
    renderActiveRightPanel();
  }



  function resetSettingsToDefault() {
    state.settings = normalizeSettings(DEFAULT_SETTINGS);
    saveSettings(state.settings);
    renderSettingsModalUi();
    state.templatePreviewCache = null;
    renderActiveRightPanel();
  }



  function focusInputWithoutPageScroll() {
    if (!els.inputText) {
      return;
    }
    try {
      els.inputText.focus({ preventScroll: true });
    } catch {
      els.inputText.focus();
    }
  }



  function navigateInputRange(options) {
    if (!els.inputText) {
      return { line: 1, total: 1 };
    }
    const opts = options && typeof options === "object" ? options : {};
    const totalLines = getCurrentInputLineCount();
    const start = Math.max(1, Math.min(totalLines, Math.floor(Number(opts.lineStart) || 1)));
    const end = Math.max(start, Math.min(totalLines, Math.floor(Number(opts.lineEnd) || start)));
    const hasSegmentIndex = opts.segmentIndex !== null
      && opts.segmentIndex !== undefined
      && String(opts.segmentIndex).trim() !== "";
    const segmentIndex = hasSegmentIndex && Number.isFinite(Number(opts.segmentIndex))
      ? Math.max(0, Math.floor(Number(opts.segmentIndex)))
      : null;
    const anchorRatio = Math.max(0, Math.min(0.9, Number(opts.anchorRatio) || 0));
    const text = String(els.inputText.value || "");
    const offsets = typeof computeLineOffsets === "function" ? computeLineOffsets(text) : [0];
    state.inputLineOffsets = offsets;

    let selectionStart = Number(offsets[start - 1]) || 0;
    let selectionEnd = Number(offsets[end]) || text.length;
    if (segmentIndex !== null) {
      const lineEndOffset = Number(offsets[start]) || text.length;
      const lineText = text.slice(selectionStart, lineEndOffset);
      const segmentRange = getSegmentRangeForLine(lineText, segmentIndex);
      if (segmentRange) {
        selectionStart += segmentRange.start;
        selectionEnd = (Number(offsets[start - 1]) || 0) + segmentRange.end;
      } else {
        selectionEnd = Math.max(selectionStart, lineEndOffset > selectionStart ? lineEndOffset - 1 : selectionStart);
      }
    }

    focusInputWithoutPageScroll();
    els.inputText.setSelectionRange(selectionStart, Math.max(selectionStart, selectionEnd));

    const applyScroll = () => {
      const metrics = typeof measureInputLineMetrics === "function"
        ? measureInputLineMetrics()
        : { effectivePitch: 18 };
      const pitch = Math.max(12, Number(metrics && metrics.effectivePitch) || 18);
      const viewportHeight = Math.max(0, Number(els.inputText.clientHeight) || 0);
      const maxTop = Math.max(0, (Number(els.inputText.scrollHeight) || 0) - viewportHeight);
      const targetTop = ((start - 1) * pitch) - (viewportHeight * anchorRatio);
      els.inputText.scrollTop = Math.max(0, Math.min(maxTop, targetTop));
      if (typeof syncInputGutterScroll === "function") {
        syncInputGutterScroll();
      }
    };

    applyScroll();
    requestAnimationFrame(applyScroll);
    return { line: start, total: totalLines };
  }



  function jumpInputToCodeRange(lineStart, lineEnd, segmentIndex) {
    return navigateInputRange({
      lineStart,
      lineEnd,
      segmentIndex,
      anchorRatio: 0.28
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

    return navigateInputRange({
      lineStart: targetLine,
      lineEnd: targetLine,
      segmentIndex: targetSegmentIndex,
      anchorRatio: 0.35
    });
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
  runtime.registerService("uiNavigation", {
    renderActiveRightPanel,
    setRightTab,
    applySettingsFromModal,
    resetSettingsToDefault,
    navigateInputRange,
    jumpInputToCodeRange,
    getSegmentRangesForLineText,
    findDeclSegmentIndex,
    goToInputLine,
    initInputGotoLineControls
  });
})(window);
