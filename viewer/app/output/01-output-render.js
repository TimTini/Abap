"use strict";

window.AbapViewerModules = window.AbapViewerModules || {};
window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};

  function isValueLikeEntryObject(value) {
    if (!isPlainObjectRecord(value)) {
      return false;
    }
    if (isDeclLikeObject(value) || isAbapStatementObject(value)) {
      return false;
    }
    return (
      hasValueLevelDescFields(value)
      || Object.prototype.hasOwnProperty.call(value, "declRef")
      || Object.prototype.hasOwnProperty.call(value, "value")
      || Object.prototype.hasOwnProperty.call(value, "name")
      || Object.prototype.hasOwnProperty.call(value, "label")
    );
  }

  function isAssignmentLikeEntryObject(value) {
    if (!isPlainObjectRecord(value)) {
      return false;
    }
    if (isDeclLikeObject(value) || isAbapStatementObject(value)) {
      return false;
    }
    const hasValueSignals = (
      Object.prototype.hasOwnProperty.call(value, "value")
      || Object.prototype.hasOwnProperty.call(value, "valueRef")
      || Object.prototype.hasOwnProperty.call(value, "valueDecl")
      || Object.prototype.hasOwnProperty.call(value, "originDecls")
    );
    if (!hasValueSignals) {
      return false;
    }
    return (
      Object.prototype.hasOwnProperty.call(value, "name")
      || Object.prototype.hasOwnProperty.call(value, "value")
      || Object.prototype.hasOwnProperty.call(value, "valueRef")
    );
  }

  function isConditionClauseLikeObject(value) {
    if (!isPlainObjectRecord(value)) {
      return false;
    }
    return (
      Object.prototype.hasOwnProperty.call(value, "leftOperand")
      || Object.prototype.hasOwnProperty.call(value, "rightOperand")
      || Object.prototype.hasOwnProperty.call(value, "comparisonOperator")
    );
  }

  function attachOutputSyntheticDeclAliases(value, ownerContext) {
    if (!value || typeof value !== "object") {
      return value;
    }
    const objectIndex = Number(ownerContext && ownerContext.__abapTemplateObjectIndex) || 0;
    if (objectIndex <= 0 || typeof attachTemplateSyntheticDeclAliases !== "function") {
      return value;
    }
    for (const key of ["decl", "valueDecl", "leftOperandDecl", "rightOperandDecl"]) {
      if (value[key] && typeof value[key] === "object") {
        attachTemplateSyntheticDeclAliases(value[key], objectIndex);
      }
    }
    return value;
  }

  function normalizeEntryObjectForPath(value, keyHint, pathParts, ownerContext) {
    if (!isPlainObjectRecord(value)) {
      return value;
    }
    if (isDeclLikeObject(value) || isAbapStatementObject(value)) {
      return value;
    }

    const pathKey = buildPathKeyFromParts(pathParts);
    const pathLower = pathKey.toLowerCase();
    const inValuesPath = pathLower.includes("/values/");
    const inExtrasPath = pathLower.includes("/extras/");
    const keyHintLower = String(keyHint || "").trim().toLowerCase();
    const extrasItemLike = keyHintLower === "item";
    const source = getDeclSourceContextFromObject(ownerContext);

    let next = value;

    if (inValuesPath && isValueLikeEntryObject(next)) {
      next = ensureEntryDeclWithSynthetic(next, {
        pathKey,
        file: source.file,
        lineStart: source.lineStart,
        raw: source.raw,
        role: "path:value"
      });
    }

    if (inExtrasPath && extrasItemLike) {
      if (isConditionClauseLikeObject(next)) {
        next = ensureConditionClauseDeclsWithSynthetic(next, {
          pathKey,
          file: source.file,
          lineStart: source.lineStart,
          raw: source.raw
        });
      }

      if (isAssignmentLikeEntryObject(next)) {
        next = ensureValueDeclWithSynthetic(next, {
          pathKey,
          file: source.file,
          lineStart: source.lineStart,
          raw: source.raw,
          role: "path:extras:value",
          nameHint: String(keyHint || "value")
        });
      }

      if (isValueLikeEntryObject(next)) {
        next = ensureEntryDeclWithSynthetic(next, {
          pathKey,
          file: source.file,
          lineStart: source.lineStart,
          raw: source.raw,
          role: "path:extras"
        });
      }
    }

    return attachOutputSyntheticDeclAliases(next, ownerContext);
  }

  function walkObjects(roots, visit) {
    const stack = Array.isArray(roots) ? roots.slice().reverse() : [];
    while (stack.length) {
      const node = stack.pop();
      if (!node) {
        continue;
      }
      visit(node);
      const children = Array.isArray(node.children) ? node.children : [];
      for (let i = children.length - 1; i >= 0; i -= 1) {
        stack.push(children[i]);
      }
    }
  }

  function computeLineOffsets(text) {
    const value = String(text || "");
    const offsets = [0];
    for (let i = 0; i < value.length; i += 1) {
      if (value[i] === "\n") {
        offsets.push(i + 1);
      }
    }
    return offsets;
  }

  function getSelectionRangeForLines(text, lineStart, lineEnd) {
    const value = String(text || "");
    const offsets = state.inputLineOffsets.length ? state.inputLineOffsets : computeLineOffsets(value);
    const start = Math.max(1, Number(lineStart) || 1);
    const end = Math.max(start, Number(lineEnd) || start);
    const startLineIndex = start - 1;
    const endLineIndex = end - 1;
    const startOffset = offsets[startLineIndex] ?? 0;
    const endOffset = offsets[endLineIndex + 1] ?? value.length;
    return { start: startOffset, end: Math.max(startOffset, endOffset) };
  }

  function selectCodeLines(lineStart, lineEnd) {
    if (typeof navigateInputRange === "function") {
      return navigateInputRange({
        lineStart,
        lineEnd,
        segmentIndex: null,
        anchorRatio: 0.28
      });
    }
    const text = els.inputText.value || "";
    state.inputLineOffsets = computeLineOffsets(text);
    const range = getSelectionRangeForLines(text, lineStart, lineEnd);
    els.inputText.focus();
    els.inputText.setSelectionRange(range.start, range.end);
    syncInputGutterScroll();
  }

  function getContainerScrollScale(container) {
    if (!container || typeof container.getBoundingClientRect !== "function") {
      return 1;
    }
    const rect = container.getBoundingClientRect();
    const layoutHeight = Math.max(0, Number(container.offsetHeight) || Number(container.clientHeight) || 0);
    const scale = layoutHeight > 0 ? (Number(rect && rect.height) || 0) / layoutHeight : 1;
    return Number.isFinite(scale) && scale >= 0.5 && scale <= 2 ? scale : 1;
  }

  function scrollElementInContainer(container, element, options) {
    if (!container || !element || typeof container.scrollTop !== "number") {
      return;
    }

    const opts = options && typeof options === "object" ? options : {};
    const mode = String(opts.mode || "nearest").toLowerCase();
    const padTop = Math.max(0, Number(opts.padTop) || 0);
    const padBottom = Math.max(0, Number(opts.padBottom) || 0);
    const behavior = opts.behavior === "smooth" ? "smooth" : "auto";

    const containerRect = container.getBoundingClientRect();
    const elementRect = element.getBoundingClientRect();
    if (!containerRect || !elementRect || !Number.isFinite(containerRect.top) || !Number.isFinite(elementRect.top)) {
      return;
    }

    const currentTop = Number(container.scrollTop || 0) || 0;
    const scrollScale = getContainerScrollScale(container);
    let nextTop = currentTop;

    if (mode === "start") {
      nextTop = currentTop + ((elementRect.top - containerRect.top - padTop) / scrollScale);
    } else if (mode === "center") {
      nextTop = currentTop
        + ((elementRect.top + (elementRect.height / 2)
          - (containerRect.top + (containerRect.height / 2))) / scrollScale);
    } else {
      const topLimit = containerRect.top + padTop;
      const bottomLimit = containerRect.bottom - padBottom;
      if (elementRect.top < topLimit) {
        nextTop = currentTop + ((elementRect.top - topLimit) / scrollScale);
      } else if (elementRect.bottom > bottomLimit) {
        nextTop = currentTop + ((elementRect.bottom - bottomLimit) / scrollScale);
      } else {
        return;
      }
    }

    const maxTop = Math.max(0, Number(container.scrollHeight || 0) - Number(container.clientHeight || 0));
    const clampedTop = Math.max(0, Math.min(maxTop, Math.round(nextTop)));
    if (Math.abs(clampedTop - currentTop) < 1) {
      return;
    }

    if (typeof container.scrollTo === "function") {
      container.scrollTo({ top: clampedTop, behavior });
    } else {
      container.scrollTop = clampedTop;
    }
  }

  function setSelectedCard(id, options) {
    const normalized = normalizeId(id);
    if (!normalized) {
      return;
    }
    const opts = options && typeof options === "object" ? options : {};
    const shouldScroll = opts.scroll !== false;
    const scrollMode = String(opts.scrollMode || "start").toLowerCase();

    if (state.selectedId) {
      const prev = els.output.querySelector(`[data-id="${escapeSelectorValue(state.selectedId)}"]`);
      if (prev) {
        prev.classList.remove("selected");
      }
    }

    state.selectedId = normalized;
    const next = els.output.querySelector(`[data-id="${escapeSelectorValue(normalized)}"]`);
    if (next) {
      next.classList.add("selected");
      if (shouldScroll) {
        scrollElementInContainer(els.output, next, { mode: scrollMode, padTop: 10, padBottom: 10 });
      }
    }
  }

  function getSelectedTemplateIndexSet() {
    if (!(state.selectedTemplateIndexes instanceof Set)) {
      state.selectedTemplateIndexes = new Set(Array.isArray(state.selectedTemplateIndexes)
        ? state.selectedTemplateIndexes.map(String)
        : []);
    }
    return state.selectedTemplateIndexes;
  }

  function getSortedSelectedTemplateIndexes() {
    return Array.from(getSelectedTemplateIndexSet())
      .map((value) => String(value || "").trim())
      .filter((value) => /^\d+$/.test(value))
      .sort((left, right) => Number(left) - Number(right));
  }

  function updateTemplateCopySelectedButton() {
    if (!els.templateCopySelectedBtn) {
      return;
    }
    const count = getSelectedTemplateIndexSet().size;
    els.templateCopySelectedBtn.textContent = `Copy Selected (${count})`;
    els.templateCopySelectedBtn.disabled = count === 0;
  }

  function syncRenderedTemplateSelection() {
    if (!els.templatePreviewOutput) {
      updateTemplateCopySelectedButton();
      return;
    }
    const selectedIndexes = getSelectedTemplateIndexSet();
    for (const block of Array.from(els.templatePreviewOutput.querySelectorAll(".template-block[data-template-index]"))) {
      const indexText = String(block.getAttribute("data-template-index") || "");
      const selected = selectedIndexes.has(indexText);
      block.classList.toggle("selected", selected);
      block.setAttribute("aria-selected", selected ? "true" : "false");
    }
    updateTemplateCopySelectedButton();
  }

  function clearTemplateBlockSelection() {
    getSelectedTemplateIndexSet().clear();
    state.selectedTemplateIndex = "";
    state.templateSelectionAnchorIndex = "";
    syncRenderedTemplateSelection();
  }

  function pruneTemplateBlockSelection(itemCount) {
    const total = Math.max(0, Number(itemCount) || 0);
    const selectedIndexes = getSelectedTemplateIndexSet();
    const primary = String(state.selectedTemplateIndex || "").trim();
    if (!selectedIndexes.size && /^\d+$/.test(primary) && Number(primary) < total) {
      selectedIndexes.add(primary);
    }
    for (const indexText of Array.from(selectedIndexes)) {
      if (!/^\d+$/.test(indexText) || Number(indexText) < 0 || Number(indexText) >= total) {
        selectedIndexes.delete(indexText);
      }
    }
    if (!selectedIndexes.has(primary)) {
      state.selectedTemplateIndex = getSortedSelectedTemplateIndexes()[0] || "";
    }
    const anchor = String(state.templateSelectionAnchorIndex || "").trim();
    if (!/^\d+$/.test(anchor) || Number(anchor) < 0 || Number(anchor) >= total) {
      state.templateSelectionAnchorIndex = "";
    }
    syncRenderedTemplateSelection();
  }

  function chooseNearestSelectedTemplateIndex(targetIndex) {
    const target = Number(targetIndex);
    const indexes = getSortedSelectedTemplateIndexes();
    indexes.sort((left, right) => {
      const leftDistance = Math.abs(Number(left) - target);
      const rightDistance = Math.abs(Number(right) - target);
      return leftDistance === rightDistance ? Number(left) - Number(right) : leftDistance - rightDistance;
    });
    return indexes[0] || "";
  }

  function updateTemplateBlockSelection(index, options) {
    if (!els.templatePreviewOutput) {
      return false;
    }
    const opts = options && typeof options === "object" ? options : {};
    const normalized = String(index === undefined || index === null ? "" : index).trim();
    if (!/^\d+$/.test(normalized)) {
      return false;
    }
    const targetIndex = Number(normalized);
    const selectedIndexes = getSelectedTemplateIndexSet();
    const event = opts.event && typeof opts.event === "object" ? opts.event : null;
    const useInteractionModifiers = opts.interactionMode === "event";
    const additive = useInteractionModifiers && Boolean(event && (event.ctrlKey || event.metaKey));
    const ranged = useInteractionModifiers && Boolean(event && event.shiftKey);

    if (ranged) {
      const primary = String(state.selectedTemplateIndex || "").trim();
      const anchorText = /^\d+$/.test(String(state.templateSelectionAnchorIndex || ""))
        ? String(state.templateSelectionAnchorIndex)
        : (/^\d+$/.test(primary) ? primary : normalized);
      const anchorIndex = Number(anchorText);
      if (!additive) {
        selectedIndexes.clear();
      }
      const start = Math.min(anchorIndex, targetIndex);
      const end = Math.max(anchorIndex, targetIndex);
      for (let current = start; current <= end; current += 1) {
        selectedIndexes.add(String(current));
      }
      state.selectedTemplateIndex = normalized;
      state.templateSelectionAnchorIndex = anchorText;
    } else if (additive) {
      if (selectedIndexes.has(normalized)) {
        selectedIndexes.delete(normalized);
        if (state.selectedTemplateIndex === normalized) {
          state.selectedTemplateIndex = chooseNearestSelectedTemplateIndex(targetIndex);
        }
      } else {
        selectedIndexes.add(normalized);
        state.selectedTemplateIndex = normalized;
      }
      state.templateSelectionAnchorIndex = normalized;
    } else {
      selectedIndexes.clear();
      selectedIndexes.add(normalized);
      state.selectedTemplateIndex = normalized;
      state.templateSelectionAnchorIndex = normalized;
    }

    const shouldEnsure = opts.ensure !== false;
    let next = els.templatePreviewOutput.querySelector(
      `.template-block[data-template-index="${escapeSelectorValue(normalized)}"]`
    );
    if (!next && shouldEnsure && typeof ensureTemplateWindowContainsIndex === "function") {
      ensureTemplateWindowContainsIndex(targetIndex);
      next = els.templatePreviewOutput.querySelector(
        `.template-block[data-template-index="${escapeSelectorValue(normalized)}"]`
      );
    }
    syncRenderedTemplateSelection();

    if (next && opts.scroll !== false) {
      const scrollMode = String(opts.scrollMode || "start").toLowerCase();
      const virtual = typeof getTemplateVirtualStateForOutput === "function"
        ? getTemplateVirtualStateForOutput()
        : null;
      if (scrollMode === "start" && virtual && virtual.isInitialized && typeof alignVirtualTargetAfterRender === "function") {
        alignVirtualTargetAfterRender(
          els.templatePreviewOutput,
          virtual,
          `.template-block[data-template-index="${escapeSelectorValue(normalized)}"]`,
          next
        );
      } else {
        scrollElementInContainer(els.templatePreviewOutput, next, { mode: scrollMode, padTop: 10, padBottom: 10 });
      }
    }
    return true;
  }

  function selectTemplateBlockFromInteraction(index, event) {
    return updateTemplateBlockSelection(index, {
      interactionMode: "event",
      event,
      scroll: false,
      ensure: false
    });
  }

  function setSelectedTemplateBlock(index, options) {
    return updateTemplateBlockSelection(index, {
      ...(options && typeof options === "object" ? options : {}),
      interactionMode: "replace"
    });
  }

  function setSelectedDeclRow(declKey) {
    const key = String(declKey || "").trim();
    if (!key || !els.declDescTable) {
      return;
    }

    if (state.selectedDeclKey) {
      const prev = els.declDescTable.querySelector(`tr[data-decl-key="${escapeSelectorValue(state.selectedDeclKey)}"]`);
      if (prev) {
        prev.classList.remove("desc-selected");
      }
    }

    state.selectedDeclKey = key;
    const next = els.declDescTable.querySelector(`tr[data-decl-key="${escapeSelectorValue(key)}"]`);
    if (next) {
      next.classList.add("desc-selected");
      scrollElementInContainer(els.declDescPanel, next, { mode: "nearest", padTop: 8, padBottom: 8 });
    }
  }

  function countInputLines(value) {
    const text = String(value || "");
    if (!text) {
      return 1;
    }
    return text.split("\n").length;
  }

  function syncInputGutterScroll() {
    if (!els.inputText || !els.inputGutterContent) {
      return;
    }

    const scrollTop = Number(els.inputText.scrollTop || 0) || 0;
    els.inputGutterContent.style.transform = `translateY(${-scrollTop}px)`;
  }

  function rebuildInputGutter() {
    if (!els.inputText || !els.inputGutterContent) {
      return;
    }

    const trimmed = String(els.inputText.value || "").trim();
    const isJsonLike = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonLike ? "json" : "abap";

    const lineCount = Math.max(1, countInputLines(els.inputText.value || ""));
    if (lineCount === state.inputLineCount && state.inputGutterButtonsByLine.size) {
      syncInputGutterScroll();
      refreshInputGutterTargets();
      return;
    }

    state.inputLineCount = lineCount;
    state.inputGutterButtonsByLine = new Map();

    const frag = document.createDocumentFragment();
    for (let line = 1; line <= lineCount; line += 1) {
      const row = document.createElement("div");
      row.className = "gutter-line";

      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "gutter-jump";
      btn.textContent = "↪";
      btn.hidden = true;
      btn.setAttribute("data-line", String(line));
      row.appendChild(btn);

      const num = document.createElement("span");
      num.className = "gutter-num";
      num.textContent = String(line);
      row.appendChild(num);

      state.inputGutterButtonsByLine.set(line, btn);
      frag.appendChild(row);
    }

    els.inputGutterContent.replaceChildren(frag);
    syncInputGutterScroll();
    refreshInputGutterTargets();
  }

  function computeInputGutterTargetsForOutput() {
    const targets = new Map();
    if (!els.output) {
      return targets;
    }

    const cards = els.output.querySelectorAll(".card[data-id][data-line-start]");
    for (const card of Array.from(cards)) {
      const line = Number(card.getAttribute("data-line-start")) || 0;
      const id = String(card.getAttribute("data-id") || "");
      if (!line || !id || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "output", id });
    }

    return targets;
  }

  function computeInputGutterTargetsForDescriptions() {
    const targets = new Map();
    if (!els.declDescTable) {
      return targets;
    }


    const rows = els.declDescTable.querySelectorAll("tr[data-decl-key][data-line-start]");
    for (const row of Array.from(rows)) {
      const line = Number(row.getAttribute("data-line-start")) || 0;
      const declKey = String(row.getAttribute("data-decl-key") || "");
      if (!line || !declKey || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "descriptions", declKey });
    }

    return targets;
  }

  function computeInputGutterTargetsForTemplate() {
    const targets = new Map();
    if (!els.templatePreviewOutput) {
      return targets;
    }

    const blocks = els.templatePreviewOutput.querySelectorAll(".template-block[data-template-index][data-line-start]");
    for (const block of Array.from(blocks)) {
      const line = Number(block.getAttribute("data-line-start")) || 0;
      const index = String(block.getAttribute("data-template-index") || "");
      if (!line || !index || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "template", index });
    }

    return targets;
  }

  function refreshInputGutterTargets() {
    if (!els.inputGutterContent || !state.inputGutterButtonsByLine.size) {
      return;
    }

    if (state.inputMode !== "abap") {
      state.inputGutterTargetsByLine = new Map();
      for (const btn of state.inputGutterButtonsByLine.values()) {
        btn.hidden = true;
      }
      return;
    }

    let targets = new Map();
    if (state.rightTab === "descriptions") {
      targets = computeInputGutterTargetsForDescriptions();
    } else if (state.rightTab === "output") {
      targets = computeInputGutterTargetsForOutput();
    } else if (state.rightTab === "template") {
      targets = computeInputGutterTargetsForTemplate();
    }

    state.inputGutterTargetsByLine = targets;
    const title = state.rightTab === "descriptions"
      ? "Jump to Descriptions"
      : (state.rightTab === "output"
        ? "Jump to Output"
        : (state.rightTab === "template" ? "Jump to Template" : ""));
    for (const [line, btn] of state.inputGutterButtonsByLine.entries()) {
      const target = targets.get(line);
      btn.hidden = !target;
      if (target) {
        btn.title = title;
        btn.setAttribute("aria-label", title);
      }
    }
  }

  function onInputGutterClick(ev) {
    const target = ev && ev.target && typeof ev.target.closest === "function"
      ? ev.target.closest("button.gutter-jump")
      : null;
    if (!target) {
      return;
    }

    const line = Number(target.getAttribute("data-line")) || 0;
    if (!line) {
      return;
    }

    const jumpTarget = state.inputGutterTargetsByLine.get(line);
    if (!jumpTarget) {
      return;
    }

    if (state.rightTab === "output" && jumpTarget.kind === "output") {
      setSelectedCard(jumpTarget.id, { scroll: true, scrollMode: "start" });
      return;
    }

    if (state.rightTab === "template" && jumpTarget.kind === "template") {
      setSelectedTemplateBlock(jumpTarget.index, { scroll: true, scrollMode: "start" });
      return;
    }

    if (state.rightTab === "descriptions" && jumpTarget.kind === "descriptions") {
      setSelectedDeclRow(jumpTarget.declKey);
    }
  }

  function openJsonModal(value) {
    openTextModal("Object JSON", safeJson(value, true));
  }

  function openTextModal(title, text) {
    if (!els.editModal.hidden) {
      closeEditModal();
    }

    if (els.jsonTitle) {
      els.jsonTitle.textContent = title ? String(title) : "";
    }

    els.jsonPre.textContent = text ? String(text) : "";
    els.jsonModal.hidden = false;
  }

  function closeJsonModal() {
    els.jsonModal.hidden = true;
    if (els.jsonTitle) {
      els.jsonTitle.textContent = "Object JSON";
    }
    els.jsonPre.textContent = "";
  }

  async function copyJsonToClipboard() {
    const text = els.jsonPre.textContent || "";
    if (!text) {
      return;
    }

    if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
      await navigator.clipboard.writeText(text);
      return;
    }

    const selection = window.getSelection();
    if (!selection) {
      return;
    }
    selection.removeAllRanges();
    const range = document.createRange();
    range.selectNodeContents(els.jsonPre);
    selection.addRange(range);
    document.execCommand("copy");
    selection.removeAllRanges();
  }

  function stringifyDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const techName = getDeclTechName(decl);
    const displayName = getDeclDisplayName(decl);

    const parts = [];
    if (decl.objectType) {
      parts.push(String(decl.objectType));
    }
    if (decl.scopeLabel) {
      parts.push(`[${String(decl.scopeLabel)}]`);
    }
    if (displayName) {
      parts.push(displayName);
    }
    if (techName && displayName && techName !== displayName) {
      parts.push(`(${techName})`);
    }
    if (decl.file) {
      parts.push(String(decl.file));
    }
    if (decl.lineStart) {
      parts.push(`#${decl.lineStart}`);
    }
    return parts.join(" ");
  }

  function getDeclTechName(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    return decl.name ? String(decl.name) : "";
  }

  function stripAngleBrackets(text) {
    const trimmed = String(text || "").trim();
    if (trimmed.startsWith("<") && trimmed.endsWith(">") && trimmed.length > 2) {
      return trimmed.slice(1, -1);
    }
    return trimmed;
  }

  function stripDeclCategoryPrefix(text) {
    const raw = String(text || "").trim();
    if (!raw) {
      return "";
    }

    const match = raw.match(
      /^\s*(HẰNG|HANG|STRUCT|TABLE|RANGETABLE|BIẾN|BIEN|CỜ|CO|FIELDSYMBOL)\b\s*[:\-\[\]]*\s*/i
    );
    if (!match) {
      return raw;
    }

    let rest = raw.slice(match[0].length).trim();
    // If the prefix included an opening "[" (e.g. "BIẾN:[") then strip ONE trailing "]" (e.g. "...]")
    // so "BIẾN:[desc]" -> "desc" instead of "desc]".
    if (match[0].includes("[") && rest.endsWith("]")) {
      rest = rest.slice(0, -1).trim();
    }
    return rest;
  }

  function isStructFieldDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    if (decl.objectType === "STRUCT_FIELD") {
      return true;
    }
    return Boolean(decl.structName && decl.fieldPath);
  }

  function getDeclDisplayName(decl) {
    const techName = getDeclTechName(decl);
    if (!techName) {
      return "";
    }

    if (isStructFieldDecl(decl)) {
      return techName;
    }

    const desc = getEffectiveDeclDesc(decl);
    const descTrimmed = String(desc || "").trim();
    if (!descTrimmed) {
      return techName;
    }

    const settings = state.settings || DEFAULT_SETTINGS;
    if (settings.normalizeDeclDesc) {
      return descTrimmed;
    }

    const bare = stripAngleBrackets(techName);
    if (bare.length < 3) {
      return techName;
    }

    const code = bare.slice(1, 3).toUpperCase();
    const templates = settings.nameTemplatesByCode || DEFAULT_SETTINGS.nameTemplatesByCode;
    const template = templates && Object.prototype.hasOwnProperty.call(templates, code) ? String(templates[code] || "") : "";
    if (!template.trim()) {
      return techName;
    }

    const strippedKnownPrefix = stripDeclCategoryPrefix(descTrimmed);
    const normalizedDesc = stripDeclTemplateAffixes(strippedKnownPrefix, template);
    const displayName = template.replace(/\{\{desc\}\}/g, normalizedDesc).trim();
    return displayName || techName;
  }

  function buildDeclTitle(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const lines = [];
    lines.push(stringifyDecl(decl));

    const desc = getEffectiveDeclDesc(decl);
    if (desc) {
      lines.push(`Desc: ${desc}`);
    }

    if (decl.raw) {
      lines.push(String(decl.raw));
    }

    return lines.filter(Boolean).join("\n");
  }

  function renderDeclNameCell(decl) {
    const text = decl ? getDeclDisplayName(decl) : "";
    const title = buildDeclTitle(decl);
    const cell = el("td", { text, attrs: title ? { title } : {} });

    if (decl && decl.lineStart) {
      cell.style.cursor = "pointer";
      cell.addEventListener("click", (ev) => {
        ev.stopPropagation();
        selectCodeLines(decl.lineStart, decl.lineStart);
        if (decl.id) {
          setSelectedCard(decl.id);
        }
      });
    }

    return cell;
  }

  function renderDeclDescCell(decl) {
    const text = getEffectiveDeclDesc(decl);
    const title = buildDeclTitle(decl);
    const cell = el("td", { attrs: title ? { title } : {} });

    const wrap = el("div", { className: "decl-desc" });
    wrap.appendChild(el("div", { className: "decl-desc-text", text: text || "" }));

    if (getDeclOverrideStorageKey(decl)) {
      const btn = el("button", {
        className: "icon-btn",
        text: "✎",
        attrs: {
          type: "button",
          title: "Edit decl description",
          "aria-label": "Edit decl description"
        }
      });
      btn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        editDeclDesc(decl);
      });
      wrap.appendChild(btn);
    }

    cell.appendChild(wrap);

    return cell;

  }

  var PERFORM_TRACE_META_KEY_OUTPUT = "__abapPerformTraceBinding";

  function getDeclRenderKey(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    return (
      getDeclKey(decl) ||
      [
        decl.objectType || "",
        decl.scopeLabel || "",
        decl.name || "",
        decl.file || "",
        decl.lineStart || ""
      ].join("|")
    );
  }

  function dedupeDecls(list) {
    const output = [];
    const seen = new Set();
    for (const decl of list || []) {
      if (!decl) {
        continue;
      }
      const key = getDeclRenderKey(decl);
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      output.push(decl);
    }
    return output;
  }

  function getExpandedPerformBindingContext(obj) {
    if (!obj || typeof obj !== "object") {
      return null;
    }
    const bindingContext = obj[PERFORM_TRACE_META_KEY_OUTPUT];
    if (!bindingContext || typeof bindingContext !== "object") {
      return null;
    }
    if (!(bindingContext.byParamUpper instanceof Map)) {
      return null;
    }
    return bindingContext;
  }

  function isExpandedPerformOutputTraceableDecl(decl) {
    if (!decl || typeof decl !== "object") {
      return false;
    }
    const objectType = String(decl.objectType || "").toUpperCase();
    if (objectType === "FORM_PARAM") {
      return true;
    }
    return objectType === "STRUCT_FIELD"
      && String(decl.structObjectType || "").toUpperCase() === "FORM_PARAM"
      && String(decl.structName || "").trim() !== ""
      && String(decl.fieldPath || "").trim() !== "";
  }

  function getExpandedPerformOutputParamUpper(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }
    const objectType = String(decl.objectType || "").toUpperCase();
    if (objectType === "FORM_PARAM") {
      return String(decl.name || "").trim().toUpperCase();
    }
    if (objectType === "STRUCT_FIELD" && String(decl.structObjectType || "").toUpperCase() === "FORM_PARAM") {
      return String(decl.structName || "").trim().toUpperCase();
    }
    return "";
  }

  function buildExpandedPerformOutputTraceDecl(baseDecl, localDecl, ownerContext) {
    if (!baseDecl || typeof baseDecl !== "object") {
      return null;
    }
    if (!localDecl || typeof localDecl !== "object" || String(localDecl.objectType || "").toUpperCase() !== "STRUCT_FIELD") {
      return baseDecl;
    }

    const localFieldPath = String(localDecl.fieldPath || "").trim();
    if (!localFieldPath) {
      return baseDecl;
    }

    let rootBaseDecl = baseDecl;
    let rootStructName = String(baseDecl.name || "").trim();
    let prefixFieldPath = "";
    if (String(baseDecl.objectType || "").toUpperCase() === "STRUCT_FIELD") {
      rootStructName = String(baseDecl.structName || rootStructName).trim();
      prefixFieldPath = String(baseDecl.fieldPath || "").trim();
      rootBaseDecl = {
        ...baseDecl,
        id: baseDecl.structId || baseDecl.id || null,
        objectType: String(baseDecl.structObjectType || "STRUCT"),
        name: rootStructName,
        lineStart: Number(baseDecl.structLineStart || baseDecl.lineStart) || null,
        raw: String(baseDecl.structRaw || baseDecl.raw || ""),
        comment: String(baseDecl.structComment || baseDecl.comment || "")
      };
    }

    if (!rootStructName || !String(rootBaseDecl.scopeLabel || "").trim()) {
      return baseDecl;
    }

    const combinedFieldPath = prefixFieldPath ? (prefixFieldPath + "-" + localFieldPath) : localFieldPath;
    const candidate = {
      fullRef: rootStructName + "-" + combinedFieldPath,
      structName: rootStructName,
      fieldPath: combinedFieldPath
    };
    const traceContext = ownerContext && typeof ownerContext === "object"
      ? { file: ownerContext.file, lineStart: ownerContext.lineStart }
      : { file: rootBaseDecl.file, lineStart: rootBaseDecl.lineStart };
    if (typeof createSyntheticStructFieldDecl === "function") {
      const syntheticDecl = createSyntheticStructFieldDecl(rootBaseDecl, candidate, traceContext);
      if (syntheticDecl && typeof syntheticDecl === "object") {
        return syntheticDecl;
      }
    }

    return {
      id: rootBaseDecl.id || null,
      objectType: "STRUCT_FIELD",
      name: candidate.fullRef,
      file: String(traceContext.file || rootBaseDecl.file || ""),
      lineStart: Number(traceContext.lineStart || rootBaseDecl.lineStart) || null,
      raw: String(rootBaseDecl.raw || ""),
      comment: "",
      scopeId: Number(rootBaseDecl.scopeId || 0) || 0,
      scopeLabel: String(rootBaseDecl.scopeLabel || ""),
      scopeType: String(rootBaseDecl.scopeType || ""),
      scopeName: String(rootBaseDecl.scopeName || ""),
      structId: rootBaseDecl.id || null,
      structName: rootStructName,
      structObjectType: String(rootBaseDecl.objectType || "STRUCT"),
      structLineStart: Number(rootBaseDecl.lineStart || 0) || null,
      structRaw: String(rootBaseDecl.raw || ""),
      structComment: String(rootBaseDecl.comment || ""),
      traceFile: String(traceContext.file || ""),
      traceLineStart: Number(traceContext.lineStart || 0) || null,
      fieldPath: combinedFieldPath,
      synthetic: true
    };
  }

  function resolveExpandedPerformTraceDecls(obj, decl) {
    if (!isExpandedPerformOutputTraceableDecl(decl)) {
      return [];
    }
    const bindingContext = getExpandedPerformBindingContext(obj);
    if (!bindingContext) {
      return [];
    }
    const paramUpper = getExpandedPerformOutputParamUpper(decl);
    if (!paramUpper) {
      return [];
    }
    const traceDecls = bindingContext.byParamUpper.get(paramUpper);
    if (!Array.isArray(traceDecls) || !traceDecls.length) {
      return [];
    }
    const localDeclType = String(decl.objectType || "").toUpperCase();
    const remappedTraceDecls = localDeclType === "STRUCT_FIELD"
      ? traceDecls.map((traceDecl) => buildExpandedPerformOutputTraceDecl(traceDecl, decl, obj)).filter(Boolean)
      : traceDecls;
    const scopedTraceDecls = typeof cloneDeclWithPerformChainOverride === "function"
      ? remappedTraceDecls.map((traceDecl) => cloneDeclWithPerformChainOverride(traceDecl, obj, decl))
      : remappedTraceDecls;
    return dedupeDecls(scopedTraceDecls.filter((item) => item && typeof item === "object"));
  }

  function buildDeclTraceListForObject(obj, decl) {
    const list = [];
    if (decl && typeof decl === "object") {
      list.push(typeof cloneDeclWithPerformChainOverride === "function"
        ? cloneDeclWithPerformChainOverride(decl, obj, decl)
        : decl);
    }
    const traceDecls = resolveExpandedPerformTraceDecls(obj, decl);
    if (traceDecls.length) {
      list.push(...traceDecls);
    }
    return dedupeDecls(list);
  }

  function getDeclListDisplayName(decl) {
    if (!decl || typeof decl !== "object") {
      return "";
    }

    const displayName = String(getDeclDisplayName(decl) || "").trim();
    const descText = String(getEffectiveDeclDesc(decl) || "").trim();
    if (
      displayName &&
      descText &&
      displayName.localeCompare(descText, undefined, { sensitivity: "accent" }) === 0
    ) {
      const techName = String(getDeclTechName(decl) || "").trim();
      if (techName) {
        return techName;
      }
    }

    return displayName || String(getDeclTechName(decl) || "").trim();
  }

  function renderDeclListCells(decls, fallbackDecl) {
    const list = dedupeDecls(Array.isArray(decls) ? decls.filter(Boolean) : []);
    const effectiveList = list.length ? list : dedupeDecls(fallbackDecl ? [fallbackDecl] : []);

    const nameCell = el("td");
    const descCell = el("td");

    if (!effectiveList.length) {
      return { nameCell, descCell };
    }

    const nameWrap = el("div");
    const descWrap = el("div");

    for (const decl of effectiveList) {
      const nameLine = el("div", { text: getDeclListDisplayName(decl) });
      const title = buildDeclTitle(decl);
      if (title) {
        nameLine.title = title;
      }

      if (decl && decl.lineStart) {
        nameLine.style.cursor = "pointer";
        nameLine.addEventListener("click", (ev) => {
          ev.stopPropagation();
          selectCodeLines(decl.lineStart, decl.lineStart);
          if (decl.id) {
            setSelectedCard(decl.id);
          }
        });
      }

      nameWrap.appendChild(nameLine);

      const descLineWrap = el("div", { className: "decl-desc-line" });
      const descLineText = el("div", { className: "decl-desc-text", text: getEffectiveDeclDesc(decl) || "" });
      if (title) {
        descLineText.title = title;
      }
      descLineWrap.appendChild(descLineText);

      if (getDeclOverrideStorageKey(decl)) {
        const btn = el("button", {
          className: "icon-btn",
          text: "✎",
          attrs: {
            type: "button",
            title: "Edit decl description",
            "aria-label": "Edit decl description"
          }
        });
        btn.addEventListener("click", (ev) => {
          ev.stopPropagation();
          editDeclDesc(decl);
        });
        descLineWrap.appendChild(btn);
      }

      descWrap.appendChild(descLineWrap);
    }

    nameCell.appendChild(nameWrap);
    descCell.appendChild(descWrap);
    return { nameCell, descCell };
  }

  function matchesFilters() {
    return true;
  }

  function filterTree(obj) {
    const children = Array.isArray(obj.children) ? obj.children : [];
    const filteredChildren = [];
    for (const child of children) {
      const filtered = filterTree(child);
      if (filtered) {
        filteredChildren.push(filtered);
      }
    }

    const selfMatches = matchesFilters(obj);
    if (!selfMatches && !filteredChildren.length) {
      return null;
    }

    return { obj, children: filteredChildren, selfMatches };
  }

  function el(tag, options) {
    const node = document.createElement(tag);
    if (options && options.className) {
      node.className = options.className;
    }
    if (options && options.text !== undefined) {
      node.textContent = options.text;
    }
    if (options && options.attrs) {
      for (const [key, value] of Object.entries(options.attrs)) {
        node.setAttribute(key, String(value));
      }
    }
    return node;
  }

  function renderKeywords(obj) {
    const keywords = getKeywordEntries(obj);
    if (!keywords.length) {
      return null;
    }

    const wrap = el("div");
    for (const keyword of keywords) {
      wrap.appendChild(
        el("span", {
          className: "pill",
          text: keyword && keyword.text ? String(keyword.text) : "",
          attrs: keyword && keyword.label ? { title: String(keyword.label) } : {}
        })
      );
    }
    return wrap;
  }

  function collectDeclsForValueEntry(obj, entry) {
    const decls = [];
    const addDecl = (decl) => {
      if (decl && typeof decl === "object") {
        decls.push(decl);
      }
    };
    const addDeclWithTrace = (decl) => {
      if (!decl || typeof decl !== "object") {
        return;
      }
      const traceList = buildDeclTraceListForObject(obj, decl);
      if (!traceList.length) {
        addDecl(decl);
        return;
      }
      for (const traceDecl of traceList) {
        addDecl(traceDecl);
      }
    };

    const normalizedEntry = ensureEntryDeclForOutput(obj, cloneValueEntryForOutput(entry));

    addDeclWithTrace(normalizedEntry && normalizedEntry.decl);
    addDeclWithTrace(normalizedEntry && normalizedEntry.valueDecl);
    if (normalizedEntry && Array.isArray(normalizedEntry.originDecls)) {
      for (const originDecl of normalizedEntry.originDecls) {
        addDeclWithTrace(originDecl);
      }
    }

    const extras = obj && obj.extras && typeof obj.extras === "object" ? obj.extras : null;
    if (!extras) {
      return dedupeDecls(decls);
    }

    const entryName = String(normalizedEntry && normalizedEntry.name ? normalizedEntry.name : "").trim().toLowerCase();
    if (!entryName) {
      return dedupeDecls(decls);
    }

    if (entryName === "condition" && extras.ifCondition) {
      collectConditionDeclsFromClauses(extras.ifCondition.conditions, addDeclWithTrace);
    } else if (entryName === "ifcondition" && extras.performCall) {
      collectConditionDeclsFromClauses(extras.performCall.ifConditions, addDeclWithTrace);
    } else if (entryName === "where") {
      if (extras.select) {
        collectConditionDeclsFromClauses(extras.select.whereConditions, addDeclWithTrace);
      }
      if (extras.loopAtItab) {
        collectConditionDeclsFromClauses(extras.loopAtItab.conditions, addDeclWithTrace);
      }
      if (extras.modifyItab) {
        collectConditionDeclsFromClauses(extras.modifyItab.conditions, addDeclWithTrace);
      }
      if (extras.deleteItab) {
        collectConditionDeclsFromClauses(extras.deleteItab.conditions, addDeclWithTrace);
      }
    } else if (entryName === "having" && extras.select) {
      collectConditionDeclsFromClauses(extras.select.havingConditions, addDeclWithTrace);
    } else if ((entryName === "withkey" || entryName === "withtablekey") && extras.readTable) {
      collectConditionDeclsFromClauses(extras.readTable.conditions, addDeclWithTrace);
    } else if (entryName === "position" && extras.write && extras.write.position) {
      addDeclWithTrace(extras.write.position.columnDecl);
      addDeclWithTrace(extras.write.position.lengthDecl);
    }

    return dedupeDecls(decls);
  }

  function isRawValueEntry(entry) {
    const name = entry && entry.name ? String(entry.name).trim() : "";
    return Boolean(name) && /raw$/i.test(name);
  }

  function makeSectionIndexName(sectionName, indexOneBased) {
    return `${sectionName}[${indexOneBased}]`;
  }

  function makeParsedValueRow({
    sectionName,
    indexOneBased,
    valueText,
    baseEntry,
    decl,
    valueDecl,
    originDecls,
    valueRef,
    rowName
  }) {
    const row = {
      name: rowName || makeSectionIndexName(sectionName, indexOneBased),
      value: valueText ? String(valueText) : "",
      label: baseEntry && baseEntry.label ? String(baseEntry.label) : sectionName,
      userDesc: baseEntry && baseEntry.userDesc ? String(baseEntry.userDesc) : "",
      codeDesc: baseEntry && baseEntry.codeDesc ? String(baseEntry.codeDesc) : ""
    };

    if (decl && typeof decl === "object") {
      row.decl = decl;
    }
    if (valueDecl && typeof valueDecl === "object") {
      row.valueDecl = valueDecl;
    }
    if (Array.isArray(originDecls) && originDecls.length) {
      row.originDecls = originDecls.filter((item) => item && typeof item === "object");
    }
    if (valueRef) {
      row.declRef = String(valueRef);
    }

    return row;
  }

  function buildParsedRowsFromPerformList(sectionName, list, baseEntry) {
    const items = Array.isArray(list) ? list : [];
    return items.map((item, index) => makeParsedValueRow({
      sectionName,
      indexOneBased: index + 1,
      valueText: item && item.value ? String(item.value) : "",
      baseEntry,
      decl: item && item.valueDecl ? item.valueDecl : (baseEntry && baseEntry.decl ? baseEntry.decl : null),
      valueDecl: item && item.valueDecl ? item.valueDecl : null,
      originDecls: item && Array.isArray(item.originDecls) ? item.originDecls : [],
      valueRef: item && item.valueRef ? String(item.valueRef) : ""
    }));
  }

  function buildParsedRowsFromWriteFormats(list, baseEntry) {
    const items = Array.isArray(list) ? list : [];
    return items.map((item, index) => {
      const keyword = String(item && item.keyword || "").trim();
      return makeParsedValueRow({
        sectionName: "format",
        indexOneBased: index + 1,
        rowName: `format[${index + 1}].${keyword}`,
        valueText: item && item.value ? String(item.value) : "",
        baseEntry,
        decl: item && item.valueDecl ? item.valueDecl : null,
        valueDecl: item && item.valueDecl ? item.valueDecl : null,
        originDecls: item && Array.isArray(item.originDecls) ? item.originDecls : [],
        valueRef: item && item.valueRef ? String(item.valueRef) : ""
      });
    });
  }

  function buildParsedRowsFromAssignments(sectionName, list, baseEntry) {
    const items = Array.isArray(list) ? list : [];
    return items.map((item, index) => {
      const leftName = item && item.name ? String(item.name) : "";
      const rightValue = item && item.value ? String(item.value) : "";
      const valueText = leftName && rightValue
        ? `${leftName} = ${rightValue}`
        : (leftName || rightValue);
      return makeParsedValueRow({
        sectionName,
        indexOneBased: index + 1,
        valueText,
        baseEntry,
        decl: item && item.valueDecl ? item.valueDecl : (baseEntry && baseEntry.decl ? baseEntry.decl : null),
        valueDecl: item && item.valueDecl ? item.valueDecl : null,
        originDecls: item && Array.isArray(item.originDecls) ? item.originDecls : [],
        valueRef: item && item.valueRef ? String(item.valueRef) : ""
      });
    });
  }

  function buildParsedRowsFromParams(sectionName, list, baseEntry) {
    const items = Array.isArray(list) ? list : [];
    return items.map((param, index) => {
      const paramName = param && param.name ? String(param.name) : "";
      const typingText = formatTyping(param && param.typing);
      const valueText = [paramName, typingText].filter(Boolean).join(" ").trim();
      const origins = param && Array.isArray(param.originDecls) ? param.originDecls : [];
      return makeParsedValueRow({
        sectionName,
        indexOneBased: index + 1,
        valueText,
        baseEntry,
        decl: origins.length ? origins[0] : (baseEntry && baseEntry.decl ? baseEntry.decl : null),
        valueDecl: null,

        originDecls: origins,
        valueRef: ""
      });
    });
  }

  function buildParsedRowsFromExceptions(sectionName, list, baseEntry) {
    const items = Array.isArray(list) ? list : [];
    return items.map((item, index) => {
      const name = item && item.name ? String(item.name) : "";
      return makeParsedValueRow({
        sectionName,
        indexOneBased: index + 1,
        valueText: name,
        baseEntry,
        decl: baseEntry && baseEntry.decl ? baseEntry.decl : null,
        valueDecl: null,
        originDecls: item && Array.isArray(item.originDecls) ? item.originDecls : [],
        valueRef: ""
      });
    });
  }

  function buildParsedRowsForRawEntry(obj, baseEntry) {
    if (!obj || !baseEntry) {
      return [];
    }

    const extras = obj.extras && typeof obj.extras === "object" ? obj.extras : null;
    if (!extras) {
      return [];
    }

    const rawName = baseEntry.name ? String(baseEntry.name).trim().toLowerCase() : "";
    if (!rawName) {
      return [];
    }

    if (extras.performCall) {
      const sectionMap = {
        usingraw: "using",
        changingraw: "changing",
        tablesraw: "tables"
      };
      const sectionName = sectionMap[rawName];
      if (sectionName) {
        return buildParsedRowsFromPerformList(sectionName, extras.performCall[sectionName], baseEntry);
      }
    }

    if (extras.message && rawName === "withraw") {
      return buildParsedRowsFromPerformList("with", extras.message.with, baseEntry);
    }

    if (extras.write && rawName === "formatraw") {
      return buildParsedRowsFromWriteFormats(extras.write.format, baseEntry);
    }

    if (extras.callFunction) {
      const sectionMap = {
        exportingraw: "exporting",
        importingraw: "importing",
        changingraw: "changing",
        tablesraw: "tables",
        exceptionsraw: "exceptions"
      };
      const sectionName = sectionMap[rawName];
      if (sectionName) {
        return buildParsedRowsFromAssignments(sectionName, extras.callFunction[sectionName], baseEntry);
      }
    }

    if (extras.callMethod) {
      const sectionMap = {
        exportingraw: "exporting",
        importingraw: "importing",
        changingraw: "changing",
        receivingraw: "receiving",
        exceptionsraw: "exceptions"
      };
      const sectionName = sectionMap[rawName];
      if (sectionName) {
        return buildParsedRowsFromAssignments(sectionName, extras.callMethod[sectionName], baseEntry);
      }
    }

    if (extras.form) {
      const paramsByRawName = {
        usingraw: "USING",
        changingraw: "CHANGING",
        tablesraw: "TABLES"
      };
      if (Object.prototype.hasOwnProperty.call(paramsByRawName, rawName)) {
        const sectionUpper = paramsByRawName[rawName];
        const params = Array.isArray(extras.form.params)
          ? extras.form.params.filter((param) => String(param && param.section ? param.section : "").toUpperCase() === sectionUpper)
          : [];
        return buildParsedRowsFromParams(sectionUpper.toLowerCase(), params, baseEntry);
      }
      if (rawName === "raisingraw") {
        return buildParsedRowsFromExceptions("raising", extras.form.exceptions, baseEntry);
      }
    }

    if (extras.methodSignature) {
      const paramsByRawName = {
        importingraw: "IMPORTING",
        exportingraw: "EXPORTING",
        changingraw: "CHANGING",
        returningraw: "RETURNING"
      };
      if (Object.prototype.hasOwnProperty.call(paramsByRawName, rawName)) {
        const sectionUpper = paramsByRawName[rawName];
        const params = Array.isArray(extras.methodSignature.params)
          ? extras.methodSignature.params.filter((param) => String(param && param.section ? param.section : "").toUpperCase() === sectionUpper)
          : [];
        return buildParsedRowsFromParams(sectionUpper.toLowerCase(), params, baseEntry);
      }
      if (rawName === "raisingraw") {
        return buildParsedRowsFromExceptions("raising", extras.methodSignature.exceptions, baseEntry);
      }
    }

    return [];
  }

  function cloneValueEntryForOutput(entry) {
    if (!entry || typeof entry !== "object") {
      return entry;
    }
    const out = { ...entry };
    if (Array.isArray(entry.originDecls)) {
      out.originDecls = entry.originDecls.slice();
    }
    return out;
  }

  function isOutputIdentifierDataOperand(value) {
    const text = String(value === undefined || value === null ? "" : value).trim();
    if (!text || /^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?$/.test(text)) {
      return false;
    }
    if (
      /^'(?:''|[^'])*'$/.test(text)
      || /^[A-Za-z_][A-Za-z0-9_]*'(?:''|[^'])*'$/.test(text)
      || /^`(?:``|[^`])*`$/.test(text)
      || /^\|[\s\S]*\|$/.test(text)
    ) {
      return false;
    }
    return /^(?:@)?(?:<[^>]+>|[A-Za-z_][A-Za-z0-9_]*)(?:(?:->|=>|~|-)[A-Za-z_][A-Za-z0-9_]*)*$/.test(text);
  }

  function shouldCreateSyntheticOutputValueDecl(obj, entry) {
    const objectType = String(obj && obj.objectType || "").toUpperCase();
    if (!objectType || !["MESSAGE", "WRITE"].includes(objectType)) {
      return true;
    }
    const entryName = String(entry && entry.name || "").trim().toLowerCase();
    if (objectType === "MESSAGE") {
      const message = obj && obj.extras && obj.extras.message;
      if (entryName === "raising" || entryName === "withraw") {
        return false;
      }
      if (message && ["shorthand", "reference"].includes(String(message.mode || "")) && ["message", "id", "messagetype", "number"].includes(entryName)) {
        return false;
      }
      return ["message", "id", "messagetype", "number", "displaylike", "into"].includes(entryName)
        && isOutputIdentifierDataOperand(entry && entry.value);
    }
    return ["output", "destination"].includes(entryName)
      && isOutputIdentifierDataOperand(entry && entry.value);
  }

  function ensureEntryDeclForOutput(obj, entry) {
    if (!entry || typeof entry !== "object") {
      return entry;
    }
    if (!shouldCreateSyntheticOutputValueDecl(obj, entry)) {
      return entry;
    }
    const source = getDeclSourceContextFromObject(obj);
    const entryName = String(entry.name || "value").trim() || "value";
    const normalized = ensureEntryDeclWithSynthetic(entry, {
      pathKey: buildPathKeyFromParts([buildObjectPathBase(obj), "values", entryName]),
      file: source.file,
      lineStart: source.lineStart,
      raw: source.raw,
      role: "value"
    });
    return attachOutputSyntheticDeclAliases(normalized, obj);
  }

  function buildRenderableValueEntries(obj) {
    const values = getValueEntries(obj);
    if (!values.length) {
      return [];
    }

    const rows = [];
    for (const entry of values) {
      const rawRow = cloneValueEntryForOutput(entry);
      if (!isRawValueEntry(entry)) {
        rows.push(ensureEntryDeclForOutput(obj, rawRow));
        continue;
      }

      const parsedRows = buildParsedRowsForRawEntry(obj, entry);
      if (parsedRows.length) {
        for (const parsedRow of parsedRows) {
          rows.push(ensureEntryDeclForOutput(obj, cloneValueEntryForOutput(parsedRow)));
        }
      } else {
        rows.push(ensureEntryDeclForOutput(obj, rawRow));
      }
    }

    return rows;
  }

  function renderValues(obj) {
    const values = buildRenderableValueEntries(obj);
    if (!values.length) {
      return null;
    }

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const name of ["name", "value", "label", "codeDesc", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: name }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (const entry of values) {
      const row = el("tr");
      const relatedDecls = collectDeclsForValueEntry(obj, entry);
      const declListCells = renderDeclListCells(
        relatedDecls.length ? relatedDecls : null,
        relatedDecls.length ? null : entry && entry.decl
      );
      row.appendChild(el("td", { text: entry && entry.name ? String(entry.name) : "" }));
      row.appendChild(el("td", { text: entry && entry.value ? String(entry.value) : "" }));
      row.appendChild(el("td", { text: entry && entry.label ? String(entry.label) : "" }));
      row.appendChild(el("td", { text: entry && entry.codeDesc ? String(entry.codeDesc) : "" }));
      row.appendChild(declListCells.nameCell);
      row.appendChild(declListCells.descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    return table;
  }

  function buildExtrasEntryPathKey(obj, extrasScope, sectionName, indexOneBased, itemKind) {
    return buildPathKeyFromParts([
      buildObjectPathBase(obj),
      "extras",
      extrasScope || "extras",
      sectionName || "section",
      `${itemKind || "item"}[${indexOneBased}]`
    ]);
  }

  function normalizeExtrasEntryForOutput(obj, extrasScope, sectionName, indexOneBased, entry) {
    if (!entry || typeof entry !== "object") {
      return entry;
    }

    const source = getDeclSourceContextFromObject(obj);
    const basePath = buildExtrasEntryPathKey(obj, extrasScope, sectionName, indexOneBased, "item");
    let next = cloneValueEntryForOutput(entry);
    const objectType = String(obj && obj.objectType || "").toUpperCase();
    if (["MESSAGE", "WRITE"].includes(objectType) && !isOutputIdentifierDataOperand(next.value)) {
      return attachOutputSyntheticDeclAliases(next, obj);
    }
    next = ensureEntryDeclWithSynthetic(next, {
      pathKey: basePath,
      file: source.file,
      lineStart: source.lineStart,
      raw: source.raw,
      role: `${extrasScope || "extras"}:${sectionName || "entry"}`
    });
    next = ensureValueDeclWithSynthetic(next, {
      pathKey: basePath,
      file: source.file,
      lineStart: source.lineStart,
      raw: source.raw,
      role: `${extrasScope || "extras"}:${sectionName || "entry"}:value`,
      nameHint: sectionName || "value"
    });
    return attachOutputSyntheticDeclAliases(next, obj);
  }

  function renderAssignmentTable(title, list, context) {
    const items = Array.isArray(list) ? list : [];
    if (!items.length) {
      return null;
    }

    const obj = context && context.obj ? context.obj : null;
    const extrasScope = context && context.extrasScope ? String(context.extrasScope) : "extras";

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of ["name", "value", "ref", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (let index = 0; index < items.length; index += 1) {
      const entry = items[index];
      const normalizedEntry = normalizeExtrasEntryForOutput(obj, extrasScope, title, index + 1, entry);
      const row = el("tr");
      row.appendChild(el("td", { text: normalizedEntry && normalizedEntry.name ? String(normalizedEntry.name) : "" }));
      row.appendChild(el("td", { text: normalizedEntry && normalizedEntry.value ? String(normalizedEntry.value) : "" }));
      row.appendChild(el("td", { text: normalizedEntry && normalizedEntry.valueRef ? String(normalizedEntry.valueRef) : "" }));
      const { nameCell, descCell } = renderDeclListCells(
        normalizedEntry && normalizedEntry.originDecls,
        normalizedEntry && normalizedEntry.valueDecl
      );
      row.appendChild(nameCell);
      row.appendChild(descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function renderValueListTable(title, list, context) {
    const items = Array.isArray(list) ? list : [];
    if (!items.length) {
      return null;
    }

    const obj = context && context.obj ? context.obj : null;
    const extrasScope = context && context.extrasScope ? String(context.extrasScope) : "extras";

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of ["value", "ref", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (let index = 0; index < items.length; index += 1) {
      const entry = items[index];
      const normalizedEntry = normalizeExtrasEntryForOutput(obj, extrasScope, title, index + 1, entry);
      const row = el("tr");
      row.appendChild(el("td", { text: normalizedEntry && normalizedEntry.value ? String(normalizedEntry.value) : "" }));
      row.appendChild(el("td", { text: normalizedEntry && normalizedEntry.valueRef ? String(normalizedEntry.valueRef) : "" }));
      const { nameCell, descCell } = renderDeclListCells(
        normalizedEntry && normalizedEntry.originDecls,
        normalizedEntry && normalizedEntry.valueDecl
      );
      row.appendChild(nameCell);
      row.appendChild(descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function resolveConditionDeclForViewer(decl, refCandidate) {
    if (decl && typeof decl === "object") {
      return decl;
    }

    const ref = String(refCandidate || "").trim();
    if (!ref) {
      return null;
    }


    const decls = state.data && typeof state.data === "object" && Array.isArray(state.data.decls)
      ? state.data.decls
      : [];
    if (!decls.length) {
      return null;
    }

    const upperRef = ref.toUpperCase();
    let matched = null;
    for (const candidate of decls) {
      if (!candidate || typeof candidate !== "object") {
        continue;
      }
      const nameUpper = String(candidate.name || "").trim().toUpperCase();
      if (!nameUpper || nameUpper !== upperRef) {
        continue;
      }

      if (!matched) {
        matched = candidate;
      }

      const scopeUpper = String(candidate.scopeLabel || "").trim().toUpperCase();
      if (scopeUpper === "GLOBAL") {
        return candidate;
      }
    }

    return matched;
  }

  function renderConditionTable(title, clauses, context) {
    const items = Array.isArray(clauses) ? clauses : [];
    if (!items.length) {
      return null;
    }

    const obj = context && context.obj ? context.obj : null;
    const extrasScope = context && context.extrasScope ? String(context.extrasScope) : "extras";
    const source = getDeclSourceContextFromObject(obj);

    const section = el("div");
    if (title) {
      section.appendChild(el("div", { className: "muted", text: title }));
    }

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of [
      "leftOperand",
      "operator",
      "rightOperand",
      "connector",
      "leftRef",
      "leftDeclName",
      "leftDeclDesc",
      "rightRef",
      "rightDeclName",
      "rightDeclDesc"
    ]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (let index = 0; index < items.length; index += 1) {
      const clause = items[index];
      const clausePath = buildExtrasEntryPathKey(obj, extrasScope, title || "conditions", index + 1, "clause");
      let normalizedClause = clause;
      let leftRef = clause && clause.leftOperandRef ? String(clause.leftOperandRef) : "";
      let rightRef = clause && clause.rightOperandRef ? String(clause.rightOperandRef) : "";
      const row = el("tr");
      const leftDecl = resolveConditionDeclForViewer(
        clause && clause.leftOperandDecl ? clause.leftOperandDecl : null,
        leftRef || (clause && clause.leftOperand ? clause.leftOperand : "")
      );
      const rightDecl = resolveConditionDeclForViewer(
        clause && clause.rightOperandDecl ? clause.rightOperandDecl : null,
        rightRef || (clause && clause.rightOperand ? clause.rightOperand : "")
      );
      let effectiveLeftDecl = leftDecl;
      let effectiveRightDecl = rightDecl;

      if (!effectiveLeftDecl || !effectiveRightDecl) {
        normalizedClause = ensureConditionClauseDeclsWithSynthetic(clause, {
          pathKey: clausePath,
          file: source.file,
          lineStart: source.lineStart,
          raw: source.raw
        });
        attachOutputSyntheticDeclAliases(normalizedClause, obj);
        leftRef = normalizedClause && normalizedClause.leftOperandRef ? String(normalizedClause.leftOperandRef) : leftRef;
        rightRef = normalizedClause && normalizedClause.rightOperandRef ? String(normalizedClause.rightOperandRef) : rightRef;
        if (!effectiveLeftDecl) {
          effectiveLeftDecl = normalizedClause && normalizedClause.leftOperandDecl
            ? normalizedClause.leftOperandDecl
            : null;
        }
        if (!effectiveRightDecl) {
          effectiveRightDecl = normalizedClause && normalizedClause.rightOperandDecl
            ? normalizedClause.rightOperandDecl
            : null;
        }
      }

      row.appendChild(el("td", { text: normalizedClause && normalizedClause.leftOperand ? String(normalizedClause.leftOperand) : "" }));
      row.appendChild(el("td", { text: normalizedClause && normalizedClause.comparisonOperator ? String(normalizedClause.comparisonOperator) : "" }));
      row.appendChild(el("td", { text: normalizedClause && normalizedClause.rightOperand ? String(normalizedClause.rightOperand) : "" }));
      row.appendChild(el("td", { text: normalizedClause && normalizedClause.logicalConnector ? String(normalizedClause.logicalConnector) : "" }));
      row.appendChild(el("td", { text: leftRef }));

      const leftTraceDecls = buildDeclTraceListForObject(obj, effectiveLeftDecl);
      const leftDeclCells = renderDeclListCells(leftTraceDecls, effectiveLeftDecl);
      row.appendChild(leftDeclCells.nameCell);
      row.appendChild(leftDeclCells.descCell);

      row.appendChild(el("td", { text: rightRef }));
      const rightTraceDecls = buildDeclTraceListForObject(obj, effectiveRightDecl);
      const rightDeclCells = renderDeclListCells(rightTraceDecls, effectiveRightDecl);
      row.appendChild(rightDeclCells.nameCell);
      row.appendChild(rightDeclCells.descCell);

      tbody.appendChild(row);
    }

    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function appendConditionSection(wrap, title, rawText, clauses, context) {
    if (!wrap) {
      return;
    }

    const raw = String(rawText || "").trim();
    if (raw) {
      wrap.appendChild(el("div", { className: "muted", text: raw }));
    }

    const table = renderConditionTable(title, clauses, context);
    if (table) {
      wrap.appendChild(table);
    }
  }

  function renderParamsTable(title, params) {
    const items = Array.isArray(params) ? params : [];
    if (!items.length) {
      return null;
    }

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const table = el("table");
    const thead = el("thead");
    const headRow = el("tr");
    for (const key of ["section", "name", "typing", "doc", "declName", "declDesc"]) {
      headRow.appendChild(el("th", { text: key }));
    }
    thead.appendChild(headRow);
    table.appendChild(thead);

    const tbody = el("tbody");
    for (const param of items) {
      const row = el("tr");
      row.appendChild(el("td", { text: param && param.section ? String(param.section) : "" }));
      row.appendChild(el("td", { text: param && param.name ? String(param.name) : "" }));
      const typingText = formatTyping(param && param.typing);
      row.appendChild(el("td", { text: typingText }));
      const docText = param && param.doc
        ? [param.doc.direction || "", param.doc.text || ""].filter(Boolean).join(" ")
        : "";
      row.appendChild(el("td", { text: docText }));
      const { nameCell, descCell } = renderDeclListCells(param && param.originDecls);
      row.appendChild(nameCell);
      row.appendChild(descCell);
      tbody.appendChild(row);
    }
    table.appendChild(tbody);
    section.appendChild(table);
    return section;
  }

  function formatTyping(typing) {
    if (!typing) {
      return "";
    }
    if (typeof typing === "string") {
      return typing;
    }
    if (Array.isArray(typing)) {
      return typing.map((item) => formatTyping(item)).filter(Boolean).join(" ");
    }
    if (typeof typing === "object") {
      const kind = typing.kind ? String(typing.kind) : "";
      let value = "";

      if (typeof typing.value === "string") {
        value = typing.value;
      } else if (typing.value && typeof typing.value === "object") {
        if (typeof typing.value.value === "string") {
          value = typing.value.value;
        } else if (typeof typing.value.raw === "string") {
          value = typing.value.raw;
        } else if (typeof typing.value.name === "string") {
          value = typing.value.name;
        } else {
          value = safeJson(typing.value, false);
        }
      } else if (typing.value !== undefined && typing.value !== null) {
        value = String(typing.value);
      }

      return [kind, value].filter(Boolean).join(" ").trim();
    }
    return String(typing);
  }

  function renderNameList(title, items) {
    const list = Array.isArray(items) ? items : [];
    if (!list.length) {
      return null;
    }

    const section = el("div");
    section.appendChild(el("div", { className: "muted", text: title }));

    const wrap = el("div");
    for (const entry of list) {
      wrap.appendChild(el("span", { className: "pill", text: entry && entry.name ? String(entry.name) : "" }));
    }
    section.appendChild(wrap);
    return section;
  }

  function renderExtras(obj) {
    const extras = obj.extras;
    if (!extras || typeof extras !== "object") {
      return null;
    }

    const wrap = el("div", { className: "extras" });
    wrap.appendChild(el("div", { className: "muted", text: "Extras" }));

    if (extras.message) {
      const message = extras.message;
      wrap.appendChild(el("div", {
        className: "muted",
        text: message.mode ? `MESSAGE (${message.mode})` : "MESSAGE"
      }));
      const withTable = renderValueListTable("with", message.with, { obj, extrasScope: "message" });
      if (withTable) {
        wrap.appendChild(withTable);
      }
      return wrap;
    }

    if (extras.write) {
      const write = extras.write;
      wrap.appendChild(el("div", { className: "muted", text: write.newLine ? "WRITE /" : "WRITE" }));
      const formatRows = (Array.isArray(write.format) ? write.format : []).map((entry) => ({
        name: entry && entry.keyword ? String(entry.keyword) : "",
        value: entry && entry.value ? String(entry.value) : "",
        valueRef: entry && entry.valueRef ? String(entry.valueRef) : "",
        valueDecl: entry && entry.valueDecl ? entry.valueDecl : null
      }));
      const formatTable = renderAssignmentTable("format", formatRows, { obj, extrasScope: "write" });
      if (formatTable) {
        wrap.appendChild(formatTable);
      }
      return wrap;
    }

    if (extras.form) {
      const form = extras.form;
      const titleParts = [form.name ? `FORM ${form.name}` : ""].filter(Boolean);
      if (form.nameFromComment && form.nameFromComment !== form.name) {
        titleParts.push(`(comment: ${form.nameFromComment})`);
      }
      if (titleParts.length) {
        wrap.appendChild(el("div", { className: "muted", text: titleParts.join(" ") }));
      }

      const paramsTable = renderParamsTable("params", form.params);
      if (paramsTable) {
        wrap.appendChild(paramsTable);
      }

      const exceptions = renderNameList("exceptions", form.exceptions);
      if (exceptions) {
        wrap.appendChild(exceptions);
      }

      return wrap;
    }

    if (extras.methodSignature) {
      const signature = extras.methodSignature;
      if (signature.name) {
        wrap.appendChild(el("div", { className: "muted", text: `METHOD ${signature.name}` }));
      }

      const paramsTable = renderParamsTable("params", signature.params);
      if (paramsTable) {
        wrap.appendChild(paramsTable);
      }

      const exceptions = renderNameList("exceptions", signature.exceptions);
      if (exceptions) {
        wrap.appendChild(exceptions);
      }

      return wrap;
    }

    if (extras.callFunction) {
      const call = extras.callFunction;
      wrap.appendChild(el("div", { className: "muted", text: call.name ? `CALL FUNCTION ${call.name}` : "CALL FUNCTION" }));
      if (call.destination) {
        wrap.appendChild(el("div", { className: "muted", text: `DESTINATION ${call.destination}` }));
      }

      for (const sectionName of ["exporting", "importing", "changing", "tables", "exceptions"]) {
        const table = renderAssignmentTable(sectionName, call[sectionName], { obj, extrasScope: "callFunction" });
        if (table) {
          wrap.appendChild(table);
        }
      }

      return wrap;
    }

    if (extras.callMethod) {
      const call = extras.callMethod;
      wrap.appendChild(el("div", { className: "muted", text: call.target ? `CALL METHOD ${call.target}` : "CALL METHOD" }));

      for (const sectionName of ["exporting", "importing", "changing", "receiving", "exceptions"]) {
        const table = renderAssignmentTable(sectionName, call[sectionName], { obj, extrasScope: "callMethod" });
        if (table) {
          wrap.appendChild(table);
        }
      }

      return wrap;
    }

    if (extras.performCall) {
      const call = extras.performCall;
      wrap.appendChild(el("div", { className: "muted", text: call.form ? `PERFORM ${call.form}` : "PERFORM" }));
      if (call.program) {
        wrap.appendChild(el("div", { className: "muted", text: `IN PROGRAM ${call.program}` }));
      }
      appendConditionSection(
        wrap,
        "ifConditions",
        call.ifCondition ? `IF ${call.ifCondition}` : "",
        call.ifConditions,
        { obj, extrasScope: "performCall" }
      );

      for (const sectionName of ["using", "changing", "tables"]) {
        const table = renderValueListTable(sectionName, call[sectionName], { obj, extrasScope: "performCall" });
        if (table) {

          wrap.appendChild(table);
        }
      }

      return wrap;
    }

    if (extras.ifCondition) {
      const info = extras.ifCondition;
      appendConditionSection(
        wrap,
        "conditions",
        info.conditionRaw ? `IF ${info.conditionRaw}` : "",
        info.conditions,
        { obj, extrasScope: "ifCondition" }
      );
      return wrap;
    }

    if (extras.select) {
      const info = extras.select;
      appendConditionSection(
        wrap,
        "whereConditions",
        info.whereRaw ? `WHERE ${info.whereRaw}` : "",
        info.whereConditions,
        { obj, extrasScope: "select.where" }
      );
      appendConditionSection(
        wrap,
        "havingConditions",
        info.havingRaw ? `HAVING ${info.havingRaw}` : "",
        info.havingConditions,
        { obj, extrasScope: "select.having" }
      );
      return wrap;
    }

    if (extras.readTable) {
      const info = extras.readTable;
      const raw = info.withTableKeyRaw
        ? `WITH TABLE KEY ${info.withTableKeyRaw}`
        : (info.withKeyRaw ? `WITH KEY ${info.withKeyRaw}` : "");
      appendConditionSection(wrap, "conditions", raw, info.conditions, { obj, extrasScope: "readTable" });
      return wrap;
    }

    if (extras.loopAtItab) {
      const info = extras.loopAtItab;
      appendConditionSection(
        wrap,
        "conditions",
        info.whereRaw ? `WHERE ${info.whereRaw}` : "",
        info.conditions,
        { obj, extrasScope: "loopAtItab" }
      );
      return wrap;
    }

    if (extras.modifyItab) {
      const info = extras.modifyItab;
      appendConditionSection(
        wrap,
        "conditions",
        info.whereRaw ? `WHERE ${info.whereRaw}` : "",
        info.conditions,
        { obj, extrasScope: "modifyItab" }
      );
      return wrap;
    }

    if (extras.deleteItab) {
      const info = extras.deleteItab;
      appendConditionSection(
        wrap,
        "conditions",
        info.whereRaw ? `WHERE ${info.whereRaw}` : "",
        info.conditions,
        { obj, extrasScope: "deleteItab" }
      );
      return wrap;
    }

    wrap.appendChild(el("pre", { text: safeJson(extras, true) }));
    return wrap;
  }

  function renderMeta(obj) {
    const parts = [];
    if (obj.file) {
      parts.push(String(obj.file));
    }
    if (obj.lineStart) {
      parts.push(`line ${obj.lineStart}`);
    }
    if (obj.block && obj.block.lineEnd) {
      parts.push(`end ${obj.block.lineEnd}`);
    }
    if (obj.id !== undefined && obj.id !== null) {
      parts.push(`#${obj.id}`);
    }
    return parts.join(" • ");
  }

  function getObjectLabel(obj) {
    for (const key of ["name", "target", "form"]) {
      const value = getFirstValueFromValues(obj.values, key);
      if (value) {
        return value;
      }
    }

    if (obj.extras && obj.extras.callFunction && obj.extras.callFunction.name) {
      return String(obj.extras.callFunction.name);
    }
    if (obj.extras && obj.extras.callMethod && obj.extras.callMethod.target) {
      return String(obj.extras.callMethod.target);
    }
    if (obj.extras && obj.extras.performCall && obj.extras.performCall.form) {
      return String(obj.extras.performCall.form);
    }
    if (obj.extras && obj.extras.form && obj.extras.form.name) {
      return String(obj.extras.form.name);
    }
    if (obj.extras && obj.extras.methodSignature && obj.extras.methodSignature.name) {
      return String(obj.extras.methodSignature.name);
    }

    return "";
  }

  function renderNode(nodeInfo, options) {
    const opts = options && typeof options === "object" ? options : {};
    const includeChildren = opts.includeChildren !== false;
    const depth = Math.max(0, Number(opts.depth) || 0);
    const obj = nodeInfo.obj;
    const id = normalizeId(obj.id);
    const children = nodeInfo.children || [];

    const cardAttrs = {};
    if (id) {
      cardAttrs["data-id"] = id;
    }
    if (obj && obj.lineStart) {
      cardAttrs["data-line-start"] = String(obj.lineStart);
    }

    const card = el("div", {
      className: `card${obj.block ? " block" : ""}${id && id === state.selectedId ? " selected" : ""}`,
      attrs: Object.keys(cardAttrs).length ? cardAttrs : {}
    });
    card.setAttribute("data-depth", String(depth));
    if (depth > 0) {
      card.style.marginLeft = `${Math.min(144, depth * 14)}px`;
    } else {
      card.style.marginLeft = "";
    }

    const header = el("div", { className: "card-header" });
    const label = getObjectLabel(obj);
    const titleText = `${String(obj.objectType || "(unknown)")}${label ? ` ${label}` : ""}`;
    const title = el("h3", { className: "card-title", text: titleText });

    const meta = renderMeta(obj);
    if (meta) {
      title.appendChild(el("span", { className: "muted", text: `  (${meta})` }));
    }

    header.appendChild(title);

    const actions = el("div", { className: "card-actions" });
    const performSourceControl = typeof createPerformSourceControl === "function"
      ? createPerformSourceControl(obj)
      : null;
    if (performSourceControl) {
      actions.appendChild(performSourceControl);
    }

    const btnCode = el("button", { className: "btn-ghost", text: "Code" });
    btnCode.type = "button";
    btnCode.addEventListener("click", (ev) => {
      ev.stopPropagation();
      setSelectedCard(id);
      const lineEnd = obj.block && obj.block.lineEnd ? obj.block.lineEnd : obj.lineStart;
      selectCodeLines(obj.lineStart, lineEnd);
    });
    actions.appendChild(btnCode);

    const btnJson = el("button", { className: "btn-ghost", text: "JSON" });
    btnJson.type = "button";
    btnJson.addEventListener("click", (ev) => {
      ev.stopPropagation();
      setSelectedCard(id);
      openJsonModal(obj);
    });
    actions.appendChild(btnJson);

    if (children.length && id) {
      const isCollapsed = state.collapsedIds.has(id);
      const btnCollapse = el("button", { className: "btn-ghost", text: isCollapsed ? "Expand" : "Collapse" });
      btnCollapse.type = "button";
      btnCollapse.addEventListener("click", (ev) => {
        ev.stopPropagation();
        if (state.collapsedIds.has(id)) {
          state.collapsedIds.delete(id);
        } else {
          state.collapsedIds.add(id);
        }
        renderOutput();
        setSelectedCard(id);
      });
      actions.appendChild(btnCollapse);
    }

    header.appendChild(actions);
    card.appendChild(header);

    if (obj.comment) {
      card.appendChild(el("div", { className: "muted", text: String(obj.comment) }));
    }

    const kw = renderKeywords(obj);
    if (kw) {
      card.appendChild(kw);
    }

    const valuesTable = renderValues(obj);
    if (valuesTable) {
      card.appendChild(valuesTable);
    }

    if (obj.raw) {
      card.appendChild(el("div", { className: "raw", text: String(obj.raw) }));
    }

    const extras = renderExtras(obj);
    if (extras) {
      card.appendChild(extras);
    }

    if (includeChildren && children.length && !state.collapsedIds.has(id)) {
      const childWrap = el("div", { className: "children" });
      for (const child of children) {
        childWrap.appendChild(renderNode(child, { includeChildren: true, depth: depth + 1 }));
      }
      card.appendChild(childWrap);
    }

    card.addEventListener("click", () => setSelectedCard(id));
    return card;
  }

  function renderOutput() {
    const scrollTop = els.output.scrollTop;

    if (!state.data || !Array.isArray(state.renderObjects)) {
      setOutputMessage("No data loaded.");
      refreshInputGutterTargets();
      return;
    }

    const filteredRoots = (state.renderObjects || [])
      .map((obj) => filterTree(obj))
      .filter(Boolean);

    if (!filteredRoots.length) {
      setOutputMessage("No matches.");
      refreshInputGutterTargets();
      return;
    }

    const frag = document.createDocumentFragment();
    for (const root of filteredRoots) {
      frag.appendChild(renderNode(root));
    }

    els.output.classList.remove("muted");
    els.output.replaceChildren(frag);
    els.output.scrollTop = scrollTop;

    if (state.selectedId) {
      setSelectedCard(state.selectedId, { scroll: false });
    }

    refreshInputGutterTargets();
  }

  function normalizeParsedJson(json) {
    if (Array.isArray(json)) {
      return { file: "", objects: json, decls: [] };
    }
    if (json && typeof json === "object" && Array.isArray(json.objects)) {
      return {
        file: String(json.file || ""),
        objects: json.objects,
        decls: Array.isArray(json.decls) ? json.decls : []
      };
    }
    return null;
  }

  function getOutputVirtualState() {
    if (!state.outputVirtual || typeof state.outputVirtual !== "object") {
      state.outputVirtual = {
        roots: [],
        itemCount: 0,
        start: 0,
        end: 0,
        lastScrollTop: 0,
        scrollDir: "down",
        pendingRaf: 0,
        isAdjustingScroll: false,
        avgItemHeight: 200,
        unknownItemHeight: 200,
        estimateCalibrated: false,
        adjustmentRaf: 0,
        adjustmentGeneration: 0,
        needsScrollSync: false,
        isRenderTransaction: false,
        geometryEpoch: 0,
        itemHeights: new Float64Array(0),
        prefixOffsets: new Float64Array(1),
        sourceRenderObjects: null,
        idToRootIndex: new Map(),
        lineTargetMap: new Map(),
        isInitialized: false
      };
    }
    if (!(state.outputVirtual.idToRootIndex instanceof Map)) {
      state.outputVirtual.idToRootIndex = new Map();
    }
    if (!(state.outputVirtual.lineTargetMap instanceof Map)) {
      state.outputVirtual.lineTargetMap = new Map();
    }
    if (!(state.outputVirtual.itemHeights instanceof Float64Array)) {
      state.outputVirtual.itemHeights = new Float64Array(0);
    }
    if (!(state.outputVirtual.prefixOffsets instanceof Float64Array)) {
      state.outputVirtual.prefixOffsets = new Float64Array(1);
    }
    ensureOutputModuleVirtualControlState(state.outputVirtual, 200);
    return state.outputVirtual;
  }

  function getTemplateVirtualStateForOutput() {
    if (!state.templateVirtual || typeof state.templateVirtual !== "object") {
      state.templateVirtual = {
        items: [],
        itemCount: 0,
        start: 0,
        end: 0,
        lastScrollTop: 0,
        scrollDir: "down",
        pendingRaf: 0,
        isAdjustingScroll: false,
        avgItemHeight: 140,
        unknownItemHeight: 140,
        estimateCalibrated: false,
        adjustmentRaf: 0,
        adjustmentGeneration: 0,
        needsScrollSync: false,
        isRenderTransaction: false,
        geometryEpoch: 0,
        lineTargetMap: new Map(),
        isInitialized: false
      };
    }
    if (!(state.templateVirtual.lineTargetMap instanceof Map)) {
      state.templateVirtual.lineTargetMap = new Map();
    }
    ensureOutputModuleVirtualControlState(state.templateVirtual, 140);
    return state.templateVirtual;
  }

  function ensureOutputModuleVirtualControlState(virtual, defaultEstimate) {
    const fallback = Math.max(1, Number(defaultEstimate) || 1);
    if (!Number.isFinite(Number(virtual.unknownItemHeight)) || Number(virtual.unknownItemHeight) <= 0) {
      virtual.unknownItemHeight = fallback;
    }
    if (typeof virtual.estimateCalibrated !== "boolean") {
      virtual.estimateCalibrated = false;
    }
    if (!Number.isFinite(Number(virtual.adjustmentRaf))) {
      virtual.adjustmentRaf = 0;
    }
    if (!Number.isFinite(Number(virtual.adjustmentGeneration))) {
      virtual.adjustmentGeneration = 0;
    }
    if (typeof virtual.needsScrollSync !== "boolean") {
      virtual.needsScrollSync = false;
    }
    if (typeof virtual.isRenderTransaction !== "boolean") {
      virtual.isRenderTransaction = false;
    }
    if (!Number.isFinite(Number(virtual.geometryEpoch))) {
      virtual.geometryEpoch = 0;
    }
  }

  function getInputGutterVirtualState() {
    if (!state.inputGutterVirtual || typeof state.inputGutterVirtual !== "object") {
      state.inputGutterVirtual = {
        lineCount: 0,
        startLine: 1,
        endLine: 1,
        lineHeightPx: 18,
        topPadPx: 0,
        bottomPadPx: 0,
        pendingRaf: 0,
        lastScrollTop: 0,
        overscanLines: 6,
        isInitialized: false
      };
    }
    return state.inputGutterVirtual;
  }

  function getVirtualWindowConfig(container, avgItemHeight) {
    const safeAvg = Math.max(1, Number(avgItemHeight) || 1);
    const clientHeight = Math.max(0, Number(container && container.clientHeight) || 0);
    const visibleEstimate = Math.max(1, Math.ceil(clientHeight / safeAvg));
    const overscanCount = Math.max(2, Math.ceil(visibleEstimate * 0.5));
    const batchCount = Math.max(4, Math.ceil(visibleEstimate * 0.5));
    const targetCount = visibleEstimate + (overscanCount * 2);
    const maxCount = targetCount + (batchCount * 2);
    const edgeThresholdPx = Math.max(40, Math.round(clientHeight * 0.2));
    return { visibleEstimate, overscanCount, batchCount, targetCount, maxCount, edgeThresholdPx };
  }

  function measureOuterHeight(node) {
    if (!node || typeof node.getBoundingClientRect !== "function") {
      return 0;
    }
    const rect = node.getBoundingClientRect();
    let marginTop = 0;
    let marginBottom = 0;
    try {
      const style = window.getComputedStyle(node);
      marginTop = Number.parseFloat(style && style.marginTop ? style.marginTop : "0") || 0;
      marginBottom = Number.parseFloat(style && style.marginBottom ? style.marginBottom : "0") || 0;
    } catch {
      // ignore
    }
    const scrollScale = getContainerScrollScale(els.output);
    return Math.max(0, (rect.height / scrollScale) + marginTop + marginBottom);
  }

  function updateAverageItemHeight(virtualState, heights, minimum) {
    const minValue = Math.max(1, Number(minimum) || 1);
    const list = Array.isArray(heights) ? heights.filter((v) => Number(v) > 0) : [];
    if (!list.length) {
      return;
    }
    const sampleAvg = list.reduce((sum, value) => sum + Number(value || 0), 0) / list.length;
    const prev = Math.max(minValue, Number(virtualState.avgItemHeight) || minValue);
    const next = Math.max(minValue, ((prev * 0.8) + (sampleAvg * 0.2)));
    virtualState.avgItemHeight = Math.round(next);
  }

  function buildOutputVirtualData(filteredRoots) {
    const roots = Array.isArray(filteredRoots) ? filteredRoots : [];
    const items = [];
    const idToRootIndex = new Map();
    const lineTargetMap = new Map();

    const registerHiddenSubtree = (nodeInfo, anchorIndex) => {
      if (!nodeInfo || typeof nodeInfo !== "object" || !nodeInfo.obj) {
        return;
      }
      const obj = nodeInfo.obj;
      const id = normalizeId(obj.id);
      if (id && !idToRootIndex.has(id)) {
        idToRootIndex.set(id, anchorIndex);
      }
      const line = Number(obj.lineStart) || 0;
      if (line > 0 && id && !lineTargetMap.has(line)) {
        lineTargetMap.set(line, { kind: "output", id });
      }
      const children = Array.isArray(nodeInfo.children) ? nodeInfo.children : [];
      for (const child of children) {
        registerHiddenSubtree(child, anchorIndex);
      }
    };

    const walkVisible = (nodeInfo, depth) => {
      if (!nodeInfo || typeof nodeInfo !== "object" || !nodeInfo.obj) {
        return;
      }
      const obj = nodeInfo.obj;
      const id = normalizeId(obj.id);
      const itemIndex = items.length;
      items.push({ nodeInfo, depth });

      if (id) {
        idToRootIndex.set(id, itemIndex);
      }
      const line = Number(obj.lineStart) || 0;
      if (line > 0 && id && !lineTargetMap.has(line)) {
        lineTargetMap.set(line, { kind: "output", id });
      }

      const children = Array.isArray(nodeInfo.children) ? nodeInfo.children : [];
      if (!children.length) {
        return;
      }
      if (id && state.collapsedIds.has(id)) {
        for (const child of children) {
          registerHiddenSubtree(child, itemIndex);
        }
        return;
      }
      for (const child of children) {
        walkVisible(child, depth + 1);
      }
    };

    for (const rootInfo of roots) {
      walkVisible(rootInfo, 0);
    }

    return { items, idToRootIndex, lineTargetMap };
  }

  function buildOutputRootElement(item, itemIndex) {
    const nodeInfo = item && item.nodeInfo ? item.nodeInfo : null;
    if (!nodeInfo || !nodeInfo.obj) {
      return null;
    }
    const depth = Math.max(0, Number(item && item.depth) || 0);
    const node = renderNode(nodeInfo, { includeChildren: false, depth });
    node.setAttribute("data-virtual-root-index", String(itemIndex));
    node.setAttribute("data-virtual-item-index", String(itemIndex));
    return node;
  }

  function getOutputUnknownItemHeight(virtual) {
    return Math.max(24, Number(virtual && virtual.unknownItemHeight) || 200);
  }

  function calibrateOutputUnknownItemHeight(virtual, heights) {
    if (!virtual || virtual.estimateCalibrated) {
      return;
    }
    const samples = Array.isArray(heights)
      ? heights.map((height) => Number(height) || 0).filter((height) => height > 0).sort((a, b) => a - b)
      : [];
    if (!samples.length) {
      return;
    }
    const middle = Math.floor(samples.length / 2);
    const median = samples.length % 2
      ? samples[middle]
      : ((samples[middle - 1] + samples[middle]) / 2);
    virtual.unknownItemHeight = Math.max(24, median);
    virtual.estimateCalibrated = true;
    virtual.geometryEpoch = (Number(virtual.geometryEpoch) || 0) + 1;
  }

  function ensureOutputHeightCache(virtual, itemCount) {
    const total = Math.max(0, Number(itemCount) || 0);
    if (!(virtual.itemHeights instanceof Float64Array) || virtual.itemHeights.length !== total) {
      virtual.itemHeights = new Float64Array(total);
    }
    if (!(virtual.prefixOffsets instanceof Float64Array) || virtual.prefixOffsets.length !== (total + 1)) {
      virtual.prefixOffsets = new Float64Array(total + 1);
    }
  }

  function rebuildOutputPrefixOffsets(virtual) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    ensureOutputHeightCache(virtual, total);
    const estimate = getOutputUnknownItemHeight(virtual);
    virtual.prefixOffsets[0] = 0;
    for (let index = 0; index < total; index += 1) {
      const measured = Number(virtual.itemHeights[index]) || 0;
      virtual.prefixOffsets[index + 1] = virtual.prefixOffsets[index] + (measured > 0 ? measured : estimate);
    }
  }

  function getOutputOffsetAtIndex(virtual, index) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    const safeIndex = Math.max(0, Math.min(total, Number(index) || 0));
    if (!(virtual.prefixOffsets instanceof Float64Array) || virtual.prefixOffsets.length !== (total + 1)) {
      rebuildOutputPrefixOffsets(virtual);
    }
    return Number(virtual.prefixOffsets[safeIndex]) || 0;
  }

  function findOutputIndexAtOffset(virtual, scrollTop) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    if (!total) {
      return 0;
    }
    if (!(virtual.prefixOffsets instanceof Float64Array) || virtual.prefixOffsets.length !== (total + 1)) {
      rebuildOutputPrefixOffsets(virtual);
    }
    const target = Math.max(0, Number(scrollTop) || 0);
    let low = 0;
    let high = total;
    while (low < high) {
      const mid = Math.floor((low + high) / 2);
      if (virtual.prefixOffsets[mid + 1] <= target) {
        low = mid + 1;
      } else {
        high = mid;
      }
    }
    return Math.max(0, Math.min(total - 1, low));
  }

  function measureRenderedOutputItems(virtual, start, end) {
    if (!els.output) {
      return;
    }
    const heights = [];
    for (const node of Array.from(els.output.querySelectorAll("[data-virtual-item-index]"))) {
      const index = Number(node.getAttribute("data-virtual-item-index"));
      if (!Number.isFinite(index) || index < start || index >= end || index >= virtual.itemHeights.length) {
        continue;
      }
      const height = measureOuterHeight(node);
      if (height > 0) {
        virtual.itemHeights[index] = height;
        heights.push(height);
      }
    }
    calibrateOutputUnknownItemHeight(virtual, heights);
    updateAverageItemHeight(virtual, heights, 80);
    rebuildOutputPrefixOffsets(virtual);
    const topSpacer = els.output.querySelector(".output-virtual-spacer-top");
    const bottomSpacer = els.output.querySelector(".output-virtual-spacer-bottom");
    if (topSpacer) {
      topSpacer.style.height = `${getOutputOffsetAtIndex(virtual, start)}px`;
    }
    if (bottomSpacer) {
      const totalHeight = getOutputOffsetAtIndex(virtual, virtual.itemCount);
      bottomSpacer.style.height = `${Math.max(0, totalHeight - getOutputOffsetAtIndex(virtual, end))}px`;
    }
  }

  function computeOutputVirtualRangeFromScroll(scrollTop) {
    const virtual = getOutputVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!els.output || !total) {
      return { start: 0, end: 0 };
    }

    const estimatedHeight = Math.max(24, Number(virtual.avgItemHeight) || 200);
    const metrics = getVirtualWindowConfig(els.output, estimatedHeight);
    if (total <= metrics.targetCount) {
      return { start: 0, end: total };
    }

    const top = Math.max(0, Number(scrollTop) || 0);
    const firstVisible = findOutputIndexAtOffset(virtual, top);
    let start = Math.max(0, firstVisible - metrics.overscanCount);
    let end = Math.min(total, start + metrics.targetCount);
    if ((end - start) < metrics.targetCount) {
      start = Math.max(0, end - metrics.targetCount);
    }

    return { start, end };
  }

  function captureOutputLogicalScrollAnchor(virtual, scrollTop) {
    const total = Math.max(0, Number(virtual && virtual.itemCount) || 0);
    if (!total) {
      return null;
    }
    const top = Math.max(0, Number(scrollTop) || 0);
    const index = findOutputIndexAtOffset(virtual, top);
    return {
      index,
      intraItemOffset: Math.max(0, top - getOutputOffsetAtIndex(virtual, index))
    };
  }

  function getOutputVirtualMaxScrollTop(virtual) {
    if (!els.output) {
      return 0;
    }
    const prefixMax = Math.max(
      0,
      getOutputOffsetAtIndex(virtual, virtual.itemCount) - (Number(els.output.clientHeight) || 0)
    );
    const domMax = Math.max(0, (Number(els.output.scrollHeight) || 0) - (Number(els.output.clientHeight) || 0));
    if (prefixMax > 0 && domMax > 0) {
      return Math.min(prefixMax, domMax);
    }
    return Math.max(prefixMax, domMax);
  }

  function restoreOutputLogicalScrollAnchor(virtual, anchor, fallbackTop) {
    if (!els.output) {
      return;
    }
    let nextTop = Math.max(0, Number(fallbackTop) || 0);
    if (anchor && Number.isFinite(Number(anchor.index))) {
      const index = Math.max(0, Math.min(virtual.itemCount - 1, Number(anchor.index) || 0));
      const itemStart = getOutputOffsetAtIndex(virtual, index);
      const itemHeight = Math.max(1, getOutputOffsetAtIndex(virtual, index + 1) - itemStart);
      const intraItemOffset = Math.max(
        0,
        Math.min(itemHeight - 1, Number(anchor.intraItemOffset) || 0)
      );
      nextTop = itemStart + intraItemOffset;
    }
    els.output.scrollTop = Math.max(0, Math.min(getOutputVirtualMaxScrollTop(virtual), nextTop));
  }

  function ensureOutputRangeContainsAnchor(virtual, anchor) {
    if (!anchor || !Number.isFinite(Number(anchor.index))) {
      return;
    }
    const index = Math.max(0, Math.min(virtual.itemCount - 1, Number(anchor.index) || 0));
    if (index >= virtual.start && index < virtual.end) {
      return;
    }
    const metrics = getVirtualWindowConfig(els.output, virtual.avgItemHeight);
    let start = Math.max(0, index - metrics.overscanCount);
    let end = Math.min(virtual.itemCount, start + metrics.targetCount);
    if ((end - start) < metrics.targetCount) {
      start = Math.max(0, end - metrics.targetCount);
    }
    virtual.start = start;
    virtual.end = end;
  }

  function renderOutputVirtualInitialRange(options) {
    if (!els.output) {
      return;
    }
    const opts = options && typeof options === "object" ? options : {};
    const virtual = getOutputVirtualState();
    const items = Array.isArray(virtual.roots) ? virtual.roots : [];
    const total = items.length;
    rebuildOutputPrefixOffsets(virtual);
    const requestedTop = Number(opts.scrollTop) || 0;
    const logicalAnchor = opts.preserveScroll && opts.preserveLogicalAnchor !== false
      ? captureOutputLogicalScrollAnchor(virtual, requestedTop)
      : null;
    ensureOutputRangeContainsAnchor(virtual, logicalAnchor);
    const start = Math.max(0, Math.min(total, Number(virtual.start) || 0));
    const end = Math.max(start, Math.min(total, Number(virtual.end) || 0));

    const frag = document.createDocumentFragment();
    const topSpacer = document.createElement("div");
    topSpacer.className = "virtual-spacer output-virtual-spacer-top";
    topSpacer.style.height = `${getOutputOffsetAtIndex(virtual, start)}px`;
    topSpacer.setAttribute("aria-hidden", "true");
    frag.appendChild(topSpacer);

    for (let index = start; index < end; index += 1) {
      const elNode = buildOutputRootElement(items[index], index);
      if (!elNode) {
        continue;
      }
      frag.appendChild(elNode);
    }

    const bottomSpacer = document.createElement("div");
    bottomSpacer.className = "virtual-spacer output-virtual-spacer-bottom";
    bottomSpacer.style.height = `${Math.max(0, getOutputOffsetAtIndex(virtual, total) - getOutputOffsetAtIndex(virtual, end))}px`;
    bottomSpacer.setAttribute("aria-hidden", "true");
    frag.appendChild(bottomSpacer);

    virtual.isRenderTransaction = true;
    try {
      els.output.classList.remove("muted");
      els.output.replaceChildren(frag);
      measureRenderedOutputItems(virtual, start, end);
      restoreOutputLogicalScrollAnchor(
        virtual,
        opts.preserveScroll ? logicalAnchor : null,
        requestedTop
      );
      virtual.lastScrollTop = Number(els.output.scrollTop || 0) || 0;
    } finally {
      virtual.isRenderTransaction = false;
      if (virtual.needsScrollSync) {
        queueVirtualScrollSync(els.output, virtual);
      }
    }
  }

  function initOutputVirtualWindow(filteredRoots, options) {
    const opts = options && typeof options === "object" ? options : {};
    const virtual = getOutputVirtualState();
    const roots = Array.isArray(filteredRoots) ? filteredRoots : [];

    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    cancelVirtualScrollAdjustment(virtual);
    virtual.geometryEpoch = (Number(virtual.geometryEpoch) || 0) + 1;
    virtual.needsScrollSync = false;
    virtual.isRenderTransaction = false;
    virtual.avgItemHeight = 200;
    virtual.unknownItemHeight = 200;
    virtual.estimateCalibrated = false;
    virtual.itemHeights = new Float64Array(0);
    virtual.prefixOffsets = new Float64Array(1);

    const virtualData = buildOutputVirtualData(roots);
    virtual.sourceRenderObjects = state.renderObjects;
    virtual.roots = virtualData.items;
    virtual.itemCount = virtualData.items.length;
    ensureOutputHeightCache(virtual, virtual.itemCount);
    rebuildOutputPrefixOffsets(virtual);
    virtual.idToRootIndex = virtualData.idToRootIndex;
    virtual.lineTargetMap = virtualData.lineTargetMap;
    virtual.start = 0;
    virtual.end = 0;
    virtual.scrollDir = "down";
    virtual.isInitialized = virtual.itemCount > 0;

    if (!virtual.itemCount) {
      return;
    }

    const metrics = getVirtualWindowConfig(els.output, virtual.avgItemHeight);
    const total = virtual.itemCount;
    let start = 0;
    let end = Math.min(total, metrics.targetCount);

    const selectedRootIndex = state.selectedId && virtual.idToRootIndex.has(state.selectedId)
      ? Number(virtual.idToRootIndex.get(state.selectedId))
      : -1;
    const hasSelectedRootIndex = selectedRootIndex >= 0;
    if (hasSelectedRootIndex) {
      start = Math.max(0, selectedRootIndex - Math.floor(metrics.targetCount / 2));
      end = Math.min(total, start + metrics.targetCount);
      if ((end - start) < metrics.targetCount) {
        start = Math.max(0, end - metrics.targetCount);
      }
    } else if (opts.preserveScroll === true) {
      const range = computeOutputVirtualRangeFromScroll(Number(opts.scrollTop) || 0);
      start = range.start;
      end = range.end;
    }

    virtual.start = start;
    virtual.end = end;
    renderOutputVirtualInitialRange({
      preserveScroll: true,
      preserveLogicalAnchor: !hasSelectedRootIndex,
      scrollTop: opts.preserveScroll === true ? (Number(opts.scrollTop) || 0) : 0
    });
  }

  function ensureOutputWindowContainsRootIndex(rootIndex) {
    const index = Number(rootIndex);
    if (!Number.isFinite(index) || index < 0 || !els.output) {
      return false;
    }
    const virtual = getOutputVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!virtual.isInitialized || total <= 0 || index >= total) {
      return false;
    }
    if (index >= virtual.start && index < virtual.end) {
      return true;
    }

    const metrics = getVirtualWindowConfig(els.output, virtual.avgItemHeight);
    let start = Math.max(0, index - Math.floor(metrics.targetCount / 2));
    let end = Math.min(total, start + metrics.targetCount);
    if ((end - start) < metrics.targetCount) {
      start = Math.max(0, end - metrics.targetCount);
    }

    virtual.start = start;
    virtual.end = end;
    const targetTop = Math.max(0, getOutputOffsetAtIndex(virtual, start));
    renderOutputVirtualInitialRange({ preserveScroll: false, scrollTop: targetTop });
    return true;
  }

  function processOutputVirtualScrollFrame() {
    if (!els.output) {
      return;
    }
    const virtual = getOutputVirtualState();
    const total = Number(virtual.itemCount) || 0;
    if (!virtual.isInitialized || !total) {
      return;
    }
    if (virtual.isAdjustingScroll || virtual.isRenderTransaction) {
      virtual.needsScrollSync = true;
      return;
    }

    virtual.needsScrollSync = false;
    const currentTop = Number(els.output.scrollTop || 0) || 0;
    const prevTop = Number(virtual.lastScrollTop || 0) || 0;
    virtual.scrollDir = currentTop >= prevTop ? "down" : "up";
    virtual.lastScrollTop = currentTop;

    const range = computeOutputVirtualRangeFromScroll(currentTop);
    if (range.start === virtual.start && range.end === virtual.end) {
      return;
    }

    virtual.start = range.start;
    virtual.end = range.end;
    renderOutputVirtualInitialRange({ preserveScroll: true, scrollTop: currentTop });
  }

  function scheduleOutputVirtualScroll() {
    const virtual = getOutputVirtualState();
    if (virtual.isAdjustingScroll || virtual.isRenderTransaction) {
      virtual.needsScrollSync = true;
      return;
    }
    if (virtual.pendingRaf) {
      return;
    }
    virtual.pendingRaf = requestAnimationFrame(() => {
      virtual.pendingRaf = 0;
      if (virtual.isAdjustingScroll || virtual.isRenderTransaction) {
        virtual.needsScrollSync = true;
        return;
      }
      processOutputVirtualScrollFrame();
    });
  }

  function handleOutputVirtualScroll() {
    const virtual = getOutputVirtualState();
    virtual.needsScrollSync = true;
    scheduleOutputVirtualScroll();
  }

  function handleOutputVirtualUserIntent() {
    const virtual = getOutputVirtualState();
    cancelVirtualScrollAdjustment(virtual);
    virtual.needsScrollSync = true;
    scheduleOutputVirtualScroll();
  }

  function cancelVirtualScrollAdjustment(virtual) {
    if (!virtual || typeof virtual !== "object") {
      return;
    }
    ensureOutputModuleVirtualControlState(virtual, virtual === state.templateVirtual ? 140 : 200);
    virtual.adjustmentGeneration = (Number(virtual.adjustmentGeneration) || 0) + 1;
    if (virtual.adjustmentRaf) {
      cancelAnimationFrame(virtual.adjustmentRaf);
      virtual.adjustmentRaf = 0;
    }
    virtual.isAdjustingScroll = false;
  }

  function beginVirtualScrollAdjustment(virtual) {
    if (!virtual || typeof virtual !== "object") {
      return 0;
    }
    cancelVirtualScrollAdjustment(virtual);
    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    virtual.needsScrollSync = false;
    virtual.isAdjustingScroll = true;
    return Number(virtual.adjustmentGeneration) || 0;
  }

  function queueVirtualScrollSync(container, virtual) {
    if (!virtual || typeof virtual !== "object") {
      return;
    }
    virtual.needsScrollSync = true;
    if (virtual.isAdjustingScroll || virtual.isRenderTransaction) {
      return;
    }
    if (container === els.output) {
      scheduleOutputVirtualScroll();
      return;
    }
    if (container === els.templatePreviewOutput && typeof scheduleTemplateVirtualScroll === "function") {
      scheduleTemplateVirtualScroll();
    }
  }

  function finishVirtualScrollAdjustment(container, virtual, generation) {
    if (!virtual || Number(virtual.adjustmentGeneration) !== Number(generation)) {
      return;
    }
    virtual.adjustmentRaf = 0;
    virtual.lastScrollTop = Number(container && container.scrollTop) || 0;
    virtual.isAdjustingScroll = false;
    if (virtual.needsScrollSync) {
      queueVirtualScrollSync(container, virtual);
    }
  }

function resetOutputVirtualState() {
    const virtual = getOutputVirtualState();
    if (virtual.pendingRaf) {
      cancelAnimationFrame(virtual.pendingRaf);
      virtual.pendingRaf = 0;
    }
    cancelVirtualScrollAdjustment(virtual);
    virtual.roots = [];
    virtual.itemCount = 0;
    virtual.start = 0;
    virtual.end = 0;
    virtual.lastScrollTop = 0;
    virtual.scrollDir = "down";
    virtual.avgItemHeight = 200;
    virtual.unknownItemHeight = 200;
    virtual.estimateCalibrated = false;
    virtual.needsScrollSync = false;
    virtual.isRenderTransaction = false;
    virtual.geometryEpoch = (Number(virtual.geometryEpoch) || 0) + 1;
    virtual.itemHeights = new Float64Array(0);
    virtual.prefixOffsets = new Float64Array(1);
    virtual.sourceRenderObjects = null;
    virtual.idToRootIndex = new Map();
    virtual.lineTargetMap = new Map();
    virtual.isInitialized = false;
  }

  function alignVirtualTargetAfterRender(container, virtual, selector, initialNode) {
    if (!container || !virtual) {
      return;
    }
    const generation = beginVirtualScrollAdjustment(virtual);

    const align = () => {
      if (Number(virtual.adjustmentGeneration) !== generation || !virtual.isAdjustingScroll) {
        return true;
      }
      const node = selector ? container.querySelector(selector) : initialNode;
      if (!node) {
        return true;
      }
      const offsetError = node.getBoundingClientRect().top - container.getBoundingClientRect().top - 10;
      if (Math.abs(offsetError) <= 1) {
        return true;
      }
      scrollElementInContainer(container, node, { mode: "start", padTop: 10, padBottom: 10 });
      return false;
    };

    const scheduleAdjustmentFrame = (callback) => {
      virtual.adjustmentRaf = requestAnimationFrame(() => {
        virtual.adjustmentRaf = 0;
        if (Number(virtual.adjustmentGeneration) !== generation || !virtual.isAdjustingScroll) {
          return;
        }
        callback();
      });
    };

    align();
    const settle = (remainingFrames) => {
      const isSettled = align();
      if (!isSettled && remainingFrames > 0) {
        scheduleAdjustmentFrame(() => settle(remainingFrames - 1));
        return;
      }
      scheduleAdjustmentFrame(() => finishVirtualScrollAdjustment(container, virtual, generation));
    };
    scheduleAdjustmentFrame(() => settle(4));
  }

  function setSelectedCard(id, options) {
    const normalized = normalizeId(id);
    if (!normalized) {
      return;
    }

    const opts = options && typeof options === "object" ? options : {};
    const shouldScroll = opts.scroll !== false;
    const shouldEnsure = opts.ensure !== false;
    const scrollMode = String(opts.scrollMode || "start").toLowerCase();

    if (state.selectedId) {
      const prev = els.output.querySelector(`[data-id="${escapeSelectorValue(state.selectedId)}"]`);
      if (prev) {
        prev.classList.remove("selected");
      }
    }

    state.selectedId = normalized;
    let next = els.output.querySelector(`[data-id="${escapeSelectorValue(normalized)}"]`);
    if (!next && shouldEnsure) {
      const virtual = getOutputVirtualState();
      const rootIndex = virtual.idToRootIndex.has(normalized) ? Number(virtual.idToRootIndex.get(normalized)) : -1;
      if (rootIndex >= 0) {
        ensureOutputWindowContainsRootIndex(rootIndex);
        next = els.output.querySelector(`[data-id="${escapeSelectorValue(normalized)}"]`);
      }
    }

    if (next) {
      next.classList.add("selected");
      if (shouldScroll) {
        const virtual = getOutputVirtualState();
        if (scrollMode === "start" && virtual.isInitialized) {
          alignVirtualTargetAfterRender(
            els.output,
            virtual,
            `[data-id="${escapeSelectorValue(normalized)}"]`,
            next
          );
        } else {
          scrollElementInContainer(els.output, next, { mode: scrollMode, padTop: 10, padBottom: 10 });
        }
      }
    }
  }

  function setSelectedTemplateBlock(index, options) {
    return updateTemplateBlockSelection(index, {
      ...(options && typeof options === "object" ? options : {}),
      interactionMode: "replace"
    });
  }

  function getInputGutterLineHeightPx() {
    return measureInputLineMetrics().effectivePitch;
  }

  function measureInputLineMetrics() {
    const fallback = {
      lineCount: Math.max(1, Number(state.inputLineCount) || 1),
      nominalPitch: 18,
      effectivePitch: 18,
      paddingTop: 0,
      paddingBottom: 0
    };
    if (!els.inputText) {
      return fallback;
    }

    const lineCount = Math.max(1, Number(state.inputLineCount) || countInputLines(els.inputText.value || ""));
    let nominalPitch = 18;
    let paddingTop = 0;
    let paddingBottom = 0;
    try {
      const style = window.getComputedStyle(els.inputText);
      nominalPitch = Number.parseFloat(style && style.lineHeight ? style.lineHeight : "18") || 18;
      paddingTop = Number.parseFloat(style && style.paddingTop ? style.paddingTop : "0") || 0;
      paddingBottom = Number.parseFloat(style && style.paddingBottom ? style.paddingBottom : "0") || 0;
    } catch {
      // ignore
    }
    nominalPitch = Math.max(12, nominalPitch);

    let effectivePitch = nominalPitch;
    const scrollHeight = Math.max(0, Number(els.inputText.scrollHeight) || 0);
    const clientHeight = Math.max(0, Number(els.inputText.clientHeight) || 0);
    const contentHeight = scrollHeight - paddingTop - paddingBottom;
    const measuredPitch = lineCount > 0 ? (contentHeight / lineCount) : 0;
    const minPitch = nominalPitch * 0.9;
    const maxPitch = nominalPitch * 1.1;
    if (
      lineCount > 1
      && scrollHeight > (clientHeight + 1)
      && Number.isFinite(measuredPitch)
      && measuredPitch >= minPitch
      && measuredPitch <= maxPitch
    ) {
      effectivePitch = measuredPitch;
    }

    return {
      lineCount,
      nominalPitch,
      effectivePitch,
      paddingTop,
      paddingBottom
    };
  }

  function scheduleInputGutterVirtualRender() {
    const virtual = getInputGutterVirtualState();
    if (virtual.pendingRaf) {
      return;
    }
    virtual.pendingRaf = requestAnimationFrame(() => {
      virtual.pendingRaf = 0;
      renderInputGutterWindow({ force: false });
    });
  }

  function renderInputGutterWindow(options) {
    if (!els.inputText || !els.inputGutterContent) {
      return;
    }
    const opts = options && typeof options === "object" ? options : {};
    let force = opts.force === true;
    const virtual = getInputGutterVirtualState();
    const lineCount = Math.max(1, Number(state.inputLineCount) || 1);
    const scrollTop = Number(els.inputText.scrollTop || 0) || 0;
    const lineMetrics = measureInputLineMetrics();
    const lineHeight = Math.max(12, Number(lineMetrics.effectivePitch) || 18);
    if (Math.abs((Number(virtual.lineHeightPx) || 0) - lineHeight) > 0.0001) {
      force = true;
    }
    if (els.inputGutter && els.inputGutter.style) {
      els.inputGutter.style.setProperty("--input-line-pitch", `${lineHeight}px`);
    }
    const visibleLines = Math.max(1, Math.ceil((Number(els.inputText.clientHeight) || 0) / lineHeight));
    const overscanLines = Math.max(6, Math.ceil(visibleLines * 0.75));
    const firstVisibleLine = Math.max(1, Math.floor(scrollTop / lineHeight) + 1);
    const startLine = Math.max(1, firstVisibleLine - overscanLines);
    const endLine = Math.min(lineCount, startLine + visibleLines + (overscanLines * 2) - 1);

    const hasRangeChange = force
      || !virtual.isInitialized
      || startLine !== virtual.startLine
      || endLine !== virtual.endLine
      || lineCount !== virtual.lineCount;

    virtual.lineCount = lineCount;
    virtual.startLine = startLine;
    virtual.endLine = endLine;
    virtual.lineHeightPx = lineHeight;
    virtual.overscanLines = overscanLines;
    virtual.topPadPx = Math.max(0, (startLine - 1) * lineHeight);
    virtual.bottomPadPx = Math.max(0, (lineCount - endLine) * lineHeight);
    virtual.lastScrollTop = scrollTop;
    virtual.isInitialized = true;

    if (!hasRangeChange) {
      els.inputGutterContent.style.transform = `translateY(${-scrollTop}px)`;
      return;
    }

    state.inputGutterButtonsByLine = new Map();
    const frag = document.createDocumentFragment();

    const topSpacer = document.createElement("div");
    topSpacer.className = "gutter-spacer";
    topSpacer.style.height = `${virtual.topPadPx}px`;
    topSpacer.setAttribute("aria-hidden", "true");
    frag.appendChild(topSpacer);

    for (let line = startLine; line <= endLine; line += 1) {
      const row = document.createElement("div");
      row.className = "gutter-line";
      row.style.height = `${lineHeight}px`;

      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "gutter-jump";
      btn.textContent = "↪";
      btn.hidden = true;
      btn.setAttribute("data-line", String(line));
      row.appendChild(btn);

      const num = document.createElement("span");
      num.className = "gutter-num";
      num.textContent = String(line);
      row.appendChild(num);

      state.inputGutterButtonsByLine.set(line, btn);
      frag.appendChild(row);
    }

    const bottomSpacer = document.createElement("div");
    bottomSpacer.className = "gutter-spacer";
    bottomSpacer.style.height = `${virtual.bottomPadPx}px`;
    bottomSpacer.setAttribute("aria-hidden", "true");
    frag.appendChild(bottomSpacer);

    els.inputGutterContent.replaceChildren(frag);
    els.inputGutterContent.style.transform = `translateY(${-scrollTop}px)`;
    refreshInputGutterTargets();
  }

  function syncInputGutterScroll() {
    scheduleInputGutterVirtualRender();
  }

  function rebuildInputGutter() {
    if (!els.inputText || !els.inputGutterContent) {
      return;
    }

    const trimmed = String(els.inputText.value || "").trim();
    const isJsonLike = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
    state.inputMode = isJsonLike ? "json" : "abap";
    state.inputLineCount = Math.max(1, countInputLines(els.inputText.value || ""));

    const virtual = getInputGutterVirtualState();
    virtual.lineHeightPx = measureInputLineMetrics().effectivePitch;
    renderInputGutterWindow({ force: true });
  }

  function computeInputGutterTargetsForOutput() {
    const targets = new Map();
    const virtual = getOutputVirtualState();
    if (virtual.lineTargetMap instanceof Map && virtual.lineTargetMap.size) {
      for (const [line, target] of virtual.lineTargetMap.entries()) {
        targets.set(line, target);
      }
      return targets;
    }

    if (!els.output) {
      return targets;
    }

    const cards = els.output.querySelectorAll(".card[data-id][data-line-start]");
    for (const card of Array.from(cards)) {
      const line = Number(card.getAttribute("data-line-start")) || 0;
      const id = String(card.getAttribute("data-id") || "");
      if (!line || !id || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "output", id });
    }
    return targets;
  }

  function computeInputGutterTargetsForTemplate() {
    const targets = new Map();
    const virtual = getTemplateVirtualStateForOutput();
    if (virtual.lineTargetMap instanceof Map && virtual.lineTargetMap.size) {
      for (const [line, target] of virtual.lineTargetMap.entries()) {
        targets.set(line, target);
      }
      return targets;
    }

    if (!els.templatePreviewOutput) {
      return targets;
    }

    const blocks = els.templatePreviewOutput.querySelectorAll(".template-block[data-template-index][data-line-start]");
    for (const block of Array.from(blocks)) {
      const line = Number(block.getAttribute("data-line-start")) || 0;
      const index = String(block.getAttribute("data-template-index") || "");
      if (!line || !index || targets.has(line)) {
        continue;
      }
      targets.set(line, { kind: "template", index });
    }
    return targets;
  }

  function captureOutputViewportAnchor() {
    if (!els.output) {
      return null;
    }
    const container = els.output;
    const containerRect = container.getBoundingClientRect();
    const renderedItems = Array.from(container.querySelectorAll("[data-virtual-item-index]"));
    const isVisible = (node) => {
      if (!node) {
        return false;
      }
      const rect = node.getBoundingClientRect();
      return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
    };
    const selected = state.selectedId
      ? container.querySelector(`[data-id="${escapeSelectorValue(state.selectedId)}"]`)
      : null;
    const node = isVisible(selected) ? selected : (renderedItems.find(isVisible) || null);
    if (!node) {
      const scrollTop = Number(container.scrollTop) || 0;
      const logical = captureOutputLogicalScrollAnchor(getOutputVirtualState(), scrollTop);
      if (logical) {
        return {
          kind: "logical",
          itemIndex: logical.index,
          intraItemOffset: logical.intraItemOffset,
          scrollTop
        };
      }
      return { kind: "scroll", scrollTop };
    }
    return {
      kind: "item",
      id: String(node.getAttribute("data-id") || ""),
      rootIndex: Number(node.getAttribute("data-virtual-item-index")),
      viewportOffset: node.getBoundingClientRect().top - containerRect.top,
      scrollTop: Number(container.scrollTop) || 0
    };
  }

  function restoreOutputViewportAnchor(anchor) {
    if (!els.output || !anchor || typeof anchor !== "object") {
      return;
    }
    const container = els.output;
    if (anchor.kind === "logical") {
      const virtual = getOutputVirtualState();
      const itemIndex = Number(anchor.itemIndex);
      cancelVirtualScrollAdjustment(virtual);
      if (
        Number.isFinite(itemIndex)
        && itemIndex >= 0
        && ensureOutputWindowContainsRootIndex(itemIndex)
      ) {
        restoreOutputLogicalScrollAnchor(virtual, {
          index: itemIndex,
          intraItemOffset: Number(anchor.intraItemOffset) || 0
        }, anchor.scrollTop);
        virtual.lastScrollTop = Number(container.scrollTop) || 0;
        return;
      }
    }
    if (anchor.kind === "scroll") {
      cancelVirtualScrollAdjustment(getOutputVirtualState());
      container.scrollTop = Math.max(0, Number(anchor.scrollTop) || 0);
      getOutputVirtualState().lastScrollTop = Number(container.scrollTop) || 0;
      return;
    }
    const virtual = getOutputVirtualState();
    const mappedIndex = anchor.id && virtual.idToRootIndex.has(anchor.id)
      ? Number(virtual.idToRootIndex.get(anchor.id))
      : Number(anchor.rootIndex);
    if (!Number.isFinite(mappedIndex) || mappedIndex < 0 || !ensureOutputWindowContainsRootIndex(mappedIndex)) {
      container.scrollTop = Math.max(0, Number(anchor.scrollTop) || 0);
      return;
    }

    const generation = beginVirtualScrollAdjustment(virtual);
    const apply = () => {
      if (Number(virtual.adjustmentGeneration) !== generation || !virtual.isAdjustingScroll) {
        return;
      }
      const node = anchor.id
        ? container.querySelector(`[data-id="${escapeSelectorValue(anchor.id)}"]`)
        : container.querySelector(`[data-virtual-item-index="${mappedIndex}"]`);
      if (!node) {
        return;
      }
      const currentOffset = node.getBoundingClientRect().top - container.getBoundingClientRect().top;
      const maxTop = Math.max(0, Number(container.scrollHeight || 0) - Number(container.clientHeight || 0));
      const nextTop = (Number(container.scrollTop) || 0)
        + ((currentOffset - (Number(anchor.viewportOffset) || 0)) / getContainerScrollScale(container));
      container.scrollTop = Math.max(0, Math.min(maxTop, nextTop));
    };
    apply();
    virtual.adjustmentRaf = requestAnimationFrame(() => {
      virtual.adjustmentRaf = 0;
      if (Number(virtual.adjustmentGeneration) !== generation || !virtual.isAdjustingScroll) {
        return;
      }
      apply();
      finishVirtualScrollAdjustment(container, virtual, generation);
    });
  }

  function renderOutput() {
    const previousScrollTop = els.output ? (Number(els.output.scrollTop || 0) || 0) : 0;
    const viewportAnchor = state.pendingOutputViewportAnchor || captureOutputViewportAnchor();
    state.pendingOutputViewportAnchor = null;

    if (!state.data || !Array.isArray(state.renderObjects)) {
      resetOutputVirtualState();
      setOutputMessage("No data loaded.");
      refreshInputGutterTargets();
      return;
    }

    const filteredRoots = (state.renderObjects || [])
      .map((obj) => filterTree(obj))
      .filter(Boolean);

    if (!filteredRoots.length) {
      resetOutputVirtualState();
      setOutputMessage("No matches.");
      refreshInputGutterTargets();
      return;
    }

    initOutputVirtualWindow(filteredRoots, { preserveScroll: true, scrollTop: previousScrollTop });
    restoreOutputViewportAnchor(viewportAnchor);
    if (state.selectedId) {
      setSelectedCard(state.selectedId, { scroll: false, ensure: false });
    }
    refreshInputGutterTargets();
  }

window.AbapViewerModules.factories = window.AbapViewerModules.factories || {};
window.AbapViewerModules.factories["04-output-render"] = function registerOutputRender(runtime) {
  const targetRuntime = runtime || (window.AbapViewerRuntime = window.AbapViewerRuntime || {});
  targetRuntime.api = targetRuntime.api || {};
  targetRuntime.api.renderOutput = renderOutput;
  targetRuntime.api.setRightTab = setRightTab;
  targetRuntime.api.selectTemplateBlockFromInteraction = selectTemplateBlockFromInteraction;
  targetRuntime.api.getSelectedTemplateIndexes = getSortedSelectedTemplateIndexes;
  targetRuntime.api.clearTemplateBlockSelection = clearTemplateBlockSelection;
  window.AbapViewerModules.parts["04-output-render"] = true;
};
window.AbapViewerModules.factories["04-output-render"](window.AbapViewerRuntime);
