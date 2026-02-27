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

  function normalizeEntryObjectForXml(value, keyHint, pathParts, ownerContext) {
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
        role: "xml:value"
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
          role: "xml:extras:value",
          nameHint: String(keyHint || "value")
        });
      }

      if (isValueLikeEntryObject(next)) {
        next = ensureEntryDeclWithSynthetic(next, {
          pathKey,
          file: source.file,
          lineStart: source.lineStart,
          raw: source.raw,
          role: "xml:extras"
        });
      }
    }

    return next;
  }

  function appendXmlValue(lines, keyHint, tagName, value, indent, pathParts, ownerContext) {
    const pad = " ".repeat(indent);
    const currentPathParts = Array.isArray(pathParts) ? pathParts : [];
    const currentOwnerContext = ownerContext && typeof ownerContext === "object" ? ownerContext : null;

    if (value === undefined) {
      return;
    }

    if (value === null) {
      lines.push(`${pad}<${tagName}/>`);
      return;
    }

    if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
      lines.push(`${pad}<${tagName}>${escapeXmlText(String(value))}</${tagName}>`);
      return;
    }

    if (Array.isArray(value)) {
      const itemTag = getArrayItemTagName(keyHint);
      lines.push(`${pad}<${tagName}>`);
      for (let index = 0; index < value.length; index += 1) {
        const item = value[index];
        const itemPathParts = currentPathParts.concat(`${itemTag}[${index + 1}]`);
        appendXmlValue(lines, itemTag, itemTag, item, indent + 2, itemPathParts, currentOwnerContext);
      }
      lines.push(`${pad}</${tagName}>`);
      return;
    }

    if (typeof value === "object") {
      const nextOwnerContext = isAbapStatementObject(value) ? value : currentOwnerContext;
      const normalizedValue = normalizeEntryObjectForXml(value, keyHint, currentPathParts, nextOwnerContext);
      lines.push(`${pad}<${tagName}>`);

      const valueIsDecl = isDeclObjectForXml(keyHint, normalizedValue);
      let hasComputedFinalDesc = false;

      if (valueIsDecl) {
        const declDesc = getEffectiveDeclDesc(normalizedValue);
        if (declDesc) {
          lines.push(`${" ".repeat(indent + 2)}<desc>${escapeXmlText(declDesc)}</desc>`);
        } else {
          lines.push(`${" ".repeat(indent + 2)}<desc/>`);
        }

        const finalDeclDesc = getFinalDeclDesc(normalizedValue);
        hasComputedFinalDesc = true;
        if (finalDeclDesc) {
          lines.push(`${" ".repeat(indent + 2)}<finalDesc>${escapeXmlText(finalDeclDesc)}</finalDesc>`);
        } else {
          lines.push(`${" ".repeat(indent + 2)}<finalDesc/>`);
        }

        const declName = getDeclDisplayName(normalizedValue) || getDeclTechName(normalizedValue);
        lines.push(`${" ".repeat(indent + 2)}<name>${escapeXmlText(declName)}</name>`);
      }

      if (!valueIsDecl) {
        let shouldEmitFinalDesc = false;
        let finalDesc = "";
        if (hasValueLevelDescFields(normalizedValue)) {
          shouldEmitFinalDesc = true;
          finalDesc = resolveValueLevelFinalDesc(normalizedValue);
        } else if (isDeclLikeObject(normalizedValue.decl)) {
          shouldEmitFinalDesc = true;
          finalDesc = getFinalDeclDesc(normalizedValue.decl);
        }

        if (shouldEmitFinalDesc) {
          hasComputedFinalDesc = true;
          if (finalDesc) {
            lines.push(`${" ".repeat(indent + 2)}<finalDesc>${escapeXmlText(finalDesc)}</finalDesc>`);
          } else {
            lines.push(`${" ".repeat(indent + 2)}<finalDesc/>`);
          }
        }
      }

      const preferredOrder = [
        "id",
        "parent",
        "objectType",
        "file",
        "lineStart",
        "comment",
        "raw",
        "keywords",
        "values",
        "extras",
        "block",
        "children"
      ];

      const keys = Object.keys(normalizedValue);
      keys.sort((a, b) => {
        const ai = preferredOrder.indexOf(a);
        const bi = preferredOrder.indexOf(b);
        if (ai !== -1 || bi !== -1) {
          return (ai === -1 ? 999 : ai) - (bi === -1 ? 999 : bi);
        }
        return a.localeCompare(b);
      });

      for (const key of keys) {
        if (key === "desc" && valueIsDecl) {
          // computed above
          continue;
        }
        if (key === "name" && valueIsDecl) {
          // computed above (normalized via settings)
          continue;
        }
        if (key === "finalDesc" && hasComputedFinalDesc) {
          continue;
        }
        const childTag = toXmlTagName(key);
        appendXmlValue(
          lines,
          key,
          childTag,
          normalizedValue[key],
          indent + 2,
          currentPathParts.concat(key),
          nextOwnerContext
        );
      }

      lines.push(`${pad}</${tagName}>`);
      return;
    }

    lines.push(`${pad}<${tagName}>${escapeXmlText(String(value))}</${tagName}>`);
  }

  function buildAbapFlowXml(data) {
    const fileName = data && typeof data === "object" ? String(data.file || "") : "";
    const objects = data && typeof data === "object" && Array.isArray(data.objects) ? data.objects : [];
    const renderObjects = buildRenderableObjects(objects, RENDER_TREE_OPTIONS);
    const exportRoots = buildXmlExportRoots(renderObjects);

    const lines = ['<?xml version="1.0" encoding="UTF-8"?>', "<abapflowObjects>"];
    if (fileName) {
      lines.push(`  <file>${escapeXmlText(fileName)}</file>`);
    }
    lines.push("  <objects>");
    for (let index = 0; index < exportRoots.length; index += 1) {
      const obj = exportRoots[index];
      appendXmlValue(lines, "object", "object", obj, 4, ["objects", `object[${index + 1}]`], obj);
    }
    lines.push("  </objects>");
    lines.push("</abapflowObjects>");

    return lines.join("\n");
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

  function collectDeclSearchTextFromExtras(extras) {
    if (!extras || typeof extras !== "object") {
      return "";
    }

    const decls = [];
    const addDecl = (decl) => {
      if (decl && typeof decl === "object") {
        decls.push(decl);
      }
    };
    const addDeclList = (list) => {
      if (!Array.isArray(list)) {
        return;
      }
      for (const decl of list) {
        addDecl(decl);
      }
    };

    const collectFromAssignmentSections = (obj, sections) => {
      for (const sectionName of sections) {
        const list = obj && Array.isArray(obj[sectionName]) ? obj[sectionName] : [];
        for (const entry of list) {
          addDeclList(entry && entry.originDecls);
          addDecl(entry && entry.valueDecl);
        }
      }
    };

    if (extras.callFunction) {
      collectFromAssignmentSections(extras.callFunction, ["exporting", "importing", "changing", "tables", "exceptions"]);
    }

    if (extras.callMethod) {
      collectFromAssignmentSections(extras.callMethod, ["exporting", "importing", "changing", "receiving", "exceptions"]);
    }

    if (extras.performCall) {
      for (const sectionName of ["using", "changing", "tables"]) {
        const list = Array.isArray(extras.performCall[sectionName]) ? extras.performCall[sectionName] : [];
        for (const entry of list) {
          addDeclList(entry && entry.originDecls);
          addDecl(entry && entry.valueDecl);
        }
      }
    }

    if (extras.form && Array.isArray(extras.form.params)) {
      for (const param of extras.form.params) {
        addDeclList(param && param.originDecls);
      }
    }

    collectConditionDeclsFromExtras(extras, addDecl);

    const seen = new Set();
    const parts = [];
    for (const decl of decls) {
      const key = getDeclOverrideStorageKey(decl) || stringifyDecl(decl);
      if (!key || seen.has(key)) {
        continue;
      }
      seen.add(key);
      parts.push(String(decl.name || ""));
      const desc = getEffectiveDeclDesc(decl);
      if (desc) {
        parts.push(desc);
      }
    }

    return parts.filter(Boolean).join("\n");
  }

  function buildSearchIndex(objects) {
    const map = new Map();
    walkObjects(objects, (obj) => {
      const id = normalizeId(obj && obj.id);
      if (!id) {
        return;
      }

      const keywordEntries = getKeywordEntries(obj);
      const keywordsText = keywordEntries.length
        ? keywordEntries.map((k) => `${k.text || ""} ${k.label || ""}`.trim()).join("\n")
        : "";

      const valueEntries = getValueEntries(obj);
      const valuesText = valueEntries.length
        ? valueEntries
            .map((v) => {
              const declText = stringifyDecl(v && v.decl);
              const declDesc = getEffectiveDeclDesc(v && v.decl);
              return [
                v && v.name ? String(v.name) : "",
                v && v.value ? String(v.value) : "",
                v && v.label ? String(v.label) : "",
                v && v.codeDesc ? String(v.codeDesc) : "",
                v && v.declRef ? String(v.declRef) : "",
                declText,
                declDesc
              ]
                .filter(Boolean)
                .join(" ");
            })
            .join("\n")
        : "";

      const extrasText = obj.extras && typeof obj.extras === "object" ? safeJson(obj.extras, false) : "";
      const extrasDeclText = collectDeclSearchTextFromExtras(obj.extras);

      const haystack = [
        obj.objectType || "",
        obj.raw || "",
        obj.comment || "",
        keywordsText,
        valuesText,
        extrasText,
        extrasDeclText
      ]
        .filter(Boolean)
        .join("\n")
        .toLowerCase();

      map.set(id, haystack);
    });
    return map;
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
    const text = els.inputText.value || "";
    state.inputLineOffsets = computeLineOffsets(text);
    const range = getSelectionRangeForLines(text, lineStart, lineEnd);
    els.inputText.focus();
    els.inputText.setSelectionRange(range.start, range.end);
    syncInputGutterScroll();
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
    let nextTop = currentTop;

    if (mode === "start") {
      nextTop = currentTop + (elementRect.top - containerRect.top) - padTop;
    } else if (mode === "center") {
      nextTop = currentTop
        + (elementRect.top + (elementRect.height / 2))
        - (containerRect.top + (containerRect.height / 2));
    } else {
      const topLimit = containerRect.top + padTop;
      const bottomLimit = containerRect.bottom - padBottom;
      if (elementRect.top < topLimit) {
        nextTop = currentTop + (elementRect.top - topLimit);
      } else if (elementRect.bottom > bottomLimit) {
        nextTop = currentTop + (elementRect.bottom - bottomLimit);
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

  function setSelectedTemplateBlock(index, options) {
    if (!els.templatePreviewOutput) {
      return;
    }

    const opts = options && typeof options === "object" ? options : {};
    const shouldScroll = opts.scroll !== false;
    const scrollMode = String(opts.scrollMode || "start").toLowerCase();
    const normalized = String(index === undefined || index === null ? "" : index).trim();
    if (!normalized) {
      return;
    }

    if (state.selectedTemplateIndex !== "") {
      const prev = els.templatePreviewOutput.querySelector(
        `.template-block[data-template-index="${escapeSelectorValue(state.selectedTemplateIndex)}"]`
      );
      if (prev) {
        prev.classList.remove("selected");
      }
    }

    state.selectedTemplateIndex = normalized;
    const next = els.templatePreviewOutput.querySelector(
      `.template-block[data-template-index="${escapeSelectorValue(normalized)}"]`
    );
    if (next) {
      next.classList.add("selected");
      if (shouldScroll) {
        scrollElementInContainer(els.templatePreviewOutput, next, { mode: scrollMode, padTop: 10, padBottom: 10 });
      }
    }
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

  function matchesFilters(obj) {
    const typeOk = !state.type || obj.objectType === state.type;
    if (!typeOk) {
      return false;
    }

    if (!state.query) {
      return true;
    }

    const id = normalizeId(obj && obj.id);
    const haystack = id ? state.haystackById.get(id) : "";
    return haystack ? haystack.includes(state.query) : false;
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

    const normalizedEntry = ensureEntryDeclForOutput(obj, cloneValueEntryForOutput(entry));

    addDecl(normalizedEntry && normalizedEntry.decl);
    addDecl(normalizedEntry && normalizedEntry.valueDecl);
    if (normalizedEntry && Array.isArray(normalizedEntry.originDecls)) {
      for (const originDecl of normalizedEntry.originDecls) {
        addDecl(originDecl);
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
      collectConditionDeclsFromClauses(extras.ifCondition.conditions, addDecl);
    } else if (entryName === "ifcondition" && extras.performCall) {
      collectConditionDeclsFromClauses(extras.performCall.ifConditions, addDecl);
    } else if (entryName === "where") {
      if (extras.select) {
        collectConditionDeclsFromClauses(extras.select.whereConditions, addDecl);
      }
      if (extras.loopAtItab) {
        collectConditionDeclsFromClauses(extras.loopAtItab.conditions, addDecl);
      }
      if (extras.modifyItab) {
        collectConditionDeclsFromClauses(extras.modifyItab.conditions, addDecl);
      }
      if (extras.deleteItab) {
        collectConditionDeclsFromClauses(extras.deleteItab.conditions, addDecl);
      }
    } else if (entryName === "having" && extras.select) {
      collectConditionDeclsFromClauses(extras.select.havingConditions, addDecl);
    } else if ((entryName === "withkey" || entryName === "withtablekey") && extras.readTable) {
      collectConditionDeclsFromClauses(extras.readTable.conditions, addDecl);
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
    valueRef
  }) {
    const row = {
      name: makeSectionIndexName(sectionName, indexOneBased),
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

  function ensureEntryDeclForOutput(obj, entry) {
    if (!entry || typeof entry !== "object") {
      return entry;
    }
    const source = getDeclSourceContextFromObject(obj);
    const entryName = String(entry.name || "value").trim() || "value";
    return ensureEntryDeclWithSynthetic(entry, {
      pathKey: buildPathKeyFromParts([buildObjectPathBase(obj), "values", entryName]),
      file: source.file,
      lineStart: source.lineStart,
      raw: source.raw,
      role: "value"
    });
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
    return next;
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

      const leftDeclCells = renderDeclListCells(null, effectiveLeftDecl);
      row.appendChild(leftDeclCells.nameCell);
      row.appendChild(leftDeclCells.descCell);

      row.appendChild(el("td", { text: rightRef }));
      const rightDeclCells = renderDeclListCells(null, effectiveRightDecl);
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

  function renderNode(nodeInfo) {
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

    if (children.length) {
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

    if (state.showKeywords) {
      const kw = renderKeywords(obj);
      if (kw) {
        card.appendChild(kw);
      }
    }

    if (state.showValues) {
      const valuesTable = renderValues(obj);
      if (valuesTable) {
        card.appendChild(valuesTable);
      }
    }

    if (state.showRaw && obj.raw) {
      card.appendChild(el("div", { className: "raw", text: String(obj.raw) }));
    }

    if (state.showExtras) {
      const extras = renderExtras(obj);
      if (extras) {
        card.appendChild(extras);
      }
    }

    if (children.length && !state.collapsedIds.has(id)) {
      const childWrap = el("div", { className: "children" });
      for (const child of children) {
        childWrap.appendChild(renderNode(child));
      }
      card.appendChild(childWrap);
    }

    card.addEventListener("click", () => setSelectedCard(id));
    return card;
  }

  function populateTypeFilter(objects) {
    const types = new Set();
    walkObjects(objects, (obj) => {
      if (obj && obj.objectType) {
        types.add(String(obj.objectType));
      }
    });

    const sorted = Array.from(types).sort((a, b) => a.localeCompare(b));
    const current = els.typeFilter.value || "";

    while (els.typeFilter.options.length > 1) {
      els.typeFilter.remove(1);
    }

    for (const type of sorted) {
      const opt = document.createElement("option");
      opt.value = type;
      opt.textContent = type;
      els.typeFilter.appendChild(opt);
    }

    els.typeFilter.value = sorted.includes(current) ? current : "";
  }

  function renderOutput() {
    const scrollTop = els.output.scrollTop;
    setError("");

    if (!state.data || !Array.isArray(state.renderObjects)) {
      setOutputMessage("No data loaded.");
      refreshInputGutterTargets();
      return;
    }

    state.query = (els.searchInput.value || "").trim().toLowerCase();
    state.type = els.typeFilter.value || "";
    state.showRaw = Boolean(els.showRaw && els.showRaw.checked);
    state.showKeywords = Boolean(els.showKeywords && els.showKeywords.checked);
    state.showValues = Boolean(els.showValues && els.showValues.checked);
    state.showExtras = Boolean(els.showExtras && els.showExtras.checked);

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

window.AbapViewerModules.factories = window.AbapViewerModules.factories || {};
window.AbapViewerModules.factories["04-output-render"] = function registerOutputRender(runtime) {
  const targetRuntime = runtime || (window.AbapViewerRuntime = window.AbapViewerRuntime || {});
  targetRuntime.api = targetRuntime.api || {};
  targetRuntime.api.renderOutput = renderOutput;
  targetRuntime.api.buildAbapFlowXml = buildAbapFlowXml;
  targetRuntime.api.setRightTab = setRightTab;
  window.AbapViewerModules.parts["04-output-render"] = true;
};
window.AbapViewerModules.factories["04-output-render"](window.AbapViewerRuntime);

