"use strict";

(function () {
  const runtime = window.AbapViewerRuntime || {};
  const state = runtime.state;
  const els = runtime.els;

  if (!state || !els) {
    return;
  }

  state.domPerf = state.domPerf || {};
  const perf = state.domPerf;
  const TEMPLATE_SHOW_ALL_TABLES_KEY = "abap-parser-viewer.templateShowAllTables.v1";
  perf.outputChildRenderCountById = perf.outputChildRenderCountById instanceof Map
    ? perf.outputChildRenderCountById
    : new Map();
  perf.outputChildInitialSize = Number(perf.outputChildInitialSize) || 24;
  perf.outputChildBatchSize = Number(perf.outputChildBatchSize) || 24;
  perf.templateExpandedBlocks = perf.templateExpandedBlocks instanceof Set
    ? perf.templateExpandedBlocks
    : new Set();
  perf.templateShowAllTables = Boolean(perf.templateShowAllTables);
  perf.descVirtual = perf.descVirtual && typeof perf.descVirtual === "object"
    ? perf.descVirtual
    : null;
  perf.outputVirtual = perf.outputVirtual && typeof perf.outputVirtual === "object"
    ? perf.outputVirtual
    : null;
  perf.templateVirtual = perf.templateVirtual && typeof perf.templateVirtual === "object"
    ? perf.templateVirtual
    : null;
  const OUTPUT_AVG_HEIGHT_DEFAULT = 220;
  const TEMPLATE_AVG_HEIGHT_DEFAULT = 170;

  function loadTemplateShowAllTablesSetting() {
    try {
      const raw = localStorage.getItem(TEMPLATE_SHOW_ALL_TABLES_KEY);
      if (!raw) {
        return false;
      }
      return raw === "1" || raw === "true";
    } catch {
      return false;
    }
  }

  function saveTemplateShowAllTablesSetting(value) {
    try {
      localStorage.setItem(TEMPLATE_SHOW_ALL_TABLES_KEY, value ? "1" : "0");
    } catch {
      // ignore
    }
  }

  perf.templateShowAllTables = loadTemplateShowAllTablesSetting();

  function clampInt(value, min, max) {
    const num = Number(value);
    if (!Number.isFinite(num)) {
      return min;
    }
    return Math.max(min, Math.min(max, Math.floor(num)));
  }

  function resetDomPerfOnDataReload() {
    perf.outputChildRenderCountById = new Map();
    perf.templateExpandedBlocks = new Set();
    perf.descVirtual = null;
    perf.outputVirtual = null;
    perf.templateVirtual = null;
  }

  function bindTemplateShowAllTablesControl() {
    const input = document.getElementById("templateShowAllTables");
    if (!input) {
      return;
    }
    input.checked = Boolean(perf.templateShowAllTables);

    input.addEventListener("change", () => {
      perf.templateShowAllTables = Boolean(input.checked);
      saveTemplateShowAllTablesSetting(perf.templateShowAllTables);
      if (typeof renderTemplatePreview === "function") {
        renderTemplatePreview();
      }
      if (state.selectedTemplateIndex !== "" && typeof setSelectedTemplateBlock === "function") {
        setSelectedTemplateBlock(state.selectedTemplateIndex, { scroll: false, ensureVisible: true });
      }
    });
  }

  function ensureOutputChildNodeLimit(nodeInfo, id) {
    const children = Array.isArray(nodeInfo && nodeInfo.children) ? nodeInfo.children : [];
    if (!children.length || !id) {
      return { nodeInfo, hasMore: false, renderedCount: children.length, totalCount: children.length };
    }

    let renderedCount = perf.outputChildRenderCountById.get(id);
    if (!Number.isInteger(renderedCount) || renderedCount < 1) {
      renderedCount = Math.min(children.length, perf.outputChildInitialSize);
    }
    renderedCount = clampInt(renderedCount, 1, children.length);
    perf.outputChildRenderCountById.set(id, renderedCount);

    if (renderedCount >= children.length) {
      return { nodeInfo, hasMore: false, renderedCount, totalCount: children.length };
    }

    const limited = { ...nodeInfo, children: children.slice(0, renderedCount) };
    return { nodeInfo: limited, hasMore: true, renderedCount, totalCount: children.length };
  }

  if (typeof parseFromTextarea === "function") {
    const originalParseFromTextarea = parseFromTextarea;
    parseFromTextarea = function parseFromTextareaWithDomPerfReset() {
      resetDomPerfOnDataReload();
      return originalParseFromTextarea.apply(this, arguments);
    };
  }

  if (typeof setRightTab === "function") {
    const originalSetRightTab = setRightTab;
    setRightTab = function setRightTabWithDomUnmount(nextTab) {
      const result = originalSetRightTab.call(this, nextTab);
      const activeTab = state.rightTab;

      if (activeTab !== "output" && els.output) {
        els.output.replaceChildren();
      }
      if (activeTab !== "template" && els.templatePreviewOutput) {
        els.templatePreviewOutput.replaceChildren();
      }
      if (activeTab !== "descriptions" && els.declDescTable) {
        els.declDescTable.replaceChildren();
      }

      if (activeTab === "output" && els.output && !els.output.firstChild && typeof renderOutput === "function") {
        renderOutput();
      }

      return result;
    };
  }

  if (typeof renderNode === "function") {
    const originalRenderNode = renderNode;
    renderNode = function renderNodeWithChildChunk(nodeInfo) {
      if (!nodeInfo || typeof nodeInfo !== "object" || !nodeInfo.obj || typeof nodeInfo.obj !== "object") {
        return originalRenderNode(nodeInfo);
      }

      const nodeId = typeof normalizeId === "function"
        ? normalizeId(nodeInfo.obj.id)
        : String(nodeInfo.obj.id || "");
      const isCollapsed = state.collapsedIds && nodeId ? state.collapsedIds.has(nodeId) : false;

      if (!nodeId || isCollapsed) {
        return originalRenderNode(nodeInfo);
      }

      const limited = ensureOutputChildNodeLimit(nodeInfo, nodeId);
      const card = originalRenderNode(limited.nodeInfo);

      if (!limited.hasMore) {
        return card;
      }

      const childWrap = card && typeof card.querySelector === "function"
        ? card.querySelector(".children")
        : null;
      if (!childWrap) {
        return card;
      }

      const loadMoreWrap = document.createElement("div");
      loadMoreWrap.style.marginTop = "8px";
      loadMoreWrap.style.display = "flex";
      loadMoreWrap.style.justifyContent = "flex-start";

      const loadMoreBtn = document.createElement("button");
      loadMoreBtn.type = "button";
      loadMoreBtn.className = "btn-ghost";
      loadMoreBtn.textContent = `Load more children (${limited.renderedCount}/${limited.totalCount})`;
      loadMoreBtn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        const nextCount = clampInt(
          limited.renderedCount + perf.outputChildBatchSize,
          1,
          limited.totalCount
        );
        perf.outputChildRenderCountById.set(nodeId, nextCount);
        if (typeof renderOutput === "function") {
          renderOutput();
        }
        if (typeof setSelectedCard === "function") {
          setSelectedCard(nodeId, { scroll: false, ensureVisible: true });
        }
      });

      loadMoreWrap.appendChild(loadMoreBtn);
      childWrap.appendChild(loadMoreWrap);
      return card;
    };
  }

  function ensureOutputVirtualState() {
    const existing = perf.outputVirtual;
    if (existing && typeof existing === "object") {
      return existing;
    }
    perf.outputVirtual = {
      roots: [],
      total: 0,
      avgHeight: OUTPUT_AVG_HEIGHT_DEFAULT,
      start: -1,
      end: -1,
      idToRootIndex: new Map(),
      lineTargetMap: new Map(),
      pendingRaf: 0,
      scrollBound: false,
      adjustingScroll: false
    };
    return perf.outputVirtual;
  }

  function ensureTemplateVirtualState() {
    const existing = perf.templateVirtual;
    if (existing && typeof existing === "object") {
      return existing;
    }
    perf.templateVirtual = {
      items: [],
      total: 0,
      avgHeight: TEMPLATE_AVG_HEIGHT_DEFAULT,
      start: -1,
      end: -1,
      lineTargetMap: new Map(),
      pendingRaf: 0,
      scrollBound: false,
      adjustingScroll: false,
      config: null
    };
    return perf.templateVirtual;
  }

  function buildVirtualRange(total, scrollTop, viewportHeight, avgHeight) {
    const height = Math.max(40, Number(avgHeight) || 1);
    const visibleEstimate = Math.max(1, Math.ceil(Math.max(1, viewportHeight) / height));
    const overscan = Math.max(4, Math.ceil(visibleEstimate * 0.75));
    let start = Math.max(0, Math.floor(Math.max(0, scrollTop) / height) - overscan);
    let end = Math.min(total, start + visibleEstimate + (overscan * 2));
    start = Math.max(0, Math.min(start, end));
    return { start, end, visibleEstimate, overscan };
  }

  function createVirtualSpacer(className, heightPx) {
    const spacer = document.createElement("div");
    spacer.className = className;
    spacer.style.height = `${Math.max(0, Math.round(heightPx))}px`;
    spacer.style.pointerEvents = "none";
    return spacer;
  }

  function setTemplatePreviewMessageCompat(message) {
    const text = String(message || "");
    if (typeof setTemplatePreviewMessage === "function") {
      setTemplatePreviewMessage(text);
      return;
    }
    if (!els.templatePreviewOutput) {
      return;
    }
    els.templatePreviewOutput.classList.add("muted");
    els.templatePreviewOutput.textContent = text;
  }

  function collectOutputVirtualMaps(roots) {
    const idToRootIndex = new Map();
    const lineTargetMap = new Map();

    const walk = (nodeInfo, rootIndex) => {
      if (!nodeInfo || typeof nodeInfo !== "object") {
        return;
      }
      const obj = nodeInfo.obj;
      if (!obj || typeof obj !== "object") {
        return;
      }
      const nodeId = typeof normalizeId === "function"
        ? normalizeId(obj.id)
        : String(obj.id || "");
      if (nodeId) {
        idToRootIndex.set(nodeId, rootIndex);
      }
      const lineStart = Number(obj.lineStart) || 0;
      if (lineStart > 0 && nodeId && !lineTargetMap.has(lineStart)) {
        lineTargetMap.set(lineStart, { kind: "output", id: nodeId });
      }
      const children = Array.isArray(nodeInfo.children) ? nodeInfo.children : [];
      for (const child of children) {
        walk(child, rootIndex);
      }
    };

    const list = Array.isArray(roots) ? roots : [];
    for (let i = 0; i < list.length; i += 1) {
      walk(list[i], i);
    }

    return { idToRootIndex, lineTargetMap };
  }

  function collectTemplateVirtualLineMap(items) {
    const lineTargetMap = new Map();
    const list = Array.isArray(items) ? items : [];
    for (let i = 0; i < list.length; i += 1) {
      const item = list[i];
      const obj = item && item.obj && typeof item.obj === "object" ? item.obj : item;
      const lineStart = Number(obj && obj.lineStart) || 0;
      if (lineStart > 0 && !lineTargetMap.has(lineStart)) {
        lineTargetMap.set(lineStart, { kind: "template", index: String(i) });
      }
    }
    return lineTargetMap;
  }

  function scheduleOutputVirtualRender(force) {
    const v = ensureOutputVirtualState();
    if (!els.output) {
      return;
    }
    if (force) {
      if (v.pendingRaf) {
        const cancel = typeof window.cancelAnimationFrame === "function"
          ? window.cancelAnimationFrame.bind(window)
          : clearTimeout;
        cancel(v.pendingRaf);
        v.pendingRaf = 0;
      }
      renderOutputVirtualWindow(true);
      return;
    }

    if (v.pendingRaf) {
      return;
    }
    const schedule = typeof window.requestAnimationFrame === "function"
      ? window.requestAnimationFrame.bind(window)
      : (cb) => setTimeout(cb, 16);
    v.pendingRaf = schedule(() => {
      v.pendingRaf = 0;
      renderOutputVirtualWindow(false);
    });
  }

  function scheduleTemplateVirtualRender(force) {
    const v = ensureTemplateVirtualState();
    if (!els.templatePreviewOutput) {
      return;
    }
    if (force) {
      if (v.pendingRaf) {
        const cancel = typeof window.cancelAnimationFrame === "function"
          ? window.cancelAnimationFrame.bind(window)
          : clearTimeout;
        cancel(v.pendingRaf);
        v.pendingRaf = 0;
      }
      renderTemplateVirtualWindow(true);
      return;
    }

    if (v.pendingRaf) {
      return;
    }
    const schedule = typeof window.requestAnimationFrame === "function"
      ? window.requestAnimationFrame.bind(window)
      : (cb) => setTimeout(cb, 16);
    v.pendingRaf = schedule(() => {
      v.pendingRaf = 0;
      renderTemplateVirtualWindow(false);
    });
  }

  function attachVirtualScrollHandlersIfNeeded() {
    const outputV = ensureOutputVirtualState();
    if (els.output && !outputV.scrollBound) {
      els.output.addEventListener("scroll", () => {
        if (outputV.adjustingScroll) {
          return;
        }
        if (state.rightTab === "output") {
          scheduleOutputVirtualRender(false);
        }
      }, { passive: true });
      outputV.scrollBound = true;
    }

    const templateV = ensureTemplateVirtualState();
    if (els.templatePreviewOutput && !templateV.scrollBound) {
      els.templatePreviewOutput.addEventListener("scroll", () => {
        if (templateV.adjustingScroll) {
          return;
        }
        if (state.rightTab === "template") {
          scheduleTemplateVirtualRender(false);
        }
      }, { passive: true });
      templateV.scrollBound = true;
    }
  }

  function applyTemplateTableVisibility(block, absIndex, interactive) {
    if (!block || interactive === false) {
      return block;
    }

    const table = block.querySelector(".template-preview-table");
    if (!table) {
      return block;
    }

    const indexKey = String(absIndex);
    const actions = block.querySelector(".template-block-actions");
    if (actions && !actions.querySelector("[data-template-action=\"toggle-table\"]")) {
      const expanded = perf.templateExpandedBlocks.has(indexKey);
      const toggleBtn = document.createElement("button");
      toggleBtn.type = "button";
      toggleBtn.className = "secondary";
      toggleBtn.setAttribute("data-template-action", "toggle-table");
      toggleBtn.textContent = expanded ? "Hide Table" : "Show Table";
      toggleBtn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        if (perf.templateExpandedBlocks.has(indexKey)) {
          perf.templateExpandedBlocks.delete(indexKey);
        } else {
          perf.templateExpandedBlocks.add(indexKey);
        }
        if (typeof renderTemplatePreview === "function") {
          renderTemplatePreview();
        }
        if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(indexKey, { scroll: false, ensureVisible: true });
        }
      });
      actions.appendChild(toggleBtn);
    }

    if (perf.templateShowAllTables) {
      return block;
    }

    const expanded = perf.templateExpandedBlocks.has(indexKey);
    if (expanded) {
      return block;
    }

    table.remove();
    const placeholder = document.createElement("div");
    placeholder.className = "template-empty";
    placeholder.textContent = "[Table hidden for performance. Click Show Table.]";
    block.appendChild(placeholder);
    return block;
  }

  function buildTemplateBlockElementCompat(item, absIndex, config, interactive) {
    if (typeof buildTemplateBlockElement === "function") {
      const block = buildTemplateBlockElement(item, absIndex, config, interactive);
      return applyTemplateTableVisibility(block, absIndex, interactive);
    }

    if (typeof resolveTemplateMapForObject !== "function" || typeof buildTemplateContextObject !== "function") {
      return null;
    }

    const obj = item && item.obj && typeof item.obj === "object" ? item.obj : item;
    if (!obj || typeof obj !== "object") {
      return null;
    }

    const depth = Math.max(0, Number(item && item.depth) || 0);
    const templateContextObj = buildTemplateContextObject(obj, absIndex + 1);
    const resolved = resolveTemplateMapForObject(obj, config);

    const block = document.createElement("div");
    block.className = "template-block";
    block.setAttribute("data-template-index", String(absIndex));
    block.setAttribute("data-depth", String(depth));
    const lineStart = Number(obj.lineStart) || 0;
    if (lineStart > 0) {
      block.setAttribute("data-line-start", String(lineStart));
    }
    const indentPx = Math.min(120, depth * 12);
    block.style.marginLeft = indentPx > 0 ? `${indentPx}px` : "";

    const header = document.createElement("div");
    header.className = "template-block-header";
    const left = document.createElement("div");
    const title = document.createElement("h4");
    title.className = "template-block-title";
    const label = typeof getObjectLabel === "function" ? getObjectLabel(obj) : "";
    title.textContent = `${absIndex + 1}. ${String(obj.objectType || "OBJECT")}${label ? ` ${label}` : ""}`;
    left.appendChild(title);
    const meta = document.createElement("div");
    meta.className = "template-block-meta";
    const metaText = typeof renderMeta === "function" ? renderMeta(obj) : "";
    const keyText = resolved && resolved.key ? `template=${resolved.key}` : "template=missing";
    meta.textContent = [metaText, keyText].filter(Boolean).join(" • ");
    left.appendChild(meta);
    header.appendChild(left);

    if (interactive !== false) {
      const actions = document.createElement("div");
      actions.className = "template-block-actions";

      const codeBtn = document.createElement("button");
      codeBtn.type = "button";
      codeBtn.className = "secondary";
      codeBtn.setAttribute("data-template-action", "code");
      codeBtn.textContent = "Code";
      codeBtn.addEventListener("click", () => {
        if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(String(absIndex));
        }
        if (lineStart > 0 && typeof selectCodeLines === "function") {
          const lineEnd = Number(obj && obj.block && obj.block.lineEnd) || lineStart;
          selectCodeLines(lineStart, lineEnd);
        }
      });
      actions.appendChild(codeBtn);

      const pathsBtn = document.createElement("button");
      pathsBtn.type = "button";
      pathsBtn.className = "secondary";
      pathsBtn.setAttribute("data-template-action", "paths");
      pathsBtn.textContent = "Paths";
      pathsBtn.addEventListener("click", (ev) => {
        ev.stopPropagation();
        if (typeof openTemplatePathDump === "function") {
          openTemplatePathDump(templateContextObj, absIndex, obj);
        }
      });
      actions.appendChild(pathsBtn);

      const copyBtn = document.createElement("button");
      copyBtn.type = "button";
      copyBtn.className = "secondary";
      copyBtn.setAttribute("data-template-action", "copy");
      copyBtn.textContent = "Copy";
      copyBtn.addEventListener("click", async () => {
        try {
          const payload = typeof buildTemplateCopyPayloadFromBlock === "function"
            ? buildTemplateCopyPayloadFromBlock(block)
            : { node: block.cloneNode(true), text: String(block.innerText || "") };
          if (!payload.node || typeof copyHtmlWithFallback !== "function") {
            return;
          }
          await copyHtmlWithFallback(payload.node.outerHTML, payload.text);
          if (typeof setError === "function") {
            setError("");
          }
        } catch (err) {
          if (typeof setError === "function") {
            setError(`Copy failed: ${err && err.message ? err.message : err}`);
          }
        }
      });
      actions.appendChild(copyBtn);
      header.appendChild(actions);
    }

    block.appendChild(header);

    if (!resolved || !resolved.map || typeof resolved.map !== "object") {
      const empty = document.createElement("div");
      empty.className = "template-empty";
      empty.textContent = "[Missing template]";
      block.appendChild(empty);
      return applyTemplateTableVisibility(block, absIndex, interactive);
    }

    if (typeof buildTemplateGridModel === "function" && typeof renderTemplateTable === "function") {
      const model = buildTemplateGridModel(templateContextObj, resolved.map, resolved.options);
      if (Array.isArray(model && model.errors) && model.errors.length) {
        const errNode = document.createElement("div");
        errNode.className = "template-error";
        errNode.textContent = model.errors.join("\n");
        block.appendChild(errNode);
      }

      const table = renderTemplateTable(model);
      if (table) {
        block.appendChild(table);
      } else {
        const empty = document.createElement("div");
        empty.className = "template-empty";
        empty.textContent = "[Missing template]";
        block.appendChild(empty);
      }
    }

    if (interactive !== false) {
      block.addEventListener("click", () => {
        if (typeof setSelectedTemplateBlock === "function") {
          setSelectedTemplateBlock(String(absIndex), { scroll: false });
        }
      });
    }

    return applyTemplateTableVisibility(block, absIndex, interactive);
  }

  function renderOutputVirtualWindow(force) {
    const v = ensureOutputVirtualState();
    if (!els.output || !Array.isArray(v.roots) || !v.roots.length) {
      return;
    }

    const scrollTop = Number(els.output.scrollTop) || 0;
    const viewportHeight = Math.max(1, Number(els.output.clientHeight) || 1);
    const total = v.total;
    const range = buildVirtualRange(total, scrollTop, viewportHeight, v.avgHeight);
    if (!force && v.start === range.start && v.end === range.end) {
      return;
    }
    v.start = range.start;
    v.end = range.end;

    const fragment = document.createDocumentFragment();
    fragment.appendChild(createVirtualSpacer("virtual-spacer-top", range.start * v.avgHeight));

    for (let i = range.start; i < range.end; i += 1) {
      const rootNode = v.roots[i];
      const card = renderNode(rootNode);
      if (card && typeof card.setAttribute === "function") {
        card.setAttribute("data-root-index", String(i));
      }
      fragment.appendChild(card);
    }

    fragment.appendChild(createVirtualSpacer("virtual-spacer-bottom", (total - range.end) * v.avgHeight));
    els.output.classList.remove("muted");
    els.output.replaceChildren(fragment);

    const renderedRootCards = Array.from(els.output.querySelectorAll(".card[data-root-index]"));
    if (renderedRootCards.length) {
      let heightSum = 0;
      for (const card of renderedRootCards) {
        heightSum += Math.max(40, Number(card.offsetHeight) || 0);
      }
      const measuredAvg = Math.max(80, heightSum / renderedRootCards.length);
      v.avgHeight = Math.max(80, Math.min(1200, (v.avgHeight * 0.75) + (measuredAvg * 0.25)));
    }

    if (state.selectedId && typeof setSelectedCard === "function") {
      setSelectedCard(state.selectedId, { scroll: false });
    }

    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
  }

  function renderTemplateVirtualWindow(force) {
    const v = ensureTemplateVirtualState();
    if (!els.templatePreviewOutput || !Array.isArray(v.items) || !v.items.length) {
      return;
    }

    const scrollTop = Number(els.templatePreviewOutput.scrollTop) || 0;
    const viewportHeight = Math.max(1, Number(els.templatePreviewOutput.clientHeight) || 1);
    const total = v.total;
    const range = buildVirtualRange(total, scrollTop, viewportHeight, v.avgHeight);
    if (!force && v.start === range.start && v.end === range.end) {
      return;
    }
    v.start = range.start;
    v.end = range.end;

    const fragment = document.createDocumentFragment();
    fragment.appendChild(createVirtualSpacer("virtual-spacer-top", range.start * v.avgHeight));

    for (let i = range.start; i < range.end; i += 1) {
      const block = buildTemplateBlockElementCompat(v.items[i], i, v.config, true);
      if (block) {
        fragment.appendChild(block);
      }
    }

    fragment.appendChild(createVirtualSpacer("virtual-spacer-bottom", (total - range.end) * v.avgHeight));
    els.templatePreviewOutput.classList.remove("muted");
    els.templatePreviewOutput.replaceChildren(fragment);

    const renderedBlocks = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block[data-template-index]"));
    if (renderedBlocks.length) {
      let heightSum = 0;
      for (const block of renderedBlocks) {
        heightSum += Math.max(40, Number(block.offsetHeight) || 0);
      }
      const measuredAvg = Math.max(60, heightSum / renderedBlocks.length);
      v.avgHeight = Math.max(60, Math.min(1000, (v.avgHeight * 0.75) + (measuredAvg * 0.25)));
    }

    if (state.selectedTemplateIndex !== "" && typeof setSelectedTemplateBlock === "function") {
      setSelectedTemplateBlock(state.selectedTemplateIndex, { scroll: false });
    }
    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
  }

  const originalRenderOutput = typeof renderOutput === "function" ? renderOutput : null;
  const originalRenderTemplatePreview = typeof renderTemplatePreview === "function" ? renderTemplatePreview : null;
  const originalSetSelectedCard = typeof setSelectedCard === "function" ? setSelectedCard : null;
  const originalSetSelectedTemplateBlock = typeof setSelectedTemplateBlock === "function" ? setSelectedTemplateBlock : null;
  const originalCopyAllTemplateBlocks = typeof copyAllTemplateBlocks === "function" ? copyAllTemplateBlocks : null;

  if (originalRenderOutput && typeof filterTree === "function") {
    renderOutput = function renderOutputVirtualized() {
      attachVirtualScrollHandlersIfNeeded();
      if (typeof setError === "function") {
        setError("");
      }

      if (!state.data || !Array.isArray(state.renderObjects)) {
        if (typeof setOutputMessage === "function") {
          setOutputMessage("No data loaded.");
        } else if (els.output) {
          els.output.classList.add("muted");
          els.output.textContent = "No data loaded.";
        }
        if (typeof refreshInputGutterTargets === "function") {
          refreshInputGutterTargets();
        }
        return;
      }

      state.query = (els.searchInput && els.searchInput.value ? String(els.searchInput.value) : "").trim().toLowerCase();
      state.type = els.typeFilter && els.typeFilter.value ? String(els.typeFilter.value) : "";
      state.showRaw = Boolean(els.showRaw && els.showRaw.checked);
      state.showKeywords = Boolean(els.showKeywords && els.showKeywords.checked);
      state.showValues = Boolean(els.showValues && els.showValues.checked);
      state.showExtras = Boolean(els.showExtras && els.showExtras.checked);

      const roots = (state.renderObjects || [])
        .map((obj) => filterTree(obj))
        .filter(Boolean);

      if (!roots.length) {
        if (typeof setOutputMessage === "function") {
          setOutputMessage("No matches.");
        } else if (els.output) {
          els.output.classList.add("muted");
          els.output.textContent = "No matches.";
        }
        if (typeof refreshInputGutterTargets === "function") {
          refreshInputGutterTargets();
        }
        return;
      }

      const v = ensureOutputVirtualState();
      v.roots = roots;
      v.total = roots.length;
      v.start = -1;
      v.end = -1;
      const maps = collectOutputVirtualMaps(roots);
      v.idToRootIndex = maps.idToRootIndex;
      v.lineTargetMap = maps.lineTargetMap;

      renderOutputVirtualWindow(true);
    };
  }

  if (originalSetSelectedCard) {
    setSelectedCard = function setSelectedCardVirtualAware(id, options) {
      const normalized = typeof normalizeId === "function" ? normalizeId(id) : String(id || "");
      const v = ensureOutputVirtualState();
      if (normalized && v && v.idToRootIndex instanceof Map && v.idToRootIndex.has(normalized) && els.output) {
        const rootIndex = Number(v.idToRootIndex.get(normalized));
        const outsideRange = rootIndex < v.start || rootIndex >= v.end;
        if (outsideRange) {
          const targetTop = Math.max(0, Math.round((rootIndex * v.avgHeight) - (Number(els.output.clientHeight || 0) * 0.2)));
          v.adjustingScroll = true;
          els.output.scrollTop = targetTop;
          renderOutputVirtualWindow(true);
          v.adjustingScroll = false;
        }
      }
      return originalSetSelectedCard.apply(this, arguments);
    };
  }

  if (originalRenderTemplatePreview && typeof getRenderableObjectListForTemplate === "function") {
    renderTemplatePreview = function renderTemplatePreviewVirtualized() {
      attachVirtualScrollHandlersIfNeeded();
      if (!els.templatePreviewOutput) {
        return;
      }

      if (!state.data || !Array.isArray(state.renderObjects)) {
        setTemplatePreviewMessageCompat("No data loaded.");
        return;
      }

      const config = state.templateConfig && typeof state.templateConfig === "object"
        ? state.templateConfig
        : (typeof getDefaultTemplateConfig === "function" ? getDefaultTemplateConfig() : {});

      if (typeof validateTemplateConfig === "function") {
        const check = validateTemplateConfig(config);
        if (!check.valid) {
          setTemplatePreviewMessageCompat("Template config is invalid.");
          if (typeof setTemplateConfigError === "function") {
            setTemplateConfigError((check.errors || []).join("\n"));
          }
          return;
        }
      }

      const items = getRenderableObjectListForTemplate();
      if (!Array.isArray(items) || !items.length) {
        setTemplatePreviewMessageCompat("No renderable objects.");
        return;
      }

      const v = ensureTemplateVirtualState();
      v.items = items;
      v.total = items.length;
      v.start = -1;
      v.end = -1;
      v.config = config;
      v.lineTargetMap = collectTemplateVirtualLineMap(items);

      renderTemplateVirtualWindow(true);
      state.templatePreviewCache = { count: items.length };
    };
  }

  if (originalSetSelectedTemplateBlock) {
    setSelectedTemplateBlock = function setSelectedTemplateBlockVirtualAware(index, options) {
      const normalized = String(index === undefined || index === null ? "" : index).trim();
      const parsed = Number(normalized);
      const v = ensureTemplateVirtualState();
      if (Number.isFinite(parsed) && parsed >= 0 && parsed < v.total && els.templatePreviewOutput) {
        const outsideRange = parsed < v.start || parsed >= v.end;
        if (outsideRange) {
          const targetTop = Math.max(0, Math.round((parsed * v.avgHeight) - (Number(els.templatePreviewOutput.clientHeight || 0) * 0.2)));
          v.adjustingScroll = true;
          els.templatePreviewOutput.scrollTop = targetTop;
          renderTemplateVirtualWindow(true);
          v.adjustingScroll = false;
        }
      }
      return originalSetSelectedTemplateBlock.apply(this, arguments);
    };
  }

  if (originalCopyAllTemplateBlocks) {
    copyAllTemplateBlocks = async function copyAllTemplateBlocksVirtualAware() {
      const v = ensureTemplateVirtualState();
      if (!Array.isArray(v.items) || !v.items.length) {
        return originalCopyAllTemplateBlocks.apply(this, arguments);
      }
      if (typeof copyHtmlWithFallback !== "function") {
        return originalCopyAllTemplateBlocks.apply(this, arguments);
      }

      const wrapper = document.createElement("div");
      const plainLines = [];
      const tableOnly = typeof isTemplateCopyTableOnlyEnabled === "function"
        ? isTemplateCopyTableOnlyEnabled()
        : false;

      for (let i = 0; i < v.items.length; i += 1) {
        const block = buildTemplateBlockElementCompat(v.items[i], i, v.config, false);
        if (!block) {
          continue;
        }
        const payload = typeof buildTemplateCopyPayloadFromBlock === "function"
          ? buildTemplateCopyPayloadFromBlock(block)
          : { node: block, text: String(block.innerText || "") };
        if (!payload || !payload.node) {
          continue;
        }
        wrapper.appendChild(payload.node);
        if (tableOnly) {
          wrapper.appendChild(document.createElement("br"));
        }
        plainLines.push(String(payload.text || ""));
      }

      if (!wrapper.childNodes.length) {
        if (typeof setError === "function") {
          setError("Nothing to copy.");
        }
        return;
      }

      await copyHtmlWithFallback(wrapper.innerHTML, plainLines.filter(Boolean).join("\n\n"));
    };
  }

  bindTemplateShowAllTablesControl();

  function clearDescriptionsVirtualState() {
    perf.descVirtual = null;
  }

  function scheduleDescriptionsVirtualRender(force) {
    const v = perf.descVirtual;
    if (!v || typeof v !== "object") {
      return;
    }

    if (force) {
      if (v.pendingRaf) {
        const cancel = typeof window.cancelAnimationFrame === "function"
          ? window.cancelAnimationFrame.bind(window)
          : clearTimeout;
        cancel(v.pendingRaf);
        v.pendingRaf = 0;
      }
      renderDescriptionsVirtualRows();
      return;
    }

    if (v.pendingRaf) {
      return;
    }

    const schedule = typeof window.requestAnimationFrame === "function"
      ? window.requestAnimationFrame.bind(window)
      : (cb) => setTimeout(cb, 16);

    v.pendingRaf = schedule(() => {
      v.pendingRaf = 0;
      renderDescriptionsVirtualRows();
    });
  }

  function renderDescriptionsVirtualRows() {
    const v = perf.descVirtual;
    if (!v || typeof v !== "object" || !els.declDescPanel) {
      return;
    }
    if (!v.table || !v.tbody || !Array.isArray(v.rows) || !v.rows.length) {
      return;
    }

    const total = v.rows.length;
    const rowHeight = Math.max(20, Number(v.rowHeight) || 28);
    const panelHeight = Math.max(1, Number(els.declDescPanel.clientHeight) || 1);
    const visibleEstimate = Math.max(1, Math.ceil(panelHeight / rowHeight));
    const overscan = Math.max(6, Math.ceil(visibleEstimate * 0.75));

    const tableTop = Number(v.table.offsetTop) || 0;
    const headerHeight = Number(v.theadHeight) || 0;
    const listScrollTop = Math.max(0, (Number(els.declDescPanel.scrollTop) || 0) - tableTop - headerHeight);

    let start = Math.max(0, Math.floor(listScrollTop / rowHeight) - overscan);
    let end = Math.min(total, start + visibleEstimate + (overscan * 2));
    start = Math.max(0, Math.min(start, end));

    if (start === v.start && end === v.end && !v.forceNext) {
      return;
    }
    v.forceNext = false;
    v.start = start;
    v.end = end;

    const fragment = document.createDocumentFragment();
    const colSpan = Math.max(1, Number(v.colCount) || 1);

    const topSpacerHeight = start * rowHeight;
    if (topSpacerHeight > 0) {
      const topSpacer = document.createElement("tr");
      const td = document.createElement("td");
      td.colSpan = colSpan;
      td.style.height = `${topSpacerHeight}px`;
      td.style.padding = "0";
      td.style.border = "none";
      topSpacer.appendChild(td);
      fragment.appendChild(topSpacer);
    }

    for (let i = start; i < end; i += 1) {
      fragment.appendChild(v.rows[i]);
    }

    const bottomSpacerHeight = Math.max(0, (total - end) * rowHeight);
    if (bottomSpacerHeight > 0) {
      const bottomSpacer = document.createElement("tr");
      const td = document.createElement("td");
      td.colSpan = colSpan;
      td.style.height = `${bottomSpacerHeight}px`;
      td.style.padding = "0";
      td.style.border = "none";
      bottomSpacer.appendChild(td);
      fragment.appendChild(bottomSpacer);
    }

    v.tbody.replaceChildren(fragment);
  }

  function enableDescriptionsVirtualRowsIfNeeded() {
    if (!els.declDescTable || !els.declDescPanel) {
      clearDescriptionsVirtualState();
      return;
    }

    const table = els.declDescTable.querySelector("table");
    if (!table) {
      clearDescriptionsVirtualState();
      return;
    }

    const tbody = table.querySelector("tbody");
    if (!tbody) {
      clearDescriptionsVirtualState();
      return;
    }

    const allRows = Array.from(tbody.querySelectorAll("tr"));
    if (allRows.length < 180) {
      clearDescriptionsVirtualState();
      return;
    }

    const header = table.querySelector("thead");
    const colCount = header ? header.querySelectorAll("th").length : 1;
    const sampleRow = allRows[0];
    const rowHeight = Math.max(20, Number(sampleRow && sampleRow.offsetHeight) || 28);

    const lineTargetMap = new Map();
    const declKeyToIndex = new Map();
    for (let i = 0; i < allRows.length; i += 1) {
      const row = allRows[i];
      const line = Number(row && row.getAttribute ? row.getAttribute("data-line-start") : 0) || 0;
      const declKey = String(row && row.getAttribute ? row.getAttribute("data-decl-key") || "" : "");
      if (line && declKey && !lineTargetMap.has(line)) {
        lineTargetMap.set(line, { kind: "descriptions", declKey });
      }
      if (declKey && !declKeyToIndex.has(declKey)) {
        declKeyToIndex.set(declKey, i);
      }
    }

    perf.descVirtual = {
      table,
      tbody,
      rows: allRows,
      rowHeight,
      colCount,
      theadHeight: Number(header && header.offsetHeight) || 0,
      lineTargetMap,
      declKeyToIndex,
      start: -1,
      end: -1,
      pendingRaf: 0,
      forceNext: true
    };

    if (!els.declDescPanel.__abapDescVirtualBound) {
      els.declDescPanel.addEventListener("scroll", () => {
        if (state.rightTab === "descriptions") {
          scheduleDescriptionsVirtualRender(false);
        }
      }, { passive: true });
      els.declDescPanel.__abapDescVirtualBound = true;
    }

    scheduleDescriptionsVirtualRender(true);
  }

  function ensureDescriptionsDeclKeyVisible(declKey) {
    const key = String(declKey || "").trim();
    const v = perf.descVirtual;
    if (!key || !v || !(v.declKeyToIndex instanceof Map)) {
      return;
    }

    const targetIndex = v.declKeyToIndex.get(key);
    if (!Number.isInteger(targetIndex) || targetIndex < 0 || targetIndex >= v.rows.length) {
      return;
    }

    if (targetIndex >= v.start && targetIndex < v.end) {
      return;
    }

    const panelHeight = Math.max(1, Number(els.declDescPanel && els.declDescPanel.clientHeight) || 1);
    const rowHeight = Math.max(20, Number(v.rowHeight) || 28);
    const visibleEstimate = Math.max(1, Math.ceil(panelHeight / rowHeight));
    const overscan = Math.max(6, Math.ceil(visibleEstimate * 0.75));
    const nextStart = clampInt(targetIndex - overscan, 0, Math.max(0, v.rows.length - 1));
    const nextEnd = clampInt(nextStart + visibleEstimate + (overscan * 2), 1, v.rows.length);

    v.start = nextStart;
    v.end = nextEnd;
    v.forceNext = true;
    scheduleDescriptionsVirtualRender(true);
  }

  if (typeof renderDeclDescPanelUi === "function") {
    const originalRenderDeclDescPanelUi = renderDeclDescPanelUi;
    renderDeclDescPanelUi = function renderDeclDescPanelUiWithVirtualRows() {
      const result = originalRenderDeclDescPanelUi.apply(this, arguments);
      enableDescriptionsVirtualRowsIfNeeded();
      return result;
    };
  }

  if (typeof setSelectedDeclRow === "function") {
    const originalSetSelectedDeclRow = setSelectedDeclRow;
    setSelectedDeclRow = function setSelectedDeclRowWithVirtualRows(declKey) {
      ensureDescriptionsDeclKeyVisible(declKey);
      return originalSetSelectedDeclRow.apply(this, arguments);
    };
  }

  if (typeof computeInputGutterTargetsForDescriptions === "function") {
    const originalComputeDescriptionsTargets = computeInputGutterTargetsForDescriptions;
    computeInputGutterTargetsForDescriptions = function computeInputGutterTargetsForDescriptionsVirtualAware() {
      const v = perf.descVirtual;
      if (v && v.lineTargetMap instanceof Map && v.lineTargetMap.size) {
        const targets = new Map();
        for (const [line, target] of v.lineTargetMap.entries()) {
          targets.set(line, target);
        }
        return targets;
      }
      return originalComputeDescriptionsTargets.apply(this, arguments);
    };
  }

  if (typeof computeInputGutterTargetsForOutput === "function") {
    const originalComputeOutputTargets = computeInputGutterTargetsForOutput;
    computeInputGutterTargetsForOutput = function computeInputGutterTargetsForOutputVirtualAware() {
      const v = perf.outputVirtual;
      if (v && v.lineTargetMap instanceof Map && v.lineTargetMap.size) {
        const targets = new Map();
        for (const [line, target] of v.lineTargetMap.entries()) {
          targets.set(line, target);
        }
        return targets;
      }
      return originalComputeOutputTargets.apply(this, arguments);
    };
  }

  if (typeof computeInputGutterTargetsForTemplate === "function") {
    const originalComputeTemplateTargets = computeInputGutterTargetsForTemplate;
    computeInputGutterTargetsForTemplate = function computeInputGutterTargetsForTemplateVirtualAware() {
      const v = perf.templateVirtual;
      if (v && v.lineTargetMap instanceof Map && v.lineTargetMap.size) {
        const targets = new Map();
        for (const [line, target] of v.lineTargetMap.entries()) {
          targets.set(line, target);
        }
        return targets;
      }
      return originalComputeTemplateTargets.apply(this, arguments);
    };
  }
})();
