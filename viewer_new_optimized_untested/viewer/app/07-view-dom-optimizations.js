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

  if (typeof buildTemplateBlockElement === "function") {
    const originalBuildTemplateBlockElement = buildTemplateBlockElement;
    buildTemplateBlockElement = function buildTemplateBlockElementWithLazyTable(item, absIndex, config, interactive) {
      const block = originalBuildTemplateBlockElement(item, absIndex, config, interactive);
      if (!block || interactive === false) {
        return block;
      }

      const indexKey = String(absIndex);
      const table = block.querySelector(".template-preview-table");
      if (!table) {
        return block;
      }

      if (perf.templateShowAllTables) {
        return block;
      }

      const expanded = perf.templateExpandedBlocks.has(indexKey);
      const actions = block.querySelector(".template-block-actions");
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

      if (actions) {
        actions.appendChild(toggleBtn);
      }

      if (!expanded) {
        table.remove();
        const placeholder = document.createElement("div");
        placeholder.className = "template-empty";
        placeholder.textContent = "[Table hidden for performance. Click Show Table.]";
        block.appendChild(placeholder);
      }

      return block;
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
})();
