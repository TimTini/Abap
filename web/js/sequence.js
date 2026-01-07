(function (ns) {
  "use strict";

  const PROGRAM_ALL_KEY = "SEQUENCE:PROGRAM_ALL";

  function getStartCandidates(model) {
    const nodes = Array.from(model.nodes.values());
    nodes.sort((a, b) => {
      const la = a.sourceRef?.startLine ?? 1e9;
      const lb = b.sourceRef?.startLine ?? 1e9;
      if (la !== lb) return la - lb;
      const ka = a.kind === "EVENT" ? 0 : a.kind === "FORM" ? 1 : 2;
      const kb = b.kind === "EVENT" ? 0 : b.kind === "FORM" ? 1 : 2;
      if (ka !== kb) return ka - kb;
      return a.name.localeCompare(b.name);
    });

    const items = nodes.map((n) => ({
      key: n.key,
      label: `${n.kind} ${n.name}${n.isDefined ? "" : " (external)"}`,
    }));

    items.unshift({ key: PROGRAM_ALL_KEY, label: "PROGRAM (All entrypoints)" });
    return items;
  }

  function getProgramEntrypoints(model) {
    const nodes = Array.from(model.nodes.values());
    const events = nodes
      .filter((n) => n.kind === "EVENT")
      .sort((a, b) => (a.sourceRef?.startLine ?? 1e9) - (b.sourceRef?.startLine ?? 1e9))
      .map((n) => n.key);

    if (events.length > 0) return events;

    const forms = nodes
      .filter((n) => n.kind === "FORM" && n.isDefined && (n.calledBy?.length || 0) === 0)
      .sort((a, b) => (a.sourceRef?.startLine ?? 1e9) - (b.sourceRef?.startLine ?? 1e9))
      .map((n) => n.key);

    return forms;
  }

  function build(model, startKey, options) {
    const maxSteps = Math.max(10, Number(options?.maxSteps || 200));
    const steps = [];

    const rowKeys = [];
    const rowSet = new Set();
    function ensureRow(key) {
      if (!rowSet.has(key)) {
        rowSet.add(key);
        rowKeys.push(key);
      }
    }

    let truncated = false;

    function walk(routineKey, depth, stack) {
      if (steps.length >= maxSteps) {
        truncated = true;
        return;
      }

      const routine = model.nodes.get(routineKey);
      if (!routine) return;

      stack.add(routineKey);

      const calls = (routine.calls || []).slice().sort((a, b) => {
        const la = a.sourceRef?.startLine ?? 1e9;
        const lb = b.sourceRef?.startLine ?? 1e9;
        return la - lb;
      });

      for (const edge of calls) {
        if (steps.length >= maxSteps) {
          truncated = true;
          break;
        }

        ensureRow(edge.fromKey);
        ensureRow(edge.toKey);

        const isRecursion = stack.has(edge.toKey);
        steps.push({
          kind: "call",
          depth,
          fromKey: edge.fromKey,
          toKey: edge.toKey,
          edge,
          isRecursion,
        });

        if (!isRecursion) walk(edge.toKey, depth + 1, stack);
      }

      stack.delete(routineKey);
    }

    if (startKey === PROGRAM_ALL_KEY) {
      const roots = getProgramEntrypoints(model);
      for (let r = 0; r < roots.length; r++) {
        const rootKey = roots[r];
        const root = model.nodes.get(rootKey);
        ensureRow(rootKey);

        steps.push({
          kind: "separator",
          label: root ? `${root.kind} ${root.name}` : rootKey,
          rootKey,
        });

        const stack = new Set();
        walk(rootKey, 0, stack);
        if (steps.length >= maxSteps) break;
      }
    } else {
      ensureRow(startKey);
      const stack = new Set();
      walk(startKey, 0, stack);
    }

    return { startKey, steps, rowKeys, truncated, maxSteps };
  }

  function render(container, model, sequence, options) {
    const onParamClick = options?.onParamClick || null;
    const onRowClick = options?.onRowClick || null;

    container.innerHTML = "";
    container.classList.remove("empty");

    const scroll = document.createElement("div");
    scroll.className = "seq-scroll";

    const grid = document.createElement("div");
    grid.className = "seq-grid";

    const labels = document.createElement("div");
    labels.className = "seq-labels";

    const canvas = document.createElement("div");
    canvas.className = "seq-canvas";

    const header = document.createElement("div");
    header.className = "seq-header";
    header.textContent = sequence.truncated
      ? `Sequence truncated at ${sequence.maxSteps} steps.`
      : `Steps: ${sequence.steps.length}`;
    labels.appendChild(header);

    function createChip(param, routineKey) {
      const b = document.createElement("button");
      b.type = "button";
      b.className = "chip";
      b.dataset.key = routineKey;
      b.dataset.var = param.name;
      const dt = param.dataType ? ` TYPE ${param.dataType}` : "";
      const desc = param.description ? ` - ${param.description}` : "";
      b.textContent = `${param.kind} ${param.name}${dt}${desc}`;
      b.title = "Jump to Trace";
      b.addEventListener("click", (ev) => {
        ev.stopPropagation();
        if (onParamClick) onParamClick({ routineKey, varName: param.name });
      });
      return b;
    }

    for (const key of sequence.rowKeys) {
      const routine = model.nodes.get(key);
      if (!routine) continue;

      const row = document.createElement("div");
      row.className = "seq-row-label";
      row.dataset.key = key;

      const title = document.createElement("div");
      title.className = "seq-row-title";
      title.textContent = `${routine.kind} ${routine.name}${routine.isDefined ? "" : " (external)"}`;
      row.appendChild(title);

      if (routine.description) {
        const desc = document.createElement("div");
        desc.className = "seq-row-desc";
        desc.textContent = routine.description;
        row.appendChild(desc);
      }

      if (routine.params && routine.params.length > 0) {
        const params = document.createElement("div");
        params.className = "seq-row-params";
        for (const p of routine.params) params.appendChild(createChip(p, key));
        row.appendChild(params);
      }

      if (onRowClick) {
        row.style.cursor = "pointer";
        row.addEventListener("click", () => onRowClick({ routineKey: key }));
      }

      labels.appendChild(row);
    }

    grid.appendChild(labels);
    grid.appendChild(canvas);
    scroll.appendChild(grid);
    container.appendChild(scroll);

    const gridRect = grid.getBoundingClientRect();
    const rowEls = Array.from(labels.querySelectorAll(".seq-row-label"));
    const rowCenters = new Map();
    const rowRanges = [];
    const rowElByKey = new Map();
    for (const rowEl of rowEls) {
      const key = rowEl.dataset.key;
      if (!key) continue;
      rowElByKey.set(key, rowEl);

      const anchorEl = rowEl.querySelector(".seq-row-title") || rowEl;
      const rect = anchorEl.getBoundingClientRect();
      const y = Math.round(rect.top - gridRect.top + rect.height / 2);
      rowCenters.set(key, y);

      const rowRect = rowEl.getBoundingClientRect();
      rowRanges.push({
        key,
        top: rowRect.top - gridRect.top,
        bottom: rowRect.bottom - gridRect.top,
      });
    }

    const steps = sequence.steps || [];
    const colW = 140;
    const marginX = 18;
    const marginY = 10;
    const headerH = header.offsetHeight;

    const width = Math.max(720, marginX * 2 + steps.length * colW);
    const height = Math.max(520, labels.scrollHeight);

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.classList.add("seq-svg");
    svg.setAttribute("width", String(width));
    svg.setAttribute("height", String(height));
    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);

    const defs = document.createElementNS("http://www.w3.org/2000/svg", "defs");
    function addMarker(id, fill) {
      const marker = document.createElementNS("http://www.w3.org/2000/svg", "marker");
      marker.setAttribute("id", id);
      marker.setAttribute("markerWidth", "10");
      marker.setAttribute("markerHeight", "10");
      marker.setAttribute("refX", "9");
      marker.setAttribute("refY", "5");
      marker.setAttribute("orient", "auto");
      const mPath = document.createElementNS("http://www.w3.org/2000/svg", "path");
      mPath.setAttribute("d", "M0 0 L10 5 L0 10 z");
      mPath.setAttribute("fill", fill);
      marker.appendChild(mPath);
      defs.appendChild(marker);
    }

    addMarker("seq-arrow", "#64748b");
    addMarker("seq-arrow-danger", "#ef4444");
    svg.appendChild(defs);

    function line(x1, y1, x2, y2, stroke, w, dash) {
      const p = document.createElementNS("http://www.w3.org/2000/svg", "line");
      p.setAttribute("x1", String(x1));
      p.setAttribute("y1", String(y1));
      p.setAttribute("x2", String(x2));
      p.setAttribute("y2", String(y2));
      p.setAttribute("stroke", stroke);
      p.setAttribute("stroke-width", String(w || 1));
      if (dash) p.setAttribute("stroke-dasharray", dash);
      svg.appendChild(p);
    }

    for (let i = 0; i < steps.length; i++) {
      const x = marginX + i * colW + colW / 2;
      line(x, headerH + marginY, x, height, "rgba(255,255,255,0.05)", 1);

      const t = document.createElementNS("http://www.w3.org/2000/svg", "text");
      t.setAttribute("x", String(x - 6));
      t.setAttribute("y", String(headerH - 10));
      t.setAttribute("fill", "rgba(255,255,255,0.45)");
      t.setAttribute("font-size", "11");
      t.setAttribute("font-family", "ui-monospace, monospace");
      t.textContent = String(i + 1);
      svg.appendChild(t);

      const step = steps[i];
      if (step && step.kind === "separator") {
        const label = document.createElementNS("http://www.w3.org/2000/svg", "text");
        label.setAttribute("x", String(x + 10));
        label.setAttribute("y", String(headerH - 28));
        label.setAttribute("fill", "rgba(96,165,250,0.85)");
        label.setAttribute("font-size", "11");
        label.setAttribute("font-family", "system-ui, sans-serif");
        label.textContent = step.label || "Section";
        svg.appendChild(label);

        line(x, headerH + marginY, x, height, "rgba(96,165,250,0.10)", 3, "8 6");
      }
    }

    for (const [key, y] of rowCenters.entries()) {
      line(0, y, width, y, "rgba(255,255,255,0.06)", 1);

      const routine = model.nodes.get(key);
      if (routine && routine.isInCycle) {
        line(0, y + 1, width, y + 1, "rgba(239,68,68,0.18)", 2);
      }
    }

    const hoverLine = document.createElementNS("http://www.w3.org/2000/svg", "line");
    hoverLine.classList.add("seq-hover-line");
    hoverLine.setAttribute("x1", "0");
    hoverLine.setAttribute("x2", String(width));
    hoverLine.setAttribute("y1", "0");
    hoverLine.setAttribute("y2", "0");
    hoverLine.style.opacity = "0";
    svg.appendChild(hoverLine);

    const connectorGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    connectorGroup.setAttribute("opacity", "0.85");
    svg.appendChild(connectorGroup);

    function bezierConnector(x1, y1, x2, y2, stroke, width) {
      const dx = Math.max(30, Math.min(70, Math.abs(x2 - x1) * 0.45));
      const p = document.createElementNS("http://www.w3.org/2000/svg", "path");
      p.setAttribute("d", `M ${x1} ${y1} C ${x1 + dx} ${y1}, ${x2 - dx} ${y2}, ${x2} ${y2}`);
      p.setAttribute("fill", "none");
      p.setAttribute("stroke", stroke);
      p.setAttribute("stroke-width", String(width));
      p.setAttribute("stroke-linecap", "round");
      p.setAttribute("stroke-linejoin", "round");
      connectorGroup.appendChild(p);
    }

    const arrowsGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svg.appendChild(arrowsGroup);

    for (let i = 0; i < steps.length; i++) {
      const st = steps[i];
      if (!st || st.kind !== "call") continue;

      const x = marginX + i * colW + colW / 2;
      const y1 = rowCenters.get(st.fromKey) ?? 0;
      const y2 = rowCenters.get(st.toKey) ?? 0;
      const isCycle = st.edge?.isInCycle || st.isRecursion;
      const isExternal = model.nodes.get(st.toKey)?.isDefined === false;

      const p = document.createElementNS("http://www.w3.org/2000/svg", "path");
      p.setAttribute("d", `M ${x} ${y1} L ${x} ${y2}`);
      p.setAttribute("fill", "none");
      p.setAttribute("stroke", isCycle ? "#ef4444" : "#94a3b8");
      p.setAttribute("stroke-width", isCycle ? "2.2" : "1.6");
      p.setAttribute("marker-end", isCycle ? "url(#seq-arrow-danger)" : "url(#seq-arrow)");
      p.setAttribute("opacity", isCycle ? "0.95" : "0.8");
      if (isExternal) p.setAttribute("stroke-dasharray", "6 4");

      const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
      const from = model.nodes.get(st.fromKey);
      const to = model.nodes.get(st.toKey);
      title.textContent = `${from ? from.name : st.fromKey} -> ${to ? to.name : st.toKey}`;
      p.appendChild(title);
      arrowsGroup.appendChild(p);

      const next = steps[i + 1];
      if (!next || next.kind !== "call") continue;
      const x2 = marginX + (i + 1) * colW + colW / 2;
      const yNext = rowCenters.get(next.fromKey);
      if (yNext == null) continue;

      const isCycle2 = isCycle || next.edge?.isInCycle || next.isRecursion;
      bezierConnector(x, y2, x2, yNext, isCycle2 ? "rgba(239,68,68,0.35)" : "rgba(96,165,250,0.35)", 3.4);
    }

    canvas.appendChild(svg);

    let labelHoverKey = null;
    let svgHoverKey = null;
    let activeHoverKey = null;

    function applyHover(key) {
      if (key === activeHoverKey) return;
      if (activeHoverKey) {
        rowElByKey.get(activeHoverKey)?.classList.remove("is-hover");
      }

      activeHoverKey = key || null;
      if (!activeHoverKey) {
        hoverLine.style.opacity = "0";
        return;
      }

      rowElByKey.get(activeHoverKey)?.classList.add("is-hover");

      const y = rowCenters.get(activeHoverKey);
      if (y == null) {
        hoverLine.style.opacity = "0";
        return;
      }

      hoverLine.setAttribute("y1", String(y));
      hoverLine.setAttribute("y2", String(y));
      hoverLine.style.opacity = "1";
    }

    function updateHover() {
      applyHover(labelHoverKey || svgHoverKey);
    }

    for (const rowEl of rowEls) {
      const key = rowEl.dataset.key;
      if (!key) continue;
      rowEl.addEventListener("mouseenter", () => {
        labelHoverKey = key;
        updateHover();
      });
      rowEl.addEventListener("mouseleave", () => {
        if (labelHoverKey === key) {
          labelHoverKey = null;
          updateHover();
        }
      });
    }

    function findRowKeyAtY(y) {
      for (let i = 0; i < rowRanges.length; i++) {
        const r = rowRanges[i];
        if (y >= r.top && y <= r.bottom) return r.key;
      }
      return null;
    }

    svg.addEventListener("mousemove", (ev) => {
      const rect = grid.getBoundingClientRect();
      const y = ev.clientY - rect.top;
      const key = findRowKeyAtY(y);
      if (key !== svgHoverKey) {
        svgHoverKey = key;
        updateHover();
      }
    });

    svg.addEventListener("mouseleave", () => {
      if (svgHoverKey) {
        svgHoverKey = null;
        updateHover();
      }
    });

    const gridW = 360 + width;
    grid.style.width = `${gridW}px`;
  }

  ns.sequence = { PROGRAM_ALL_KEY, getStartCandidates, build, render };
})(window.AbapFlow);
