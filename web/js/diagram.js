(function (ns) {
  "use strict";

  const SVG_NS = "http://www.w3.org/2000/svg";

  function el(name, attrs) {
    const node = document.createElementNS(SVG_NS, name);
    for (const [k, v] of Object.entries(attrs || {})) node.setAttribute(k, String(v));
    return node;
  }

  function render(model, host, options) {
    const onNodeClick = options?.onNodeClick || null;
    host.innerHTML = "";

    const nodes = Array.from(model.nodes.values()).sort((a, b) => {
      if (a.depth !== b.depth) return a.depth - b.depth;
      const ka = a.kind === "EVENT" ? 0 : a.kind === "FORM" ? 1 : 2;
      const kb = b.kind === "EVENT" ? 0 : b.kind === "FORM" ? 1 : 2;
      if (ka !== kb) return ka - kb;
      return a.name.localeCompare(b.name);
    });

    const maxDepth = Math.max(0, ...nodes.map((n) => n.depth ?? 0));
    const cols = Array.from({ length: maxDepth + 1 }, () => []);
    for (const n of nodes) cols[n.depth ?? 0].push(n);

    const margin = 18;
    const nodeW = 230;
    const nodeH = 72;
    const colGap = 80;
    const rowGap = 18;

    const positions = new Map();

    let totalH = 0;
    for (const col of cols) {
      const h = margin * 2 + col.length * nodeH + Math.max(0, col.length - 1) * rowGap;
      totalH = Math.max(totalH, h);
    }
    const totalW = margin * 2 + cols.length * nodeW + Math.max(0, cols.length - 1) * colGap;

    const svg = el("svg", { width: totalW, height: totalH, viewBox: `0 0 ${totalW} ${totalH}` });

    const defs = el("defs");
    const marker = el("marker", { id: "arrow", markerWidth: 10, markerHeight: 10, refX: 9, refY: 5, orient: "auto" });
    marker.appendChild(el("path", { d: "M0 0 L10 5 L0 10 z", fill: "#64748b" }));
    defs.appendChild(marker);
    svg.appendChild(defs);

    for (let d = 0; d < cols.length; d++) {
      const col = cols[d];
      const x = margin + d * (nodeW + colGap);
      let y = margin;
      for (const n of col) {
        positions.set(n.key, { x, y, w: nodeW, h: nodeH });
        y += nodeH + rowGap;
      }
    }

    const edgesGroup = el("g", {});
    for (const edge of model.edges) {
      const from = positions.get(edge.fromKey);
      const to = positions.get(edge.toKey);
      if (!from || !to) continue;

      const sx = from.x + from.w;
      const sy = from.y + from.h / 2;
      const ex = to.x;
      const ey = to.y + to.h / 2;
      const bend = Math.max(40, Math.min(140, Math.abs(ex - sx) * 0.35));
      const c1x = sx + bend;
      const c1y = sy;
      const c2x = ex - bend;
      const c2y = ey;
      const dAttr = `M ${sx} ${sy} C ${c1x} ${c1y}, ${c2x} ${c2y}, ${ex} ${ey}`;

      edgesGroup.appendChild(
        el("path", {
          d: dAttr,
          fill: "none",
          stroke: edge.isInCycle ? "#ef4444" : "#94a3b8",
          "stroke-width": edge.isInCycle ? 2.2 : 1.6,
          "marker-end": "url(#arrow)",
          opacity: edge.isInCycle ? 0.95 : 0.75,
        }),
      );
    }
    svg.appendChild(edgesGroup);

    const nodesGroup = el("g", {});
    for (const n of nodes) {
      const pos = positions.get(n.key);
      if (!pos) continue;

      const g = el("g", { class: "node", "data-key": n.key });
      const fill = n.isDefined ? "#f8fafc" : "#e5e7eb";
      const stroke = n.isInCycle ? "#ef4444" : n.kind === "EVENT" ? "#60a5fa" : "#64748b";

      const rect = el("rect", {
        x: pos.x,
        y: pos.y,
        width: pos.w,
        height: pos.h,
        rx: 12,
        ry: 12,
        fill,
        stroke,
        "stroke-width": n.isInCycle ? 2.4 : 1.6,
      });
      if (!n.isDefined) rect.setAttribute("stroke-dasharray", "6 4");

      const title = el("text", { x: pos.x + 12, y: pos.y + 24, fill: "#0b1020", "font-size": 13, "font-family": "system-ui, sans-serif" });
      title.textContent = `${n.kind} ${n.name}`;

      const meta = el("text", { x: pos.x + 12, y: pos.y + 46, fill: "#0b1020", "font-size": 11, "font-family": "ui-monospace, monospace" });
      meta.textContent = `P:${n.paramsCount} D:${n.localDataCount} C:${n.localConstCount} W:${n.writesCount}`;

      g.appendChild(rect);
      g.appendChild(title);
      g.appendChild(meta);

      if (onNodeClick) {
        g.style.cursor = "pointer";
        g.addEventListener("click", () => onNodeClick(n.key));
      }

      nodesGroup.appendChild(g);
    }
    svg.appendChild(nodesGroup);

    host.appendChild(svg);

    return { svg, positions };
  }

  ns.diagram = { render };
})(window.AbapFlow);

