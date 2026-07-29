/**
 * Shared helpers for chain-lineage UX demos.
 * Requires window.ChainDotPopoverFixture (chain-dot-popover.fixture.js).
 */
(function (global) {
  "use strict";

  var COL_GAP = 140;
  var ROW_GAP = 78;
  var NODE_W = 118;
  var NODE_H = 36;

  function escapeHtml(text) {
    return String(text)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function fixture() {
    return global.ChainDotPopoverFixture || null;
  }

  function findChain(rootId) {
    var fx = fixture();
    if (!fx || !Array.isArray(fx.chains)) {
      return null;
    }
    for (var i = 0; i < fx.chains.length; i += 1) {
      if (fx.chains[i].rootId === rootId) {
        return fx.chains[i];
      }
    }
    return null;
  }

  function childrenOf(nodes, parentId) {
    return (nodes || []).filter(function (n) {
      return n.parentId === parentId;
    });
  }

  function collectEdges(chain) {
    var nodes = chain.nodes || [];
    var edges = [];
    var seen = Object.create(null);

    function pushEdge(from, to, kind) {
      if (!from || !to) {
        return;
      }
      var key = from + "->" + to + ":" + (kind || "hop");
      if (seen[key]) {
        return;
      }
      seen[key] = true;
      edges.push({ from: from, to: to, kind: kind || "hop" });
    }

    nodes.forEach(function (node) {
      if (node.parentId) {
        pushEdge(node.parentId, node.id, "hop");
      }
      (node.links || []).forEach(function (link) {
        pushEdge(node.id, link.to, link.kind || "hop");
      });
    });
    (chain.edges || []).forEach(function (edge) {
      pushEdge(edge.from, edge.to, edge.kind || "hop");
    });
    return edges;
  }

  function collectRanges(fx) {
    fx = fx || fixture();
    var byKey = Object.create(null);
    (fx.chains || []).forEach(function (chain) {
      (chain.tokenRanges || []).forEach(function (range) {
        var key = range.start + ":" + range.end + ":" + chain.rootId;
        byKey[key] = {
          start: range.start,
          end: range.end,
          rootId: chain.rootId,
          name: range.name || chain.rootId
        };
      });
    });
    return Object.keys(byKey).map(function (k) { return byKey[k]; })
      .sort(function (a, b) { return a.start - b.start || b.end - a.end; });
  }

  function renderCodeHtml(fx, options) {
    fx = fx || fixture();
    options = options || {};
    var text = fx.sourceText || "";
    var ranges = collectRanges(fx);
    var html = "";
    var cursor = 0;
    ranges.forEach(function (range) {
      if (range.start < cursor) {
        return;
      }
      html += escapeHtml(text.slice(cursor, range.start));
      var extra = options.tokenClass ? (" " + options.tokenClass) : "";
      html += '<span class="chain-token' + extra + '" tabindex="0" data-root-id="'
        + escapeHtml(range.rootId)
        + '" data-start="' + range.start + '">'
        + escapeHtml(text.slice(range.start, range.end))
        + "</span>";
      cursor = range.end;
    });
    html += escapeHtml(text.slice(cursor));
    return html;
  }

  function computeBranchLayout(chain) {
    var nodes = chain.nodes || [];
    var roots = nodes.filter(function (n) { return n.role === "root"; });
    if (!roots.length) {
      roots = nodes.filter(function (n) { return !n.parentId; });
    }

    var positions = Object.create(null);
    var visited = Object.create(null);
    var nextLeafCol = 0;

    function place(node, depth) {
      if (!node || visited[node.id]) {
        return;
      }
      visited[node.id] = true;
      var kids = childrenOf(nodes, node.id);
      if (!kids.length) {
        positions[node.id] = { col: nextLeafCol, depth: depth };
        nextLeafCol += 1;
        return;
      }
      kids.forEach(function (kid) {
        place(kid, depth + 1);
      });
      var first = positions[kids[0].id];
      var last = positions[kids[kids.length - 1].id];
      positions[node.id] = {
        col: (first.col + last.col) / 2,
        depth: depth
      };
    }

    roots.forEach(function (root) {
      place(root, 0);
    });
    nodes.forEach(function (n) {
      if (!positions[n.id]) {
        place(n, 0);
      }
    });

    var maxCol = 0;
    var maxDepth = 0;
    Object.keys(positions).forEach(function (id) {
      maxCol = Math.max(maxCol, positions[id].col);
      maxDepth = Math.max(maxDepth, positions[id].depth);
    });

    return {
      positions: positions,
      colCount: Math.max(1, maxCol + 1),
      maxDepth: maxDepth
    };
  }

  function chainComplexity(chain) {
    var nodes = chain.nodes || [];
    var root = nodes.filter(function (n) { return n.role === "root"; })[0];
    var branchKids = root ? childrenOf(nodes, root.id).length : 0;
    var hasCycle = nodes.some(function (n) {
      return (n.links || []).some(function (l) { return l.kind === "cycle"; });
    }) || (chain.edges || []).some(function (e) { return e.kind === "cycle"; });
    var hops = nodes.filter(function (n) { return n.role === "hop"; }).length;
    return {
      branchKids: branchKids,
      hasCycle: hasCycle,
      hops: hops,
      isSimple: branchKids <= 1 && !hasCycle && hops <= 2
    };
  }

  function shortLabel(node) {
    var label = node.label || node.id || "";
    if (label.length > 22) {
      return label.slice(0, 20) + "…";
    }
    return label;
  }

  function edgePath(from, to, kind, index) {
    var dx = to.x - from.x;
    var dy = to.y - from.y;
    if (kind === "cycle") {
      var side = (index % 2 === 0) ? 1 : -1;
      var bulge = Math.max(40, Math.abs(dx) * 0.25 + 36) * side;
      return "M " + from.x + " " + from.y
        + " C " + (from.x + bulge) + " " + (from.y + dy * 0.2)
        + ", " + (to.x + bulge) + " " + (to.y - dy * 0.2)
        + ", " + to.x + " " + to.y;
    }
    var midX = from.x + dx / 2;
    return "M " + from.x + " " + from.y
      + " C " + midX + " " + from.y
      + ", " + midX + " " + to.y
      + ", " + to.x + " " + to.y;
  }

  /**
   * Left→right layered DAG into an SVG element.
   * options: { selectedId, onNodeClick, showCodePreview }
   */
  function renderDagSvg(svgEl, chain, options) {
    options = options || {};
    if (!svgEl || !chain) {
      return null;
    }
    var layout = computeBranchLayout(chain);
    var nodes = chain.nodes || [];
    var byId = Object.create(null);
    nodes.forEach(function (n) { byId[n.id] = n; });
    var edges = collectEdges(chain);
    var pad = 24;
    var width = pad * 2 + (layout.maxDepth + 1) * COL_GAP;
    var height = pad * 2 + layout.colCount * ROW_GAP;

    // Remap: depth → X (L→R), col → Y
    var coords = Object.create(null);
    Object.keys(layout.positions).forEach(function (id) {
      var p = layout.positions[id];
      coords[id] = {
        x: pad + p.depth * COL_GAP + NODE_W / 2,
        y: pad + p.col * ROW_GAP + NODE_H / 2,
        left: pad + p.depth * COL_GAP,
        top: pad + p.col * ROW_GAP
      };
    });

    var parts = [];
    parts.push('<defs><marker id="arrowHead" markerWidth="8" markerHeight="6" refX="7" refY="3" orient="auto"><path d="M0,0 L8,3 L0,6 Z" fill="#4b6a8a"/></marker>');
    parts.push('<marker id="arrowCycle" markerWidth="8" markerHeight="6" refX="7" refY="3" orient="auto"><path d="M0,0 L8,3 L0,6 Z" fill="#c47a2a"/></marker></defs>');

    edges.forEach(function (edge, index) {
      var a = coords[edge.from];
      var b = coords[edge.to];
      if (!a || !b) {
        return;
      }
      var from = { x: a.x + NODE_W / 2 - 4, y: a.y };
      var to = { x: b.x - NODE_W / 2 + 4, y: b.y };
      // For L→R: from right of node to left of node
      from = { x: a.left + NODE_W, y: a.top + NODE_H / 2 };
      to = { x: b.left, y: b.top + NODE_H / 2 };
      var isCycle = edge.kind === "cycle";
      var cls = isCycle ? "dag-edge is-cycle" : "dag-edge";
      var marker = isCycle ? "url(#arrowCycle)" : "url(#arrowHead)";
      parts.push(
        '<path class="' + cls + '" marker-end="' + marker + '" d="'
          + edgePath(from, to, edge.kind, index) + '" />'
      );
    });

    nodes.forEach(function (node) {
      var c = coords[node.id];
      if (!c) {
        return;
      }
      var isRoot = node.role === "root";
      var selected = options.selectedId && options.selectedId === node.id;
      var cls = "dag-node" + (isRoot ? " is-root" : "") + (selected ? " is-selected" : "");
      var title = escapeHtml((node.label || node.id) + "\n" + (node.code || ""));
      parts.push(
        '<g class="' + cls + '" data-node-id="' + escapeHtml(node.id) + '" transform="translate('
          + c.left + "," + c.top + ')">'
          + '<title>' + title + "</title>"
          + '<rect width="' + NODE_W + '" height="' + NODE_H
          + '" rx="8" ry="8"></rect>'
          + '<text x="' + (NODE_W / 2) + '" y="' + (NODE_H / 2 + 4)
          + '" text-anchor="middle">' + escapeHtml(shortLabel(node)) + "</text>"
          + "</g>"
      );
    });

    svgEl.setAttribute("viewBox", "0 0 " + width + " " + height);
    svgEl.setAttribute("width", String(width));
    svgEl.setAttribute("height", String(height));
    svgEl.innerHTML = parts.join("");

    if (typeof options.onNodeClick === "function") {
      svgEl.querySelectorAll(".dag-node").forEach(function (g) {
        g.addEventListener("click", function (event) {
          event.stopPropagation();
          options.onNodeClick(g.getAttribute("data-node-id"), byId[g.getAttribute("data-node-id")]);
        });
      });
    }

    return { width: width, height: height, coords: coords, layout: layout };
  }

  function nodeDetailHtml(node) {
    if (!node) {
      return '<p class="muted">No node.</p>';
    }
    return '<div class="node-detail">'
      + "<strong>" + escapeHtml(node.label || node.id) + "</strong>"
      + '<span class="pill">' + escapeHtml(node.role || "node") + "</span>"
      + '<pre class="code-snip">' + escapeHtml(node.code || "") + "</pre>"
      + "</div>";
  }

  function pathChipsHtml(chain) {
    var nodes = chain.nodes || [];
    var root = nodes.filter(function (n) { return n.role === "root"; })[0];
    if (!root) {
      return "";
    }
    var edges = collectEdges(chain);
    var kids = childrenOf(nodes, root.id);
    var html = '<div class="chip-row" data-root-id="' + escapeHtml(chain.rootId) + '">';
    html += '<button type="button" class="chip is-root" data-node-id="'
      + escapeHtml(root.id) + '">' + escapeHtml(root.label || root.id) + "</button>";

    function addBranch(startNode, prefix) {
      var path = [startNode];
      var cur = startNode;
      var guard = 0;
      while (guard < 20) {
        guard += 1;
        var next = childrenOf(nodes, cur.id)[0];
        if (!next) {
          break;
        }
        path.push(next);
        cur = next;
      }
      html += '<span class="chip-branch">';
      path.forEach(function (n, idx) {
        if (idx === 0 && prefix) {
          html += '<span class="chip-sep">→</span>';
        } else if (idx > 0) {
          html += '<span class="chip-sep">→</span>';
        }
        html += '<button type="button" class="chip" data-node-id="'
          + escapeHtml(n.id) + '">' + escapeHtml(shortLabel(n)) + "</button>";
      });
      var hasCycle = (startNode.links || []).concat(
        path.reduce(function (acc, n) { return acc.concat(n.links || []); }, [])
      ).some(function (l) { return l.kind === "cycle"; });
      // mark cycle on last if any node in path has cycle link
      path.forEach(function (n) {
        (n.links || []).forEach(function (l) {
          if (l.kind === "cycle") {
            html += '<span class="chip-sep cycle">↺</span>';
            html += '<button type="button" class="chip is-cycle" data-node-id="'
              + escapeHtml(l.to) + '">' + escapeHtml(l.to) + "</button>";
          }
        });
      });
      html += "</span>";
    }

    if (!kids.length) {
      html += "</div>";
      return html;
    }
    kids.forEach(function (kid) {
      addBranch(kid, true);
    });
    html += "</div>";
    return html;
  }

  global.ChainLineage = {
    escapeHtml: escapeHtml,
    fixture: fixture,
    findChain: findChain,
    childrenOf: childrenOf,
    collectEdges: collectEdges,
    collectRanges: collectRanges,
    renderCodeHtml: renderCodeHtml,
    computeBranchLayout: computeBranchLayout,
    chainComplexity: chainComplexity,
    renderDagSvg: renderDagSvg,
    nodeDetailHtml: nodeDetailHtml,
    pathChipsHtml: pathChipsHtml,
    COL_GAP: COL_GAP,
    ROW_GAP: ROW_GAP
  };
})(typeof window !== "undefined" ? window : globalThis);
