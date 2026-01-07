(function (ns) {
  "use strict";

  function build(model) {
    for (const node of model.nodes.values()) {
      node.calls = [];
      node.calledBy = [];
      node.depth = null;
      node.isInCycle = false;
    }

    for (const edge of model.edges) {
      const from = model.nodes.get(edge.fromKey);
      const to = model.nodes.get(edge.toKey);
      if (!from || !to) continue;
      from.calls.push(edge);
      to.calledBy.push(edge);
      edge.isInCycle = false;
    }

    detectCycles(model);
    computeDepths(model);
  }

  function detectCycles(model) {
    let index = 0;
    const stack = [];
    const indices = new Map();
    const lowlinks = new Map();
    const onStack = new Set();
    const sccId = new Map();
    const sccs = [];

    function strongconnect(vKey) {
      indices.set(vKey, index);
      lowlinks.set(vKey, index);
      index++;
      stack.push(vKey);
      onStack.add(vKey);

      const v = model.nodes.get(vKey);
      for (const edge of v.calls) {
        const wKey = edge.toKey;
        if (!indices.has(wKey)) {
          strongconnect(wKey);
          lowlinks.set(vKey, Math.min(lowlinks.get(vKey), lowlinks.get(wKey)));
        } else if (onStack.has(wKey)) {
          lowlinks.set(vKey, Math.min(lowlinks.get(vKey), indices.get(wKey)));
        }
      }

      if (lowlinks.get(vKey) === indices.get(vKey)) {
        const comp = [];
        let w;
        do {
          w = stack.pop();
          onStack.delete(w);
          comp.push(w);
        } while (w !== vKey);
        const id = sccs.length;
        for (const k of comp) sccId.set(k, id);
        sccs.push(comp);
      }
    }

    for (const key of model.nodes.keys()) {
      if (!indices.has(key)) strongconnect(key);
    }

    for (let i = 0; i < sccs.length; i++) {
      const comp = sccs[i];
      if (comp.length > 1) {
        for (const key of comp) model.nodes.get(key).isInCycle = true;
        continue;
      }

      const key = comp[0];
      const node = model.nodes.get(key);
      const hasSelfLoop = node.calls.some((e) => e.toKey === key);
      if (hasSelfLoop) node.isInCycle = true;
    }

    for (const edge of model.edges) {
      const fromId = sccId.get(edge.fromKey);
      const toId = sccId.get(edge.toKey);
      if (fromId != null && toId != null && fromId === toId) {
        const comp = sccs[fromId] || [];
        if (comp.length > 1 || edge.fromKey === edge.toKey) edge.isInCycle = true;
      }
    }
  }

  function computeDepths(model) {
    const nodes = Array.from(model.nodes.values());
    const roots = nodes.filter((n) => n.kind === "EVENT" || n.calledBy.length === 0);
    const queue = [];

    for (const r of roots) {
      if (r.depth == null) r.depth = 0;
      queue.push(r);
    }

    while (queue.length > 0) {
      const cur = queue.shift();
      const nextDepth = (cur.depth ?? 0) + 1;
      for (const edge of cur.calls) {
        const callee = model.nodes.get(edge.toKey);
        if (!callee) continue;
        if (callee.depth == null || nextDepth < callee.depth) {
          callee.depth = nextDepth;
          queue.push(callee);
        }
      }
    }

    for (const n of nodes) {
      if (n.depth == null) n.depth = 0;
    }
  }

  ns.graph = { build, computeDepths };
})(window.AbapFlow);

