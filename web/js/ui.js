(function (ns) {
  "use strict";

  const utils = ns.utils;

  const state = {
    model: null,
    selectedKey: null,
  };

  function $(id) {
    return document.getElementById(id);
  }

  function setStatus(message, isError) {
    const el = $("statusBar");
    el.textContent = message;
    el.style.color = isError ? "#fecaca" : "";
  }

  function setActiveTab(name) {
    const tabs = document.querySelectorAll(".tab");
    const panels = document.querySelectorAll(".tab-panel");
    for (const t of tabs) t.classList.toggle("is-active", t.dataset.tab === name);
    for (const p of panels) p.classList.toggle("is-active", p.id === `tab-${name}`);
  }

  function getDisplayObjects(model) {
    const out = [];
    out.push({
      key: "PROGRAM",
      kind: "PROGRAM",
      name: "(Globals)",
      depth: "",
      isDefined: true,
      isInCycle: false,
      paramsCount: 0,
      localDataCount: model.globalData.length,
      localConstCount: model.globalConstants.length,
      writesCount: 0,
    });

    const nodes = Array.from(model.nodes.values()).sort((a, b) => {
      if (a.depth !== b.depth) return a.depth - b.depth;
      const ka = a.kind === "EVENT" ? 0 : a.kind === "FORM" ? 1 : 2;
      const kb = b.kind === "EVENT" ? 0 : b.kind === "FORM" ? 1 : 2;
      if (ka !== kb) return ka - kb;
      return a.name.localeCompare(b.name);
    });

    for (const n of nodes) out.push(n);
    return out;
  }

  function renderObjectsTable() {
    const model = state.model;
    const tbody = $("objectsTable").querySelector("tbody");
    tbody.innerHTML = "";
    if (!model) return;

    const filter = String($("objectSearch").value || "").trim().toLowerCase();
    const objects = getDisplayObjects(model);
    const filtered = filter
      ? objects.filter((o) => `${o.kind} ${o.name}`.toLowerCase().includes(filter) || String(o.key).toLowerCase().includes(filter))
      : objects;

    for (const obj of filtered) {
      const tr = document.createElement("tr");
      tr.dataset.key = obj.key;
      if (obj.key === state.selectedKey) tr.classList.add("is-selected");

      const isDefined = obj.isDefined !== false;
      const cycle = obj.isInCycle === true;
      const definedBadge = isDefined ? '<span class="badge badge-ok">Yes</span>' : '<span class="badge">No</span>';
      const cycleBadge = cycle ? '<span class="badge badge-danger">Yes</span>' : '<span class="badge">No</span>';

      tr.innerHTML = `
        <td>${utils.escapeHtml(obj.kind)}</td>
        <td>${utils.escapeHtml(obj.name)}</td>
        <td>${utils.escapeHtml(String(obj.depth ?? ""))}</td>
        <td>${utils.escapeHtml(String(obj.paramsCount ?? obj.params?.length ?? 0))}</td>
        <td>${utils.escapeHtml(String(obj.localDataCount ?? obj.localData?.length ?? 0))}</td>
        <td>${utils.escapeHtml(String(obj.localConstCount ?? obj.localConstants?.length ?? 0))}</td>
        <td>${utils.escapeHtml(String(obj.writesCount ?? obj.writes?.length ?? 0))}</td>
        <td>${definedBadge}</td>
        <td>${cycleBadge}</td>
      `;

      tr.addEventListener("click", () => selectObject(obj.key, true));
      tbody.appendChild(tr);
    }
  }

  function sourceLink(label, sourceRef) {
    if (!sourceRef || !sourceRef.startLine) return utils.escapeHtml(label);
    const start = Number(sourceRef.startLine || 0);
    const end = Number(sourceRef.endLine || sourceRef.startLine || 0);
    return `<a href="#" class="source-link" data-start="${start}" data-end="${end}">${utils.escapeHtml(label)}</a>`;
  }

  function renderProgramDetails(model) {
    const globalsData = model.globalData
      .map((d) => `<li>${sourceLink(`${d.variableName} TYPE ${d.dataType || "?"} ${d.description ? "- " + d.description : ""}`, d.sourceRef)}</li>`)
      .join("");
    const globalsConst = model.globalConstants
      .map((c) =>
        `<li>${sourceLink(
          `${c.variableName} TYPE ${c.dataType || "?"}${c.value ? " VALUE " + c.value : ""} ${c.description ? "- " + c.description : ""}`,
          c.sourceRef,
        )}</li>`,
      )
      .join("");

    return `
      <h3>PROGRAM (Globals)</h3>
      <div class="meta">Global DATA: ${model.globalData.length} • Global CONSTANTS: ${model.globalConstants.length}</div>
      <div class="section">
        <div class="section__title">Global DATA</div>
        <ul class="list">${globalsData || "<li>(none)</li>"}</ul>
      </div>
      <div class="section">
        <div class="section__title">Global CONSTANTS</div>
        <ul class="list">${globalsConst || "<li>(none)</li>"}</ul>
      </div>
    `;
  }

  function renderRoutineDetails(r) {
    const model = state.model;
    const lineInfo = r.sourceRef?.startLine ? `Lines ${r.sourceRef.startLine}-${r.sourceRef.endLine || r.sourceRef.startLine}` : "Lines ?";

    const params = r.params
      .map((p) => {
        const dt = p.dataType ? ` TYPE ${p.dataType}` : "";
        const desc = p.description ? ` - ${p.description}` : "";
        return `<li>${sourceLink(`${p.kind} ${p.name}${dt}${desc}`, p.sourceRef || r.sourceRef)}</li>`;
      })
      .join("");

    const localsData = r.localData
      .map((d) => `<li>${sourceLink(`${d.variableName}${d.dataType ? " TYPE " + d.dataType : ""}${d.description ? " - " + d.description : ""}`, d.sourceRef)}</li>`)
      .join("");

    const localsConst = r.localConstants
      .map((c) =>
        `<li>${sourceLink(
          `${c.variableName}${c.dataType ? " TYPE " + c.dataType : ""}${c.value ? " VALUE " + c.value : ""}${c.description ? " - " + c.description : ""}`,
          c.sourceRef,
        )}</li>`,
      )
      .join("");

    const calls = r.calls
      .map((e) => {
        const callee = model.nodes.get(e.toKey);
        const name = callee ? `${callee.kind} ${callee.name}` : e.targetName;
        return `<li>${sourceLink(`PERFORM ${name}`, e.sourceRef)}${e.isInCycle ? ' <span class="badge badge-danger">cycle</span>' : ""}</li>`;
      })
      .join("");

    const calledBy = r.calledBy
      .map((e) => {
        const caller = model.nodes.get(e.fromKey);
        const name = caller ? `${caller.kind} ${caller.name}` : e.fromKey;
        return `<li>${sourceLink(`Called by ${name}`, e.sourceRef)}</li>`;
      })
      .join("");

    const writes = r.writes.map((w) => `<li>${sourceLink(`${w.variableName}  ⇐  ${w.statement}`, w.sourceRef)}</li>`).join("");

    const desc = r.description
      ? `<div class="section"><div class="section__title">Description</div><div>${utils.escapeHtml(r.description)}</div></div>`
      : "";

    return `
      <h3>${utils.escapeHtml(r.kind)} ${utils.escapeHtml(r.name)}</h3>
      <div class="meta">${utils.escapeHtml(lineInfo)} • Defined: ${r.isDefined ? "Yes" : "No"} • Cycle: ${r.isInCycle ? "Yes" : "No"} • Depth: ${r.depth}</div>
      ${desc}
      <div class="section">
        <div class="section__title">Parameters</div>
        <ul class="list">${params || "<li>(none)</li>"}</ul>
      </div>
      <div class="section">
        <div class="section__title">Local DATA</div>
        <ul class="list">${localsData || "<li>(none)</li>"}</ul>
      </div>
      <div class="section">
        <div class="section__title">Local CONSTANTS</div>
        <ul class="list">${localsConst || "<li>(none)</li>"}</ul>
      </div>
      <div class="section">
        <div class="section__title">Writes</div>
        <ul class="list">${writes || "<li>(none)</li>"}</ul>
      </div>
      <div class="section">
        <div class="section__title">Calls (PERFORM)</div>
        <ul class="list">${calls || "<li>(none)</li>"}</ul>
      </div>
      <div class="section">
        <div class="section__title">Called by</div>
        <ul class="list">${calledBy || "<li>(none)</li>"}</ul>
      </div>
    `;
  }

  function renderDetails() {
    const model = state.model;
    const el = $("objectDetails");
    if (!model) {
      el.textContent = "Analyze to see details.";
      el.classList.add("empty");
      return;
    }

    if (!state.selectedKey) {
      el.textContent = "Select an object to see details.";
      el.classList.add("empty");
      return;
    }

    el.classList.remove("empty");
    if (state.selectedKey === "PROGRAM") {
      el.innerHTML = renderProgramDetails(model);
      return;
    }

    const r = model.nodes.get(state.selectedKey);
    if (!r) {
      el.textContent = "Object not found.";
      return;
    }

    el.innerHTML = renderRoutineDetails(r);
  }

  function highlightSource(startLine, endLine) {
    const model = state.model;
    if (!model) return;
    const ta = $("abapInput");

    const startIdx = Math.max(1, Number(startLine || 1));
    const endIdx = Math.max(startIdx, Number(endLine || startIdx));

    const offsets = model.lineStartOffsets;
    const start = offsets[startIdx - 1] ?? 0;
    const end = offsets[endIdx] ?? ta.value.length;

    ta.focus();
    try {
      ta.setSelectionRange(start, end);
    } catch (_) {
      // ignore
    }

    const approxLineHeight = 16;
    ta.scrollTop = Math.max(0, (startIdx - 3) * approxLineHeight);
  }

  function selectObject(key, syncTrace) {
    state.selectedKey = key;
    renderObjectsTable();
    renderDetails();

    if (syncTrace && state.model && key !== "PROGRAM") {
      const sel = $("traceSubroutine");
      sel.value = key;
      updateTraceVariables();
    }
  }

  function renderDiagram() {
    const model = state.model;
    const host = $("diagramHost");
    host.innerHTML = "";
    if (!model) return;

    ns.diagram.render(model, host, {
      onNodeClick: (key) => {
        selectObject(key, true);
        setActiveTab("objects");
      },
    });
  }

  function goToTrace(routineKey, varName) {
    const model = state.model;
    if (!model) return;
    setActiveTab("trace");
    const subSel = $("traceSubroutine");
    subSel.value = routineKey;
    updateTraceVariables();
    const varSel = $("traceVariable");
    varSel.value = varName;
    runTrace();
  }

  function renderSequenceControls() {
    const model = state.model;
    const sel = $("seqRoot");
    sel.innerHTML = "";
    if (!model) return;

    const items = ns.sequence.getStartCandidates(model);
    for (const it of items) {
      const opt = document.createElement("option");
      opt.value = it.key;
      opt.textContent = it.label;
      sel.appendChild(opt);
    }

    const preferred =
      items.find((x) => x.key === ns.sequence.PROGRAM_ALL_KEY) ||
      items.find((x) => x.key === "EVENT:START-OF-SELECTION") ||
      items.find((x) => x.key === "EVENT:INITIALIZATION") ||
      items[0] ||
      null;
    if (preferred) sel.value = preferred.key;
  }

  function renderSequence() {
    const model = state.model;
    const host = $("sequenceHost");
    if (!model) {
      host.textContent = "Analyze to see sequence.";
      host.classList.add("empty");
      return;
    }

    host.classList.remove("empty");
    const startKey = $("seqRoot").value;
    const maxSteps = Number($("seqMaxSteps").value || 200);
    const seq = ns.sequence.build(model, startKey, { maxSteps });
    ns.sequence.render(host, model, seq, {
      onParamClick: ({ routineKey, varName }) => goToTrace(routineKey, varName),
      onRowClick: ({ routineKey }) => selectObject(routineKey, true),
    });
  }

  function renderJson() {
    const out = $("jsonOutput");
    const model = state.model;
    if (!model) {
      out.textContent = "Analyze to see JSON.";
      out.classList.add("empty");
      return;
    }
    out.classList.remove("empty");
    out.textContent = JSON.stringify(model.serialize(), null, 2);
  }

  function renderTraceSubroutines() {
    const model = state.model;
    const sel = $("traceSubroutine");
    sel.innerHTML = "";
    if (!model) return;

    const nodes = Array.from(model.nodes.values()).sort((a, b) => {
      const ka = a.kind === "EVENT" ? 0 : a.kind === "FORM" ? 1 : 2;
      const kb = b.kind === "EVENT" ? 0 : b.kind === "FORM" ? 1 : 2;
      if (ka !== kb) return ka - kb;
      return a.name.localeCompare(b.name);
    });

    for (const n of nodes) {
      const opt = document.createElement("option");
      opt.value = n.key;
      opt.textContent = `${n.kind} ${n.name}${n.isDefined ? "" : " (external)"}`;
      sel.appendChild(opt);
    }

    if (nodes.length > 0) sel.value = nodes[0].key;
    updateTraceVariables();
  }

  function updateTraceVariables() {
    const model = state.model;
    const subKey = $("traceSubroutine").value;
    const sel = $("traceVariable");
    sel.innerHTML = "";
    if (!model || !subKey) return;

    const vars = ns.lineage.getVariablesForRoutine(model, subKey);
    for (const v of vars) {
      const opt = document.createElement("option");
      opt.value = v.name;
      opt.textContent = `${v.name} (${v.scope})`;
      sel.appendChild(opt);
    }
  }

  function renderTraceResults(result) {
    const el = $("traceResults");
    if (!result || result.error) {
      el.textContent = result?.error || "Trace failed.";
      el.classList.add("empty");
      return;
    }

    const root = result.root;
    if (!root) {
      el.textContent = "No results.";
      el.classList.add("empty");
      return;
    }

    el.classList.remove("empty");

    function renderNode(node, indent) {
      const pad = indent * 14;
      const header = `<div class="trace-node__header" style="padding-left:${pad}px">${sourceLink(
        `${node.routineKind} ${node.routineName} (var ${node.varName})`,
        node.sourceRef,
      )}${node.cycle ? ' <span class="badge badge-danger">cycle</span>' : ""}${node.truncated ? ' <span class="badge">truncated</span>' : ""}</div>`;

      const decl = node.resolution?.decl;
      const declText = decl?.description ? ` - ${utils.escapeHtml(decl.description)}` : "";
      const scope = node.resolution?.scope ? `${node.resolution.scope}` : "unknown";
      const declLine = decl?.sourceRef?.startLine ? ` (line ${decl.sourceRef.startLine})` : "";
      let declStmt = "";
      if (decl && decl.declKind) {
        declStmt = `${decl.declKind} ${decl.variableName}${decl.dataType ? ` TYPE ${decl.dataType}` : ""}${decl.value ? ` VALUE ${decl.value}` : ""}`;
      } else if (decl && decl.kind && decl.name) {
        declStmt = `${decl.kind} ${decl.name}${decl.dataType ? ` TYPE ${decl.dataType}` : ""}`;
      }
      const declStmtText = declStmt ? ` — ${utils.escapeHtml(declStmt)}` : "";
      const meta = `<div class="trace-node__meta" style="padding-left:${pad}px">Scope: ${utils.escapeHtml(scope)}${utils.escapeHtml(declLine)}${declText}${declStmtText}</div>`;

      const writes = (node.writes || [])
        .map((w) => `<li>${sourceLink(`${w.variableName} ⇐ ${w.statement}`, w.sourceRef)}</li>`)
        .join("");
      const writesHtml = `<ul class="list trace-node__writes" style="margin-left:${pad}px">${writes || "<li>(no writes)</li>"}</ul>`;

      const children = (node.calls || [])
        .map((c) => {
          const dir = c.direction === "fromCaller" ? "from" : "via";
          const target = c.direction === "fromCaller" ? node.routineName : c.child.routineName;
          const mapText = `<div class="trace-node__call" style="padding-left:${pad + 14}px">${sourceLink(
            `${dir} PERFORM ${target}: ${c.fromVar} -> ${c.toVar} (${c.mappingKind})`,
            c.edge.sourceRef,
          )}</div>`;
          return mapText + renderNode(c.child, indent + 1);
        })
        .join("");

      return `<div class="trace-node">${header}${meta}${writesHtml}${children}</div>`;
    }

    el.innerHTML = renderNode(root, 0);
  }

  function runTrace() {
    const model = state.model;
    if (!model) return;
    const subKey = $("traceSubroutine").value;
    const varName = $("traceVariable").value;
    const result = ns.lineage.traceVariable(model, subKey, varName, { maxNodes: 400 });
    renderTraceResults(result);
  }

  function analyze() {
    const input = $("abapInput").value;
    if (!String(input || "").trim()) {
      setStatus("Paste ABAP code first.", true);
      return;
    }

    try {
      const model = ns.parser.parseProgram(input);
      state.model = model;
      state.selectedKey = "PROGRAM";

      renderObjectsTable();
      renderDetails();
      renderDiagram();
      renderSequenceControls();
      if (document.getElementById("tab-sequence").classList.contains("is-active")) {
        renderSequence();
      } else {
        const host = $("sequenceHost");
        host.textContent = "Open the Sequence tab (or click Render) to render.";
        host.classList.add("empty");
      }
      renderJson();
      renderTraceSubroutines();
      setStatus(`Parsed ${model.nodes.size} objects, ${model.edges.length} PERFORM calls.`, false);
    } catch (err) {
      console.error(err);
      setStatus(String(err?.message || err), true);
    }
  }

  function init() {
    document.addEventListener("click", (ev) => {
      const t = ev.target;
      if (!(t instanceof HTMLElement)) return;
      if (!t.classList.contains("source-link")) return;
      ev.preventDefault();
      highlightSource(t.dataset.start, t.dataset.end);
    });

    document.querySelectorAll(".tab").forEach((tab) => {
      tab.addEventListener("click", () => {
        const name = tab.dataset.tab;
        setActiveTab(name);
        if (name === "sequence") {
          requestAnimationFrame(() => renderSequence());
        }
      });
    });

    $("btnAnalyze").addEventListener("click", analyze);
    $("btnLoadSample").addEventListener("click", () => {
      $("abapInput").value = ns.sampleCode || "";
      setStatus("Sample loaded. Click Analyze.", false);
    });
    $("btnClear").addEventListener("click", () => {
      $("abapInput").value = "";
      state.model = null;
      state.selectedKey = null;
      $("objectsTable").querySelector("tbody").innerHTML = "";
      $("objectDetails").textContent = "Analyze to see details.";
      $("objectDetails").classList.add("empty");
      $("diagramHost").innerHTML = "";
      $("sequenceHost").textContent = "Analyze to see sequence.";
      $("sequenceHost").classList.add("empty");
      $("jsonOutput").textContent = "Analyze to see JSON.";
      $("jsonOutput").classList.add("empty");
      $("traceResults").textContent = "Select a subroutine + variable and run trace.";
      $("traceResults").classList.add("empty");
      $("traceSubroutine").innerHTML = "";
      $("traceVariable").innerHTML = "";
      $("seqRoot").innerHTML = "";
      setStatus("Cleared.", false);
    });

    $("objectSearch").addEventListener("input", () => renderObjectsTable());
    $("traceSubroutine").addEventListener("change", updateTraceVariables);
    $("btnTrace").addEventListener("click", runTrace);
    $("btnSeqRender").addEventListener("click", renderSequence);
    $("seqRoot").addEventListener("change", renderSequence);

    if (!$("abapInput").value.trim() && ns.sampleCode) {
      $("abapInput").value = ns.sampleCode;
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})(window.AbapFlow);
