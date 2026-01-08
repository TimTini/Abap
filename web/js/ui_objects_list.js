(function (ns) {
  "use strict";

  const utils = ns.utils;
  const ui = ns.ui;
  const state = ui.state;

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
      assignmentsCount: 0,
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

  function selectObject(key, syncTrace) {
    state.selectedKey = key;
    if (typeof ui.renderObjectsTable === "function") ui.renderObjectsTable();
    if (typeof ui.renderDetails === "function") ui.renderDetails();

    if (syncTrace && state.model && key !== "PROGRAM") {
      const sel = ui.$("traceSubroutine");
      if (sel) sel.value = key;
      if (typeof ui.updateTraceVariables === "function") ui.updateTraceVariables();
    }
  }

  function renderObjectsTable() {
    const model = state.model;
    const table = ui.$("objectsTable");
    const tbody = table ? table.querySelector("tbody") : null;
    if (!tbody) return;
    tbody.innerHTML = "";
    if (!model) return;

    const filter = String(ui.$("objectSearch")?.value || "").trim().toLowerCase();
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
        <td>${utils.escapeHtml(String(obj.assignmentsCount ?? obj.assignments?.length ?? 0))}</td>
        <td>${definedBadge}</td>
        <td>${cycleBadge}</td>
      `;

      tr.addEventListener("click", () => selectObject(obj.key, true));
      tbody.appendChild(tr);
    }
  }

  ui.selectObject = selectObject;
  ui.renderObjectsTable = renderObjectsTable;
})(window.AbapFlow);

