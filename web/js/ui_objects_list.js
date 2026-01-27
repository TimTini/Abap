(function (ns) {
  "use strict";

  const utils = ns.utils;
  const desc = ns.desc;
  const ui = ns.ui;
  const state = ui.state;

  function getRoutineDescription(obj, model) {
    if (!obj) return "";
    if (obj.key === "PROGRAM") {
      const u = String(model?.userDescription || "").trim();
      const c = String(model?.description || "").trim();
      return u || c || "";
    }

    const user = String(obj.userDescription || "").trim();
    const code = String(obj.description || "").trim();
    return user || code || "";
  }

  function getParamDescription(param) {
    const user = String(param?.userDescription || "").trim();
    const code = String(param?.description || "").trim();
    return user || code || "";
  }

  function buildParamIndexMap(routine) {
    const params = Array.isArray(routine?.params) ? routine.params : [];
    const counters = { TABLES: 0, USING: 0, CHANGING: 0, RAISING: 0 };
    const out = new Map();

    for (const p of params) {
      const kind = String(p?.kind || "").trim().toUpperCase();
      const nameLower = String(p?.name || "").trim().toLowerCase();
      if (!nameLower) continue;
      const idx = Number(counters[kind] || 0);
      counters[kind] = idx + 1;
      out.set(`${kind}:${nameLower}`, idx);
    }

    return out;
  }

  function firstSourceForParam(model, routine, param, incomingEdges, paramIndexMap) {
    const kindUpper = String(param?.kind || "").trim().toUpperCase();
    const nameLower = String(param?.name || "").trim().toLowerCase();
    if (!kindUpper || !nameLower) return null;

    if (kindUpper === "RAISING") {
      return {
        callerKey: "",
        callerName: "",
        actualExpr: "",
        originSourceRef: null,
        descText: "",
        statusText: "(RAISING)",
      };
    }

    const argsKey = kindUpper === "TABLES" ? "tables" : kindUpper === "USING" ? "using" : kindUpper === "CHANGING" ? "changing" : "";
    if (!argsKey) return null;

    const idx = paramIndexMap?.get ? paramIndexMap.get(`${kindUpper}:${nameLower}`) : -1;
    if (!Number.isFinite(idx) || idx < 0) return null;

    const edges = Array.isArray(incomingEdges) ? incomingEdges : [];
    for (const e of edges) {
      const actual = String(e?.args?.[argsKey]?.[idx] || "").trim();
      if (!actual) continue;

      const callerKey = String(e?.fromKey || "").trim();
      const caller = callerKey && model?.nodes?.get ? model.nodes.get(callerKey) : null;
      const callerName = caller ? `${caller.kind} ${caller.name}` : callerKey;

      let originSourceRef = null;
      let descText = "";
      if (typeof desc?.describeExpressionWithOrigin === "function" && callerKey) {
        const d = desc.describeExpressionWithOrigin(model, callerKey, actual);
        originSourceRef = d?.primary?.entity?.sourceRef || null;
        descText = String(d?.text || "").trim();
      }

      return {
        callerKey,
        callerName,
        actualExpr: actual,
        originSourceRef,
        descText,
        statusText: "",
      };
    }

    if ((routine?.calledBy?.length || 0) === 0) {
      return {
        callerKey: "",
        callerName: "",
        actualExpr: "",
        originSourceRef: null,
        descText: "",
        statusText: "(no caller)",
      };
    }

    return {
      callerKey: "",
      callerName: "",
      actualExpr: "",
      originSourceRef: null,
      descText: "",
      statusText: "(not passed)",
    };
  }

  function getDisplayObjects(model) {
    const out = [];
    out.push({
      key: "PROGRAM",
      kind: "PROGRAM",
      name: "(Globals)",
      description: String(model?.description || "").trim(),
      userDescription: String(model?.userDescription || "").trim(),
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

  function selectObject(key, options) {
    state.selectedKey = key;
    state.selectedParamKey = String(options?.paramKey || "").trim();
    if (typeof ui.renderObjectsTable === "function") ui.renderObjectsTable();
    if (typeof ui.renderDetails === "function") ui.renderDetails();
  }

  function renderObjectsTable() {
    const model = state.model;
    const table = ui.$("objectsTable");
    const tbody = table ? table.querySelector("tbody") : null;
    if (!tbody) return;
    tbody.innerHTML = "";
    if (!model) return;

    const filter = String(ui.$("objectSearch")?.value || "").trim().toLowerCase();
    const noDescOnly = Boolean(ui.$("objectNoDescOnly")?.checked);
    const collapseEnabled = !filter && !noDescOnly;
    state.objectsCollapsed = state.objectsCollapsed && typeof state.objectsCollapsed === "object" ? state.objectsCollapsed : {};
    const objects = getDisplayObjects(model);

    const outRows = [];

    for (const obj of objects) {
      const routineDesc = getRoutineDescription(obj, model);
      const baseHay = `${obj.kind} ${obj.name} ${String(obj.key || "")} ${routineDesc}`.toLowerCase();
      const routineMatchesText = !filter || baseHay.includes(filter);
      const routineMissingDesc = !routineDesc;
      const routinePass = routineMatchesText && (!noDescOnly || routineMissingDesc);

      const params = Array.isArray(obj?.params) ? obj.params : [];
      const paramMatches = [];
      for (const p of params) {
        const pDesc = getParamDescription(p);
        const hay = `${obj.kind} ${obj.name} ${p.kind} ${p.name} ${p.dataType || ""} ${pDesc}`.toLowerCase();
        const matchesText = !filter || hay.includes(filter);
        const missingDesc = !pDesc;
        const pass = matchesText && (!noDescOnly || missingDesc);
        if (pass) {
          paramMatches.push({ param: p, desc: pDesc });
        }
      }

      if (!routinePass && paramMatches.length === 0) continue;

      const hasParams = params.length > 0;
      const isCollapsed = Boolean(collapseEnabled && hasParams && state.objectsCollapsed?.[obj.key]);

      outRows.push({ rowKind: "routine", obj, desc: routineDesc, hasParams, isCollapsed, collapseEnabled });

      const showAllParams = !noDescOnly && filter && routinePass;
      if (!hasParams) continue;
      if (isCollapsed) continue;

      const incomingEdges = Array.isArray(obj?.calledBy)
        ? obj.calledBy.slice().sort((a, b) => (a?.sourceRef?.startLine ?? 1e9) - (b?.sourceRef?.startLine ?? 1e9))
        : [];
      const paramIndexMap = buildParamIndexMap(obj);

      const paramsToShow =
        !filter && !noDescOnly ? params : showAllParams ? params : paramMatches.map((x) => x.param);

      for (const p of paramsToShow) {
        outRows.push({ rowKind: "param", obj, param: p, desc: getParamDescription(p) });
        outRows.push({
          rowKind: "source",
          obj,
          param: p,
          source: firstSourceForParam(model, obj, p, incomingEdges, paramIndexMap),
        });
      }
    }

    for (const row of outRows) {
      const obj = row.obj;
      const tr = document.createElement("tr");

      if (row.rowKind === "param") {
        const p = row.param;
        const kind = String(p?.kind || "").trim();
        const name = String(p?.name || "").trim();
        const dt = String(p?.dataType || "").trim();
        const desc = row.desc || "";
        const paramKey = `${kind}:${name}`;

        tr.dataset.key = String(obj.key || "");
        tr.dataset.paramKey = paramKey;
        tr.classList.add("objects-row--param");
        if (String(obj.key || "") === state.selectedKey && String(state.selectedParamKey || "") === paramKey) tr.classList.add("is-selected");

        const nameText = `${name}${dt ? ` TYPE ${dt}` : ""}`;

        tr.innerHTML = `
          <td class="cell-mono cell-muted">${utils.escapeHtml(kind || "PARAM")}</td>
          <td class="cell-mono">
            <div class="objects-tree objects-tree--lvl1">
              <span class="objects-tree-spacer" aria-hidden="true"></span>
              <span class="objects-tree-bullet" aria-hidden="true">↳</span>
              <span class="objects-tree-text">${utils.escapeHtml(nameText)}</span>
            </div>
          </td>
          <td class="${desc ? "" : "cell-muted"}">${utils.escapeHtml(desc || "(chưa có)")}</td>
        `;

        tr.addEventListener("click", () => selectObject(obj.key, { paramKey }));
      } else if (row.rowKind === "source") {
        const p = row.param;
        const kind = String(p?.kind || "").trim();
        const name = String(p?.name || "").trim();
        const paramKey = `${kind}:${name}`;
        const s = row.source;

        tr.dataset.key = String(obj.key || "");
        tr.dataset.paramKey = paramKey;
        tr.classList.add("objects-row--source");
        if (String(obj.key || "") === state.selectedKey && String(state.selectedParamKey || "") === paramKey) tr.classList.add("is-selected");

        const actualExpr = String(s?.actualExpr || "").trim();
        const statusText = String(s?.statusText || "").trim();
        const callerName = String(s?.callerName || "").trim();
        const originSourceRef = s?.originSourceRef || null;

        const label = actualExpr ? ui.sourceLink(actualExpr, originSourceRef) : utils.escapeHtml(statusText || "(none)");
        const callerInfo = callerName ? ` from ${callerName}` : "";

        const rawDescText = String(s?.descText || "").trim();
        const showDescText = rawDescText && actualExpr && rawDescText.toLowerCase() !== actualExpr.toLowerCase() ? rawDescText : "";
        const descCell = showDescText ? utils.escapeHtml(showDescText) : '<span class="cell-muted">(chưa có)</span>';

        tr.innerHTML = `
          <td class="cell-mono cell-muted">SRC</td>
          <td class="cell-mono">
            <div class="objects-tree objects-tree--lvl2">
              <span class="objects-tree-spacer" aria-hidden="true"></span>
              <span class="objects-tree-bullet cell-muted" aria-hidden="true">⤷</span>
              <span class="objects-tree-text">${label}${callerInfo ? ` <span class="cell-muted">${utils.escapeHtml(callerInfo)}</span>` : ""}</span>
            </div>
          </td>
          <td>${descCell}</td>
        `;

        tr.addEventListener("click", () => selectObject(obj.key, { paramKey }));
      } else {
        tr.dataset.key = obj.key;
        if (obj.key === state.selectedKey && !state.selectedParamKey) tr.classList.add("is-selected");

        const desc = row.desc || "";
        const hasToggle = Boolean(row.hasParams);
        const canToggle = Boolean(row.collapseEnabled && hasToggle);
        const isCollapsed = Boolean(row.isCollapsed);
        const toggle = canToggle
          ? `<button type="button" class="objects-tree-toggle" aria-label="${isCollapsed ? "Expand" : "Collapse"}" aria-expanded="${isCollapsed ? "false" : "true"}">${
              isCollapsed ? "▸" : "▾"
            }</button>`
          : `<span class="objects-tree-spacer" aria-hidden="true"></span>`;

        tr.innerHTML = `
          <td>${utils.escapeHtml(obj.kind)}</td>
          <td>
            <div class="objects-tree objects-tree--lvl0">
              ${toggle}
              <span class="objects-tree-text">${utils.escapeHtml(obj.name)}</span>
            </div>
          </td>
          <td class="${desc ? "" : "cell-muted"}">${utils.escapeHtml(desc || "(chưa có)")}</td>
        `;

        tr.addEventListener("click", () => selectObject(obj.key, { paramKey: "" }));

        const toggleBtn = tr.querySelector("button.objects-tree-toggle");
        if (toggleBtn) {
          toggleBtn.addEventListener("click", (ev) => {
            ev.preventDefault();
            ev.stopPropagation();
            const key = String(obj.key || "");
            if (!key) return;
            state.objectsCollapsed[key] = !Boolean(state.objectsCollapsed?.[key]);
            renderObjectsTable();
          });
        }
      }
      tbody.appendChild(tr);
    }
  }

  ui.selectObject = selectObject;
  ui.renderObjectsTable = renderObjectsTable;
})(window.AbapFlow);
