(function (ns) {
  "use strict";

  const utils = ns.utils;

  const state = {
    model: null,
    selectedKey: null,
  };

  const TRACE_ALL_GLOBALS_KEY = "TRACE:ALL_GLOBALS";

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
        <td>${utils.escapeHtml(String(obj.assignmentsCount ?? obj.assignments?.length ?? 0))}</td>
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

  function renderAnnoSummaryHtml(options) {
    const codeDesc = String(options?.codeDesc || "").trim();
    const userDesc = String(options?.userDesc || "").trim();
    const userNote = String(options?.userNote || "").trim();

    const lines = [];
    if (userDesc) lines.push(`<div><span class="anno-summary__k">Desc:</span> ${utils.escapeHtml(userDesc)}</div>`);
    if (codeDesc && (!userDesc || codeDesc !== userDesc)) {
      const label = userDesc ? "Desc (code)" : "Desc";
      lines.push(`<div><span class="anno-summary__k">${label}:</span> ${utils.escapeHtml(codeDesc)}</div>`);
    }
    if (userNote) {
      lines.push(`<div><span class="anno-summary__k">Note:</span> ${utils.escapeHtml(userNote)}</div>`);
    }

    if (!lines.length) return "";
    return `<div class="anno-summary">${lines.join("")}</div>`;
  }

  function renderProgramDetails(model) {
    function renderGlobalDecl(decl) {
      const name = String(decl?.variableName || "").trim();
      const declKind = String(decl?.declKind || "").trim();
      const key = ns.notes?.makeDeclKey ? ns.notes.makeDeclKey("PROGRAM", declKind, name) : "";
      const codeDesc = String(decl?.description || "").trim();
      const userDesc = String(decl?.userDescription || "").trim();
      const userNote = String(decl?.userNote || "").trim();
      const shownDesc = userDesc || codeDesc;

      const dt = decl?.dataType ? ` TYPE ${decl.dataType}` : " TYPE ?";
      const valuePart = decl?.value ? ` VALUE ${decl.value}` : "";
      const desc = shownDesc ? ` - ${shownDesc}` : "";

      const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

      const edit = key
        ? `<details class="param-notes">
            <summary class="param-notes__summary">Edit notes</summary>
            <div class="param-notes__body">
              <div class="anno-grid">
                <div class="anno-label">Description (from code)</div>
                <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                <div class="anno-label">Your description</div>
                <textarea class="textarea param-notes__input" rows="2" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                  key,
                )}" data-anno-field="description" data-scope-key="PROGRAM" data-decl-kind="${utils.escapeHtml(
                  declKind,
                )}" data-var-name="${utils.escapeHtml(name)}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
                  userDesc,
                )}</textarea>

                <div class="anno-label">Your note</div>
                <textarea class="textarea param-notes__input" rows="3" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                  key,
                )}" data-anno-field="note" data-scope-key="PROGRAM" data-decl-kind="${utils.escapeHtml(
                  declKind,
                )}" data-var-name="${utils.escapeHtml(name)}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
                  userNote,
                )}</textarea>
              </div>
            </div>
          </details>`
        : "";

      return `<li class="param-item">${sourceLink(`${name}${dt}${valuePart}${desc}`, decl?.sourceRef)}${summary}${edit}</li>`;
    }

    const globalsData = (model.globalData || []).map(renderGlobalDecl).join("");
    const globalsConst = (model.globalConstants || []).map(renderGlobalDecl).join("");

    const userDesc = String(model.userDescription || "").trim();
    const userNote = String(model.userNote || "").trim();
    const notesSection = `
      <div class="section">
        <div class="section__title">Notes</div>
        <div class="anno-grid">
          <div class="anno-label">Your description</div>
          <textarea id="annoDesc" class="textarea" rows="2" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
            userDesc,
          )}</textarea>

          <div class="anno-label">Your note</div>
          <textarea id="annoNote" class="textarea" rows="4" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
            userNote,
          )}</textarea>

          <div class="anno-actions">
            <button id="btnAnnoClear" class="btn" type="button">Clear</button>
            <span id="annoSaveState" class="anno-status"></span>
          </div>
        </div>
      </div>
    `;

    return `
      <h3>PROGRAM (Globals)</h3>
      ${notesSection}
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
        const key = ns.notes?.makeParamKey ? ns.notes.makeParamKey(r.key, p.name) : "";
        const codeDesc = String(p.description || "").trim();
        const userDesc = String(p.userDescription || "").trim();
        const userNote = String(p.userNote || "").trim();
        const shownDesc = userDesc || codeDesc;
        const desc = shownDesc ? ` - ${shownDesc}` : "";
        const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

        const edit = key
          ? `<details class="param-notes">
              <summary class="param-notes__summary">Edit notes</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-type="param" data-anno-field="description" data-routine-key="${utils.escapeHtml(
                    r.key,
                  )}" data-param-name="${utils.escapeHtml(String(p.name || ""))}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
                    userDesc,
                  )}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-type="param" data-anno-field="note" data-routine-key="${utils.escapeHtml(
                    r.key,
                  )}" data-param-name="${utils.escapeHtml(String(p.name || ""))}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
                    userNote,
                  )}</textarea>
                </div>
              </div>
            </details>`
          : "";

        return `<li class="param-item">${sourceLink(`${p.kind} ${p.name}${dt}${desc}`, p.sourceRef || r.sourceRef)}${summary}${edit}</li>`;
      })
      .join("");

    const localsData = r.localData
      .map((d) => {
        const name = String(d?.variableName || "").trim();
        const declKind = String(d?.declKind || "").trim();
        const key = ns.notes?.makeDeclKey ? ns.notes.makeDeclKey(r.key, declKind, name) : "";
        const codeDesc = String(d?.description || "").trim();
        const userDesc = String(d?.userDescription || "").trim();
        const userNote = String(d?.userNote || "").trim();
        const shownDesc = userDesc || codeDesc;
        const desc = shownDesc ? ` - ${shownDesc}` : "";
        const dt = d?.dataType ? ` TYPE ${d.dataType}` : "";
        const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

        const edit = key
          ? `<details class="param-notes">
              <summary class="param-notes__summary">Edit notes</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="description" data-scope-key="${utils.escapeHtml(r.key)}" data-decl-kind="${utils.escapeHtml(
                    declKind,
                  )}" data-var-name="${utils.escapeHtml(name)}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
                    userDesc,
                  )}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="note" data-scope-key="${utils.escapeHtml(r.key)}" data-decl-kind="${utils.escapeHtml(
                    declKind,
                  )}" data-var-name="${utils.escapeHtml(name)}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
                    userNote,
                  )}</textarea>
                </div>
              </div>
            </details>`
          : "";

        return `<li class="param-item">${sourceLink(`${name}${dt}${desc}`, d?.sourceRef)}${summary}${edit}</li>`;
      })
      .join("");

    const localsConst = r.localConstants
      .map((c) => {
        const name = String(c?.variableName || "").trim();
        const declKind = String(c?.declKind || "").trim();
        const key = ns.notes?.makeDeclKey ? ns.notes.makeDeclKey(r.key, declKind, name) : "";
        const codeDesc = String(c?.description || "").trim();
        const userDesc = String(c?.userDescription || "").trim();
        const userNote = String(c?.userNote || "").trim();
        const shownDesc = userDesc || codeDesc;
        const desc = shownDesc ? ` - ${shownDesc}` : "";
        const dt = c?.dataType ? ` TYPE ${c.dataType}` : "";
        const valuePart = c?.value ? ` VALUE ${c.value}` : "";
        const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

        const edit = key
          ? `<details class="param-notes">
              <summary class="param-notes__summary">Edit notes</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="description" data-scope-key="${utils.escapeHtml(r.key)}" data-decl-kind="${utils.escapeHtml(
                    declKind,
                  )}" data-var-name="${utils.escapeHtml(name)}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
                    userDesc,
                  )}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="note" data-scope-key="${utils.escapeHtml(r.key)}" data-decl-kind="${utils.escapeHtml(
                    declKind,
                  )}" data-var-name="${utils.escapeHtml(name)}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
                    userNote,
                  )}</textarea>
                </div>
              </div>
            </details>`
          : "";

        return `<li class="param-item">${sourceLink(`${name}${dt}${valuePart}${desc}`, c?.sourceRef)}${summary}${edit}</li>`;
      })
      .join("");

    const calls = r.calls
      .map((e) => {
        const callee = model.nodes.get(e.toKey);
        const name = callee ? `${callee.kind} ${callee.name}` : e.targetName;
        const codeDesc = String(callee?.description || "").trim();
        const userDesc = String(callee?.userDescription || "").trim();
        const userNote = String(callee?.userNote || "").trim();
        const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });
        const key = String(e.toKey || "");

        const edit = key
          ? `<details class="param-notes">
              <summary class="param-notes__summary">Edit notes</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="routine" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="description" data-routine-key="${utils.escapeHtml(
                    key,
                  )}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(userDesc)}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="routine" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="note" data-routine-key="${utils.escapeHtml(
                    key,
                  )}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(userNote)}</textarea>
                </div>
              </div>
            </details>`
          : "";

        return `<li class="param-item">${sourceLink(`PERFORM ${name}`, e.sourceRef)}${e.isInCycle ? ' <span class="badge badge-danger">cycle</span>' : ""}${summary}${edit}</li>`;
      })
      .join("");

    const calledBy = r.calledBy
      .map((e) => {
        const caller = model.nodes.get(e.fromKey);
        const name = caller ? `${caller.kind} ${caller.name}` : e.fromKey;
        const codeDesc = String(caller?.description || "").trim();
        const userDesc = String(caller?.userDescription || "").trim();
        const userNote = String(caller?.userNote || "").trim();
        const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });
        const key = String(e.fromKey || "");

        const edit = key
          ? `<details class="param-notes">
              <summary class="param-notes__summary">Edit notes</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="routine" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="description" data-routine-key="${utils.escapeHtml(
                    key,
                  )}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(userDesc)}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="routine" data-anno-key="${utils.escapeHtml(
                    key,
                  )}" data-anno-field="note" data-routine-key="${utils.escapeHtml(
                    key,
                  )}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(userNote)}</textarea>
                </div>
              </div>
            </details>`
          : "";

        return `<li class="param-item">${sourceLink(`Called by ${name}`, e.sourceRef)}${summary}${edit}</li>`;
      })
      .join("");

    const assignments = (r.assignments || [])
      .map((a) => `<li>${sourceLink(`${a.lhs} = ${a.rhs}`, a.sourceRef)}</li>`)
      .join("");
    const writes = r.writes.map((w) => `<li>${sourceLink(`${w.variableName}  ⇐  ${w.statement}`, w.sourceRef)}</li>`).join("");

    const codeDesc = String(r.description || "").trim();
    const userDesc = String(r.userDescription || "").trim();
    const userNote = String(r.userNote || "").trim();

    const notesSection = `
      <div class="section">
        <div class="section__title">Notes</div>
        <div class="anno-grid">
          <div class="anno-label">Description (from code)</div>
          <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

          <div class="anno-label">Your description</div>
          <textarea id="annoDesc" class="textarea" rows="2" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
            userDesc,
          )}</textarea>

          <div class="anno-label">Your note</div>
          <textarea id="annoNote" class="textarea" rows="4" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
            userNote,
          )}</textarea>

          <div class="anno-actions">
            <button id="btnAnnoClear" class="btn" type="button">Clear</button>
            <span id="annoSaveState" class="anno-status"></span>
          </div>
        </div>
      </div>
    `;

    return `
      <h3>${utils.escapeHtml(r.kind)} ${utils.escapeHtml(r.name)}</h3>
      <div class="meta">${utils.escapeHtml(lineInfo)} • Defined: ${r.isDefined ? "Yes" : "No"} • Cycle: ${r.isInCycle ? "Yes" : "No"} • Depth: ${r.depth}</div>
      ${notesSection}
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
        <div class="section__title">Assignments</div>
        <ul class="list">${assignments || "<li>(none)</li>"}</ul>
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
      wireNotesEditorForKey(
        "PROGRAM",
        (description, note) => {
          if (description) model.userDescription = description;
          else delete model.userDescription;
          if (note) model.userNote = note;
          else delete model.userNote;
        },
        { rerenderSequence: false },
      );
      wireInlineAnnoEditors();
      return;
    }

    const r = model.nodes.get(state.selectedKey);
    if (!r) {
      el.textContent = "Object not found.";
      return;
    }

    el.innerHTML = renderRoutineDetails(r);
    wireNotesEditorForKey(
      r.key,
      (description, note) => {
        if (description) r.userDescription = description;
        else delete r.userDescription;
        if (note) r.userNote = note;
        else delete r.userNote;
      },
      { rerenderSequence: true },
    );
    wireInlineAnnoEditors();
  }

  function wireInlineAnnoEditors(rootEl) {
    const model = state.model;
    if (!model || !ns.notes) return;
    const root = rootEl || $("objectDetails");
    if (!root) return;
    const inputs = Array.from(root.querySelectorAll("textarea.param-notes__input[data-anno-key][data-anno-field]"));
    if (inputs.length === 0) return;

    for (const el of inputs) {
      let saveTimer = null;

      function saveNow() {
        if (saveTimer) {
          clearTimeout(saveTimer);
          saveTimer = null;
        }
        const key = String(el.dataset.annoKey || "");
        const field = String(el.dataset.annoField || "");
        const annoType = String(el.dataset.annoType || "");
        const routineKey = String(el.dataset.routineKey || "");
        const paramName = String(el.dataset.paramName || "");
        const scopeKey = String(el.dataset.scopeKey || "");
        const declKind = String(el.dataset.declKind || "");
        const varName = String(el.dataset.varName || "");
        const value = String(el.value || "").trim();
        if (!key) return;

        if (field === "note") ns.notes.setEntry(key, { note: value });
        else ns.notes.setEntry(key, { description: value });

        function applyUserFields(target) {
          if (!target) return;
          if (field === "note") {
            if (value) target.userNote = value;
            else delete target.userNote;
          } else {
            if (value) target.userDescription = value;
            else delete target.userDescription;
          }
        }

        if (annoType === "param") {
          const routine = model.nodes.get(routineKey);
          const p =
            routine?.params?.find((x) => String(x.name || "").toLowerCase() === String(paramName || "").toLowerCase()) || null;
          applyUserFields(p);
        } else if (annoType === "decl") {
          const dk = String(declKind || "").toUpperCase();
          const vnLower = String(varName || "").toLowerCase();
          if (scopeKey === "PROGRAM") {
            const list = dk === "CONSTANTS" ? model.globalConstants : model.globalData;
            const d = list?.find((x) => String(x.variableName || "").toLowerCase() === vnLower) || null;
            applyUserFields(d);
          } else {
            const routine = model.nodes.get(scopeKey);
            const list = dk === "CONSTANTS" ? routine?.localConstants : routine?.localData;
            const d = list?.find((x) => String(x.variableName || "").toLowerCase() === vnLower) || null;
            applyUserFields(d);
          }
        } else if (annoType === "routine") {
          const rk = routineKey || key;
          if (rk === "PROGRAM") applyUserFields(model);
          else applyUserFields(model.nodes.get(rk));
        }

        if (document.getElementById("tab-sequence").classList.contains("is-active")) {
          renderSequence();
        }
        if (document.getElementById("tab-templates")?.classList.contains("is-active")) {
          renderTemplates();
        }
      }

      function scheduleSave() {
        if (saveTimer) clearTimeout(saveTimer);
        saveTimer = window.setTimeout(saveNow, 350);
      }

      el.addEventListener("input", scheduleSave);
      el.addEventListener("blur", saveNow);
    }
  }

  function wireNotesEditorForKey(objectKey, applyLocal, options) {
    if (!objectKey || !ns.notes) return;
    const descEl = $("annoDesc");
    const noteEl = $("annoNote");
    const clearBtn = $("btnAnnoClear");
    const statusEl = $("annoSaveState");
    if (!descEl || !noteEl || !statusEl) return;

    let saveTimer = null;

    function saveNow() {
      if (saveTimer) {
        clearTimeout(saveTimer);
        saveTimer = null;
      }
      const description = String(descEl.value || "").trim();
      const note = String(noteEl.value || "").trim();
      ns.notes.setEntry(objectKey, { description, note });
      if (applyLocal) applyLocal(description, note);

      statusEl.textContent = "Saved.";
      window.setTimeout(() => {
        if (statusEl.textContent === "Saved.") statusEl.textContent = "";
      }, 1200);

      if (options?.rerenderSequence && document.getElementById("tab-sequence").classList.contains("is-active")) {
        renderSequence();
      }
      if (document.getElementById("tab-templates")?.classList.contains("is-active")) {
        renderTemplates();
      }
    }

    function scheduleSave() {
      statusEl.textContent = "Saving...";
      if (saveTimer) clearTimeout(saveTimer);
      saveTimer = window.setTimeout(saveNow, 350);
    }

    descEl.addEventListener("input", scheduleSave);
    noteEl.addEventListener("input", scheduleSave);
    descEl.addEventListener("blur", saveNow);
    noteEl.addEventListener("blur", saveNow);

    if (clearBtn) {
      clearBtn.addEventListener("click", () => {
        descEl.value = "";
        noteEl.value = "";
        saveNow();
        renderDetails();
      });
    }
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

  // ---------------------------------------------------------------------------
  // Templates (official; execution-flow order)
  // ---------------------------------------------------------------------------

  const TEMPLATE_OVERRIDES_STORAGE_KEY = "abapFlow.templateLocalOverrides.v1";
  const TEMPLATE_OVERRIDES_SCHEMA = "abapflow-template-local-overrides";
  const TEMPLATE_OVERRIDES_VERSION = 1;

  let templateLocalOverrides = loadTemplateLocalOverrides();
  let templateResultById = new Map();
  let selectedTemplateCard = null;

  function nowIso() {
    return new Date().toISOString();
  }

  function safeJsonParse(text) {
    try {
      return { ok: true, value: JSON.parse(text) };
    } catch (err) {
      return { ok: false, error: String(err?.message || err) };
    }
  }

  function getActiveProgramId() {
    if (ns.notes?.getActiveProgramId) return ns.notes.getActiveProgramId();
    return "default";
  }

  function loadTemplateLocalOverrides() {
    try {
      const raw = localStorage.getItem(TEMPLATE_OVERRIDES_STORAGE_KEY);
      if (!raw) return new Map();
      const parsed = safeJsonParse(raw);
      if (!parsed.ok) return new Map();
      const obj = parsed.value;
      if (obj?.schema !== TEMPLATE_OVERRIDES_SCHEMA || obj?.version !== TEMPLATE_OVERRIDES_VERSION) return new Map();

      const out = new Map();
      const overrides = obj?.overrides && typeof obj.overrides === "object" ? obj.overrides : {};
      for (const [key, binds] of Object.entries(overrides)) {
        if (!binds || typeof binds !== "object") continue;
        const bindMap = new Map();
        for (const [bind, value] of Object.entries(binds)) {
          const v = String(value ?? "");
          if (!v.trim()) continue;
          bindMap.set(String(bind), v);
        }
        if (bindMap.size) out.set(String(key), bindMap);
      }
      return out;
    } catch (_) {
      return new Map();
    }
  }

  function saveTemplateLocalOverrides() {
    try {
      const obj = {
        schema: TEMPLATE_OVERRIDES_SCHEMA,
        version: TEMPLATE_OVERRIDES_VERSION,
        updatedAt: nowIso(),
        overrides: {},
      };

      for (const [key, bindMap] of templateLocalOverrides.entries()) {
        const bucket = {};
        for (const [bind, value] of bindMap.entries()) {
          const v = String(value ?? "");
          if (!v.trim()) continue;
          bucket[String(bind)] = v;
        }
        if (Object.keys(bucket).length) obj.overrides[String(key)] = bucket;
      }

      localStorage.setItem(TEMPLATE_OVERRIDES_STORAGE_KEY, JSON.stringify(obj, null, 2));
    } catch (_) {
      // ignore
    }
  }

  function templateBucketKey(templateId, resultId) {
    const pid = String(getActiveProgramId() || "default");
    const tid = String(templateId || "").trim() || "default";
    const rid = String(resultId || "").trim();
    return `${pid}::${tid}::${rid}`;
  }

  function getLocalTemplateOverrides(templateId, resultId) {
    const key = templateBucketKey(templateId, resultId);
    return templateLocalOverrides.get(key) || null;
  }

  function setLocalTemplateOverride(templateId, resultId, bind, value) {
    const key = templateBucketKey(templateId, resultId);
    const b = String(bind || "").trim();
    if (!b) return;

    const v = String(value ?? "");
    const hasValue = Boolean(v.trim());

    let bucket = templateLocalOverrides.get(key);
    if (!bucket && hasValue) {
      bucket = new Map();
      templateLocalOverrides.set(key, bucket);
    }
    if (!bucket) return;

    if (hasValue) bucket.set(b, v);
    else bucket.delete(b);

    if (bucket.size === 0) templateLocalOverrides.delete(key);
    saveTemplateLocalOverrides();
  }

  function applyOverridesToConfig(config, overridesMap) {
    if (!config || !Array.isArray(config.cells) || !overridesMap) return;
    for (const cell of config.cells) {
      const bind = String(cell?.bind || "").trim();
      if (!bind) continue;
      if (!overridesMap.has(bind)) continue;
      cell.text = overridesMap.get(bind);
    }
  }

  function listTemplateEntries() {
    const templates =
      ns.templateRegistry?.templates && typeof ns.templateRegistry.templates === "object" ? ns.templateRegistry.templates : {};
    const order = Array.isArray(ns.templateRegistry?.order)
      ? ns.templateRegistry.order
      : Object.keys(templates).sort((a, b) => a.localeCompare(b));

    const out = [];
    for (const id of order) {
      const t = templates[id];
      if (!t || typeof t !== "object") continue;
      const entry = { ...t, id: String(t.id || id) };
      out.push(entry);
    }

    return out;
  }

  function pickAutoTemplatesBySource(entries) {
    const out = new Map();
    for (const e of entries || []) {
      if (!e || typeof e !== "object") continue;
      if (e.auto === false) continue;
      const source = String(e.source || "").trim();
      if (!source) continue;
      if (!out.has(source)) out.set(source, e);
    }
    return out;
  }

  function getProgramEntrypoints(model) {
    const nodes = Array.from(model?.nodes?.values ? model.nodes.values() : []);
    const byLine = (a, b) => (a?.sourceRef?.startLine ?? 1e9) - (b?.sourceRef?.startLine ?? 1e9);

    const events = nodes
      .filter((n) => n && n.kind === "EVENT")
      .slice()
      .sort(byLine)
      .map((n) => n.key);
    if (events.length) return events;

    const roots = nodes
      .filter((n) => n && n.kind === "FORM" && n.isDefined && (n.calledBy?.length || 0) === 0)
      .slice()
      .sort(byLine)
      .map((n) => n.key);
    if (roots.length) return roots;

    return nodes
      .filter((n) => n && n.kind === "FORM" && n.isDefined)
      .slice()
      .sort(byLine)
      .map((n) => n.key);
  }

  function extractSource(model, sourceRef) {
    if (!model || !Array.isArray(model.lines) || !sourceRef) return "";
    const start = Math.max(1, Math.floor(Number(sourceRef.startLine || 1)));
    const end = Math.min(model.lines.length, Math.floor(Number(sourceRef.endLine || start)));
    return model.lines.slice(start - 1, end).join("\n").trim();
  }

  function edgeIdFrom(edge) {
    const fromKey = String(edge?.fromKey || "");
    const toKey = String(edge?.toKey || "");
    const start = Math.max(0, Math.floor(Number(edge?.sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(edge?.sourceRef?.endLine || start)));
    return `${fromKey}->${toKey}@${start}-${end}`;
  }

  function assignmentIdFrom(routineKey, sourceRef) {
    const rk = String(routineKey || "").trim() || "ASSIGNMENT";
    const start = Math.max(0, Math.floor(Number(sourceRef?.startLine || 0)));
    const end = Math.max(start, Math.floor(Number(sourceRef?.endLine || start)));
    return `${rk}@${start}-${end}`;
  }

  function routineTemplateSteps(model, routineKey, templatesBySource) {
    const routine = model?.nodes?.get ? model.nodes.get(routineKey) : null;
    if (!routine) return [];

    const steps = [];

    if (templatesBySource.has("assignments") && Array.isArray(routine.assignments)) {
      for (const a of routine.assignments) {
        if (!a?.sourceRef) continue;
        steps.push({ kind: "assignment", sourceRef: a.sourceRef, routineKey, routine, assignment: a });
      }
    }

    if (templatesBySource.has("performCalls") && Array.isArray(routine.calls)) {
      for (const edge of routine.calls) {
        const toKey = String(edge?.toKey || "");
        if (!toKey.toUpperCase().startsWith("FORM:")) continue;
        if (!edge?.sourceRef) continue;
        steps.push({ kind: "perform", sourceRef: edge.sourceRef, routineKey, routine, edge });
      }
    }

    steps.sort((a, b) => Number(a?.sourceRef?.startLine || 0) - Number(b?.sourceRef?.startLine || 0));
    return steps;
  }

  function buildTemplatesFlow(model, templatesBySource, options) {
    const maxSteps = Math.max(50, Number(options?.maxSteps || 900));
    const items = [];
    let truncated = false;

    const converter = ns.templateConverter;
    if (!converter) return { items: [], truncated: false };

    function convertPerform(edge) {
      const entry = templatesBySource.get("performCalls");
      if (!entry?.config) return null;
      const context = converter.buildPerformContext(model, edge);
      const expanded = converter.expandExcelLikeTableTemplate(entry.config, context);
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(expanded, context));
      const resultId = edgeIdFrom(edge);
      applyOverridesToConfig(filled, getLocalTemplateOverrides(entry.id, resultId));
      return {
        kind: "perform",
        templateId: String(entry.id || ""),
        resultId,
        edge,
        context,
        sourceRef: edge?.sourceRef || null,
        original: extractSource(model, edge?.sourceRef) || `PERFORM ${context.perform?.name || ""}`,
        filledConfig: filled,
      };
    }

    function convertAssignment(routine, assignment) {
      const entry = templatesBySource.get("assignments");
      if (!entry?.config) return null;
      const context = converter.buildAssignmentContext(model, routine, assignment);
      const filled = converter.compactExcelLikeTableConfig(converter.fillTemplateConfig(entry.config, context));
      const resultId = assignmentIdFrom(routine?.key, assignment?.sourceRef);
      applyOverridesToConfig(filled, getLocalTemplateOverrides(entry.id, resultId));
      return {
        kind: "assignment",
        templateId: String(entry.id || ""),
        resultId,
        assignment,
        routineKey: String(routine?.key || ""),
        context,
        sourceRef: assignment?.sourceRef || null,
        original: extractSource(model, assignment?.sourceRef) || `${context.assignment?.lhs || ""} = ${context.assignment?.rhs || ""}`,
        filledConfig: filled,
      };
    }

    function walk(routineKey, depth, stack) {
      const routine = model?.nodes?.get ? model.nodes.get(routineKey) : null;
      if (!routine) return;

      if (items.length >= maxSteps) {
        truncated = true;
        return;
      }

      stack.add(routineKey);

      const steps = routineTemplateSteps(model, routineKey, templatesBySource);
      for (const step of steps) {
        if (items.length >= maxSteps) {
          truncated = true;
          break;
        }

        if (step.kind === "assignment") {
          const r = convertAssignment(step.routine, step.assignment);
          if (r) items.push({ kind: "template", depth, isRecursion: false, result: r });
          continue;
        }

        if (step.kind === "perform") {
          const isRecursion = stack.has(step.edge?.toKey);
          const r = convertPerform(step.edge);
          if (r) items.push({ kind: "template", depth, isRecursion, result: r });
          if (!isRecursion) walk(step.edge.toKey, depth + 1, stack);
        }
      }

      stack.delete(routineKey);
    }

    const roots = getProgramEntrypoints(model);
    for (const rootKey of roots) {
      const root = model.nodes.get(rootKey);
      items.push({ kind: "separator", label: root ? `${root.kind} ${root.name}` : rootKey, rootKey });
      walk(rootKey, 0, new Set());
      if (truncated) break;
    }

    if (truncated) {
      items.push({ kind: "note", text: `Truncated at ${maxSteps} steps.` });
    }

    return { items, truncated, maxSteps };
  }

  function isEditableDescriptionBind(bind) {
    const b = String(bind || "").trim();
    if (!b) return false;
    if (b === "perform.description") return true;
    return /^(tables|using|changing|raising)\[\d+\]\.description$/.test(b);
  }

  function labelForDescriptionBind(result, bind) {
    const b = String(bind || "").trim();
    const ctx = result?.context;
    const performName = String(ctx?.perform?.name || "").trim();
    if (!b) return "Mô tả";

    if (b === "perform.description") return performName ? `Mô tả FORM ${performName}` : "Mô tả FORM";

    const m = /^(tables|using|changing|raising)\[(\d+)\]\.description$/.exec(b);
    if (!m) return "Mô tả";

    const list = m[1];
    const idx = Number(m[2]);
    const item = ctx?.[list]?.[idx] || null;
    const kind = String(list || "").toUpperCase();
    const actual = String(item?.actual || "").trim();
    const formal = String(item?.name || "").trim();

    if (actual && formal) return `Mô tả ${kind}: ${actual} → ${formal}`;
    if (formal) return `Mô tả ${kind}: ${formal}`;
    if (actual) return `Mô tả ${kind}: ${actual}`;
    return `Mô tả ${kind}`;
  }

  function resolveGlobalDescriptionKey(result, bind) {
    if (!ns.notes?.makeParamKey) return null;
    const b = String(bind || "").trim();
    const ctx = result?.context;
    if (!ctx) return null;

    if (b === "perform.description") {
      const key = String(ctx.perform?.key || "").trim();
      return key ? { key } : null;
    }

    const m = /^(tables|using|changing|raising)\[(\d+)\]\.description$/.exec(b);
    if (!m) return null;

    const list = m[1];
    const idx = Number(m[2]);
    const item = ctx?.[list]?.[idx] || null;
    const formalName = String(item?.name || "").trim();
    const routineKey = String(ctx.perform?.key || "").trim();
    if (!routineKey || !formalName) return null;

    const key = ns.notes.makeParamKey(routineKey, formalName);
    return key ? { key } : null;
  }

  function openDescriptionEditorDialog(options) {
    const titleText = String(options?.title || "Edit description");
    const initialValue = String(options?.value ?? "");

    return new Promise((resolve) => {
      const overlay = document.createElement("div");
      overlay.className = "demo-modal";

      const dialog = document.createElement("div");
      dialog.className = "demo-modal__dialog";
      overlay.appendChild(dialog);

      const title = document.createElement("div");
      title.className = "demo-modal__title";
      title.textContent = titleText;
      dialog.appendChild(title);

      const textarea = document.createElement("textarea");
      textarea.className = "input demo-modal__textarea";
      textarea.value = initialValue;
      textarea.placeholder = "Nhập mô tả...";
      dialog.appendChild(textarea);

      const hint = document.createElement("div");
      hint.className = "demo-modal__hint";
      hint.textContent = "Chọn phạm vi cập nhật:";
      dialog.appendChild(hint);

      const actions = document.createElement("div");
      actions.className = "demo-modal__actions";
      dialog.appendChild(actions);

      const btnCancel = document.createElement("button");
      btnCancel.type = "button";
      btnCancel.className = "btn";
      btnCancel.textContent = "Hủy";

      const btnLocal = document.createElement("button");
      btnLocal.type = "button";
      btnLocal.className = "btn";
      btnLocal.textContent = "Chỉ template hiện tại";

      const btnGlobal = document.createElement("button");
      btnGlobal.type = "button";
      btnGlobal.className = "btn btn-primary";
      btnGlobal.textContent = "Toàn bộ template";

      actions.appendChild(btnCancel);
      actions.appendChild(btnLocal);
      actions.appendChild(btnGlobal);

      function cleanup() {
        window.removeEventListener("keydown", onKeyDown);
        overlay.remove();
      }

      function close(action) {
        const value = textarea.value;
        cleanup();
        resolve({ action, value });
      }

      function onKeyDown(e) {
        if (e.key === "Escape") {
          e.preventDefault();
          close("cancel");
        }
      }

      overlay.addEventListener("click", (e) => {
        if (e.target === overlay) close("cancel");
      });
      btnCancel.addEventListener("click", () => close("cancel"));
      btnLocal.addEventListener("click", () => close("local"));
      btnGlobal.addEventListener("click", () => close("global"));
      window.addEventListener("keydown", onKeyDown);

      document.body.appendChild(overlay);
      textarea.focus();
      textarea.select();
    });
  }

  function handleTemplatesDblClick(event) {
    const t = event?.target && event.target.nodeType === 1 ? event.target : event?.target?.parentElement;
    const td = t?.closest ? t.closest("td[data-bind]") : null;
    if (!td) return;

    const bind = String(td.dataset.bind || "").trim();
    if (!isEditableDescriptionBind(bind)) return;

    const card = td.closest(".template-block");
    const resultId = String(card?.dataset?.resultId || "").trim();
    const templateId = String(card?.dataset?.templateId || "").trim();
    if (!resultId || !templateId) return;

    const result = templateResultById.get(resultId) || null;
    if (!result) return;

    event.preventDefault();
    if (card && typeof card.click === "function") card.click();

    const title = labelForDescriptionBind(result, bind);
    const initial = String(td.textContent ?? "");

    openDescriptionEditorDialog({ title, value: initial }).then(({ action, value }) => {
      const nextValue = String(value ?? "").trim();
      if (action === "cancel") return;

      if (action === "local") {
        setLocalTemplateOverride(templateId, resultId, bind, nextValue);
        renderTemplates({ autoSelectResultId: resultId });
        setStatus("Đã lưu mô tả (chỉ template hiện tại).", false);
        return;
      }

      if (action === "global") {
        const target = resolveGlobalDescriptionKey(result, bind);
        if (!target || !ns.notes?.setEntry) {
          setStatus("Không thể cập nhật toàn bộ cho mục này.", true);
          return;
        }

        setLocalTemplateOverride(templateId, resultId, bind, "");
        ns.notes.setEntry(target.key, { description: nextValue });
        if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
        renderTemplates({ autoSelectResultId: resultId });
        setStatus("Đã lưu mô tả (toàn bộ template).", false);
      }
    });
  }

  function renderTemplates(options) {
    const host = $("templatesHost");
    if (!host) return;

    const model = state.model;
    if (!model) {
      host.textContent = "Analyze to see templates.";
      host.classList.add("empty");
      return;
    }

    if (!ns.templateRegistry || !ns.templateConverter || !ns.tableRenderer) {
      host.textContent = "Template modules not loaded.";
      host.classList.add("empty");
      return;
    }

    const entries = listTemplateEntries();
    const templatesBySource = pickAutoTemplatesBySource(entries);
    if (templatesBySource.size === 0) {
      host.textContent = "No templates configured.";
      host.classList.add("empty");
      return;
    }

    const flow = buildTemplatesFlow(model, templatesBySource, { maxSteps: 900 });
    if (!flow.items.length) {
      host.textContent = "No template-mapped statements found.";
      host.classList.add("empty");
      return;
    }

    const autoSelectResultId = String(options?.autoSelectResultId || "");
    let autoSelectCard = null;

    host.classList.remove("empty");
    host.textContent = "";
    selectedTemplateCard = null;
    templateResultById = new Map();

    for (const item of flow.items) {
      if (item.kind === "separator") {
        const sep = document.createElement("div");
        sep.className = "flow-separator";
        sep.textContent = item.label;
        host.appendChild(sep);
        continue;
      }

      if (item.kind === "note") {
        const note = document.createElement("div");
        note.className = "flow-note";
        note.textContent = item.text;
        host.appendChild(note);
        continue;
      }

      if (item.kind !== "template") continue;

      const result = item.result;
      templateResultById.set(result.resultId, result);

      const card = document.createElement("div");
      card.className = "template-block is-clickable";
      card.dataset.resultId = String(result.resultId || "");
      card.dataset.templateId = String(result.templateId || "");
      card.style.marginLeft = `${Math.max(0, item.depth) * 18}px`;
      if (item.isRecursion || result.edge?.isInCycle) card.classList.add("is-cycle");

      const header = document.createElement("div");
      header.className = "template-block__header";
      const src = result.sourceRef;
      const lineText = src?.startLine ? (src.endLine && src.endLine !== src.startLine ? ` (L${src.startLine}-L${src.endLine})` : ` (L${src.startLine})`) : "";
      const loopText = item.isRecursion ? " ↻" : "";

      if (result.kind === "perform") {
        header.textContent = `PERFORM ${result.context?.perform?.name || ""}${lineText}${loopText}`;
      } else if (result.kind === "assignment") {
        const lhs = String(result.context?.assignment?.lhs || "").trim();
        const rhs = String(result.context?.assignment?.rhs || "").trim();
        const text = lhs && rhs ? `${lhs} = ${rhs}` : lhs ? `${lhs} = ...` : "Assignment";
        header.textContent = `${text}${lineText}`;
      } else {
        header.textContent = `Template${lineText}`;
      }

      card.appendChild(header);

      const tableWrap = document.createElement("div");
      tableWrap.className = "template-block__table";
      tableWrap.appendChild(ns.tableRenderer.renderExcelLikeTable(result.filledConfig));
      card.appendChild(tableWrap);

      card.addEventListener("click", () => {
        if (selectedTemplateCard) selectedTemplateCard.classList.remove("is-selected");
        selectedTemplateCard = card;
        card.classList.add("is-selected");
        highlightSource(result.sourceRef?.startLine, result.sourceRef?.endLine);
      });

      host.appendChild(card);
      if (autoSelectResultId && result.resultId === autoSelectResultId) autoSelectCard = card;
    }

    if (autoSelectCard) autoSelectCard.click();
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

    const globalsOpt = document.createElement("option");
    globalsOpt.value = TRACE_ALL_GLOBALS_KEY;
    globalsOpt.textContent = "CUSTOMISE (All globals)";
    sel.appendChild(globalsOpt);

    const sep = document.createElement("option");
    sep.disabled = true;
    sep.textContent = "──────────────";
    sel.appendChild(sep);

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
    else sel.value = TRACE_ALL_GLOBALS_KEY;
    updateTraceVariables();
  }

  function updateTraceVariables() {
    const model = state.model;
    const subKey = $("traceSubroutine").value;
    const sel = $("traceVariable");
    sel.innerHTML = "";
    if (!model || !subKey) return;

    const vars =
      subKey === TRACE_ALL_GLOBALS_KEY
        ? ns.lineage.getGlobalVariables(model)
        : ns.lineage.getVariablesForRoutine(model, subKey, { globalMode: "used" });
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
      const declLinkText = declStmt ? ` | Decl: ${sourceLink(declStmt, decl?.sourceRef || null)}` : "";
      const meta = `<div class="trace-node__meta" style="padding-left:${pad}px">Scope: ${utils.escapeHtml(scope)}${utils.escapeHtml(declLine)}${declText}${declLinkText}</div>`;

      const routineKey = String(node.routineKey || "");
      const model = state.model;
      const routine = routineKey && routineKey !== "PROGRAM" ? model?.nodes?.get(routineKey) || null : null;
      const codeRoutineDesc = String(routine?.description || "").trim();
      const userRoutineDesc =
        routineKey === "PROGRAM" ? String(model?.userDescription || "").trim() : String(routine?.userDescription || "").trim();
      const userRoutineNote = routineKey === "PROGRAM" ? String(model?.userNote || "").trim() : String(routine?.userNote || "").trim();
      const routineSummary = renderAnnoSummaryHtml({
        codeDesc: codeRoutineDesc,
        userDesc: userRoutineDesc,
        userNote: userRoutineNote,
      });

      const routineNotesBlock = routineSummary ? `<div class="trace-node__notes" style="padding-left:${pad}px">${routineSummary}</div>` : "";

      const routineEdit =
        routineKey && ns.notes
          ? `<details class="param-notes" style="padding-left:${pad}px">
              <summary class="param-notes__summary">Edit object notes</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeRoutineDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(
                    codeRoutineDesc || "(none)",
                  )}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="routine" data-anno-key="${utils.escapeHtml(
                    routineKey,
                  )}" data-anno-field="description" data-routine-key="${utils.escapeHtml(
                    routineKey,
                  )}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(userRoutineDesc)}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="routine" data-anno-key="${utils.escapeHtml(
                    routineKey,
                  )}" data-anno-field="note" data-routine-key="${utils.escapeHtml(
                    routineKey,
                  )}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(userRoutineNote)}</textarea>
                </div>
              </div>
            </details>`
          : "";

      let varSummary = "";
      let varEdit = "";

      if (decl && ns.notes) {
        const scopeName = String(node.resolution?.scope || "").toLowerCase();

        if (scopeName === "parameter" && decl.name && ns.notes.makeParamKey) {
          const paramKey = ns.notes.makeParamKey(routineKey, decl.name);
          const codeDesc = String(decl.description || "").trim();
          const userDesc = String(decl.userDescription || "").trim();
          const userNote = String(decl.userNote || "").trim();
          varSummary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

          if (paramKey) {
            varEdit = `<details class="param-notes" style="padding-left:${pad}px">
              <summary class="param-notes__summary">Edit variable notes (${utils.escapeHtml(decl.name)})</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="param" data-anno-key="${utils.escapeHtml(
                    paramKey,
                  )}" data-anno-field="description" data-routine-key="${utils.escapeHtml(
              routineKey,
            )}" data-param-name="${utils.escapeHtml(decl.name)}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(
              userDesc,
            )}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="param" data-anno-key="${utils.escapeHtml(
                    paramKey,
                  )}" data-anno-field="note" data-routine-key="${utils.escapeHtml(
              routineKey,
            )}" data-param-name="${utils.escapeHtml(decl.name)}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(
              userNote,
            )}</textarea>
                </div>
              </div>
            </details>`;
          }
        } else if ((scopeName === "local" || scopeName === "global") && decl.declKind && decl.variableName && ns.notes.makeDeclKey) {
          const scopeKey = scopeName === "global" ? "PROGRAM" : routineKey;
          const declKey = ns.notes.makeDeclKey(scopeKey, decl.declKind, decl.variableName);
          const codeDesc = String(decl.description || "").trim();
          const userDesc = String(decl.userDescription || "").trim();
          const userNote = String(decl.userNote || "").trim();
          varSummary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

          if (declKey) {
            varEdit = `<details class="param-notes" style="padding-left:${pad}px">
              <summary class="param-notes__summary">Edit variable notes (${utils.escapeHtml(decl.variableName)})</summary>
              <div class="param-notes__body">
                <div class="anno-grid">
                  <div class="anno-label">Description (from code)</div>
                  <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(none)")}</div>

                  <div class="anno-label">Your description</div>
                  <textarea class="textarea param-notes__input" rows="2" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                    declKey,
                  )}" data-anno-field="description" data-scope-key="${utils.escapeHtml(
              scopeKey,
            )}" data-decl-kind="${utils.escapeHtml(decl.declKind)}" data-var-name="${utils.escapeHtml(
              decl.variableName,
            )}" placeholder="Add your description (stored locally)...">${utils.escapeHtml(userDesc)}</textarea>

                  <div class="anno-label">Your note</div>
                  <textarea class="textarea param-notes__input" rows="3" data-anno-type="decl" data-anno-key="${utils.escapeHtml(
                    declKey,
                  )}" data-anno-field="note" data-scope-key="${utils.escapeHtml(
              scopeKey,
            )}" data-decl-kind="${utils.escapeHtml(decl.declKind)}" data-var-name="${utils.escapeHtml(
              decl.variableName,
            )}" placeholder="Add notes (stored locally)...">${utils.escapeHtml(userNote)}</textarea>
                </div>
              </div>
            </details>`;
          }
        }
      }

      const varNotesBlock = varSummary
        ? `<div class="trace-node__notes" style="padding-left:${pad}px">${varSummary}</div>`
        : "";

      const notesHtml = `${routineNotesBlock}${routineEdit}${varNotesBlock}${varEdit}`;

      const writes = (node.writes || [])
        .map((w) => `<li>${sourceLink(`${w.variableName} ⇐ ${w.statement}`, w.sourceRef)}</li>`)
        .join("");
      const writesHtml = `<ul class="list trace-node__writes" style="margin-left:${pad}px">${writes || "<li>(no writes)</li>"}</ul>`;

      const children = (node.calls || [])
        .map((c) => {
          const dir = c.direction === "fromCaller" ? "from" : "via";
          const target = c.direction === "fromCaller" ? node.routineName : c.child.routineName;
          const label =
            c.label ||
            `${dir} PERFORM ${target}: ${c.fromVar} -> ${c.toVar} (${c.mappingKind})`;
          const mapText = `<div class="trace-node__call" style="padding-left:${pad + 14}px">${sourceLink(
            label,
            c.edge?.sourceRef || null,
          )}</div>`;
          return mapText + renderNode(c.child, indent + 1);
        })
        .join("");

      return `<div class="trace-node">${header}${meta}${notesHtml}${writesHtml}${children}</div>`;
    }

    el.innerHTML = renderNode(root, 0);
    wireInlineAnnoEditors(el);
  }

  function runTrace() {
    const model = state.model;
    if (!model) return;
    const subKey = $("traceSubroutine").value;
    const varName = $("traceVariable").value;
    const result =
      subKey === TRACE_ALL_GLOBALS_KEY
        ? ns.lineage.traceGlobalVariable(model, varName, { maxNodes: 400 })
        : ns.lineage.traceVariable(model, subKey, varName, { maxNodes: 400 });
    renderTraceResults(result);
  }

  function downloadTextFile(filename, text, mimeType) {
    const blob = new Blob([String(text || "")], { type: mimeType || "text/plain" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    a.remove();
    window.setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  function exportNotes() {
    if (!ns.notes) {
      setStatus("Notes module not loaded.", true);
      return;
    }
    const pid = ns.notes.getActiveProgramId ? ns.notes.getActiveProgramId() : "default";
    const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);
    const filename = `abapflow-notes-${pid}-${stamp}.json`;
    downloadTextFile(filename, ns.notes.exportJson(), "application/json");
    setStatus("Notes exported.", false);
  }

  async function importNotesFromFile(file) {
    if (!ns.notes) {
      setStatus("Notes module not loaded.", true);
      return;
    }
    if (!file) return;
    const text = await file.text();
    const res = ns.notes.importJson(text, { mode: "merge" });
    if (!res.ok) {
      setStatus(res.error || "Import failed.", true);
      return;
    }

    if (state.model) ns.notes.applyToModel(state.model);
    renderObjectsTable();
    renderDetails();
    if (document.getElementById("tab-sequence").classList.contains("is-active")) {
      renderSequence();
    }
    if (document.getElementById("tab-templates")?.classList.contains("is-active")) {
      renderTemplates();
    }
    setStatus("Notes imported.", false);
  }

  function analyze() {
    const input = $("abapInput").value;
    if (!String(input || "").trim()) {
      setStatus("Paste ABAP code first.", true);
      return;
    }

    try {
      if (ns.notes) ns.notes.setActiveProgramFromText(input);
      const model = ns.parser.parseProgram(input);
      if (ns.notes) ns.notes.applyToModel(model);
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
      renderTemplates();
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
        if (name === "templates") {
          requestAnimationFrame(() => renderTemplates());
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
      const th = $("templatesHost");
      if (th) {
        th.textContent = "Analyze to see templates.";
        th.classList.add("empty");
      }
      selectedTemplateCard = null;
      templateResultById = new Map();
      setStatus("Cleared.", false);
    });

    $("btnExportNotes").addEventListener("click", exportNotes);
    $("btnImportNotes").addEventListener("click", () => $("notesImportFile").click());
    $("notesImportFile").addEventListener("change", async (ev) => {
      const input = ev.target;
      const file = input?.files?.[0] || null;
      await importNotesFromFile(file);
      if (input) input.value = "";
    });

    $("objectSearch").addEventListener("input", () => renderObjectsTable());
    $("traceSubroutine").addEventListener("change", updateTraceVariables);
    $("btnTrace").addEventListener("click", runTrace);
    $("btnSeqRender").addEventListener("click", renderSequence);
    $("seqRoot").addEventListener("change", renderSequence);

    $("templatesHost")?.addEventListener("dblclick", handleTemplatesDblClick);

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
