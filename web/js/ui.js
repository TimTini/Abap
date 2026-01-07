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

  function renderAnnoSummaryHtml(options) {
    const codeDesc = String(options?.codeDesc || "").trim();
    const userDesc = String(options?.userDesc || "").trim();
    const userNote = String(options?.userNote || "").trim();

    const lines = [];
    // Always show Desc if available (codeDesc OR userDesc)
    if (userDesc) {
      lines.push(`<div><span class="anno-summary__k">Desc:</span> ${utils.escapeHtml(userDesc)}</div>`);
    }
    if (codeDesc) {
      lines.push(`<div><span class="anno-summary__k">Desc:</span> ${utils.escapeHtml(codeDesc)}</div>`);
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
          const r = model.nodes.get(routineKey || key);
          applyUserFields(r);
        }

        if (document.getElementById("tab-sequence").classList.contains("is-active")) {
          renderSequence();
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

      const routineKey = String(node.routineKey || "");
      const routine = state.model?.nodes?.get(routineKey) || null;
      const codeRoutineDesc = String(routine?.description || "").trim();
      const userRoutineDesc = String(routine?.userDescription || "").trim();
      const userRoutineNote = String(routine?.userNote || "").trim();
      const noteLines = [];
      if (userRoutineDesc) noteLines.push(`<div><span class="trace-note__k">Your description:</span> ${utils.escapeHtml(userRoutineDesc)}</div>`);
      if (userRoutineNote) noteLines.push(`<div><span class="trace-note__k">Your note:</span> ${utils.escapeHtml(userRoutineNote)}</div>`);
      if (codeRoutineDesc) noteLines.push(`<div><span class="trace-note__k">From code:</span> ${utils.escapeHtml(codeRoutineDesc)}</div>`);
      const routineSummary = renderAnnoSummaryHtml({
        codeDesc: codeRoutineDesc,
        userDesc: userRoutineDesc,
        userNote: userRoutineNote,
      });

      const routineNotesBlock =
        noteLines.length || routineSummary
          ? `<div class="trace-node__notes" style="padding-left:${pad}px">${noteLines.join("")}${routineSummary}</div>`
          : "";

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
          const mapText = `<div class="trace-node__call" style="padding-left:${pad + 14}px">${sourceLink(
            `${dir} PERFORM ${target}: ${c.fromVar} -> ${c.toVar} (${c.mappingKind})`,
            c.edge.sourceRef,
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
    const result = ns.lineage.traceVariable(model, subKey, varName, { maxNodes: 400 });
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
