(function (ns) {
  "use strict";

  const utils = ns.utils;
  const ui = ns.ui;
  const state = ui.state;

  const sourceLink = ui.sourceLink;
  const renderAnnoSummaryHtml = ui.renderAnnoSummaryHtml;
  const renderInlineAnnoEditorHtml = ui.renderInlineAnnoEditorHtml;

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
        ? renderInlineAnnoEditorHtml({
            title: "Edit notes",
            codeDesc,
            userDesc,
            userNote,
            annoType: "decl",
            annoKey: key,
            attrs: {
              "data-scope-key": "PROGRAM",
              "data-decl-kind": declKind,
              "data-var-name": name,
            },
          })
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
      <div class="meta">Global DATA: ${model.globalData.length} ƒ?› Global CONSTANTS: ${model.globalConstants.length}</div>
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
          ? renderInlineAnnoEditorHtml({
              title: "Edit notes",
              codeDesc,
              userDesc,
              userNote,
              annoType: "param",
              annoKey: key,
              attrs: {
                "data-routine-key": r.key,
                "data-param-name": String(p.name || ""),
              },
            })
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
          ? renderInlineAnnoEditorHtml({
              title: "Edit notes",
              codeDesc,
              userDesc,
              userNote,
              annoType: "decl",
              annoKey: key,
              attrs: {
                "data-scope-key": r.key,
                "data-decl-kind": declKind,
                "data-var-name": name,
              },
            })
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
          ? renderInlineAnnoEditorHtml({
              title: "Edit notes",
              codeDesc,
              userDesc,
              userNote,
              annoType: "decl",
              annoKey: key,
              attrs: {
                "data-scope-key": r.key,
                "data-decl-kind": declKind,
                "data-var-name": name,
              },
            })
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
          ? renderInlineAnnoEditorHtml({
              title: "Edit notes",
              codeDesc,
              userDesc,
              userNote,
              annoType: "routine",
              annoKey: key,
              attrs: { "data-routine-key": key },
            })
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
          ? renderInlineAnnoEditorHtml({
              title: "Edit notes",
              codeDesc,
              userDesc,
              userNote,
              annoType: "routine",
              annoKey: key,
              attrs: { "data-routine-key": key },
            })
          : "";

        return `<li class="param-item">${sourceLink(`Called by ${name}`, e.sourceRef)}${summary}${edit}</li>`;
      })
      .join("");

    const assignments = (r.assignments || []).map((a) => `<li>${sourceLink(`${a.lhs} = ${a.rhs}`, a.sourceRef)}</li>`).join("");
    const ifStatements = (r.ifStatements || [])
      .map((st) => {
        const kind = String(st?.kind || "IF").trim().toUpperCase() || "IF";
        const cond = String(st?.condition || "").trim();
        const label = cond ? `${kind} ${cond}` : kind;
        return `<li>${sourceLink(label, st.sourceRef)}</li>`;
      })
      .join("");
    const writes = r.writes.map((w) => `<li>${sourceLink(`${w.variableName}  ƒØ?  ${w.statement}`, w.sourceRef)}</li>`).join("");

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
      <div class="meta">${utils.escapeHtml(lineInfo)} ƒ?› Defined: ${r.isDefined ? "Yes" : "No"} ƒ?› Cycle: ${r.isInCycle ? "Yes" : "No"} ƒ?› Depth: ${r.depth}</div>
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
        <div class="section__title">IF / ELSEIF</div>
        <ul class="list">${ifStatements || "<li>(none)</li>"}</ul>
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
    const el = ui.$("objectDetails");
    if (!el) return;

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
      ui.wireNotesEditorForKey(
        "PROGRAM",
        (description, note) => {
          if (description) model.userDescription = description;
          else delete model.userDescription;
          if (note) model.userNote = note;
          else delete model.userNote;
        },
        { rerenderSequence: false },
      );
      ui.wireInlineAnnoEditors();
      return;
    }

    const r = model.nodes.get(state.selectedKey);
    if (!r) {
      el.textContent = "Object not found.";
      return;
    }

    el.innerHTML = renderRoutineDetails(r);
    ui.wireNotesEditorForKey(
      r.key,
      (description, note) => {
        if (description) r.userDescription = description;
        else delete r.userDescription;
        if (note) r.userNote = note;
        else delete r.userNote;
      },
      { rerenderSequence: true },
    );
    ui.wireInlineAnnoEditors();
  }

  ui.renderDetails = renderDetails;
})(window.AbapFlow);
