(function (ns) {
  "use strict";

  const utils = ns.utils;
  const ui = ns.ui;
  const state = ui.state;

  const sourceLink = ui.sourceLink;
  const renderAnnoSummaryHtml = ui.renderAnnoSummaryHtml;
  const renderInlineAnnoEditorHtml = ui.renderInlineAnnoEditorHtml;

  function isSimpleStructFieldName(name) {
    const s = String(name || "");
    if (!s.includes("-")) return false;
    if (s.includes("->") || s.includes("=>") || s.includes("~")) return false;
    return true;
  }

  function resolveTypeFieldForVirtualDecl(model, decl) {
    const origin = decl?.virtualOrigin && typeof decl.virtualOrigin === "object" ? decl.virtualOrigin : null;
    if (!origin || origin.kind !== "typeField") return null;

    const typeScopeKey = String(origin.typeScopeKey || "").trim() || "PROGRAM";
    const typeName = String(origin.typeName || "").trim();
    const fieldPath = String(origin.fieldPath || "").trim();
    if (!typeName || !fieldPath) return null;

    const typeKey = `${typeScopeKey}|${typeName.toLowerCase()}`;
    const typeDef = model?.typeDefs?.get ? model.typeDefs.get(typeKey) : null;
    const field = typeDef?.fields?.get ? typeDef.fields.get(fieldPath.toLowerCase()) : null;
    if (!field) return null;

    return { typeScopeKey, typeName, fieldPath, field };
  }

  function renderDeclInner(model, decl, scopeKey, options) {
    const name = String(decl?.variableName || "").trim();
    const declKind = String(decl?.declKind || "").trim();
    const upperKind = declKind.toUpperCase();

    const isVirtualTypeField = Boolean(decl?.isVirtual && decl?.virtualOrigin?.kind === "typeField");
    const typeField = isVirtualTypeField ? resolveTypeFieldForVirtualDecl(model, decl) : null;

    const codeDesc = String((typeField?.field?.description ?? decl?.description) || "").trim();
    const userDesc = String((typeField?.field?.userDescription ?? decl?.userDescription) || "").trim();
    const userNote = String((typeField?.field?.userNote ?? decl?.userNote) || "").trim();
    const shownDesc = userDesc || codeDesc;

    const dt = decl?.dataType ? ` TYPE ${decl.dataType}` : options?.showUnknownType ? " TYPE ?" : "";
    const valueKeyword = upperKind === "PARAMETERS" ? "DEFAULT" : "VALUE";
    const valuePart = decl?.value ? ` ${valueKeyword} ${decl.value}` : "";
    const desc = shownDesc ? ` - ${shownDesc}` : "";

    const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

    let edit = "";
    if (typeField && ns.notes?.makeTypeFieldKey) {
      const key = ns.notes.makeTypeFieldKey(typeField.typeScopeKey, typeField.typeName, typeField.fieldPath);
      edit = key
        ? renderInlineAnnoEditorHtml({
            title: `Edit type field notes — ${typeField.typeName}-${typeField.fieldPath}`,
            codeDesc,
            userDesc,
            userNote,
            annoType: "typefield",
            annoKey: key,
            attrs: {
              "data-type-scope-key": typeField.typeScopeKey,
              "data-type-name": typeField.typeName,
              "data-field-path": typeField.fieldPath,
            },
          })
        : "";
    } else if (ns.notes?.makeDeclKey) {
      const key = ns.notes.makeDeclKey(scopeKey, declKind, name);
      edit = key
        ? renderInlineAnnoEditorHtml({
            title: "Edit notes",
            codeDesc,
            userDesc,
            userNote,
            annoType: "decl",
            annoKey: key,
            attrs: {
              "data-scope-key": scopeKey,
              "data-decl-kind": declKind,
              "data-var-name": name,
            },
          })
        : "";
    }

    const labelName = String(options?.labelName || name);
    return `${sourceLink(`${labelName}${dt}${valuePart}${desc}`, decl?.sourceRef)}${summary}${edit}`;
  }

  function renderDeclListGrouped(model, decls, scopeKey, options) {
    const list = Array.isArray(decls) ? decls : [];
    if (!list.length) return "";

    const fieldsByRoot = new Map();
    for (const d of list) {
      const name = String(d?.variableName || "").trim();
      if (!isSimpleStructFieldName(name)) continue;
      const root = name.split("-")[0] || "";
      const rootLower = root.toLowerCase();
      if (!rootLower) continue;
      if (!fieldsByRoot.has(rootLower)) fieldsByRoot.set(rootLower, []);
      fieldsByRoot.get(rootLower).push(d);
    }

    const rendered = new Set();
    const out = [];

    function renderSingle(decl, extra) {
      return `<li class="param-item">${renderDeclInner(model, decl, scopeKey, { ...options, ...(extra || {}) })}</li>`;
    }

    function renderGroup(rootDecl, rootName, fields) {
      const rootLower = String(rootName || "").toLowerCase();
      if (rendered.has(rootLower)) return;
      rendered.add(rootLower);

      const sorted = (fields || [])
        .slice()
        .sort((a, b) => String(a?.variableName || "").localeCompare(String(b?.variableName || "")));

      const headerInner = rootDecl
        ? renderDeclInner(model, rootDecl, scopeKey, options)
        : `<span>${utils.escapeHtml(rootName || "")}</span>`;

      const fieldItems = sorted
        .map((d) => {
          const full = String(d?.variableName || "").trim();
          const rel = full.toLowerCase().startsWith(`${rootLower}-`) ? full.slice(rootLower.length + 1) : full;
          const depth = rel.split("-").filter(Boolean).length;
          const pad = Math.max(0, depth - 1) * 14;
          return `<li class="param-item decl-group__field" style="padding-left:${pad}px">${renderDeclInner(model, d, scopeKey, {
            ...options,
            showUnknownType: false,
            labelName: rel,
          })}</li>`;
        })
        .join("");

      out.push(`<li class="decl-group">
        <div class="decl-group__header param-item">
          <button type="button" class="decl-group__toggle" aria-expanded="true" title="Collapse/expand">▾</button>
          <div class="decl-group__header-content">${headerInner}</div>
        </div>
        <ul class="list decl-group__fields">${fieldItems || "<li>(none)</li>"}</ul>
      </li>`);
    }

    for (const d of list) {
      const name = String(d?.variableName || "").trim();
      if (isSimpleStructFieldName(name)) {
        const rootLower = String(name.split("-")[0] || "").toLowerCase();
        if (fieldsByRoot.has(rootLower)) continue;
      }

      const rootLower = name.toLowerCase();
      const fields = fieldsByRoot.get(rootLower) || null;
      if (fields) {
        renderGroup(d, name, fields);
      } else {
        out.push(renderSingle(d));
      }
    }

    for (const [rootLower, fields] of fieldsByRoot.entries()) {
      if (rendered.has(rootLower)) continue;
      renderGroup(null, rootLower, fields);
    }

    return out.join("");
  }

  function renderProgramDetails(model) {
    const globalsData = renderDeclListGrouped(model, model.globalData || [], "PROGRAM", { showUnknownType: true });
    const globalsConst = renderDeclListGrouped(model, model.globalConstants || [], "PROGRAM", { showUnknownType: true });

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

    const localsData = renderDeclListGrouped(model, r.localData || [], r.key, { showUnknownType: false });

    const localsConst = renderDeclListGrouped(model, r.localConstants || [], r.key, { showUnknownType: false });

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

    const writes = r.writes.map((w) => `<li>${sourceLink(`${w.variableName}  ƒØ?  ${w.statement}`, w.sourceRef)}</li>`).join("");

    const statementSections = (() => {
      const registry = ns.abapObjects?.getRegistry?.() || null;
      const defs = Array.isArray(registry?.statementObjects) ? registry.statementObjects : [];
      const items = Array.isArray(r.statementItems) ? r.statementItems : [];
      if (!defs.length || !items.length) return "";

      const byObjectId = new Map();
      for (const it of items) {
        const objectId = String(it?.objectId || "").trim();
        if (!objectId) continue;
        if (!byObjectId.has(objectId)) byObjectId.set(objectId, []);
        byObjectId.get(objectId).push(it);
      }

      function labelForItem(objectId, payload) {
        if (objectId === "assignment") {
          const lhs = String(payload?.lhs || "").trim();
          const rhs = String(payload?.rhs || "").trim();
          if (lhs && rhs) return `${lhs} = ${rhs}`;
        }

        if (objectId === "if") {
          const kind = String(payload?.kind || "IF").trim().toUpperCase() || "IF";
          const cond = String(payload?.condition || "").trim();
          return cond ? `${kind} ${cond}` : kind;
        }

        const stmt = String(payload?.statement || payload?.raw || "").trim();
        if (stmt) return stmt;

        const kind = String(payload?.kind || "").trim();
        return kind || String(objectId || "Statement");
      }

      const sections = [];
      for (const def of defs) {
        const objectId = String(def?.id || "").trim();
        if (!objectId) continue;
        const list = byObjectId.get(objectId) || [];
        if (!list.length) continue;

        const rows = list
          .map((it) => {
            const payload = it?.payload || null;
            const src = it?.sourceRef || payload?.sourceRef || null;
            return `<li>${sourceLink(labelForItem(objectId, payload), src)}</li>`;
          })
          .join("");

        sections.push(`
          <div class="section">
            <div class="section__title">${utils.escapeHtml(String(def?.label || objectId))}</div>
            <ul class="list">${rows || "<li>(none)</li>"}</ul>
          </div>
        `);
      }

      return sections.join("");
    })();

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
      ${statementSections}
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

    if (!el.dataset.declGroupsBound) {
      el.dataset.declGroupsBound = "1";
      el.addEventListener("click", (ev) => {
        const t = ev.target;
        if (!(t instanceof HTMLElement)) return;
        const btn = t.closest("button.decl-group__toggle");
        if (!btn) return;
        const group = btn.closest(".decl-group");
        if (!group) return;
        ev.preventDefault();
        ev.stopPropagation();
        const collapsed = group.classList.toggle("is-collapsed");
        btn.setAttribute("aria-expanded", String(!collapsed));
      });
    }

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
