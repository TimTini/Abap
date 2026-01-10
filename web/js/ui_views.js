(function (ns) {
  "use strict";

  const utils = ns.utils;
  const ui = ns.ui;
  const state = ui.state;
  const TRACE_ALL_GLOBALS_KEY = ui.constants.TRACE_ALL_GLOBALS_KEY;

  const sourceLink = ui.sourceLink;
  const renderAnnoSummaryHtml = ui.renderAnnoSummaryHtml;
  const renderInlineAnnoEditorHtml = ui.renderInlineAnnoEditorHtml;
  const wireInlineAnnoEditors = ui.wireInlineAnnoEditors;

  function renderDiagram() {
    const model = state.model;
    const host = ui.$("diagramHost");
    if (!host) return;
    host.innerHTML = "";
    if (!model) return;

    ns.diagram.render(model, host, {
      onNodeClick: (key) => {
        ui.selectObject(key, true);
        ui.setActiveTab("objects");
      },
    });
  }

  function goToTrace(routineKey, varName) {
    const model = state.model;
    if (!model) return;
    ui.setActiveTab("trace");
    const subSel = ui.$("traceSubroutine");
    if (subSel) subSel.value = routineKey;
    updateTraceVariables();
    const varSel = ui.$("traceVariable");
    if (varSel) varSel.value = varName;
    runTrace();
  }

  function renderSequenceControls() {
    const model = state.model;
    const sel = ui.$("seqRoot");
    if (!sel) return;
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
    const host = ui.$("sequenceHost");
    if (!host) return;

    if (!model) {
      host.textContent = "Analyze to see sequence.";
      host.classList.add("empty");
      return;
    }

    host.classList.remove("empty");
    const startKey = ui.$("seqRoot")?.value;
    const maxSteps = Number(ui.$("seqMaxSteps")?.value || 200);
    const seq = ns.sequence.build(model, startKey, { maxSteps });
    ns.sequence.render(host, model, seq, {
      onParamClick: ({ routineKey, varName }) => goToTrace(routineKey, varName),
      onRowClick: ({ routineKey }) => ui.selectObject(routineKey, true),
    });
  }

  function renderJson() {
    const out = ui.$("jsonOutput");
    const model = state.model;
    if (!out) return;
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
    const sel = ui.$("traceSubroutine");
    if (!sel) return;
    sel.innerHTML = "";
    if (!model) return;

    const globalsOpt = document.createElement("option");
    globalsOpt.value = TRACE_ALL_GLOBALS_KEY;
    globalsOpt.textContent = "CUSTOMISE (All globals)";
    sel.appendChild(globalsOpt);

    const sep = document.createElement("option");
    sep.disabled = true;
    sep.textContent = "────────────";
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
    const subKey = ui.$("traceSubroutine")?.value;
    const sel = ui.$("traceVariable");
    if (!sel) return;
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
    const el = ui.$("traceResults");
    if (!el) return;
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
      const headerText = `${sourceLink(
        `${node.routineKind} ${node.routineName} (var ${node.varName})`,
        node.sourceRef,
      )}${node.cycle ? ' <span class="badge badge-danger">cycle</span>' : ""}${node.truncated ? ' <span class="badge">truncated</span>' : ""}`;
      const header = `<div class="trace-node__header" style="padding-left:${pad}px"><button type="button" class="trace-node__toggle" aria-expanded="true" title="Collapse/expand">▾</button><span class="trace-node__header-text">${headerText}</span></div>`;

      const decl = node.resolution?.decl;
      const declText = decl?.description ? ` - ${utils.escapeHtml(decl.description)}` : "";
      const scope = node.resolution?.scope ? `${node.resolution.scope}` : "unknown";
      const declLine = decl?.sourceRef?.startLine ? ` (line ${decl.sourceRef.startLine})` : "";
      let declStmt = "";
      if (decl && decl.declKind) {
        const declUpper = String(decl.declKind || "").toUpperCase();
        const valueKeyword = declUpper === "PARAMETERS" ? "DEFAULT" : "VALUE";
        declStmt = `${decl.declKind} ${decl.variableName}${decl.dataType ? ` TYPE ${decl.dataType}` : ""}${decl.value ? ` ${valueKeyword} ${decl.value}` : ""}`;
      } else if (decl && decl.kind && decl.name) {
        declStmt = `${decl.kind} ${decl.name}${decl.dataType ? ` TYPE ${decl.dataType}` : ""}`;
      }
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

      const routineEdit =
        routineKey && ns.notes
          ? renderInlineAnnoEditorHtml({
              title: `Edit object notes — ${node.routineKind} ${node.routineName}`,
              codeDesc: codeRoutineDesc,
              userDesc: userRoutineDesc,
              userNote: userRoutineNote,
              annoType: "routine",
              annoKey: routineKey,
              attrs: { "data-routine-key": routineKey },
            })
          : "";

      let varSummary = "";
      let varEdit = "";
      let varTitle = "";
      const varCards = [];

      if (decl && ns.notes) {
        let originNode = node;
        let originDecl = decl;
        let originScopeName = String(node.resolution?.scope || "").toLowerCase();
        let originRoutineKey = routineKey;

        // If this variable is a PARAM, follow the first caller-chain until we reach a declared DATA/CONSTANTS/global.
        if (originScopeName === "parameter") {
          const seen = new Set();
          for (let i = 0; i < 30; i++) {
            const visitKey = `${originNode?.routineKey || ""}|${String(originNode?.varName || "").toLowerCase()}`;
            if (seen.has(visitKey)) break;
            seen.add(visitKey);

            const s = String(originNode?.resolution?.scope || "").toLowerCase();
            if (s !== "parameter") break;

            const next = (originNode?.calls || []).find((c) => c?.direction === "fromCaller" && c?.child) || null;
            if (!next?.child) break;
            originNode = next.child;
          }

          originRoutineKey = String(originNode?.routineKey || routineKey);
          originDecl = originNode?.resolution?.decl || null;
          originScopeName = String(originNode?.resolution?.scope || "").toLowerCase();
        }

        function isSimpleStructFieldName(name) {
          const s = String(name || "");
          if (!s.includes("-")) return false;
          if (s.includes("->") || s.includes("=>") || s.includes("~")) return false;
          return true;
        }

        function resolveTypeFieldForVirtualDecl(declObj) {
          const origin = declObj?.virtualOrigin && typeof declObj.virtualOrigin === "object" ? declObj.virtualOrigin : null;
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

        function findDeclInScope(scopeKey, declKind, varName) {
          const sk = String(scopeKey || "").trim();
          const dk = String(declKind || "").trim().toUpperCase();
          const vnLower = String(varName || "").trim().toLowerCase();
          if (!sk || !dk || !vnLower) return null;

          if (sk === "PROGRAM") {
            const list = dk === "CONSTANTS" ? model.globalConstants : model.globalData;
            return (list || []).find((x) => String(x?.variableName || "").toLowerCase() === vnLower) || null;
          }

          const rNode = model?.nodes?.get ? model.nodes.get(sk) : null;
          const list = dk === "CONSTANTS" ? rNode?.localConstants : rNode?.localData;
          return (list || []).find((x) => String(x?.variableName || "").toLowerCase() === vnLower) || null;
        }

        function pushDeclVarCard(scopeKey, scopeLabel, declObj, label) {
          if (!declObj || !declObj.declKind || !declObj.variableName) return;
          const title = `Variable notes — ${label}${scopeLabel ? ` (${scopeLabel})` : ""}`;

          const typeField = declObj.isVirtual ? resolveTypeFieldForVirtualDecl(declObj) : null;
          const codeDesc = String((typeField?.field?.description ?? declObj.description) || "").trim();
          const userDesc = String((typeField?.field?.userDescription ?? declObj.userDescription) || "").trim();
          const userNote = String((typeField?.field?.userNote ?? declObj.userNote) || "").trim();
          const summary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

          if (typeField && ns.notes?.makeTypeFieldKey) {
            const key = ns.notes.makeTypeFieldKey(typeField.typeScopeKey, typeField.typeName, typeField.fieldPath);
            const editor = key
              ? renderInlineAnnoEditorHtml({
                  title,
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
            varCards.push(renderNotesCard("var", title, summary, editor));
            return;
          }

          if (ns.notes?.makeDeclKey) {
            const key = ns.notes.makeDeclKey(scopeKey, declObj.declKind, declObj.variableName);
            const editor = key
              ? renderInlineAnnoEditorHtml({
                  title,
                  codeDesc,
                  userDesc,
                  userNote,
                  annoType: "decl",
                  annoKey: key,
                  attrs: {
                    "data-scope-key": scopeKey,
                    "data-decl-kind": declObj.declKind,
                    "data-var-name": declObj.variableName,
                  },
                })
              : "";
            varCards.push(renderNotesCard("var", title, summary, editor));
          }
        }

        if (originDecl && originScopeName === "parameter" && originDecl.name && ns.notes.makeParamKey) {
          const originRoutine = originRoutineKey && originRoutineKey !== "PROGRAM" ? model?.nodes?.get(originRoutineKey) || null : null;
          const originRoutineLabel = originRoutine ? `${originRoutine.kind} ${originRoutine.name}` : originRoutineKey || "";
          varTitle = `Variable notes — PARAM ${String(originDecl.name || "")}${originRoutineLabel ? ` (${originRoutineLabel})` : ""}`;
          const paramKey = ns.notes.makeParamKey(originRoutineKey, originDecl.name);
          const codeDesc = String(originDecl.description || "").trim();
          const userDesc = String(originDecl.userDescription || "").trim();
          const userNote = String(originDecl.userNote || "").trim();
          varSummary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

          if (paramKey) {
            varEdit = renderInlineAnnoEditorHtml({
              title: `Edit variable notes — PARAM ${String(originDecl.name || "")}${originRoutineLabel ? ` (${originRoutineLabel})` : ""}`,
              codeDesc,
              userDesc,
              userNote,
              annoType: "param",
              annoKey: paramKey,
              attrs: {
                "data-routine-key": originRoutineKey,
                "data-param-name": String(originDecl.name || ""),
              },
            });
          }

          varCards.push(renderNotesCard("var", varTitle, varSummary, varEdit));
        } else if (
          originDecl &&
          (originScopeName === "local" || originScopeName === "global") &&
          originDecl.declKind &&
          originDecl.variableName &&
          ns.notes.makeDeclKey
        ) {
          const scopeKey = originScopeName === "global" ? "PROGRAM" : originRoutineKey;
          const scopeRoutine = scopeKey && scopeKey !== "PROGRAM" ? model?.nodes?.get(scopeKey) || null : null;
          const scopeLabel = scopeKey === "PROGRAM" ? "PROGRAM (Globals)" : scopeRoutine ? `${scopeRoutine.kind} ${scopeRoutine.name}` : scopeKey || "";
          varTitle = `Variable notes — ${String(originDecl.declKind || "")} ${String(originDecl.variableName || "")}${
            scopeLabel ? ` (${scopeLabel})` : ""
          }`;
          const declKey = ns.notes.makeDeclKey(scopeKey, originDecl.declKind, originDecl.variableName);
          const codeDesc = String(originDecl.description || "").trim();
          const userDesc = String(originDecl.userDescription || "").trim();
          const userNote = String(originDecl.userNote || "").trim();
          varSummary = renderAnnoSummaryHtml({ codeDesc, userDesc, userNote });

          if (isSimpleStructFieldName(originDecl.variableName)) {
            const parts = String(originDecl.variableName || "")
              .split("-")
              .filter(Boolean);
            let cur = "";
            const fullLower = String(originDecl.variableName || "").toLowerCase();
            for (let i = 0; i < parts.length; i++) {
              cur = cur ? `${cur}-${parts[i]}` : parts[i];
              const declObj = findDeclInScope(scopeKey, originDecl.declKind, cur) || (cur.toLowerCase() === fullLower ? originDecl : null);
              if (!declObj) continue;
              const label = `${String(originDecl.declKind || "")} ${cur}`;
              pushDeclVarCard(scopeKey, scopeLabel, declObj, label);
            }
          }

          if (declKey) {
            varEdit = renderInlineAnnoEditorHtml({
              title: `Edit variable notes — ${String(originDecl.declKind || "")} ${String(originDecl.variableName || "")}${
                scopeLabel ? ` (${scopeLabel})` : ""
              }`,
              codeDesc,
              userDesc,
              userNote,
              annoType: "decl",
              annoKey: declKey,
              attrs: {
                "data-scope-key": scopeKey,
                "data-decl-kind": originDecl.declKind,
                "data-var-name": originDecl.variableName,
              },
            });
          }
        }
      }

      function renderNotesCard(cardKind, titleText, summaryHtml, editorHtml) {
        const title = String(titleText || "").trim();
        if (!title || (!summaryHtml && !editorHtml)) return "";
        const kind = String(cardKind || "").trim();
        const kindClass = kind ? ` trace-node__notes--${utils.escapeHtml(kind)}` : "";
        return `<div class="trace-node__notes${kindClass}" style="margin-left:${pad}px">
          <div class="trace-node__notes-title">${utils.escapeHtml(title)}</div>
          ${summaryHtml || ""}
          ${editorHtml || ""}
        </div>`;
      }

      const routineNotes = renderNotesCard("object", `Object notes — ${node.routineKind} ${node.routineName}`, routineSummary, routineEdit);
      if (varCards.length === 0) {
        const single = renderNotesCard("var", varTitle, varSummary, varEdit);
        if (single) varCards.push(single);
      }
      const notesHtml = `${routineNotes}${varCards.join("")}`;

      const writes = (node.writes || [])
        .map((w) => `<li>${sourceLink(`${w.variableName} ⇐ ${w.statement}`, w.sourceRef)}</li>`)
        .join("");
      const writesHtml = `<ul class="list trace-node__writes" style="margin-left:${pad}px">${writes || "<li>(no writes)</li>"}</ul>`;

      const children = (node.calls || [])
        .map((c) => {
          const dir = c.direction === "fromCaller" ? "from" : "via";
          const target = c.direction === "fromCaller" ? node.routineName : c.child.routineName;
          const label = c.label || `${dir} PERFORM ${target}: ${c.fromVar} -> ${c.toVar} (${c.mappingKind})`;
          const mapText = `<div class="trace-node__call" style="padding-left:${pad + 14}px">${sourceLink(label, c.edge?.sourceRef || null)}</div>`;
          return mapText + renderNode(c.child, indent + 1);
        })
        .join("");

      return `<div class="trace-node">${header}${meta}${notesHtml}${writesHtml}${children}</div>`;
    }

    el.innerHTML = renderNode(root, 0);
    wireInlineAnnoEditors(el);

    if (!el.dataset.traceTreeBound) {
      el.dataset.traceTreeBound = "1";
      el.addEventListener("click", (ev) => {
        const t = ev.target;
        if (!(t instanceof HTMLElement)) return;
        const btn = t.closest("button.trace-node__toggle");
        if (!btn) return;
        const nodeEl = btn.closest(".trace-node");
        if (!nodeEl) return;
        ev.preventDefault();
        ev.stopPropagation();
        const collapsed = nodeEl.classList.toggle("is-collapsed");
        btn.setAttribute("aria-expanded", String(!collapsed));
      });
    }
  }

  function runTrace() {
    const model = state.model;
    if (!model) return;
    const subKey = ui.$("traceSubroutine")?.value;
    const varName = ui.$("traceVariable")?.value;
    if (!subKey || !varName) return;
    const result =
      subKey === TRACE_ALL_GLOBALS_KEY
        ? ns.lineage.traceGlobalVariable(model, varName, { maxNodes: 400 })
        : ns.lineage.traceVariable(model, subKey, varName, { maxNodes: 400 });
    renderTraceResults(result);
  }

  ui.renderDiagram = renderDiagram;
  ui.renderSequenceControls = renderSequenceControls;
  ui.renderSequence = renderSequence;
  ui.renderJson = renderJson;
  ui.renderTraceSubroutines = renderTraceSubroutines;
  ui.updateTraceVariables = updateTraceVariables;
  ui.runTrace = runTrace;
})(window.AbapFlow);
