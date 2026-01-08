(function (ns) {
  "use strict";

  const utils = ns.utils;
  const ui = ns.ui;

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

  function wireInlineAnnoEditors(rootEl) {
    const model = ui.state.model;
    if (!model || !ns.notes) return;
    const root = rootEl || ui.$("objectDetails");
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

        if (document.getElementById("tab-sequence").classList.contains("is-active") && typeof ui.renderSequence === "function") {
          ui.renderSequence();
        }
        if (document.getElementById("tab-templates")?.classList.contains("is-active") && typeof ui.renderTemplates === "function") {
          ui.renderTemplates();
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
    const descEl = ui.$("annoDesc");
    const noteEl = ui.$("annoNote");
    const clearBtn = ui.$("btnAnnoClear");
    const statusEl = ui.$("annoSaveState");
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

      if (options?.rerenderSequence && document.getElementById("tab-sequence").classList.contains("is-active") && typeof ui.renderSequence === "function") {
        ui.renderSequence();
      }
      if (document.getElementById("tab-templates")?.classList.contains("is-active") && typeof ui.renderTemplates === "function") {
        ui.renderTemplates();
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
        if (typeof ui.renderDetails === "function") ui.renderDetails();
      });
    }
  }

  ui.sourceLink = sourceLink;
  ui.renderAnnoSummaryHtml = renderAnnoSummaryHtml;
  ui.wireInlineAnnoEditors = wireInlineAnnoEditors;
  ui.wireNotesEditorForKey = wireNotesEditorForKey;
})(window.AbapFlow);

