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
    if (userDesc) lines.push(`<div><span class="anno-summary__k">Mô tả:</span> ${utils.escapeHtml(userDesc)}</div>`);
    if (codeDesc && (!userDesc || codeDesc !== userDesc)) {
      const label = userDesc ? "Mô tả (code)" : "Mô tả";
      lines.push(`<div><span class="anno-summary__k">${label}:</span> ${utils.escapeHtml(codeDesc)}</div>`);
    }
    if (userNote) {
      lines.push(`<div><span class="anno-summary__k">Ghi chú:</span> ${utils.escapeHtml(userNote)}</div>`);
    }

    if (!lines.length) return "";
    return `<div class="anno-summary">${lines.join("")}</div>`;
  }

  function renderInlineAnnoEditorHtml(options) {
    const annoKey = String(options?.annoKey || "").trim();
    if (!annoKey) return "";

    const titleText = String(options?.title || "Sửa ghi chú");
    const codeDesc = String(options?.codeDesc || "").trim();
    const userDesc = String(options?.userDesc || "").trim();
    const userNote = String(options?.userNote || "").trim();
    const annoType = String(options?.annoType || "").trim();
    const style = String(options?.style || "").trim();

    const attrs = options?.attrs && typeof options.attrs === "object" ? options.attrs : {};

    function toDataAttrs(field) {
      const parts = [];
      parts.push(`data-anno-type="${utils.escapeHtml(annoType)}"`);
      parts.push(`data-anno-key="${utils.escapeHtml(annoKey)}"`);
      parts.push(`data-anno-field="${utils.escapeHtml(field)}"`);

      for (const [nameRaw, valueRaw] of Object.entries(attrs)) {
        const name = String(nameRaw || "").trim();
        if (!name) continue;
        if (!name.startsWith("data-")) continue;
        if (!/^data-[a-z0-9-]+$/.test(name)) continue;
        parts.push(`${name}="${utils.escapeHtml(String(valueRaw ?? ""))}"`);
      }

      return parts.join(" ");
    }

    const styleAttr = style ? ` style="${utils.escapeHtml(style)}"` : "";

    return `<details class="param-notes"${styleAttr}>
        <summary class="param-notes__summary">${utils.escapeHtml(titleText)}</summary>
        <div class="param-notes__body">
          <div class="anno-grid">
            <div class="anno-label">Mô tả (từ code)</div>
            <div class="anno-code${codeDesc ? "" : " anno-code--empty"}">${utils.escapeHtml(codeDesc || "(không có)")}</div>

            <div class="anno-label">Mô tả của bạn</div>
            <textarea class="textarea param-notes__input" rows="2" ${toDataAttrs("description")} placeholder="Nhập mô tả (lưu cục bộ)...">${utils.escapeHtml(
              userDesc,
            )}</textarea>

            <div class="anno-label">Ghi chú của bạn</div>
            <textarea class="textarea param-notes__input" rows="3" ${toDataAttrs("note")} placeholder="Nhập ghi chú (lưu cục bộ)...">${utils.escapeHtml(
              userNote,
            )}</textarea>
          </div>
        </div>
      </details>`;
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
        const typeScopeKey = String(el.dataset.typeScopeKey || "");
        const typeName = String(el.dataset.typeName || "");
        const fieldPath = String(el.dataset.fieldPath || "");
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
        } else if (annoType === "typefield") {
          const sk = typeScopeKey || "PROGRAM";
          const tn = String(typeName || "").trim();
          const fp = String(fieldPath || "").trim();
          if (tn && fp && model.typeDefs && typeof model.typeDefs.get === "function") {
            const typeKey = `${sk}|${tn.toLowerCase()}`;
            const typeDef = model.typeDefs.get(typeKey) || null;
            const typeField = typeDef?.fields?.get ? typeDef.fields.get(fp.toLowerCase()) : null;
            applyUserFields(typeField);

            function syncVirtualDecls(list) {
              const arr = Array.isArray(list) ? list : [];
              for (const d of arr) {
                if (!d || !d.isVirtual) continue;
                const origin = d.virtualOrigin && typeof d.virtualOrigin === "object" ? d.virtualOrigin : null;
                if (!origin || origin.kind !== "typeField") continue;
                if (String(origin.typeScopeKey || "PROGRAM") !== sk) continue;
                if (String(origin.typeName || "").trim().toLowerCase() !== tn.toLowerCase()) continue;
                if (String(origin.fieldPath || "").trim().toLowerCase() !== fp.toLowerCase()) continue;
                applyUserFields(d);
              }
            }

            syncVirtualDecls(model.globalData);
            syncVirtualDecls(model.globalConstants);
            for (const node of model.nodes.values()) {
              syncVirtualDecls(node?.localData);
              syncVirtualDecls(node?.localConstants);
            }
          }
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

  function wireNotesEditorForKey(objectKey, applyLocal) {
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

      statusEl.textContent = "Đã lưu.";
      window.setTimeout(() => {
        if (statusEl.textContent === "Đã lưu.") statusEl.textContent = "";
      }, 1200);

      if (document.getElementById("tab-templates")?.classList.contains("is-active") && typeof ui.renderTemplates === "function") {
        ui.renderTemplates();
      }
    }

    function scheduleSave() {
      statusEl.textContent = "Đang lưu...";
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
  ui.renderInlineAnnoEditorHtml = renderInlineAnnoEditorHtml;
  ui.wireInlineAnnoEditors = wireInlineAnnoEditors;
  ui.wireNotesEditorForKey = wireNotesEditorForKey;
})(window.AbapFlow);
