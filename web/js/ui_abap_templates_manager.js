(function (ns) {
  "use strict";

  const ui = ns.ui;
  const utils = ns.utils;

  const state = {
    selectedId: "",
    lastListHash: "",
  };

  function $(id) {
    return ui.$(id);
  }

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function isPlainObject(x) {
    return Boolean(x) && typeof x === "object" && !Array.isArray(x);
  }

  function safeJsonParse(text) {
    if (utils && typeof utils.safeJsonParse === "function") return utils.safeJsonParse(text);
    try {
      return { ok: true, value: JSON.parse(String(text || "")) };
    } catch (e) {
      return { ok: false, error: String(e?.message || e) };
    }
  }

  function escapeHtml(s) {
    if (utils && typeof utils.escapeHtml === "function") return utils.escapeHtml(s);
    return String(s ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  function safeJsonStringify(value) {
    try {
      return JSON.stringify(value, null, 2);
    } catch (e) {
      return String(e?.message || e || "");
    }
  }

  function getDefs() {
    return ns.templateDefs || null;
  }

  function buildEntries() {
    const defs = getDefs();
    const store = defs?.loadStore ? defs.loadStore() : { templates: {} };
    const out = [];

    for (const [idRaw, entry] of Object.entries(store.templates || {})) {
      const id = asNonEmptyString(idRaw);
      if (!id || !isPlainObject(entry)) continue;
      const meta = isPlainObject(entry.meta) ? entry.meta : {};
      const label = asNonEmptyString(meta.label) || id;
      const source = asNonEmptyString(meta.source);
      const auto = meta.auto !== false;
      const custom = Boolean(meta.custom);
      const when = isPlainObject(meta.when) ? meta.when : null;
      const updatedAt = asNonEmptyString(entry.updatedAt);
      out.push({
        id,
        idLower: id.toLowerCase(),
        label,
        source,
        auto,
        custom,
        when,
        updatedAt,
        config: entry.config,
      });
    }

    out.sort((a, b) => a.id.localeCompare(b.id));
    return out;
  }

  function computeListHash(entries, query) {
    const q = asNonEmptyString(query).toLowerCase();
    return `${q}::${entries.map((e) => `${e.idLower}:${e.updatedAt || ""}:${e.custom ? 1 : 0}`).join("|")}`;
  }

  function listMatchesQuery(entry, query) {
    const q = asNonEmptyString(query).toLowerCase();
    if (!q) return true;
    const id = entry.idLower;
    const label = String(entry.label || "").toLowerCase();
    const source = String(entry.source || "").toLowerCase();
    return id.includes(q) || label.includes(q) || source.includes(q);
  }

  function setActionState(hasSelection) {
    const btnSave = $("abapTplSave");
    const btnDelete = $("abapTplDelete");
    if (btnSave) btnSave.disabled = !hasSelection;
    if (btnDelete) btnDelete.disabled = !hasSelection;
  }

  function renderList() {
    const host = $("abapTplList");
    if (!host) return;

    const entries = buildEntries();
    const q = $("abapTplSearch")?.value || "";
    const hash = computeListHash(entries, q);
    if (hash === state.lastListHash) return;
    state.lastListHash = hash;

    host.replaceChildren();

    const filtered = entries.filter((e) => listMatchesQuery(e, q));
    if (filtered.length === 0) {
      const empty = document.createElement("div");
      empty.className = "tplmgr__empty";
      empty.textContent = "(chưa có template trong localStorage)";
      host.appendChild(empty);
      return;
    }

    for (const e of filtered) {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "tplmgr__item";
      if (e.idLower === String(state.selectedId || "").toLowerCase()) btn.classList.add("is-active");
      if (e.custom) btn.classList.add("is-custom");

      const title = document.createElement("div");
      title.className = "tplmgr__item-title";
      title.textContent = e.id;

      const meta = document.createElement("div");
      meta.className = "tplmgr__item-meta";
      const origin = e.custom ? "tùy chỉnh" : "cục bộ";
      meta.textContent = `${origin}${e.source ? ` • ${e.source}` : ""}`;

      btn.appendChild(title);
      btn.appendChild(meta);
      btn.addEventListener("click", () => selectTemplate(e.id));
      host.appendChild(btn);
    }
  }

  function renderEditor() {
    const host = $("abapTplEditor");
    if (!host) return;

    const selected = asNonEmptyString(state.selectedId);
    if (!selected) {
      host.textContent = "Chọn template bên trái.";
      host.classList.add("empty");
      setActionState(false);
      return;
    }

    const entries = buildEntries();
    const entry = entries.find((e) => e.idLower === selected.toLowerCase()) || null;
    if (!entry) {
      host.textContent = "Không tìm thấy template.";
      host.classList.add("empty");
      setActionState(false);
      return;
    }

    host.classList.remove("empty");
    setActionState(true);

    const whenPath = asNonEmptyString(entry.when?.path);
    const whenEquals = asNonEmptyString(entry.when?.equals);
    const origin = entry.custom ? "tùy chỉnh" : "cục bộ";

    host.innerHTML = `
      <div class="tplmgr__head">
        <div class="tplmgr__title">Template: <code>${escapeHtml(entry.id)}</code></div>
        <div class="tplmgr__meta">${escapeHtml(origin)}${entry.source ? ` • ${escapeHtml(entry.source)}` : ""}</div>
      </div>
      <div class="tplmgr__fields">
        <label class="tplmgr__field">
          <span class="tplmgr__label">Id</span>
          <input id="abapTplId" class="input" type="text" value="${escapeHtml(entry.id)}" disabled />
        </label>
        <label class="tplmgr__field">
          <span class="tplmgr__label">Nhãn</span>
          <input id="abapTplLabel" class="input" type="text" value="${escapeHtml(entry.label)}" />
        </label>
        <label class="tplmgr__field">
          <span class="tplmgr__label">Object nguồn (source)</span>
          <input id="abapTplSource" class="input" type="text" value="${escapeHtml(entry.source)}" placeholder="vd: append" />
        </label>
        <label class="tplmgr__field tplmgr__field--checkbox">
          <span class="tplmgr__label">Auto</span>
          <input id="abapTplAuto" type="checkbox" ${entry.auto ? "checked" : ""} />
        </label>
        <label class="tplmgr__field">
          <span class="tplmgr__label">When.path</span>
          <input id="abapTplWhenPath" class="input" type="text" value="${escapeHtml(whenPath)}" placeholder="payload.mode" />
        </label>
        <label class="tplmgr__field">
          <span class="tplmgr__label">When.equals</span>
          <input id="abapTplWhenEquals" class="input" type="text" value="${escapeHtml(whenEquals)}" placeholder="BYTE" />
        </label>
        <label class="tplmgr__field tplmgr__field--wide">
          <span class="tplmgr__label">Template JSON</span>
          <textarea id="abapTplJson" class="objcfg-json tplmgr__json" spellcheck="false"></textarea>
        </label>
      </div>
      <div class="tplmgr__hint">
        Nếu để trống "Object nguồn", template chỉ override theo id và sẽ không tự thêm vào danh sách.
      </div>
    `;

    const jsonEl = $("abapTplJson");
    if (jsonEl) jsonEl.value = safeJsonStringify(entry.config || {});
  }

  function renderTemplateManager() {
    renderList();
    renderEditor();
  }

  function selectTemplate(id) {
    state.selectedId = asNonEmptyString(id);
    state.lastListHash = "";
    renderTemplateManager();
  }

  function createDefaultTemplateConfig() {
    const gen = ns.abapObjects?.syntaxGenerator;
    if (gen && typeof gen.createDefaultKeyValueExcelLikeTableTemplate === "function") {
      return gen.createDefaultKeyValueExcelLikeTableTemplate("pairs");
    }

    return {
      type: "excel-like-table",
      compact: { removeEmptyRows: true },
      grid: {
        rows: 2,
        cols: 2,
        colWidths: { A: 240, B: 640 },
        rowHeights: { 1: 30, 2: 30 },
      },
      css: {
        header: "background:#9dc3e6;font-weight:700;color:#111;",
        cell: "border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;",
        wrap: "white-space:normal;line-height:1.25;",
      },
      cells: [
        { addr: "A1", text: "Nội dung", class: ["cell", "header"] },
        { addr: "B1", text: "{line.description}", class: ["cell", "wrap"] },
        { addr: "A2", text: "Chi tiết", class: ["cell", "header"] },
        { addr: "B2", text: "{line.text}", class: ["cell", "wrap"] },
      ],
    };
  }

  function onNewTemplate() {
    const defs = getDefs();
    if (!defs?.upsertTemplate) {
      ui.setStatus("Chưa tải module templateDefs.", true);
      return;
    }

    const suggested = "new-template.excel-like-table";
    const id = asNonEmptyString(window.prompt("Template id mới:", suggested));
    if (!id) return;

    const entries = buildEntries();
    const existing = entries.find((e) => e.idLower === id.toLowerCase());
    if (existing) {
      const ok = window.confirm(`"${id}" đã tồn tại. Ghi đè?`);
      if (!ok) return;
    }

    const source = asNonEmptyString(window.prompt("Object nguồn (source) cho template (bỏ trống nếu chỉ override):", ""));
    const cfg = createDefaultTemplateConfig();
    const meta = {
      label: id,
      source,
      auto: true,
      custom: Boolean(source),
    };

    const saved = defs.upsertTemplate(id, cfg, meta);
    if (!saved.ok) {
      ui.setStatus(saved.error || "Lưu template thất bại.", true);
      return;
    }

    state.selectedId = id;
    state.lastListHash = "";
    ui.setStatus(`Đã tạo template: ${id}.`, false);
    renderTemplateManager();
  }

  function onSaveTemplate() {
    const defs = getDefs();
    if (!defs?.upsertTemplate) return;

    const id = asNonEmptyString($("abapTplId")?.value || state.selectedId);
    if (!id) {
      ui.setStatus("Cần template id.", true);
      return;
    }

    const label = asNonEmptyString($("abapTplLabel")?.value) || id;
    const source = asNonEmptyString($("abapTplSource")?.value);
    const auto = Boolean($("abapTplAuto")?.checked);
    const whenPath = asNonEmptyString($("abapTplWhenPath")?.value);
    const whenEquals = asNonEmptyString($("abapTplWhenEquals")?.value);

    const jsonEl = $("abapTplJson");
    const parsed = safeJsonParse(jsonEl?.value || "");
    if (!parsed.ok) {
      ui.setStatus(parsed.error || "JSON không hợp lệ.", true);
      return;
    }
    if (!isPlainObject(parsed.value)) {
      ui.setStatus("Template JSON phải là object.", true);
      return;
    }

    const when = whenPath && whenEquals ? { path: whenPath, equals: whenEquals } : null;
    const meta = {
      label,
      source,
      auto,
      custom: Boolean(source),
      ...(when ? { when } : {}),
    };

    const res = defs.upsertTemplate(id, parsed.value, meta);
    if (!res.ok) {
      ui.setStatus(res.error || "Lưu template thất bại.", true);
      return;
    }

    state.selectedId = id;
    state.lastListHash = "";
    ui.setStatus(`Đã lưu template: ${id}. Nhấn "Áp dụng" để nạp.`, false);
    renderTemplateManager();
  }

  function onDeleteTemplate() {
    const defs = getDefs();
    if (!defs?.deleteTemplate) return;

    const id = asNonEmptyString(state.selectedId);
    if (!id) return;
    const ok = window.confirm(`Xóa template "${id}"?`);
    if (!ok) return;

    const res = defs.deleteTemplate(id);
    if (!res.ok) {
      ui.setStatus(res.error || "Xóa thất bại.", true);
      return;
    }

    state.selectedId = "";
    state.lastListHash = "";
    ui.setStatus(`Đã xóa template: ${id}.`, false);
    renderTemplateManager();
  }

  function onApplyTemplates() {
    if (ns.abapObjects?.reset) ns.abapObjects.reset();
    ui.setStatus("Đã áp dụng template localStorage. Hãy phân tích lại để nạp.", false);
    const input = $("abapInput")?.value || "";
    if (String(input).trim()) $("btnAnalyze")?.click();
  }

  function exportTemplates() {
    const defs = getDefs();
    if (!defs?.exportJson) return;
    const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);
    ui.downloadTextFile(`abapflow-templates-${stamp}.json`, defs.exportJson(), "application/json");
    ui.setStatus("Đã xuất templates localStorage.", false);
  }

  async function importTemplatesFromFile(file) {
    const defs = getDefs();
    if (!defs?.importJson || !file) return;

    const text = await file.text();
    const res = defs.importJson(text, { mode: "merge" });
    if (!res.ok) {
      ui.setStatus(res.error || "Nhập thất bại.", true);
      return;
    }

    state.lastListHash = "";
    ui.setStatus("Đã nhập templates (gộp).", false);
    renderTemplateManager();
  }

  function resetTemplates() {
    const defs = getDefs();
    if (!defs?.clearStore) return;
    const ok = window.confirm("Xóa toàn bộ template localStorage?");
    if (!ok) return;
    defs.clearStore();
    state.selectedId = "";
    state.lastListHash = "";
    ui.setStatus("Đã xóa templates localStorage.", false);
    renderTemplateManager();
  }

  function bindOnce() {
    const mgr = $("abapTplMgr");
    if (!mgr || mgr.dataset.bound) return;
    mgr.dataset.bound = "1";

    $("abapTplSearch")?.addEventListener("input", () => {
      state.lastListHash = "";
      renderList();
    });
    $("abapTplNew")?.addEventListener("click", onNewTemplate);
    $("abapTplSave")?.addEventListener("click", onSaveTemplate);
    $("abapTplDelete")?.addEventListener("click", onDeleteTemplate);
    $("abapTplApply")?.addEventListener("click", onApplyTemplates);
    $("abapTplExport")?.addEventListener("click", exportTemplates);
    $("abapTplImport")?.addEventListener("click", () => $("abapTplImportFile")?.click());
    $("abapTplReset")?.addEventListener("click", resetTemplates);
    $("abapTplImportFile")?.addEventListener("change", async (ev) => {
      const input = ev.target;
      const file = input?.files?.[0] || null;
      await importTemplatesFromFile(file);
      if (input) input.value = "";
    });
  }

  function hookRender() {
    if (!ui?.renderAbapObjectsConfig || ui.renderAbapObjectsConfig._tplMgrHooked) return;
    const prev = ui.renderAbapObjectsConfig;
    ui.renderAbapObjectsConfig = function renderAbapObjectsConfigWrapped() {
      prev.apply(this, arguments);
      renderTemplateManager();
    };
    ui.renderAbapObjectsConfig._tplMgrHooked = true;
  }

  function init() {
    bindOnce();
    hookRender();
    renderTemplateManager();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})(window.AbapFlow);
