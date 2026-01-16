(function (ns) {
  "use strict";

  const store = ns.templateDefs || null;
  let selectedAddr = "";
  const EDITOR_SPLIT_KEY = "abapflow-template-editor-left-px";

  function $(id) {
    return document.getElementById(id);
  }

  function asNonEmptyString(x) {
    const s = String(x ?? "").trim();
    return s ? s : "";
  }

  function setStatus(message, isError) {
    const el = $("statusBar");
    if (!el) return;
    el.textContent = String(message || "");
    el.style.color = isError ? "#fecaca" : "";
  }

  function parseJsonStrict(text) {
    const raw = String(text || "").trim();
    if (!raw) return { ok: false, error: "Empty JSON." };
    try {
      return { ok: true, value: JSON.parse(raw) };
    } catch (e) {
      return { ok: false, error: String(e?.message || e) };
    }
  }

  function validateTemplateConfig(cfg) {
    if (!cfg || typeof cfg !== "object") return "Template config must be an object.";
    if (cfg.type !== "excel-like-table") return 'Only type "excel-like-table" is supported.';
    return "";
  }

  function normalizeAddress(addr) {
    return String(addr || "").trim().toUpperCase();
  }

  function colToIndex(colLetters) {
    const s = String(colLetters || "").trim().toUpperCase();
    if (!/^[A-Z]+$/.test(s)) return 0;
    let n = 0;
    for (let i = 0; i < s.length; i++) n = n * 26 + (s.charCodeAt(i) - 64);
    return n;
  }

  function indexToCol(index1) {
    let n = Number(index1);
    if (!Number.isFinite(n) || n <= 0) return "";
    n = Math.floor(n);
    let s = "";
    while (n > 0) {
      const rem = (n - 1) % 26;
      s = String.fromCharCode(65 + rem) + s;
      n = Math.floor((n - 1) / 26);
    }
    return s;
  }

  function parseAddress(addr) {
    const s = normalizeAddress(addr);
    const m = /^([A-Z]+)(\d+)$/.exec(s);
    if (!m) return null;
    const colIndex = colToIndex(m[1]);
    const row = Number(m[2]);
    if (!Number.isFinite(colIndex) || !Number.isFinite(row) || colIndex <= 0 || row <= 0) return null;
    return { col: m[1], colIndex, row };
  }

  function addrFromRC(colIndex, row) {
    return `${indexToCol(colIndex)}${row}`;
  }

  function listRegistryTemplates() {
    const templates =
      ns.templateRegistry?.templates && typeof ns.templateRegistry.templates === "object" ? ns.templateRegistry.templates : {};
    const order = Array.isArray(ns.templateRegistry?.order)
      ? ns.templateRegistry.order
      : Object.keys(templates).sort((a, b) => a.localeCompare(b));

    const out = [];
    for (const id of order) {
      const t = templates[id];
      if (!t || typeof t !== "object") continue;
      out.push({ ...t, id: String(t.id || id) });
    }
    return out;
  }

  function listAllTemplates() {
    const registry = listRegistryTemplates();
    const map = new Map(registry.map((t) => [t.id, { ...t, _origin: "registry" }]));

    const custom = store && typeof store.listCustomTemplateEntries === "function" ? store.listCustomTemplateEntries() : [];
    for (const t of custom) {
      if (!t || typeof t !== "object") continue;
      const id = asNonEmptyString(t.id);
      if (!id || map.has(id)) continue;
      map.set(id, { ...t, _origin: "store" });
    }

    return Array.from(map.values()).sort((a, b) => String(a.id || "").localeCompare(String(b.id || "")));
  }

  function populateTemplateSelect(selectedId) {
    const sel = $("tplSelect");
    if (!sel) return;

    const templates = listAllTemplates();
    sel.replaceChildren();

    for (const t of templates) {
      const opt = document.createElement("option");
      opt.value = String(t.id || "");

      const label = String(t.label || "").trim();
      const src = String(t.source || t.objectId || "").trim();
      const parts = [opt.value];
      if (label && label !== opt.value) parts.push(`- ${label}`);
      if (src) parts.push(`(${src})`);
      opt.textContent = parts.join(" ");

      sel.appendChild(opt);
    }

    const want = asNonEmptyString(selectedId);
    if (want && Array.from(sel.options).some((o) => o.value === want)) sel.value = want;
  }

  function populateSourceSelect(selectedId) {
    const sel = $("tplSource");
    if (!sel) return;

    const reg = ns.abapObjects?.getRegistry?.() || null;
    const ids = reg?.objectsById?.keys ? Array.from(reg.objectsById.keys()) : [];
    ids.sort((a, b) => String(a).localeCompare(String(b)));

    sel.replaceChildren();
    for (const id of ids) {
      const opt = document.createElement("option");
      opt.value = String(id || "");
      opt.textContent = opt.value;
      sel.appendChild(opt);
    }

    const want = asNonEmptyString(selectedId);
    if (want && Array.from(sel.options).some((o) => o.value === want)) sel.value = want;
  }

  function getTemplateMeta(templateId) {
    const id = asNonEmptyString(templateId);
    const regEntry = ns.templateRegistry?.templates?.[id] || null;
    const stored = store && typeof store.loadStore === "function" ? store.loadStore().templates?.[id] || null : null;
    const meta = stored?.meta || {};

    const label = asNonEmptyString(meta.label) || asNonEmptyString(regEntry?.label) || id;
    const source = asNonEmptyString(meta.source) || asNonEmptyString(regEntry?.source) || asNonEmptyString(regEntry?.objectId);
    const auto = meta.auto != null ? meta.auto !== false : regEntry?.auto !== false;
    const when = meta.when || regEntry?.when || null;
    const custom = Boolean(meta.custom) || (!regEntry && Boolean(stored));

    return { id, label, source, auto, when, custom };
  }

  function getTemplateConfig(templateId) {
    const id = asNonEmptyString(templateId);
    if (!id) return null;

    const override = store && typeof store.getTemplateConfig === "function" ? store.getTemplateConfig(id) : null;
    if (override) return override;

    return ns.templateRegistry?.templates?.[id]?.config || null;
  }

  function loadTemplate(templateId) {
    const id = asNonEmptyString(templateId);
    if (!id) return;

    const cfg = getTemplateConfig(id);
    if (!cfg) {
      setStatus(`Template config not found: ${id}`, true);
      return;
    }

    const meta = getTemplateMeta(id);

    $("tplId").value = meta.id;
    $("tplLabel").value = meta.label;
    populateSourceSelect(meta.source);

    $("tplAuto").checked = meta.auto !== false;
    $("tplWhenPath").value = String(meta.when?.path || "");
    $("tplWhenEquals").value = String(meta.when?.equals || "");

    const preferredId = store && typeof store.getPreferredTemplateId === "function" ? store.getPreferredTemplateId(meta.source) : "";
    $("tplPreferred").checked = preferredId === id;

    writeTemplateJson(cfg);
    renderPreview();

    setStatus(`Loaded: ${id}`, false);
  }

  function buildWhenFromForm() {
    const path = asNonEmptyString($("tplWhenPath")?.value);
    const equals = asNonEmptyString($("tplWhenEquals")?.value);
    if (!path || !equals) return null;
    return { path, equals };
  }

  function readTemplateJsonFromEditor() {
    const parsed = parseJsonStrict($("tplJson")?.value);
    if (!parsed.ok) return parsed;

    const err = validateTemplateConfig(parsed.value);
    if (err) return { ok: false, error: err };
    return parsed;
  }

  function syncCompactFormFromConfig(cfg) {
    const el = $("tplCompactRemoveEmptyRows");
    if (!el) return;
    el.checked = Boolean(cfg?.compact?.removeEmptyRows);
  }

  function syncCompactFormFromEditorJson() {
    const parsed = readTemplateJsonFromEditor();
    if (!parsed.ok) return;
    syncCompactFormFromConfig(parsed.value);
  }

  function applyCompactFormToConfig(cfg) {
    if (!cfg || typeof cfg !== "object") return;
    const el = $("tplCompactRemoveEmptyRows");
    if (!el) return;

    if (el.checked) {
      if (!cfg.compact || typeof cfg.compact !== "object") cfg.compact = {};
      cfg.compact.removeEmptyRows = true;
      return;
    }

    if (cfg.compact && typeof cfg.compact === "object") {
      delete cfg.compact.removeEmptyRows;
      if (!Object.keys(cfg.compact).length) delete cfg.compact;
    }
  }

  function writeTemplateJson(cfg) {
    $("tplJson").value = JSON.stringify(cfg, null, 2);
    syncCompactFormFromConfig(cfg);
  }

  function upsertCellText(cfg, addrRaw, textValue) {
    if (!cfg || typeof cfg !== "object") return;
    cfg.cells = Array.isArray(cfg.cells) ? cfg.cells : [];

    const addr = normalizeAddress(addrRaw);
    if (!addr) return;

    const text = String(textValue ?? "");
    const idx = cfg.cells.findIndex((c) => normalizeAddress(c?.addr) === addr);
    if (idx >= 0) {
      cfg.cells[idx] = { ...(cfg.cells[idx] || {}), addr, text };
      return;
    }
    cfg.cells.push({ addr, text });
  }

  function parseStyleDeclarations(styleText) {
    const map = new Map();
    const raw = String(styleText || "").trim();
    if (!raw) return map;

    const parts = raw.split(";");
    for (const part of parts) {
      const t = String(part || "").trim();
      if (!t) continue;
      const idx = t.indexOf(":");
      if (idx <= 0) continue;
      const prop = t.slice(0, idx).trim().toLowerCase();
      const value = t.slice(idx + 1).trim();
      if (!prop) continue;
      map.set(prop, value);
    }
    return map;
  }

  function serializeStyleDeclarations(map) {
    if (!map || map.size === 0) return "";
    return Array.from(map.entries())
      .map(([k, v]) => `${k}:${v}`)
      .join(";")
      .concat(";");
  }

  function upsertCellStyleProp(cfg, addrRaw, propRaw, valueRaw) {
    if (!cfg || typeof cfg !== "object") return;
    cfg.cells = Array.isArray(cfg.cells) ? cfg.cells : [];

    const addr = normalizeAddress(addrRaw);
    if (!addr) return;

    const prop = String(propRaw || "").trim().toLowerCase();
    if (!prop) return;
    const value = String(valueRaw ?? "").trim();

    const idx = cfg.cells.findIndex((c) => normalizeAddress(c?.addr) === addr);
    const existing = idx >= 0 ? cfg.cells[idx] : null;
    const next = { ...(existing || {}), addr };

    const decls = parseStyleDeclarations(next.style);
    if (!value) decls.delete(prop);
    else decls.set(prop, value);

    const style = serializeStyleDeclarations(decls);
    if (style) next.style = style;
    else delete next.style;

    if (idx >= 0) cfg.cells[idx] = next;
    else cfg.cells.push(next);
  }

  function clearCellStyle(cfg, addrRaw) {
    if (!cfg || typeof cfg !== "object") return;
    cfg.cells = Array.isArray(cfg.cells) ? cfg.cells : [];
    const addr = normalizeAddress(addrRaw);
    if (!addr) return;
    const idx = cfg.cells.findIndex((c) => normalizeAddress(c?.addr) === addr);
    if (idx < 0) return;
    const existing = cfg.cells[idx] || {};
    if (existing.style == null) return;
    const next = { ...existing };
    delete next.style;
    cfg.cells[idx] = next;
  }

  function hex2(x) {
    return String(x || "").padStart(2, "0");
  }

  function rgbToHex(r, g, b) {
    const rr = Math.max(0, Math.min(255, Math.floor(Number(r))));
    const gg = Math.max(0, Math.min(255, Math.floor(Number(g))));
    const bb = Math.max(0, Math.min(255, Math.floor(Number(b))));
    return `#${hex2(rr.toString(16))}${hex2(gg.toString(16))}${hex2(bb.toString(16))}`.toLowerCase();
  }

  function parseCssColorToHex(colorText, fallback) {
    const raw = String(colorText || "").trim();
    if (!raw) return fallback || "";
    const s = raw.toLowerCase();
    if (s === "transparent") return fallback || "";

    const hex = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.exec(raw);
    if (hex) {
      const h = hex[1];
      if (h.length === 3) return `#${h[0]}${h[0]}${h[1]}${h[1]}${h[2]}${h[2]}`.toLowerCase();
      return `#${h}`.toLowerCase();
    }

    const m = /^rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)/i.exec(s);
    if (m) return rgbToHex(m[1], m[2], m[3]);

    return fallback || "";
  }

  function setPreviewToolbarEnabled(enabled) {
    const on = Boolean(enabled);
    const ids = ["btnPreviewDeleteRow", "btnPreviewDeleteCol", "btnClearFormat", "selBgColor", "selTextColor", "selBorder", "btnApplyBorder"];
    for (const id of ids) {
      const el = $(id);
      if (el) el.disabled = !on;
    }
  }

  function syncPreviewToolbarFromSelection() {
    const label = $("selAddr");
    if (label) label.textContent = selectedAddr || "-";

    if (!selectedAddr) {
      setPreviewToolbarEnabled(false);
      return;
    }

    const host = $("previewHost");
    const td = host ? host.querySelector(`td[data-addr="${selectedAddr}"]`) : null;
    if (!td || !window.getComputedStyle) {
      setPreviewToolbarEnabled(false);
      return;
    }

    const css = window.getComputedStyle(td);
    const bg = parseCssColorToHex(css.backgroundColor, "#ffffff");
    const fg = parseCssColorToHex(css.color, "#111111");

    const bgEl = $("selBgColor");
    const fgEl = $("selTextColor");
    const borderEl = $("selBorder");

    if (bgEl && bg) bgEl.value = bg;
    if (fgEl && fg) fgEl.value = fg;
    if (borderEl) borderEl.value = String(css.border || "").trim();

    setPreviewToolbarEnabled(true);
  }

  function initEditorSplitter() {
    const main = document.querySelector(".app-main.editor-main");
    const splitter = $("editorSplitter");
    const leftPanel = document.querySelector(".panel.panel-left");

    if (!main || !splitter || !leftPanel) return;

    function readStored() {
      try {
        const n = Number(localStorage.getItem(EDITOR_SPLIT_KEY));
        return Number.isFinite(n) && n > 0 ? n : 0;
      } catch (_) {
        return 0;
      }
    }

    function writeStored(px) {
      try {
        localStorage.setItem(EDITOR_SPLIT_KEY, String(Math.round(px)));
      } catch (_) {}
    }

    function clampLeft(px) {
      const mainW = main.getBoundingClientRect().width;
      const splitterW = splitter.getBoundingClientRect().width;
      const minLeft = 320;
      const minRight = 320;
      const maxLeft = Math.max(minLeft, mainW - minRight - splitterW);
      return Math.max(minLeft, Math.min(maxLeft, px));
    }

    function applyLeft(px) {
      main.style.setProperty("--editor-left", `${Math.round(px)}px`);
    }

    const stored = readStored();
    if (stored) applyLeft(clampLeft(stored));

    let dragging = false;
    let startX = 0;
    let startLeft = 0;
    let lastApplied = 0;

    function onMove(e) {
      if (!dragging) return;
      const dx = Number(e.clientX) - startX;
      const next = clampLeft(startLeft + dx);
      if (Math.abs(next - lastApplied) < 1) return;
      lastApplied = next;
      applyLeft(next);
    }

    function stopDrag() {
      if (!dragging) return;
      dragging = false;
      splitter.classList.remove("is-dragging");
      document.body.style.cursor = "";
      document.body.style.userSelect = "";
      if (lastApplied) writeStored(lastApplied);
      window.removeEventListener("pointermove", onMove);
      window.removeEventListener("pointerup", stopDrag);
      window.removeEventListener("pointercancel", stopDrag);
    }

    splitter.addEventListener("pointerdown", (e) => {
      if (!e || typeof e.clientX !== "number") return;
      dragging = true;
      splitter.classList.add("is-dragging");
      splitter.setPointerCapture?.(e.pointerId);
      startX = e.clientX;
      startLeft = leftPanel.getBoundingClientRect().width;
      lastApplied = startLeft;
      document.body.style.cursor = "col-resize";
      document.body.style.userSelect = "none";
      window.addEventListener("pointermove", onMove);
      window.addEventListener("pointerup", stopDrag);
      window.addEventListener("pointercancel", stopDrag);
    });

    splitter.addEventListener("keydown", (e) => {
      if (!e) return;
      if (e.key !== "ArrowLeft" && e.key !== "ArrowRight") return;
      e.preventDefault();
      const delta = e.key === "ArrowLeft" ? -24 : 24;
      const cur = leftPanel.getBoundingClientRect().width;
      const next = clampLeft(cur + delta);
      applyLeft(next);
      writeStored(next);
    });

    window.addEventListener("resize", () => {
      const cur = leftPanel.getBoundingClientRect().width;
      const next = clampLeft(cur);
      if (Math.abs(next - cur) >= 1) applyLeft(next);
    });
  }

  function shiftRowHeightsOnDelete(rowHeights, delRow) {
    const src = rowHeights && typeof rowHeights === "object" ? rowHeights : {};
    const out = {};
    for (const [k, v] of Object.entries(src)) {
      const r = Number(k);
      if (!Number.isFinite(r) || r <= 0) continue;
      if (r === delRow) continue;
      const nr = r > delRow ? r - 1 : r;
      out[String(nr)] = v;
    }
    return out;
  }

  function shiftColWidthsOnDelete(colWidths, delColIndex) {
    const src = colWidths && typeof colWidths === "object" ? colWidths : {};
    const out = {};
    for (const [k, v] of Object.entries(src)) {
      const idx = colToIndex(k);
      if (!idx) continue;
      if (idx === delColIndex) continue;
      const nidx = idx > delColIndex ? idx - 1 : idx;
      const nk = indexToCol(nidx);
      if (!nk) continue;
      out[nk] = v;
    }
    return out;
  }

  function deleteRow(cfg, delRow) {
    if (!cfg || cfg.type !== "excel-like-table") return { ok: false, error: "Invalid template config." };
    const grid = cfg.grid && typeof cfg.grid === "object" ? cfg.grid : null;
    if (!grid) return { ok: false, error: "Missing grid." };

    const rows = Math.max(1, Math.floor(Number(grid.rows || 1)));
    const cols = Math.max(1, Math.floor(Number(grid.cols || 1)));
    const r = Math.floor(Number(delRow));
    if (!Number.isFinite(r) || r < 1 || r > rows) return { ok: false, error: `Row out of range: ${delRow}` };

    const cells = Array.isArray(cfg.cells) ? cfg.cells : [];
    cfg.cells = cells
      .map((c) => {
        const a = parseAddress(c?.addr);
        if (!a) return c;
        if (a.row === r) return null;
        const nr = a.row > r ? a.row - 1 : a.row;
        return { ...c, addr: addrFromRC(a.colIndex, nr) };
      })
      .filter(Boolean);

    grid.rowHeights = shiftRowHeightsOnDelete(grid.rowHeights, r);
    grid.rows = Math.max(1, rows - 1);
    cfg.merges = [];

    return { ok: true, row: r };
  }

  function deleteCol(cfg, delColIndex) {
    if (!cfg || cfg.type !== "excel-like-table") return { ok: false, error: "Invalid template config." };
    const grid = cfg.grid && typeof cfg.grid === "object" ? cfg.grid : null;
    if (!grid) return { ok: false, error: "Missing grid." };

    const rows = Math.max(1, Math.floor(Number(grid.rows || 1)));
    const cols = Math.max(1, Math.floor(Number(grid.cols || 1)));
    const cidx = Math.floor(Number(delColIndex));
    if (!Number.isFinite(cidx) || cidx < 1 || cidx > cols) return { ok: false, error: `Column out of range: ${delColIndex}` };

    const cells = Array.isArray(cfg.cells) ? cfg.cells : [];
    cfg.cells = cells
      .map((c) => {
        const a = parseAddress(c?.addr);
        if (!a) return c;
        if (a.colIndex === cidx) return null;
        const nc = a.colIndex > cidx ? a.colIndex - 1 : a.colIndex;
        return { ...c, addr: addrFromRC(nc, a.row) };
      })
      .filter(Boolean);

    grid.colWidths = shiftColWidthsOnDelete(grid.colWidths, cidx);
    grid.cols = Math.max(1, cols - 1);
    cfg.merges = [];

    return { ok: true, colIndex: cidx, col: indexToCol(cidx) };
  }

  function highlightSelectedCell() {
    const host = $("previewHost");
    if (!host) return;
    host.querySelectorAll("td.is-selected").forEach((td) => td.classList.remove("is-selected"));
    if (!selectedAddr) return;
    const td = host.querySelector(`td[data-addr="${selectedAddr}"]`);
    if (td) td.classList.add("is-selected");
  }

  function selectCell(addr) {
    selectedAddr = normalizeAddress(addr);
    highlightSelectedCell();
    syncPreviewToolbarFromSelection();
    if (selectedAddr) setStatus(`Selected: ${selectedAddr}`, false);
  }

  function cancelCellEdit(td) {
    td?.classList?.remove("is-editing");
    renderPreview();
    highlightSelectedCell();
  }

  function startCellEdit(td, addr) {
    const host = $("previewHost");
    if (!host || !td) return;

    const parsed = readTemplateJsonFromEditor();
    if (!parsed.ok) {
      setStatus(String(parsed.error || "Invalid template JSON."), true);
      return;
    }

    const cfg = parsed.value;
    const cellAddr = normalizeAddress(addr);
    if (!cellAddr) return;
    selectedAddr = cellAddr;

    const existing = Array.isArray(cfg.cells) ? cfg.cells.find((c) => normalizeAddress(c?.addr) === cellAddr) : null;
    const initial = existing ? String(existing.text ?? "") : "";

    host.querySelectorAll("td.is-editing").forEach((x) => x.classList.remove("is-editing"));
    td.classList.add("is-editing");
    td.textContent = "";

    const editor = document.createElement("textarea");
    editor.className = "cell-editor";
    editor.value = initial;
    editor.rows = 1;
    td.appendChild(editor);
    editor.focus();
    editor.setSelectionRange(editor.value.length, editor.value.length);

    let finished = false;
    function finish(commit) {
      if (finished) return;
      finished = true;

      if (!commit) {
        cancelCellEdit(td);
        return;
      }

      const nextCfg = readTemplateJsonFromEditor();
      if (!nextCfg.ok) {
        setStatus(String(nextCfg.error || "Invalid template JSON."), true);
        cancelCellEdit(td);
        return;
      }

      upsertCellText(nextCfg.value, cellAddr, editor.value);
      writeTemplateJson(nextCfg.value);
      renderPreview();
      highlightSelectedCell();
      setStatus(`Updated: ${cellAddr}`, false);
    }

    editor.addEventListener("keydown", (e) => {
      if (e.key === "Escape") {
        e.preventDefault();
        finish(false);
        return;
      }
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        finish(true);
      }
    });
    editor.addEventListener("blur", () => finish(true));
  }

  function deleteSelectedRow() {
    const parsedSel = parseAddress(selectedAddr);
    const row = parsedSel?.row;
    if (!row) {
      const raw = window.prompt("Row number to delete:", "");
      const n = Math.floor(Number(raw));
      if (!Number.isFinite(n) || n <= 0) {
        setStatus("Invalid row number.", true);
        return;
      }
      selectedAddr = `A${n}`;
      return deleteSelectedRow();
    }

    const parsed = readTemplateJsonFromEditor();
    if (!parsed.ok) {
      setStatus(String(parsed.error || "Invalid template JSON."), true);
      return;
    }

    const rows = Math.max(1, Math.floor(Number(parsed.value?.grid?.rows || 1)));
    if (row > rows) {
      setStatus("Row is outside template grid. Clear Preview context and try again.", true);
      return;
    }

    if (!window.confirm(`Delete row ${row}?`)) return;
    const res = deleteRow(parsed.value, row);
    if (!res.ok) {
      setStatus(String(res.error || "Delete row failed."), true);
      return;
    }
    writeTemplateJson(parsed.value);
    selectedAddr = "";
    renderPreview();
    setStatus(`Deleted row ${row}.`, false);
  }

  function deleteSelectedCol() {
    const parsedSel = parseAddress(selectedAddr);
    let colIndex = parsedSel?.colIndex;

    if (!colIndex) {
      const raw = window.prompt("Column letter to delete (A, B, C...):", "");
      const idx = colToIndex(raw);
      if (!idx) {
        setStatus("Invalid column letter.", true);
        return;
      }
      selectedAddr = `${indexToCol(idx)}1`;
      colIndex = idx;
    }

    const parsed = readTemplateJsonFromEditor();
    if (!parsed.ok) {
      setStatus(String(parsed.error || "Invalid template JSON."), true);
      return;
    }

    const cols = Math.max(1, Math.floor(Number(parsed.value?.grid?.cols || 1)));
    if (colIndex > cols) {
      setStatus("Column is outside template grid. Clear Preview context and try again.", true);
      return;
    }

    const colLetter = indexToCol(colIndex);
    if (!window.confirm(`Delete column ${colLetter}?`)) return;
    const res = deleteCol(parsed.value, colIndex);
    if (!res.ok) {
      setStatus(String(res.error || "Delete column failed."), true);
      return;
    }
    writeTemplateJson(parsed.value);
    selectedAddr = "";
    renderPreview();
    setStatus(`Deleted column ${colLetter}.`, false);
  }

  function renderPreview() {
    const host = $("previewHost");
    if (!host) return;

    const parsed = readTemplateJsonFromEditor();
    if (!parsed.ok) {
      host.textContent = String(parsed.error || "Invalid template JSON.");
      host.classList.add("empty");
      setStatus(String(parsed.error || "Invalid template JSON."), true);
      return;
    }

    let cfg = parsed.value;

    const ctxRaw = asNonEmptyString($("ctxJson")?.value);
    if (ctxRaw) {
      const ctxParsed = parseJsonStrict(ctxRaw);
      if (!ctxParsed.ok) {
        setStatus(`Context JSON error: ${ctxParsed.error}`, true);
      } else if (ns.templateConverter) {
        try {
          const expanded = ns.templateConverter.expandExcelLikeTableTemplate(cfg, ctxParsed.value);
          const filled = ns.templateConverter.fillTemplateConfig(expanded, ctxParsed.value);
          cfg = ns.templateConverter.compactExcelLikeTableConfig(filled);
        } catch (e) {
          setStatus(`Preview build error: ${String(e?.message || e)}`, true);
        }
      }
    }

    try {
      host.classList.remove("empty");
      host.textContent = "";
      host.appendChild(ns.tableRenderer.renderExcelLikeTable(cfg));
      highlightSelectedCell();
      syncPreviewToolbarFromSelection();
    } catch (e) {
      host.textContent = String(e?.message || e);
      host.classList.add("empty");
      setStatus(`Preview render error: ${String(e?.message || e)}`, true);
    }
  }

  let previewTimer = null;
  function schedulePreview() {
    if (previewTimer) window.clearTimeout(previewTimer);
    previewTimer = window.setTimeout(() => {
      previewTimer = null;
      renderPreview();
    }, 250);
  }

  function newTemplate() {
    $("tplId").value = "";
    $("tplLabel").value = "";
    $("tplAuto").checked = true;
    $("tplPreferred").checked = false;
    $("tplCompactRemoveEmptyRows").checked = false;
    $("tplWhenPath").value = "";
    $("tplWhenEquals").value = "";
    if ($("tplSource")?.options?.length) $("tplSource").value = $("tplSource").options[0].value;
    $("ctxJson").value = "";

    const skeleton = {
      type: "excel-like-table",
      grid: {
        rows: 8,
        cols: 8,
        defaultColWidth: 32,
        defaultRowHeight: 32,
      },
      css: {
        cell: "border:1px solid #222;padding:2px 4px;vertical-align:middle;background:#fff;color:#111;white-space:pre-wrap;",
      },
      merges: [],
      cells: [{ addr: "A1", text: "Title", class: ["cell"] }],
    };
    writeTemplateJson(skeleton);
    renderPreview();
    setStatus("New template skeleton created.", false);
  }

  function saveTemplate(mode) {
    if (!store || typeof store.upsertTemplate !== "function") {
      setStatus("Template store not available.", true);
      return;
    }

    const selectedId = asNonEmptyString($("tplSelect")?.value);
    const id = mode === "new" ? asNonEmptyString($("tplId")?.value) : selectedId;
    if (!id) {
      setStatus("Template id is required.", true);
      return;
    }

    const parsed = readTemplateJsonFromEditor();
    if (!parsed.ok) {
      setStatus(String(parsed.error || "Invalid template JSON."), true);
      return;
    }

    const metaFromForm = {
      label: asNonEmptyString($("tplLabel")?.value) || id,
      source: asNonEmptyString($("tplSource")?.value),
      auto: Boolean($("tplAuto")?.checked),
      when: buildWhenFromForm(),
    };

    const regEntry = ns.templateRegistry?.templates?.[id] || null;
    const stored = store.loadStore?.().templates?.[id] || null;
    const isCustom = Boolean(stored?.meta?.custom) || (!regEntry && mode === "new");

    const res = store.upsertTemplate(id, parsed.value, { ...metaFromForm, custom: isCustom });
    if (!res.ok) {
      setStatus(String(res.error || "Save failed."), true);
      return;
    }

    const preferred = Boolean($("tplPreferred")?.checked);
    if (preferred && metaFromForm.source) store.setPreferredTemplateId?.(metaFromForm.source, id);
    if (!preferred && metaFromForm.source) {
      const cur = store.getPreferredTemplateId?.(metaFromForm.source);
      if (cur === id) store.setPreferredTemplateId?.(metaFromForm.source, "");
    }

    populateTemplateSelect(id);
    if ($("tplSelect")?.value !== id) $("tplSelect").value = id;
    setStatus(`Saved (${mode === "new" ? "new" : "override"}): ${id}. Reload pages to apply.`, false);
  }

  function deleteSaved() {
    if (!store || typeof store.deleteTemplate !== "function") {
      setStatus("Template store not available.", true);
      return;
    }

    const id = asNonEmptyString($("tplSelect")?.value);
    if (!id) return;

    if (!window.confirm(`Delete saved template/override: ${id}?`)) return;
    const res = store.deleteTemplate(id);
    if (!res.ok) {
      setStatus(String(res.error || "Delete failed."), true);
      return;
    }

    populateTemplateSelect("");
    setStatus(res.existed ? `Deleted: ${id}. Reload pages to apply.` : `Nothing to delete for: ${id}`, false);
  }

  function wireUi() {
    $("btnLoadTemplate")?.addEventListener("click", () => loadTemplate($("tplSelect")?.value));
    $("btnNewTemplate")?.addEventListener("click", newTemplate);
    $("btnSaveTemplate")?.addEventListener("click", () => saveTemplate("save"));
    $("btnSaveNewTemplate")?.addEventListener("click", () => saveTemplate("new"));
    $("btnDeleteTemplate")?.addEventListener("click", deleteSaved);
    $("btnDeleteRow")?.addEventListener("click", deleteSelectedRow);
    $("btnDeleteCol")?.addEventListener("click", deleteSelectedCol);
    $("tplCompactRemoveEmptyRows")?.addEventListener("change", () => {
      const parsed = readTemplateJsonFromEditor();
      if (!parsed.ok) {
        setStatus(String(parsed.error || "Invalid template JSON."), true);
        return;
      }

      applyCompactFormToConfig(parsed.value);
      writeTemplateJson(parsed.value);
      renderPreview();
      setStatus(`Compact removeEmptyRows: ${$("tplCompactRemoveEmptyRows").checked ? "ON" : "OFF"}.`, false);
    });
    $("btnCopyExcel")?.addEventListener("click", async () => {
      const exporter = ns.templateExcelExport || null;
      const table = $("previewHost")?.querySelector("table.excel-like-table") || null;
      const res = exporter?.copyExcelLikeTableToClipboard ? await exporter.copyExcelLikeTableToClipboard(table) : { ok: false, error: "Excel export module not loaded." };
      setStatus(String(res.ok ? `Copied to clipboard (${res.method || "copy"}). Paste into Excel.` : res.error || "Copy failed."), !res.ok);
    });
    $("btnPasteExcel")?.addEventListener("click", () => {
      const modal = $("excelPasteModal");
      const target = $("excelPasteTarget");
      if (!modal || !target) return;
      modal.hidden = false;
      target.textContent = "";
      window.setTimeout(() => target.focus(), 0);
    });
    $("btnExcelPasteCancel")?.addEventListener("click", () => {
      const modal = $("excelPasteModal");
      if (modal) modal.hidden = true;
    });
    $("excelPasteModal")?.addEventListener("click", (e) => {
      const modal = $("excelPasteModal");
      if (!modal) return;
      const panel = e?.target?.closest ? e.target.closest(".excel-paste-modal__panel") : null;
      if (panel) return;
      modal.hidden = true;
    });
    $("excelPasteTarget")?.addEventListener("keydown", (e) => {
      if (e.key !== "Escape") return;
      const modal = $("excelPasteModal");
      if (modal) modal.hidden = true;
    });
    $("excelPasteTarget")?.addEventListener("paste", (e) => {
      const ev = e || window.event;
      const html = ev?.clipboardData?.getData ? ev.clipboardData.getData("text/html") : "";
      const plain = ev?.clipboardData?.getData ? ev.clipboardData.getData("text/plain") : "";
      if (ev?.preventDefault) ev.preventDefault();

      const importer = ns.templateExcelImport || null;
      if (!importer?.buildTemplateConfigFromExcelHtml) {
        setStatus("Excel import module not loaded.", true);
        return;
      }

      const res = importer.buildTemplateConfigFromExcelHtml(html, plain);
      if (!res.ok) {
        setStatus(res.error || "Paste failed.", true);
        return;
      }

      writeTemplateJson(res.config);
      selectedAddr = "";
      renderPreview();
      setStatus("Imported from Excel clipboard.", false);

      const modal = $("excelPasteModal");
      if (modal) modal.hidden = true;
    });

    $("btnPreviewDeleteRow")?.addEventListener("click", deleteSelectedRow);
    $("btnPreviewDeleteCol")?.addEventListener("click", deleteSelectedCol);

    $("selBgColor")?.addEventListener("input", () => {
      if (!selectedAddr) return;
      const parsed = readTemplateJsonFromEditor();
      if (!parsed.ok) return;
      upsertCellStyleProp(parsed.value, selectedAddr, "background", $("selBgColor").value);
      writeTemplateJson(parsed.value);
      renderPreview();
    });

    $("selTextColor")?.addEventListener("input", () => {
      if (!selectedAddr) return;
      const parsed = readTemplateJsonFromEditor();
      if (!parsed.ok) return;
      upsertCellStyleProp(parsed.value, selectedAddr, "color", $("selTextColor").value);
      writeTemplateJson(parsed.value);
      renderPreview();
    });

    $("btnApplyBorder")?.addEventListener("click", () => {
      if (!selectedAddr) return;
      const parsed = readTemplateJsonFromEditor();
      if (!parsed.ok) return;
      const border = asNonEmptyString($("selBorder")?.value);
      upsertCellStyleProp(parsed.value, selectedAddr, "border", border);
      writeTemplateJson(parsed.value);
      renderPreview();
    });

    $("selBorder")?.addEventListener("keydown", (e) => {
      if (e.key !== "Enter") return;
      e.preventDefault();
      $("btnApplyBorder")?.click();
    });

    $("btnClearFormat")?.addEventListener("click", () => {
      if (!selectedAddr) return;
      const parsed = readTemplateJsonFromEditor();
      if (!parsed.ok) return;
      clearCellStyle(parsed.value, selectedAddr);
      writeTemplateJson(parsed.value);
      renderPreview();
      setStatus(`Cleared format: ${selectedAddr}`, false);
    });

    const previewHost = $("previewHost");
    previewHost?.addEventListener("click", (e) => {
      if (e?.target?.closest && e.target.closest("textarea.cell-editor")) return;
      const td = e?.target?.closest ? e.target.closest("td[data-addr]") : null;
      if (!td) return;
      selectCell(td.dataset.addr);
    });
    previewHost?.addEventListener("dblclick", (e) => {
      if (e?.target?.closest && e.target.closest("textarea.cell-editor")) return;
      const td = e?.target?.closest ? e.target.closest("td[data-addr]") : null;
      if (!td) return;
      if (td.classList.contains("is-editing")) return;
      startCellEdit(td, td.dataset.addr);
    });

    $("tplSelect")?.addEventListener("change", () => loadTemplate($("tplSelect")?.value));
    $("tplJson")?.addEventListener("input", () => {
      syncCompactFormFromEditorJson();
      schedulePreview();
    });
    $("ctxJson")?.addEventListener("input", schedulePreview);
  }

  async function init() {
    setStatus("Loading templates...", false);

    try {
      if (ns.abapObjects?.whenReady) await ns.abapObjects.whenReady();
    } catch (e) {
      setStatus(`Failed to load templates: ${String(e?.message || e)}`, true);
      return;
    }

    populateSourceSelect("");
    populateTemplateSelect("");
    initEditorSplitter();
    wireUi();

    const sel = $("tplSelect");
    if (sel && sel.options.length) {
      loadTemplate(sel.value);
    } else {
      setStatus("No templates found.", true);
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})(window.AbapFlow);
