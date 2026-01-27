(function (ns) {
  "use strict";

  const ui = ns.ui;
  const utils = ns.utils;

  const state = {
    selectedId: "",
    lastListHash: "",
    lastTplHash: "",
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

  function deepClone(value) {
    if (value == null) return value;
    if (value instanceof RegExp) return new RegExp(value.source, value.flags);
    if (Array.isArray(value)) return value.map((x) => deepClone(x));
    if (typeof value === "object") {
      const out = {};
      for (const [k, v] of Object.entries(value)) out[k] = deepClone(v);
      return out;
    }
    return value;
  }

  function normalizeRegexForEditor(obj) {
    if (!isPlainObject(obj)) return obj;
    const parse = isPlainObject(obj.parse) ? obj.parse : null;
    const kind = asNonEmptyString(parse?.kind).toLowerCase();
    if (kind !== "regex") return obj;

    const rx = parse?.regex;
    if (rx instanceof RegExp) {
      parse.regex = rx.source;
      if (rx.flags) parse.flags = rx.flags;
    } else if (isPlainObject(rx)) {
      const source = asNonEmptyString(rx.source);
      const flags = asNonEmptyString(rx.flags);
      if (source) {
        parse.regex = source;
        if (flags) parse.flags = flags;
      }
    }

    return obj;
  }

  function stringifyForEditor(obj) {
    const clone = deepClone(obj);
    normalizeRegexForEditor(clone);
    return JSON.stringify(clone, null, 2);
  }

  function getCustomStore() {
    return ns.abapObjects?.customStore || null;
  }

  function getBaseConfig() {
    return ns.abapObjectsMasterConfig || null;
  }

  function getBaseObjects() {
    const base = getBaseConfig();
    return Array.isArray(base?.objects) ? base.objects : [];
  }

  function getBaseIndexByIdLower() {
    const map = new Map();
    const base = getBaseObjects();
    for (let i = 0; i < base.length; i++) {
      const id = asNonEmptyString(base[i]?.id);
      if (!id) continue;
      const lower = id.toLowerCase();
      if (!map.has(lower)) map.set(lower, i);
    }
    return map;
  }

  function buildEntries() {
    const base = getBaseObjects();
    const byLower = getBaseIndexByIdLower();
    const storeApi = getCustomStore();
    const store = storeApi ? storeApi.loadStore() : { objects: {}, disabled: {} };

    const disabledSet = new Set(
      Object.entries(store.disabled || {})
        .filter(([, flag]) => Boolean(flag))
        .map(([id]) => String(id).trim().toLowerCase())
        .filter(Boolean),
    );

    const storeByLower = new Map();
    for (const [id, obj] of Object.entries(store.objects || {})) {
      const oid = asNonEmptyString(obj?.id) || asNonEmptyString(id);
      if (!oid) continue;
      storeByLower.set(oid.toLowerCase(), { ...(obj || {}), id: oid });
    }

    const entries = [];

    for (const baseObj of base) {
      const id = asNonEmptyString(baseObj?.id);
      if (!id) continue;
      const lower = id.toLowerCase();
      const override = storeByLower.get(lower) || null;
      entries.push({
        id,
        idLower: lower,
        kind: asNonEmptyString((override || baseObj)?.kind),
        label: asNonEmptyString((override || baseObj)?.label) || id,
        isBase: true,
        isCustom: false,
        isOverride: Boolean(override),
        disabled: disabledSet.has(lower),
        baseDef: baseObj,
        def: override || baseObj,
      });
    }

    const custom = [];
    for (const obj of storeByLower.values()) {
      const id = asNonEmptyString(obj?.id);
      if (!id) continue;
      const lower = id.toLowerCase();
      if (byLower.has(lower)) continue;
      custom.push({
        id,
        idLower: lower,
        kind: asNonEmptyString(obj?.kind),
        label: asNonEmptyString(obj?.label) || id,
        isBase: false,
        isCustom: true,
        isOverride: false,
        disabled: disabledSet.has(lower),
        baseDef: null,
        def: obj,
      });
    }

    custom.sort((a, b) => a.id.localeCompare(b.id));
    return entries.concat(custom);
  }

  function computeListHash(entries, query) {
    const q = asNonEmptyString(query).toLowerCase();
    return `${q}::${entries
      .map((e) => `${e.idLower}:${e.isOverride ? 1 : 0}:${e.isCustom ? 1 : 0}:${e.disabled ? 1 : 0}`)
      .join("|")}`;
  }

  function listMatchesQuery(entry, query) {
    const q = asNonEmptyString(query).toLowerCase();
    if (!q) return true;
    const id = entry.idLower;
    const label = String(entry.label || "").toLowerCase();
    return id.includes(q) || label.includes(q) || String(entry.kind || "").toLowerCase().includes(q);
  }

  function renderList() {
    const host = $("abapObjCfgList");
    if (!host) return;

    const entries = buildEntries();
    const q = $("abapObjCfgSearch")?.value || "";
    const hash = computeListHash(entries, q);
    if (hash === state.lastListHash) return;
    state.lastListHash = hash;

    host.replaceChildren();

    const filtered = entries.filter((e) => listMatchesQuery(e, q));
    for (const e of filtered) {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "objcfg-item";
      if (e.idLower === String(state.selectedId || "").toLowerCase()) btn.classList.add("is-active");
      if (e.disabled) btn.classList.add("is-disabled");
      if (e.isCustom) btn.classList.add("is-custom");
      if (e.isOverride) btn.classList.add("is-override");

      const title = document.createElement("div");
      title.className = "objcfg-item__title";
      title.textContent = e.id;

      const meta = document.createElement("div");
      meta.className = "objcfg-item__meta";
      const origin = e.isCustom ? "tùy chỉnh" : e.isOverride ? "ghi đè" : "mặc định";
      meta.textContent = `${origin} • ${asNonEmptyString(e.kind) || "?"}`;

      btn.appendChild(title);
      btn.appendChild(meta);
      btn.addEventListener("click", () => selectObject(e.id));
      host.appendChild(btn);
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

  function buildTemplateCatalog() {
    const byIdLower = new Map();

    for (const obj of getBaseObjects()) {
      for (const t of Array.isArray(obj?.templates) ? obj.templates : []) {
        const id = asNonEmptyString(t?.id);
        if (!id) continue;
        const lower = id.toLowerCase();
        if (byIdLower.has(lower)) continue;
        byIdLower.set(lower, {
          id,
          label: asNonEmptyString(t?.label) || id,
          file: typeof t?.file === "string" ? t.file : "",
          origin: "built-in",
        });
      }
    }

    const defs = ns.templateDefs?.loadStore ? ns.templateDefs.loadStore() : null;
    const defsMap = isPlainObject(defs?.templates) ? defs.templates : {};
    for (const [idRaw, entry] of Object.entries(defsMap)) {
      const id = asNonEmptyString(idRaw);
      if (!id) continue;
      const lower = id.toLowerCase();
      if (byIdLower.has(lower)) continue;
      const meta = entry?.meta || {};
      byIdLower.set(lower, {
        id,
        label: asNonEmptyString(meta.label) || id,
        file: "",
        origin: meta.custom ? "custom" : "local",
      });
    }

    const out = Array.from(byIdLower.values());
    out.sort((a, b) => a.id.localeCompare(b.id));
    return out;
  }

  function computeTplHash(list) {
    return list.map((t) => `${t.id.toLowerCase()}:${t.origin}`).join("|");
  }

  function renderEditor() {
    const host = $("abapObjCfgEditor");
    if (!host) return;

    const selectedLower = String(state.selectedId || "").trim().toLowerCase();
    if (!selectedLower) {
      host.textContent = "Chọn đối tượng bên trái.";
      host.classList.add("empty");
      return;
    }

    const entries = buildEntries();
    const entry = entries.find((e) => e.idLower === selectedLower) || null;
    if (!entry) {
      host.textContent = "Không tìm thấy đối tượng.";
      host.classList.add("empty");
      return;
    }

    host.classList.remove("empty");

    const origin = entry.isCustom ? "tùy chỉnh" : entry.isOverride ? "ghi đè" : "mặc định";
    const disabledBadge = entry.disabled ? `<span class="badge badge-danger">đang tắt</span>` : "";

    host.innerHTML = `
      <div class="objcfg-editor__head">
        <div class="objcfg-editor__title">Đối tượng: <code>${escapeHtml(entry.id)}</code></div>
        <div class="objcfg-editor__badges">
          <span class="badge">${escapeHtml(origin)}</span>
          ${disabledBadge}
        </div>
      </div>

      <div class="objcfg-editor__actions">
        <button id="abapObjCfgBtnSave" class="btn btn-primary" type="button">Lưu</button>
        <button id="abapObjCfgBtnSaveNew" class="btn" type="button">Lưu mới</button>
        <button id="abapObjCfgBtnReset" class="btn" type="button">Khôi phục override</button>
        <button id="abapObjCfgBtnDelete" class="btn" type="button">Xóa</button>
        <button id="abapObjCfgBtnToggleDisable" class="btn" type="button">${entry.disabled ? "Bật" : "Tắt"}</button>
        <button id="abapObjCfgBtnApply" class="btn" type="button">Áp dụng</button>
      </div>

      <div class="objcfg-quick">
        <div class="objcfg-label__text">Chỉnh nhanh</div>
        <div id="abapObjCfgQuick" class="objcfg-quick__host"></div>
      </div>

      <details class="objcfg-advanced" open>
        <summary class="objcfg-advanced__summary">JSON nâng cao</summary>
        <textarea id="abapObjCfgJson" class="objcfg-json" spellcheck="false"></textarea>
      </details>

      <div class="objcfg-templates">
        <div class="objcfg-label__text">Gán template</div>
        <div class="objcfg-templates__row">
          <select id="abapObjCfgTplSelect" class="input"></select>
          <button id="abapObjCfgTplAdd" class="btn" type="button">Thêm mapping</button>
        </div>
        <div id="abapObjCfgTplList" class="objcfg-templates__list"></div>
      </div>

      <div class="hint">Lưu trong localStorage key: <code>${escapeHtml(getCustomStore()?.STORAGE_KEY || "")}</code></div>
    `;

    const jsonEl = $("abapObjCfgJson");
    if (jsonEl) {
      jsonEl.value = stringifyForEditor(entry.def);
    }

    const btnReset = $("abapObjCfgBtnReset");
    if (btnReset) btnReset.disabled = !(entry.isBase && entry.isOverride);

    const btnDelete = $("abapObjCfgBtnDelete");
    if (btnDelete) btnDelete.disabled = !entry.isCustom;

    renderTemplateSelect();
    renderTemplateMappingsList();
    renderQuickEditorFromJson();
    $("abapObjCfgBtnSave")?.addEventListener("click", () => onSave({ mode: "save" }));
    $("abapObjCfgBtnSaveNew")?.addEventListener("click", () => onSave({ mode: "saveNew" }));
    $("abapObjCfgBtnReset")?.addEventListener("click", () => onResetOverride(entry));
    $("abapObjCfgBtnDelete")?.addEventListener("click", () => onDeleteCustom(entry));
    $("abapObjCfgBtnToggleDisable")?.addEventListener("click", () => onToggleDisable(entry));
    $("abapObjCfgBtnApply")?.addEventListener("click", onApply);
    $("abapObjCfgTplAdd")?.addEventListener("click", onAddTemplateMapping);
  }

  function renderTemplateSelect() {
    const sel = $("abapObjCfgTplSelect");
    if (!sel) return;
    const list = buildTemplateCatalog();
    const hash = computeTplHash(list);
    if (hash === state.lastTplHash) return;
    state.lastTplHash = hash;

    sel.replaceChildren();

    const optEmpty = document.createElement("option");
    optEmpty.value = "";
    optEmpty.textContent = "Chọn template...";
    sel.appendChild(optEmpty);

    for (const t of list) {
      const opt = document.createElement("option");
      opt.value = t.id;
      const origin = t.origin === "built-in" ? "mặc định" : t.origin === "custom" ? "tùy chỉnh" : "cục bộ";
      opt.textContent = `${t.id} (${origin})`;
      sel.appendChild(opt);
    }
  }

  function getJsonObject() {
    const el = $("abapObjCfgJson");
    if (!el) return { ok: false, error: "Trình soạn thảo chưa sẵn sàng." };
    const parsed = safeJsonParse(el.value);
    if (!parsed.ok) return { ok: false, error: parsed.error || "JSON không hợp lệ." };
    if (!isPlainObject(parsed.value)) return { ok: false, error: "JSON object phải là object." };
    return { ok: true, value: parsed.value };
  }

  function setJsonObject(obj) {
    const el = $("abapObjCfgJson");
    if (!el) return;
    el.value = stringifyForEditor(obj);
  }

  function validateObjectDef(obj) {
    const schema = ns.abapObjects?.schema || null;
    if (!schema?.validateMasterConfig) return { ok: true, errors: [] };

    const master = {
      schema: "abapflow-abap-objects-master-config",
      version: 1,
      parserConfig: { version: 1 },
      objects: [obj],
    };
    const res = schema.validateMasterConfig(master);
    return res.ok ? { ok: true, errors: [] } : res;
  }

  function updateJsonObject(mutator, options) {
    const res = getJsonObject();
    if (!res.ok) {
      ui.setStatus(res.error || "JSON không hợp lệ.", true);
      return { ok: false, error: res.error || "JSON không hợp lệ." };
    }
    const obj = res.value;
    try {
      mutator(obj);
    } catch (e) {
      const msg = String(e?.message || e);
      ui.setStatus(msg, true);
      return { ok: false, error: msg };
    }
    setJsonObject(obj);
    if (options?.rerender) renderEditor();
    else if (options?.rerenderTemplatesList) renderTemplateMappingsList();
    else if (options?.rerenderQuick) renderQuickEditorFromJson();
    return { ok: true, value: obj };
  }

  function renderQuickEditorFromJson() {
    const host = $("abapObjCfgQuick");
    if (!host) return;
    const parsed = getJsonObject();
    if (!parsed.ok) {
      host.innerHTML = `<div class="objcfg-quick__error">Sửa JSON để dùng chỉnh nhanh.</div>`;
      return;
    }
    const obj = parsed.value || {};
    const kind = asNonEmptyString(obj.kind).toLowerCase();
    const builderKind = asNonEmptyString(obj?.builder?.kind);
    const parseKind = asNonEmptyString(obj?.parse?.kind);
    const isStatement = kind === "statement";
    const isRegex = isStatement && parseKind.toLowerCase() === "regex";
    const isMappingBuilder = builderKind.toLowerCase() === "mapping";
    host.innerHTML = `
      <div class="objcfg-form">
        <label class="objcfg-field">
          <span class="objcfg-field__label">Nhãn</span>
          <input id="abapObjCfgFieldLabel" class="input" type="text" value="${escapeHtml(asNonEmptyString(obj.label))}" />
        </label>

        <label class="objcfg-field">
          <span class="objcfg-field__label">Loại</span>
          <select id="abapObjCfgFieldKind" class="input">
            <option value="statement">statement</option>
            <option value="callEdge">callEdge</option>
          </select>
        </label>

        <label class="objcfg-field">
          <span class="objcfg-field__label">Kiểu builder</span>
          <input id="abapObjCfgFieldBuilderKind" class="input" list="abapObjCfgBuilderKinds" value="${escapeHtml(builderKind)}" />
          <datalist id="abapObjCfgBuilderKinds">
            <option value="performCall"></option>
            <option value="assignment"></option>
            <option value="if"></option>
            <option value="message"></option>
            <option value="itabOp"></option>
            <option value="append"></option>
            <option value="mapping"></option>
          </datalist>
        </label>

        ${
          isStatement
            ? `
          <label class="objcfg-field">
            <span class="objcfg-field__label">Kiểu parse</span>
            <input id="abapObjCfgFieldParseKind" class="input" list="abapObjCfgParseKinds" value="${escapeHtml(parseKind)}" />
            <datalist id="abapObjCfgParseKinds">
              <option value="regex"></option>
              <option value="assignment"></option>
              <option value="conditional"></option>
              <option value="message"></option>
              <option value="itabOp"></option>
            </datalist>
          </label>

          <label class="objcfg-field objcfg-field--checkbox">
            <span class="objcfg-field__label">continueAfterMatch</span>
            <input id="abapObjCfgFieldContinue" type="checkbox" ${obj?.parse?.continueAfterMatch ? "checked" : ""} />
          </label>
        `
            : `
          <label class="objcfg-field">
            <span class="objcfg-field__label">match.toKeyPrefix</span>
            <input id="abapObjCfgFieldToKeyPrefix" class="input" type="text" value="${escapeHtml(asNonEmptyString(obj?.match?.toKeyPrefix))}" />
          </label>
        `
        }
      </div>

      ${
        isRegex
          ? `
        <div class="objcfg-sub">
          <div class="objcfg-sub__title">Regex</div>
          <div class="objcfg-sub__grid">
            <label class="objcfg-field objcfg-field--wide">
              <span class="objcfg-field__label">parse.regex (pattern)</span>
              <textarea id="abapObjCfgFieldRegex" class="objcfg-json objcfg-json--small" spellcheck="false">${escapeHtml(
                asNonEmptyString(obj?.parse?.regex),
              )}</textarea>
            </label>
            <label class="objcfg-field">
              <span class="objcfg-field__label">parse.flags</span>
              <input id="abapObjCfgFieldFlags" class="input" type="text" value="${escapeHtml(asNonEmptyString(obj?.parse?.flags) || "i")}" />
            </label>
          </div>

          <div class="objcfg-sub__title">Trường regex</div>
          <div class="objcfg-mini-table">
            <div class="objcfg-mini-table__head">
              <div>Trường</div>
              <div>Nhóm</div>
              <div></div>
            </div>
            <div id="abapObjCfgRegexFields"></div>
          </div>
          <button id="abapObjCfgRegexAddField" class="btn" type="button">Thêm trường regex</button>
        </div>
      `
          : ""
      }

      ${
        isMappingBuilder
          ? `
        <div class="objcfg-sub">
          <div class="objcfg-sub__title">Trường mapping (builder)</div>
          <div class="objcfg-mini-table">
            <div class="objcfg-mini-table__head objcfg-mini-table__head--4">
              <div>Khoá output</div>
              <div>Kiểu</div>
              <div>Nguồn</div>
              <div></div>
            </div>
            <div id="abapObjCfgMappingFields"></div>
          </div>
          <button id="abapObjCfgMapAddField" class="btn" type="button">Thêm trường mapping</button>
        </div>
      `
          : ""
      }
    `;

    const kindSel = $("abapObjCfgFieldKind");
    if (kindSel) kindSel.value = isStatement ? "statement" : "callEdge";

    $("abapObjCfgFieldLabel")?.addEventListener("input", (ev) => {
      const v = String(ev?.target?.value ?? "");
      updateJsonObject((o) => {
        o.label = v;
      });
    });

    kindSel?.addEventListener("change", (ev) => {
      const next = asNonEmptyString(ev?.target?.value).toLowerCase();
      updateJsonObject(
        (o) => {
          o.kind = next === "calledge" || next === "callEdge" ? "callEdge" : "statement";

          if (o.kind.toLowerCase() === "statement") {
            if (!isPlainObject(o.parse)) o.parse = { kind: "regex" };
          } else {
            if (!isPlainObject(o.match)) o.match = { toKeyPrefix: "" };
          }

          if (!isPlainObject(o.builder)) o.builder = {};
          if (!asNonEmptyString(o.builder.kind)) {
            o.builder.kind = o.kind.toLowerCase() === "statement" ? "mapping" : "performCall";
          }
        },
        { rerender: true },
      );
    });

    $("abapObjCfgFieldBuilderKind")?.addEventListener("change", (ev) => {
      const v = asNonEmptyString(ev?.target?.value);
      updateJsonObject(
        (o) => {
          if (!isPlainObject(o.builder)) o.builder = {};
          o.builder.kind = v;
          if (v.toLowerCase() === "mapping") {
            if (!isPlainObject(o.builder.fields)) o.builder.fields = {};
          }
        },
        { rerender: true },
      );
    });

    if (isStatement) {
      $("abapObjCfgFieldParseKind")?.addEventListener("change", (ev) => {
        const v = asNonEmptyString(ev?.target?.value);
        updateJsonObject(
          (o) => {
            if (!isPlainObject(o.parse)) o.parse = {};
            o.parse.kind = v;
            if (v.toLowerCase() === "regex") {
              if (!asNonEmptyString(o.parse.regex)) o.parse.regex = "^$";
              if (o.parse.flags == null) o.parse.flags = "i";
              if (!isPlainObject(o.parse.fields)) o.parse.fields = {};
            }
          },
          { rerender: true },
        );
      });

      $("abapObjCfgFieldContinue")?.addEventListener("change", (ev) => {
        const checked = Boolean(ev?.target?.checked);
        updateJsonObject((o) => {
          if (!isPlainObject(o.parse)) o.parse = {};
          if (checked) o.parse.continueAfterMatch = true;
          else delete o.parse.continueAfterMatch;
        });
      });
    } else {
      $("abapObjCfgFieldToKeyPrefix")?.addEventListener("input", (ev) => {
        const v = String(ev?.target?.value ?? "");
        updateJsonObject((o) => {
          if (!isPlainObject(o.match)) o.match = {};
          o.match.toKeyPrefix = v;
        });
      });
    }

    if (isRegex) {
      $("abapObjCfgFieldRegex")?.addEventListener("input", (ev) => {
        const v = String(ev?.target?.value ?? "");
        updateJsonObject((o) => {
          if (!isPlainObject(o.parse)) o.parse = {};
          o.parse.regex = v;
        });
      });
      $("abapObjCfgFieldFlags")?.addEventListener("input", (ev) => {
        const v = asNonEmptyString(ev?.target?.value);
        updateJsonObject((o) => {
          if (!isPlainObject(o.parse)) o.parse = {};
          if (v) o.parse.flags = v;
          else delete o.parse.flags;
        });
      });

      renderRegexFieldsTable(obj);
      $("abapObjCfgRegexAddField")?.addEventListener("click", addRegexField);
    }

    if (isMappingBuilder) {
      renderMappingFieldsTable(obj);
      $("abapObjCfgMapAddField")?.addEventListener("click", addMappingField);
    }
  }

  function getRegexFieldsFromJsonObject(obj) {
    const fields = isPlainObject(obj?.parse?.fields) ? obj.parse.fields : {};
    const out = [];
    for (const [k, v] of Object.entries(fields)) {
      const key = asNonEmptyString(k);
      const n = Number(v);
      if (!key) continue;
      if (!Number.isFinite(n)) continue;
      out.push({ key, index: n });
    }
    out.sort((a, b) => a.index - b.index || a.key.localeCompare(b.key));
    return out;
  }

  function renderRegexFieldsTable(obj) {
    const host = $("abapObjCfgRegexFields");
    if (!host) return;
    host.replaceChildren();

    const list = getRegexFieldsFromJsonObject(obj);
    if (list.length === 0) {
      const empty = document.createElement("div");
      empty.className = "objcfg-mini-table__empty";
      empty.textContent = "(không có trường)";
      host.appendChild(empty);
      return;
    }

    for (const row of list) {
      const wrap = document.createElement("div");
      wrap.className = "objcfg-mini-table__row";
      wrap.dataset.key = row.key;

      const inKey = document.createElement("input");
      inKey.className = "input objcfg-mini-table__input";
      inKey.value = row.key;

      const inIdx = document.createElement("input");
      inIdx.className = "input objcfg-mini-table__input";
      inIdx.type = "number";
      inIdx.min = "0";
      inIdx.value = String(row.index);

      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "btn";
      btn.textContent = "Xóa";
      btn.addEventListener("click", () => {
        updateJsonObject(
          (o) => {
            if (!isPlainObject(o.parse)) return;
            if (!isPlainObject(o.parse.fields)) return;
            delete o.parse.fields[row.key];
          },
          { rerender: true },
        );
      });

      const applyFromInputs = () => {
        const nextKey = asNonEmptyString(inKey.value);
        const n = Number(inIdx.value);
        if (!nextKey || !Number.isFinite(n)) return;
        updateJsonObject(
          (o) => {
            if (!isPlainObject(o.parse)) o.parse = {};
            if (!isPlainObject(o.parse.fields)) o.parse.fields = {};
            const oldKey = row.key;
            if (oldKey !== nextKey) delete o.parse.fields[oldKey];
            o.parse.fields[nextKey] = n;
          },
          { rerender: true },
        );
      };

      inKey.addEventListener("change", applyFromInputs);
      inIdx.addEventListener("change", applyFromInputs);

      wrap.appendChild(inKey);
      wrap.appendChild(inIdx);
      wrap.appendChild(btn);
      host.appendChild(wrap);
    }
  }

  function addRegexField() {
    const key = asNonEmptyString(window.prompt("Tên field:", ""));
    if (!key) return;
    const idx = Number(window.prompt("Chỉ số group (số):", "1"));
    if (!Number.isFinite(idx)) return;
    updateJsonObject(
      (o) => {
        if (!isPlainObject(o.parse)) o.parse = {};
        if (!isPlainObject(o.parse.fields)) o.parse.fields = {};
        o.parse.fields[key] = idx;
      },
      { rerender: true },
    );
  }

  function getMappingFieldsFromJsonObject(obj) {
    const fields = isPlainObject(obj?.builder?.fields) ? obj.builder.fields : {};
    const out = [];
    for (const [outKeyRaw, spec] of Object.entries(fields)) {
      const outKey = asNonEmptyString(outKeyRaw);
      if (!outKey) continue;
      const s = isPlainObject(spec) ? spec : {};
      out.push({
        outKey,
        type: asNonEmptyString(s.type) || "expr",
        from: asNonEmptyString(s.from),
      });
    }
    out.sort((a, b) => a.outKey.localeCompare(b.outKey));
    return out;
  }

  function renderMappingFieldsTable(obj) {
    const host = $("abapObjCfgMappingFields");
    if (!host) return;
    host.replaceChildren();

    const list = getMappingFieldsFromJsonObject(obj);
    if (list.length === 0) {
      const empty = document.createElement("div");
      empty.className = "objcfg-mini-table__empty";
      empty.textContent = "(không có trường)";
      host.appendChild(empty);
      return;
    }

    for (const row of list) {
      const wrap = document.createElement("div");
      wrap.className = "objcfg-mini-table__row objcfg-mini-table__row--4";
      wrap.dataset.key = row.outKey;

      const inKey = document.createElement("input");
      inKey.className = "input objcfg-mini-table__input";
      inKey.value = row.outKey;

      const selType = document.createElement("select");
      selType.className = "input objcfg-mini-table__input";
      for (const t of ["expr", "exprlist", "text"]) {
        const opt = document.createElement("option");
        opt.value = t;
        opt.textContent = t;
        selType.appendChild(opt);
      }
      selType.value = row.type;

      const inFrom = document.createElement("input");
      inFrom.className = "input objcfg-mini-table__input";
      inFrom.value = row.from;

      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "btn";
      btn.textContent = "Xóa";
      btn.addEventListener("click", () => {
        updateJsonObject(
          (o) => {
            if (!isPlainObject(o.builder)) return;
            if (!isPlainObject(o.builder.fields)) return;
            delete o.builder.fields[row.outKey];
          },
          { rerender: true },
        );
      });

      const applyFromInputs = () => {
        const nextKey = asNonEmptyString(inKey.value);
        const type = asNonEmptyString(selType.value) || "expr";
        const from = asNonEmptyString(inFrom.value);
        if (!nextKey) return;
        updateJsonObject(
          (o) => {
            if (!isPlainObject(o.builder)) o.builder = { kind: "mapping" };
            if (!isPlainObject(o.builder.fields)) o.builder.fields = {};
            const oldKey = row.outKey;
            if (oldKey !== nextKey) delete o.builder.fields[oldKey];
            o.builder.fields[nextKey] = { type, from };
          },
          { rerender: true },
        );
      };

      inKey.addEventListener("change", applyFromInputs);
      selType.addEventListener("change", applyFromInputs);
      inFrom.addEventListener("change", applyFromInputs);

      wrap.appendChild(inKey);
      wrap.appendChild(selType);
      wrap.appendChild(inFrom);
      wrap.appendChild(btn);
      host.appendChild(wrap);
    }
  }

  function addMappingField() {
    const outKey = asNonEmptyString(window.prompt("Khoá output:", ""));
    if (!outKey) return;
    const from = asNonEmptyString(window.prompt("Từ (tên trường payload):", ""));
    const type = asNonEmptyString(window.prompt("Kiểu (expr|exprlist|text):", "expr")) || "expr";
    updateJsonObject(
      (o) => {
        if (!isPlainObject(o.builder)) o.builder = { kind: "mapping" };
        if (!isPlainObject(o.builder.fields)) o.builder.fields = {};
        o.builder.fields[outKey] = { type, from };
      },
      { rerender: true },
    );
  }

  function onSave(options) {
    const storeApi = getCustomStore();
    if (!storeApi) {
      ui.setStatus("Chưa tải module lưu ABAP Objects.", true);
      return;
    }

    const res = getJsonObject();
    if (!res.ok) {
      ui.setStatus(res.error || "JSON không hợp lệ.", true);
      return;
    }

    const obj = res.value;
    const id = asNonEmptyString(obj?.id);
    if (!id) {
      ui.setStatus("Cần Object id.", true);
      return;
    }

    const validation = validateObjectDef(obj);
    if (!validation.ok) {
      const msg = (validation.errors || [])
        .map((e) => `${e.path || "(gốc)"}: ${e.message || "Giá trị không hợp lệ"}`)
        .join("\n");
      ui.setStatus(msg || "Cấu hình object không hợp lệ.", true);
      return;
    }

    const mode = asNonEmptyString(options?.mode);
    if (mode === "saveNew") {
      const suggested = `${id}_copy`;
      const nextId = asNonEmptyString(window.prompt("Object id mới:", suggested));
      if (!nextId) return;
      const cloned = deepClone({ ...obj, id: nextId });
      const saved = storeApi.upsertObjectDef(cloned);
      if (!saved.ok) {
        ui.setStatus(saved.error || "Lưu thất bại.", true);
        return;
      }
      state.selectedId = nextId;
      state.lastListHash = "";
      ui.setStatus(`Đã lưu object mới: ${nextId}.`, false);
      renderList();
      renderEditor();
      return;
    }

    const saved = storeApi.upsertObjectDef(obj);
    if (!saved.ok) {
      ui.setStatus(saved.error || "Lưu thất bại.", true);
      return;
    }

    state.selectedId = id;
    state.lastListHash = "";
    ui.setStatus(`Đã lưu: ${id}.`, false);
    renderList();
    renderEditor();
  }

  function onResetOverride(entry) {
    const storeApi = getCustomStore();
    if (!storeApi) return;
    if (!entry?.isBase || !entry?.isOverride) return;
    const ok = window.confirm(`Khôi phục override cho "${entry.id}"?`);
    if (!ok) return;
    storeApi.deleteObjectDef(entry.id);
    state.lastListHash = "";
    ui.setStatus(`Đã khôi phục override: ${entry.id}.`, false);
    renderList();
    renderEditor();
  }

  function onDeleteCustom(entry) {
    const storeApi = getCustomStore();
    if (!storeApi) return;
    if (!entry?.isCustom) return;
    const ok = window.confirm(`Xóa object tuỳ chỉnh "${entry.id}"?`);
    if (!ok) return;
    storeApi.deleteObjectDef(entry.id);
    state.selectedId = "";
    state.lastListHash = "";
    ui.setStatus(`Đã xóa: ${entry.id}.`, false);
    renderList();
    renderEditor();
  }

  function onToggleDisable(entry) {
    const storeApi = getCustomStore();
    if (!storeApi) return;
    storeApi.setObjectDisabled(entry.id, !entry.disabled);
    state.lastListHash = "";
    ui.setStatus(`${entry.disabled ? "Đã bật" : "Đã tắt"}: ${entry.id}.`, false);
    renderList();
    renderEditor();
  }

  function onApply() {
    if (ns.abapObjects?.reset) ns.abapObjects.reset();
    ui.setStatus("Đã áp dụng cấu hình ABAP Objects. Hãy phân tích lại để nạp.", false);
    const input = $("abapInput")?.value || "";
    if (String(input).trim()) $("btnAnalyze")?.click();
  }

  function normalizeTemplatesList(value) {
    if (!Array.isArray(value)) return [];
    const out = [];
    for (const t of value) {
      if (!isPlainObject(t)) continue;
      const id = asNonEmptyString(t.id);
      if (!id) continue;
      out.push({
        id,
        label: asNonEmptyString(t.label) || id,
        auto: t.auto !== false,
        file: typeof t.file === "string" ? t.file : "",
        when: isPlainObject(t.when) ? t.when : null,
      });
    }
    return out;
  }

  function renderTemplateMappingsList() {
    const host = $("abapObjCfgTplList");
    if (!host) return;

    const res = getJsonObject();
    host.replaceChildren();
    if (!res.ok) return;

    const list = normalizeTemplatesList(res.value.templates);
    if (list.length === 0) {
      const empty = document.createElement("div");
      empty.className = "objcfg-templates__empty";
      empty.textContent = "(chưa gán template)";
      host.appendChild(empty);
      return;
    }

    for (const t of list) {
      const row = document.createElement("div");
      row.className = "objcfg-templates__item";

      const left = document.createElement("div");
      left.className = "objcfg-templates__item-text";
      left.textContent = `${t.id}`;

      const right = document.createElement("button");
      right.type = "button";
      right.className = "btn objcfg-templates__remove";
      right.textContent = "Xóa";
      right.addEventListener("click", () => removeTemplateMapping(t.id));

      row.appendChild(left);
      row.appendChild(right);
      host.appendChild(row);
    }
  }

  function onAddTemplateMapping() {
    const sel = $("abapObjCfgTplSelect");
    const tid = asNonEmptyString(sel?.value);
    if (!tid) return;

    const res = getJsonObject();
    if (!res.ok) {
      ui.setStatus(res.error || "JSON không hợp lệ.", true);
      return;
    }

    const obj = res.value;
    if (!Array.isArray(obj.templates)) obj.templates = [];

    const existing = normalizeTemplatesList(obj.templates);
    const exists = existing.some((t) => String(t.id || "").trim().toLowerCase() === tid.toLowerCase());
    if (exists) {
      ui.setStatus(`Template đã được gán: ${tid}.`, true);
      return;
    }

    const catalog = buildTemplateCatalog();
    const found = catalog.find((t) => t.id.toLowerCase() === tid.toLowerCase()) || null;

    obj.templates.push({
      id: tid,
      label: found?.label || tid,
      auto: true,
      file: found?.file || "",
    });

    setJsonObject(obj);
    renderTemplateMappingsList();
  }

  function removeTemplateMapping(templateId) {
    const tid = asNonEmptyString(templateId);
    if (!tid) return;

    const res = getJsonObject();
    if (!res.ok) return;

    const obj = res.value;
    const list = Array.isArray(obj.templates) ? obj.templates : [];
    const next = list.filter((t) => String(t?.id || "").trim().toLowerCase() !== tid.toLowerCase());
    obj.templates = next;

    setJsonObject(obj);
    renderTemplateMappingsList();
  }

  function selectObject(objectId) {
    state.selectedId = asNonEmptyString(objectId);
    state.lastListHash = "";
    ui.setActiveTab("config");
    renderList();
    renderEditor();
  }

  function createNewObject() {
    const storeApi = getCustomStore();
    if (!storeApi) {
      ui.setStatus("Chưa tải module lưu ABAP Objects.", true);
      return;
    }

    const suggested = "newObject";
    const id = asNonEmptyString(window.prompt("Object id mới:", suggested));
    if (!id) return;

    const exists = buildEntries().some((e) => e.idLower === id.toLowerCase());
    if (exists) {
      const ok = window.confirm(`"${id}" đã tồn tại. Ghi đè/override?`);
      if (!ok) return;
    }

    const obj = {
      id,
      kind: "statement",
      label: "Object mới",
      parse: { kind: "regex", regex: "^$" },
      builder: { kind: "mapping", fields: {} },
      templates: [],
    };
    storeApi.upsertObjectDef(obj);

    state.selectedId = id;
    state.lastListHash = "";
    ui.setStatus(`Đã tạo: ${id}.`, false);
    renderList();
    renderEditor();
  }

  function exportStore() {
    const storeApi = getCustomStore();
    if (!storeApi) return;
    const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);
    ui.downloadTextFile(`abapflow-abap-objects-custom-${stamp}.json`, storeApi.exportJson(), "application/json");
    ui.setStatus("Đã xuất cấu hình ABAP Objects.", false);
  }

  async function importStoreFromFile(file) {
    const storeApi = getCustomStore();
    if (!storeApi) return;
    if (!file) return;

    const text = await file.text();
    const res = storeApi.importJson(text, { mode: "merge" });
    if (!res.ok) {
      ui.setStatus(res.error || "Nhập thất bại.", true);
      return;
    }
    state.selectedId = "";
    state.lastListHash = "";
    ui.setStatus("Đã nhập cấu hình ABAP Objects (gộp).", false);
    renderList();
    renderEditor();
  }

  function resetAll() {
    const storeApi = getCustomStore();
    if (!storeApi) return;
    const ok = window.confirm("Reset toàn bộ cấu hình ABAP Objects tùy chỉnh trong localStorage?");
    if (!ok) return;
    storeApi.clearStore();
    state.selectedId = "";
    state.lastListHash = "";
    ui.setStatus("Đã reset cấu hình ABAP Objects.", false);
    renderList();
    renderEditor();
  }

  function bindOnce() {
    const tab = $("tab-config");
    if (!tab || tab.dataset.bound) return;
    tab.dataset.bound = "1";

    $("abapObjCfgSearch")?.addEventListener("input", () => {
      state.lastListHash = "";
      renderList();
    });
    $("abapObjCfgNew")?.addEventListener("click", createNewObject);
    $("abapObjCfgExport")?.addEventListener("click", exportStore);
    $("abapObjCfgImport")?.addEventListener("click", () => $("abapObjCfgImportFile")?.click());
    $("abapObjCfgResetAll")?.addEventListener("click", resetAll);
    $("abapObjCfgImportFile")?.addEventListener("change", async (ev) => {
      const input = ev.target;
      const file = input?.files?.[0] || null;
      await importStoreFromFile(file);
      if (input) input.value = "";
    });
  }

  ui.renderAbapObjectsConfig = function renderAbapObjectsConfig() {
    bindOnce();
    state.lastListHash = "";
    state.lastTplHash = "";
    renderList();
    renderEditor();
  };

  ui.selectAbapObjectConfig = selectObject;
})(window.AbapFlow);
