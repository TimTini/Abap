(function (ns) {
  "use strict";

  const ui = ns.ui;
  const tpl = ui.templates;
  const state = ui.state;

  let templateResultById = new Map();
  let selectedTemplateCard = null;
  let selectedTemplateResultIds = new Set();
  let collapsedTemplateResultIds = new Set();
  let tableByResultId = new Map();
  let lastRenderOrder = [];
  let lastToolbarUpdate = null;

  function scrollToTemplateResultId(resultId) {
    const id = String(resultId || "").trim();
    if (!id) return;
    if (!state.model) return;

    if (typeof ui.setActiveTab === "function") ui.setActiveTab("templates");

    requestAnimationFrame(() => {
      renderTemplates({ autoSelectResultId: id });
    });
  }

  function isEditableDescriptionBind(bind) {
    const b = String(bind || "").trim();
    if (!b) return false;
    if (b === "perform.description") return true;
    if (b === "item.description" || b === "value.description") return true;
    if (b === "table.description" || b === "target.description") return true;
    if (/^(?:msgClass|msgNo|displayLike|messageText|into|raising)\.description$/.test(b)) return true;
    if (/^with\[\d+\]\.description$/.test(b)) return true;
    if (/^conditions\[\d+\]\.(?:item1|item2)\.description$/.test(b)) return true;
    return /^(tables|using|changing|raising)\[\d+\]\.description$/.test(b);
  }

  function labelForDescriptionBind(result, bind) {
    const b = String(bind || "").trim();
    const ctx = result?.context;
    const performName = String(ctx?.perform?.name || "").trim();

    if (b === "item.description") {
      const item = String(ctx?.assignment?.lhs || "").trim();
      return item ? `Desc Item: ${item}` : "Desc Item";
    }

    if (b === "value.description") {
      const value = String(ctx?.assignment?.rhs || "").trim();
      return value ? `Desc Value: ${value}` : "Desc Value";
    }

    if (b === "table.description") {
      const text = String(ctx?.table?.text || "").trim();
      return text ? `Desc Internal table: ${text}` : "Desc Internal table";
    }

    if (b === "target.description") {
      const text = String(ctx?.target?.text || "").trim();
      return text ? `Desc Target: ${text}` : "Desc Target";
    }

    const msgMatch = /^(msgClass|msgNo|displayLike|messageText|into|raising)\.description$/.exec(b);
    if (msgMatch) {
      const prop = String(msgMatch[1] || "");
      const node = ctx?.[prop] || null;
      const text = String(node?.text || "").trim();
      const label =
        prop === "msgClass"
          ? "Message class"
          : prop === "msgNo"
            ? "Message No."
            : prop === "displayLike"
              ? "Display like"
              : prop === "messageText"
                ? "Message text"
                : prop === "into"
                  ? "Destination"
                  : prop === "raising"
                    ? "Raising"
                    : prop;
      return text ? `Desc ${label}: ${text}` : `Desc ${label}`;
    }

    const withMatch = /^with\[(\d+)\]\.description$/.exec(b);
    if (withMatch) {
      const idx = Number(withMatch[1]);
      const node = ctx?.with?.[idx] || null;
      const text = String(node?.text || "").trim();
      const label = `Message &${idx + 1}`;
      return text ? `Desc ${label}: ${text}` : `Desc ${label}`;
    }

    const condMatch = /^conditions\[(\d+)\]\.(item1|item2)\.description$/.exec(b);
    if (condMatch) {
      const idx = Number(condMatch[1]);
      const side = String(condMatch[2] || "").trim();
      const cond = ctx?.conditions?.[idx] || null;
      const itemText = side === "item2" ? String(cond?.item2?.text || "").trim() : String(cond?.item1?.text || "").trim();
      const op = String(cond?.operator || "").trim();
      const label = side === "item2" ? "Item 2" : "Item 1";
      const tail = itemText ? `${itemText}${op ? ` (${op})` : ""}` : `#${idx + 1}`;
      return `Desc ${label}: ${tail}`;
    }
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
    const b = String(bind || "").trim();
    const ctx = result?.context;
    if (!ctx) return null;

    function keyFromExprNode(node) {
      const originKey = String(node?.originKey || "").trim();
      if (originKey) return { key: originKey };

      if (!ns.notes?.makeDeclKey || !ns.lineage?.resolveSymbol) return null;

      const model = state.model;
      const routineKey = String(result?.routineKey || ctx.routine?.key || "").trim();
      const exprText = String(node?.text || "").trim();
      if (!model || !routineKey || !exprText) return null;

      const root =
        typeof ns.desc?.rootFromPath === "function"
          ? ns.desc.rootFromPath(exprText)
          : (/^[A-Za-z_][A-Za-z0-9_\/]*/.exec(exprText) || [])[0] || "";
      if (!root) return null;

      const resolution = ns.lineage.resolveSymbol(model, routineKey, root);
      if (!resolution || resolution.scope === "unknown") return null;

      if (resolution.scope === "parameter") {
        const key = ns.notes.makeParamKey?.(routineKey, root);
        return key ? { key } : null;
      }

      if (resolution.scope === "local" || resolution.scope === "global") {
        const declKind = String(resolution.decl?.declKind || "").trim();
        if (!declKind) return null;
        const scopeKey = resolution.scope === "global" ? "PROGRAM" : routineKey;
        const key = ns.notes.makeDeclKey(scopeKey, declKind, root);
        return key ? { key } : null;
      }

      return null;
    }

    if (b === "table.description") return keyFromExprNode(ctx?.table);
    if (b === "target.description") return keyFromExprNode(ctx?.target);

    const msgMatch = /^(msgClass|msgNo|displayLike|messageText|into|raising)\.description$/.exec(b);
    if (msgMatch) return keyFromExprNode(ctx?.[msgMatch[1]]);

    const withMatch = /^with\[(\d+)\]\.description$/.exec(b);
    if (withMatch) return keyFromExprNode(ctx?.with?.[Number(withMatch[1])]);

    if (b === "perform.description") {
      const key = String(ctx.perform?.originKey || ctx.perform?.key || "").trim();
      return key ? { key } : null;
    }

    const condMatch = /^conditions\[(\d+)\]\.(item1|item2)\.description$/.exec(b);
    if (condMatch) {
      const idx = Number(condMatch[1]);
      const side = String(condMatch[2] || "").trim();
      const k = String(ctx?.conditions?.[idx]?.[side]?.originKey || "").trim();
      if (k) return { key: k };

      if (!ns.notes?.makeDeclKey || !ns.lineage?.resolveSymbol) return null;

      const model = state.model;
      const routineKey = String(result?.routineKey || ctx.routine?.key || "").trim();
      const cond = ctx?.conditions?.[idx] || null;
      const root = side === "item2" ? String(cond?.item2?.root || "").trim() : String(cond?.item1?.root || "").trim();
      if (!model || !routineKey || !root) return null;

      const resolution = ns.lineage.resolveSymbol(model, routineKey, root);
      if (!resolution || resolution.scope === "unknown") return null;

      if (resolution.scope === "parameter") {
        const key = ns.notes.makeParamKey(routineKey, root);
        return key ? { key } : null;
      }

      if (resolution.scope === "local" || resolution.scope === "global") {
        const declKind = String(resolution.decl?.declKind || "").trim();
        if (!declKind) return null;
        const scopeKey = resolution.scope === "global" ? "PROGRAM" : routineKey;
        const key = ns.notes.makeDeclKey(scopeKey, declKind, root);
        return key ? { key } : null;
      }

      return null;
    }

    if (b === "item.description" || b === "value.description") {
      const key =
        b === "item.description"
          ? String(ctx?.item?.originKey || "").trim()
          : String(ctx?.value?.originKey || "").trim();
      if (key) return { key };

      if (!ns.notes?.makeDeclKey || !ns.lineage?.resolveSymbol) return null;

      const model = state.model;
      const routineKey = String(result?.routineKey || ctx.routine?.key || "").trim();
      const root = b === "item.description" ? String(ctx?.item?.root || "").trim() : String(ctx?.value?.root || "").trim();
      if (!model || !routineKey || !root) return null;

      const resolution = ns.lineage.resolveSymbol(model, routineKey, root);
      if (!resolution || resolution.scope === "unknown") return null;

      if (resolution.scope === "parameter") {
        const key = ns.notes.makeParamKey(routineKey, root);
        return key ? { key } : null;
      }

      if (resolution.scope === "local" || resolution.scope === "global") {
        const declKind = String(resolution.decl?.declKind || "").trim();
        if (!declKind) return null;
        const scopeKey = resolution.scope === "global" ? "PROGRAM" : routineKey;
        const key = ns.notes.makeDeclKey(scopeKey, declKind, root);
        return key ? { key } : null;
      }

      return null;
    }

    const m = /^(tables|using|changing|raising)\[(\d+)\]\.description$/.exec(b);
    if (!m) return null;

    const list = m[1];
    const idx = Number(m[2]);
    const item = ctx?.[list]?.[idx] || null;
    const originKey = String(item?.originKey || "").trim();
    if (originKey) return { key: originKey };

    const formalName = String(item?.name || "").trim();
    const routineKey = String(ctx.perform?.key || "").trim();
    if (!ns.notes?.makeParamKey || !routineKey || !formalName) return null;

    const key = ns.notes.makeParamKey(routineKey, formalName);
    return key ? { key } : null;
  }

  // Config-driven templates support:
  // - Allow editing any `*.description` cell (local override or global notes).
  // - Resolve the "global" target using `originKey` when available, otherwise fall back to symbol resolution.
  function isEditableDescriptionBind(bind) {
    const b = String(bind || "").trim();
    return b.endsWith(".description") && b.length > ".description".length;
  }

  function labelForDescriptionBind(result, bind) {
    const b = String(bind || "").trim();
    if (!b || !isEditableDescriptionBind(b)) return "Edit description";

    const ctx = result?.context || null;
    const basePath = b.slice(0, -".description".length);
    const node = typeof tpl.resolveBindPath === "function" ? tpl.resolveBindPath(ctx, basePath) : null;

    if (basePath === "perform") {
      const performName = String(ctx?.perform?.name || "").trim();
      return performName ? `Description FORM ${performName}` : "Description FORM";
    }

    const text = String(node?.text ?? node?.actual ?? node?.name ?? "").trim();
    if (text) return `Description: ${text}`;
    return `Description: ${basePath}`;
  }

  function resolveGlobalDescriptionKey(result, bind) {
    const b = String(bind || "").trim();
    if (!b || !isEditableDescriptionBind(b)) return null;

    const ctx = result?.context || null;
    if (!ctx) return null;

    const basePath = b.slice(0, -".description".length);
    const node = typeof tpl.resolveBindPath === "function" ? tpl.resolveBindPath(ctx, basePath) : null;
    if (!node || typeof node !== "object") return null;

    const originKey = String(node?.originKey || "").trim();
    if (originKey) return { key: originKey };

    const calleeKey = String(ctx?.perform?.key || "").trim();
    const formalName = String(node?.name || "").trim();
    if (calleeKey && formalName && typeof ns.notes?.makeParamKey === "function") {
      const key = ns.notes.makeParamKey(calleeKey, formalName);
      if (key) return { key };
    }

    if (!ns.notes?.makeDeclKey || !ns.lineage?.resolveSymbol) return null;

    const model = state.model;
    if (!model) return null;

    const exprText = String(
      (typeof node?.text === "string" && node.text.trim()) || (typeof node?.actual === "string" && node.actual.trim()) || "",
    ).trim();
    if (!exprText) return null;

    const callerKey = String(ctx?.caller?.key || "").trim();
    const routineKey = String(result?.routineKey || ctx?.routine?.key || "").trim();
    const evalRoutineKey = callerKey && typeof node?.actual === "string" ? callerKey : routineKey || callerKey;
    if (!evalRoutineKey) return null;

    const root =
      typeof ns.desc?.rootFromPath === "function"
        ? ns.desc.rootFromPath(exprText)
        : (/^[A-Za-z_][A-Za-z0-9_\/]*/.exec(exprText) || [])[0] || "";
    if (!root) return null;

    const resolution = ns.lineage.resolveSymbol(model, evalRoutineKey, root);
    if (!resolution || resolution.scope === "unknown") return null;

    if (resolution.scope === "parameter") {
      const key = ns.notes.makeParamKey?.(evalRoutineKey, root);
      return key ? { key } : null;
    }

    if (resolution.scope === "local" || resolution.scope === "global") {
      const declKind = String(resolution.decl?.declKind || "").trim();
      if (!declKind) return null;
      const scopeKey = resolution.scope === "global" ? "PROGRAM" : evalRoutineKey;
      const key = ns.notes.makeDeclKey(scopeKey, declKind, root);
      return key ? { key } : null;
    }

    return null;
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

      const btnClear = document.createElement("button");
      btnClear.type = "button";
      btnClear.className = "btn";
      btnClear.textContent = "Xóa mô tả";

      const btnLocal = document.createElement("button");
      btnLocal.type = "button";
      btnLocal.className = "btn";
      btnLocal.textContent = "Chỉ template hiện tại";

      const btnGlobal = document.createElement("button");
      btnGlobal.type = "button";
      btnGlobal.className = "btn btn-primary";
      btnGlobal.textContent = "Toàn bộ template";

      actions.appendChild(btnCancel);
      actions.appendChild(btnClear);
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
      btnClear.addEventListener("click", () => close("clear"));
      btnLocal.addEventListener("click", () => close("local"));
      btnGlobal.addEventListener("click", () => close("global"));
      window.addEventListener("keydown", onKeyDown);

      document.body.appendChild(overlay);
      textarea.focus();
      textarea.select();
    });
  }

  function originLabel(origin) {
    const kind = String(origin?.kind || "").trim().toUpperCase();
    const key = String(origin?.key || "").trim();

    if (kind === "DECL") {
      const vn = String(origin?.entity?.variableName || "").trim();
      if (vn) return vn;
      return String(origin?.name || "").trim();
    }

    if (kind === "PARAM") {
      const pn = String(origin?.entity?.name || "").trim();
      if (pn) return pn;
      return String(origin?.name || "").trim();
    }

    if (kind === "TYPEFIELD") {
      const m = /^TYPEFIELD:[^:]*:([^:]+):(.+)$/.exec(key);
      if (m) return `${m[1]}-${m[2]}`;
      return String(origin?.name || "").trim();
    }

    return String(origin?.name || "").trim() || key;
  }

  function openDescriptionEditorDialogForTargets(options) {
    const titleText = String(options?.title || "Edit description");
    const targets = Array.isArray(options?.targets) ? options.targets : [];
    const initialByKey = options?.initialByKey && typeof options.initialByKey === "object" ? options.initialByKey : {};
    const initialKey = String(options?.initialKey || (targets[0] ? targets[0].key : "") || "").trim();

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

      let selectedKey = initialKey;
      const draftByKey = new Map();

      function getInitialForKey(key) {
        return String(initialByKey[String(key || "")] ?? "");
      }

      for (const t of targets) {
        const k = String(t?.key || "").trim();
        if (!k) continue;
        draftByKey.set(k, getInitialForKey(k));
      }

      let select = null;
      if (targets.length > 1) {
        const row = document.createElement("div");
        row.style.display = "flex";
        row.style.gap = "8px";
        row.style.alignItems = "center";
        row.style.marginBottom = "8px";

        const label = document.createElement("div");
        label.textContent = "Object:";
        label.style.minWidth = "64px";
        row.appendChild(label);

        select = document.createElement("select");
        select.className = "input";
        select.style.flex = "1";
        for (const t of targets) {
          const key = String(t?.key || "").trim();
          const labelText = String(t?.label || "").trim() || key;
          if (!key) continue;
          const opt = document.createElement("option");
          opt.value = key;
          opt.textContent = labelText;
          select.appendChild(opt);
        }
        if (selectedKey) select.value = selectedKey;
        row.appendChild(select);

        dialog.appendChild(row);
      }

      const textarea = document.createElement("textarea");
      textarea.className = "input demo-modal__textarea";
      textarea.value = selectedKey ? draftByKey.get(selectedKey) || "" : "";
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

      const btnClear = document.createElement("button");
      btnClear.type = "button";
      btnClear.className = "btn";
      btnClear.textContent = "Xóa mô tả";

      const btnLocal = document.createElement("button");
      btnLocal.type = "button";
      btnLocal.className = "btn";
      btnLocal.textContent = "Chỉ template hiện tại";

      const btnGlobal = document.createElement("button");
      btnGlobal.type = "button";
      btnGlobal.className = "btn btn-primary";
      btnGlobal.textContent = "Toàn bộ template";

      actions.appendChild(btnCancel);
      actions.appendChild(btnClear);
      actions.appendChild(btnLocal);
      actions.appendChild(btnGlobal);

      function cleanup() {
        window.removeEventListener("keydown", onKeyDown);
        overlay.remove();
      }

      function close(action) {
        if (selectedKey) draftByKey.set(selectedKey, textarea.value);
        const value = selectedKey ? draftByKey.get(selectedKey) || "" : textarea.value;
        cleanup();
        resolve({ action, value, targetKey: selectedKey });
      }

      function onKeyDown(e) {
        if (e.key === "Escape") {
          e.preventDefault();
          close("cancel");
        }
      }

      if (select) {
        select.addEventListener("change", () => {
          if (selectedKey) draftByKey.set(selectedKey, textarea.value);
          selectedKey = String(select.value || "").trim();
          textarea.value = selectedKey ? draftByKey.get(selectedKey) || "" : "";
          textarea.focus();
          textarea.select();
        });
      }

      overlay.addEventListener("click", (e) => {
        if (e.target === overlay) close("cancel");
      });
      btnCancel.addEventListener("click", () => close("cancel"));
      btnClear.addEventListener("click", () => close("clear"));
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

    // Multi-origin edit support (expressions / struct segments)
    try {
      const ctx = result?.context || null;
      const basePath = bind.slice(0, -".description".length);
      const node = ctx && typeof tpl.resolveBindPath === "function" ? tpl.resolveBindPath(ctx, basePath) : null;
      const baseOriginKey = String(node?.originKey || "").trim();

      const exprText = String(
        (typeof node?.text === "string" && node.text.trim()) ||
          (typeof node?.actual === "string" && node.actual.trim()) ||
          (typeof node?.name === "string" && node.name.trim()) ||
          "",
      ).trim();
      const callerKey = String(ctx?.caller?.key || "").trim();
      const routineKey = String(result?.routineKey || ctx?.routine?.key || "").trim();
      const evalRoutineKey = callerKey && typeof node?.actual === "string" ? callerKey : routineKey || callerKey;
      const callPath = Array.isArray(result?.callPath) ? result.callPath : null;

      if (state.model && evalRoutineKey && exprText && typeof ns.desc?.describeExpressionWithOrigin === "function") {
        const described = ns.desc.describeExpressionWithOrigin(state.model, evalRoutineKey, exprText, { callPath });
        const origins = Array.isArray(described?.origins) ? described.origins : [];

        if (origins.length > 1) {
          const targets = [];
          const seen = new Set();
          for (const o of origins) {
            const k = String(o?.key || "").trim();
            if (!k || seen.has(k)) continue;
            seen.add(k);
            targets.push({ key: k, label: originLabel(o), origin: o });
          }

          const overrides = typeof tpl.getLocalTemplateOverrides === "function" ? tpl.getLocalTemplateOverrides(templateId, resultId) : null;
          const overridesEntry = overrides && typeof overrides.get === "function" ? overrides.get(bind) : null;

          function hasLocalOverrideForTarget(targetKey) {
            const tk = String(targetKey || "").trim();
            if (!tk) return false;
            if (overridesEntry instanceof Map) return overridesEntry.has(tk);
            if (typeof overridesEntry === "string") return Boolean(baseOriginKey && tk === baseOriginKey && String(overridesEntry || "").trim());
            return false;
          }

          function localOverrideValueForTarget(targetKey) {
            const tk = String(targetKey || "").trim();
            if (!tk) return "";
            if (overridesEntry instanceof Map) return String(overridesEntry.get(tk) ?? "");
            if (typeof overridesEntry === "string" && baseOriginKey && tk === baseOriginKey) return String(overridesEntry ?? "");
            return "";
          }

          const initialByKey = {};
          for (const t2 of targets) {
            const k = String(t2?.key || "").trim();
            if (!k) continue;
            const localValue = localOverrideValueForTarget(k);
            if (localValue.trim()) {
              initialByKey[k] = localValue;
              continue;
            }
            const origin = t2.origin;
            const fallback = String(origin?.name || "").trim();
            initialByKey[k] = typeof ns.desc?.pickDescription === "function" ? ns.desc.pickDescription(origin?.entity || null, fallback) : fallback;
          }

          openDescriptionEditorDialogForTargets({ title, targets, initialByKey, initialKey: targets[0]?.key || "" }).then(
            ({ action, value, targetKey }) => {
              const tk = String(targetKey || "").trim();
              const nextValue = String(value ?? "").trim();
              if (action === "cancel") return;
              if (!tk) {
                ui.setStatus("Cannot resolve object for this cell.", true);
                return;
              }

              if (action === "clear") {
                if (hasLocalOverrideForTarget(tk)) {
                  tpl.setLocalTemplateOverrideByOriginKey?.(templateId, resultId, bind, tk, "", baseOriginKey);
                  renderTemplates({ autoSelectResultId: resultId });
                  ui.setStatus("Cleared description (local).", false);
                  return;
                }

                if (!ns.notes?.setEntry) {
                  ui.setStatus("Cannot clear description for this cell.", true);
                  return;
                }

                tpl.setLocalTemplateOverrideByOriginKey?.(templateId, resultId, bind, tk, "", baseOriginKey);
                ns.notes.setEntry(tk, { description: "" });
                if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
                renderTemplates({ autoSelectResultId: resultId });
                ui.setStatus("Cleared description.", false);
                return;
              }

              if (action === "local") {
                if (typeof tpl.setLocalTemplateOverrideByOriginKey === "function") {
                  tpl.setLocalTemplateOverrideByOriginKey(templateId, resultId, bind, tk, nextValue, baseOriginKey);
                } else {
                  tpl.setLocalTemplateOverride?.(templateId, resultId, bind, nextValue);
                }
                renderTemplates({ autoSelectResultId: resultId });
                ui.setStatus("Saved description (local).", false);
                return;
              }

              if (action === "global") {
                if (!ns.notes?.setEntry) {
                  ui.setStatus("Cannot update global description for this cell.", true);
                  return;
                }
                tpl.setLocalTemplateOverrideByOriginKey?.(templateId, resultId, bind, tk, "", baseOriginKey);
                ns.notes.setEntry(tk, { description: nextValue });
                if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
                renderTemplates({ autoSelectResultId: resultId });
                ui.setStatus("Saved description (global).", false);
              }
            },
          );

          return;
        }
      }
    } catch (_) {
      // ignore and fall back to single-key editor
    }

    const initial = String(td.textContent ?? "");

    openDescriptionEditorDialog({ title, value: initial }).then(({ action, value }) => {
      const nextValue = String(value ?? "").trim();
      if (action === "cancel") return;

      if (action === "clear") {
        const overrides = typeof tpl.getLocalTemplateOverrides === "function" ? tpl.getLocalTemplateOverrides(templateId, resultId) : null;
        if (overrides && typeof overrides.has === "function" && overrides.has(bind)) {
          tpl.setLocalTemplateOverride?.(templateId, resultId, bind, "");
          renderTemplates({ autoSelectResultId: resultId });
          ui.setStatus("Đã xóa mô tả (template hiện tại).", false);
          return;
        }

        const target = resolveGlobalDescriptionKey(result, bind);
        if (!target || !ns.notes?.setEntry) {
          ui.setStatus("Không thể xóa mô tả cho mục này.", true);
          return;
        }

        tpl.setLocalTemplateOverride?.(templateId, resultId, bind, "");
        ns.notes.setEntry(target.key, { description: "" });
        if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
        renderTemplates({ autoSelectResultId: resultId });
        ui.setStatus("Đã xóa mô tả.", false);
        return;
      }

      if (action === "local") {
        tpl.setLocalTemplateOverride(templateId, resultId, bind, nextValue);
        renderTemplates({ autoSelectResultId: resultId });
        ui.setStatus("Đã lưu mô tả (chỉ template hiện tại).", false);
        return;
      }

      if (action === "global") {
        const target = resolveGlobalDescriptionKey(result, bind);
        if (!target || !ns.notes?.setEntry) {
          ui.setStatus("Không thể cập nhật toàn bộ cho mục này.", true);
          return;
        }

        tpl.setLocalTemplateOverride(templateId, resultId, bind, "");
        ns.notes.setEntry(target.key, { description: nextValue });
        if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
        renderTemplates({ autoSelectResultId: resultId });
        ui.setStatus("Đã lưu mô tả (toàn bộ template).", false);
      }
    });
  }

  function updateTemplatesToolbar() {
    if (typeof lastToolbarUpdate === "function") lastToolbarUpdate();
  }

  function applyTemplateTreeState(host) {
    if (!host) return;

    const cards = Array.from(host.querySelectorAll(".template-block"));
    const collapsedDepths = [];

    for (const card of cards) {
      const depth = Math.max(0, Math.floor(Number(card.dataset.depth || 0)));
      while (collapsedDepths.length && depth <= collapsedDepths[collapsedDepths.length - 1]) collapsedDepths.pop();

      const hidden = collapsedDepths.length > 0;
      card.classList.toggle("is-tree-hidden", hidden);

      const resultId = String(card.dataset.resultId || "");
      const collapsed = collapsedTemplateResultIds.has(resultId);
      card.classList.toggle("is-collapsed", collapsed);

      const toggle = card.querySelector("button.template-block__toggle");
      if (toggle) toggle.setAttribute("aria-expanded", String(!collapsed));

      if (!hidden && collapsed) collapsedDepths.push(depth);
    }
  }

  function toggleTemplateCollapsed(resultId, host) {
    const id = String(resultId || "").trim();
    if (!id) return;
    if (collapsedTemplateResultIds.has(id)) collapsedTemplateResultIds.delete(id);
    else collapsedTemplateResultIds.add(id);
    applyTemplateTreeState(host || ui.$("templatesHost"));
  }

  function setTemplateSelected(resultId, selected) {
    const id = String(resultId || "").trim();
    if (!id) return;
    if (selected) selectedTemplateResultIds.add(id);
    else selectedTemplateResultIds.delete(id);
    updateTemplatesToolbar();
  }

  function cloneTableHtmlWithTrailingBlankRow(tableEl) {
    if (!tableEl || typeof tableEl.cloneNode !== "function") return "";
    const clone = tableEl.cloneNode(true);

    const colCount = clone.querySelectorAll("colgroup col").length || 1;

    const td = document.createElement("td");
    td.textContent = "\u00a0";
    td.style.border = "none";
    td.style.padding = "0";
    td.style.height = "18px";
    td.colSpan = colCount;

    const tr = document.createElement("tr");
    tr.appendChild(td);

    const tbody = clone.tBodies && clone.tBodies.length ? clone.tBodies[clone.tBodies.length - 1] : clone;
    tbody.appendChild(tr);

    return clone.outerHTML;
  }

  async function copySelectedTables() {
    if (!ui.clipboard?.copyHtml) {
      ui.setStatus("Clipboard module not loaded.", true);
      return;
    }

    const orderedIds = lastRenderOrder.filter((id) => selectedTemplateResultIds.has(id));
    const tables = orderedIds.map((id) => tableByResultId.get(id)).filter(Boolean);
    if (!tables.length) {
      ui.setStatus("No template selected.", true);
      return;
    }

    const html = tables
      .map((t, idx) => {
        const tableHtml = idx < tables.length - 1 ? cloneTableHtmlWithTrailingBlankRow(t) : t.outerHTML;
        return `<div>${tableHtml}</div>`;
      })
      .join("\n");
    const plain = tables.map((t) => t.innerText || "").join("\n\n");

    const ok = await ui.clipboard.copyHtml(html, plain);
    ui.setStatus(ok ? `Copied ${tables.length} table(s) to clipboard.` : "Copy failed (browser blocked clipboard).", !ok);
  }

  function renderTemplates(options) {
    const host = ui.$("templatesHost");
    if (!host) return;

    selectedTemplateCard = null;
    templateResultById = new Map();
    tableByResultId = new Map();
    lastRenderOrder = [];
    lastToolbarUpdate = null;

    if (typeof ui.clearAbapTemplateMarkers === "function") ui.clearAbapTemplateMarkers();

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

    const entries = tpl.listTemplateEntries();
    const templatesBySource = tpl.pickAutoTemplatesBySource(entries);
    if (templatesBySource.size === 0) {
      host.textContent = "No templates configured.";
      host.classList.add("empty");
      return;
    }

    const flow = tpl.buildTemplatesFlow(model, templatesBySource, { maxSteps: 900 });
    if (!flow.items.length) {
      host.textContent = "No template-mapped statements found.";
      host.classList.add("empty");
      return;
    }

    const markerItems = [];
    const autoSelectResultId = String(options?.autoSelectResultId || "");
    let autoSelectCard = null;

    host.classList.remove("empty");
    host.textContent = "";

    const toolbar = document.createElement("div");
    toolbar.className = "templates-toolbar";

    const toolbarCount = document.createElement("div");
    toolbarCount.className = "templates-toolbar__count";

    const toolbarActions = document.createElement("div");
    toolbarActions.className = "templates-toolbar__actions";

    const btnCopySelected = document.createElement("button");
    btnCopySelected.type = "button";
    btnCopySelected.className = "btn btn-sm btn-primary";
    btnCopySelected.textContent = "Copy selected";
    btnCopySelected.addEventListener("click", (e) => {
      e.preventDefault();
      copySelectedTables();
    });

    const btnClearSelected = document.createElement("button");
    btnClearSelected.type = "button";
    btnClearSelected.className = "btn btn-sm";
    btnClearSelected.textContent = "Clear selection";
    btnClearSelected.addEventListener("click", (e) => {
      e.preventDefault();
      selectedTemplateResultIds.clear();
      host.querySelectorAll(".template-block.is-multi-selected").forEach((c) => c.classList.remove("is-multi-selected"));
      host.querySelectorAll("input.template-block__select").forEach((cb) => (cb.checked = false));
      updateTemplatesToolbar();
    });

    toolbarActions.appendChild(btnCopySelected);
    toolbarActions.appendChild(btnClearSelected);

    toolbar.appendChild(toolbarCount);
    toolbar.appendChild(toolbarActions);

    host.appendChild(toolbar);

    lastToolbarUpdate = () => {
      const n = selectedTemplateResultIds.size;
      toolbarCount.textContent = `Selected: ${n}`;
      btnCopySelected.disabled = n === 0;
      btnClearSelected.disabled = n === 0;
    };
    updateTemplatesToolbar();

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
      lastRenderOrder.push(String(result.resultId || ""));

      if (result?.sourceRef?.startLine) {
        markerItems.push({
          resultId: result.resultId,
          objectId: result.objectId,
          templateId: result.templateId,
          startLine: result.sourceRef.startLine,
          endLine: result.sourceRef.endLine || result.sourceRef.startLine,
        });
      }

      const card = document.createElement("div");
      card.className = "template-block is-clickable";
      card.dataset.resultId = String(result.resultId || "");
      card.dataset.templateId = String(result.templateId || "");
      card.dataset.depth = String(item.depth || 0);
      card.style.marginLeft = `${Math.max(0, item.depth) * 18}px`;
      if (item.isRecursion || result.edge?.isInCycle) card.classList.add("is-cycle");

      const resultIdStr = String(result.resultId || "");

      const header = document.createElement("div");
      header.className = "template-block__header";

      const headerLeft = document.createElement("div");
      headerLeft.className = "template-block__header-left";

      const btnToggle = document.createElement("button");
      btnToggle.type = "button";
      btnToggle.className = "template-block__toggle";
      btnToggle.textContent = "▾";
      btnToggle.setAttribute("aria-label", "Collapse/expand");
      btnToggle.setAttribute("aria-expanded", String(!collapsedTemplateResultIds.has(resultIdStr)));
      btnToggle.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();
        toggleTemplateCollapsed(resultIdStr, host);
      });

      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.className = "template-block__select";
      checkbox.checked = selectedTemplateResultIds.has(resultIdStr);
      card.classList.toggle("is-multi-selected", checkbox.checked);
      checkbox.addEventListener("click", (e) => e.stopPropagation());
      checkbox.addEventListener("change", (e) => {
        e.stopPropagation();
        const isOn = Boolean(checkbox.checked);
        setTemplateSelected(resultIdStr, isOn);
        card.classList.toggle("is-multi-selected", isOn);
      });

      const title = document.createElement("div");
      title.className = "template-block__title";
      const src = result.sourceRef;
      const lineText = src?.startLine
        ? src.endLine && src.endLine !== src.startLine
          ? ` (L${src.startLine}-L${src.endLine})`
          : ` (L${src.startLine})`
        : "";
      const loopText = item.isRecursion ? " (loop)" : "";

      const ctx = result?.context || null;
      const originalFirstLine = String(result?.original || "")
        .replace(/\r\n/g, "\n")
        .split("\n")[0]
        .trim();

      let mainText = "";

      const performName = String(ctx?.perform?.name || "").trim();
      if (performName) {
        mainText = `PERFORM ${performName}`;
      } else if (ctx?.if) {
        const k = String(ctx?.if?.kind || "IF").trim().toUpperCase() || "IF";
        const cond = String(ctx?.if?.condition || "").trim();
        mainText = cond ? `${k} ${cond}` : k;
      } else if (ctx?.assignment) {
        const lhs = String(ctx?.assignment?.lhs || "").trim();
        const rhs = String(ctx?.assignment?.rhs || "").trim();
        mainText = lhs && rhs ? `${lhs} = ${rhs}` : lhs ? `${lhs} = ...` : "Assignment";
      } else if (ctx?.message) {
        mainText = String(ctx?.message?.statement || "").trim() || "MESSAGE";
      } else if (ctx?.itabOp) {
        const st = String(ctx?.itabOp?.statement || "").trim();
        const kind = String(ctx?.itabOp?.kind || "ITAB").trim();
        const table = String(ctx?.table?.text || "").trim();
        const fallback = `${kind}${table ? ` ${table}` : ""}`.trim();
        mainText = st || fallback || "ITAB";
      } else if (originalFirstLine) {
        mainText = originalFirstLine;
      } else {
        mainText = String(result?.objectId || result?.kind || "Template");
      }

      title.textContent = `${mainText}${lineText}${loopText}`;

      headerLeft.appendChild(btnToggle);
      headerLeft.appendChild(checkbox);
      headerLeft.appendChild(title);

      const headerActions = document.createElement("div");
      headerActions.className = "template-block__actions";

      const btnCopy = document.createElement("button");
      btnCopy.type = "button";
      btnCopy.className = "btn btn-sm";
      btnCopy.textContent = "Copy";
      btnCopy.title = "Copy HTML table to clipboard (paste into Excel)";
      btnCopy.addEventListener("click", async (e) => {
        e.preventDefault();
        e.stopPropagation();
        if (!ui.clipboard?.copyHtml) {
          ui.setStatus("Clipboard module not loaded.", true);
          return;
        }
        const t = tableByResultId.get(resultIdStr);
        if (!t) {
          ui.setStatus("No table to copy.", true);
          return;
        }
        const ok = await ui.clipboard.copyHtml(`<div>${t.outerHTML}</div>`, t.innerText || title.textContent || "");
        ui.setStatus(ok ? "Copied HTML table (paste into Excel)." : "Copy failed (browser blocked clipboard).", !ok);
      });

      headerActions.appendChild(btnCopy);

      header.appendChild(headerLeft);
      header.appendChild(headerActions);
      card.appendChild(header);

      const tableWrap = document.createElement("div");
      tableWrap.className = "template-block__table";
      const tableEl = ns.tableRenderer.renderExcelLikeTable(result.filledConfig);
      tableWrap.appendChild(tableEl);
      card.appendChild(tableWrap);

      if (tableEl && tableEl.tagName === "TABLE") {
        tableByResultId.set(resultIdStr, tableEl);
      }

      card.addEventListener("click", () => {
        if (selectedTemplateCard) selectedTemplateCard.classList.remove("is-selected");
        selectedTemplateCard = card;
        card.classList.add("is-selected");
        ui.highlightSource(result.sourceRef?.startLine, result.sourceRef?.endLine);
      });

      host.appendChild(card);
      if (autoSelectResultId && result.resultId === autoSelectResultId) autoSelectCard = card;
    }

    const available = new Set(lastRenderOrder);
    for (const id of Array.from(selectedTemplateResultIds)) {
      if (!available.has(id)) selectedTemplateResultIds.delete(id);
    }
    for (const id of Array.from(collapsedTemplateResultIds)) {
      if (!available.has(id)) collapsedTemplateResultIds.delete(id);
    }
    applyTemplateTreeState(host);
    updateTemplatesToolbar();

    if (typeof ui.renderAbapTemplateMarkers === "function") ui.renderAbapTemplateMarkers(markerItems);

    if (autoSelectCard) {
      autoSelectCard.click();
      try {
        autoSelectCard.scrollIntoView({ block: "center", behavior: "smooth" });
      } catch (_) {
        autoSelectCard.scrollIntoView();
      }
    }
  }

  ui.handleTemplatesDblClick = handleTemplatesDblClick;
  ui.renderTemplates = renderTemplates;
  ui.scrollToTemplateResultId = scrollToTemplateResultId;
})(window.AbapFlow);
