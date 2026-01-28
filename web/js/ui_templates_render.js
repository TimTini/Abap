(function (ns) {
  "use strict";

  const ui = ns.ui;
  const tpl = ui.templates;
  const state = ui.state;

  let templateResultById = new Map();
  let selectedTemplateCard = null;
  let selectedTemplateResultIds = new Set();
  let collapsedTemplateResultIds = new Set();
  let lastRenderOrder = [];
  let lastToolbarUpdate = null;

  const TEMPLATES_FLOW_MAX_STEPS_KEY = "abapflow-templates-flow-max-steps";
  const TEMPLATES_FLOW_MAX_STEPS_DEFAULT = 900;
  const TEMPLATES_FLOW_FILTER_TEXT_KEY = "abapflow-templates-flow-filter-text";
  const TEMPLATES_FLOW_FILTER_SELECTED_KEY = "abapflow-templates-flow-filter-selected";
  const TEMPLATES_FLOW_FILTER_LOOP_KEY = "abapflow-templates-flow-filter-loop";

  function readTemplatesFlowMaxSteps() {
    try {
      const raw = localStorage.getItem(TEMPLATES_FLOW_MAX_STEPS_KEY);
      const n = Number(raw);
      return Number.isFinite(n) && n > 0 ? Math.floor(n) : TEMPLATES_FLOW_MAX_STEPS_DEFAULT;
    } catch (_) {
      return TEMPLATES_FLOW_MAX_STEPS_DEFAULT;
    }
  }

  function writeTemplatesFlowMaxSteps(n) {
    const v = Number(n);
    const next = Number.isFinite(v) && v > 0 ? Math.floor(v) : TEMPLATES_FLOW_MAX_STEPS_DEFAULT;
    try {
      localStorage.setItem(TEMPLATES_FLOW_MAX_STEPS_KEY, String(next));
    } catch (_) {}
    return next;
  }

  function readStoredText(key, fallback) {
    try {
      const raw = localStorage.getItem(key);
      if (raw == null) return String(fallback || "");
      return String(raw);
    } catch (_) {
      return String(fallback || "");
    }
  }

  function writeStoredText(key, value) {
    try {
      localStorage.setItem(key, String(value ?? ""));
    } catch (_) {}
  }

  function readStoredBool(key, fallback) {
    try {
      const raw = localStorage.getItem(key);
      if (raw == null) return Boolean(fallback);
      return raw === "1" || raw === "true";
    } catch (_) {
      return Boolean(fallback);
    }
  }

  function writeStoredBool(key, value) {
    try {
      localStorage.setItem(key, value ? "1" : "0");
    } catch (_) {}
  }

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
      return item ? `Mô tả Item: ${item}` : "Mô tả Item";
    }

    if (b === "value.description") {
      const value = String(ctx?.assignment?.rhs || "").trim();
      return value ? `Mô tả giá trị: ${value}` : "Mô tả giá trị";
    }

    if (b === "table.description") {
      const text = String(ctx?.table?.text || "").trim();
      return text ? `Mô tả bảng nội bộ: ${text}` : "Mô tả bảng nội bộ";
    }

    if (b === "target.description") {
      const text = String(ctx?.target?.text || "").trim();
      return text ? `Mô tả đích: ${text}` : "Mô tả đích";
    }

    const msgMatch = /^(msgClass|msgNo|displayLike|messageText|into|raising)\.description$/.exec(b);
    if (msgMatch) {
      const prop = String(msgMatch[1] || "");
      const node = ctx?.[prop] || null;
      const text = String(node?.text || "").trim();
      const label =
        prop === "msgClass"
          ? "Nhóm message"
          : prop === "msgNo"
            ? "Số message"
            : prop === "displayLike"
              ? "Hiển thị như"
              : prop === "messageText"
                ? "Nội dung message"
                : prop === "into"
                  ? "Đích"
                  : prop === "raising"
                    ? "Ngoại lệ"
                    : prop;
      return text ? `Mô tả ${label}: ${text}` : `Mô tả ${label}`;
    }

    const withMatch = /^with\[(\d+)\]\.description$/.exec(b);
    if (withMatch) {
      const idx = Number(withMatch[1]);
      const node = ctx?.with?.[idx] || null;
      const text = String(node?.text || "").trim();
      const label = `Biến message &${idx + 1}`;
      return text ? `Mô tả ${label}: ${text}` : `Mô tả ${label}`;
    }

    const condMatch = /^conditions\[(\d+)\]\.(item1|item2)\.description$/.exec(b);
    if (condMatch) {
      const idx = Number(condMatch[1]);
      const side = String(condMatch[2] || "").trim();
      const cond = ctx?.conditions?.[idx] || null;
      const itemText = side === "item2" ? String(cond?.item2?.text || "").trim() : String(cond?.item1?.text || "").trim();
      const op = String(cond?.operator || "").trim();
      const label = side === "item2" ? "Mục 2" : "Mục 1";
      const tail = itemText ? `${itemText}${op ? ` (${op})` : ""}` : `#${idx + 1}`;
      return `Mô tả ${label}: ${tail}`;
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
    if (!b || !isEditableDescriptionBind(b)) return "Sửa mô tả";

    const ctx = result?.context || null;
    const basePath = b.slice(0, -".description".length);
    const node = typeof tpl.resolveBindPath === "function" ? tpl.resolveBindPath(ctx, basePath) : null;

    if (basePath === "perform") {
      const performName = String(ctx?.perform?.name || "").trim();
      return performName ? `Mô tả FORM ${performName}` : "Mô tả FORM";
    }

    const text = String(node?.text ?? node?.actual ?? node?.name ?? "").trim();
    if (text) return `Mô tả: ${text}`;
    return `Mô tả: ${basePath}`;
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
    if (typeof ui.openTextEditorModal !== "function") {
      return Promise.resolve({ action: "cancel", value: String(options?.value ?? "") });
    }

    return ui.openTextEditorModal({
      title: String(options?.title || "Sửa mô tả"),
      value: String(options?.value ?? ""),
      placeholder: "Nhập mô tả...",
      hint: "Chọn phạm vi cập nhật:",
      actions: [
        { key: "cancel", label: "Hủy" },
        { key: "clear", label: "Xóa mô tả" },
        { key: "local", label: "Chỉ template hiện tại" },
        { key: "global", label: "Toàn bộ template", className: "btn btn-primary" },
      ],
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
    if (typeof ui.openTextEditorModal !== "function") {
      return Promise.resolve({ action: "cancel", value: "", targetKey: "" });
    }

    return ui.openTextEditorModal({
      title: String(options?.title || "Sửa mô tả"),
      targets: Array.isArray(options?.targets) ? options.targets : [],
      initialByKey: options?.initialByKey && typeof options.initialByKey === "object" ? options.initialByKey : {},
      initialKey: String(options?.initialKey || ""),
      selectLabel: "Đối tượng:",
      placeholder: "Nhập mô tả...",
      hint: "Chọn phạm vi cập nhật:",
      actions: [
        { key: "cancel", label: "Hủy" },
        { key: "clear", label: "Xóa mô tả" },
        { key: "local", label: "Chỉ template hiện tại" },
        { key: "global", label: "Toàn bộ template", className: "btn btn-primary" },
      ],
    });
  }

  function handleTemplatesDblClick(event) {
    const t = event?.target && event.target.nodeType === 1 ? event.target : event?.target?.parentElement;
    const el = t?.closest ? t.closest("[data-bind]") : null;
    if (!el) return;

    const bind = String(el.dataset.bind || "").trim();
    if (!isEditableDescriptionBind(bind)) return;

    const card = el.closest(".template-block");
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
                ui.setStatus("Không thể xác định đối tượng cho ô này.", true);
                return;
              }

              if (action === "clear") {
                if (hasLocalOverrideForTarget(tk)) {
                  tpl.setLocalTemplateOverrideByOriginKey?.(templateId, resultId, bind, tk, "", baseOriginKey);
                  renderTemplates({ autoSelectResultId: resultId });
                  ui.setStatus("Đã xóa mô tả (cục bộ).", false);
                  return;
                }

                if (!ns.notes?.setEntry) {
                  ui.setStatus("Không thể xóa mô tả cho ô này.", true);
                  return;
                }

                tpl.setLocalTemplateOverrideByOriginKey?.(templateId, resultId, bind, tk, "", baseOriginKey);
                ns.notes.setEntry(tk, { description: "" });
                if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
                renderTemplates({ autoSelectResultId: resultId });
                ui.setStatus("Đã xóa mô tả.", false);
                return;
              }

              if (action === "local") {
                if (typeof tpl.setLocalTemplateOverrideByOriginKey === "function") {
                  tpl.setLocalTemplateOverrideByOriginKey(templateId, resultId, bind, tk, nextValue, baseOriginKey);
                } else {
                  tpl.setLocalTemplateOverride?.(templateId, resultId, bind, nextValue);
                }
                renderTemplates({ autoSelectResultId: resultId });
                ui.setStatus("Đã lưu mô tả (cục bộ).", false);
                return;
              }

              if (action === "global") {
                if (!ns.notes?.setEntry) {
                  ui.setStatus("Không thể cập nhật mô tả toàn cục cho ô này.", true);
                  return;
                }
                tpl.setLocalTemplateOverrideByOriginKey?.(templateId, resultId, bind, tk, "", baseOriginKey);
                ns.notes.setEntry(tk, { description: nextValue });
                if (state.model && ns.notes?.applyToModel) ns.notes.applyToModel(state.model);
                renderTemplates({ autoSelectResultId: resultId });
                ui.setStatus("Đã lưu mô tả (toàn cục).", false);
              }
            },
          );

          return;
        }
      }
    } catch (_) {
      // ignore and fall back to single-key editor
    }

    const initial = String(el.textContent ?? "");

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

  function escapeXmlAttr(value) {
    return String(value ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&apos;");
  }

  function wrapCdata(text) {
    const raw = String(text ?? "");
    if (!raw) return "<![CDATA[]]>";
    return `<![CDATA[${raw.replace(/]]>/g, "]]]]><![CDATA[>")}]]>`;
  }

  function safeXmlTagName(name) {
    const raw = String(name ?? "").trim();
    if (!raw) return "item";

    let out = raw.replace(/[^A-Za-z0-9_.:-]/g, "_");
    if (!/^[A-Za-z_]/.test(out)) out = `k_${out}`;
    return out;
  }

  function appendXmlValue(lines, tagName, value, indent, seen) {
    const pad = " ".repeat(Math.max(0, Number(indent || 0)));
    const tag = safeXmlTagName(tagName);

    if (value == null) {
      lines.push(`${pad}<${tag} />`);
      return;
    }

    if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
      lines.push(`${pad}<${tag}>${wrapCdata(String(value))}</${tag}>`);
      return;
    }

    if (Array.isArray(value)) {
      lines.push(`${pad}<${tag}>`);
      for (const it of value) appendXmlValue(lines, "item", it, indent + 2, seen);
      lines.push(`${pad}</${tag}>`);
      return;
    }

    if (typeof value === "object") {
      if (seen.has(value)) {
        lines.push(`${pad}<${tag} />`);
        return;
      }

      seen.add(value);
      lines.push(`${pad}<${tag}>`);
      for (const [k, v] of Object.entries(value)) appendXmlValue(lines, k, v, indent + 2, seen);
      lines.push(`${pad}</${tag}>`);
      seen.delete(value);
      return;
    }

    lines.push(`${pad}<${tag}>${wrapCdata(String(value))}</${tag}>`);
  }

  function buildTemplatesExportXml(items) {
    const stamp = new Date().toISOString();
    const lines = [];
    lines.push('<?xml version="1.0" encoding="utf-8"?>');
    lines.push(`<abapflowObjects schema="abapflow-objects-export" version="1" createdAt="${escapeXmlAttr(stamp)}">`);

    for (const it of items || []) {
      if (!it) continue;
      const src = it.sourceRef || null;
      const startLine = src?.startLine != null ? String(src.startLine) : "";
      const endLine = src?.endLine != null ? String(src.endLine) : "";
      const depth = it.depth != null ? String(it.depth) : "";

      lines.push(
        `  <object kind="${escapeXmlAttr(it.kind)}" resultId="${escapeXmlAttr(it.resultId)}" objectId="${escapeXmlAttr(it.objectId)}" templateId="${escapeXmlAttr(it.templateId)}"${depth ? ` depth="${escapeXmlAttr(depth)}"` : ""}${startLine ? ` startLine="${escapeXmlAttr(startLine)}"` : ""}${endLine ? ` endLine="${escapeXmlAttr(endLine)}"` : ""}>`,
      );
      lines.push(`    <routineKey>${wrapCdata(it.routineKey)}</routineKey>`);
      lines.push(`    <original>${wrapCdata(it.original)}</original>`);

      lines.push("    <context>");
      const ctx = it.context && typeof it.context === "object" ? it.context : null;
      if (ctx) {
        const seen = new Set();
        for (const [k, v] of Object.entries(ctx)) appendXmlValue(lines, k, v, 6, seen);
      }
      lines.push("    </context>");

      lines.push("  </object>");
    }

    lines.push("</abapflowObjects>");
    lines.push("");
    return lines.join("\n");
  }

  function getExportResultIds() {
    const orderedIds = lastRenderOrder.filter((id) => selectedTemplateResultIds.has(id));
    if (orderedIds.length) return orderedIds;
    const active = String(selectedTemplateCard?.dataset?.resultId || "").trim();
    if (active) return [active];
    return [];
  }

  function collectTemplatesExportItems(resultIds) {
    const out = [];

    for (const id of resultIds || []) {
      const rid = String(id || "").trim();
      if (!rid) continue;
      const result = templateResultById.get(rid) || null;
      if (!result) continue;

      out.push({
        kind: String(result.kind || ""),
        resultId: String(result.resultId || rid),
        objectId: String(result.objectId || ""),
        templateId: String(result.templateId || ""),
        routineKey: String(result.routineKey || ""),
        sourceRef: result.sourceRef || null,
        original: String(result.original || ""),
        context: result.context || null,
      });
    }

    return out;
  }

  async function handleTemplatesExport(action) {
    const ids = getExportResultIds();
    if (!ids.length) {
      ui.setStatus("Hãy chọn mẫu (bấm) hoặc tick checkbox trước.", true);
      return;
    }

    const items = collectTemplatesExportItems(ids);
    if (!items.length) {
      ui.setStatus("Không có gì để xuất.", true);
      return;
    }

    const stamp = new Date().toISOString().replace(/[:]/g, "-").slice(0, 19);

    const text = buildTemplatesExportXml(items);
    const filename = `abapflow-objects-${stamp}.xml`;
    const mimeType = "application/xml";

    if (action === "download") {
      ui.downloadTextFile(filename, text, mimeType);
      ui.setStatus(`Đã tải xuống: ${filename}`, false);
      return;
    }

    if (!ui.clipboard?.copyHtml) {
      ui.setStatus("Chưa tải module clipboard.", true);
      return;
    }

    const ok = await ui.clipboard.copyHtml("", text);
    ui.setStatus(ok ? `Đã sao chép XML (${items.length} mục).` : "Sao chép thất bại (trình duyệt chặn clipboard).", !ok);
  }

  function compareNaturalStrings(a, b) {
    const aa = String(a ?? "");
    const bb = String(b ?? "");
    if (aa === bb) return 0;

    const ra = aa.match(/\d+|\D+/g) || [];
    const rb = bb.match(/\d+|\D+/g) || [];
    const n = Math.min(ra.length, rb.length);

    for (let i = 0; i < n; i++) {
      const ta = ra[i];
      const tb = rb[i];
      if (ta === tb) continue;

      const da = /^\d+$/.test(ta);
      const db = /^\d+$/.test(tb);
      if (da && db) {
        const na = Number(ta);
        const nb = Number(tb);
        if (na !== nb) return na - nb;
      }

      const cmp = ta.localeCompare(tb);
      if (cmp) return cmp;
    }

    return ra.length - rb.length;
  }

  function parseExcelAddress(addr) {
    const raw = String(addr ?? "").trim();
    if (!raw) return null;

    const m = /^([A-Za-z]+)(\d+)$/.exec(raw);
    if (!m) return null;

    const letters = String(m[1] || "").toUpperCase();
    const row = Number(m[2]);
    if (!Number.isFinite(row) || row <= 0) return null;

    let col = 0;
    for (let i = 0; i < letters.length; i++) {
      const code = letters.charCodeAt(i);
      if (code < 65 || code > 90) return null;
      col = col * 26 + (code - 64);
    }

    if (!Number.isFinite(col) || col <= 0) return null;
    return { row, col };
  }

  function excelPosKey(addr) {
    const parsed = parseExcelAddress(addr);
    if (!parsed) return Number.POSITIVE_INFINITY;
    return parsed.row * 10000 + parsed.col;
  }

  function renderTemplateStruct(result) {
    const filled = result?.filledConfig || null;
    const cells = Array.isArray(filled?.cells) ? filled.cells : [];
    const bindToEntry = new Map();

    for (const cell of cells) {
      const bind = String(cell?.bind || "").trim();
      if (!bind) continue;
      const text = String(cell?.text ?? "");
      const pos = excelPosKey(cell?.addr);

      if (!bindToEntry.has(bind)) bindToEntry.set(bind, { text, pos });
      else {
        const entry = bindToEntry.get(bind);
        entry.text = text;
        entry.pos = Math.min(entry.pos, pos);
      }
    }

    const entries = Array.from(bindToEntry.entries())
      .map(([bind, entry]) => ({ bind, text: entry.text, pos: entry.pos }))
      .sort((a, b) => {
        if (a.pos !== b.pos) return a.pos - b.pos;
        return compareNaturalStrings(a.bind, b.bind);
      });

    const root = { key: "", bind: "", value: null, children: new Map() };

    for (const { bind, text } of entries) {
      const parts = bind
        .split(".")
        .map((p) => p.trim())
        .filter(Boolean);
      if (!parts.length) continue;

      let cur = root;
      let path = "";
      for (const part of parts) {
        path = path ? `${path}.${part}` : part;
        if (!cur.children.has(part)) cur.children.set(part, { key: part, bind: path, value: null, children: new Map() });
        cur = cur.children.get(part);
      }
      cur.value = text;
    }

    const host = document.createElement("div");
    host.className = "template-struct";

    function renderNode(node, depth) {
      const row = document.createElement("div");
      row.className = "template-struct__row";
      row.style.paddingLeft = `${Math.max(0, depth) * 14}px`;

      const keyEl = document.createElement("div");
      keyEl.className = "template-struct__key";
      keyEl.textContent = node.key;

      const valueEl = document.createElement("div");
      valueEl.className = "template-struct__value";

      const hasValue = typeof node.value === "string";
      if (hasValue) {
        const raw = String(node.value ?? "");
        const trimmed = raw.trim();
        valueEl.textContent = trimmed ? raw : "(trống)";
        if (!trimmed) valueEl.classList.add("template-struct__value--empty");

        if (isEditableDescriptionBind(node.bind)) {
          valueEl.dataset.bind = node.bind;
          valueEl.classList.add("template-struct__value--editable");
          valueEl.title = "Double-click để sửa mô tả (cục bộ/toàn cục).";
        } else {
          valueEl.title = node.bind;
        }
      } else if (node.children.size) {
        valueEl.textContent = "";
      } else {
        valueEl.textContent = "(trống)";
        valueEl.classList.add("template-struct__value--empty");
      }

      row.appendChild(keyEl);
      row.appendChild(valueEl);
      host.appendChild(row);

      for (const child of node.children.values()) renderNode(child, depth + 1);
    }

    if (!root.children.size) {
      const empty = document.createElement("div");
      empty.className = "template-struct__empty";
      empty.textContent = "Không có giá trị ràng buộc.";
      host.appendChild(empty);
      return host;
    }

    for (const child of root.children.values()) renderNode(child, 0);
    return host;
  }

  function renderTemplates(options) {
    const host = ui.$("templatesHost");
    if (!host) return;

    selectedTemplateCard = null;
    templateResultById = new Map();
    lastRenderOrder = [];
    lastToolbarUpdate = null;

    if (typeof ui.clearAbapTemplateMarkers === "function") ui.clearAbapTemplateMarkers();

    const model = state.model;
    if (!model) {
      host.textContent = "Hãy phân tích để xem mẫu.";
      host.classList.add("empty");
      return;
    }

    if (!ns.templateRegistry || !ns.templateConverter) {
      host.textContent = "Chưa tải module mẫu.";
      host.classList.add("empty");
      return;
    }

    const entries = tpl.listTemplateEntries();
    const templatesBySource = tpl.pickAutoTemplatesBySource(entries);
    if (templatesBySource.size === 0) {
      host.textContent = "Chưa cấu hình mẫu.";
      host.classList.add("empty");
      return;
    }

    const maxStepsRaw = Number(options?.maxSteps ?? readTemplatesFlowMaxSteps());
    const maxSteps = Number.isFinite(maxStepsRaw) && maxStepsRaw > 0 ? Math.floor(maxStepsRaw) : TEMPLATES_FLOW_MAX_STEPS_DEFAULT;

    const filterTextRaw = String(options?.filterText ?? readStoredText(TEMPLATES_FLOW_FILTER_TEXT_KEY, "")).trim();
    const filterText = filterTextRaw.toLowerCase();
    const filterSelectedOnly = Boolean(options?.filterSelectedOnly ?? readStoredBool(TEMPLATES_FLOW_FILTER_SELECTED_KEY, false));
    const filterLoopOnly = Boolean(options?.filterLoopOnly ?? readStoredBool(TEMPLATES_FLOW_FILTER_LOOP_KEY, false));
    const hasFilter = Boolean(filterText || filterSelectedOnly || filterLoopOnly);

    const flow = tpl.buildTemplatesFlow(model, templatesBySource, { maxSteps });
    if (!flow.items.length) {
      host.textContent = "Không có câu lệnh khớp mẫu.";
      host.classList.add("empty");
      return;
    }

    templateResultById = new Map();
    lastRenderOrder = [];
    for (const it of flow.items) {
      if (!it || it.kind !== "template") continue;
      const res = it.result;
      if (!res) continue;
      templateResultById.set(String(res.resultId || ""), res);
      lastRenderOrder.push(String(res.resultId || ""));
    }

    function matchesFilter(item) {
      const result = item?.result;
      if (!result) return false;
      const rid = String(result.resultId || "");
      if (filterSelectedOnly && !selectedTemplateResultIds.has(rid)) return false;
      if (filterLoopOnly && !(item.isRecursion || result.edge?.isInCycle)) return false;
      if (!filterText) return true;
      const hay = [
        result.objectId,
        result.templateId,
        result.kind,
        result.original,
        result.context?.perform?.name,
      ]
        .filter(Boolean)
        .join(" ")
        .toLowerCase();
      return hay.includes(filterText);
    }

    const renderItems = [];
    let pendingSeparator = null;
    let noteText = "";
    for (const item of flow.items) {
      if (!item) continue;
      if (item.kind === "separator") {
        pendingSeparator = item;
        continue;
      }
      if (item.kind === "note") {
        noteText = String(item.text || "");
        continue;
      }
      if (item.kind !== "template") continue;
      if (!hasFilter || matchesFilter(item)) {
        if (pendingSeparator) {
          renderItems.push(pendingSeparator);
          pendingSeparator = null;
        }
        renderItems.push(item);
      }
    }
    if (noteText && (!hasFilter || renderItems.length)) renderItems.push({ kind: "note", text: noteText });

    const visibleTemplates = renderItems.reduce((acc, it) => acc + (it?.kind === "template" ? 1 : 0), 0);
    if (!renderItems.length) {
      host.textContent = hasFilter ? "Không có mẫu phù hợp với bộ lọc." : "Không có câu lệnh khớp mẫu.";
      host.classList.add("empty");
      return;
    }

    const markerItems = [];
    const autoSelectResultId = String(options?.autoSelectResultId || "");
    let autoSelectCard = null;
    const totalTemplates = lastRenderOrder.length;
    let visibleResultIds = [];

    host.classList.remove("empty");
    host.textContent = "";

    const toolbar = document.createElement("div");
    toolbar.className = "templates-toolbar";

    const toolbarCount = document.createElement("div");
    toolbarCount.className = "templates-toolbar__count";

    const toolbarFilters = document.createElement("div");
    toolbarFilters.className = "templates-toolbar__filters";

    const searchInput = document.createElement("input");
    searchInput.type = "search";
    searchInput.className = "input input-sm";
    searchInput.placeholder = "Lọc theo object/template/câu lệnh...";
    searchInput.value = filterTextRaw;
    toolbarFilters.appendChild(searchInput);

    const filterSelectedLabel = document.createElement("label");
    filterSelectedLabel.className = "toolbar-check";
    const filterSelectedInput = document.createElement("input");
    filterSelectedInput.type = "checkbox";
    filterSelectedInput.checked = filterSelectedOnly;
    filterSelectedLabel.appendChild(filterSelectedInput);
    filterSelectedLabel.appendChild(document.createTextNode("Chỉ đã chọn"));
    toolbarFilters.appendChild(filterSelectedLabel);

    const filterLoopLabel = document.createElement("label");
    filterLoopLabel.className = "toolbar-check";
    const filterLoopInput = document.createElement("input");
    filterLoopInput.type = "checkbox";
    filterLoopInput.checked = filterLoopOnly;
    filterLoopLabel.appendChild(filterLoopInput);
    filterLoopLabel.appendChild(document.createTextNode("Chỉ vòng lặp"));
    toolbarFilters.appendChild(filterLoopLabel);

    const btnClearFilter = document.createElement("button");
    btnClearFilter.type = "button";
    btnClearFilter.className = "btn btn-sm";
    btnClearFilter.textContent = "Xóa lọc";
    btnClearFilter.disabled = !hasFilter;
    toolbarFilters.appendChild(btnClearFilter);

    const toolbarActions = document.createElement("div");
    toolbarActions.className = "templates-toolbar__actions";

    const btnSelectVisible = document.createElement("button");
    btnSelectVisible.type = "button";
    btnSelectVisible.className = "btn btn-sm";
    btnSelectVisible.textContent = "Chọn hiển thị";

    const btnClearSelected = document.createElement("button");
    btnClearSelected.type = "button";
    btnClearSelected.className = "btn btn-sm";
    btnClearSelected.textContent = "Bỏ chọn";
    btnClearSelected.addEventListener("click", (e) => {
      e.preventDefault();
      selectedTemplateResultIds.clear();
      host.querySelectorAll(".template-block.is-multi-selected").forEach((c) => c.classList.remove("is-multi-selected"));
      host.querySelectorAll("input.template-block__select").forEach((cb) => (cb.checked = false));
      updateTemplatesToolbar();
    });

    const btnCollapseAll = document.createElement("button");
    btnCollapseAll.type = "button";
    btnCollapseAll.className = "btn btn-sm";
    btnCollapseAll.textContent = "Thu gọn hết";

    const btnExpandAll = document.createElement("button");
    btnExpandAll.type = "button";
    btnExpandAll.className = "btn btn-sm";
    btnExpandAll.textContent = "Mở hết";

    toolbarActions.appendChild(btnSelectVisible);
    toolbarActions.appendChild(btnClearSelected);
    toolbarActions.appendChild(btnCollapseAll);
    toolbarActions.appendChild(btnExpandAll);

    const maxStepsLabel = document.createElement("label");
    maxStepsLabel.className = "toolbar-check";

    const maxStepsText = document.createElement("span");
    maxStepsText.textContent = "Giới hạn bước";

    const maxStepsInput = document.createElement("input");
    maxStepsInput.type = "number";
    maxStepsInput.min = "100";
    maxStepsInput.step = "100";
    maxStepsInput.value = String(maxSteps);
    maxStepsInput.className = "input";
    maxStepsInput.style.width = "110px";

    maxStepsLabel.appendChild(maxStepsText);
    maxStepsLabel.appendChild(maxStepsInput);
    toolbarActions.appendChild(maxStepsLabel);

    function rerenderWithFilters(nextMaxSteps) {
      const keepActive = String(selectedTemplateCard?.dataset?.resultId || "").trim();
      const max = Number(nextMaxSteps ?? maxStepsInput?.value ?? maxSteps);
      renderTemplates({
        autoSelectResultId: keepActive,
        maxSteps: max,
        filterText: searchInput.value,
        filterSelectedOnly: filterSelectedInput.checked,
        filterLoopOnly: filterLoopInput.checked,
      });
    }

    searchInput.addEventListener("input", () => {
      writeStoredText(TEMPLATES_FLOW_FILTER_TEXT_KEY, searchInput.value);
      rerenderWithFilters();
    });

    filterSelectedInput.addEventListener("change", () => {
      writeStoredBool(TEMPLATES_FLOW_FILTER_SELECTED_KEY, filterSelectedInput.checked);
      rerenderWithFilters();
    });

    filterLoopInput.addEventListener("change", () => {
      writeStoredBool(TEMPLATES_FLOW_FILTER_LOOP_KEY, filterLoopInput.checked);
      rerenderWithFilters();
    });

    btnClearFilter.addEventListener("click", (e) => {
      e.preventDefault();
      searchInput.value = "";
      filterSelectedInput.checked = false;
      filterLoopInput.checked = false;
      writeStoredText(TEMPLATES_FLOW_FILTER_TEXT_KEY, "");
      writeStoredBool(TEMPLATES_FLOW_FILTER_SELECTED_KEY, false);
      writeStoredBool(TEMPLATES_FLOW_FILTER_LOOP_KEY, false);
      rerenderWithFilters();
    });

    maxStepsInput.addEventListener("change", () => {
      const next = writeTemplatesFlowMaxSteps(maxStepsInput.value);
      rerenderWithFilters(next);
    });

    const exportLabel = document.createElement("span");
    exportLabel.textContent = "Xuất (đã chọn):";
    exportLabel.style.color = "var(--muted)";
    exportLabel.style.fontSize = "12px";
    exportLabel.style.alignSelf = "center";

    const btnCopyExport = document.createElement("button");
    btnCopyExport.type = "button";
    btnCopyExport.className = "btn btn-sm";
    btnCopyExport.textContent = "Sao chép";
    btnCopyExport.addEventListener("click", async (e) => {
      e.preventDefault();
      await handleTemplatesExport("copy");
    });

    const btnDownloadExport = document.createElement("button");
    btnDownloadExport.type = "button";
    btnDownloadExport.className = "btn btn-sm";
    btnDownloadExport.textContent = "Tải xuống";
    btnDownloadExport.addEventListener("click", (e) => {
      e.preventDefault();
      handleTemplatesExport("download");
    });

    toolbarActions.appendChild(exportLabel);
    toolbarActions.appendChild(btnCopyExport);
    toolbarActions.appendChild(btnDownloadExport);

    toolbar.appendChild(toolbarCount);
    toolbar.appendChild(toolbarFilters);
    toolbar.appendChild(toolbarActions);

    host.appendChild(toolbar);

    btnSelectVisible.addEventListener("click", (e) => {
      e.preventDefault();
      const visibleSet = new Set(visibleResultIds.map((id) => String(id || "")));
      if (!visibleSet.size) return;
      host.querySelectorAll("input.template-block__select").forEach((cb) => {
        const card = cb.closest(".template-block");
        const rid = String(card?.dataset?.resultId || "");
        if (!visibleSet.has(rid)) return;
        cb.checked = true;
        selectedTemplateResultIds.add(rid);
        card?.classList.add("is-multi-selected");
      });
      updateTemplatesToolbar();
    });

    btnCollapseAll.addEventListener("click", (e) => {
      e.preventDefault();
      for (const id of visibleResultIds) collapsedTemplateResultIds.add(String(id || ""));
      applyTemplateTreeState(host);
    });

    btnExpandAll.addEventListener("click", (e) => {
      e.preventDefault();
      for (const id of visibleResultIds) collapsedTemplateResultIds.delete(String(id || ""));
      applyTemplateTreeState(host);
    });

    lastToolbarUpdate = () => {
      const n = selectedTemplateResultIds.size;
      const visibleText = hasFilter ? ` • Hiển thị: ${visibleTemplates}` : "";
      toolbarCount.textContent = `Mẫu: ${totalTemplates}${visibleText} • Đã chọn: ${n}`;
      btnClearSelected.disabled = n === 0;
      btnSelectVisible.disabled = visibleTemplates === 0;
      btnCollapseAll.disabled = visibleTemplates === 0;
      btnExpandAll.disabled = visibleTemplates === 0;
    };
    updateTemplatesToolbar();

    for (const item of renderItems) {
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
      if (!result) continue;
      visibleResultIds.push(String(result.resultId || ""));

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
      btnToggle.setAttribute("aria-label", "Thu gọn/mở rộng");
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
      const loopText = item.isRecursion ? " (vòng lặp)" : "";

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
        mainText = lhs && rhs ? `${lhs} = ${rhs}` : lhs ? `${lhs} = ...` : "Gán";
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
        mainText = String(result?.objectId || result?.kind || "Mẫu");
      }

      title.textContent = `${mainText}${lineText}${loopText}`;

      headerLeft.appendChild(btnToggle);
      headerLeft.appendChild(checkbox);
      headerLeft.appendChild(title);

      header.appendChild(headerLeft);
      card.appendChild(header);

      const contentWrap = document.createElement("div");
      contentWrap.className = "template-block__table";
      contentWrap.appendChild(renderTemplateStruct(result));
      card.appendChild(contentWrap);

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
  ui.buildAbapflowObjectsXml = buildTemplatesExportXml;
})(window.AbapFlow);
