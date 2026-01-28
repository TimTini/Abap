(function (ns) {
  "use strict";

  ns.ui = ns.ui || {};
  const ui = ns.ui;

  ui.state = ui.state || {
    model: null,
    selectedKey: null,
  };

  ui.constants = ui.constants || {};
  if (!ui.constants.TRACE_ALL_GLOBALS_KEY) ui.constants.TRACE_ALL_GLOBALS_KEY = "TRACE:ALL_GLOBALS";

  ui.$ =
    ui.$ ||
    function $(id) {
      return document.getElementById(id);
    };

  ui.setStatus =
    ui.setStatus ||
    function setStatus(message, isError) {
      const el = ui.$("statusBar");
      if (!el) return;
      el.textContent = message;
      el.style.color = isError ? "#fecaca" : "";
    };

  ui.setActiveTab =
    ui.setActiveTab ||
    function setActiveTab(name) {
      const tabs = document.querySelectorAll(".tab");
      const panels = document.querySelectorAll(".tab-panel");
      for (const t of tabs) t.classList.toggle("is-active", t.dataset.tab === name);
      for (const p of panels) p.classList.toggle("is-active", p.id === `tab-${name}`);
    };

  ui.highlightSource =
    ui.highlightSource ||
    function highlightSource(startLine, endLine) {
      const model = ui.state.model;
      if (!model) return;
      const ta = ui.$("abapInput");
      if (!ta) return;

      const startIdx = Math.max(1, Number(startLine || 1));
      const endIdx = Math.max(startIdx, Number(endLine || startIdx));

      const offsets = model.lineStartOffsets;
      const start = offsets[startIdx - 1] ?? 0;
      const end = offsets[endIdx] ?? ta.value.length;

      ta.focus();
      try {
        ta.setSelectionRange(start, end);
      } catch (_) {
        // ignore
      }

      const approxLineHeight = 16;
      ta.scrollTop = Math.max(0, (startIdx - 3) * approxLineHeight);
    };

  ui.downloadTextFile =
    ui.downloadTextFile ||
    function downloadTextFile(filename, text, mimeType) {
      const blob = new Blob([String(text || "")], { type: mimeType || "text/plain" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      a.remove();
      window.setTimeout(() => URL.revokeObjectURL(url), 1000);
    };

  ui.openTextEditorModal =
    ui.openTextEditorModal ||
    function openTextEditorModal(options) {
      const titleText = String(options?.title || "Sửa mô tả");
      const placeholder = String(options?.placeholder || "Nhập mô tả...");
      const hintText = String(options?.hint || "");
      const actions = Array.isArray(options?.actions) && options.actions.length ? options.actions : [{ key: "cancel", label: "Đóng" }];

      const targets = Array.isArray(options?.targets) ? options.targets : [];
      const initialByKey = options?.initialByKey && typeof options.initialByKey === "object" ? options.initialByKey : {};
      const initialValue = String(options?.value ?? "");
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
        if (targets.length) {
          for (const t of targets) {
            const k = String(t?.key || "").trim();
            if (!k) continue;
            draftByKey.set(k, String(initialByKey[k] ?? ""));
          }
        }

        let select = null;
        if (targets.length > 1) {
          const row = document.createElement("div");
          row.style.display = "flex";
          row.style.gap = "8px";
          row.style.alignItems = "center";
          row.style.marginBottom = "8px";

          const label = document.createElement("div");
          label.textContent = String(options?.selectLabel || "Đối tượng:");
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
        textarea.value = targets.length ? (selectedKey ? draftByKey.get(selectedKey) || "" : "") : initialValue;
        textarea.placeholder = placeholder;
        dialog.appendChild(textarea);

        if (hintText) {
          const hint = document.createElement("div");
          hint.className = "demo-modal__hint";
          hint.textContent = hintText;
          dialog.appendChild(hint);
        }

        const actionsRow = document.createElement("div");
        actionsRow.className = "demo-modal__actions";
        dialog.appendChild(actionsRow);

        function cleanup() {
          window.removeEventListener("keydown", onKeyDown);
          overlay.remove();
        }

        function close(action) {
          if (targets.length && selectedKey) draftByKey.set(selectedKey, textarea.value);
          const value = targets.length ? (selectedKey ? draftByKey.get(selectedKey) || "" : textarea.value) : textarea.value;
          cleanup();
          resolve({ action, value, targetKey: selectedKey });
        }

        function onKeyDown(e) {
          if (e.key === "Escape") {
            e.preventDefault();
            close("cancel");
          }
        }

        for (const act of actions) {
          const key = String(act?.key || "").trim() || "action";
          const label = String(act?.label || key);
          const btn = document.createElement("button");
          btn.type = "button";
          btn.className = act?.className || "btn";
          btn.textContent = label;
          btn.addEventListener("click", () => close(key));
          actionsRow.appendChild(btn);
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
        window.addEventListener("keydown", onKeyDown);

        document.body.appendChild(overlay);
        textarea.focus();
        textarea.select();
      });
    };
})(window.AbapFlow);
