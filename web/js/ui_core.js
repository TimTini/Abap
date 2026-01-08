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
})(window.AbapFlow);

