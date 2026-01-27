(function (ns) {
  "use strict";

  ns.ui = ns.ui || {};
  const ui = ns.ui;

  let rafPending = false;
  let lastLineCount = 0;

  function getTextArea() {
    const ta = ui.$ ? ui.$("abapInput") : document.getElementById("abapInput");
    return ta instanceof HTMLTextAreaElement ? ta : null;
  }

  function getHost() {
    const host = ui.$ ? ui.$("abapLineNumbers") : document.getElementById("abapLineNumbers");
    return host instanceof HTMLElement ? host : null;
  }

  function countLines(text) {
    const s = String(text ?? "");
    if (!s) return 1;
    let n = 1;
    for (let i = 0; i < s.length; i++) {
      if (s.charCodeAt(i) === 10) n++;
    }
    return n;
  }

  function buildLineNumbersText(lineCount) {
    const n = Math.max(1, Math.floor(Number(lineCount || 1)));
    const lines = new Array(n);
    for (let i = 0; i < n; i++) lines[i] = String(i + 1);
    return lines.join("\n");
  }

  function applyMetrics(host, ta) {
    const style = window.getComputedStyle(ta);
    if (!style) return;

    if (style.lineHeight) host.style.lineHeight = style.lineHeight;
    if (style.fontFamily) host.style.fontFamily = style.fontFamily;
    if (style.fontSize) host.style.fontSize = style.fontSize;
    if (style.paddingTop) host.style.paddingTop = style.paddingTop;
    if (style.paddingBottom) host.style.paddingBottom = style.paddingBottom;
  }

  function updateNow() {
    const ta = getTextArea();
    const host = getHost();
    if (!ta || !host) return;

    applyMetrics(host, ta);

    const lineCount = countLines(ta.value);
    if (lineCount !== lastLineCount) {
      host.textContent = buildLineNumbersText(lineCount);
      lastLineCount = lineCount;
    }

    host.scrollTop = ta.scrollTop;
  }

  function scheduleUpdate() {
    if (rafPending) return;
    rafPending = true;
    requestAnimationFrame(() => {
      rafPending = false;
      updateNow();
    });
  }

  function ensureWired() {
    const ta = getTextArea();
    if (!ta) return;
    if (ta.dataset.abapLineNumbersWired === "1") return;
    ta.dataset.abapLineNumbersWired = "1";

    ta.addEventListener("scroll", scheduleUpdate);
    ta.addEventListener("input", scheduleUpdate);
    window.addEventListener("resize", scheduleUpdate);

    scheduleUpdate();
  }

  ui.updateAbapLineNumbers = function updateAbapLineNumbers() {
    ensureWired();
    scheduleUpdate();
  };

  ensureWired();
})(window.AbapFlow);

