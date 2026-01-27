(function (ns) {
  "use strict";

  ns.ui = ns.ui || {};
  const ui = ns.ui;

  let currentMarkers = [];
  let rafPending = false;

  function getTextArea() {
    const ta = ui.$ ? ui.$("abapInput") : document.getElementById("abapInput");
    return ta instanceof HTMLTextAreaElement ? ta : null;
  }

  function getHost() {
    const host = ui.$ ? ui.$("abapTemplateMarkers") : document.getElementById("abapTemplateMarkers");
    return host instanceof HTMLElement ? host : null;
  }

  function schedulePositionUpdate() {
    if (rafPending) return;
    rafPending = true;
    requestAnimationFrame(() => {
      rafPending = false;
      updatePositions();
    });
  }

  function readLayoutMetrics(ta) {
    const style = window.getComputedStyle(ta);
    const lineHeight = Number.parseFloat(style.lineHeight) || 16;
    const paddingTop = Number.parseFloat(style.paddingTop) || 0;
    return { lineHeight, paddingTop };
  }

  function normalizeMarkers(markers) {
    const out = [];
    const seenLine = new Set();
    for (const m of Array.isArray(markers) ? markers : []) {
      const resultId = String(m?.resultId || "").trim();
      if (!resultId) continue;
      const startLine = Math.max(1, Math.floor(Number(m?.startLine || 0)));
      if (!Number.isFinite(startLine) || startLine <= 0) continue;
      const key = String(startLine);
      if (seenLine.has(key)) continue;
      seenLine.add(key);
      out.push({
        resultId,
        templateId: String(m?.templateId || "").trim(),
        objectId: String(m?.objectId || "").trim(),
        startLine,
        endLine: Math.max(startLine, Math.floor(Number(m?.endLine || startLine))),
      });
    }
    out.sort((a, b) => a.startLine - b.startLine);
    return out;
  }

  function clearMarkersUi() {
    currentMarkers = [];
    const host = getHost();
    if (!host) return;
    host.replaceChildren();
    host.classList.add("is-hidden");
  }

  function ensureWired() {
    const ta = getTextArea();
    if (!ta) return;
    if (ta.dataset.templateMarkersWired === "1") return;
    ta.dataset.templateMarkersWired = "1";

    ta.addEventListener("scroll", schedulePositionUpdate);
    window.addEventListener("resize", schedulePositionUpdate);

    ta.addEventListener("input", () => {
      // Avoid stale markers after code edits.
      clearMarkersUi();
    });
  }

  function renderMarkersUi(markers) {
    const ta = getTextArea();
    const host = getHost();
    if (!ta || !host) return;

    host.replaceChildren();

    currentMarkers = normalizeMarkers(markers);
    if (currentMarkers.length === 0) {
      host.classList.add("is-hidden");
      return;
    }

    host.classList.remove("is-hidden");

    for (const m of currentMarkers) {
      const btn = document.createElement("button");
      btn.type = "button";
      btn.className = "code-marker";
      btn.textContent = "T";
      btn.dataset.resultId = m.resultId;
      btn.dataset.startLine = String(m.startLine);
      btn.dataset.endLine = String(m.endLine);

      const label = m.objectId ? `${m.objectId}${m.templateId ? ` (${m.templateId})` : ""}` : m.templateId || "template";
      btn.title = `Đến kết quả mẫu - ${label} (L${m.startLine})`;

      btn.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();
        if (typeof ui.scrollToTemplateResultId === "function") {
          ui.scrollToTemplateResultId(m.resultId);
          return;
        }
        if (typeof ui.setActiveTab === "function") ui.setActiveTab("templates");
      });

      host.appendChild(btn);
    }

    schedulePositionUpdate();
  }

  function updatePositions() {
    const ta = getTextArea();
    const host = getHost();
    if (!ta || !host) return;
    if (host.classList.contains("is-hidden")) return;

    const { lineHeight, paddingTop } = readLayoutMetrics(ta);
    const scrollTop = ta.scrollTop;
    const markerSize = 14;
    const visibleTop = -lineHeight;
    const visibleBottom = ta.clientHeight + lineHeight;

    for (const btn of Array.from(host.querySelectorAll("button.code-marker"))) {
      const line = Math.max(1, Math.floor(Number(btn.dataset.startLine || 1)));
      const y = paddingTop + (line - 1) * lineHeight - scrollTop + Math.max(0, (lineHeight - markerSize) / 2);
      btn.style.top = `${Math.round(y)}px`;
      btn.style.display = y < visibleTop || y > visibleBottom ? "none" : "inline-flex";
    }
  }

  ui.renderAbapTemplateMarkers = function renderAbapTemplateMarkers(markers) {
    ensureWired();
    renderMarkersUi(markers);
  };

  ui.clearAbapTemplateMarkers = function clearAbapTemplateMarkers() {
    clearMarkersUi();
  };
})(window.AbapFlow);
