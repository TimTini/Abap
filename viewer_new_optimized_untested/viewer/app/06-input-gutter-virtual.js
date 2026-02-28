"use strict";

(function () {
  const runtime = window.AbapViewerRuntime || {};
  const state = runtime.state;
  const els = runtime.els;

  if (!state || !els || !els.inputText || !els.inputGutterContent) {
    return;
  }

  const INPUT_GUTTER_LINE_HEIGHT = 18;
  const INPUT_GUTTER_OVERSCAN = 12;

  function ensureInputGutterVirtualState() {
    const current = state.inputGutterVirtual;
    if (current && typeof current === "object") {
      return current;
    }

    const next = {
      lineHeight: INPUT_GUTTER_LINE_HEIGHT,
      overscan: INPUT_GUTTER_OVERSCAN,
      lineCount: 0,
      startLine: 0,
      endLine: 0,
      pendingRaf: 0,
      canvas: null
    };
    state.inputGutterVirtual = next;
    return next;
  }

  function computeInputLineCount(text) {
    const source = String(text || "");
    if (!source) {
      return 1;
    }
    return source.split("\n").length;
  }

  function ensureInputGutterVirtualCanvas(virtualState) {
    if (!els.inputGutterContent) {
      return null;
    }

    const currentCanvas = virtualState && virtualState.canvas;
    if (currentCanvas && currentCanvas.parentNode === els.inputGutterContent) {
      return currentCanvas;
    }

    const canvas = document.createElement("div");
    canvas.style.position = "relative";
    canvas.style.width = "100%";
    els.inputGutterContent.replaceChildren(canvas);
    virtualState.canvas = canvas;
    return canvas;
  }

  function renderInputGutterVirtualWindow(force) {
    if (!els.inputText || !els.inputGutter || !els.inputGutterContent) {
      return;
    }

    const virtualState = ensureInputGutterVirtualState();
    const canvas = ensureInputGutterVirtualCanvas(virtualState);
    if (!canvas) {
      return;
    }

    const lineCount = Math.max(1, Number(state.inputLineCount) || 1);
    const scrollTop = Number(els.inputText.scrollTop || 0) || 0;
    const viewportHeight = Math.max(
      1,
      Number(els.inputText.clientHeight || 0) || Number(els.inputGutter.clientHeight || 0) || 1
    );
    const lineHeight = Math.max(1, Number(virtualState.lineHeight) || INPUT_GUTTER_LINE_HEIGHT);
    const visibleCount = Math.max(1, Math.ceil(viewportHeight / lineHeight));
    const overscan = Math.max(virtualState.overscan || 0, Math.ceil(visibleCount * 0.75));

    let startLine = Math.floor(scrollTop / lineHeight) + 1 - overscan;
    startLine = Math.max(1, Math.min(lineCount, startLine));

    let endLine = Math.min(lineCount, startLine + visibleCount + (overscan * 2) - 1);
    startLine = Math.max(1, Math.min(startLine, endLine));

    const unchanged = (
      !force
      && lineCount === virtualState.lineCount
      && startLine === virtualState.startLine
      && endLine === virtualState.endLine
    );

    canvas.style.height = `${lineCount * lineHeight}px`;
    if (unchanged) {
      return;
    }

    const fragment = document.createDocumentFragment();
    const visibleButtonsByLine = new Map();

    for (let line = startLine; line <= endLine; line += 1) {
      const row = document.createElement("div");
      row.className = "gutter-line";
      row.style.position = "absolute";
      row.style.left = "0";
      row.style.right = "0";
      row.style.top = `${(line - 1) * lineHeight}px`;

      const jumpBtn = document.createElement("button");
      jumpBtn.type = "button";
      jumpBtn.className = "gutter-jump";
      jumpBtn.textContent = "↪";
      jumpBtn.hidden = true;
      jumpBtn.setAttribute("data-line", String(line));
      row.appendChild(jumpBtn);

      const num = document.createElement("span");
      num.className = "gutter-num";
      num.textContent = String(line);
      row.appendChild(num);

      visibleButtonsByLine.set(line, jumpBtn);
      fragment.appendChild(row);
    }

    canvas.replaceChildren(fragment);
    state.inputGutterButtonsByLine = visibleButtonsByLine;
    virtualState.lineCount = lineCount;
    virtualState.startLine = startLine;
    virtualState.endLine = endLine;

    if (typeof refreshInputGutterTargets === "function") {
      refreshInputGutterTargets();
    }
  }

  function scheduleInputGutterVirtualRender(force) {
    const virtualState = ensureInputGutterVirtualState();
    if (force) {
      if (virtualState.pendingRaf) {
        const cancel = typeof window.cancelAnimationFrame === "function"
          ? window.cancelAnimationFrame.bind(window)
          : clearTimeout;
        cancel(virtualState.pendingRaf);
        virtualState.pendingRaf = 0;
      }
      renderInputGutterVirtualWindow(true);
      return;
    }

    if (virtualState.pendingRaf) {
      return;
    }

    const schedule = typeof window.requestAnimationFrame === "function"
      ? window.requestAnimationFrame.bind(window)
      : (cb) => setTimeout(cb, 16);

    virtualState.pendingRaf = schedule(() => {
      virtualState.pendingRaf = 0;
      renderInputGutterVirtualWindow(false);
    });
  }

  if (typeof rebuildInputGutter === "function") {
    // Override with virtualized gutter rendering to avoid full-line DOM.
    rebuildInputGutter = function rebuildInputGutterVirtualized() {
      if (!els.inputText || !els.inputGutterContent) {
        return;
      }

      const text = String(els.inputText.value || "");
      const trimmed = text.trim();
      const isJsonLike = (trimmed.startsWith("{") || trimmed.startsWith("[")) && trimmed.length > 1;
      state.inputMode = isJsonLike ? "json" : "abap";

      state.inputLineCount = Math.max(1, computeInputLineCount(text));
      state.inputGutterButtonsByLine = new Map();

      const virtualState = ensureInputGutterVirtualState();
      virtualState.lineCount = state.inputLineCount;
      virtualState.startLine = 0;
      virtualState.endLine = 0;

      const canvas = ensureInputGutterVirtualCanvas(virtualState);
      if (canvas) {
        canvas.style.height = `${state.inputLineCount * virtualState.lineHeight}px`;
      }

      syncInputGutterScroll();
      scheduleInputGutterVirtualRender(true);
    };
  }

  if (typeof syncInputGutterScroll === "function") {
    syncInputGutterScroll = function syncInputGutterScrollVirtualized() {
      if (!els.inputText || !els.inputGutterContent) {
        return;
      }
      const scrollTop = Number(els.inputText.scrollTop || 0) || 0;
      els.inputGutterContent.style.transform = `translateY(${-scrollTop}px)`;
      scheduleInputGutterVirtualRender(false);
    };
  }

  window.addEventListener("resize", () => {
    scheduleInputGutterVirtualRender(true);
  });
})();
