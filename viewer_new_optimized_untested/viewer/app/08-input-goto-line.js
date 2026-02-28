"use strict";

(function () {
  const inputText = document.getElementById("inputText");
  const gotoInput = document.getElementById("inputGotoLineInput");
  const gotoBtn = document.getElementById("inputGotoLineBtn");
  const gotoMeta = document.getElementById("inputGotoLineMeta");

  if (!inputText || !gotoInput || !gotoBtn || !gotoMeta) {
    return;
  }

  function countLines(text) {
    const source = String(text || "");
    if (!source) {
      return 1;
    }
    return source.split("\n").length;
  }

  function clampLine(line, total) {
    const numeric = Number(line);
    if (!Number.isFinite(numeric)) {
      return 1;
    }
    const value = Math.floor(numeric);
    return Math.max(1, Math.min(total, value));
  }

  function computeLineOffsets(text) {
    const source = String(text || "");
    const offsets = [0];
    for (let i = 0; i < source.length; i += 1) {
      if (source.charCodeAt(i) === 10) {
        offsets.push(i + 1);
      }
    }
    return offsets;
  }

  function selectLineByNumber(lineNumber) {
    const text = String(inputText.value || "");
    const total = countLines(text);
    const line = clampLine(lineNumber, total);

    const offsets = computeLineOffsets(text);
    const start = offsets[line - 1] || 0;

    inputText.focus();
    // Keep caret at the target line start; avoid full-line selection auto-jumps.
    inputText.setSelectionRange(start, start);
    inputText.scrollLeft = 0;

    const style = window.getComputedStyle(inputText);
    const lineHeightPx = Number.parseFloat(style.lineHeight || "18") || 18;
    const targetTop = Math.max(0, Math.round((line - 1) * lineHeightPx - (lineHeightPx * 2)));
    inputText.scrollTop = targetTop;
    if (typeof syncInputGutterScroll === "function") {
      syncInputGutterScroll();
    }

    gotoInput.value = String(line);
    updateGotoLineMeta();
  }

  function getCaretLine() {
    const position = Number(inputText.selectionStart || 0);
    const source = String(inputText.value || "");
    if (!source) {
      return 1;
    }
    let line = 1;
    for (let i = 0; i < position && i < source.length; i += 1) {
      if (source.charCodeAt(i) === 10) {
        line += 1;
      }
    }
    return line;
  }

  function updateGotoLineMeta() {
    const total = countLines(inputText.value || "");
    gotoInput.max = String(total);

    const raw = String(gotoInput.value || "").trim();
    if (!raw) {
      gotoMeta.textContent = `1-${total}`;
      return;
    }

    const line = clampLine(raw, total);
    gotoMeta.textContent = `${line}/${total}`;
  }

  function runGotoLine() {
    const raw = String(gotoInput.value || "").trim();
    if (!raw) {
      return;
    }
    const line = Number(raw);
    if (!Number.isFinite(line)) {
      return;
    }
    selectLineByNumber(line);
  }

  function syncGotoLineFromCaret() {
    const line = getCaretLine();
    gotoInput.value = String(line);
    updateGotoLineMeta();
  }

  gotoBtn.addEventListener("click", runGotoLine);

  gotoInput.addEventListener("keydown", (ev) => {
    if (ev.key === "Enter") {
      ev.preventDefault();
      runGotoLine();
    }
  });

  gotoInput.addEventListener("input", updateGotoLineMeta);
  inputText.addEventListener("input", updateGotoLineMeta);
  inputText.addEventListener("click", syncGotoLineFromCaret);
  inputText.addEventListener("keyup", syncGotoLineFromCaret);

  window.addEventListener("keydown", (ev) => {
    const key = String(ev.key || "").toLowerCase();
    if (!ev.ctrlKey || key !== "g") {
      return;
    }
    ev.preventDefault();
    gotoInput.focus();
    gotoInput.select();
  });

  if (typeof parseFromTextarea === "function") {
    const originalParseFromTextarea = parseFromTextarea;
    parseFromTextarea = function parseFromTextareaWithGotoLineSync() {
      const result = originalParseFromTextarea.apply(this, arguments);
      updateGotoLineMeta();
      return result;
    };
  }

  updateGotoLineMeta();
})();
