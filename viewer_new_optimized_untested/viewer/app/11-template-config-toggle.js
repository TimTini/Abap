"use strict";

(function () {
  const STORAGE_KEY = "abap-parser-viewer.templateConfigEditorOpen.v1";
  const btn = document.getElementById("templateConfigToggleBtn");
  const wrap = document.getElementById("templateConfigWrap");

  if (!btn || !wrap) {
    return;
  }

  function loadOpenState() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      return raw === "1" || raw === "true";
    } catch {
      return false;
    }
  }

  function saveOpenState(value) {
    try {
      localStorage.setItem(STORAGE_KEY, value ? "1" : "0");
    } catch {
      // ignore
    }
  }

  function applyOpenState(open) {
    const isOpen = Boolean(open);
    wrap.hidden = !isOpen;
    btn.textContent = isOpen ? "Hide JSON editor" : "Show JSON editor";
    btn.setAttribute("aria-expanded", String(isOpen));
  }

  let isOpen = loadOpenState();
  applyOpenState(isOpen);

  btn.addEventListener("click", () => {
    isOpen = !isOpen;
    applyOpenState(isOpen);
    saveOpenState(isOpen);
  });
})();

