"use strict";

(function () {
  window.AbapViewerModules = window.AbapViewerModules || {};
  window.AbapViewerModules.parts = window.AbapViewerModules.parts || {};
  window.AbapViewerRuntime = window.AbapViewerRuntime || {};
  window.AbapViewerRuntime.api = window.AbapViewerRuntime.api || {};

  const requiredParts = [
    "01-core",
    "02-descriptions",
    "03-template-preview",
    "04-output-render",
    "05-main"
  ];

  const missing = requiredParts.filter((name) => !window.AbapViewerModules.parts[name]);
  if (missing.length) {
    const message = `Viewer modules missing: ${missing.join(", ")}. Check script order in viewer/index.html.`;
    const errorEl = document.getElementById("error");
    if (errorEl) {
      errorEl.textContent = message;
    }
    try {
      console.error(message);
    } catch {
      // ignore
    }
    return;
  }

  const runtime = window.AbapViewerRuntime;
  runtime.els = runtime.els || null;
  runtime.state = runtime.state || null;
  runtime.constants = runtime.constants || {};
  runtime.api = runtime.api || {};

  if (typeof window.AbapViewerModules.start !== "function") {
    const message = "Viewer start function not found (window.AbapViewerModules.start).";
    const errorEl = document.getElementById("error");
    if (errorEl) {
      errorEl.textContent = message;
    }
    try {
      console.error(message);
    } catch {
      // ignore
    }
    return;
  }

  window.AbapViewerModules.start(runtime);
})();
