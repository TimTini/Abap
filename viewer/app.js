"use strict";

(function startAbapViewerApp(global) {
  const runtime = global.AbapViewerRuntime = global.AbapViewerRuntime || {};
  runtime.api = runtime.api || {};
  const requiredServices = ["runtimeState", "output", "descriptions", "performSources", "template", "uiNavigation", "parserController", "bootstrap"];
  const missing = requiredServices.filter((name) => !runtime.services || !runtime.services[name]);
  if (missing.length) {
    const message = "Viewer services missing: " + missing.join(", ") + ". Check script order in viewer/index.html.";
    const errorEl = document.getElementById("error");
    if (errorEl) { errorEl.textContent = message; }
    try { console.error(message); } catch {}
    return;
  }
  const bootstrap = runtime.services.bootstrap;
  if (!bootstrap || typeof bootstrap.start !== "function") {
    const message = "Viewer bootstrap.start not found.";
    const errorEl = document.getElementById("error");
    if (errorEl) { errorEl.textContent = message; }
    try { console.error(message); } catch {}
    return;
  }
  bootstrap.start();
})(window);
