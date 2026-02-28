"use strict";

(function (root) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);
  const registry = globalRoot.__AbapSourceParts || {};
  const targetKey = "viewer/app/01-core.js";
  const bucket = registry[targetKey] || {};
  const orderedParts = [
    "viewer/app/core/01-runtime-state.js",
    "viewer/app/core/02-build-info.js",
    "viewer/app/core/03-storage.js",
    "viewer/app/core/04-template-config.js",
    "viewer/app/core/05-theme-layout.js",
    "viewer/app/core/06-rules.js",
    "viewer/app/core/07-settings-modal.js"
  ];

  const missing = orderedParts.filter((part) => typeof bucket[part] !== "string");
  if (missing.length) {
    const message = `Source parts missing for ${targetKey}: ${missing.join(", ")}`;
    try {
      if (globalRoot.console && typeof globalRoot.console.error === "function") {
        globalRoot.console.error(message);
      }
    } catch {
      // ignore
    }
    return;
  }

  const source = orderedParts.map((part) => bucket[part]).join("\n");
  if (typeof document !== "undefined" && document.createElement) {
    const script = document.createElement("script");
    script.type = "text/javascript";
    script.setAttribute("data-abap-source-wrapper", targetKey);
    script.textContent = `${source}\n//# sourceURL=${targetKey}`;
    (document.head || document.documentElement || document.body).appendChild(script);
    script.remove();
  } else {
    (0, eval)(source);
  }
})(typeof globalThis !== "undefined" ? globalThis : (typeof self !== "undefined" ? self : this));
