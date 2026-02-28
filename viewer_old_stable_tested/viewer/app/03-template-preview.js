"use strict";

(function (root) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);
  const registry = globalRoot.__AbapSourceParts || {};
  const targetKey = "viewer/app/03-template-preview.js";
  const bucket = registry[targetKey] || {};
  const orderedParts = [
    "viewer/app/template/01-path-resolver.js",
    "viewer/app/template/02-grid-and-style.js",
    "viewer/app/template/03-preview-render.js",
    "viewer/app/template/04-copy-import-export.js"
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
