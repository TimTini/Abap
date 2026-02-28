"use strict";

(function (root) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);
  const registry = globalRoot.__AbapSourceParts || {};
  const targetKey = "viewer/app/04-output-render.js";
  const bucket = registry[targetKey] || {};
  const orderedParts = [
    "viewer/app/output/01-xml-export.js",
    "viewer/app/output/02-search-index.js",
    "viewer/app/output/03-selection-gutter.js",
    "viewer/app/output/04-render-shared.js",
    "viewer/app/output/05-render-values.js",
    "viewer/app/output/06-render-extras.js",
    "viewer/app/output/07-render-tree.js"
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
