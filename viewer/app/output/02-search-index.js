"use strict";

(function (root) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);
  const registry = globalRoot.__AbapSourceParts = globalRoot.__AbapSourceParts || {};
  const targetKey = "viewer/app/04-output-render.js";
  const partKey = "viewer/app/output/02-search-index.js";
  const bucket = registry[targetKey] = registry[targetKey] || {};
  bucket[partKey] = "";
})(typeof globalThis !== "undefined" ? globalThis : (typeof self !== "undefined" ? self : this));
