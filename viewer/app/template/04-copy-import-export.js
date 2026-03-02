"use strict";

(function (root) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);
  const registry = globalRoot.__AbapSourceParts = globalRoot.__AbapSourceParts || {};
  const targetKey = "viewer/app/03-template-preview.js";
  const partKey = "viewer/app/template/04-copy-import-export.js";
  const bucket = registry[targetKey] = registry[targetKey] || {};
  bucket[partKey] = "";
})(typeof globalThis !== "undefined" ? globalThis : (typeof self !== "undefined" ? self : this));
