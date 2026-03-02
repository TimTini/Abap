"use strict";

(function (root) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);
  const registry = globalRoot.__AbapSourceParts = globalRoot.__AbapSourceParts || {};
  const targetKey = "viewer/app/01-core.js";
  const partKey = "viewer/app/core/02-build-info.js";
  const bucket = registry[targetKey] = registry[targetKey] || {};
  bucket[partKey] = "";
})(typeof globalThis !== "undefined" ? globalThis : (typeof self !== "undefined" ? self : this));
