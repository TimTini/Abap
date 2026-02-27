"use strict";

(function (root, factory) {
  if (typeof module === "object" && module.exports) {
    module.exports = factory(root, true);
    return;
  }

  root.AbapParser = factory(root, false);
})(typeof globalThis !== "undefined" ? globalThis : (typeof self !== "undefined" ? self : this), function (root, isNode) {
  const globalRoot = root || (typeof globalThis !== "undefined" ? globalThis : this);

  if (isNode && typeof require === "function") {
    const requireParts = [
        "./abap-parser/01-context.js",
        "./abap-parser/02-config.js",
        "./abap-parser/03-statements.js",
        "./abap-parser/04-parse-core.js",
        "./abap-parser/05-extras.js",
        "./abap-parser/06-conditions.js",
        "./abap-parser/07-declarations.js",
        "./abap-parser/08-helpers.js",
        "./abap-parser/09-public-api.js"
    ];
    for (const relPath of requireParts) {
      require(relPath);
    }
  }

  const registry = globalRoot.__AbapSourceParts || {};
  const targetKey = "shared/abap-parser.js";
  const bucket = registry[targetKey] || {};
  const orderedParts = [
      "shared/abap-parser/01-context.js",
      "shared/abap-parser/02-config.js",
      "shared/abap-parser/03-statements.js",
      "shared/abap-parser/04-parse-core.js",
      "shared/abap-parser/05-extras.js",
      "shared/abap-parser/06-conditions.js",
      "shared/abap-parser/07-declarations.js",
      "shared/abap-parser/08-helpers.js",
      "shared/abap-parser/09-public-api.js"
  ];

  const missing = orderedParts.filter((part) => typeof bucket[part] !== "string");
  if (missing.length) {
    throw new Error(`Parser source parts missing for ${targetKey}: ${missing.join(", ")}`);
  }

  const source = orderedParts.map((part) => bucket[part]).join("\n");
  (0, eval)(source);

  const api = globalRoot.AbapParser;
  if (!api || typeof api.parseAbapText !== "function") {
    throw new Error("Failed to initialize AbapParser from split parts.");
  }
  return api;
});
