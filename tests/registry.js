"use strict";

const path = require("path");

const entries = [
  { file: "tests/parser-sap-coverage.test.js", suites: ["parser", "fast", "full"], labels: ["sap-coverage"] },
  { file: "tests/parser-regression.statements.test.js", suites: ["parser", "fast", "full"], labels: ["statements"] },
  { file: "tests/parser-regression.conditions.test.js", suites: ["parser", "fast", "full"], labels: ["conditions"] },
  { file: "tests/parser-regression.model.test.js", suites: ["parser", "fast", "full"], labels: ["model"] },
  { file: "tests/parser-regression.demo.test.js", suites: ["parser", "full"], labels: ["demo"] },
  { file: "tests/parser-grammar-inventory.test.js", suites: ["parser", "fast", "full"], labels: ["grammar-inventory"] },
  { file: "tests/parser-lexer.test.js", suites: ["parser", "fast", "full"], labels: [] },
  { file: "tests/parser-grammar.test.js", suites: ["parser", "fast", "full"], labels: [] },
  { file: "tests/parser-contracts.test.js", suites: ["parser", "contracts", "fast", "full"], labels: ["parser-contracts"] },
  { file: "tests/runtime-contracts.loader.test.js", suites: ["runtime", "contracts", "fast", "full"], labels: ["loader"] },
  { file: "tests/runtime-contracts.offline.test.js", suites: ["runtime", "contracts", "fast", "full"], labels: ["offline"] },
  { file: "tests/viewer-contracts.fixtures.test.js", suites: ["viewer", "contracts", "fast", "full"], labels: ["fixtures", "render-error", "template-import-error"] },
  { file: "tests/viewer-contracts.config.test.js", suites: ["viewer", "contracts", "fast", "full"], labels: ["config-export", "template-reset", "template-configs"] },
  { file: "tests/viewer-contracts.perform.test.js", suites: ["viewer", "contracts", "fast", "full"], labels: ["if-template", "perform-root-trace", "perform-source-selection", "perform-source-picker", "template-header-compact", "template-multi-value-perform-call", "template-multi-value-perform-root", "data-perform-trace"] },
  { file: "tests/viewer-contracts.template.test.js", suites: ["viewer", "contracts", "fast", "full"], labels: ["struct-field-finaldesc", "constant-finaldesc", "template-multi-value-conditions", "template-multi-value-safe-lists", "template-row-description-loop", "template-row-description-perform", "template-row-description-condition", "template-provenance", "append-variants", "template-multi-select", "message-write", "template-excel-roundtrip"] },
  { file: "tests/viewer-contracts.data.test.js", suites: ["viewer", "contracts", "fast", "full"], labels: ["data-catalog", "output-removal"] },
  { file: "tests/viewer-contracts.sap-coverage.test.js", suites: ["viewer", "contracts", "fast", "full"], labels: ["sap-viewer-coverage"] },
  { file: "tests/viewer-contracts.navigation.test.js", suites: ["viewer", "contracts", "full"], labels: ["scroll-navigation", "scroll-manual-takeover", "scroll-range-coverage", "scroll-geometry"] },
  { file: "tests/run.contracts.test.js", suites: ["contracts", "fast", "full"], labels: ["runner"] }
];

const suiteNames = ["parser", "runtime", "viewer", "contracts", "fast", "full"];

function getEntriesForSuite(suite) {
  return entries.filter((entry) => entry.suites.includes(suite));
}

function resolveFocus(entriesForSuite, requestedFocus) {
  const normalizedFocus = String(requestedFocus || "").trim();
  if (!normalizedFocus) {
    return {
      matches: entriesForSuite,
      resolvedFocus: ""
    };
  }

  const exactMatches = entriesForSuite.filter((entry) => entry.labels.includes(normalizedFocus));
  if (exactMatches.length === 1) {
    return { matches: exactMatches, resolvedFocus: normalizedFocus };
  }
  if (exactMatches.length > 1) {
    return { error: `Ambiguous focus "${normalizedFocus}" matched multiple files.` };
  }
  return { error: `Unknown focus "${normalizedFocus}" for suite.` };
}

function toAbsoluteFile(entry) {
  return path.resolve(__dirname, "..", entry.file);
}

module.exports = {
  entries,
  getEntriesForSuite,
  resolveFocus,
  suiteNames,
  toAbsoluteFile
};
