"use strict";

/**
 * Sync demo sample text from examples/deep_form_demo.abap
 * (same SSOT as Viewer SAMPLE_ABAP via scripts/sync-default-sample.js).
 *
 * Usage:
 *   node scripts/sync-data-source-chain-map-sample.js
 *   node scripts/sync-data-source-chain-map-sample.js --check
 */

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..");
const samplePath = path.join(repoRoot, "examples", "deep_form_demo.abap");
const outPath = path.join(repoRoot, "demo", "data-source-chain-map.sample.js");

function normalizeLf(text) {
  return String(text).replace(/^\uFEFF/, "").replace(/\r\n?/g, "\n");
}

function buildOutput(sampleText) {
  return [
    "/**",
    " * Auto-generated from examples/deep_form_demo.abap (same SSOT as Viewer SAMPLE_ABAP).",
    " * Do not edit by hand. Run: node scripts/sync-data-source-chain-map-sample.js",
    " */",
    "(function (global) {",
    "  \"use strict\";",
    "  global.DataSourceChainMapSample = {",
    "    sourceRef: \"examples/deep_form_demo.abap\",",
    "    sourceText: " + JSON.stringify(sampleText),
    "  };",
    "})(typeof window !== \"undefined\" ? window : globalThis);",
    ""
  ].join("\n");
}

function parseCliArgs(argv) {
  const args = new Set(argv);
  for (const arg of args) {
    if (arg !== "--check") {
      throw new Error("Unsupported argument: " + arg);
    }
  }
  return { checkOnly: args.has("--check") };
}

function main() {
  const options = parseCliArgs(process.argv.slice(2));
  const sampleText = normalizeLf(fs.readFileSync(samplePath, "utf8"));
  const next = buildOutput(sampleText);
  const current = fs.existsSync(outPath) ? fs.readFileSync(outPath, "utf8") : "";

  if (options.checkOnly) {
    if (current !== next) {
      console.error("Chain-map sample is stale. Run: node scripts/sync-data-source-chain-map-sample.js");
      process.exit(1);
    }
    console.log("Chain-map sample is up to date.");
    return;
  }

  if (current === next) {
    console.log("Chain-map sample already up to date.");
    return;
  }

  fs.writeFileSync(outPath, next, "utf8");
  console.log("Synced demo/data-source-chain-map.sample.js from examples/deep_form_demo.abap");
}

if (require.main === module) {
  try {
    main();
  } catch (err) {
    console.error(String(err && err.stack ? err.stack : err));
    process.exit(1);
  }
}
