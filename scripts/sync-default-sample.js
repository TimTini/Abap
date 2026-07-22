"use strict";

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..");
const runtimePath = path.join(repoRoot, "viewer", "app", "core", "01-runtime-state.js");
const samplePath = path.join(repoRoot, "examples", "deep_form_demo.abap");
const endMarker = '].join("\\n");';

function normalizeLf(text) {
  return String(text).replace(/\r\n?/g, "\n");
}

function buildSampleSnippet(sampleText) {
  const lines = normalizeLf(sampleText).replace(/^\uFEFF/, "").split("\n");
  if (lines.length && lines[lines.length - 1] === "") {
    lines.pop();
  }
  return [
    "const SAMPLE_ABAP = [",
    ...lines.map((line, index) => `    ${JSON.stringify(line)}${index === lines.length - 1 ? "" : ","}`),
    endMarker
  ].join("\n");
}

function buildNextRuntimeText(runtimeText, sampleText) {
  const start = runtimeText.indexOf("const SAMPLE_ABAP = [");
  if (start < 0) {
    throw new Error("SAMPLE_ABAP start not found in viewer/app/core/01-runtime-state.js");
  }
  const end = runtimeText.indexOf(endMarker, start);
  if (end < 0) {
    throw new Error("SAMPLE_ABAP end marker not found in viewer/app/core/01-runtime-state.js");
  }
  return runtimeText.slice(0, start)
    + buildSampleSnippet(sampleText)
    + runtimeText.slice(end + endMarker.length);
}

function writeIfChanged(filePath, nextText, options) {
  const currentText = fs.existsSync(filePath) ? fs.readFileSync(filePath, "utf8") : "";
  if (currentText === nextText) {
    return { changed: false, stale: false };
  }
  if (options.checkOnly) {
    return { changed: false, stale: true };
  }
  fs.writeFileSync(filePath, nextText, "utf8");
  return { changed: true, stale: true };
}

function parseCliArgs(argv) {
  const args = new Set(argv);
  const allowed = new Set(["--check"]);
  for (const arg of args) {
    if (!allowed.has(arg)) {
      throw new Error(`Unsupported argument: ${arg}`);
    }
  }
  return { checkOnly: args.has("--check") };
}

function main() {
  const options = parseCliArgs(process.argv.slice(2));
  const runtimeText = fs.readFileSync(runtimePath, "utf8");
  const sampleText = fs.readFileSync(samplePath, "utf8");
  const nextRuntimeText = buildNextRuntimeText(runtimeText, sampleText);
  const result = writeIfChanged(runtimePath, nextRuntimeText, options);

  if (options.checkOnly) {
    if (result.stale) {
      console.error("Default viewer sample is stale. Run node scripts/sync-default-sample.js.");
      process.exit(1);
    }
    console.log("Default viewer sample is up to date.");
    return;
  }

  if (result.changed) {
    console.log("Synced SAMPLE_ABAP from examples/deep_form_demo.abap");
    return;
  }

  console.log("Default viewer sample already up to date.");
}

if (require.main === module) {
  try {
    main();
  } catch (error) {
    console.error(error && error.message ? error.message : error);
    process.exit(1);
  }
}

module.exports = {
  buildNextRuntimeText,
  buildSampleSnippet,
  normalizeLf
};
