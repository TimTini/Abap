"use strict";

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..");
const runtimePath = path.join(repoRoot, "viewer", "app", "core", "01-runtime-state.js");
const samplePath = path.join(repoRoot, "examples", "deep_form_demo.abap");

const runtimeText = fs.readFileSync(runtimePath, "utf8");
const sampleText = fs.readFileSync(samplePath, "utf8").replace(/^\uFEFF/, "");

const start = runtimeText.indexOf("const SAMPLE_ABAP = [");
if (start < 0) {
  throw new Error("SAMPLE_ABAP start not found in viewer/app/core/01-runtime-state.js");
}

const endMarker = '].join(\\"\\\\n\\");';
const end = runtimeText.indexOf(endMarker, start);
if (end < 0) {
  throw new Error("SAMPLE_ABAP end marker not found in viewer/app/core/01-runtime-state.js");
}

const lines = sampleText.split(/\r?\n/);
if (lines.length && lines[lines.length - 1] === "") {
  lines.pop();
}

const innerSnippet = [
  "const SAMPLE_ABAP = [",
  ...lines.map((line, index) => `    ${JSON.stringify(line)}${index === lines.length - 1 ? "" : ","}`),
  '  ].join("\\n");'
].join("\n");

const embeddedSnippet = innerSnippet
  .replace(/\\/g, "\\\\")
  .replace(/"/g, '\\"')
  .replace(/\r?\n/g, "\\n");

const nextRuntimeText =
  runtimeText.slice(0, start) +
  embeddedSnippet +
  runtimeText.slice(end + endMarker.length);

fs.writeFileSync(runtimePath, nextRuntimeText, "utf8");
console.log("Synced SAMPLE_ABAP from examples/deep_form_demo.abap");
