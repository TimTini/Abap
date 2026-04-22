"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..");

const runtimeBundles = [
  "shared/abap-parser.js",
  "viewer/app/01-core.js",
  "viewer/app/02-descriptions.js",
  "viewer/app/03-template-preview.js",
  "viewer/app/04-output-render.js"
];

const forbiddenPatterns = [
  { pattern: /\b__AbapSourceParts\b/, message: "should not depend on runtime source registries" },
  { pattern: /\beval\s*\(/, message: "should not use eval" },
  { pattern: /createElement\(\s*["']script["']\s*\)/, message: "should not inject runtime scripts" },
  { pattern: /script\.textContent\s*=/, message: "should not build executable scripts from textContent" }
];

function readRepoFile(relPath) {
  return fs.readFileSync(path.resolve(repoRoot, relPath), "utf8");
}

for (const relPath of runtimeBundles) {
  const text = readRepoFile(relPath);
  for (const rule of forbiddenPatterns) {
    assert(
      !rule.pattern.test(text),
      `${relPath} ${rule.message}.`
    );
  }
}

const indexHtml = readRepoFile("viewer/index.html");

assert(
  !/shared\/abap-parser\/0\d-[^"]+\.js/.test(indexHtml),
  "viewer/index.html should load the parser bundle, not individual parser source parts."
);

assert(
  !/app\/(?:core|descriptions|template|output)\/0\d-[^"]+\.js/.test(indexHtml),
  "viewer/index.html should load viewer runtime bundles, not individual source parts."
);

assert(
  /<script src="\.\.\/shared\/abap-parser\.js" defer><\/script>/.test(indexHtml),
  "viewer/index.html must load the parser runtime bundle."
);

for (const relPath of [
  "./app/01-core.js",
  "./app/02-descriptions.js",
  "./app/03-template-preview.js",
  "./app/04-output-render.js"
]) {
  assert(
    indexHtml.includes(`<script src="${relPath}" defer></script>`),
    `viewer/index.html must load ${relPath}.`
  );
}

console.log("Runtime bundle contracts passed.");
