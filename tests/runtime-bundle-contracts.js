"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const {
  CONFIG_BUNDLE_REL_PATH,
  LEGACY_CONFIG_DIR_REL_PATH,
  buildConfigBundleSource,
  loadConfigsFromDisk,
  replaceConfigScriptBlock
} = require("../scripts/build-viewer-configs.js");

const repoRoot = path.resolve(__dirname, "..");
const configBundleRelPath = CONFIG_BUNDLE_REL_PATH;
const legacyConfigDirPath = path.resolve(repoRoot, LEGACY_CONFIG_DIR_REL_PATH);

const runtimeBundles = [
  "shared/abap-parser.js",
  configBundleRelPath,
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

function collectRemoteAssetUrls(html) {
  const remoteRefs = [];
  const assetPattern = /\b(?:src|href)\s*=\s*["']([^"']+)["']/gi;
  let match = assetPattern.exec(html);
  while (match) {
    const url = match[1].trim();
    if (/^(?:https?:)?\/\//i.test(url)) {
      remoteRefs.push(url);
    }
    match = assetPattern.exec(html);
  }

  const cssUrlPattern = /\burl\(\s*(["']?)([^"')]+)\1\s*\)/gi;
  match = cssUrlPattern.exec(html);
  while (match) {
    const url = match[2].trim();
    if (/^(?:https?:)?\/\//i.test(url)) {
      remoteRefs.push(url);
    }
    match = cssUrlPattern.exec(html);
  }
  return remoteRefs;
}

function assertNoRemoteUrls(html, label) {
  const remoteRefs = collectRemoteAssetUrls(html);
  assert.deepStrictEqual(remoteRefs, [], `${label} must stay offline-only. Found remote refs: ${remoteRefs.join(", ")}`);
}

for (const relPath of runtimeBundles) {
  assert(fs.existsSync(path.resolve(repoRoot, relPath)), `Missing runtime bundle: ${relPath}`);
  const text = readRepoFile(relPath);
  for (const rule of forbiddenPatterns) {
    assert(!rule.pattern.test(text), `${relPath} ${rule.message}.`);
  }
}

assert(!fs.existsSync(legacyConfigDirPath), "Legacy viewer/configs.generated directory should be removed after consolidating config bundle.");

const indexHtml = readRepoFile("viewer/index.html");
assert.throws(
  () => assertNoRemoteUrls('<img src="https://example.invalid/pixel.png">', "synthetic HTML"),
  /offline-only/,
  "Offline guard must reject remote src/href attributes on any element."
);
assert.throws(
  () => assertNoRemoteUrls('<style>.x{background:url(//example.invalid/x.png)}</style>', "synthetic CSS"),
  /offline-only/,
  "Offline guard must reject remote CSS url() references."
);
assertNoRemoteUrls(indexHtml, "viewer/index.html");
assert.strictEqual(
  indexHtml,
  replaceConfigScriptBlock(indexHtml),
  "viewer/index.html is stale. Run node scripts/build-viewer-configs.js."
);

const expectedConfigBundle = buildConfigBundleSource(loadConfigsFromDisk());
assert.strictEqual(
  readRepoFile(configBundleRelPath),
  expectedConfigBundle,
  "viewer/configs.generated.js is stale. Run node scripts/build-viewer-configs.js."
);

assert(
  !/shared\/abap-parser\/0\d-[^"]+\.js/.test(indexHtml),
  "viewer/index.html should load the parser bundle, not individual parser source parts."
);

assert(
  !/app\/(?:core|descriptions|template|output)\/0\d-[^"]+\.js/.test(indexHtml),
  "viewer/index.html should load viewer runtime bundles, not individual source parts."
);

assert(
  !/configs\.generated\//.test(indexHtml),
  "viewer/index.html should not reference legacy per-config generated wrapper files."
);

assert(
  /<script src="\.\.\/shared\/abap-parser\.js" defer><\/script>/.test(indexHtml),
  "viewer/index.html must load the parser runtime bundle."
);

assert(
  /<script src="\.\/configs\.generated\.js" defer><\/script>/.test(indexHtml),
  "viewer/index.html must load the consolidated config runtime bundle."
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
