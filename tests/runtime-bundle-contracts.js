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
const { buildNextRuntimeText } = require("../scripts/sync-default-sample.js");
const {
  assertSelfContainedInlineHtml,
  buildInlineViewerHtmlFromSources,
  loadViewerDom
} = require("./helpers/viewer-harness");

const repoRoot = path.resolve(__dirname, "..");
const configBundleRelPath = CONFIG_BUNDLE_REL_PATH;
const legacyConfigDirPath = path.resolve(repoRoot, LEGACY_CONFIG_DIR_REL_PATH);
const indexHtmlPath = path.resolve(repoRoot, "viewer", "index.html");
const inlineHtmlPath = path.resolve(repoRoot, "viewer", "index.inline.html");
const runtimeStatePath = path.resolve(repoRoot, "viewer", "app", "core", "01-runtime-state.js");
const samplePath = path.resolve(repoRoot, "examples", "deep_form_demo.abap");

const expectedScriptSrcs = [
  "../shared/abap-parser.js",
  "./configs.generated.js",
  "./variable-descriptions.js",
  "./app/core/00-service-registry.js",
  "./app/core/01-runtime-state.js",
  "./app/output/01-output-render.js",
  "./app/descriptions/01-normalize-and-desc.js",
  "./app/perform/01-perform-sources.js",
  "./app/template/01-path-resolver.js",
  "./app/ui/01-navigation.js",
  "./app/parser/01-parser-controller.js",
  "./app/bootstrap/01-bootstrap.js",
  "./app.js"
];

const expectedStylesheetHrefs = [
  "./styles/viewer.css"
];

const expectedServices = [
  "runtimeState",
  "output",
  "descriptions",
  "performSources",
  "template",
  "uiNavigation",
  "parserController",
  "bootstrap"
];

const expectedApiKeys = [
  "applyTemplateConfigFromEditor",
  "buildPerformCallPathRegistry",
  "buildTemplateCollectionCopyPayload",
  "buildViewerConfigBundle",
  "clearTemplateBlockSelection",
  "ensureTemplateWindowContainsIndex",
  "findDeclSegmentIndex",
  "getEffectiveDeclDesc",
  "getFinalDeclDesc",
  "getSegmentRangesForLineText",
  "getSelectedTemplateIndexes",
  "getViewerConfigExportFileName",
  "goToInputLine",
  "importViewerConfigObject",
  "init",
  "jumpInputToCodeRange",
  "parseFromTextarea",
  "renderDeclDescPanelUi",
  "renderTemplatePreview",
  "resolveValueLevelFinalDesc",
  "selectPerformSourceCandidate",
  "selectTemplateBlockFromInteraction",
  "setRightTab"
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

function collectTagAttributeValues(html, tagName, attrName) {
  const values = [];
  const regex = new RegExp(`<${tagName}\\b[^>]*\\b${attrName}=["']([^"']+)["'][^>]*>`, "gi");
  let match = regex.exec(html);
  while (match) {
    values.push(String(match[1] || "").trim());
    match = regex.exec(html);
  }
  return values;
}

async function main() {
  const removedLegacyFiles = [
    "viewer/app/01-core.js",
    "viewer/app/02-descriptions.js",
    "viewer/app/03-template-preview.js",
    "viewer/app/04-output-render.js",
    "viewer/app/05-main.js",
    "scripts/build-runtime-bundles.js"
  ];

  const sourceFiles = [
    "shared/abap-parser.js",
    configBundleRelPath,
    "viewer/app/core/00-service-registry.js",
    "viewer/app/core/01-runtime-state.js",
    "viewer/app/output/01-output-render.js",
    "viewer/app/descriptions/01-normalize-and-desc.js",
    "viewer/app/perform/01-perform-sources.js",
    "viewer/app/template/01-path-resolver.js",
    "viewer/app/ui/01-navigation.js",
    "viewer/app/parser/01-parser-controller.js",
    "viewer/app/bootstrap/01-bootstrap.js",
    "viewer/app.js"
  ];

  for (const relPath of removedLegacyFiles) {
    assert(!fs.existsSync(path.resolve(repoRoot, relPath)), `${relPath} should be removed after switching to direct source loading.`);
  }

  for (const relPath of sourceFiles) {
    const text = readRepoFile(relPath);
    assert(!/\b__AbapSourceParts\b/.test(text), `${relPath} should not depend on runtime source registries.`);
    assert(!/\bAbapViewerModules\b/.test(text), `${relPath} should not depend on legacy AbapViewerModules globals.`);
    assert(!/\beval\s*\(/.test(text), `${relPath} should not use eval.`);
  }

  assert(!fs.existsSync(legacyConfigDirPath), "Legacy viewer/configs.generated directory should be removed after consolidating config bundle.");

  const indexHtml = fs.readFileSync(indexHtmlPath, "utf8");
  assertNoRemoteUrls(indexHtml, "viewer/index.html");
  assert.strictEqual(
    indexHtml,
    replaceConfigScriptBlock(indexHtml),
    "viewer/index.html is stale. Run node scripts/build-viewer-configs.js."
  );
  assert.strictEqual(collectTagAttributeValues(indexHtml, "link", "href").join("|"), expectedStylesheetHrefs.join("|"));
  assert.deepStrictEqual(collectTagAttributeValues(indexHtml, "script", "src"), expectedScriptSrcs);
  assert(!/app\/0[1-5]-/.test(indexHtml), "viewer/index.html should not reference legacy runtime bundles.");
  assert(!/configs\.generated\//.test(indexHtml), "viewer/index.html should not reference legacy per-config wrapper files.");
  assert(!/<style\b/i.test(indexHtml), "viewer/index.html should load external CSS, not inline the app stylesheet.");

  const expectedConfigBundle = buildConfigBundleSource(loadConfigsFromDisk());
  assert.strictEqual(
    readRepoFile(configBundleRelPath),
    expectedConfigBundle,
    "viewer/configs.generated.js is stale. Run node scripts/build-viewer-configs.js."
  );

  assert(fs.existsSync(inlineHtmlPath), "viewer/index.inline.html must exist.");
  const inlineHtml = fs.readFileSync(inlineHtmlPath, "utf8");
  assertNoRemoteUrls(inlineHtml, "viewer/index.inline.html");
  assertSelfContainedInlineHtml(inlineHtml);
  assert.strictEqual(
    inlineHtml,
    buildInlineViewerHtmlFromSources(),
    "viewer/index.inline.html is stale. Rebuild with the known Python runtime."
  );
  assert.throws(
    () => assertSelfContainedInlineHtml('<style>.x{background:url(sprite.svg)}</style>'),
    /CSS url\(\) assets/,
    "Inline offline contract must reject local CSS url() assets."
  );
  assert.doesNotThrow(
    () => assertSelfContainedInlineHtml('<style>.x{background:url(data:image/png;base64,AA==)}.y{mask:url(#shape)}</style>'),
    "Inline offline contract should allow data: and fragment-only CSS url() values."
  );

  const runtimeText = fs.readFileSync(runtimeStatePath, "utf8");
  const sampleText = fs.readFileSync(samplePath, "utf8");
  assert.strictEqual(
    runtimeText,
    buildNextRuntimeText(runtimeText, sampleText),
    "viewer/app/core/01-runtime-state.js SAMPLE_ABAP is stale. Run node scripts/sync-default-sample.js."
  );

  const dom = await loadViewerDom();
  try {
    const runtime = dom.window.AbapViewerRuntime || {};
    assert.deepStrictEqual(Array.from(runtime.serviceOrder || []), expectedServices);
    assert.deepStrictEqual(Array.from(Object.keys(runtime.services || {})), expectedServices);
    assert.deepStrictEqual(Array.from(Object.keys(runtime.api || {})).sort(), expectedApiKeys.slice().sort());
  } finally {
    dom.window.close();
  }

  console.log("runtime-bundle-contracts: ok");
}

main().catch((err) => {
  console.error(err && err.stack ? err.stack : err);
  process.exit(1);
});
