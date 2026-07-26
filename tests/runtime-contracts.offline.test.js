"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { test } = require("node:test");
const { replaceConfigScriptBlock } = require("../scripts/build-viewer-configs.js");
const {
  assertSelfContainedInlineHtml,
  buildInlineViewerHtmlFromSources
} = require("./helpers/viewer-harness");
const { defineFocusedTest } = require("./helpers/test-focus");

const repoRoot = path.resolve(__dirname, "..");
const indexHtmlPath = path.resolve(repoRoot, "viewer", "index.html");
const inlineHtmlPath = path.resolve(repoRoot, "viewer", "index.inline.html");

const expectedScriptSrcs = [
  "../shared/abap-parser.js",
  "./configs.generated.js",
  "./app/core/00-service-registry.js",
  "./variable-descriptions.js",
  "./app/core/01-runtime-state.js",
  "./app/output/01-output-render.js",
  "./app/descriptions/01-normalize-and-desc.js",
  "./app/perform/01-perform-sources.js",
  "./app/template/00-excel-roundtrip.js",
  "./app/template/01-path-resolver.js",
  "./app/ui/01-navigation.js",
  "./app/parser/01-parser-controller.js",
  "./app/bootstrap/01-bootstrap.js",
  "./app.js"
];

const expectedStylesheetHrefs = ["./styles/viewer.css"];

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

defineFocusedTest(test, "runtime offline contracts", ["offline"], async (t) => {
  await t.test("index html stays offline-only and points at direct source assets", () => {
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
  });

  await t.test("inline html stays self-contained and fresh", () => {
    assert(fs.existsSync(inlineHtmlPath), "viewer/index.inline.html must exist.");
    const inlineHtml = fs.readFileSync(inlineHtmlPath, "utf8");
    assertNoRemoteUrls(inlineHtml, "viewer/index.inline.html");
    assertSelfContainedInlineHtml(inlineHtml);
    assert.strictEqual(
      inlineHtml,
      buildInlineViewerHtmlFromSources(),
      "viewer/index.inline.html is stale. Rebuild with the known Python runtime."
    );
  });

  await t.test("inline offline contract rejects asset urls but allows data and fragments", () => {
    assert.throws(
      () => assertSelfContainedInlineHtml('<style>.x{background:url(sprite.svg)}</style>'),
      /CSS url\(\) assets/
    );
    assert.doesNotThrow(
      () => assertSelfContainedInlineHtml('<style>.x{background:url(data:image/png;base64,AA==)}.y{mask:url(#shape)}</style>')
    );
  });
});
