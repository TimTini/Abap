"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { test } = require("node:test");
const {
  CONFIG_BUNDLE_REL_PATH,
  buildConfigBundleSource,
  loadConfigsFromDisk
} = require("../scripts/build-viewer-configs.js");
const { buildNextRuntimeText } = require("../scripts/sync-default-sample.js");
const { loadViewerDom } = require("./helpers/viewer-harness");
const { defineFocusedTest } = require("./helpers/test-focus");

const repoRoot = path.resolve(__dirname, "..");
const configBundleRelPath = CONFIG_BUNDLE_REL_PATH;
const legacyConfigDirPath = path.resolve(repoRoot, "viewer", "configs.generated");
const runtimeStatePath = path.resolve(repoRoot, "viewer", "app", "core", "01-runtime-state.js");
const samplePath = path.resolve(repoRoot, "examples", "deep_form_demo.abap");

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

defineFocusedTest(test, "runtime loader contracts", ["loader"], async (t) => {
  await t.test("legacy runtime bundle files stay removed", () => {
    const removedLegacyFiles = [
      "viewer/app/01-core.js",
      "viewer/app/02-descriptions.js",
      "viewer/app/03-template-preview.js",
      "viewer/app/04-output-render.js",
      "viewer/app/05-main.js",
      "scripts/build-runtime-bundles.js"
    ];

    for (const relPath of removedLegacyFiles) {
      assert(!fs.existsSync(path.resolve(repoRoot, relPath)), `${relPath} should be removed after switching to direct source loading.`);
    }
    assert(!fs.existsSync(legacyConfigDirPath), "Legacy viewer/configs.generated directory should be removed after consolidating config bundle.");
  });

  await t.test("runtime source files avoid legacy registries and eval", () => {
    const sourceFiles = [
      "shared/abap-parser.js",
      configBundleRelPath,
      "viewer/app/core/00-service-registry.js",
      "viewer/variable-descriptions.js",
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

    for (const relPath of sourceFiles) {
      const text = readRepoFile(relPath);
      assert(!/\b__AbapSourceParts\b/.test(text), `${relPath} should not depend on runtime source registries.`);
      assert(!/\bAbapViewerModules\b/.test(text), `${relPath} should not depend on legacy AbapViewerModules globals.`);
      assert(!/\beval\s*\(/.test(text), `${relPath} should not use eval.`);
    }
  });

  await t.test("generated viewer config bundle stays fresh", () => {
    const expectedConfigBundle = buildConfigBundleSource(loadConfigsFromDisk());
    assert.strictEqual(
      readRepoFile(configBundleRelPath),
      expectedConfigBundle,
      "viewer/configs.generated.js is stale. Run node scripts/build-viewer-configs.js."
    );
  });

  await t.test("runtime sample stays synchronized with deep form demo", () => {
    const runtimeText = fs.readFileSync(runtimeStatePath, "utf8");
    const sampleText = fs.readFileSync(samplePath, "utf8");
    assert.strictEqual(
      runtimeText,
      buildNextRuntimeText(runtimeText, sampleText),
      "viewer/app/core/01-runtime-state.js SAMPLE_ABAP is stale. Run node scripts/sync-default-sample.js."
    );
  });

  await t.test("service registry exposes the expected services and API", async () => {
    const dom = await loadViewerDom();
    try {
      const runtime = dom.window.AbapViewerRuntime || {};
      assert.deepStrictEqual(Array.from(runtime.serviceOrder || []), expectedServices);
      assert.deepStrictEqual(Array.from(Object.keys(runtime.services || {})), expectedServices);
      assert.deepStrictEqual(Array.from(Object.keys(runtime.api || {})).sort(), expectedApiKeys.slice().sort());
      for (const apiKey of expectedApiKeys) {
        assert.strictEqual(typeof runtime.api[apiKey], "function", `Expected runtime.api.${apiKey} to stay callable.`);
      }
      assert.strictEqual(Object.prototype.hasOwnProperty.call(dom.window, "AbapVarDescriptions"), false);
      assert.strictEqual(runtime.constants?.VARIABLE_DESCRIPTIONS?.system?.["SY-UNAME"], "Current user name");
    } finally {
      dom.window.close();
    }
  });
});
