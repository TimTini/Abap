"use strict";

const fs = require("fs");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assert,
  assertViewerFixtureDirectoriesStayInSync,
  findDataDeclGroup,
  findDataDeclRow,
  getDeclOverrideStorageKeyFromRuntime,
  path,
  renderFixture,
  waitForViewerUi
} = require("./helpers/viewer-contract-test-helpers");

async function assertDataCatalogGroupsEverySourceDeclaration() {
  const source = [
    "TYPES ty_code TYPE string.",
    "DATA gv_root TYPE ty_code. \"Root description",
    "CONSTANTS gc_kind TYPE string VALUE 'A'. \"Kind description",
    "PERFORM frm_catalog USING gv_root.",
    "FORM frm_catalog USING iv_value TYPE string.",
    "  DATA lv_local TYPE string. \"Local description",
    "  DATA(lv_inline) = iv_value.",
    "  FIELD-SYMBOLS <fs_value> TYPE string.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);

  assert.strictEqual(String(els.rightTabDescBtn.textContent || "").trim(), "Data");
  assert.strictEqual(String(els.rightPanelTitle.textContent || "").trim(), "Data");
  assert.strictEqual(els.declDescMissingOnly.checked, false, "Data must show every declaration by default.");

  const globalGroup = findDataDeclGroup(els, "GLOBAL");
  const formGroup = findDataDeclGroup(els, "FORM:FRM_CATALOG");
  assert(globalGroup, "Expected a Global declaration group.");
  assert(formGroup, "Expected a FORM:FRM_CATALOG declaration group.");
  assert(findDataDeclRow(globalGroup, "ty_code"), "Expected TYPES in the Global group.");
  assert(findDataDeclRow(globalGroup, "gv_root"), "Expected DATA in the Global group.");
  assert(findDataDeclRow(globalGroup, "gc_kind"), "Expected CONSTANTS in the Global group.");
  assert(findDataDeclRow(formGroup, "iv_value"), "Expected FORM_PARAM in the FORM group.");
  assert(findDataDeclRow(formGroup, "lv_local"), "Expected local DATA in the FORM group.");
  assert(findDataDeclRow(formGroup, "lv_inline"), "Expected INLINE data in the FORM group.");
  assert(findDataDeclRow(formGroup, "<fs_value>"), "Expected FIELD-SYMBOLS in the FORM group.");

  const expectedKeys = new Set((state.data.decls || [])
    .filter((decl) => {
      const objectType = String(decl && decl.objectType || "").toUpperCase();
      const scopeType = String(decl && decl.scopeType || "").toUpperCase();
      return decl && decl.name
        && !["SYSTEM", "CONDITION", "PATH_DECL"].includes(objectType)
        && !["SYSTEM", "PATH"].includes(scopeType);
    })
    .map((decl) => getDeclOverrideStorageKeyFromRuntime(window, decl))
    .filter(Boolean));
  const renderedKeys = new Set(Array.from(els.declDescTable.querySelectorAll("tbody tr[data-source-decl-key]"))
    .map((row) => String(row.getAttribute("data-source-decl-key") || ""))
    .filter(Boolean));
  assert.deepStrictEqual(Array.from(renderedKeys).sort(), Array.from(expectedKeys).sort());

  const headerLabels = Array.from(globalGroup.querySelectorAll("thead th"))
    .map((cell) => String(cell.textContent || "").trim());
  assert.deepStrictEqual(headerLabels, [
    "Type",
    "Technical ID",
    "Trace → Root",
    "Code description",
    "User description",
    "Effective description",
    "Edit"
  ]);

  dom.window.close();
}

async function assertOutputFeatureRemoved() {
  const sourceFiles = [
    "viewer/index.html",
    "viewer/app/core/00-service-registry.js",
    "viewer/app/core/01-runtime-state.js",
    "viewer/app/descriptions/01-normalize-and-desc.js",
    "viewer/app/perform/01-perform-sources.js",
    "viewer/app/template/01-path-resolver.js",
    "viewer/app/output/01-output-render.js",
    "viewer/app/ui/01-navigation.js",
    "viewer/app/parser/01-parser-controller.js",
    "viewer/app/bootstrap/01-bootstrap.js"
  ];
  const removedRuntimeTokens = [
    "rightTabOutputBtn",
    'id="output"',
    "els.output",
    'rightTab === "output"',
    "renderOutput",
    "outputVirtual",
    "pendingOutputViewportAnchor",
    "expandAllBtn",
    "collapseAllBtn",
    "clearFiltersBtn"
  ];
  for (const sourceFile of sourceFiles) {
    const sourceText = fs.readFileSync(path.resolve(__dirname, "..", sourceFile), "utf8");
    for (const token of removedRuntimeTokens) {
      assert(!sourceText.includes(token), `Expected ${sourceFile} not to retain removed Output runtime token ${token}.`);
    }
  }

  const dom = await renderFixture('DATA gv_value TYPE string. "Value description');
  const { window } = dom;
  const { document } = window;
  const { els, state, api } = window.AbapViewerRuntime;

  assert.strictEqual(document.querySelector("#rightTabOutputBtn"), null);
  assert.strictEqual(document.querySelector("#output"), null);
  assert.strictEqual(document.querySelector("#expandAllBtn"), null);
  assert.strictEqual(document.querySelector("#collapseAllBtn"), null);
  assert.strictEqual(document.querySelector("#clearFiltersBtn"), null);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(els, "rightTabOutputBtn"), false);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(els, "output"), false);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state, "collapsedIds"), false);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state, "selectedId"), false);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state, "outputVirtual"), false);
  assert.strictEqual(typeof api.renderOutput, "undefined");

  api.setRightTab("output");
  await waitForViewerUi(window);
  assert.strictEqual(state.rightTab, "template");
  assert.strictEqual(els.templatePreviewPanel.hidden, false);
  assert.strictEqual(els.declDescPanel.hidden, true);

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(state.rightTab, "descriptions");
  assert.strictEqual(els.declDescPanel.hidden, false);

  dom.window.close();
}

defineFocusedTest(test, "viewer data catalog contract", ["data-catalog"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("data catalog groups every source declaration", async () => {
    await assertDataCatalogGroupsEverySourceDeclaration();
  });
});

defineFocusedTest(test, "viewer output removal contract", ["output-removal"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("output feature removed", async () => {
    await assertOutputFeatureRemoved();
  });
});
