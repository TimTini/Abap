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

function createTypeFanoutFixture() {
  const typeComponentIdentity = "LOCAL:GLOBAL:TY_ORDER:CODE";
  const typeComponentDecl = {
    objectType: "TYPE_COMPONENT",
    name: "ty_order-code",
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL",
    typeIdentity: "LOCAL:GLOBAL:TY_ORDER",
    typeComponentIdentity,
    fieldPath: "code",
    comment: "Type component comment"
  };
  const makeInstance = (rootName, lineStart) => ({
    objectType: "STRUCT_FIELD",
    name: `${rootName}-code`,
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL",
    lineStart,
    structName: rootName,
    structObjectType: "DATA",
    structComment: `${rootName} root comment`,
    structTypeComment: "TYPE ROOT COMMENT MUST NOT LEAK",
    fieldPath: "code",
    typeIdentity: "LOCAL:GLOBAL:TY_ORDER",
    typeComponentIdentity
  });
  return {
    typeComponentDecl,
    instances: [makeInstance("gs_first", 10), makeInstance("gs_second", 11)]
  };
}

async function assertScopedKeysAndTypeDescriptionFanout() {
  const dom = await renderFixture("DATA gv_seed TYPE string.");
  const { window } = dom;
  const { state, services } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;
  state.settings = { ...state.settings, normalizeDeclDesc: false };

  const firstSameName = {
    objectType: "DATA",
    name: "gv_same",
    scopeLabel: "FORM:ONE",
    scopeType: "FORM",
    comment: "First code"
  };
  const secondSameName = {
    objectType: "DATA",
    name: "gv_same",
    scopeLabel: "FORM:TWO",
    scopeType: "FORM",
    comment: "Second code"
  };
  const inlineFirst = {
    objectType: "INLINE",
    name: "lv_same",
    scopeLabel: "FORM:ONE",
    scopeType: "FORM",
    declScopePath: "FORM:ONE/BLOCK:1",
    visibleFromLine: 10
  };
  const inlineSecond = { ...inlineFirst, declScopePath: "FORM:ONE/BLOCK:2", visibleFromLine: 20 };

  const firstKey = descriptions.getDeclOverrideStorageKey(firstSameName);
  const secondKey = descriptions.getDeclOverrideStorageKey(secondSameName);
  assert.notStrictEqual(firstKey, secondKey, "same-name declarations need scoped keys");
  assert.notStrictEqual(
    descriptions.getDeclOverrideStorageKey(inlineFirst),
    descriptions.getDeclOverrideStorageKey(inlineSecond),
    "INLINE declarations need deterministic scope-and-line keys"
  );

  state.descOverrides = { [firstKey]: "First override", [secondKey]: "Second override" };
  assert.strictEqual(descriptions.getEffectiveDeclDesc(firstSameName), "First override");
  assert.strictEqual(descriptions.getEffectiveDeclDesc(secondSameName), "Second override");
  assert.strictEqual(
    descriptions.getEffectiveDeclDesc({ ...firstSameName, scopeLabel: "FORM:THREE", comment: "Third code" }),
    "Third code",
    "legacy name-only descriptions must not bleed into another scope"
  );

  const { typeComponentDecl, instances } = createTypeFanoutFixture();
  state.data = { objects: [], decls: [typeComponentDecl, ...instances] };
  const typeUsageIndex = descriptions.rebuildTypeUsageIndex(state.data);
  assert.strictEqual(typeUsageIndex.get(typeComponentDecl.typeComponentIdentity).instanceDecls.length, 2);
  assert.strictEqual(
    descriptions.getEffectiveDeclDesc(instances[0]),
    "gs_first root comment-Type component comment",
    "instance roots use their own comment while items use the linked type comment"
  );

  const rootKey = descriptions.getDeclOverrideStorageKey({
    objectType: "DATA",
    name: "gs_first",
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL"
  });
  const firstInstanceKey = descriptions.getDeclOverrideStorageKey(instances[0]);
  const secondInstanceKey = descriptions.getDeclOverrideStorageKey(instances[1]);
  state.descOverrides[rootKey] = "Root override stays untouched";
  descriptions.applyDeclDescriptionOverride({ decl: instances[0], text: "Direct instance override", skipNormalize: true });
  assert.strictEqual(state.descOverrides[firstInstanceKey].text, "Direct instance override");

  const saveResult = descriptions.applyDeclDescriptionOverride({
    decl: typeComponentDecl,
    text: "Shared type override",
    skipNormalize: true
  });
  assert.strictEqual(saveResult.ok, true);
  assert.deepStrictEqual(new Set(saveResult.affectedKeys), new Set([
    descriptions.getDeclOverrideStorageKey(typeComponentDecl),
    firstInstanceKey,
    secondInstanceKey
  ]));
  assert.strictEqual(state.descOverrides[firstInstanceKey].text, "Shared type override");
  assert.strictEqual(state.descOverrides[secondInstanceKey].text, "Shared type override");
  assert.strictEqual(state.descOverrides[rootKey], "Root override stays untouched");

  const clearResult = descriptions.applyDeclDescriptionOverride({ decl: typeComponentDecl, clear: true });
  assert.strictEqual(clearResult.ok, true);
  assert.strictEqual(state.descOverrides[descriptions.getDeclOverrideStorageKey(typeComponentDecl)], undefined);
  assert.strictEqual(state.descOverrides[firstInstanceKey], undefined);
  assert.strictEqual(state.descOverrides[secondInstanceKey], undefined);
  assert.strictEqual(state.descOverrides[rootKey], "Root override stays untouched");
  assert.strictEqual(
    descriptions.getEffectiveDeclDesc(instances[0]),
    "Root override stays untouched-Type component comment",
    "clearing the type override falls back to the linked type component comment"
  );

  const typeRootDecl = {
    objectType: "TYPES",
    name: "ty_order",
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL"
  };
  descriptions.applyDeclDescriptionOverride({ decl: typeRootDecl, text: "Type root override", skipNormalize: true });
  assert.strictEqual(state.descOverrides[descriptions.getDeclOverrideStorageKey(typeRootDecl)].text, "Type root override");
  assert.strictEqual(state.descOverrides[firstInstanceKey], undefined, "TYPE root edits must not fan out to instances");

  const externalTypeComponent = {
    objectType: "TYPE_COMPONENT",
    name: "zty_external-id",
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL",
    typeIdentity: "EXTERNAL:ZTY_EXTERNAL",
    typeComponentIdentity: "EXTERNAL:ZTY_EXTERNAL:ID",
    fieldPath: "id",
    comment: "External id comment",
    dynamic: true
  };
  const externalInstance = {
    objectType: "STRUCT_FIELD",
    name: "gs_external-id",
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL",
    structName: "gs_external",
    structObjectType: "DATA",
    structComment: "External root comment",
    fieldPath: "id",
    typeIdentity: externalTypeComponent.typeIdentity,
    typeComponentIdentity: externalTypeComponent.typeComponentIdentity,
    dynamic: true
  };
  state.data = { objects: [], decls: [externalTypeComponent, externalInstance] };
  descriptions.rebuildTypeUsageIndex(state.data);
  const externalResult = descriptions.applyDeclDescriptionOverride({
    decl: externalTypeComponent,
    text: "External shared override",
    skipNormalize: true
  });
  assert.strictEqual(externalResult.ok, true);
  assert.strictEqual(
    state.descOverrides[descriptions.getDeclOverrideStorageKey(externalInstance)].text,
    "External shared override",
    "external dynamic type components fan out to matching instances"
  );

  state.rightTab = "descriptions";
  descriptions.renderDeclDescPanelUi();
  const externalGroup = findDataDeclGroup(window.AbapViewerRuntime.els, "EXTERNAL:ZTY_EXTERNAL");
  assert(externalGroup, "external dynamic type components need their own Data group");
  const externalTypeRow = findDataDeclRow(externalGroup, "zty_external-id");
  assert(externalTypeRow, "external dynamic type components need an editable Data row");
  assert.match(
    String(externalTypeRow.querySelector('[data-column="trace"]').textContent || ""),
    /zty_external-id → 1 instances/i
  );
  const globalGroup = findDataDeclGroup(window.AbapViewerRuntime.els, "GLOBAL");
  const externalInstanceRow = findDataDeclRow(globalGroup, "gs_external-id");
  assert.match(
    String(externalInstanceRow.querySelector('[data-column="trace"]').textContent || ""),
    /gs_external-id ← zty_external-id/i
  );

  dom.window.close();
}

async function assertOverrideWritesAreAtomicAndCanonical() {
  const dom = await renderFixture("DATA gv_seed TYPE string.");
  const { window } = dom;
  const { state, services, constants } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;
  state.settings = { ...state.settings, normalizeDeclDesc: false };

  const pathDecl = {
    objectType: "PATH_DECL",
    name: "carrier",
    scopeType: "PATH",
    scopeLabel: "PATH:OBJECT:4/VALUES/CARRIER",
    canonicalOverrideKey: "PATH:OBJECT:4/VALUES/CARRIER:CARRIER",
    overrideLookupKeys: [
      "PATH:OBJECT:4/VALUES/CARRIER:CARRIER",
      "PATH:OBJECTS/OBJECT[4]/VALUES/CARRIER:CARRIER"
    ]
  };
  const lookupKeys = descriptions.getDeclOverrideLookupKeys(pathDecl);
  assert(lookupKeys.length >= 2, "PATH fixture needs a primary key plus a compatibility alias.");
  const [primaryKey, aliasKey] = lookupKeys;
  state.descOverrides = {
    [primaryKey]: "Old primary",
    [aliasKey]: "Old alias"
  };

  const snapshot = JSON.parse(JSON.stringify(state.descOverrides));
  const originalSetItem = window.localStorage.setItem;
  window.localStorage.setItem = () => {
    throw new Error("simulated storage failure");
  };
  const failed = descriptions.applyDeclDescriptionOverride({
    decl: pathDecl,
    text: "Must roll back",
    skipNormalize: true
  });
  window.localStorage.setItem = originalSetItem;
  assert.strictEqual(failed, false);
  assert.deepStrictEqual(state.descOverrides, snapshot, "failed persistence must roll RAM back atomically");

  const saved = descriptions.applyDeclDescriptionOverride({
    decl: pathDecl,
    text: "Canonical value",
    skipNormalize: true
  });
  assert.strictEqual(saved.ok, true);
  assert.strictEqual(state.descOverrides[primaryKey].text, "Canonical value");
  assert.strictEqual(state.descOverrides[aliasKey], undefined, "successful writes purge compatibility aliases");

  state.descOverrides[aliasKey] = "Stale alias";
  const cleared = descriptions.applyDeclDescriptionOverride({ decl: pathDecl, clear: true });
  assert.strictEqual(cleared.ok, true);
  assert.strictEqual(state.descOverrides[primaryKey], undefined);
  assert.strictEqual(state.descOverrides[aliasKey], undefined, "clear purges compatibility aliases too");

  const registry = constants.VARIABLE_DESCRIPTIONS;
  registry.customGlobal.GV_SHARED = "Global registry description";
  const globalDecl = {
    objectType: "DATA",
    name: "gv_shared",
    scopeLabel: "GLOBAL",
    scopeType: "GLOBAL"
  };
  const localDecl = {
    ...globalDecl,
    scopeLabel: "FORM:LOCAL",
    scopeType: "FORM",
    scopeName: "local"
  };
  assert.strictEqual(descriptions.getEffectiveDeclDesc(globalDecl), "Global registry description");
  assert.strictEqual(
    descriptions.getEffectiveDeclDesc(localDecl),
    "gv_shared",
    "customGlobal must not leak into local declarations"
  );
  delete registry.customGlobal.GV_SHARED;

  dom.window.close();
}

async function assertActualParserBuildsLexicalInlineAndEligibleTypeUsage() {
  const source = [
    "TYPES: BEGIN OF ty_row,",
    '         id TYPE i, "Type item comment',
    "       END OF ty_row.",
    'DATA gs_row TYPE ty_row. "Data root',
    'FIELD-SYMBOLS <fs_row> TYPE ty_row. "Field-symbol root',
    'PARAMETERS ps_row TYPE ty_row. "Parameter root',
    "IF sy-subrc = 0.",
    "  DATA(ls_same) = gs_row.",
    "  WRITE ls_same-id.",
    "ELSE.",
    "  DATA(ls_same) = gs_row.",
    "  WRITE ls_same-id.",
    "ENDIF.",
    "WRITE <fs_row>-id.",
    "FIELD-SYMBOLS <fs_ext> TYPE zext.",
    "WRITE <fs_ext>-observed."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { state, services } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;
  state.settings = { ...state.settings, normalizeDeclDesc: false };

  const inlineFields = (state.data.decls || []).filter((decl) => (
    String(decl.objectType || "").toUpperCase() === "STRUCT_FIELD"
    && String(decl.name || "").toUpperCase() === "LS_SAME-ID"
  ));
  assert.strictEqual(inlineFields.length, 2, "IF and ELSE inline fields need separate synthetic declarations");
  assert.deepStrictEqual(
    new Set(inlineFields.map((decl) => String(decl.declScopePath || ""))).size,
    2,
    "synthetic inline fields retain their lexical scope paths"
  );
  const inlineKeys = inlineFields.map((decl) => descriptions.getDeclOverrideStorageKey(decl));
  assert(inlineKeys.every(Boolean), "every lexical synthetic field needs an editable storage key");
  assert.strictEqual(new Set(inlineKeys).size, 2, "lexical synthetic field keys must not collide");

  const localTypeComponent = (state.data.decls || []).find((decl) => (
    String(decl.objectType || "").toUpperCase() === "TYPE_COMPONENT"
    && String(decl.typeComponentIdentity || "") === "LOCAL:GLOBAL:TY_ROW:ID"
  ));
  assert(localTypeComponent, "actual parser should emit the local TYPE component");
  const usage = state.typeUsageIndex.get(localTypeComponent.typeComponentIdentity);
  assert.deepStrictEqual(
    new Set(usage.instanceDecls.map((decl) => String(decl.name || "").toUpperCase())),
    new Set(["GS_ROW-ID", "<FS_ROW>-ID"]),
    "fan-out index includes DATA and FIELD-SYMBOLS only"
  );

  const parameterRoot = (state.data.decls || []).find((decl) => (
    String(decl.objectType || "").toUpperCase() === "PARAMETERS"
    && String(decl.name || "").toUpperCase() === "PS_ROW"
  ));
  assert(parameterRoot, "actual parser should emit the PARAMETERS root for the exclusion check");
  const parameterField = {
    objectType: "STRUCT_FIELD",
    name: "ps_row-id",
    scopeLabel: parameterRoot.scopeLabel,
    scopeType: parameterRoot.scopeType,
    structName: parameterRoot.name,
    structObjectType: "PARAMETERS",
    fieldPath: "id",
    typeIdentity: localTypeComponent.typeIdentity,
    typeComponentIdentity: localTypeComponent.typeComponentIdentity
  };
  state.data.decls.push(parameterField);
  descriptions.rebuildTypeUsageIndex(state.data);
  assert(
    !state.typeUsageIndex.get(localTypeComponent.typeComponentIdentity).instanceDecls.includes(parameterField),
    "non-DATA/non-FIELD-SYMBOL structure items stay outside the fan-out index"
  );
  const parameterKey = descriptions.getDeclOverrideStorageKey(parameterField);
  state.descOverrides[parameterKey] = "Parameter item stays";

  const rootDeclNames = ["gs_row", "<fs_row>", "ps_row"];
  const rootKeys = rootDeclNames.map((name) => descriptions.getDeclOverrideStorageKey(
    (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === name.toUpperCase())
  ));
  rootKeys.forEach((key, index) => {
    state.descOverrides[key] = `Root ${index} stays`;
  });

  descriptions.applyDeclDescriptionOverride({
    decl: localTypeComponent,
    text: "Shared actual parser item",
    skipNormalize: true
  });
  for (const instanceDecl of usage.instanceDecls) {
    assert.strictEqual(
      state.descOverrides[descriptions.getDeclOverrideStorageKey(instanceDecl)].text,
      "Shared actual parser item"
    );
  }
  assert.strictEqual(state.descOverrides[parameterKey], "Parameter item stays");
  rootKeys.forEach((key, index) => {
    assert.strictEqual(state.descOverrides[key], `Root ${index} stays`);
  });

  const externalComponent = (state.data.decls || []).find((decl) => (
    decl.dynamic === true
    && String(decl.typeComponentIdentity || "") === "EXTERNAL:ZEXT:OBSERVED"
  ));
  const externalFieldSymbolItem = (state.data.decls || []).find((decl) => (
    String(decl.name || "").toUpperCase() === "<FS_EXT>-OBSERVED"
  ));
  assert(externalComponent, "observed external FIELD-SYMBOL item needs a dynamic TYPE component");
  assert(externalFieldSymbolItem, "observed <fs>-field usage needs an instance item");
  const externalUsage = state.typeUsageIndex.get(externalComponent.typeComponentIdentity);
  assert.strictEqual(externalUsage.instanceDecls.length, 1);
  assert.strictEqual(externalUsage.instanceDecls[0], externalFieldSymbolItem);

  dom.window.close();
}

async function assertActualPerformChainTypeFanout() {
  const source = [
    "TYPES: BEGIN OF ty_order,",
    "         code TYPE string,",
    "       END OF ty_order.",
    "DATA gs_one TYPE ty_order.",
    "PERFORM frm USING gs_one.",
    "FORM frm USING is_order TYPE ty_order.",
    "  WRITE is_order-code.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { state, services, els } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);
  const formGroup = findDataDeclGroup(els, "FORM:FRM");
  const formalItemRow = findDataDeclRow(formGroup, "is_order-code");
  assert(formalItemRow, "actual FORM trace should expose the typed formal item");
  const chainKey = String(formalItemRow.getAttribute("data-decl-key") || "");
  assert.match(chainKey, /^PERFORM_CHAIN:/);
  state.descOverrides[chainKey] = "Selected chain item";

  const typeComponent = (state.data.decls || []).find((decl) => (
    String(decl.typeComponentIdentity || "") === "LOCAL:GLOBAL:TY_ORDER:CODE"
    && String(decl.objectType || "").toUpperCase() === "TYPE_COMPONENT"
  ));
  descriptions.applyDeclDescriptionOverride({
    decl: typeComponent,
    text: "Type overwrites selected chain",
    skipNormalize: true
  });
  assert.strictEqual(
    state.descOverrides[chainKey].text,
    "Type overwrites selected chain",
    "TYPE save overwrites the exact registered selected-chain item override"
  );
  descriptions.applyDeclDescriptionOverride({ decl: typeComponent, clear: true });
  assert.strictEqual(state.descOverrides[chainKey], undefined, "TYPE clear removes the selected-chain item override");

  dom.window.close();
}

async function assertUnselectedPerformCandidateTypeFanout() {
  const source = [
    "TYPES: BEGIN OF ty_order,",
    "         code TYPE string,",
    "       END OF ty_order.",
    "DATA gs_one TYPE ty_order.",
    "DATA gs_two TYPE ty_order.",
    "PERFORM frm USING gs_one.",
    "PERFORM frm USING gs_two.",
    "FORM frm USING is_order TYPE ty_order.",
    "  WRITE is_order-code.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { state, services, els } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;

  const candidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM") || [];
  assert.strictEqual(candidates.length, 2, "two local PERFORM calls must create two candidates");
  const selectedCandidate = state.performSourceRegistry.getSelectedCandidate("FRM");
  const unselectedCandidate = candidates.find((candidate) => candidate.key !== selectedCandidate.key);
  assert(unselectedCandidate, "one candidate must remain unselected");

  const formalItem = (state.data.decls || []).find((decl) => (
    String(decl.objectType || "").toUpperCase() === "STRUCT_FIELD"
    && String(decl.structObjectType || "").toUpperCase() === "FORM_PARAM"
    && String(decl.name || "").toUpperCase() === "IS_ORDER-CODE"
  ));
  assert(formalItem, "typed FORM item must be present before rendering an alternate candidate");
  const unselectedChainKey = [
    "PERFORM_CHAIN",
    unselectedCandidate.sourceScope,
    encodeURIComponent("FORM:FRM|IS_ORDER|CODE")
  ].join(":");
  state.descOverrides[unselectedChainKey] = "Stale unselected candidate override";

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);

  const typeComponent = (state.data.decls || []).find((decl) => (
    String(decl.typeComponentIdentity || "") === "LOCAL:GLOBAL:TY_ORDER:CODE"
    && String(decl.objectType || "").toUpperCase() === "TYPE_COMPONENT"
  ));
  descriptions.applyDeclDescriptionOverride({
    decl: typeComponent,
    text: "Type updates every candidate",
    skipNormalize: true
  });
  assert.strictEqual(
    state.descOverrides[unselectedChainKey].text,
    "Type updates every candidate",
    "TYPE save must overwrite the exact unselected candidate chain override"
  );

  descriptions.applyDeclDescriptionOverride({ decl: typeComponent, clear: true });
  assert.strictEqual(
    state.descOverrides[unselectedChainKey],
    undefined,
    "TYPE clear must remove the exact unselected candidate chain override"
  );

  dom.window.close();
}

async function assertInlineFieldSymbolScopeAndTypeFanout() {
  const source = [
    "TYPES: BEGIN OF ty_row,",
    "         id TYPE i,",
    "       END OF ty_row.",
    "DATA gt_rows TYPE STANDARD TABLE OF ty_row.",
    "READ TABLE gt_rows INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_inline>).",
    "WRITE <fs_inline>-id.",
    "IF sy-subrc = 0.",
    "  READ TABLE gt_rows INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_branch>).",
    "  WRITE <fs_branch>-id.",
    "ENDIF.",
    "WRITE <fs_branch>-id."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { state, services } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;

  const inlineRoot = (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === "<FS_INLINE>");
  const inlineItem = (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === "<FS_INLINE>-ID");
  const branchRoot = (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === "<FS_BRANCH>");
  const branchItem = (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === "<FS_BRANCH>-ID");
  assert(inlineRoot && inlineItem && branchRoot && branchItem, "inline FIELD-SYMBOL roots and items must be catalogued");

  const lexicalKeys = [inlineRoot, inlineItem, branchRoot, branchItem]
    .map((decl) => descriptions.getDeclOverrideStorageKey(decl));
  assert(lexicalKeys.every(Boolean), "inline FIELD-SYMBOL roots/items need editable lexical keys");
  assert.strictEqual(new Set(lexicalKeys).size, lexicalKeys.length);

  const writes = [];
  const visit = (objects) => {
    for (const obj of Array.isArray(objects) ? objects : []) {
      if (String(obj.objectType || "").toUpperCase() === "WRITE") {
        writes.push(obj);
      }
      visit(obj.children);
    }
  };
  visit(state.data.objects);
  const insideBranchWrite = writes.find((obj) => Number(obj.lineStart) === 9);
  const outsideBranchWrite = writes.find((obj) => Number(obj.lineStart) === 11);
  assert.strictEqual(insideBranchWrite.values.output.decl, branchItem, "usage inside owner branch binds to inline item");
  assert.notStrictEqual(
    outsideBranchWrite.values.output.decl,
    branchItem,
    "usage outside owner branch must not bind to its inline FIELD-SYMBOL item"
  );

  const typeComponent = (state.data.decls || []).find((decl) => (
    String(decl.typeComponentIdentity || "") === "LOCAL:GLOBAL:TY_ROW:ID"
    && String(decl.objectType || "").toUpperCase() === "TYPE_COMPONENT"
  ));
  const typeUsage = state.typeUsageIndex.get(typeComponent.typeComponentIdentity);
  assert(typeUsage.instanceDecls.includes(inlineItem), "table-line inference links inline FIELD-SYMBOL item to TYPE component");

  const inlineRootKey = descriptions.getDeclOverrideStorageKey(inlineRoot);
  const inlineItemKey = descriptions.getDeclOverrideStorageKey(inlineItem);
  state.descOverrides[inlineRootKey] = "Inline root stays";
  descriptions.applyDeclDescriptionOverride({
    decl: typeComponent,
    text: "Inline item follows type",
    skipNormalize: true
  });
  assert.strictEqual(state.descOverrides[inlineRootKey], "Inline root stays");
  assert.strictEqual(state.descOverrides[inlineItemKey].text, "Inline item follows type");

  dom.window.close();
}

async function assertExternalInstanceItemEditsFanOutAcrossFormChains() {
  const source = [
    "DATA a TYPE ztype.",
    "DATA b TYPE ztype.",
    "DATA c TYPE zother.",
    "TYPES: BEGIN OF ty_local,",
    "         createdate TYPE d,",
    "       END OF ty_local.",
    "DATA x TYPE ty_local.",
    "DATA y TYPE ty_local.",
    "a-createdate = sy-datum.",
    "b-createdate = sy-datum.",
    "c-createdate = sy-datum.",
    "x-createdate = sy-datum.",
    "y-createdate = sy-datum.",
    "PERFORM frm_a USING a.",
    "PERFORM frm_b USING b.",
    "FORM frm_a USING p_a TYPE ztype.",
    "  WRITE p_a-createdate.",
    "ENDFORM.",
    "FORM frm_b USING p_b TYPE ztype.",
    "  WRITE p_b-createdate.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state, services } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;
  state.settings = { ...state.settings, normalizeDeclDesc: false };

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);

  const findDecl = (name, objectType) => (state.data.decls || []).find((decl) => (
    String(decl.name || "").toUpperCase() === String(name || "").toUpperCase()
    && String(decl.objectType || "").toUpperCase() === String(objectType || "").toUpperCase()
  ));
  const getOverrideText = (decl) => descriptions.getDeclOverrideEntry(decl).text;

  const rootA = findDecl("a", "DATA");
  const rootB = findDecl("b", "DATA");
  const itemA = findDecl("a-createdate", "STRUCT_FIELD");
  const itemB = findDecl("b-createdate", "STRUCT_FIELD");
  const otherTypeItem = findDecl("c-createdate", "STRUCT_FIELD");
  const localItemX = findDecl("x-createdate", "STRUCT_FIELD");
  const localItemY = findDecl("y-createdate", "STRUCT_FIELD");
  assert(
    rootA && rootB && itemA && itemB && otherTypeItem && localItemX && localItemY,
    "external and local roots and observed items must be catalogued"
  );

  let globalGroup = findDataDeclGroup(els, "GLOBAL");
  findDataDeclRow(globalGroup, "a")
    .querySelector('button[data-action="edit-description"]')
    .click();
  await waitForViewerUi(window);
  els.editDesc.value = "Root A only";
  els.editSaveBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(getOverrideText(rootA), "Root A only");
  assert.strictEqual(getOverrideText(rootB), "", "root descriptions stay isolated");

  globalGroup = findDataDeclGroup(els, "GLOBAL");
  findDataDeclRow(globalGroup, "a-createdate")
    .querySelector('button[data-action="edit-description"]')
    .click();
  await waitForViewerUi(window);
  els.editStructDesc.value = "Root A only";
  els.editItemDesc.value = "Shared create date";
  els.editSaveBtn.click();
  await waitForViewerUi(window);

  assert.strictEqual(getOverrideText(rootA), "Root A only");
  assert.strictEqual(getOverrideText(rootB), "", "editing an item must not fan out its root description");
  assert.strictEqual(getOverrideText(itemA), "Shared create date");
  assert.strictEqual(getOverrideText(itemB), "Shared create date", "same external type item follows the edit");
  assert.strictEqual(getOverrideText(otherTypeItem), "", "same-named items from another external type stay isolated");

  globalGroup = findDataDeclGroup(els, "GLOBAL");
  findDataDeclRow(globalGroup, "x-createdate")
    .querySelector('button[data-action="edit-description"]')
    .click();
  await waitForViewerUi(window);
  els.editStructDesc.value = "";
  els.editItemDesc.value = "Local X only";
  els.editSaveBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(getOverrideText(localItemX), "Local X only");
  assert.strictEqual(getOverrideText(localItemY), "", "declared local type instances keep their existing isolated item edits");

  let formAGroup = findDataDeclGroup(els, "FORM:FRM_A");
  let formBGroup = findDataDeclGroup(els, "FORM:FRM_B");
  let formAItemRow = findDataDeclRow(formAGroup, "p_a-createdate");
  let formBItemRow = findDataDeclRow(formBGroup, "p_b-createdate");
  const formAChainKey = String(formAItemRow.getAttribute("data-decl-key") || "");
  const formBChainKey = String(formBItemRow.getAttribute("data-decl-key") || "");
  assert.match(formAChainKey, /^PERFORM_CHAIN:/);
  assert.match(formBChainKey, /^PERFORM_CHAIN:/);
  assert.strictEqual(descriptions.normalizeDescOverrideEntry(state.descOverrides[formAChainKey]).text, "Shared create date");
  assert.strictEqual(descriptions.normalizeDescOverrideEntry(state.descOverrides[formBChainKey]).text, "Shared create date");

  formBItemRow.querySelector('button[data-action="edit-description"]').click();
  await waitForViewerUi(window);
  els.editDesc.value = "Updated from FORM B";
  els.editSaveBtn.click();
  await waitForViewerUi(window);

  assert.strictEqual(getOverrideText(itemA), "Updated from FORM B");
  assert.strictEqual(getOverrideText(itemB), "Updated from FORM B");
  assert.strictEqual(descriptions.normalizeDescOverrideEntry(state.descOverrides[formAChainKey]).text, "Updated from FORM B");
  assert.strictEqual(descriptions.normalizeDescOverrideEntry(state.descOverrides[formBChainKey]).text, "Updated from FORM B");
  assert.strictEqual(getOverrideText(rootA), "Root A only");
  assert.strictEqual(getOverrideText(rootB), "");

  globalGroup = findDataDeclGroup(els, "GLOBAL");
  findDataDeclRow(globalGroup, "b-createdate")
    .querySelector('button[data-action="edit-description"]')
    .click();
  await waitForViewerUi(window);
  els.editStructDesc.value = "";
  els.editItemDesc.value = "";
  els.editSaveBtn.click();
  await waitForViewerUi(window);

  assert.strictEqual(getOverrideText(itemA), "");
  assert.strictEqual(getOverrideText(itemB), "");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, formAChainKey), false);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, formBChainKey), false);
  assert.strictEqual(getOverrideText(rootA), "Root A only");
  assert.strictEqual(getOverrideText(rootB), "");

  const snapshot = JSON.parse(JSON.stringify(state.descOverrides));
  globalGroup = findDataDeclGroup(els, "GLOBAL");
  findDataDeclRow(globalGroup, "a-createdate")
    .querySelector('button[data-action="edit-description"]')
    .click();
  await waitForViewerUi(window);
  els.editStructDesc.value = "Must roll back root";
  els.editItemDesc.value = "Must roll back item";
  const originalSetItem = window.localStorage.setItem;
  window.localStorage.setItem = () => {
    throw new Error("simulated storage failure");
  };
  const failed = descriptions.applyEditModal("save");
  window.localStorage.setItem = originalSetItem;
  descriptions.closeEditModal();
  assert.strictEqual(failed, false);
  assert.deepStrictEqual(
    JSON.parse(JSON.stringify(state.descOverrides)),
    snapshot,
    "external root and item writes roll back atomically"
  );

  dom.window.close();
}

async function assertLoopInlineFieldSymbolTypeFanout() {
  const source = [
    "TYPES: BEGIN OF ty_row,",
    "         id TYPE i,",
    "       END OF ty_row.",
    "DATA gt_rows TYPE STANDARD TABLE OF ty_row.",
    "LOOP AT gt_rows ASSIGNING FIELD-SYMBOL(<fs_loop>).",
    "  WRITE <fs_loop>-id.",
    "ENDLOOP."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { state, services } = window.AbapViewerRuntime;
  const descriptions = services.descriptions;

  const inlineRoot = (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === "<FS_LOOP>");
  const inlineItem = (state.data.decls || []).find((decl) => String(decl.name || "").toUpperCase() === "<FS_LOOP>-ID");
  const typeComponent = (state.data.decls || []).find((decl) => (
    String(decl.typeComponentIdentity || "") === "LOCAL:GLOBAL:TY_ROW:ID"
    && String(decl.objectType || "").toUpperCase() === "TYPE_COMPONENT"
  ));
  assert(inlineRoot && inlineItem && typeComponent, "LOOP inline FIELD-SYMBOL and typed item must be catalogued");
  assert.strictEqual(inlineRoot.typeIdentity, "LOCAL:GLOBAL:TY_ROW");
  assert.strictEqual(inlineItem.typeComponentIdentity, typeComponent.typeComponentIdentity);
  assert(state.typeUsageIndex.get(typeComponent.typeComponentIdentity).instanceDecls.includes(inlineItem));

  const inlineRootKey = descriptions.getDeclOverrideStorageKey(inlineRoot);
  const inlineItemKey = descriptions.getDeclOverrideStorageKey(inlineItem);
  state.descOverrides[inlineRootKey] = "LOOP root stays";
  descriptions.applyDeclDescriptionOverride({
    decl: typeComponent,
    text: "LOOP item follows type",
    skipNormalize: true
  });
  assert.strictEqual(state.descOverrides[inlineRootKey], "LOOP root stays");
  assert.strictEqual(state.descOverrides[inlineItemKey].text, "LOOP item follows type");

  dom.window.close();
}

defineFocusedTest(test, "viewer scoped descriptions and type fan-out contract", ["data-catalog"], async (t) => {
  assertViewerFixtureDirectoriesStayInSync();

  await t.test("keeps scopes isolated and fans type component edits to instances", async () => {
    await assertScopedKeysAndTypeDescriptionFanout();
  });

  await t.test("writes canonical aliases atomically and scopes global registry descriptions", async () => {
    await assertOverrideWritesAreAtomicAndCanonical();
  });

  await t.test("actual parser keeps inline scopes and eligible DATA/FIELD-SYMBOL type usage", async () => {
    await assertActualParserBuildsLexicalInlineAndEligibleTypeUsage();
  });

  await t.test("actual PERFORM chain item follows TYPE save and clear", async () => {
    await assertActualPerformChainTypeFanout();
  });

  await t.test("TYPE save and clear include unselected local PERFORM candidates", async () => {
    await assertUnselectedPerformCandidateTypeFanout();
  });

  await t.test("external instance item edits fan out across FORM chains without sharing roots", async () => {
    await assertExternalInstanceItemEditsFanOutAcrossFormChains();
  });

  await t.test("inline FIELD-SYMBOL stays lexical and its item follows inferred TYPE", async () => {
    await assertInlineFieldSymbolScopeAndTypeFanout();
  });

  await t.test("LOOP inline FIELD-SYMBOL item follows inferred table-line TYPE", async () => {
    await assertLoopInlineFieldSymbolTypeFanout();
  });
});
