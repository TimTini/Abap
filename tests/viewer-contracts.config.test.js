"use strict";

const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assert,
  assertViewerFixtureDirectoriesStayInSync,
  cloneTestJson,
  findVisibleConfigExportModal,
  getTemplateTableRows,
  renderFixture,
  STATEMENT_TEMPLATE_KEYS,
  VIEWER_CONFIG_SECTION_KEYS,
  VIEWER_CONFIG_STORAGE_KEYS,
  waitForViewerUi
} = require("./helpers/viewer-contract-test-helpers");

async function assertGroupedConfigExportIsDeterministic() {
  const dom = await renderFixture("DATA lv_export TYPE string.\nlv_export = 'A'.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { api, els, state } = runtime;

  assert.strictEqual(
    typeof api.buildViewerConfigBundle,
    "function",
    "Expected Viewer config export to expose its deterministic bundle builder."
  );
  assert.strictEqual(
    typeof api.getViewerConfigExportFileName,
    "function",
    "Expected Viewer config export to expose deterministic filenames."
  );

  state.templateConfig.templates.DEFAULT["Z98"] = { text: "EXPORT_TEMPLATE_MARKER" };
  state.settings = {
    normalizeDeclDesc: false,
    declFilterTypes: ["DATA", "TYPES"],
    structDescTemplate: "{{struct}} :: {{item}}",
    nameTemplatesByCode: cloneTestJson(state.settings.nameTemplatesByCode)
  };
  state.descOverrides = {
    "input.abap|PROGRAM|Z_EXPORT|DATA|LV_EXPORT|1": "Manual override",
    "input.abap|PROGRAM|Z_EXPORT|DATA|LV_SKIP|2": { text: "Raw override", noNormalize: true }
  };
  state.descOverridesLegacy = { "legacy-secret": "LEGACY_OVERRIDE_MARKER" };
  state.theme = "light";
  state.layoutLeftPane = 63;
  state.templateGuiHiddenTypes = new window.Set(["IF", "DATA"]);
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.formEditorPct, "64");
  state.data = { source: "RUNTIME_SOURCE_MARKER" };
  state.renderObjects = [{ source: "RUNTIME_TREE_MARKER" }];

  const selectedBundle = api.buildViewerConfigBundle(
    ["templateUi", "templates"],
    "2026-07-15T03:04:05.000Z"
  );
  assert.deepStrictEqual(Array.from(Object.keys(selectedBundle.sections)), ["templates", "templateUi"]);
  assert.strictEqual(selectedBundle.kind, "abap-viewer-config");
  assert.strictEqual(selectedBundle.version, 1);
  assert.strictEqual(selectedBundle.exportedAt, "2026-07-15T03:04:05.000Z");
  assert.strictEqual(selectedBundle.sections.templates.templates.DEFAULT.Z98.text, "EXPORT_TEMPLATE_MARKER");
  assert.deepStrictEqual(Array.from(selectedBundle.sections.templateUi.hiddenObjectTypes), ["DATA", "IF"]);
  assert.strictEqual(selectedBundle.sections.templateUi.formEditorPct, 64);
  assert.strictEqual(
    api.getViewerConfigExportFileName(["templateUi", "templates"]),
    "abap-viewer-config-templates-template-ui.json"
  );
  assert.strictEqual(
    api.getViewerConfigExportFileName(VIEWER_CONFIG_SECTION_KEYS.slice().reverse()),
    "abap-viewer-config.json"
  );

  const allBundle = api.buildViewerConfigBundle(
    VIEWER_CONFIG_SECTION_KEYS.slice().reverse(),
    "2026-07-15T03:04:05.000Z"
  );
  assert.deepStrictEqual(Array.from(Object.keys(allBundle.sections)), VIEWER_CONFIG_SECTION_KEYS);
  assert.deepStrictEqual(cloneTestJson(allBundle.sections.descriptionOverrides), state.descOverrides);
  const allJson = JSON.stringify(allBundle);
  assert(!allJson.includes("LEGACY_OVERRIDE_MARKER"), "Expected export to exclude legacy overrides.");
  assert(!allJson.includes("RUNTIME_SOURCE_MARKER"), "Expected export to exclude parsed source state.");
  assert(!allJson.includes("RUNTIME_TREE_MARKER"), "Expected export to exclude runtime render state.");
  assert(!allJson.includes("abap-parser-viewer."), "Expected export not to expose localStorage key names.");

  let downloadedName = "";
  let downloadedText = "";
  const OriginalBlob = window.Blob;
  const originalAnchorClick = window.HTMLAnchorElement.prototype.click;
  window.Blob = class CapturedBlob {
    constructor(parts) {
      downloadedText = parts.map((part) => String(part)).join("");
    }
  };
  window.HTMLAnchorElement.prototype.click = function captureDownload() {
    downloadedName = String(this.download || "");
  };

  try {
    els.templateExportBtn.click();
    await waitForViewerUi(window);
    const modal = findVisibleConfigExportModal(window);
    assert(modal, "Expected Export config to open a section-selection modal.");
    for (const label of ["Templates", "Description settings", "Description overrides", "Appearance", "Template UI"] ) {
      assert(String(modal.textContent || "").includes(label), `Expected export modal section ${label}.`);
    }
    const selectAll = modal.querySelector('input[data-config-select-all="true"]');
    assert(selectAll && selectAll.checked, "Expected Select all to default to checked.");
    const exportButton = Array.from(modal.querySelectorAll("button"))
      .find((button) => String(button.textContent || "").trim() === "Export selected");
    assert(exportButton, "Expected export modal to expose Export selected.");
    exportButton.click();
    await waitForViewerUi(window);
  } finally {
    window.Blob = OriginalBlob;
    window.HTMLAnchorElement.prototype.click = originalAnchorClick;
  }

  assert.strictEqual(downloadedName, "abap-viewer-config.json");
  const downloadedBundle = JSON.parse(downloadedText);
  assert.deepStrictEqual(Array.from(Object.keys(downloadedBundle.sections)), VIEWER_CONFIG_SECTION_KEYS);
  assert(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/.test(downloadedBundle.exportedAt));

  dom.window.close();
}

async function assertGroupedConfigRoundTripsStateStorageAndDom() {
  const dom = await renderFixture("DATA lv_roundtrip TYPE string.\nlv_roundtrip = 'A'.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { api, els, state } = runtime;

  state.templateConfig.templates.DEFAULT["Z97"] = { text: "ROUNDTRIP_TEMPLATE" };
  state.settings = {
    normalizeDeclDesc: false,
    declFilterTypes: ["DATA"],
    structDescTemplate: "{{struct}} / {{item}}",
    nameTemplatesByCode: cloneTestJson(state.settings.nameTemplatesByCode)
  };
  state.settings.nameTemplatesByCode.DS = "ROUNDTRIP[{{desc}}]";
  state.descOverrides = { "roundtrip-key": { text: "Round trip override", noNormalize: true } };
  state.theme = "light";
  state.layoutLeftPane = 61;
  state.templateGuiHiddenTypes = new window.Set(["DATA"]);
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.formEditorPct, "64");

  const exported = api.buildViewerConfigBundle(VIEWER_CONFIG_SECTION_KEYS, "2026-07-15T04:05:06.000Z");

  state.templateConfig = cloneTestJson(state.templateConfig);
  delete state.templateConfig.templates.DEFAULT.Z97;
  state.settings = {
    normalizeDeclDesc: true,
    declFilterTypes: ["TYPES"],
    structDescTemplate: "changed",
    nameTemplatesByCode: cloneTestJson(state.settings.nameTemplatesByCode)
  };
  state.descOverrides = { changed: "changed" };
  state.descOverridesLegacy = { "roundtrip-key": "Stale legacy override" };
  state.templateGuiHiddenTypes = new window.Set();
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.templates, JSON.stringify(state.templateConfig));
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionSettings, JSON.stringify(state.settings));
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionOverrides, JSON.stringify(state.descOverrides));
  window.localStorage.setItem(
    VIEWER_CONFIG_STORAGE_KEYS.legacyDescriptionOverrides,
    JSON.stringify(state.descOverridesLegacy)
  );
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.hiddenObjectTypes, "[]");
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.formEditorPct, "35");
  window.AbapViewerRuntime.services.runtimeState.applyTheme("dark");
  window.AbapViewerRuntime.services.runtimeState.applyLayoutSplit(35);

  let confirmationText = "";
  window.confirm = (message) => {
    confirmationText = String(message || "");
    return true;
  };
  const groupedConfigFile = {
    name: "abap-viewer-config.json",
    async text() {
      return JSON.stringify(exported);
    }
  };
  Object.defineProperty(els.templateImportInput, "files", {
    configurable: true,
    value: [groupedConfigFile]
  });
  els.templateImportInput.dispatchEvent(new window.Event("change", { bubbles: true }));
  await waitForViewerUi(window);
  await waitForViewerUi(window);
  for (const label of ["Templates", "Description settings", "Description overrides", "Appearance", "Template UI"]) {
    assert(confirmationText.includes(label), `Expected import confirmation to list ${label}.`);
  }

  assert.strictEqual(state.templateConfig.templates.DEFAULT.Z97.text, "ROUNDTRIP_TEMPLATE");
  assert.strictEqual(state.settings.normalizeDeclDesc, false);
  assert.deepStrictEqual(Array.from(state.settings.declFilterTypes), ["DATA"]);
  assert.strictEqual(state.settings.nameTemplatesByCode.DS, "ROUNDTRIP[{{desc}}]");
  assert.deepStrictEqual(cloneTestJson(state.descOverrides), cloneTestJson(exported.sections.descriptionOverrides));
  assert.deepStrictEqual(cloneTestJson(state.descOverridesLegacy), {});
  assert.strictEqual(state.theme, "light");
  assert.strictEqual(state.layoutLeftPane, 61);
  assert.deepStrictEqual(Array.from(state.templateGuiHiddenTypes.values()), ["DATA"]);
  assert.strictEqual(window.document.documentElement.getAttribute("data-theme"), "light");
  assert.strictEqual(window.document.documentElement.style.getPropertyValue("--layout-left-pane"), "61%");
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.theme), "light");
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.layout), "61");
  assert.deepStrictEqual(
    JSON.parse(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionOverrides)),
    cloneTestJson(exported.sections.descriptionOverrides)
  );
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.legacyDescriptionOverrides), null);
  assert.deepStrictEqual(
    JSON.parse(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.hiddenObjectTypes)),
    ["DATA"]
  );
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.formEditorPct), "64");

  els.settingsBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(els.settingsNormalizeDesc.checked, false);
  assert.strictEqual(els.settingsStructTemplate.value, "{{struct}} / {{item}}");
  els.settingsCloseBtn.click();

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(
    els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="DATA"]'),
    null,
    "Expected imported Template UI filters to rerender immediately."
  );

  els.templateApplyBtn.click();
  await waitForViewerUi(window);
  const editorPane = window.document.querySelector(".template-config-editor-pane");
  assert(editorPane, "Expected Template Form to open after import.");
  assert.strictEqual(editorPane.style.flex, "0 0 64%");
  const backButton = Array.from(window.document.querySelectorAll(".template-dynamic-page button"))
    .find((button) => String(button.textContent || "").trim() === "Back");
  if (backButton) {
    backButton.click();
  }

  const preservedTemplate = cloneTestJson(state.templateConfig);
  const preservedSettings = cloneTestJson(state.settings);
  const preservedOverrides = cloneTestJson(state.descOverrides);
  window.AbapViewerRuntime.services.runtimeState.applyTheme("dark");
  window.AbapViewerRuntime.services.runtimeState.applyLayoutSplit(42);
  const appearanceOnly = {
    kind: "abap-viewer-config",
    version: 1,
    exportedAt: "2026-07-15T05:06:07.000Z",
    sections: {
      appearance: { theme: "light", layoutLeftPane: 58 }
    }
  };
  assert.strictEqual(api.importViewerConfigObject(appearanceOnly), true);
  assert.deepStrictEqual(cloneTestJson(state.templateConfig), preservedTemplate);
  assert.deepStrictEqual(cloneTestJson(state.settings), preservedSettings);
  assert.deepStrictEqual(cloneTestJson(state.descOverrides), preservedOverrides);
  assert.strictEqual(state.theme, "light");
  assert.strictEqual(state.layoutLeftPane, 58);

  dom.window.close();
}

async function assertGroupedConfigImportValidatesAndRollsBack() {
  const dom = await renderFixture("DATA lv_atomic TYPE string.\nlv_atomic = 'A'.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { api, els, state } = runtime;
  const originalTemplate = cloneTestJson(state.templateConfig);
  const originalSettings = cloneTestJson(state.settings);
  const originalTemplateStorage = JSON.stringify(originalTemplate);
  const originalSettingsStorage = JSON.stringify(originalSettings);
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.templates, originalTemplateStorage);
  window.localStorage.setItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionSettings, originalSettingsStorage);

  let confirmCalls = 0;
  window.confirm = () => {
    confirmCalls += 1;
    return true;
  };
  const invalidBundle = {
    kind: "abap-viewer-config",
    version: 1,
    exportedAt: "2026-07-15T06:07:08.000Z",
    sections: {
      templates: cloneTestJson(originalTemplate),
      descriptionSettings: { normalizeDeclDesc: "not-a-boolean" }
    }
  };
  invalidBundle.sections.templates.templates.DEFAULT.Z96 = { text: "MUST_NOT_APPLY" };
  assert.strictEqual(api.importViewerConfigObject(invalidBundle), false);
  assert.strictEqual(confirmCalls, 0, "Expected validation to finish before confirmation or mutation.");
  assert.deepStrictEqual(cloneTestJson(state.templateConfig), originalTemplate);
  assert.deepStrictEqual(cloneTestJson(state.settings), originalSettings);
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.templates), originalTemplateStorage);
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionSettings), originalSettingsStorage);

  const unknownWithAppearance = {
    kind: "abap-viewer-config",
    version: 1,
    exportedAt: "2026-07-15T06:07:08.000Z",
    sections: {
      futureSection: { enabled: true },
      appearance: { theme: "light", layoutLeftPane: 57 }
    }
  };
  assert.strictEqual(api.importViewerConfigObject(unknownWithAppearance), true);
  assert(
    String(els.templateConfigError.textContent || "").includes("futureSection"),
    "Expected unknown imported sections to remain visible as a warning."
  );

  const unknownOnly = {
    kind: "abap-viewer-config",
    version: 1,
    exportedAt: "2026-07-15T06:07:08.000Z",
    sections: { futureSection: {} }
  };
  assert.strictEqual(api.importViewerConfigObject(unknownOnly), false);
  assert(
    String(els.templateConfigError.textContent || "").includes("no known"),
    "Expected files without a known section to be rejected visibly."
  );

  const rollbackBundle = {
    kind: "abap-viewer-config",
    version: 1,
    exportedAt: "2026-07-15T06:07:08.000Z",
    sections: {
      templates: cloneTestJson(originalTemplate),
      descriptionSettings: cloneTestJson(originalSettings)
    }
  };
  rollbackBundle.sections.templates.templates.DEFAULT.Z95 = { text: "ROLLBACK_MARKER" };
  rollbackBundle.sections.descriptionSettings.structDescTemplate = "ROLLBACK_SETTINGS_MARKER";

  const storagePrototype = window.Storage.prototype;
  const originalSetItem = storagePrototype.setItem;
  let injectedFailure = false;
  storagePrototype.setItem = function failSettingsWriteOnce(key, value) {
    if (!injectedFailure && key === VIEWER_CONFIG_STORAGE_KEYS.descriptionSettings) {
      injectedFailure = true;
      throw new Error("injected storage failure");
    }
    return originalSetItem.call(this, key, value);
  };
  try {
    assert.strictEqual(api.importViewerConfigObject(rollbackBundle), false);
  } finally {
    storagePrototype.setItem = originalSetItem;
  }

  assert.strictEqual(injectedFailure, true, "Expected the rollback test to reach the injected write failure.");
  assert.deepStrictEqual(cloneTestJson(state.templateConfig), originalTemplate);
  assert.deepStrictEqual(cloneTestJson(state.settings), originalSettings);
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.templates), originalTemplateStorage);
  assert.strictEqual(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionSettings), originalSettingsStorage);
  assert(
    String(els.templateConfigError.textContent || "").includes("rolled back"),
    "Expected failed imports to report their rollback."
  );

  const originalOverrides = { "rollback-v2": "Original v2" };
  const originalLegacyOverrides = { "rollback-v1": "Original legacy" };
  state.descOverrides = cloneTestJson(originalOverrides);
  state.descOverridesLegacy = cloneTestJson(originalLegacyOverrides);
  window.localStorage.setItem(
    VIEWER_CONFIG_STORAGE_KEYS.descriptionOverrides,
    JSON.stringify(originalOverrides)
  );
  window.localStorage.setItem(
    VIEWER_CONFIG_STORAGE_KEYS.legacyDescriptionOverrides,
    JSON.stringify(originalLegacyOverrides)
  );
  const uiNavigation = window.AbapViewerRuntime && window.AbapViewerRuntime.services
    ? window.AbapViewerRuntime.services.uiNavigation
    : null;
  const originalRenderActiveRightPanel = uiNavigation && typeof uiNavigation.renderActiveRightPanel === "function"
    ? uiNavigation.renderActiveRightPanel
    : null;
  let renderFailureInjected = false;
  uiNavigation.renderActiveRightPanel = function failFirstImportedOverrideRender() {
    if (!renderFailureInjected) {
      renderFailureInjected = true;
      throw new Error("injected render failure");
    }
    return originalRenderActiveRightPanel.apply(this, arguments);
  };
  try {
    assert.strictEqual(api.importViewerConfigObject({
      kind: "abap-viewer-config",
      version: 1,
      exportedAt: "2026-07-15T06:07:09.000Z",
      sections: { descriptionOverrides: { replacement: "Replacement" } }
    }), false);
  } finally {
    uiNavigation.renderActiveRightPanel = originalRenderActiveRightPanel;
  }
  assert.strictEqual(renderFailureInjected, true);
  assert.deepStrictEqual(cloneTestJson(state.descOverrides), originalOverrides);
  assert.deepStrictEqual(cloneTestJson(state.descOverridesLegacy), originalLegacyOverrides);
  assert.deepStrictEqual(
    JSON.parse(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.descriptionOverrides)),
    originalOverrides
  );
  assert.deepStrictEqual(
    JSON.parse(window.localStorage.getItem(VIEWER_CONFIG_STORAGE_KEYS.legacyDescriptionOverrides)),
    originalLegacyOverrides
  );

  dom.window.close();
}

async function assertTemplateResetCanBeCancelled() {
  const dom = await renderFixture("REPORT z_template_reset.\nDATA gv_value TYPE i.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  let confirmCalls = 0;

  state.templateConfig.templates.DEFAULT["Z99"] = { text: "Keep my custom template" };
  window.confirm = () => {
    confirmCalls += 1;
    return false;
  };

  els.templateResetBtn.click();
  await waitForViewerUi(window);

  assert.strictEqual(confirmCalls, 1, "Expected Reset default to ask for confirmation.");
  assert(
    state.templateConfig.templates.DEFAULT["Z99"],
    "Expected cancelling Reset default to preserve the current template config."
  );

  dom.window.close();
}

async function assertStatementSpecificTwentyCellTemplates() {
  const source = [
    "TYPES ty_text TYPE string.",
    "DATA lv_a TYPE string.",
    "DATA lv_b TYPE string.",
    "DATA lt_rows TYPE TABLE OF string.",
    "DATA ls_row TYPE string.",
    "CONSTANTS gc_flag TYPE abap_bool VALUE abap_true.",
    "PARAMETERS p_user TYPE syuname.",
    "SELECT-OPTIONS s_user FOR sy-uname.",
    "FIELD-SYMBOLS <ls_any> TYPE any.",
    "lv_a = lv_b.",
    "CLEAR lv_a.",
    "APPEND ls_row TO lt_rows.",
    "READ TABLE lt_rows WITH KEY table_line = lv_a INTO ls_row.",
    "MODIFY lt_rows FROM ls_row WHERE table_line = lv_a.",
    "DELETE lt_rows WHERE table_line = lv_a.",
    "SORT lt_rows BY table_line.",
    "MOVE-CORRESPONDING ls_row TO lv_b.",
    "CALL FUNCTION 'Z_DEMO' EXPORTING iv_user = p_user IMPORTING ev_text = lv_b.",
    "PERFORM missing_form USING lv_a.",
    "MESSAGE lv_a TYPE 'I'.",
    "WRITE lv_a.",
    "SELECT * FROM usr02 INTO TABLE lt_rows WHERE bname = p_user.",
    "DO 1 TIMES.",
    "ENDDO.",
    "LOOP AT lt_rows INTO ls_row.",
    "ENDLOOP.",
    "CASE lv_a.",
    "WHEN 'A'.",
    "ENDCASE.",
    "IF lv_a IS INITIAL.",
    "ELSEIF lv_b IS NOT INITIAL.",
    "ELSE.",
    "ENDIF."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  for (const templateKey of STATEMENT_TEMPLATE_KEYS) {
    assert(
      Object.prototype.hasOwnProperty.call(state.templateConfig.templates, templateKey),
      `Expected a dedicated template config for ${templateKey}.`
    );
  }

  const genericKeys = STATEMENT_TEMPLATE_KEYS.filter((key) => !["ASSIGNMENT", "IF", "ELSEIF"].includes(key));
  for (const templateKey of genericKeys) {
    const template = state.templateConfig.templates[templateKey];
    assert(template["A1:T1"], `Expected ${templateKey} keyword frame to span A:T.`);
    assert(template["U1:AN1"], `Expected ${templateKey} description frame to span U:AN.`);
    assert.strictEqual(template.A1.text, "{rows.keyword}");
    assert.strictEqual(template.U1.text, "{rows.finalDesc}");
  }

  const appendLinesOf = state.templateConfig.templates.APPEND_LINES_OF;
  assert(appendLinesOf, "Expected a dedicated APPEND_LINES_OF template config.");
  assert.strictEqual(appendLinesOf.A1.text, "APPEND LINES OF");
  assert.strictEqual(appendLinesOf.U1.text, "{extras.append.source.finalDesc}");
  assert.strictEqual(appendLinesOf.A6.text, "TO");
  assert.strictEqual(appendLinesOf.U6.text, "{extras.append.target.finalDesc}");

  const assignment = state.templateConfig.templates.ASSIGNMENT;
  assert.strictEqual(assignment.A1.text, "Đích");
  assert.strictEqual(assignment.U1.text, "Nguồn");
  assert.strictEqual(assignment.A2.text, "{values.target.finalDesc}");
  assert.strictEqual(assignment.U2.text, "{values.expr.finalDesc}");
  for (const rangeKey of ["A1:T1", "U1:AN1", "A2:T2", "U2:AN2"]) {
    assert(assignment[rangeKey], `Expected ASSIGNMENT range ${rangeKey}.`);
  }

  const CONDITION_WIDTH_TEMPLATE_KEYS = [
    "IF",
    "ELSEIF",
    "SELECT",
    "READ_TABLE",
    "LOOP_AT_ITAB",
    "MODIFY_ITAB",
    "DELETE_ITAB"
  ];

  for (const templateKey of ["IF", "ELSEIF"]) {
    const conditionTemplate = state.templateConfig.templates[templateKey];
    for (const rangeKey of [
      "A1:T1", "U1:AN1", "AO1:BH1", "BI1:CB1",
      "A2:T2", "U2:AN2", "AO2:BH2", "BI2:CB2"
    ]) {
      assert(conditionTemplate[rangeKey], `Expected ${templateKey} range ${rangeKey}.`);
    }
  }

  for (const templateKey of ["SELECT", "READ_TABLE", "LOOP_AT_ITAB", "MODIFY_ITAB", "DELETE_ITAB"]) {
    const hybrid = state.templateConfig.templates[templateKey];
    assert.strictEqual(hybrid.A1.text, "{rows.keyword}");
    assert.strictEqual(hybrid.U1.text, "{rows.finalDesc}");
    assert(hybrid.A2 || hybrid.A3, `Expected ${templateKey} condition block rows.`);
  }

  Object.defineProperty(els.templatePreviewOutput, "clientHeight", {
    configurable: true,
    get() {
      return 100000;
    }
  });
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  for (const templateKey of STATEMENT_TEMPLATE_KEYS) {
    const table = els.templatePreviewOutput.querySelector(`.template-preview-table[data-object-type="${templateKey}"]`);
    assert(table, `Expected a rendered template table for ${templateKey}.`);
    assert.strictEqual(
      table.getAttribute("data-template-key"),
      templateKey,
      `Expected ${templateKey} to render with its dedicated config instead of DEFAULT.`
    );
    const expectedCellCount = CONDITION_WIDTH_TEMPLATE_KEYS.includes(templateKey) ? 80 : 40;
    for (const row of Array.from(table.querySelectorAll("tr"))) {
      assert.strictEqual(row.querySelectorAll("td").length, expectedCellCount, `${templateKey} row width mismatch.`);
    }
  }

  const readTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert.deepStrictEqual(getTemplateTableRows(readTable), [
    ["READ TABLE", "lt_rows"],
    ["INTO", "ls_row"],
    ["WITH KEY", "="],
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "="],
    ["table_line", "=", "lv_a"]
  ]);

  const appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert.deepStrictEqual(getTemplateTableRows(appendTable), [
    ["APPEND", "ls_row"],
    ["TO", "lt_rows"]
  ]);

  const moveTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="MOVE-CORRESPONDING"]');
  assert.deepStrictEqual(getTemplateTableRows(moveTable), [
    ["MOVE-CORRESPONDING", "ls_row"],
    ["TO", "lv_b"]
  ]);

  const callFunctionTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="CALL_FUNCTION"]');
  assert.deepStrictEqual(getTemplateTableRows(callFunctionTable), [
    ["CALL FUNCTION", "'Z_DEMO'"],
    ["EXPORTING", "iv_user = p_user"],
    ["IMPORTING", "ev_text = lv_b"]
  ]);

  const loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert.deepStrictEqual(getTemplateTableRows(loopTable), [
    ["LOOP AT", "lt_rows"],
    ["INTO", "ls_row"]
  ]);

  const selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert.deepStrictEqual(getTemplateTableRows(selectTable), [
    ["SELECT", "*"],
    ["FROM", "usr02"],
    ["INTO TABLE", "lt_rows"],
    ["WHERE", "="],
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "="],
    ["bname", "=", "p_user"]
  ]);

  const elseTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="ELSE"]');
  const elseRow = elseTable.querySelector("tr");
  assert.strictEqual(elseRow.querySelectorAll("td").length, 40);
  assert.strictEqual(elseRow.querySelectorAll("td")[0].textContent.trim(), "ELSE");
  assert.strictEqual(elseRow.querySelectorAll("td")[20].textContent.trim(), "");

  dom.window.close();
}

async function assertLegacyTemplateImportAddsMissingSpecificConfigs() {
  const dom = await renderFixture("DATA lv_a TYPE string.\nlv_a = 'A'.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const legacyConfig = {
    version: 1,
    templates: {
      DEFAULT: {
        _options: { hideEmptyRows: true },
        "A1:T1": { text: "Custom default" }
      },
      ASSIGNMENT: {
        _options: { hideEmptyRows: true },
        "A1:T1": { text: "Custom assignment" }
      },
      APPEND: {
        _options: { hideEmptyRows: true },
        "A1:T1": { text: "Custom append" },
        "U1:AN1": { text: "{rows.finalDesc}" }
      }
    }
  };
  const legacyFile = {
    name: "legacy-template.json",
    async text() {
      return JSON.stringify(legacyConfig);
    }
  };
  Object.defineProperty(els.templateImportInput, "files", {
    configurable: true,
    value: [legacyFile]
  });
  els.templateImportInput.dispatchEvent(new window.Event("change", { bubbles: true }));
  await waitForViewerUi(window);
  await waitForViewerUi(window);

  assert.strictEqual(state.templateConfig.version, 1);
  assert.strictEqual(state.templateConfig.templates.DEFAULT["A1:T1"].text, "Custom default");
  assert.strictEqual(state.templateConfig.templates.ASSIGNMENT["A1:T1"].text, "Custom assignment");
  assert.deepStrictEqual(
    state.templateConfig.templates.APPEND_LINES_OF,
    state.templateConfig.templates.APPEND,
    "Expected a legacy custom APPEND template to clone into APPEND_LINES_OF."
  );
  for (const templateKey of STATEMENT_TEMPLATE_KEYS) {
    assert(
      Object.prototype.hasOwnProperty.call(state.templateConfig.templates, templateKey),
      `Expected legacy import migration to add ${templateKey}.`
    );
  }

  dom.window.close();
}

defineFocusedTest(test, "viewer config export contracts", ["config-export"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("grouped config export is deterministic", async () => {
    await assertGroupedConfigExportIsDeterministic();
  });

  await t.test("grouped config round trips state storage and dom", async () => {
    await assertGroupedConfigRoundTripsStateStorageAndDom();
  });

  await t.test("grouped config import validates and rolls back", async () => {
    await assertGroupedConfigImportValidatesAndRollsBack();
  });
});

defineFocusedTest(test, "viewer template reset contract", ["template-reset"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template reset can be cancelled", async () => {
    await assertTemplateResetCanBeCancelled();
  });
});

defineFocusedTest(test, "viewer template config contracts", ["template-configs"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("statement specific twenty cell templates", async () => {
    await assertStatementSpecificTwentyCellTemplates();
  });

  await t.test("legacy template import adds missing specific configs", async () => {
    await assertLegacyTemplateImportAddsMissingSpecificConfigs();
  });
});
