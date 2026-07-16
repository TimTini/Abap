"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const {
  diffJson,
  filterDiffsByAllowedPaths,
  listFixtureFiles,
  normalizeViewerState,
  readJson
} = require("./helpers/contracts");
const { renderFixture } = require("./helpers/viewer-harness");

const fixturesDir = path.resolve(__dirname, "fixtures", "viewer");
const baselineDir = path.resolve(__dirname, "baselines", "viewer");
const allowedDir = path.resolve(__dirname, "allowed-deltas", "viewer");

const STATEMENT_TEMPLATE_KEYS = [
  "APPEND",
  "ASSIGNMENT",
  "CALL_FUNCTION",
  "CASE",
  "CLEAR",
  "CONSTANTS",
  "DATA",
  "DELETE_ITAB",
  "DO",
  "ELSE",
  "ELSEIF",
  "FIELD-SYMBOLS",
  "IF",
  "LOOP_AT_ITAB",
  "MESSAGE",
  "MODIFY_ITAB",
  "MOVE-CORRESPONDING",
  "PARAMETERS",
  "PERFORM",
  "READ_TABLE",
  "SELECT",
  "SELECT-OPTIONS",
  "SORT_ITAB",
  "TYPES",
  "WHEN",
  "WRITE"
];

const VIEWER_CONFIG_SECTION_KEYS = [
  "templates",
  "descriptionSettings",
  "descriptionOverrides",
  "appearance",
  "templateUi"
];

const VIEWER_CONFIG_STORAGE_KEYS = {
  templates: "abap-parser-viewer.templateConfig.v1",
  descriptionSettings: "abap-parser-viewer.settings.v1",
  descriptionOverrides: "abap-parser-viewer.declDescOverrides.v2",
  legacyDescriptionOverrides: "abap-parser-viewer.descOverrides.v1",
  theme: "abap-parser-viewer.theme.v1",
  layout: "abap-parser-viewer.layoutSplit.v1",
  hiddenObjectTypes: "abap-parser-viewer.templateGuiHiddenObjectTypes.v1",
  formEditorPct: "abap-parser-viewer.templateFormEditorPct.v1"
};

function cloneTestJson(value) {
  return JSON.parse(JSON.stringify(value));
}

function findVisibleConfigExportModal(window) {
  return Array.from(window.document.querySelectorAll(".modal"))
    .find((modal) => !modal.hidden && String(modal.textContent || "").includes("Export Viewer Config"));
}

function findVisibleTemplateEditModal(window) {
  return Array.from(window.document.querySelectorAll(".modal"))
    .find((modal) => !modal.hidden && String(modal.textContent || "").includes("Edit Template Cell"));
}

async function openTemplateCellDescriptionTab(window, cell) {
  assert(cell, "Expected a rendered Template cell to edit.");
  cell.dispatchEvent(new window.MouseEvent("dblclick", { bubbles: true }));
  await waitForViewerUi(window);

  const modal = findVisibleTemplateEditModal(window);
  assert(modal, "Expected double-click to open the Template cell editor.");
  const descriptionTab = Array.from(modal.querySelectorAll("button"))
    .find((button) => String(button.textContent || "").trim() === "Description");
  assert(descriptionTab, "Expected the Template cell editor to expose the Description tab.");
  descriptionTab.click();
  await waitForViewerUi(window);
  return modal;
}

async function saveTemplateCellDescription(window, modal, nextDescription) {
  const textarea = modal.querySelector("textarea.template-config-json");
  assert(textarea && !textarea.disabled, "Expected an editable Description textarea.");
  textarea.value = String(nextDescription || "");
  textarea.dispatchEvent(new window.Event("input", { bubbles: true }));

  const saveButton = Array.from(modal.querySelectorAll("button"))
    .find((button) => String(button.textContent || "").trim() === "Save");
  assert(saveButton, "Expected the Template cell editor to expose Save.");
  saveButton.click();
  await waitForViewerUi(window);
}

function findTemplateCellByText(table, expectedText) {
  return Array.from(table ? table.querySelectorAll("td") : [])
    .find((cell) => String(cell.textContent || "").trim() === expectedText);
}

function findVisibleTemplateConfigPage(window) {
  return Array.from(window.document.querySelectorAll(".template-dynamic-page, .modal"))
    .find((root) => {
      if (!root.isConnected || root.hidden) {
        return false;
      }
      return String(root.textContent || "").includes("Template Form");
    });
}

function findTemplateConfigRow(modal, labelText) {
  return Array.from(modal.querySelectorAll("tr"))
    .find((row) => String(row.textContent || "").includes(labelText));
}

async function waitForViewerUi(window) {
  await new Promise((resolve) => window.setTimeout(resolve, 0));
}

async function settleViewerUi(window, ticks = 6) {
  for (let index = 0; index < ticks; index += 1) {
    await waitForViewerUi(window);
  }
}

function createLayoutRect(top, height, width = 640) {
  return {
    x: 0,
    y: top,
    top,
    right: width,
    bottom: top + height,
    left: 0,
    width,
    height,
    toJSON() {
      return {};
    }
  };
}

function installVirtualLayoutMock(window, container, kind, itemCount, getItemHeight, options) {
  const prototype = window.HTMLElement.prototype;
  const originalGetBoundingClientRect = prototype.getBoundingClientRect;
  const isTemplate = kind === "template";
  const rootSelector = isTemplate
    ? ".template-block[data-template-index]"
    : "[data-virtual-item-index]";
  const topSpacerSelector = isTemplate
    ? ".template-virtual-spacer-top"
    : ".output-virtual-spacer-top";
  const bottomSpacerSelector = isTemplate
    ? ".template-virtual-spacer-bottom"
    : ".output-virtual-spacer-bottom";
  const containerTop = 100;
  const clientHeight = 800;
  const opts = options && typeof options === "object" ? options : {};
  const visualScale = Math.max(0.5, Math.min(2, Number(opts.visualScale) || 1));
  const outerMargin = isTemplate ? 10 : 12;
  const outerHeightAt = (index) => Math.max(1, Number(getItemHeight(index)) || 1) + outerMargin;

  Object.defineProperty(container, "clientHeight", {
    configurable: true,
    get() {
      return clientHeight;
    }
  });
  Object.defineProperty(container, "offsetHeight", {
    configurable: true,
    get() {
      return clientHeight;
    }
  });
  Object.defineProperty(container, "scrollHeight", {
    configurable: true,
    get() {
      if (opts.scrollHeightMode === "virtual-dom") {
        const topSpacer = container.querySelector(topSpacerSelector);
        const bottomSpacer = container.querySelector(bottomSpacerSelector);
        const topHeight = Number.parseFloat(topSpacer && topSpacer.style.height ? topSpacer.style.height : "0") || 0;
        const bottomHeight = Number.parseFloat(bottomSpacer && bottomSpacer.style.height ? bottomSpacer.style.height : "0") || 0;
        const renderedHeight = Array.from(container.querySelectorAll(rootSelector)).reduce((sum, node) => {
          const index = Number(node.getAttribute(isTemplate ? "data-template-index" : "data-virtual-item-index"));
          return sum + outerHeightAt(index);
        }, 0);
        return topHeight + renderedHeight + bottomHeight;
      }
      let total = 0;
      for (let index = 0; index < itemCount; index += 1) {
        total += outerHeightAt(index);
      }
      return total;
    }
  });
  container.scrollTo = ({ top }) => {
    const requestedTop = Number(top) || 0;
    const convergenceFactor = Math.max(0.05, Math.min(1, Number(opts.scrollConvergenceFactor) || 1));
    container.scrollTop = (Number(container.scrollTop) || 0)
      + ((requestedTop - (Number(container.scrollTop) || 0)) * convergenceFactor);
    if (opts.scrollEventViaRaf) {
      window.requestAnimationFrame(() => container.dispatchEvent(new window.Event("scroll")));
    } else {
      window.setTimeout(() => container.dispatchEvent(new window.Event("scroll")), 0);
    }
  };

  prototype.getBoundingClientRect = function getMockedBoundingClientRect() {
    if (this === container) {
      return createLayoutRect(containerTop, clientHeight * visualScale);
    }
    if (!this.isConnected || !container.contains(this)) {
      return originalGetBoundingClientRect.call(this);
    }

    const root = typeof this.closest === "function" ? this.closest(rootSelector) : null;
    if (!root || !container.contains(root)) {
      return originalGetBoundingClientRect.call(this);
    }

    const renderedRoots = Array.from(container.querySelectorAll(rootSelector));
    const rootPosition = renderedRoots.indexOf(root);
    const itemIndex = Number(root.getAttribute(isTemplate ? "data-template-index" : "data-virtual-item-index"));
    const topSpacer = container.querySelector(topSpacerSelector);
    let top = containerTop + (
      (Number.parseFloat(topSpacer && topSpacer.style.height ? topSpacer.style.height : "0") || 0)
      - (Number(container.scrollTop) || 0)
    ) * visualScale;
    for (let position = 0; position < rootPosition; position += 1) {
      const previousIndex = Number(renderedRoots[position].getAttribute(
        isTemplate ? "data-template-index" : "data-virtual-item-index"
      ));
      top += outerHeightAt(previousIndex) * visualScale;
    }

    let innerOffset = 0;
    let height = Math.max(1, Number(getItemHeight(itemIndex)) || 1);
    if (this !== root) {
      height = 18;
      if (this.matches && this.matches("td.template-preview-editable")) {
        innerOffset = 42;
      }
    }
    return createLayoutRect(top + (innerOffset * visualScale), height * visualScale);
  };

  return () => {
    prototype.getBoundingClientRect = originalGetBoundingClientRect;
  };
}

async function assertFailedRenderClearsPreviousResult() {
  const dom = await renderFixture([
    "REPORT z_render_error.",
    "DATA gv_total TYPE i.",
    "gv_total = gv_total + 1."
  ].join("\n"));
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  assert(
    String(els.templatePreviewOutput.textContent || "").includes("gv_total"),
    "Expected a successful render before testing the failure path."
  );

  els.inputText.value = "{bad";
  els.inputText.dispatchEvent(new window.Event("input", { bubbles: true }));
  els.parseBtn.click();
  await waitForViewerUi(window);

  assert.strictEqual(state.data, null, "Expected failed Render to discard previously parsed data.");
  assert.deepStrictEqual(Array.from(state.renderObjects || []), [], "Expected failed Render to discard render objects.");
  assert.strictEqual(
    String(els.templatePreviewOutput.textContent || "").trim(),
    "No data loaded.",
    "Expected the visible Template tab to clear stale output after a failed Render."
  );
  assert(
    String(els.error.textContent || "").includes("JSON parse error"),
    "Expected failed Render to keep a useful error message visible."
  );

  els.rightTabOutputBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(
    String(els.output.textContent || "").trim(),
    "No data loaded.",
    "Expected switching tabs after a failed Render not to restore stale output."
  );
  assert(
    String(els.error.textContent || "").includes("JSON parse error"),
    "Expected switching tabs not to erase the Render error."
  );

  dom.window.close();
}

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
  window.applyTheme("dark");
  window.applyLayoutSplit(35);

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
  window.applyTheme("dark");
  window.applyLayoutSplit(42);
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
  const originalRenderOutput = window.renderOutput;
  let renderFailureInjected = false;
  window.renderOutput = function failFirstImportedOverrideRender() {
    if (!renderFailureInjected) {
      renderFailureInjected = true;
      throw new Error("injected render failure");
    }
    return originalRenderOutput.apply(this, arguments);
  };
  try {
    assert.strictEqual(api.importViewerConfigObject({
      kind: "abap-viewer-config",
      version: 1,
      exportedAt: "2026-07-15T06:07:09.000Z",
      sections: { descriptionOverrides: { replacement: "Replacement" } }
    }), false);
  } finally {
    window.renderOutput = originalRenderOutput;
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

async function assertTemplateImportErrorIsVisible() {
  const dom = await renderFixture("REPORT z_template_import.\nDATA gv_value TYPE i.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els } = runtime;

  assert(els.templateConfigError, "Expected the Template panel to expose a permanent error area.");

  const invalidFile = {
    name: "invalid-template.json",
    async text() {
      return "{bad";
    }
  };
  Object.defineProperty(els.templateImportInput, "files", {
    configurable: true,
    value: [invalidFile]
  });
  els.templateImportInput.dispatchEvent(new window.Event("change", { bubbles: true }));
  await waitForViewerUi(window);
  await waitForViewerUi(window);

  assert(
    String(els.templateConfigError.textContent || "").includes("Import JSON parse error"),
    "Expected invalid Template import to explain why it failed."
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

async function assertIfInitialUsesConditionTemplate() {
  const dom = await renderFixture([
    "SELECT-OPTIONS so_date FOR sy-datum.",
    "IF so_date[] IS INITIAL.",
    "ENDIF."
  ].join("\n"));
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  assert(
    state.templateConfig.templates.IF,
    "Expected IF to use its condition-specific template instead of the generic keyword template."
  );

  delete state.templateConfig.templates.IF;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const ifTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  assert(ifTable, "Expected the IF template preview table to render.");

  const nonEmptyCells = Array.from(ifTable.querySelectorAll("td"))
    .map((cell) => String(cell.textContent || "").trim())
    .filter(Boolean);
  assert.deepStrictEqual(
    nonEmptyCells,
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "Kết nối", "so_date", "IS", "INITIAL"],
    "Expected the rendered description to omit the empty table-body suffix."
  );

  dom.window.close();

  const multiDom = await renderFixture([
    "DATA lv_a TYPE string.",
    "DATA lv_b TYPE string.",
    "IF lv_a IS INITIAL OR lv_b IS NOT INITIAL.",
    "ENDIF."
  ].join("\n"));
  const multiWindow = multiDom.window;
  const multiEls = multiWindow.AbapViewerRuntime.els;
  multiEls.rightTabTemplateBtn.click();
  await waitForViewerUi(multiWindow);

  const multiIfTable = multiEls.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  assert(multiIfTable, "Expected the multi-clause IF template preview table to render.");
  const rowValues = Array.from(multiIfTable.querySelectorAll("tr")).map((row) => (
    Array.from(row.querySelectorAll("td"))
      .map((cell) => String(cell.textContent || "").trim())
      .filter(Boolean)
  ));
  assert.deepStrictEqual(
    rowValues,
    [
      ["Điều kiện trái", "Toán tử", "Điều kiện phải", "Kết nối"],
      ["lv_a", "IS", "INITIAL", "OR"],
      ["lv_b", "IS", "NOT INITIAL"]
    ],
    "Expected each IF clause to stay on its own row without repeating the previous connector."
  );

  multiDom.window.close();
}

function getTemplateTableRows(table) {
  return Array.from(table.querySelectorAll("tr")).map((row) => (
    Array.from(row.querySelectorAll("td"))
      .map((cell) => String(cell.textContent || "").trim())
      .filter(Boolean)
  ));
}

async function assertPerformAndCallMultiValueRows() {
  const source = [
    "DATA iv_carrid TYPE string.",
    "DATA iv_connid TYPE string.",
    "DATA lv_next TYPE string.",
    "DATA lv_occ_local TYPE string.",
    "DATA lv_free TYPE string.",
    "DATA cv_found TYPE string.",
    "DATA cv_text TYPE string.",
    "PERFORM frm_deep_chain_lvl04",
    "  USING    iv_carrid",
    "           iv_connid",
    "           lv_next",
    "           lv_occ_local",
    "           lv_free",
    "  CHANGING cv_found",
    "           cv_text.",
    "CALL FUNCTION 'Z_MULTI_VALUE'",
    "  EXPORTING iv_carrid = iv_carrid",
    "            iv_connid = iv_connid",
    "            iv_limit = 5",
    "            iv_calc = lv_next + 1",
    "  IMPORTING ev_text = cv_text.",
    "CALL METHOD lo_demo->run",
    "  EXPORTING iv_next = lv_next",
    "            iv_free = lv_free",
    "  CHANGING  cv_text = cv_text.",
    "METHODS do_work",
    "  IMPORTING iv_first TYPE string",
    "            iv_second TYPE string",
    "  CHANGING  cv_first TYPE string",
    "            cv_second TYPE string."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const performTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="PERFORM"]');
  assert(performTable, "Expected PERFORM template table.");
  assert.deepStrictEqual(getTemplateTableRows(performTable), [
    ["PERFORM", "frm_deep_chain_lvl04"],
    ["USING", "iv_carrid"],
    ["USING", "iv_connid"],
    ["USING", "lv_next"],
    ["USING", "lv_occ_local"],
    ["USING", "lv_free"],
    ["CHANGING", "cv_found"],
    ["CHANGING", "cv_text"]
  ]);
  for (const row of Array.from(performTable.querySelectorAll("tr"))) {
    assert.strictEqual(row.querySelectorAll("td").length, 40, "Expected every expanded PERFORM row to keep 20 + 20 cells.");
  }

  const callFunctionTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="CALL_FUNCTION"]');
  assert(callFunctionTable, "Expected CALL FUNCTION template table.");
  assert.deepStrictEqual(getTemplateTableRows(callFunctionTable), [
    ["CALL FUNCTION", "'Z_MULTI_VALUE'"],
    ["EXPORTING", "iv_carrid = iv_carrid"],
    ["EXPORTING", "iv_connid = iv_connid"],
    ["EXPORTING", "iv_limit = 5"],
    ["EXPORTING", "iv_calc = lv_next + 1"],
    ["IMPORTING", "ev_text = cv_text"]
  ]);

  const callMethodTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="CALL_METHOD"]');
  assert(callMethodTable, "Expected CALL METHOD template table.");
  assert.deepStrictEqual(getTemplateTableRows(callMethodTable), [
    ["CALL METHOD", "lo_demo->run"],
    ["EXPORTING", "iv_next = lv_next"],
    ["EXPORTING", "iv_free = lv_free"],
    ["CHANGING", "cv_text = cv_text"]
  ]);

  const methodSignatureTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="METHODS"]');
  assert(methodSignatureTable, "Expected METHODS signature template table.");
  assert.deepStrictEqual(getTemplateTableRows(methodSignatureTable), [
    ["METHODS", "do_work"],
    ["IMPORTING", "iv_first"],
    ["IMPORTING", "iv_second"],
    ["CHANGING", "cv_first"],
    ["CHANGING", "cv_second"]
  ]);

  dom.window.close();
}

async function assertExpandedPerformMultiValueRowsUseRootDescriptions() {
  const source = [
    "DATA gv_root_one TYPE string. \"Root one",
    "DATA gv_root_two TYPE string. \"Root two",
    "PERFORM frm_outer USING gv_root_one gv_root_two.",
    "FORM frm_outer USING iv_outer_one TYPE string iv_outer_two TYPE string.",
    "  PERFORM frm_inner USING iv_outer_one iv_outer_two.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner_one TYPE string iv_inner_two TYPE string.",
    "  CLEAR iv_inner_one.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const rootPerform = (state.renderObjects || []).find((obj) => obj && obj.objectType === "PERFORM");
  assert(rootPerform, "Expected root PERFORM object.");
  const nestedPerform = (rootPerform.children || []).find((obj) => (
    obj
    && obj.objectType === "PERFORM"
    && obj.extras
    && obj.extras.performCall
    && obj.extras.performCall.form === "frm_inner"
  ));
  assert(nestedPerform, "Expected nested PERFORM object expanded from frm_outer.");

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const nestedBlock = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
    .find((block) => String(block.querySelector(".template-block-meta")?.textContent || "").includes(`#${nestedPerform.id}`));
  assert(nestedBlock, "Expected Template block for nested PERFORM.");
  assert.deepStrictEqual(getTemplateTableRows(nestedBlock.querySelector("table.template-preview-table")), [
    ["PERFORM", "frm_inner"],
    ["USING", "Root one"],
    ["USING", "Root two"]
  ]);

  dom.window.close();
}

async function assertConditionListsExpandRows() {
  const source = [
    "DATA gv_a TYPE string.",
    "DATA gv_b TYPE string.",
    "DATA lt_rows TYPE TABLE OF string.",
    "DATA ls_row TYPE string.",
    "READ TABLE lt_rows WITH KEY col1 = gv_a col2 = gv_b INTO ls_row.",
    "MODIFY lt_rows FROM ls_row TRANSPORTING col1 col2 WHERE col1 = gv_a AND col2 = gv_b.",
    "SELECT col1, col2 FROM dbtab INTO TABLE @lt_rows WHERE col1 = @gv_a AND col2 = @gv_b HAVING col3 = @gv_a OR col4 = @gv_b."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const readTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert.deepStrictEqual(getTemplateTableRows(readTable), [
    ["READ TABLE", "lt_rows"],
    ["WITH KEY", "col1 = gv_a AND"],
    ["WITH KEY", "col2 = gv_b"],
    ["INTO", "ls_row"]
  ]);

  const modifyTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="MODIFY_ITAB"]');
  assert.deepStrictEqual(getTemplateTableRows(modifyTable), [
    ["MODIFY", "lt_rows"],
    ["FROM", "ls_row"],
    ["TRANSPORTING", "col1"],
    ["TRANSPORTING", "col2"],
    ["WHERE", "col1 = gv_a AND"],
    ["WHERE", "col2 = gv_b"]
  ]);

  const selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert.deepStrictEqual(getTemplateTableRows(selectTable), [
    ["SELECT", "col1"],
    ["SELECT", "col2"],
    ["FROM", "dbtab"],
    ["INTO TABLE", "@lt_rows"],
    ["WHERE", "col1 = @gv_a AND"],
    ["WHERE", "col2 = @gv_b"],
    ["HAVING", "col3 = @gv_a OR"],
    ["HAVING", "col4 = @gv_b"]
  ]);

  dom.window.close();
}

async function assertSafeRawListsExpandWithoutSplittingExpressions() {
  const source = [
    "DATA lt_rows TYPE TABLE OF string.",
    "DATA ls_row TYPE string.",
    "SELECT col1, concat_with_space( col2, col3, 1 ) AS text, col4 FROM dbtab INTO TABLE @lt_rows.",
    "SORT lt_rows BY col1 col2.",
    "MODIFY lt_rows FROM ls_row TRANSPORTING col1 col2."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert.deepStrictEqual(getTemplateTableRows(selectTable), [
    ["SELECT", "col1"],
    ["SELECT", "concat_with_space( col2, col3, 1 ) AS text"],
    ["SELECT", "col4"],
    ["FROM", "dbtab"],
    ["INTO TABLE", "@lt_rows"]
  ]);

  const sortTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SORT_ITAB"]');
  assert.deepStrictEqual(getTemplateTableRows(sortTable), [
    ["SORT", "lt_rows"],
    ["BY", "col1"],
    ["BY", "col2"]
  ]);

  const modifyTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="MODIFY_ITAB"]');
  assert.deepStrictEqual(getTemplateTableRows(modifyTable), [
    ["MODIFY", "lt_rows"],
    ["FROM", "ls_row"],
    ["TRANSPORTING", "col1"],
    ["TRANSPORTING", "col2"]
  ]);

  dom.window.close();
}

async function assertTemplateRowDescriptionEditsLocalLoopDecl() {
  const source = [
    "PERFORM frm_main.",
    "FORM frm_main.",
    "  DATA lt_abc TYPE TABLE OF string. \"Table local",
    "  DATA ls_abc TYPE string. \"Row local",
    "  LOOP AT lt_abc INTO ls_abc.",
    "  ENDLOOP.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  let loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert(loopTable, "Expected the expanded FORM to render its LOOP template.");
  let modal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(loopTable, "Table local"));
  const modalText = String(modal.textContent || "");
  assert(!modalText.includes("Khong tim thay decl cho o nay."), "Expected the LOOP table row to retain its declaration target.");
  assert(modalText.includes("lt_abc"), "Expected the LOOP table row to target lt_abc.");
  assert(!modalText.includes("ls_abc"), "Expected the LOOP table row not to target the INTO declaration.");

  const tableDecl = (state.data && Array.isArray(state.data.decls) ? state.data.decls : [])
    .find((decl) => String(decl && decl.name || "") === "lt_abc");
  assert(tableDecl, "Expected the parser result to contain the local table declaration.");
  const tableDeclKey = window.getDeclOverrideStorageKey(tableDecl);

  await saveTemplateCellDescription(window, modal, "Updated list");
  const storedOverride = state.descOverrides[tableDeclKey];
  assert.strictEqual(
    typeof storedOverride === "object" ? String(storedOverride.text || "") : String(storedOverride || ""),
    "Updated list",
    "Expected Save to persist the exact local table override."
  );
  assert(String(els.output.textContent || "").includes("Updated list"), "Expected Output to refresh after the row description edit.");

  loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert(findTemplateCellByText(loopTable, "Updated list"), "Expected Template Preview to refresh after Save.");
  modal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(loopTable, "Updated list"));
  await saveTemplateCellDescription(window, modal, "");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, tableDeclKey), false, "Expected clearing the description to remove its override.");

  loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert(findTemplateCellByText(loopTable, "Table local"), "Expected clearing the override to restore the code description.");
  const intoModal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(loopTable, "Row local"));
  const intoModalText = String(intoModal.textContent || "");
  assert(intoModalText.includes("ls_abc"), "Expected the INTO row to target ls_abc.");
  assert(!intoModalText.includes("lt_abc"), "Expected the INTO row not to target the table declaration.");

  dom.window.close();
}

async function assertTemplateRowDescriptionKeepsNestedPerformTrace() {
  const source = [
    "DATA gv_root_one TYPE string. \"Root one",
    "DATA gv_root_two TYPE string. \"Root two",
    "PERFORM frm_outer USING gv_root_one gv_root_two.",
    "PERFORM frm_literal USING 'X'.",
    "FORM frm_outer USING iv_outer_one TYPE string iv_outer_two TYPE string.",
    "  PERFORM frm_inner USING iv_outer_one iv_outer_two.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner_one TYPE string iv_inner_two TYPE string.",
    "  CLEAR iv_inner_one.",
    "ENDFORM.",
    "FORM frm_literal USING iv_literal TYPE string.",
    "  CLEAR iv_literal.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const performTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="PERFORM"]'));
  let nestedTable = performTables.find((table) => getTemplateTableRows(table).some((row) => row.includes("frm_inner")));
  assert(nestedTable, "Expected the nested PERFORM template table.");
  const rootTwoCell = findTemplateCellByText(nestedTable, "Root two");
  assert(rootTwoCell, "Expected Root two cell in nested PERFORM.");
  const cellCandidates = rootTwoCell.__templateCellMeta && Array.isArray(rootTwoCell.__templateCellMeta.declCandidates)
    ? rootTwoCell.__templateCellMeta.declCandidates
    : [];
  assert(cellCandidates.length > 1, "Expected the traced PERFORM row to keep the full decl chain in cell meta.");
  assert(
    cellCandidates.some((decl) => String(decl && decl.name || "").toLowerCase() === "gv_root_two"),
    "Expected chain to include gv_root_two."
  );
  assert(
    cellCandidates.some((decl) => String(decl && decl.name || "").toLowerCase() === "iv_outer_two"),
    "Expected chain to include iv_outer_two."
  );
  assert(
    !cellCandidates.some((decl) => {
      const name = String(decl && decl.name || "").toLowerCase();
      return name === "gv_root_one" || name === "iv_outer_one";
    }),
    "Expected the second PERFORM row not to inherit targets from the first row."
  );

  let modal = await openTemplateCellDescriptionTab(window, rootTwoCell);
  assert(
    !modal.querySelector("select"),
    `Expected a single PERFORM call chain not to show Description Target select. Keys: ${cellCandidates.map((decl) => window.getDeclOverrideStorageKey(decl)).join(", ")}`
  );
  assert(String(modal.textContent || "").includes("gv_root_two"), "Expected Description tab to show the root decl label.");

  await saveTemplateCellDescription(window, modal, "Root two edited");
  const rootTwoDecl = cellCandidates.find((decl) => String(decl && decl.name || "").toLowerCase() === "gv_root_two");
  const rootTwoKey = window.getDeclOverrideStorageKey(rootTwoDecl);
  assert(rootTwoKey, "Expected a storage key for gv_root_two.");
  const storedRoot = state.descOverrides[rootTwoKey];
  assert.strictEqual(
    typeof storedRoot === "object" ? String(storedRoot.text || "") : String(storedRoot || ""),
    "Root two edited",
    "Expected save to persist the root declaration override."
  );
  for (const decl of cellCandidates) {
    assert.strictEqual(
      window.getDeclOverrideStorageKey(decl),
      rootTwoKey,
      "Expected every cloned declaration in the PERFORM chain to share the scoped override key."
    );
  }
  const globalFormParam = (state.data.decls || []).find((decl) => String(decl && decl.name || "").toLowerCase() === "iv_outer_two");
  const globalFormParamKey = window.getDeclOverrideStorageKey(globalFormParam);
  assert.strictEqual(
    Object.prototype.hasOwnProperty.call(state.descOverrides || {}, globalFormParamKey),
    false,
    `Expected global FORM_PARAM ${globalFormParamKey} to stay free of source-scoped overrides.`
  );

  nestedTable = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="PERFORM"]'))
    .find((table) => getTemplateTableRows(table).some((row) => row.includes("frm_inner")));
  assert.deepStrictEqual(getTemplateTableRows(nestedTable), [
    ["PERFORM", "frm_inner"],
    ["USING", "Root one"],
    ["USING", "Root two edited"]
  ]);

  const literalTable = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="PERFORM"]'))
    .find((table) => getTemplateTableRows(table).some((row) => row.includes("frm_literal")));
  assert(literalTable, "Expected the literal PERFORM template table.");
  const literalCell = findTemplateCellByText(literalTable, "'X'");
  assert.strictEqual(literalCell && literalCell.__templateCellMeta && literalCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");
  modal = await openTemplateCellDescriptionTab(window, literalCell);
  assert(!String(modal.textContent || "").includes("Khong tim thay decl cho o nay."), "Expected the generic missing-declaration message to be replaced.");
  const literalTextarea = modal.querySelector("textarea.template-config-json");
  assert(literalTextarea && literalTextarea.disabled, "Expected the literal Description textarea to stay disabled.");

  dom.window.close();
}

async function assertTemplatePerformSourceEditDoesNotBleedAcrossSources() {
  const source = [
    'DATA gs_request TYPE string. "Request"',
    'DATA gs_preview TYPE string. "Preview request"',
    "PERFORM frm_validate_request USING gs_request.",
    "PERFORM frm_validate_request USING gs_preview.",
    "PERFORM frm_validate_request USING gs_request.",
    "FORM frm_validate_request USING iv_request TYPE string.",
    "  CLEAR iv_request.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const registry = state.performSourceRegistry;
  assert(registry && typeof registry.selectCandidate === "function", "Expected PERFORM source registry.");
  const candidates = registry.candidatesByFormUpper.get("FRM_VALIDATE_REQUEST") || [];
  assert.strictEqual(candidates.length, 3, "Expected three PERFORM sources for frm_validate_request.");

  assert(
    window.AbapViewerRuntime.api.selectPerformSourceCandidate("FRM_VALIDATE_REQUEST", candidates[2].key),
    "Expected switch to source 3."
  );
  await waitForViewerUi(window);

  const findClearChainCell = (rootName) => Array.from(els.templatePreviewOutput.querySelectorAll("td")).find((cell) => {
    const declCandidates = cell && cell.__templateCellMeta && Array.isArray(cell.__templateCellMeta.declCandidates)
      ? cell.__templateCellMeta.declCandidates
      : [];
    return String(cell && cell.textContent || "").trim().toUpperCase() !== "CLEAR"
      && declCandidates.length > 1
      && declCandidates.some((decl) => String(decl && decl.name || "").toLowerCase() === "iv_request")
      && declCandidates.some((decl) => String(decl && decl.name || "").toLowerCase() === rootName);
  });

  let clearCell = findClearChainCell("gs_request");
  assert(clearCell, "Expected expanded CLEAR cell with gs_request + iv_request chain under source 3.");
  const sourceThreeDecls = clearCell.__templateCellMeta.declCandidates;
  const sourceThreeKeys = new Set(sourceThreeDecls.map((decl) => window.getDeclOverrideStorageKey(decl)));
  assert.strictEqual(sourceThreeKeys.size, 1, "Expected every declaration in one PERFORM chain to share one scoped key.");
  const sourceThreeKey = Array.from(sourceThreeKeys)[0];
  assert.match(sourceThreeKey, /^PERFORM_CHAIN:/, "Expected a source-scoped PERFORM override key.");

  const modal = await openTemplateCellDescriptionTab(window, clearCell);
  assert(!modal.querySelector("select.template-config-json"), "Expected no Description Target select for PERFORM chain.");
  await saveTemplateCellDescription(window, modal, "Source 3 only");

  const requestDecl = (state.data.decls || []).find((decl) => String(decl && decl.name || "").toLowerCase() === "gs_request");
  const previewDecl = (state.data.decls || []).find((decl) => String(decl && decl.name || "").toLowerCase() === "gs_preview");
  const formalDecl = (state.data.decls || []).find((decl) => String(decl && decl.name || "").toLowerCase() === "iv_request");
  assert.strictEqual(String(state.descOverrides[sourceThreeKey] || ""), "Source 3 only", "Expected source 3 scoped override.");
  for (const globalDecl of [requestDecl, previewDecl, formalDecl]) {
    const globalKey = window.getDeclOverrideStorageKey(globalDecl);
    assert.strictEqual(
      Object.prototype.hasOwnProperty.call(state.descOverrides || {}, globalKey),
      false,
      `Expected global override ${globalKey} to stay untouched.`
    );
  }

  assert(
    window.AbapViewerRuntime.api.selectPerformSourceCandidate("FRM_VALIDATE_REQUEST", candidates[0].key),
    "Expected switch to source 1."
  );
  await waitForViewerUi(window);
  clearCell = findClearChainCell("gs_request");
  const sourceOneKey = window.getDeclOverrideStorageKey(clearCell.__templateCellMeta.declCandidates[0]);
  assert.notStrictEqual(sourceOneKey, sourceThreeKey, "Expected source 1 and source 3 to stay separate despite the same root declaration.");
  assert(!String(clearCell.textContent || "").includes("Source 3 only"), "Expected source 1 description to stay unchanged.");

  assert(window.AbapViewerRuntime.api.selectPerformSourceCandidate("FRM_VALIDATE_REQUEST", candidates[1].key));
  await waitForViewerUi(window);
  clearCell = findClearChainCell("gs_preview");
  assert(clearCell, "Expected source 2 to trace the preview request.");
  assert(!String(clearCell.textContent || "").includes("Source 3 only"), "Expected source 2 description to stay unchanged.");

  assert(window.AbapViewerRuntime.api.selectPerformSourceCandidate("FRM_VALIDATE_REQUEST", candidates[2].key));
  await waitForViewerUi(window);
  clearCell = findClearChainCell("gs_request");
  const returnedSourceThreeDecl = clearCell.__templateCellMeta.declCandidates[0];
  assert(
    String(clearCell.textContent || "").includes("Source 3 only"),
    `Expected source 3 override to return after source switching. Text=${String(clearCell.textContent || "")} Key=${window.getDeclOverrideStorageKey(returnedSourceThreeDecl)} Effective=${window.AbapViewerRuntime.api.getEffectiveDeclDesc(returnedSourceThreeDecl)}`
  );

  els.parseBtn.click();
  await waitForViewerUi(window);
  const reparsedCandidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM_VALIDATE_REQUEST") || [];
  assert(window.AbapViewerRuntime.api.selectPerformSourceCandidate("FRM_VALIDATE_REQUEST", reparsedCandidates[2].key));
  await waitForViewerUi(window);
  clearCell = findClearChainCell("gs_request");
  assert(String(clearCell.textContent || "").includes("Source 3 only"), "Expected identical reparse to restore the source 3 override.");

  const clearModal = await openTemplateCellDescriptionTab(window, clearCell);
  await saveTemplateCellDescription(window, clearModal, "");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides || {}, sourceThreeKey), false, "Expected clear to remove only the scoped key.");

  dom.window.close();
}

async function assertTemplateRowDescriptionTargetsExactConditionDecls() {
  const source = [
    "DATA lt_rows TYPE TABLE OF string.",
    "DATA ls_row TYPE string.",
    "DATA lv_a TYPE string. \"A",
    "DATA lv_b TYPE string. \"B",
    "DATA lv_c TYPE string. \"C",
    "DATA lv_d TYPE string. \"D",
    "LOOP AT lt_rows INTO ls_row WHERE lv_a = lv_b AND lv_c = lv_d.",
    "ENDLOOP."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  let loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert(loopTable, "Expected the conditional LOOP template table.");
  const modal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(loopTable, "C = D"));
  const targetLabels = Array.from(modal.querySelectorAll("select option"), (option) => String(option.textContent || ""));
  assert(targetLabels.some((label) => label.includes("lv_c")), "Expected the second condition row to target lv_c.");
  assert(targetLabels.some((label) => label.includes("lv_d")), "Expected the second condition row to target lv_d.");
  assert(!targetLabels.some((label) => label.includes("lv_a") || label.includes("lv_b")), "Expected condition targets to stay isolated by rendered row.");

  await saveTemplateCellDescription(window, modal, "C edited");
  loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert(findTemplateCellByText(loopTable, "C edited = D"), "Expected saving the selected condition target to refresh only that operand.");

  dom.window.close();
}

async function assertTemplateAppendUnboundOperandsUseCanonicalTargets() {
  const dom = await renderFixture("APPEND a TO b.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const legacySourceKey = "PATH:OBJECTS/OBJECT[1]/VALUES/WHAT/DECL:A";
  state.descOverrides[legacySourceKey] = "Legacy A";
  runtime.api.renderOutput();
  runtime.api.renderTemplatePreview();
  assert(String(els.output.textContent || "").includes("Legacy A"), "Expected Output to read the legacy PATH_DECL alias before migration.");

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  let appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(appendTable, "Expected the APPEND template table.");
  let sourceModal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(appendTable, "Legacy A"));
  assert(!String(sourceModal.textContent || "").includes("Khong tim thay decl cho o nay."), "Expected unbound APPEND source a to stay editable.");
  assert(String(sourceModal.textContent || "").includes("a"), "Expected APPEND source cell to target a.");
  assert(!String(sourceModal.textContent || "").includes("b @"), "Expected APPEND source cell not to target b.");

  const sourceKey = "PATH:OBJECT:1/VALUES/WHAT/DECL:A";
  const targetKey = "PATH:OBJECT:1/VALUES/TO/DECL:B";
  await saveTemplateCellDescription(window, sourceModal, "Source A");
  assert.strictEqual(String(state.descOverrides[sourceKey] || ""), "Source A", "Expected APPEND source to save under the canonical Output PATH_DECL key.");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, legacySourceKey), false, "Expected canonical Save to remove the legacy Template alias.");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, targetKey), false, "Expected editing a not to modify b.");
  assert(String(els.output.textContent || "").includes("Source A"), "Expected Output to refresh the canonical source override.");

  appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(findTemplateCellByText(appendTable, "Source A"), "Expected Template to refresh the APPEND source override.");
  sourceModal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(appendTable, "Source A"));
  await saveTemplateCellDescription(window, sourceModal, "");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, sourceKey), false, "Expected clearing a to remove its canonical override.");

  state.descOverrides[legacySourceKey] = "Legacy clear";
  runtime.api.renderTemplatePreview();
  appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  sourceModal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(appendTable, "Legacy clear"));
  await saveTemplateCellDescription(window, sourceModal, "");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, legacySourceKey), false, "Expected Clear to remove the legacy Template alias without resurfacing it.");

  appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  const targetModal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(appendTable, "b"));
  assert(String(targetModal.textContent || "").includes("b"), "Expected APPEND target cell to target b.");
  await saveTemplateCellDescription(window, targetModal, "Target B");
  assert.strictEqual(String(state.descOverrides[targetKey] || ""), "Target B", "Expected APPEND target to save under its own canonical Output PATH_DECL key.");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, sourceKey), false, "Expected editing b not to recreate a.");

  dom.window.close();
}

async function assertLegacyPathAliasUsesTemplateIndexAfterPerformExpansion() {
  const source = [
    "PERFORM f.",
    "APPEND a TO b.",
    "FORM f.",
    "  CLEAR x.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const legacySourceKey = "PATH:OBJECTS/OBJECT[3]/VALUES/WHAT/DECL:A";

  const templateItems = typeof window.getRenderableObjectListForTemplate === "function"
    ? window.getRenderableObjectListForTemplate()
    : [];
  const appendIndex = templateItems.findIndex((item) => item && item.obj && item.obj.objectType === "APPEND");
  assert.strictEqual(appendIndex, 2, "Expected expanded FORM content to shift APPEND to Template object 3.");

  state.descOverrides[legacySourceKey] = "Legacy shifted A";
  runtime.api.renderOutput();
  runtime.api.renderTemplatePreview();

  assert(
    String(els.output.textContent || "").includes("Legacy shifted A"),
    "Expected Output to resolve the legacy Template index alias after PERFORM expansion."
  );
  const appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(findTemplateCellByText(appendTable, "Legacy shifted A"), "Expected Template to resolve the same shifted legacy alias.");

  dom.window.close();
}

async function assertInputPitchAndCodeNavigationUseNativeTextareaMetrics() {
  const dom = await renderFixture("CLEAR seed.");
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const lineCount = 8001;
  const targetLine = 7000;
  let nativeScrollHeight = 144101;
  const paddingY = 20;
  const expectedPitch = (nativeScrollHeight - paddingY) / lineCount;
  const lines = Array.from({ length: lineCount }, (_, index) => (
    `CLEAR gv_${String(index + 1).padStart(5, "0")}. \"日本語の説明 ${index + 1}`
  ));

  Object.defineProperty(els.inputText, "clientHeight", {
    configurable: true,
    get() {
      return 540;
    }
  });
  Object.defineProperty(els.inputText, "scrollHeight", {
    configurable: true,
    get() {
      return nativeScrollHeight;
    }
  });
  els.inputText.value = lines.join("\n");
  els.inputText.dispatchEvent(new window.Event("input", { bubbles: true }));
  await settleViewerUi(window);

  assert(
    Math.abs(Number(state.inputGutterVirtual.lineHeightPx) - expectedPitch) <= 0.001,
    "Expected the gutter to use the native textarea pitch instead of nominal 18px."
  );

  els.inputText.scrollTop = (targetLine - 1) * expectedPitch;
  els.inputText.dispatchEvent(new window.Event("scroll"));
  await settleViewerUi(window);
  const targetButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
  const targetRow = targetButton && targetButton.closest(".gutter-line");
  assert(targetRow, "Expected the target gutter row to be virtualized.");
  assert(
    Math.abs(Number.parseFloat(targetRow.style.height || "0") - expectedPitch) <= 0.001,
    "Expected each gutter row to use the same fractional pitch as the textarea."
  );

  assert.strictEqual(typeof window.jumpInputToCodeRange, "function", "Expected shared source navigation to remain available.");
  window.jumpInputToCodeRange(targetLine, targetLine, null);
  await settleViewerUi(window);
  const expectedTop = ((targetLine - 1) * expectedPitch) - (540 * 0.28);
  assert(
    Math.abs(Number(els.inputText.scrollTop) - expectedTop) <= 1,
    "Expected Template/Output Code navigation to use the measured textarea pitch."
  );
  assert.strictEqual(
    String(els.inputText.value || "").slice(els.inputText.selectionStart, els.inputText.selectionEnd).trim(),
    lines[targetLine - 1],
    "Expected Japanese source selection to remain exact."
  );

  nativeScrollHeight = 145701;
  const resizedPitch = (nativeScrollHeight - paddingY) / lineCount;
  window.dispatchEvent(new window.Event("resize"));
  await settleViewerUi(window);
  assert(
    Math.abs(Number(state.inputGutterVirtual.lineHeightPx) - resizedPitch) <= 0.001,
    "Expected resize/zoom metric changes to update gutter pitch without editing the source."
  );

  dom.window.close();
}

async function assertVirtualGutterJumpSettlesInOneClick() {
  const itemCount = 700;
  const targetLine = 600;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  const runJumpAssertion = async ({ kind, container, tabButton, getItemHeight, selectedSelector }) => {
    tabButton.click();
    await settleViewerUi(window);
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      getItemHeight,
      {
        scrollEventViaRaf: true,
        scrollConvergenceFactor: 1,
        visualScale: kind === "output" ? 0.8 : 1.25
      }
    );
    try {
      els.inputText.scrollTop = (targetLine - 1) * 18;
      els.inputText.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window);
      const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
      assert(gutterButton && !gutterButton.hidden, `Expected ${kind} gutter target for line ${targetLine}.`);

      gutterButton.click();
      await settleViewerUi(window, 10);
      const selected = container.querySelector(selectedSelector);
      assert(selected, `Expected ${kind} target to stay selected after virtual settling.`);
      const firstOffset = selected.getBoundingClientRect().top - container.getBoundingClientRect().top;
      assert(
        Math.abs(firstOffset - 10) <= 1,
        `Expected one ${kind} gutter click to settle at 10px, got ${firstOffset}.`
      );

      const firstScrollTop = Number(container.scrollTop) || 0;
      gutterButton.click();
      await settleViewerUi(window, 10);
      const secondOffset = selected.getBoundingClientRect().top - container.getBoundingClientRect().top;
      assert(Math.abs(secondOffset - 10) <= 1, `Expected repeated ${kind} click to remain aligned.`);
      assert(
        Math.abs((Number(container.scrollTop) || 0) - firstScrollTop) <= 1,
        `Expected repeated ${kind} click to be idempotent.`
      );
    } finally {
      restoreLayout();
    }
  };

  await runJumpAssertion({
    kind: "output",
    container: els.output,
    tabButton: els.rightTabOutputBtn,
    getItemHeight: (index) => 90 + ((index % 4) * 70),
    selectedSelector: ".card.selected"
  });
  await runJumpAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    getItemHeight: (index) => 70 + ((index % 3) * 65),
    selectedSelector: ".template-block.selected"
  });

  dom.window.close();
}

async function assertManualScrollCancelsPendingVirtualAlignment() {
  const itemCount = 700;
  const targetLine = 600;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  const runTakeoverAssertion = async ({ kind, container, tabButton, getItemHeight }) => {
    tabButton.click();
    await settleViewerUi(window);
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      getItemHeight,
      { scrollEventViaRaf: true, visualScale: kind === "output" ? 0.8 : 1.25 }
    );
    try {
      els.inputText.scrollTop = (targetLine - 1) * 18;
      els.inputText.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window);
      const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
      assert(gutterButton && !gutterButton.hidden, `Expected ${kind} gutter target for manual takeover.`);

      const userIntentCases = [
        {
          name: "wheel",
          delta: 420,
          createEvent: () => new window.WheelEvent("wheel", { bubbles: true, cancelable: true, deltaY: 420 })
        },
        {
          name: "pointerdown",
          delta: -240,
          createEvent: () => new window.Event("pointerdown", { bubbles: true, cancelable: true })
        },
        {
          name: "touchstart",
          delta: 300,
          createEvent: () => new window.Event("touchstart", { bubbles: true, cancelable: true })
        },
        {
          name: "PageDown",
          delta: -180,
          createEvent: () => new window.KeyboardEvent("keydown", { bubbles: true, cancelable: true, key: "PageDown" })
        }
      ];

      for (const userIntent of userIntentCases) {
        gutterButton.click();
        const alignedTop = Number(container.scrollTop) || 0;
        const userTop = Math.max(0, alignedTop + userIntent.delta);
        container.dispatchEvent(userIntent.createEvent());
        container.scrollTop = userTop;
        container.dispatchEvent(new window.Event("scroll"));
        await settleViewerUi(window, 12);

        assert(
          Math.abs((Number(container.scrollTop) || 0) - userTop) <= 1,
          `Expected ${kind} ${userIntent.name} input to cancel stale gutter alignment; `
            + `requested ${userTop}, got ${container.scrollTop}.`
        );
      }
    } finally {
      restoreLayout();
    }
  };

  await runTakeoverAssertion({
    kind: "output",
    container: els.output,
    tabButton: els.rightTabOutputBtn,
    getItemHeight: (index) => 120 + ((index % 4) * 24)
  });
  await runTakeoverAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    getItemHeight: (index) => 90 + ((index % 3) * 22)
  });

  dom.window.close();
}

async function assertHeterogeneousVirtualRangesKeepViewportCovered() {
  const itemCount = 700;
  const targetLine = 600;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  const runCoverageAssertion = async ({ kind, container, tabButton, rootSelector, virtualState }) => {
    tabButton.click();
    await settleViewerUi(window);
    const getItemHeight = (index) => {
      if (index >= 330 && index <= 380) {
        return 1200 + ((index % 3) * 80);
      }
      if (index >= 585 && index <= 615) {
        return kind === "template" ? 130 : 140;
      }
      return 55 + ((index % 5) * 8);
    };
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      getItemHeight,
      { scrollHeightMode: "virtual-dom", visualScale: kind === "output" ? 0.8 : 1.25 }
    );
    try {
      els.inputText.scrollTop = (targetLine - 1) * 18;
      els.inputText.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window);
      const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
      assert(gutterButton && !gutterButton.hidden, `Expected ${kind} gutter target before range coverage test.`);
      gutterButton.click();
      await settleViewerUi(window, 10);

      const estimateBeforeRangeChange = Number(virtualState.avgItemHeight) || (kind === "template" ? 140 : 200);
      const requestedTop = 350 * estimateBeforeRangeChange;
      container.dispatchEvent(new window.WheelEvent("wheel", {
        bubbles: true,
        cancelable: true,
        deltaY: requestedTop - (Number(container.scrollTop) || 0)
      }));
      container.scrollTop = requestedTop;
      container.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window, 8);

      const containerRect = container.getBoundingClientRect();
      const renderedRoots = Array.from(container.querySelectorAll(rootSelector));
      const coveringRoots = renderedRoots.filter((node) => {
        const rect = node.getBoundingClientRect();
        return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
      });
      assert(
        coveringRoots.length > 0,
        `Expected ${kind} virtual range to cover the viewport after heterogeneous measurement; `
          + `scrollTop=${container.scrollTop}, start=${virtualState.start}, end=${virtualState.end}.`
      );

      const firstVisibleIndex = Number(coveringRoots[0].getAttribute(
        kind === "template" ? "data-template-index" : "data-virtual-item-index"
      ));
      container.dispatchEvent(new window.Event("scroll"));
      await settleViewerUi(window, 6);
      const nextVisible = Array.from(container.querySelectorAll(rootSelector)).find((node) => {
        const rect = node.getBoundingClientRect();
        return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
      });
      assert(nextVisible, `Expected repeated ${kind} scroll processing not to leave a blank viewport.`);
      assert.strictEqual(
        Number(nextVisible.getAttribute(kind === "template" ? "data-template-index" : "data-virtual-item-index")),
        firstVisibleIndex,
        `Expected repeated ${kind} scroll processing at the same offset not to oscillate ranges; `
          + `scrollTop=${container.scrollTop}, start=${virtualState.start}, end=${virtualState.end}, `
          + `first=${firstVisibleIndex}, next=${nextVisible.getAttribute(kind === "template" ? "data-template-index" : "data-virtual-item-index")}.`
      );
    } finally {
      restoreLayout();
    }
  };

  await runCoverageAssertion({
    kind: "output",
    container: els.output,
    tabButton: els.rightTabOutputBtn,
    rootSelector: "[data-virtual-item-index]",
    virtualState: state.outputVirtual
  });
  await runCoverageAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    rootSelector: ".template-block[data-template-index]",
    virtualState: state.templateVirtual
  });

  dom.window.close();
}

async function assertSplitterRefreshesActiveVirtualGeometry() {
  const source = Array.from({ length: 700 }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  const runAssertion = async ({ kind, tabButton, virtualState }) => {
    tabButton.click();
    await settleViewerUi(window);
    const epochBefore = Number(virtualState.geometryEpoch) || 0;
    window.applyLayoutSplit((Number(state.layoutLeftPane) || 48) + 2, { save: false });
    await settleViewerUi(window, 6);
    assert(
      (Number(virtualState.geometryEpoch) || 0) > epochBefore,
      `Expected splitter width change to invalidate active ${kind} virtual geometry.`
    );
  };

  await runAssertion({
    kind: "output",
    tabButton: els.rightTabOutputBtn,
    virtualState: state.outputVirtual
  });
  await runAssertion({
    kind: "template",
    tabButton: els.rightTabTemplateBtn,
    virtualState: state.templateVirtual
  });

  dom.window.close();
}

async function assertBlankViewportFallbackUsesLogicalAnchor() {
  const itemCount = 700;
  const source = Array.from({ length: itemCount }, (_, index) => `CLEAR gv_${index + 1}.`).join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  const runAssertion = async ({ kind, container, tabButton, rootSelector, captureAnchor }) => {
    tabButton.click();
    await settleViewerUi(window);
    const restoreLayout = installVirtualLayoutMock(
      window,
      container,
      kind,
      itemCount,
      (index) => 80 + ((index % 4) * 20)
    );
    try {
      container.scrollTop = 24000;
      const containerRect = container.getBoundingClientRect();
      const hasVisibleRoot = Array.from(container.querySelectorAll(rootSelector)).some((node) => {
        const rect = node.getBoundingClientRect();
        return rect.bottom > containerRect.top && rect.top < containerRect.bottom;
      });
      assert.strictEqual(hasVisibleRoot, false, `Expected ${kind} fallback precondition to start from a blank viewport.`);

      const anchor = captureAnchor();
      assert(anchor && anchor.kind === "logical", `Expected blank ${kind} viewport to capture a logical item anchor.`);
      assert(Number.isFinite(Number(anchor.itemIndex)), `Expected blank ${kind} logical anchor to retain an item index.`);
    } finally {
      restoreLayout();
    }
  };

  await runAssertion({
    kind: "output",
    container: els.output,
    tabButton: els.rightTabOutputBtn,
    rootSelector: "[data-virtual-item-index]",
    captureAnchor: () => window.captureOutputViewportAnchor()
  });
  await runAssertion({
    kind: "template",
    container: els.templatePreviewOutput,
    tabButton: els.rightTabTemplateBtn,
    rootSelector: ".template-block[data-template-index]",
    captureAnchor: () => window.captureTemplateViewportAnchor()
  });

  dom.window.close();
}

async function assertDescriptionSaveAndClearPreserveTemplateCellAnchor() {
  const clearCount = 700;
  const targetLine = 600;
  const source = [
    'DATA gv_shared TYPE string. "Shared value',
    ...Array.from({ length: clearCount }, () => "CLEAR gv_shared.")
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await settleViewerUi(window);
  const restoreLayout = installVirtualLayoutMock(
    window,
    els.templatePreviewOutput,
    "template",
    clearCount + 1,
    (index) => 72 + ((index % 3) * 18) + (Object.keys(state.descOverrides || {}).length ? 54 : 0),
    { visualScale: 1.25 }
  );

  try {
    els.inputText.scrollTop = (targetLine - 1) * 18;
    els.inputText.dispatchEvent(new window.Event("scroll"));
    await settleViewerUi(window);
    const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
    assert(gutterButton && !gutterButton.hidden, "Expected a deep Template gutter target for the anchor test.");
    gutterButton.click();
    await settleViewerUi(window, 10);

    const getSelectedCell = () => {
      const block = els.templatePreviewOutput.querySelector(".template-block.selected");
      return block && findTemplateCellByText(block, Object.keys(state.descOverrides || {}).length ? "Shared override" : "Shared value");
    };
    let cell = getSelectedCell();
    assert(cell, "Expected the selected deep Template block to expose the shared Description cell.");

    const containerTop = els.templatePreviewOutput.getBoundingClientRect().top;
    const beforeSaveOffset = cell.getBoundingClientRect().top - containerTop;
    els.inputText.setSelectionRange(3, 11);
    els.inputText.scrollTop = 123;
    const sourceSelection = [els.inputText.selectionStart, els.inputText.selectionEnd];
    const sourceScrollTop = Number(els.inputText.scrollTop) || 0;

    let modal = await openTemplateCellDescriptionTab(window, cell);
    await saveTemplateCellDescription(window, modal, "Shared override");
    await settleViewerUi(window, 10);

    cell = getSelectedCell();
    assert(cell, "Expected the edited Template cell to be restored after Save.");
    const afterSaveOffset = cell.getBoundingClientRect().top - containerTop;
    assert(
      Math.abs(afterSaveOffset - beforeSaveOffset) <= 1,
      `Expected Description Save to preserve the edited cell anchor, moved ${afterSaveOffset - beforeSaveOffset}px.`
    );
    assert.deepStrictEqual(
      [els.inputText.selectionStart, els.inputText.selectionEnd],
      sourceSelection,
      "Expected Description Save not to change the source selection."
    );
    assert.strictEqual(Number(els.inputText.scrollTop) || 0, sourceScrollTop, "Expected Description Save not to scroll source code.");
    assert(String(els.output.textContent || "").includes("Shared override"), "Expected Output to update after Description Save.");
    assert(String(els.templatePreviewOutput.textContent || "").includes("Shared override"), "Expected Template to update after Description Save.");

    const beforeClearOffset = cell.getBoundingClientRect().top - containerTop;
    modal = await openTemplateCellDescriptionTab(window, cell);
    await saveTemplateCellDescription(window, modal, "");
    await settleViewerUi(window, 10);

    cell = getSelectedCell();
    assert(cell, "Expected the cleared Template cell to be restored.");
    const afterClearOffset = cell.getBoundingClientRect().top - containerTop;
    assert(
      Math.abs(afterClearOffset - beforeClearOffset) <= 1,
      `Expected Description Clear to preserve the edited cell anchor, moved ${afterClearOffset - beforeClearOffset}px.`
    );
    assert.deepStrictEqual(
      [els.inputText.selectionStart, els.inputText.selectionEnd],
      sourceSelection,
      "Expected Description Clear not to change the source selection."
    );
    assert.strictEqual(Number(els.inputText.scrollTop) || 0, sourceScrollTop, "Expected Description Clear not to scroll source code.");
    assert(String(els.output.textContent || "").includes("Shared value"), "Expected Output to restore code Description after Clear.");
    assert(String(els.templatePreviewOutput.textContent || "").includes("Shared value"), "Expected Template to restore code Description after Clear.");
  } finally {
    restoreLayout();
    dom.window.close();
  }
}

async function assertDescriptionEditPreservesOutputSelectedItemAnchor() {
  const clearCount = 700;
  const targetLine = 600;
  const source = [
    'DATA gv_shared TYPE string. "Shared value',
    ...Array.from({ length: clearCount }, () => "CLEAR gv_shared.")
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  els.rightTabOutputBtn.click();
  await settleViewerUi(window);
  const restoreLayout = installVirtualLayoutMock(
    window,
    els.output,
    "output",
    clearCount + 1,
    (index) => 160 + ((index % 4) * 25) + (Object.keys(state.descOverrides || {}).length ? 48 : 0),
    { visualScale: 0.8 }
  );

  try {
    els.inputText.scrollTop = (targetLine - 1) * 18;
    els.inputText.dispatchEvent(new window.Event("scroll"));
    await settleViewerUi(window);
    const gutterButton = els.inputGutterContent.querySelector(`button[data-line="${targetLine}"]`);
    assert(gutterButton && !gutterButton.hidden, "Expected a deep Output gutter target for the anchor test.");
    gutterButton.click();
    await settleViewerUi(window, 10);

    let selected = els.output.querySelector(".card.selected");
    assert(selected, "Expected a selected Output item before Description edit.");
    const containerTop = els.output.getBoundingClientRect().top;
    const beforeSaveOffset = selected.getBoundingClientRect().top - containerTop;
    assert(Math.abs(beforeSaveOffset - 10) <= 1, `Expected Output gutter jump to align before edit, got ${beforeSaveOffset}.`);
    let editButton = selected.querySelector('button[aria-label="Edit decl description"]');
    assert(editButton, "Expected the selected Output item to expose Description edit.");
    editButton.click();
    assert(!els.editModal.hidden, "Expected Output Description edit modal.");
    els.editDesc.value = "Shared output override";
    els.editSaveBtn.click();
    await settleViewerUi(window, 10);

    selected = els.output.querySelector(".card.selected");
    assert(selected, "Expected the selected Output item to survive Description Save.");
    const afterSaveOffset = selected.getBoundingClientRect().top - containerTop;
    assert(
      Math.abs(afterSaveOffset - beforeSaveOffset) <= 1,
      `Expected Description Save to preserve the selected Output item, moved ${afterSaveOffset - beforeSaveOffset}px.`
    );
    assert(String(els.output.textContent || "").includes("Shared output override"), "Expected Output Description Save to rerender text.");

    const beforeClearOffset = selected.getBoundingClientRect().top - containerTop;
    editButton = selected.querySelector('button[aria-label="Edit decl description"]');
    editButton.click();
    els.editClearBtn.click();
    await settleViewerUi(window, 10);

    selected = els.output.querySelector(".card.selected");
    assert(selected, "Expected the selected Output item to survive Description Clear.");
    const afterClearOffset = selected.getBoundingClientRect().top - containerTop;
    assert(
      Math.abs(afterClearOffset - beforeClearOffset) <= 1,
      `Expected Description Clear to preserve the selected Output item, moved ${afterClearOffset - beforeClearOffset}px.`
    );
    assert(String(els.output.textContent || "").includes("Shared value"), "Expected Output to restore code Description after Clear.");
  } finally {
    restoreLayout();
    dom.window.close();
  }
}

async function assertTemplateAppendDeclaredOperandsPreferRealDeclarations() {
  const source = [
    "DATA a TYPE i. \"Real A",
    "DATA b TYPE STANDARD TABLE OF i. \"Real B",
    "APPEND a TO b."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(appendTable, "Expected the declared APPEND template table.");
  const sourceCell = findTemplateCellByText(appendTable, "Real A");
  assert(sourceCell, "Expected the declared APPEND source description.");
  const candidates = sourceCell.__templateCellMeta && sourceCell.__templateCellMeta.declCandidates;
  assert(Array.isArray(candidates) && candidates.length > 0, "Expected declared APPEND source provenance.");
  assert.strictEqual(String(candidates[0].name || ""), "a");
  assert.notStrictEqual(String(candidates[0].objectType || "").toUpperCase(), "PATH_DECL", "Expected the real declaration before any Viewer fallback.");

  const realDecl = (Array.isArray(state.data && state.data.decls) ? state.data.decls : [])
    .find((decl) => String(decl && decl.name || "") === "a");
  assert(realDecl, "Expected the parser declaration for a.");
  const realKey = window.getDeclOverrideStorageKey(realDecl);
  const modal = await openTemplateCellDescriptionTab(window, sourceCell);
  await saveTemplateCellDescription(window, modal, "Real A edited");
  assert.strictEqual(String(state.descOverrides[realKey] || ""), "Real A edited", "Expected a declared APPEND source to retain its real declaration key.");

  dom.window.close();
}

async function assertTemplateAppendLiteralKeepsOnlyTargetEditable() {
  const dom = await renderFixture("APPEND 'X' TO b.");
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(appendTable, "Expected the literal APPEND template table.");
  const literalCell = findTemplateCellByText(appendTable, "'X'");
  assert(literalCell, "Expected the literal APPEND source cell.");
  assert.strictEqual(literalCell.__templateCellMeta && literalCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");
  const literalModal = await openTemplateCellDescriptionTab(window, literalCell);
  const literalTextarea = literalModal.querySelector("textarea.template-config-json");
  assert(literalTextarea && literalTextarea.disabled, "Expected the literal APPEND source to stay locked.");

  const targetCell = findTemplateCellByText(appendTable, "b");
  assert(targetCell, "Expected the APPEND target cell.");
  assert.strictEqual(targetCell.__templateCellMeta && targetCell.__templateCellMeta.status, "editable");
  assert.strictEqual(targetCell.__templateCellMeta && targetCell.__templateCellMeta.reasonCode, "");
  assert.strictEqual(targetCell.__templateCellMeta && targetCell.__templateCellMeta.sourcePath, "rows[1].finalDesc");
  const targetModal = await openTemplateCellDescriptionTab(window, targetCell);
  const targetTextarea = targetModal.querySelector("textarea.template-config-json");
  assert(targetTextarea && !targetTextarea.disabled, "Expected b to remain editable when APPEND source is a literal.");

  dom.window.close();

  const typedDom = await renderFixture("APPEND X'01' TO b.");
  const typedWindow = typedDom.window;
  const typedEls = typedWindow.AbapViewerRuntime.els;
  typedEls.rightTabTemplateBtn.click();
  await waitForViewerUi(typedWindow);
  const typedAppendTable = typedEls.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  const typedLiteralCell = findTemplateCellByText(typedAppendTable, "X'01'");
  assert(typedLiteralCell, "Expected the typed ABAP literal APPEND source cell.");
  assert.strictEqual(typedLiteralCell.__templateCellMeta && typedLiteralCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");
  assert.deepStrictEqual(Array.from(typedLiteralCell.__templateCellMeta.declCandidates || []), [], "Expected a typed ABAP literal not to create PATH_DECL.");
  const typedTargetCell = findTemplateCellByText(typedAppendTable, "b");
  assert(Array.isArray(typedTargetCell && typedTargetCell.__templateCellMeta && typedTargetCell.__templateCellMeta.declCandidates)
    && typedTargetCell.__templateCellMeta.declCandidates.length > 0, "Expected the APPEND target to remain editable beside a typed literal.");
  typedDom.window.close();

  const staticDom = await renderFixture("APPEND INITIAL LINE TO b.");
  const staticWindow = staticDom.window;
  const staticEls = staticWindow.AbapViewerRuntime.els;
  staticEls.rightTabTemplateBtn.click();
  await waitForViewerUi(staticWindow);
  const staticAppendTable = staticEls.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  const initialCell = findTemplateCellByText(staticAppendTable, "INITIAL");
  assert(initialCell, "Expected the APPEND INITIAL LINE marker.");
  assert.strictEqual(initialCell.__templateCellMeta && initialCell.__templateCellMeta.reasonCode, "NON_DECL_SCHEMA_VALUE");
  assert.deepStrictEqual(Array.from(initialCell.__templateCellMeta.declCandidates || []), [], "Expected INITIAL not to create a data declaration target.");
  staticDom.window.close();
}

async function assertTemplateIfArrayProvenanceIsPerRenderedLine() {
  const source = [
    "DATA a TYPE i. \"A",
    "DATA b TYPE i. \"B",
    "DATA c TYPE i. \"C",
    "DATA d TYPE i. \"D",
    "IF a = b OR c = d.",
    "ENDIF."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const ifTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  assert(ifTable, "Expected the IF template table.");
  const secondLeftCell = findTemplateCellByText(ifTable, "C");
  assert(secondLeftCell, "Expected the second IF left operand row.");
  const modal = await openTemplateCellDescriptionTab(window, secondLeftCell);
  const modalText = String(modal.textContent || "");
  assert(modalText.includes("c"), "Expected the second IF line to target c.");
  assert(!modalText.includes("a @"), "Expected the second IF line not to inherit a.");
  assert.strictEqual(secondLeftCell.__templateCellMeta && secondLeftCell.__templateCellMeta.sourcePath, "extras.ifCondition.conditions[1].leftOperandDecl.finalDesc");

  await saveTemplateCellDescription(window, modal, "C edited");
  const refreshedIfTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  assert(findTemplateCellByText(refreshedIfTable, "A"), "Expected the first IF left operand to stay unchanged.");
  assert(findTemplateCellByText(refreshedIfTable, "C edited"), "Expected only the second IF left operand to refresh.");

  dom.window.close();
}

async function assertTemplateIndexedAndCompositePlaceholdersKeepProvenance() {
  const dom = await renderFixture("APPEND a TO b.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  state.templateConfig.templates.APPEND = {
    _options: {
      hideEmptyRows: true,
      hideRowsWithoutValues: false,
      expandMultilineRows: true,
      squareCells: true,
      squareCellSize: 18
    },
    A1: { text: "{rows[0].finalDesc}" },
    U1: { text: "From {values.what.value} to {values.to.value}" },
    AO1: { text: "Static" },
    BI1: { text: "{values.missing.value}" }
  };
  runtime.api.renderTemplatePreview();
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(appendTable, "Expected the custom APPEND template table.");
  const indexedCell = findTemplateCellByText(appendTable, "a");
  assert(indexedCell, "Expected indexed rows[0] output.");
  assert.strictEqual(indexedCell.__templateCellMeta && indexedCell.__templateCellMeta.sourcePath, "rows[0].finalDesc");
  const indexedModal = await openTemplateCellDescriptionTab(window, indexedCell);
  assert(String(indexedModal.textContent || "").includes("a"), "Expected indexed rows[0] to retain a provenance.");

  const compositeCell = findTemplateCellByText(appendTable, "From a to b");
  assert(compositeCell, "Expected the composite placeholder output.");
  const compositeModal = await openTemplateCellDescriptionTab(window, compositeCell);
  const targetSelect = compositeModal.querySelector("select");
  assert(targetSelect, "Expected composite placeholders to expose both targets.");
  const labels = Array.from(targetSelect.options, (option) => String(option.textContent || ""));
  assert(labels[0].includes("a"), "Expected the first placeholder target a to stay first.");
  assert(labels[1].includes("b"), "Expected the second placeholder target b to stay second.");
  assert.strictEqual(compositeCell.__templateCellMeta && compositeCell.__templateCellMeta.sourcePath, "values.what.value, values.to.value");

  const staticCell = appendTable.querySelector('td[data-template-range-key="AO1"]');
  assert(staticCell, "Expected the static custom template cell.");
  assert.strictEqual(staticCell.__templateCellMeta && staticCell.__templateCellMeta.reasonCode, "STATIC_TEXT");
  const staticModal = await openTemplateCellDescriptionTab(window, staticCell);
  assert(String(staticModal.textContent || "").includes("static text"), "Expected a clear static-text Description reason.");
  assert(staticModal.querySelector("textarea.template-config-json").disabled, "Expected static text to stay non-editable.");
  const missingPathCell = appendTable.querySelector('td[data-template-range-key="BI1"]');
  assert(missingPathCell, "Expected the unresolved custom template cell.");
  assert.strictEqual(missingPathCell.__templateCellMeta && missingPathCell.__templateCellMeta.reasonCode, "UNRESOLVED_TEMPLATE_PATH");
  assert.strictEqual(missingPathCell.__templateCellMeta && missingPathCell.__templateCellMeta.sourcePath, "values.missing.value");
  const missingPathModal = await openTemplateCellDescriptionTab(window, missingPathCell);
  assert(String(missingPathModal.textContent || "").includes("Template path"), "Expected a clear unresolved-path Description reason.");
  assert(missingPathModal.querySelector("textarea.template-config-json").disabled, "Expected an unresolved path to stay non-editable.");

  dom.window.close();
}

async function assertTemplateSemanticFallbacksMatchOutputPaths() {
  const source = [
    "PERFORM f USING x.",
    "CALL FUNCTION 'F' EXPORTING p = y.",
    "IF z = 1.",
    "ENDIF."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const performTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="PERFORM"]');
  assert(performTable, "Expected the unbound PERFORM template table.");
  const routineCell = findTemplateCellByText(performTable, "f");
  assert.strictEqual(routineCell && routineCell.__templateCellMeta && routineCell.__templateCellMeta.reasonCode, "NON_DECL_SCHEMA_VALUE");
  const performValueCell = findTemplateCellByText(performTable, "x");
  const performDecl = performValueCell && performValueCell.__templateCellMeta && performValueCell.__templateCellMeta.declCandidates[0];
  assert(performDecl, "Expected an unbound PERFORM parameter target.");
  assert.strictEqual(performValueCell.__templateCellMeta.reasonCode, "");
  assert.strictEqual(
    window.getDeclOverrideStorageKey(performDecl),
    "PATH:OBJECT:1/EXTRAS/PERFORMCALL/USING/ITEM[1]/VALUEDECL:X"
  );

  const callTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="CALL_FUNCTION"]');
  assert(callTable, "Expected the unbound CALL FUNCTION template table.");
  const functionCell = findTemplateCellByText(callTable, "'F'");
  assert.strictEqual(functionCell && functionCell.__templateCellMeta && functionCell.__templateCellMeta.reasonCode, "NON_DECL_SCHEMA_VALUE");
  const callValueCell = findTemplateCellByText(callTable, "p = y");
  const callDecl = callValueCell && callValueCell.__templateCellMeta && callValueCell.__templateCellMeta.declCandidates[0];
  assert(callDecl, "Expected an unbound CALL parameter target.");
  assert.strictEqual(
    window.getDeclOverrideStorageKey(callDecl),
    "PATH:OBJECT:2/EXTRAS/CALLFUNCTION/EXPORTING/ITEM[1]/VALUEDECL:Y"
  );

  const ifTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  assert(ifTable, "Expected the unbound IF template table.");
  const conditionLeft = findTemplateCellByText(ifTable, "z");
  const conditionRight = findTemplateCellByText(ifTable, "1");
  assert(Array.isArray(conditionLeft && conditionLeft.__templateCellMeta && conditionLeft.__templateCellMeta.declCandidates)
    && conditionLeft.__templateCellMeta.declCandidates.length > 0, "Expected the existing condition synthetic target for z.");
  assert(Array.isArray(conditionRight && conditionRight.__templateCellMeta && conditionRight.__templateCellMeta.declCandidates)
    && conditionRight.__templateCellMeta.declCandidates.length > 0, "Expected the existing condition synthetic target for literal 1 to remain unchanged.");
  const conditionLeftDecl = conditionLeft.__templateCellMeta.declCandidates[0];
  const conditionLeftKey = "CONDITION:Z";
  assert.strictEqual(window.getDeclOverrideStorageKey(conditionLeftDecl), conditionLeftKey, "Expected Template and Output to keep the existing condition-operand synthetic key.");
  const conditionModal = await openTemplateCellDescriptionTab(window, conditionLeft);
  await saveTemplateCellDescription(window, conditionModal, "Z edited");
  assert.strictEqual(String(window.AbapViewerRuntime.state.descOverrides[conditionLeftKey] || ""), "Z edited");
  assert(String(els.output.textContent || "").includes("Z edited"), "Expected Output to refresh the condition override saved from Template.");
  const refreshedIfTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  assert(findTemplateCellByText(refreshedIfTable, "Z edited"), "Expected Template to refresh the same condition operand.");

  dom.window.close();
}

async function assertTemplateDirectSchemaPathsStayLocked() {
  const dom = await renderFixture("SELECT col FROM dbtab INTO TABLE out WHERE q = 1.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  state.templateConfig.templates.SELECT = {
    _options: {
      hideEmptyRows: true,
      hideRowsWithoutValues: false,
      expandMultilineRows: true,
      squareCells: true,
      squareCellSize: 18
    },
    A1: { text: "{extras.select.whereConditions.leftOperandDecl.finalDesc}" },
    U1: { text: "{values.fields.value}" },
    AO1: { text: "{values.from.value}" },
    BI1: { text: "{values.intoTable.value}" }
  };
  runtime.api.renderTemplatePreview();
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert(selectTable, "Expected the custom SELECT template table.");
  const conditionCell = findTemplateCellByText(selectTable, "q");
  assert(Array.isArray(conditionCell && conditionCell.__templateCellMeta && conditionCell.__templateCellMeta.declCandidates)
    && conditionCell.__templateCellMeta.declCandidates.length > 0, "Expected direct condition paths to keep operand provenance.");

  const fieldsCell = findTemplateCellByText(selectTable, "col");
  assert.strictEqual(fieldsCell && fieldsCell.__templateCellMeta && fieldsCell.__templateCellMeta.reasonCode, "NON_DECL_SCHEMA_VALUE");
  assert.deepStrictEqual(Array.from(fieldsCell.__templateCellMeta.declCandidates || []), [], "Expected raw SELECT fields not to create PATH_DECL targets.");
  const fieldsModal = await openTemplateCellDescriptionTab(window, fieldsCell);
  assert(String(fieldsModal.textContent || "").includes("không phải data operand"), "Expected a clear schema-value Description reason.");
  assert(fieldsModal.querySelector("textarea.template-config-json").disabled, "Expected raw SELECT fields to stay non-editable.");

  const dbCell = findTemplateCellByText(selectTable, "dbtab");
  assert.strictEqual(dbCell && dbCell.__templateCellMeta && dbCell.__templateCellMeta.reasonCode, "NON_DECL_SCHEMA_VALUE");
  assert.deepStrictEqual(Array.from(dbCell.__templateCellMeta.declCandidates || []), [], "Expected DB names not to create PATH_DECL targets.");

  const intoCell = findTemplateCellByText(selectTable, "out");
  const intoDecl = intoCell && intoCell.__templateCellMeta && intoCell.__templateCellMeta.declCandidates[0];
  assert(intoDecl, "Expected SELECT destination out to remain editable.");
  assert.strictEqual(window.getDeclOverrideStorageKey(intoDecl), "PATH:OBJECT:1/VALUES/INTOTABLE/DECL:OUT");

  dom.window.close();
}

async function assertTemplateFallbackAllowlistCoversExistingItabOperands() {
  const cases = [
    {
      source: "MODIFY t FROM x INDEX i.",
      objectType: "MODIFY_ITAB",
      targets: [
        ["t", "PATH:OBJECT:1/VALUES/ITABORDBTAB/DECL:T"],
        ["x", "PATH:OBJECT:1/VALUES/FROM/DECL:X"],
        ["i", "PATH:OBJECT:1/VALUES/INDEX/DECL:I"]
      ]
    },
    {
      source: "DELETE t INDEX i.",
      objectType: "DELETE_ITAB",
      targets: [
        ["t", "PATH:OBJECT:1/VALUES/TARGET/DECL:T"],
        ["i", "PATH:OBJECT:1/VALUES/INDEX/DECL:I"]
      ]
    },
    {
      source: "INSERT a INTO TABLE t.",
      objectType: "INSERT_ITAB",
      targets: [
        ["a", "PATH:OBJECT:1/VALUES/WHAT/DECL:A"],
        ["t", "PATH:OBJECT:1/VALUES/INTOTABLE/DECL:T"]
      ]
    },
    {
      source: "INSERT a INTO t INDEX i.",
      objectType: "INSERT_ITAB",
      targets: [
        ["a", "PATH:OBJECT:1/VALUES/WHAT/DECL:A"],
        ["t", "PATH:OBJECT:1/VALUES/INTO/DECL:T"],
        ["i", "PATH:OBJECT:1/VALUES/INDEX/DECL:I"]
      ]
    },
    {
      source: "APPEND a TO t REFERENCE INTO r.",
      objectType: "APPEND",
      targets: [
        ["a", "PATH:OBJECT:1/VALUES/WHAT/DECL:A"],
        ["t", "PATH:OBJECT:1/VALUES/TO/DECL:T"],
        ["r", "PATH:OBJECT:1/VALUES/REFINTO/DECL:R"]
      ]
    },
    {
      source: "CLEAR a WITH b.",
      objectType: "CLEAR",
      targets: [
        ["a", "PATH:OBJECT:1/VALUES/TARGET/DECL:A"],
        ["b", "PATH:OBJECT:1/VALUES/WITH/DECL:B"]
      ]
    },
    {
      source: "READ TABLE t REFERENCE INTO r.",
      objectType: "READ_TABLE",
      targets: [
        ["t", "PATH:OBJECT:1/VALUES/ITAB/DECL:T"],
        ["r", "PATH:OBJECT:1/VALUES/REFINTO/DECL:R"]
      ]
    },
    {
      source: "LOOP AT t REFERENCE INTO r.\nENDLOOP.",
      objectType: "LOOP_AT_ITAB",
      targets: [
        ["t", "PATH:OBJECT:1/VALUES/ITAB/DECL:T"],
        ["r", "PATH:OBJECT:1/VALUES/REFINTO/DECL:R"]
      ]
    },
    {
      source: "SELECT col FROM db APPENDING TABLE t.",
      objectType: "SELECT",
      targets: [
        ["t", "PATH:OBJECT:1/VALUES/APPENDINGTABLE/DECL:T"]
      ]
    },
    {
      source: "INSERT a INTO TABLE t REFERENCE INTO r.",
      objectType: "INSERT_ITAB",
      targets: [
        ["r", "PATH:OBJECT:1/VALUES/REFINTO/DECL:R"]
      ]
    }
  ];

  for (const testCase of cases) {
    const dom = await renderFixture(testCase.source);
    const { window } = dom;
    const { els } = window.AbapViewerRuntime;
    els.rightTabTemplateBtn.click();
    await waitForViewerUi(window);

    const table = els.templatePreviewOutput.querySelector(`.template-preview-table[data-object-type="${testCase.objectType}"]`);
    assert(table, `Expected the ${testCase.objectType} template table.`);
    for (const [cellText, expectedKey] of testCase.targets) {
      const cell = findTemplateCellByText(table, cellText);
      const decl = cell && cell.__templateCellMeta && cell.__templateCellMeta.declCandidates[0];
      assert(decl, `Expected ${testCase.objectType} operand ${cellText} to be editable.`);
      assert.strictEqual(window.getDeclOverrideStorageKey(decl), expectedKey);
    }
    dom.window.close();
  }
}

async function assertTemplateResolverWarnsOnceWithCellMetadata() {
  const dom = await renderFixture("APPEND a TO b.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  state.templateConfig.templates.APPEND = {
    _options: {
      hideEmptyRows: false,
      hideRowsWithoutValues: false,
      expandMultilineRows: true
    },
    A1: { text: "{values.what.value}" }
  };

  const originalNormalize = window.normalizeEntryObjectForPath;
  const originalWarn = window.console.warn;
  const provenanceWarnings = [];
  window.normalizeEntryObjectForPath = () => {
    throw new Error("forced provenance failure");
  };
  window.console.warn = (message, details) => {
    if (String(message || "").includes("Template description provenance resolution failed")) {
      provenanceWarnings.push(details);
    }
  };

  try {
    runtime.api.renderTemplatePreview();
    runtime.api.renderTemplatePreview();
    els.rightTabTemplateBtn.click();
    await waitForViewerUi(window);

    assert.strictEqual(provenanceWarnings.length, 1, "Expected the same resolver failure to warn only once.");
    const warning = provenanceWarnings[0] || {};
    assert.strictEqual(warning.objectId, 1);
    assert.strictEqual(warning.line, 1);
    assert.strictEqual(warning.template, "APPEND");
    assert.strictEqual(warning.range, "A1");
    assert.strictEqual(warning.token, "values.what.value");
    assert(!JSON.stringify(warning).includes("APPEND a TO b."), "Expected the resolver warning not to dump source code.");

    const cell = els.templatePreviewOutput.querySelector('td[data-template-range-key="A1"]');
    assert(cell, "Expected the resolver-error template cell.");
    assert.strictEqual(cell.__templateCellMeta && cell.__templateCellMeta.reasonCode, "RESOLUTION_ERROR");
    const modal = await openTemplateCellDescriptionTab(window, cell);
    assert(String(modal.textContent || "").includes("Resolver phát sinh lỗi"), "Expected a clear resolver-error Description reason.");
    assert(modal.querySelector("textarea.template-config-json").disabled, "Expected a resolver-error cell to stay non-editable.");
  } finally {
    window.normalizeEntryObjectForPath = originalNormalize;
    window.console.warn = originalWarn;
    dom.window.close();
  }
}

async function assertExpandedPerformTraceUsesRootDeclarations() {
  const source = [
    "TYPES: BEGIN OF ty_ctx,",
    "         name TYPE string,",
    "         city TYPE string,",
    "       END OF ty_ctx.",
    "DATA gv_root TYPE string.",
    "DATA gv_other TYPE string.",
    "DATA gv_change TYPE string.",
    "DATA gv_change_other TYPE string.",
    "DATA gs_root TYPE ty_ctx.",
    "DATA gs_other TYPE ty_ctx.",
    "DATA gt_root TYPE TABLE OF string.",
    "DATA gt_other TYPE TABLE OF string.",
    "PERFORM frm_outer USING gv_root gs_root CHANGING gv_change TABLES gt_root.",
    "PERFORM frm_outer USING gv_other gs_other CHANGING gv_change_other TABLES gt_other.",
    "PERFORM frm_literal USING 'X'.",
    "PERFORM frm_external IN PROGRAM zother USING gv_root.",
    "FORM frm_outer USING iv_outer TYPE string is_outer TYPE ty_ctx",
    "               CHANGING cv_outer TYPE string",
    "               TABLES tt_outer.",
    "  DATA lv_local TYPE string.",
    "  lv_local = iv_outer.",
    "  PERFORM frm_local USING lv_local.",
    "  PERFORM frm_inner USING iv_outer is_outer CHANGING cv_outer TABLES tt_outer.",
    "ENDFORM.",
    "FORM frm_local USING iv_local TYPE string.",
    "  CLEAR iv_local.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string is_inner TYPE ty_ctx",
    "               CHANGING cv_inner TYPE string",
    "               TABLES tt_inner.",
    "  iv_inner = iv_inner && '-x'.",
    "  is_inner-city = is_inner-name.",
    "  cv_inner = iv_inner.",
    "  APPEND iv_inner TO tt_inner.",
    "  IF iv_inner IS INITIAL OR is_inner-city IS NOT INITIAL.",
    "  ENDIF.",
    "  IF iv_inner = cv_inner AND is_inner-city = is_inner-name.",
    "  ENDIF.",
    "ENDFORM.",
    "FORM frm_literal USING iv_literal TYPE string.",
    "  CLEAR iv_literal.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  const performRoots = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
    .filter((obj) => obj && obj.objectType === "PERFORM");
  assert.strictEqual(performRoots.length, 4, "Expected two normal, one literal, and one external PERFORM root.");

  const findDescendant = (root, predicate) => {
    const stack = root ? [root] : [];
    while (stack.length) {
      const current = stack.shift();
      if (predicate(current)) {
        return current;
      }
      stack.unshift(...(Array.isArray(current && current.children) ? current.children : []));
    }
    return null;
  };
  const findByRaw = (root, raw) => findDescendant(root, (obj) => String(obj && obj.raw || "").trim() === raw);
  const getBindingNames = (obj, paramName) => {
    const binding = obj && obj.__abapPerformTraceBinding;
    const decls = binding && binding.byParamUpper && typeof binding.byParamUpper.get === "function"
      ? binding.byParamUpper.get(String(paramName || "").toUpperCase())
      : [];
    return Array.from(decls || [], (decl) => String(decl && decl.name || ""));
  };

  const firstIf = findByRaw(performRoots[0], "IF iv_inner IS INITIAL OR is_inner-city IS NOT INITIAL.");
  const secondIf = findByRaw(performRoots[1], "IF iv_inner IS INITIAL OR is_inner-city IS NOT INITIAL.");
  assert(firstIf && secondIf, "Expected each normal PERFORM path to expand into the inner IF.");
  assert.deepStrictEqual(getBindingNames(firstIf, "iv_inner"), ["iv_outer", "gv_root"]);
  assert.deepStrictEqual(getBindingNames(firstIf, "is_inner"), ["is_outer", "gs_root"]);
  assert.deepStrictEqual(getBindingNames(firstIf, "cv_inner"), ["cv_outer", "gv_change"]);
  assert.deepStrictEqual(getBindingNames(firstIf, "tt_inner"), ["tt_outer", "gt_root"]);
  assert.deepStrictEqual(getBindingNames(secondIf, "iv_inner"), ["iv_outer", "gv_root"]);
  assert.deepStrictEqual(getBindingNames(secondIf, "is_inner"), ["is_outer", "gs_root"]);

  const externalPerform = performRoots[3];
  assert.strictEqual(
    Array.isArray(externalPerform.children) ? externalPerform.children.length : 0,
    0,
    "Expected PERFORM IN PROGRAM to remain unresolved and unexpanded."
  );

  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // JSDOM may already expose a configurable size; virtual rendering still works for this small fixture.
  }
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const findTemplateTable = (obj) => {
    const idText = "#" + String(obj && obj.id || "");
    const block = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
      .find((candidate) => String(candidate.querySelector(".template-block-meta")?.textContent || "").includes(idText));
    return block ? block.querySelector("table.template-preview-table") : null;
  };
  const assertTemplateRows = (root, raw, expectedRows) => {
    const obj = findByRaw(root, raw);
    assert(obj, `Expected expanded object for ${raw}`);
    const table = findTemplateTable(obj);
    assert(table, `Expected Template preview table for ${raw}`);
    assert.deepStrictEqual(getTemplateTableRows(table), expectedRows);
  };

  assertTemplateRows(performRoots[0], "iv_inner = iv_inner && '-x'.", [
    ["Đích", "Nguồn"],
    ["gv_root", "gv_root && '-x'"]
  ]);
  assertTemplateRows(performRoots[0], "is_inner-city = is_inner-name.", [
    ["Đích", "Nguồn"],
    ["gs_root-city", "gs_root-name"]
  ]);
  assertTemplateRows(performRoots[0], "cv_inner = iv_inner.", [
    ["Đích", "Nguồn"],
    ["gv_change", "gv_root"]
  ]);
  assertTemplateRows(performRoots[0], "APPEND iv_inner TO tt_inner.", [
    ["APPEND", "gv_root"],
    ["TO", "gt_root"]
  ]);
  assertTemplateRows(performRoots[0], "IF iv_inner IS INITIAL OR is_inner-city IS NOT INITIAL.", [
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "Kết nối"],
    ["gv_root", "IS", "INITIAL", "OR"],
    ["gs_root-city", "IS", "NOT INITIAL"]
  ]);
  assertTemplateRows(performRoots[0], "IF iv_inner = cv_inner AND is_inner-city = is_inner-name.", [
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "Kết nối"],
    ["gv_root", "=", "gv_change", "AND"],
    ["gs_root-city", "=", "gs_root-name"]
  ]);
  assertTemplateRows(performRoots[1], "iv_inner = iv_inner && '-x'.", [
    ["Đích", "Nguồn"],
    ["gv_root", "gv_root && '-x'"]
  ]);
  assertTemplateRows(performRoots[1], "is_inner-city = is_inner-name.", [
    ["Đích", "Nguồn"],
    ["gs_root-city", "gs_root-name"]
  ]);
  assertTemplateRows(performRoots[0], "CLEAR iv_local.", [["CLEAR", "lv_local"]]);
  assertTemplateRows(performRoots[2], "CLEAR iv_literal.", [["CLEAR", "iv_literal"]]);

  try {
    Object.defineProperty(els.output, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // Keep the normal virtual viewport when JSDOM does not allow overriding it.
  }
  els.rightTabOutputBtn.click();
  await waitForViewerUi(window);

  const firstIfCard = Array.from(els.output.querySelectorAll(".card"))
    .find((card) => String(card.dataset.id || "") === String(firstIf.id));
  assert(firstIfCard, "Expected Output card for the first expanded IF.");
  const conditionRows = Array.from(firstIfCard.querySelectorAll(".extras table tbody tr"));
  assert.strictEqual(conditionRows.length, 2, "Expected two IF condition rows in Output.");
  const getDeclNamesFromCell = (cell) => Array.from(cell?.firstElementChild?.children || [], (line) => (
    String(line.textContent || "").trim()
  ));
  assert.deepStrictEqual(
    getDeclNamesFromCell(conditionRows[0].cells[5]),
    ["iv_inner", "iv_outer", "gv_root"],
    "Expected Output to keep the complete scalar trace chain."
  );
  assert.deepStrictEqual(
    getDeclNamesFromCell(conditionRows[1].cells[5]),
    ["is_inner-city", "is_outer-city", "gs_root-city"],
    "Expected Output to keep the complete structure-field trace chain."
  );

  dom.window.close();
}

async function assertGlobalPerformSourceSelection() {
  const source = [
    "DATA gv_first TYPE string.",
    "DATA gv_second TYPE string.",
    "DATA gv_third TYPE string.",
    "DATA gv_change_first TYPE string.",
    "DATA gv_change_second TYPE string.",
    "DATA gv_change_third TYPE string.",
    "PERFORM frm_outer USING gv_first CHANGING gv_change_first.",
    "PERFORM frm_outer USING gv_second CHANGING gv_change_second.",
    "PERFORM frm_outer USING gv_third CHANGING gv_change_third.",
    "PERFORM frm_literal USING 'X'.",
    "PERFORM frm_external IN PROGRAM zother USING gv_first.",
    "PERFORM frm_cycle USING gv_first.",
    "FORM frm_outer USING iv_outer TYPE string iv_missing TYPE string",
    "               CHANGING cv_outer TYPE string.",
    "  DATA lv_local TYPE string.",
    "  lv_local = iv_outer.",
    "  iv_missing = iv_outer.",
    "  PERFORM frm_inner USING iv_outer.",
    "  PERFORM frm_inner USING cv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  CLEAR iv_inner.",
    "ENDFORM.",
    "FORM frm_literal USING iv_literal TYPE string.",
    "  CLEAR iv_literal.",
    "ENDFORM.",
    "FORM frm_cycle USING iv_cycle TYPE string.",
    "  PERFORM frm_cycle USING iv_cycle.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  const registry = state.performSourceRegistry;
  assert(registry && registry.candidatesByFormUpper instanceof window.Map, "Expected a per-input PERFORM source registry.");
  assert(registry.selectedKeyByFormUpper instanceof window.Map, "Expected global FORM selections in runtime-only state.");

  const outerCandidates = registry.candidatesByFormUpper.get("FRM_OUTER") || [];
  const innerCandidates = registry.candidatesByFormUpper.get("FRM_INNER") || [];
  assert.strictEqual(outerCandidates.length, 3, "Expected all three top-level call sites to be indexed.");
  assert.strictEqual(innerCandidates.length, 6, "Expected two static nested calls under each of three ancestries.");
  assert.strictEqual(registry.selectedKeyByFormUpper.get("FRM_OUTER"), outerCandidates[0].key);
  assert.strictEqual(registry.selectedKeyByFormUpper.get("FRM_INNER"), innerCandidates[0].key);
  assert.deepStrictEqual(Array.from(outerCandidates[0].ancestry || []), []);
  assert.deepStrictEqual(Array.from(innerCandidates[0].ancestry || []), [outerCandidates[0].key]);
  assert.strictEqual(outerCandidates[0].lineStart, 7);
  assert.strictEqual(outerCandidates[0].bindingContext.bySection.USING.length, 2);
  assert.strictEqual(outerCandidates[0].bindingContext.bySection.USING[0].actualArg.value, "gv_first");
  assert.strictEqual(outerCandidates[0].bindingContext.bySection.USING[1].actualArg, null);
  assert.strictEqual(outerCandidates[0].bindingContext.bySection.CHANGING[0].actualArg.value, "gv_change_first");

  const firstNestedPerformId = innerCandidates[0].performId;
  const sameStaticNestedCandidates = innerCandidates.filter((candidate) => candidate.performId === firstNestedPerformId);
  assert.strictEqual(sameStaticNestedCandidates.length, 3, "Expected the same static nested call to stay distinct per ancestry.");
  assert.strictEqual(new Set(sameStaticNestedCandidates.map((candidate) => candidate.key)).size, 3);

  els.rightTabOutputBtn.click();
  await settleViewerUi(window);
  const getSourceSelect = (container, formUpper) => container.querySelector(
    `.perform-source-select[data-perform-form="${formUpper}"]`
  );
  const getSourceBadge = (container, formUpper) => container.querySelector(
    `.perform-source-badge[data-perform-form="${formUpper}"]`
  );
  let outputOuterSelect = getSourceSelect(els.output, "FRM_OUTER");
  assert(outputOuterSelect, "Expected Output PERFORM header source selector.");
  assert.strictEqual(getSourceBadge(els.output, "FRM_OUTER")?.textContent, "⇄ 3 nguồn");
  assert.strictEqual(outputOuterSelect.options.length, 3);
  assert.strictEqual(outputOuterSelect.options[0].textContent, "Nguồn 1/3 · line 7 · USING gv_first · CHANGING gv_change_first");
  assert.strictEqual(outputOuterSelect.options[1].textContent, "Nguồn 2/3 · line 8 · USING gv_second · CHANGING gv_change_second");

  els.rightTabTemplateBtn.click();
  await settleViewerUi(window);
  const templateOuterSelect = getSourceSelect(els.templatePreviewOutput, "FRM_OUTER");
  assert(templateOuterSelect, "Expected Template PERFORM header source selector.");
  assert.strictEqual(templateOuterSelect.value, outerCandidates[0].key);

  state.selectedId = String((state.renderObjects || []).find((obj) => obj && obj.objectType === "PERFORM")?.id || "");
  state.selectedTemplateIndex = "0";
  els.output.scrollTop = 120;
  els.templatePreviewOutput.scrollTop = 80;

  outputOuterSelect = getSourceSelect(els.output, "FRM_OUTER");
  outputOuterSelect.value = outerCandidates[1].key;
  outputOuterSelect.dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 10);

  assert.strictEqual(registry.selectedKeyByFormUpper.get("FRM_OUTER"), outerCandidates[1].key);
  assert.strictEqual(getSourceSelect(els.output, "FRM_OUTER")?.value, outerCandidates[1].key);
  assert.strictEqual(getSourceSelect(els.templatePreviewOutput, "FRM_OUTER")?.value, outerCandidates[1].key);
  assert.strictEqual(state.selectedId, String((state.renderObjects || []).find((obj) => obj && obj.objectType === "PERFORM")?.id || ""));
  assert.strictEqual(state.selectedTemplateIndex, "0");
  assert(Math.abs(els.output.scrollTop - 120) <= 40, "Expected Output logical viewport anchor to stay near its prior offset.");
  assert(Math.abs(els.templatePreviewOutput.scrollTop - 80) <= 40, "Expected Template logical viewport anchor to stay near its prior offset.");
  assert(els.output.querySelector(`.card.selected[data-id="${state.selectedId}"]`), "Expected Output selection anchor to survive rebuild.");
  assert(els.templatePreviewOutput.querySelector('.template-block.selected[data-template-index="0"]'), "Expected Template selection anchor to survive rebuild.");

  Object.defineProperty(els.output, "clientHeight", { configurable: true, value: 100000 });
  Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  runtime.api.renderOutput();
  runtime.api.renderTemplatePreview();
  await settleViewerUi(window, 8);

  const findDescendant = (root, predicate) => {
    const queue = root ? [root] : [];
    while (queue.length) {
      const current = queue.shift();
      if (predicate(current)) {
        return current;
      }
      queue.push(...(Array.isArray(current && current.children) ? current.children : []));
    }
    return null;
  };
  const getBindingNames = (obj, paramName) => {
    const binding = obj && obj.__abapPerformTraceBinding;
    const decls = binding && binding.byParamUpper instanceof window.Map
      ? binding.byParamUpper.get(String(paramName || "").toUpperCase())
      : [];
    return Array.from(decls || [], (decl) => String(decl && decl.name || ""));
  };
  const outerPerformRoots = (state.renderObjects || []).filter((obj) => (
    obj && obj.objectType === "PERFORM" && obj.extras?.performCall?.form === "frm_outer"
  ));
  assert.strictEqual(outerPerformRoots.length, 3);
  let firstLocalAssignment = null;
  let firstMissingAssignment = null;
  for (const root of outerPerformRoots) {
    const localAssignment = findDescendant(root, (obj) => String(obj && obj.raw || "").trim() === "lv_local = iv_outer.");
    const missingAssignment = findDescendant(root, (obj) => String(obj && obj.raw || "").trim() === "iv_missing = iv_outer.");
    assert(localAssignment && missingAssignment);
    firstLocalAssignment = firstLocalAssignment || localAssignment;
    firstMissingAssignment = firstMissingAssignment || missingAssignment;
    assert.deepStrictEqual(getBindingNames(localAssignment, "iv_outer"), ["gv_second"]);
    assert.strictEqual(localAssignment.values?.target?.decl?.name, "lv_local", "Expected local DATA to stay local.");
    assert.deepStrictEqual(getBindingNames(missingAssignment, "iv_missing"), [], "Expected missing actual argument to use local FORM_PARAM fallback.");
  }

  const findTemplateTable = (obj) => {
    const idText = `#${String(obj && obj.id || "")}`;
    const block = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
      .find((candidate) => String(candidate.querySelector(".template-block-meta")?.textContent || "").includes(idText));
    return block ? block.querySelector("table.template-preview-table") : null;
  };
  const firstLocalCard = els.output.querySelector(`[data-id="${String(firstLocalAssignment.id)}"]`);
  assert(firstLocalCard && String(firstLocalCard.textContent || "").includes("gv_second"), "Expected Output FORM params to use call-site 2 globally.");
  assert.deepStrictEqual(getTemplateTableRows(findTemplateTable(firstLocalAssignment)), [
    ["Đích", "Nguồn"],
    ["lv_local", "gv_second"]
  ]);
  assert.deepStrictEqual(getTemplateTableRows(findTemplateTable(firstMissingAssignment)), [
    ["Đích", "Nguồn"],
    ["iv_missing", "gv_second"]
  ]);

  let activeInnerCandidates = registry.getActiveCandidates("FRM_INNER");
  assert.strictEqual(activeInnerCandidates.length, 2, "Expected nested selector candidates only from the selected parent branch.");
  let outputInnerSelect = getSourceSelect(els.output, "FRM_INNER");
  assert(outputInnerSelect);
  assert.strictEqual(outputInnerSelect.options.length, 2);
  outputInnerSelect.value = activeInnerCandidates[1].key;
  outputInnerSelect.dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 10);
  assert.strictEqual(registry.selectedKeyByFormUpper.get("FRM_INNER"), activeInnerCandidates[1].key);

  const currentOuterSelect = getSourceSelect(els.output, "FRM_OUTER");
  currentOuterSelect.value = outerCandidates[2].key;
  currentOuterSelect.dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 10);
  activeInnerCandidates = registry.getActiveCandidates("FRM_INNER");
  assert.strictEqual(activeInnerCandidates.length, 2);
  assert.strictEqual(
    registry.selectedKeyByFormUpper.get("FRM_INNER"),
    activeInnerCandidates[0].key,
    "Expected changing a parent source to reset descendant FORM selection to the new branch default."
  );
  const innerClear = findDescendant(
    (state.renderObjects || []).find((obj) => obj && obj.objectType === "PERFORM" && obj.extras?.performCall?.form === "frm_outer"),
    (obj) => String(obj && obj.raw || "").trim() === "CLEAR iv_inner."
  );
  assert(innerClear);
  assert.deepStrictEqual(getBindingNames(innerClear, "iv_inner"), ["iv_outer", "gv_third"]);
  const innerClearCard = els.output.querySelector(`[data-id="${String(innerClear.id)}"]`);
  assert(innerClearCard && String(innerClearCard.textContent || "").includes("gv_third"), "Expected nested Output bindings to follow the new parent branch.");
  assert.deepStrictEqual(getTemplateTableRows(findTemplateTable(innerClear)), [["CLEAR", "gv_third"]]);

  const literalRoot = (state.renderObjects || []).find((obj) => obj && obj.extras?.performCall?.form === "frm_literal");
  const literalClear = findDescendant(literalRoot, (obj) => String(obj && obj.raw || "").trim() === "CLEAR iv_literal.");
  assert(literalClear);
  assert.deepStrictEqual(getBindingNames(literalClear, "iv_literal"), [], "Expected literal actual to keep local fallback.");
  const literalCard = els.output.querySelector(`[data-id="${String(literalRoot.id)}"]`);
  assert(literalCard);
  assert.strictEqual(
    getSourceSelect(literalCard, "FRM_LITERAL"),
    null,
    "Expected a FORM with only one source not to show a redundant selector."
  );
  assert.strictEqual(getSourceBadge(literalCard, "FRM_LITERAL"), null);

  const externalRoot = (state.renderObjects || []).find((obj) => obj && obj.extras?.performCall?.program === "zother");
  assert(externalRoot);
  assert.strictEqual(Array.isArray(externalRoot.children) ? externalRoot.children.length : 0, 0);
  const externalCard = els.output.querySelector(`[data-id="${String(externalRoot.id)}"]`);
  assert(externalCard);
  assert.strictEqual(getSourceSelect(externalCard, "FRM_EXTERNAL"), null);

  const cycleRoot = (state.renderObjects || []).find((obj) => obj && obj.extras?.performCall?.form === "frm_cycle");
  const recursivePerform = findDescendant(cycleRoot, (obj) => obj !== cycleRoot && obj?.extras?.performCall?.form === "frm_cycle");
  assert(recursivePerform, "Expected the recursive call statement itself to remain visible.");
  assert.strictEqual(Array.isArray(recursivePerform.children) ? recursivePerform.children.length : 0, 0, "Expected cycle guard to stop recursive expansion.");

  assert(!Object.prototype.hasOwnProperty.call(state.data, "performSourceRegistry"));
  assert(!JSON.stringify(state.data).includes("__abapPerformSource"));
  assert(!Array.from({ length: window.localStorage.length }, (_, index) => window.localStorage.key(index))
    .some((key) => /perform.*source/i.test(String(key || ""))), "Expected source selection not to be persisted.");

  els.parseBtn.click();
  await settleViewerUi(window, 10);
  assert.notStrictEqual(state.performSourceRegistry, registry, "Expected a new input/render to replace runtime selection state.");
  assert.strictEqual(state.performSourceRegistry.selectedKeyByFormUpper.get("FRM_OUTER"), state.performSourceRegistry.candidatesByFormUpper.get("FRM_OUTER")[0].key);

  dom.window.close();
}

async function assertPerformSourcesUseSourceOrderAndLazyLargeSelectors() {
  const mixedSource = [
    "DATA gv_outer TYPE string.",
    "DATA gv_direct TYPE string.",
    "PERFORM frm_outer USING gv_outer.",
    "PERFORM frm_inner USING gv_direct.",
    "FORM frm_outer USING iv_outer TYPE string.",
    "  PERFORM frm_inner USING iv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  CLEAR iv_inner.",
    "ENDFORM."
  ].join("\n");
  const mixedDom = await renderFixture(mixedSource);
  const mixedRuntime = mixedDom.window.AbapViewerRuntime;
  const mixedRegistry = mixedRuntime.state.performSourceRegistry;
  const mixedCandidates = mixedRegistry.candidatesByFormUpper.get("FRM_INNER") || [];
  assert.deepStrictEqual(
    Array.from(mixedCandidates, (candidate) => candidate.lineStart),
    [4, 6],
    "Expected default candidates to follow physical source-line order, not DFS expansion order."
  );
  assert.strictEqual(mixedRegistry.getSelectedCandidate("FRM_INNER")?.lineStart, 4);
  mixedDom.window.close();

  const definitionsFirstSource = [
    "DATA gv_one TYPE string.",
    "DATA gv_two TYPE string.",
    "FORM frm_outer USING iv_outer TYPE string.",
    "  PERFORM frm_inner USING iv_outer.",
    "  PERFORM frm_inner USING 'X'.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  CLEAR iv_inner.",
    "ENDFORM.",
    "PERFORM frm_outer USING gv_one.",
    "PERFORM frm_outer USING gv_two."
  ].join("\n");
  const definitionsFirstDom = await renderFixture(definitionsFirstSource);
  const definitionsFirstRegistry = definitionsFirstDom.window.AbapViewerRuntime.state.performSourceRegistry;
  assert.strictEqual(definitionsFirstRegistry.getSelectedCandidate("FRM_OUTER")?.lineStart, 10);
  assert.strictEqual(
    definitionsFirstRegistry.getSelectedCandidate("FRM_INNER")?.lineStart,
    4,
    "Expected parent selection to initialize before choosing a nested FORM source when definitions appear first."
  );
  definitionsFirstDom.window.close();

  const callCount = 120;
  const largeLines = ["DATA gv_shared TYPE string."];
  for (let index = 1; index <= callCount; index += 1) {
    largeLines.push("PERFORM frm_many USING gv_shared.");
  }
  largeLines.push(
    "FORM frm_many USING iv_value TYPE string.",
    "  CLEAR iv_value.",
    "ENDFORM."
  );
  const largeDom = await renderFixture(largeLines.join("\n"));
  const { window } = largeDom;
  const { els, state } = window.AbapViewerRuntime;
  const largeCandidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM_MANY") || [];
  assert.strictEqual(largeCandidates.length, callCount);
  const visibleSelect = els.templatePreviewOutput.querySelector(
    '.perform-source-select[data-perform-form="FRM_MANY"]'
  );
  assert(visibleSelect, "Expected a visible source selector for the large FORM registry.");
  assert.strictEqual(
    visibleSelect.options.length,
    1,
    "Expected a large source selector to defer option creation instead of duplicating every source per visible call."
  );
  visibleSelect.dispatchEvent(new window.Event("focus"));
  assert.strictEqual(visibleSelect.options.length, callCount, "Expected focusing the selector to populate all sources on demand.");
  largeDom.window.close();
}

async function assertStructFieldFinalDescNormalizesParentOnly() {
  const source = [
    "TYPES: BEGIN OF ty_order,",
    "         item TYPE string,",
    "         BEGIN OF address,",
    "           city TYPE string,",
    "         END OF address,",
    "       END OF ty_order.",
    "DATA lds_order TYPE ty_order.",
    "DATA lv_result TYPE string.",
    "lv_result = lds_order-item && '-x'.",
    "PERFORM frm_outer USING lds_order CHANGING lv_result.",
    "FORM frm_outer USING ids_outer TYPE ty_order CHANGING cv_outer TYPE string.",
    "  PERFORM frm_inner USING ids_outer CHANGING cv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING ids_inner TYPE ty_order CHANGING cv_inner TYPE string.",
    "  cv_inner = ids_inner-item.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const { getEffectiveDeclDesc, getFinalDeclDesc, resolveValueLevelFinalDesc } = runtime.api;
  const decls = Array.isArray(state.data && state.data.decls) ? state.data.decls : [];
  const findDecl = (name) => decls.find((decl) => String(decl && decl.name || "") === name);
  const parentDecl = findDecl("lds_order");
  const itemDecl = findDecl("lds_order-item");
  const nestedItemDecl = findDecl("lds_order-address-city");

  assert(parentDecl && itemDecl && nestedItemDecl, "Expected parent, direct item, and nested item declarations.");
  assert.strictEqual(typeof window.getDeclOverrideStorageKey, "function");

  state.settings.nameTemplatesByCode.DS = "PARENT[{{desc}}]";
  state.settings.structDescTemplate = "{{struct}}-{{item}}";
  state.descOverrides[window.getDeclOverrideStorageKey(parentDecl)] = "Đơn hàng";
  state.descOverrides[window.getDeclOverrideStorageKey(itemDecl)] = "Mặt hàng";
  state.descOverrides[window.getDeclOverrideStorageKey(nestedItemDecl)] = "Địa chỉ-Thành phố";

  assert.strictEqual(getFinalDeclDesc(parentDecl), "PARENT[Đơn hàng]");
  assert.strictEqual(
    getFinalDeclDesc(itemDecl),
    "PARENT[Đơn hàng]-Mặt hàng",
    "Expected only the parent portion of a structure field finalDesc to use name normalization."
  );
  assert.strictEqual(
    getFinalDeclDesc(nestedItemDecl),
    "PARENT[Đơn hàng]-Địa chỉ-Thành phố",
    "Expected nested structure items not to apply name normalization."
  );

  const directAssignment = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
    .find((obj) => String(obj && obj.raw || "").trim() === "lv_result = lds_order-item && '-x'.");
  assert(directAssignment && directAssignment.values && directAssignment.values.expr, "Expected direct assignment expression.");
  assert.strictEqual(
    resolveValueLevelFinalDesc(directAssignment.values.expr),
    "PARENT[Đơn hàng]-Mặt hàng && '-x'",
    "Expected expression finalDesc to preserve the expression while normalizing only the field parent."
  );

  assert.strictEqual(
    getEffectiveDeclDesc(itemDecl),
    "PARENT[Đơn hàng]-PARENT[Mặt hàng]",
    "Expected the existing .desc behavior to remain unchanged."
  );
  assert.strictEqual(
    /\{[^{}]*\.desc\}/i.test(JSON.stringify(state.templateConfig && state.templateConfig.templates || {})),
    false,
    "Expected committed Viewer templates to keep using finalDesc rather than desc."
  );

  const performRoot = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
    .find((obj) => obj && obj.objectType === "PERFORM");
  assert(performRoot, "Expected expanded PERFORM root.");
  const findDescendantByRaw = (root, raw) => {
    const stack = root ? [root] : [];
    while (stack.length) {
      const current = stack.shift();
      if (String(current && current.raw || "").trim() === raw) {
        return current;
      }
      stack.unshift(...(Array.isArray(current && current.children) ? current.children : []));
    }
    return null;
  };
  const tracedAssignment = findDescendantByRaw(performRoot, "cv_inner = ids_inner-item.");
  assert(tracedAssignment, "Expected structure-field assignment inside expanded PERFORM.");

  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // The small fixture still renders with JSDOM's default virtual viewport.
  }
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  const tracedBlock = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
    .find((block) => String(block.querySelector(".template-block-meta")?.textContent || "")
      .includes(`#${tracedAssignment.id}`));
  assert(tracedBlock, "Expected Template block for traced structure-field assignment.");
  assert.deepStrictEqual(getTemplateTableRows(tracedBlock.querySelector("table.template-preview-table")), [
    ["Đích", "Nguồn"],
    ["lv_result", "PARENT[Đơn hàng]-Mặt hàng"]
  ]);

  dom.window.close();
}

async function assertConstantInitializersAndEmptyTableBodiesShapeFinalDesc() {
  const source = [
    "CONSTANTS gc_max TYPE i VALUE 20. \"Maximum rows",
    "CONSTANTS gc_initial TYPE string VALUE IS INITIAL. \"Initial marker",
    "DATA lt_source TYPE TABLE OF string. \"Source table",
    "DATA lt_target TYPE TABLE OF string. \"Target table",
    "DATA lv_line TYPE string. \"Line",
    "DATA lv_total TYPE i. \"Total",
    "lv_total = gc_max + 1.",
    "lt_target[] = lt_source[].",
    "lv_line = lt_source[ 1 ].",
    "lv_line = '[]'."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { state } = runtime;
  const { getFinalDeclDesc, resolveValueLevelFinalDesc } = runtime.api;
  const decls = Array.isArray(state.data && state.data.decls) ? state.data.decls : [];
  const findDecl = (name) => decls.find((decl) => String(decl && decl.name || "") === name);
  const constantDecl = findDecl("gc_max");
  const initialDecl = findDecl("gc_initial");

  state.settings.normalizeDeclDesc = false;
  assert(constantDecl && initialDecl, "Expected constant declarations.");
  assert.strictEqual(getFinalDeclDesc(constantDecl), "20");
  assert.strictEqual(getFinalDeclDesc(initialDecl), "IS INITIAL");

  const objects = Array.isArray(state.renderObjects) ? state.renderObjects : [];
  const findByRaw = (raw) => objects.find((obj) => String(obj && obj.raw || "").trim() === raw);
  const constantAssignment = findByRaw("lv_total = gc_max + 1.");
  assert(constantAssignment && constantAssignment.values && constantAssignment.values.expr);
  assert.strictEqual(resolveValueLevelFinalDesc(constantAssignment.values.expr), "20 + 1");

  state.descOverrides[window.getDeclOverrideStorageKey(constantDecl)] = "Manual maximum";
  assert.strictEqual(
    resolveValueLevelFinalDesc(constantAssignment.values.expr),
    "Manual maximum + 1",
    "Expected an explicit Description override to stay above the constant initializer."
  );

  const tableBodyAssignment = findByRaw("lt_target[] = lt_source[].");
  assert(tableBodyAssignment && tableBodyAssignment.values);
  assert.strictEqual(tableBodyAssignment.values.target.value, "lt_target[]");
  assert.strictEqual(tableBodyAssignment.values.expr.value, "lt_source[]");
  assert.strictEqual(resolveValueLevelFinalDesc(tableBodyAssignment.values.target), "Target table");
  assert.strictEqual(resolveValueLevelFinalDesc(tableBodyAssignment.values.expr), "Source table");

  const tableExpressionAssignment = findByRaw("lv_line = lt_source[ 1 ].");
  assert(tableExpressionAssignment && tableExpressionAssignment.values && tableExpressionAssignment.values.expr);
  assert.strictEqual(
    resolveValueLevelFinalDesc(tableExpressionAssignment.values.expr),
    "Source table[ 1 ]",
    "Expected a non-empty table expression to keep its brackets."
  );

  const literalAssignment = findByRaw("lv_line = '[]'.");
  assert(literalAssignment && literalAssignment.values && literalAssignment.values.expr);
  assert.strictEqual(resolveValueLevelFinalDesc(literalAssignment.values.expr), "'[]'");

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

  const assignment = state.templateConfig.templates.ASSIGNMENT;
  assert.strictEqual(assignment.A1.text, "Đích");
  assert.strictEqual(assignment.U1.text, "Nguồn");
  assert.strictEqual(assignment.A2.text, "{values.target.finalDesc}");
  assert.strictEqual(assignment.U2.text, "{values.expr.finalDesc}");
  for (const rangeKey of ["A1:T1", "U1:AN1", "A2:T2", "U2:AN2"]) {
    assert(assignment[rangeKey], `Expected ASSIGNMENT range ${rangeKey}.`);
  }

  for (const templateKey of ["IF", "ELSEIF"]) {
    const conditionTemplate = state.templateConfig.templates[templateKey];
    for (const rangeKey of [
      "A1:T1", "U1:AN1", "AO1:BH1", "BI1:CB1",
      "A2:T2", "U2:AN2", "AO2:BH2", "BI2:CB2"
    ]) {
      assert(conditionTemplate[rangeKey], `Expected ${templateKey} range ${rangeKey}.`);
    }
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
    const expectedCellCount = ["IF", "ELSEIF"].includes(templateKey) ? 80 : 40;
    for (const row of Array.from(table.querySelectorAll("tr"))) {
      assert.strictEqual(row.querySelectorAll("td").length, expectedCellCount, `${templateKey} row width mismatch.`);
    }
  }

  const readTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert.deepStrictEqual(getTemplateTableRows(readTable), [
    ["READ TABLE", "lt_rows"],
    ["WITH KEY", "table_line = lv_a"],
    ["INTO", "ls_row"]
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
    ["WHERE", "bname = p_user"]
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
  for (const templateKey of STATEMENT_TEMPLATE_KEYS) {
    assert(
      Object.prototype.hasOwnProperty.call(state.templateConfig.templates, templateKey),
      `Expected legacy import migration to add ${templateKey}.`
    );
  }

  dom.window.close();
}

async function assertViewerFixture(fileName) {
  const fixturePath = path.join(fixturesDir, fileName);
  const baselinePath = path.join(baselineDir, fileName.replace(/\.abap$/i, ".json"));
  const allowedPath = path.join(allowedDir, fileName.replace(/\.abap$/i, ".json"));
  assert(fs.existsSync(baselinePath), `Missing viewer baseline for ${fileName}. Run npm run build:baselines.`);
  assert(fs.existsSync(allowedPath), `Missing viewer allowed-delta manifest for ${fileName}.`);

  const source = fs.readFileSync(fixturePath, "utf8");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const els = runtime.els;
  const state = runtime.state;
  const actual = normalizeViewerState(window);
  const expected = readJson(baselinePath);
  const allowed = readJson(allowedPath);
  const diffs = filterDiffsByAllowedPaths(diffJson(expected, actual), allowed.allowedPaths);

  assert.deepStrictEqual(diffs, [], `${fileName}: viewer contract mismatch.\n${JSON.stringify(diffs.slice(0, 20), null, 2)}`);

  if (fileName === "multi-statement-navigation.abap") {
    const dataObjects = Array.isArray(state.renderObjects)
      ? state.renderObjects.filter((obj) => obj && obj.objectType === "DATA")
      : [];
    const segmentIndexes = Array.from(dataObjects, (obj) => Number(obj && obj.segmentIndex));
    const lineStarts = Array.from(dataObjects, (obj) => Number(obj && obj.lineStart));
    assert.deepStrictEqual(
      segmentIndexes,
      [0, 1],
      "Expected DATA statements on the same line to keep segment indexes 0 and 1."
    );
    assert.deepStrictEqual(
      lineStarts,
      [3, 3],
      "Expected both DATA statements to stay on the same source line."
    );

    els.rightTabDescBtn.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const secondRow = els.declDescTable.querySelectorAll("tbody tr")[1];
    assert(secondRow, "Expected second descriptions row for multi-statement fixture.");
    const techCell = secondRow.querySelector("td:nth-child(3) div div");
    assert(techCell, "Expected clickable technical id cell.");
    techCell.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const selectedText = String(els.inputText.value || "").slice(els.inputText.selectionStart, els.inputText.selectionEnd).trim();
    assert.strictEqual(selectedText, "DATA lv_b TYPE i.", "Expected navigation selection to land exactly on the second DATA statement.");

    els.rightTabTemplateBtn.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const secondTemplateBlock = els.templatePreviewOutput.querySelector('.template-block[data-template-index="1"]');
    assert(secondTemplateBlock, "Expected the second template block to exist.");
    const codeButton = secondTemplateBlock.querySelector('button[data-template-action="code"]');
    assert(codeButton, "Expected template block to expose a code button.");
    codeButton.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const templateSelectedText = String(els.inputText.value || "").slice(els.inputText.selectionStart, els.inputText.selectionEnd).trim();
    assert.strictEqual(templateSelectedText, "DATA lv_b TYPE i.", "Expected template code button to jump to the second DATA statement.");

    const editableCell = els.templatePreviewOutput.querySelector("td.template-preview-editable");
    assert(editableCell, "Expected template preview to expose editable cells.");
    const previewTable = els.templatePreviewOutput.querySelector(".template-preview-table");
    assert(previewTable, "Expected template preview block to render a table.");
    const previewEditableCells = Array.from(previewTable.querySelectorAll("td.template-preview-editable"));
    const tabbablePreviewCells = previewEditableCells.filter((cell) => Number(cell.tabIndex) === 0);
    assert.strictEqual(
      tabbablePreviewCells.length,
      1,
      "Expected each Template preview table to expose only one Tab stop."
    );
    assert(
      /[A-Z]+\d+/.test(String(tabbablePreviewCells[0].getAttribute("aria-label") || "")),
      "Expected the editable Template cell label to include its grid coordinate."
    );
    assert(previewEditableCells.length > 1, "Expected enough Template cells to test arrow-key navigation.");
    previewEditableCells[0].dispatchEvent(new window.KeyboardEvent("keydown", { key: "ArrowRight", bubbles: true }));
    assert.strictEqual(previewEditableCells[0].tabIndex, -1, "Expected ArrowRight to move the active Template cell.");
    assert.strictEqual(previewEditableCells[1].tabIndex, 0, "Expected ArrowRight to activate the next Template cell.");
    assert.strictEqual(window.document.activeElement, previewEditableCells[1], "Expected ArrowRight to focus the next Template cell.");
    const squareCell = Array.from(els.templatePreviewOutput.querySelectorAll("td.template-preview-editable"))
      .find((cell) => Number(cell.rowSpan || 1) === 1 && Number(cell.colSpan || 1) === 1);
    assert(squareCell, "Expected at least one non-merged editable template cell.");
    const squareCellText = squareCell.firstElementChild;
    assert(squareCellText, "Expected square template cell to render inner text wrapper.");
    const squareWidth = Number.parseFloat(squareCell.style.width || "");
    const squareHeight = Number.parseFloat(squareCell.style.height || "");
    assert.strictEqual(squareWidth, squareHeight, "Expected square template cells to keep equal width and height.");
    assert.strictEqual(
      String(squareCell.style.textOverflow || "").trim(),
      "",
      "Expected square template cells not to use ellipsis clipping."
    );
    assert.notStrictEqual(
      String(squareCell.style.overflow || "").trim().toLowerCase(),
      "hidden",
      "Expected square template cells not to hide overflow."
    );
    assert.notStrictEqual(
      String(previewTable.style.minWidth || "").trim().toLowerCase(),
      "100%",
      "Expected square-cell tables not to force full-width stretching."
    );
    assert.strictEqual(
      String(squareCell.style.padding || "").trim(),
      "0px",
      "Expected square template cells to remove padding so overflow text starts from the full cell width."
    );
    assert.strictEqual(
      String(squareCellText.style.whiteSpace || "").trim().toLowerCase(),
      "nowrap",
      "Expected square template cell text to stay on one line when overflowing."
    );
    assert.strictEqual(
      String(squareCellText.style.overflow || "").trim().toLowerCase(),
      "visible",
      "Expected square template cell text to show overflow instead of clipping."
    );
    assert.strictEqual(
      String(squareCellText.style.pointerEvents || "").trim().toLowerCase(),
      "none",
      "Expected square template cell text wrapper not to block cell editing interactions."
    );
    assert.strictEqual(
      String(squareCellText.style.textOverflow || "").trim().toLowerCase(),
      "clip",
      "Expected square template cell text wrapper not to use ellipsis clipping."
    );
    editableCell.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    assert.strictEqual(Boolean(findVisibleTemplateEditModal(window)), false, "Expected single click on editable template cell to stay in preview mode.");
    editableCell.dispatchEvent(new window.MouseEvent("dblclick", { bubbles: true }));
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    assert.strictEqual(Boolean(findVisibleTemplateEditModal(window)), true, "Expected double click on editable template cell to open the editor modal.");
    if (typeof window.closeTemplateDynamicModal === "function") {
      window.closeTemplateDynamicModal();
    } else if (els.editModal) {
      els.editModal.hidden = true;
    }

    editableCell.dispatchEvent(new window.KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    assert.strictEqual(Boolean(findVisibleTemplateEditModal(window)), true, "Expected Enter on editable template cell to open the editor modal.");

    if (typeof window.closeTemplateDynamicModal === "function") {
      window.closeTemplateDynamicModal();
    } else if (els.editModal) {
      els.editModal.hidden = true;
    }

    els.templateApplyBtn.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const configPage = findVisibleTemplateConfigPage(window);
    assert(configPage, "Expected Template Form page to open.");
    const activeConfigPage = configPage;
    assert(activeConfigPage.querySelector(".template-config-builder"), "Expected Template Form page to expose the drag-drop builder.");
    assert.strictEqual(
      Boolean(activeConfigPage.querySelector(".template-config-ranges-table")),
      false,
      "Expected drag-drop builder to replace the old range table form as the primary UI."
    );

    const builderGrid = activeConfigPage.querySelector(".template-builder-grid");
    assert(builderGrid, "Expected Template Builder to expose an editable grid.");
    assert.strictEqual(
      String(builderGrid.style.getPropertyValue("--template-builder-cell-size") || "").trim(),
      "18px",
      "Expected Template Builder cells to match Template Preview default square cell size."
    );
    const paletteText = activeConfigPage.querySelector('[data-template-builder-tool="text"]');
    const palettePlaceholder = activeConfigPage.querySelector('[data-template-builder-tool="placeholder"]');
    const paletteFormatBlue = activeConfigPage.querySelector('[data-template-builder-tool="format-blue"]');
    const paletteFormatWhite = activeConfigPage.querySelector('[data-template-builder-tool="format-white"]');
    assert(paletteText, "Expected Template Builder palette to expose Text.");
    assert(palettePlaceholder, "Expected Template Builder palette to expose Placeholder.");
    assert(paletteFormatBlue, "Expected Template Builder palette to expose format-blue.");
    assert(paletteFormatWhite, "Expected Template Builder palette to expose format-white.");

    function dispatchPointer(type, target) {
      target.dispatchEvent(new window.MouseEvent(type, { bubbles: true, cancelable: true, button: 0 }));
    }

    function dispatchDrop(target, tool) {
      const event = new window.Event("drop", { bubbles: true, cancelable: true });
      Object.defineProperty(event, "dataTransfer", {
        value: {
          dropEffect: "copy",
          effectAllowed: "copy",
          getData: () => tool,
          setData: () => {}
        }
      });
      target.dispatchEvent(event);
    }

    let cellH8 = activeConfigPage.querySelector('.template-builder-grid td[data-row="8"][data-col="8"]');
    assert(cellH8, "Expected builder grid cell H8.");
    dispatchPointer("pointerdown", cellH8);
    dispatchPointer("pointerup", window.document);
    await new Promise((resolve) => window.setTimeout(resolve, 0));

    cellH8 = activeConfigPage.querySelector('.template-builder-grid td[data-row="8"][data-col="8"]');
    assert(cellH8, "Expected builder grid cell H8 after selecting it.");
    dispatchDrop(cellH8, "text");
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    let builderTextArea = activeConfigPage.querySelector("textarea.template-builder-textarea");
    assert(builderTextArea, "Expected builder inspector text area.");
    assert.strictEqual(String(builderTextArea.value || ""), "Text", "Expected dropping Text to set default text on selected cell.");

    cellH8 = activeConfigPage.querySelector('.template-builder-grid td[data-row="8"][data-col="8"]');
    dispatchDrop(cellH8, "placeholder");
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    builderTextArea = activeConfigPage.querySelector("textarea.template-builder-textarea");
    assert(/\{[^{}]+\}/.test(String(builderTextArea.value || "")), "Expected dropping Placeholder to insert a {path} token.");

    let cellA1 = activeConfigPage.querySelector('.template-builder-grid td[data-row="1"][data-col="1"]');
    let cellC2 = activeConfigPage.querySelector('.template-builder-grid td[data-row="2"][data-col="3"]');
    assert(cellA1, "Expected builder grid cell A1.");
    assert(cellC2, "Expected builder grid cell C2.");
    dispatchPointer("pointerdown", cellA1);
    cellC2.dispatchEvent(new window.MouseEvent("pointerenter", { bubbles: true, cancelable: true }));
    dispatchPointer("pointerup", window.document);
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const rangeInput = activeConfigPage.querySelector(".template-builder-inspector input.template-config-cell-input");
    assert(rangeInput, "Expected builder inspector range input.");
    assert.strictEqual(String(rangeInput.value || ""), "A1:C2", "Expected pointer drag to select range A1:C2.");

    cellA1 = activeConfigPage.querySelector('.template-builder-grid td[data-row="1"][data-col="1"]');
    dispatchDrop(cellA1, "format-blue");
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const mergeToggle = Array.from(activeConfigPage.querySelectorAll("label.toggle"))
      .find((label) => String(label.textContent || "").includes("Merge"))
      ?.querySelector('input[type="checkbox"]');
    assert(mergeToggle, "Expected builder inspector to expose Merge toggle.");
    mergeToggle.checked = true;
    mergeToggle.dispatchEvent(new window.Event("change", { bubbles: true }));
    await new Promise((resolve) => window.setTimeout(resolve, 0));

    const backgroundInput = Array.from(activeConfigPage.querySelectorAll(".template-builder-field"))
      .find((field) => String(field.textContent || "").includes("Background"))
      ?.querySelector("input.template-config-cell-input");
    assert(backgroundInput, "Expected builder inspector to expose Background input.");
    const applyButton = Array.from(activeConfigPage.querySelectorAll("button"))
      .find((btn) => String(btn.textContent || "").trim() === "Apply");
    assert(applyButton, "Expected Template Form modal to expose an Apply button.");

    backgroundInput.value = "not-a-color";
    backgroundInput.dispatchEvent(new window.Event("input", { bubbles: true }));
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const configError = String(activeConfigPage.querySelector(".template-error")?.textContent || "").trim();
    assert(configError.includes("hex color"), "Expected invalid color input to show a validation error.");

    applyButton.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    assert.strictEqual(Boolean(findVisibleTemplateConfigPage(window)), true, "Expected Template Form modal to stay open after invalid color apply.");

    const validBackgroundInput = Array.from(activeConfigPage.querySelectorAll(".template-builder-field"))
      .find((field) => String(field.textContent || "").includes("Background"))
      ?.querySelector("input.template-config-cell-input");
    assert(validBackgroundInput, "Expected Background input to still be available after invalid apply.");
    validBackgroundInput.value = "#123abc";
    validBackgroundInput.dispatchEvent(new window.Event("input", { bubbles: true }));
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    applyButton.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    assert.strictEqual(Boolean(findVisibleTemplateConfigPage(window)), false, "Expected Template Form modal to close after valid builder apply.");
    assert(state.templateConfig.templates.DEFAULT["A1:C2"], "Expected builder to save selected A1:C2 range.");
    assert.strictEqual(state.templateConfig.templates.DEFAULT["A1:C2"].background, "#123abc");
    assert.strictEqual(state.templateConfig.templates.DEFAULT["A1:C2"].border, "outside-thin");
    assert.strictEqual(state.templateConfig.templates.DEFAULT["A1:C2"]["font color"], "#111111");
    assert.strictEqual(state.templateConfig.templates.DEFAULT["A1:C2"].merge, true);

  }

  if (fileName === "perform-trace-struct.abap") {
    const performNode = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
      .find((obj) => obj && obj.objectType === "PERFORM");
    assert(performNode, "Expected PERFORM node in renderObjects.");
    assert(Array.isArray(performNode.children) && performNode.children.length > 0, "Expected expanded FORM children under PERFORM.");

    const ifNode = performNode.children.find((obj) => obj && obj.objectType === "IF");
    assert(ifNode, "Expected expanded IF node inside PERFORM.");
    assert(ifNode.values && ifNode.values.condition && ifNode.values.condition.decl, "Expected IF condition decl inside expanded PERFORM.");
    assert.strictEqual(ifNode.values.condition.decl.name, "is_ctx-uname");
    assert.strictEqual(ifNode.values.condition.decl.file, "input.abap");
    assert.strictEqual(ifNode.values.condition.decl.lineStart, 17);

    const assignmentNode = ifNode.children.find((obj) => obj && obj.objectType === "ASSIGNMENT");
    assert(assignmentNode, "Expected assignment inside expanded IF.");
    assert(assignmentNode.values && assignmentNode.values.target && assignmentNode.values.target.decl, "Expected assignment target decl.");
    assert.strictEqual(assignmentNode.values.target.decl.name, "is_ctx-city");
    assert.strictEqual(assignmentNode.values.target.decl.file, "input.abap");
    assert.strictEqual(assignmentNode.values.target.decl.lineStart, 18);
  }

  dom.window.close();
}

async function assertMessageAndWriteViewerContracts() {
  const source = [
    "DATA lv_message TYPE string. \"Message text",
    "DATA lv_first TYPE string. \"First value",
    "DATA lv_output TYPE string. \"Output value",
    "DATA lv_destination TYPE string. \"Destination",
    "DATA lv_column TYPE i. \"Column",
    "DATA lv_length TYPE i. \"Length",
    "DATA lv_currency TYPE string. \"Currency",
    "MESSAGE lv_message TYPE lv_type WITH lv_first 'fixed' DISPLAY LIKE lv_like INTO lv_target RAISING static_error.",
    "MESSAGE 'literal message' TYPE 'I'.",
    "MESSAGE e001.",
    "WRITE AT /lv_column(lv_length) lv_output TO lv_destination NO-GAP CURRENCY lv_currency USING EDIT MASK '==XX'.",
    "WRITE 5(10) lv_output.",
    "WRITE 'literal output'.",
    ""
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  assert(state.templateConfig.templates.MESSAGE, "Expected an explicit MESSAGE default template.");
  assert(state.templateConfig.templates.WRITE, "Expected an explicit WRITE default template.");
  const renderObjects = Array.isArray(state.renderObjects) ? state.renderObjects : [];
  assert.strictEqual(renderObjects.filter((obj) => obj && obj.objectType === "MESSAGE").length, 3);
  assert.strictEqual(renderObjects.filter((obj) => obj && obj.objectType === "WRITE").length, 3);

  Object.defineProperty(els.templatePreviewOutput, "clientHeight", {
    configurable: true,
    get() {
      return 100000;
    }
  });
  els.rightTabTemplateBtn.click();
  await settleViewerUi(window);

  const messageTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="MESSAGE"]'));
  const writeTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="WRITE"]'));
  assert.strictEqual(messageTables.length, 3, "Expected dynamic, literal, and static-reference MESSAGE blocks.");
  assert.strictEqual(writeTables.length, 3, "Expected positioned, numeric-position, and literal WRITE blocks.");

  const messageRows = getTemplateTableRows(messageTables[0]);
  assert(messageRows.some((row) => row.includes("MESSAGE") && row.includes("Message text")), "MESSAGE primary row must map to values.message.");
  assert(messageRows.some((row) => row.includes("TYPE") && row.includes("lv_type")));
  assert(messageRows.some((row) => row.includes("WITH") && row.includes("First value")), "WITH operands must expand to editable rows.");
  assert(messageRows.some((row) => row.includes("WITH") && row.includes("'fixed'")), "Literal WITH operands must remain visible.");
  assert(messageRows.some((row) => row.includes("DISPLAY LIKE") && row.includes("lv_like")));
  assert(messageRows.some((row) => row.includes("INTO") && row.includes("lv_target")));
  assert(messageRows.some((row) => row.includes("RAISING") && row.includes("static_error")));

  const writeRows = getTemplateTableRows(writeTables[0]);
  assert(writeRows.some((row) => row.includes("WRITE") && row.includes("Output value")), "WRITE primary row must map to values.output.");
  assert(writeRows.some((row) => row.includes("AT") && row.join(" ").includes("Column") && row.join(" ").includes("Length")), "WRITE position operands must retain both edit targets.");
  assert(writeRows.some((row) => row.includes("TO") && row.includes("Destination")));
  assert(writeRows.some((row) => row.includes("NO-GAP")), "Flag formatting must stay visible.");
  assert(writeRows.some((row) => row.includes("CURRENCY") && row.includes("Currency")), "Formatting data operands must retain provenance.");
  assert(writeRows.some((row) => row.includes("USING EDIT MASK") && row.includes("'==XX'")), "Literal format operands must stay visible.");
  const numericPositionRows = getTemplateTableRows(writeTables[1]);
  assert(
    numericPositionRows.some((row) => row.includes("AT") && row.includes("5(10)")),
    "WRITE with an omitted AT keyword must still render its numeric position."
  );
  assert(numericPositionRows.some((row) => row.includes("WRITE") && row.includes("Output value")));

  const messageCell = findTemplateCellByText(messageTables[0], "Message text");
  assert(messageCell, "Expected editable MESSAGE primary cell.");
  const messageCandidates = messageCell.__templateCellMeta && messageCell.__templateCellMeta.declCandidates;
  assert(Array.isArray(messageCandidates) && messageCandidates.length > 0);
  assert.strictEqual(String(messageCandidates[0].name || ""), "lv_message");
  const messageDeclKey = window.getDeclOverrideStorageKey(messageCandidates[0]);
  let modal = await openTemplateCellDescriptionTab(window, messageCell);
  await saveTemplateCellDescription(window, modal, "Edited message");
  assert.strictEqual(String(state.descOverrides[messageDeclKey] || ""), "Edited message");
  assert(String(els.output.textContent || "").includes("Edited message"), "MESSAGE save must refresh Output.");
  assert(findTemplateCellByText(els.templatePreviewOutput, "Edited message"), "MESSAGE save must refresh Template.");

  const refreshedMessageTable = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="MESSAGE"]'))[0];
  modal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(refreshedMessageTable, "Edited message"));
  await saveTemplateCellDescription(window, modal, "");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, messageDeclKey), false);
  assert(findTemplateCellByText(els.templatePreviewOutput, "Message text"), "MESSAGE clear must restore declaration description.");

  const literalMessageCell = findTemplateCellByText(messageTables[1], "'literal message'");
  assert.strictEqual(literalMessageCell && literalMessageCell.__templateCellMeta && literalMessageCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");
  modal = await openTemplateCellDescriptionTab(window, literalMessageCell);
  assert(modal.querySelector("textarea.template-config-json").disabled, "Literal MESSAGE must stay locked.");

  const literalWriteCell = findTemplateCellByText(writeTables[2], "'literal output'");
  assert.strictEqual(literalWriteCell && literalWriteCell.__templateCellMeta && literalWriteCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");
  modal = await openTemplateCellDescriptionTab(window, literalWriteCell);
  assert(modal.querySelector("textarea.template-config-json").disabled, "Literal WRITE output must stay locked.");

  const staticReferenceCell = findTemplateCellByText(messageTables[2], "e001");
  assert(staticReferenceCell, "Expected the short MESSAGE reference cell.");
  assert.strictEqual(staticReferenceCell.__templateCellMeta && staticReferenceCell.__templateCellMeta.reasonCode, "NON_DECL_SCHEMA_VALUE");
  assert.deepStrictEqual(Array.from(staticReferenceCell.__templateCellMeta && staticReferenceCell.__templateCellMeta.declCandidates || []), []);
  modal = await openTemplateCellDescriptionTab(window, staticReferenceCell);
  assert(modal.querySelector("textarea.template-config-json").disabled, "Short static MESSAGE reference must not create PATH_DECL.");

  const literalWithCell = findTemplateCellByText(messageTables[0], "'fixed'");
  assert.strictEqual(literalWithCell && literalWithCell.__templateCellMeta && literalWithCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");
  const literalMaskCell = findTemplateCellByText(writeTables[0], "'==XX'");
  assert.strictEqual(literalMaskCell && literalMaskCell.__templateCellMeta && literalMaskCell.__templateCellMeta.reasonCode, "LITERAL_NO_DECL");

  els.rightTabOutputBtn.click();
  await settleViewerUi(window);
  els.output.scrollTop = 2200;
  els.output.dispatchEvent(new window.Event("scroll"));
  await settleViewerUi(window, 10);
  const outputText = String(els.output.textContent || "");
  assert(outputText.includes("with[1]"), "Output Values must expose each MESSAGE WITH operand.");
  assert(outputText.includes("format[1].NO-GAP"), "Output Values must expose WRITE formatting rows.");
  assert(outputText.includes("format[2].CURRENCY"), "Output Values must retain formatting operand provenance.");

  dom.window.close();
}

async function main() {
  const focus = String(process.argv[2] || "").trim();
  if (!focus || focus === "fixtures") {
    for (const fileName of listFixtureFiles(fixturesDir)) {
      await assertViewerFixture(fileName);
    }
  }
  if (!focus || focus === "render-error") {
    await assertFailedRenderClearsPreviousResult();
  }
  if (!focus || focus === "template-import-error") {
    await assertTemplateImportErrorIsVisible();
  }
  if (!focus || focus === "config-export") {
    await assertGroupedConfigExportIsDeterministic();
    await assertGroupedConfigRoundTripsStateStorageAndDom();
    await assertGroupedConfigImportValidatesAndRollsBack();
  }
  if (!focus || focus === "template-reset") {
    await assertTemplateResetCanBeCancelled();
  }
  if (!focus || focus === "if-template") {
    await assertIfInitialUsesConditionTemplate();
  }
  if (!focus || focus === "template-configs") {
    await assertStatementSpecificTwentyCellTemplates();
    await assertLegacyTemplateImportAddsMissingSpecificConfigs();
  }
  if (!focus || focus === "perform-root-trace") {
    await assertExpandedPerformTraceUsesRootDeclarations();
  }
  if (!focus || focus === "perform-source-selection") {
    await assertGlobalPerformSourceSelection();
    await assertPerformSourcesUseSourceOrderAndLazyLargeSelectors();
  }
  if (!focus || focus === "struct-field-finaldesc") {
    await assertStructFieldFinalDescNormalizesParentOnly();
  }
  if (!focus || focus === "constant-finaldesc") {
    await assertConstantInitializersAndEmptyTableBodiesShapeFinalDesc();
  }
  if (!focus || focus === "template-multi-value-perform-call") {
    await assertPerformAndCallMultiValueRows();
  }
  if (!focus || focus === "template-multi-value-perform-root") {
    await assertExpandedPerformMultiValueRowsUseRootDescriptions();
  }
  if (!focus || focus === "template-multi-value-conditions") {
    await assertConditionListsExpandRows();
  }
  if (!focus || focus === "template-multi-value-safe-lists") {
    await assertSafeRawListsExpandWithoutSplittingExpressions();
  }
  if (!focus || focus === "template-row-description-loop") {
    await assertTemplateRowDescriptionEditsLocalLoopDecl();
  }
  if (!focus || focus === "template-row-description-perform") {
    await assertTemplateRowDescriptionKeepsNestedPerformTrace();
    await assertTemplatePerformSourceEditDoesNotBleedAcrossSources();
  }
  if (!focus || focus === "template-row-description-condition") {
    await assertTemplateRowDescriptionTargetsExactConditionDecls();
  }
  if (!focus || focus === "template-provenance") {
    await assertTemplateAppendDeclaredOperandsPreferRealDeclarations();
    await assertTemplateAppendUnboundOperandsUseCanonicalTargets();
    await assertLegacyPathAliasUsesTemplateIndexAfterPerformExpansion();
    await assertTemplateAppendLiteralKeepsOnlyTargetEditable();
    await assertTemplateIfArrayProvenanceIsPerRenderedLine();
    await assertTemplateIndexedAndCompositePlaceholdersKeepProvenance();
    await assertTemplateSemanticFallbacksMatchOutputPaths();
    await assertTemplateDirectSchemaPathsStayLocked();
    await assertTemplateFallbackAllowlistCoversExistingItabOperands();
    await assertTemplateResolverWarnsOnceWithCellMetadata();
  }
  if (!focus || focus === "message-write") {
    await assertMessageAndWriteViewerContracts();
  }
  if (!focus || focus === "scroll-navigation") {
    await assertInputPitchAndCodeNavigationUseNativeTextareaMetrics();
    await assertVirtualGutterJumpSettlesInOneClick();
    await assertManualScrollCancelsPendingVirtualAlignment();
    await assertHeterogeneousVirtualRangesKeepViewportCovered();
    await assertSplitterRefreshesActiveVirtualGeometry();
    await assertBlankViewportFallbackUsesLogicalAnchor();
    await assertDescriptionSaveAndClearPreserveTemplateCellAnchor();
    await assertDescriptionEditPreservesOutputSelectedItemAnchor();
  }
  if (focus === "scroll-manual-takeover") {
    await assertManualScrollCancelsPendingVirtualAlignment();
  }
  if (focus === "scroll-range-coverage") {
    await assertHeterogeneousVirtualRangesKeepViewportCovered();
  }
  if (focus === "scroll-geometry") {
    await assertSplitterRefreshesActiveVirtualGeometry();
    await assertBlankViewportFallbackUsesLogicalAnchor();
  }
  console.log("viewer-contracts: ok");
}

main().catch((err) => {
  console.error(err && err.stack ? err.stack : err);
  process.exitCode = 1;
});
