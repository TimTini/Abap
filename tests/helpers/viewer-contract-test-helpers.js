"use strict";

const assert = require("assert");
const path = require("path");
const {
  assertJsonArtifactsMatchFixtures,
  diffJson,
  filterDiffsByAllowedPaths,
  listFixtureFiles,
  loadAllowedPaths,
  normalizeViewerState,
  readJson
} = require("./contracts");
const { renderFixture } = require("./viewer-harness");

const fixturesDir = path.resolve(__dirname, "..", "fixtures", "viewer");
const baselineDir = path.resolve(__dirname, "..", "baselines", "viewer");
const allowedDir = path.resolve(__dirname, "..", "allowed-deltas", "viewer");

function assertViewerFixtureDirectoriesStayInSync() {
  const fixtureFiles = listFixtureFiles(fixturesDir);
  assertJsonArtifactsMatchFixtures(baselineDir, fixtureFiles, "viewer baseline");
  assertJsonArtifactsMatchFixtures(allowedDir, fixtureFiles, "viewer allowed-delta", { optional: true });
  return fixtureFiles;
}

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

function getDeclOverrideStorageKeyFromRuntime(window, decl) {
  const runtime = window && window.AbapViewerRuntime ? window.AbapViewerRuntime : null;
  const descriptions = runtime && runtime.services ? runtime.services.descriptions : null;
  return descriptions && typeof descriptions.getDeclOverrideStorageKey === "function"
    ? descriptions.getDeclOverrideStorageKey(decl)
    : "";
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

function getTemplateTableRows(table) {
  return Array.from(table.querySelectorAll("tr")).map((row) => (
    Array.from(row.querySelectorAll("td"))
      .map((cell) => String(cell.textContent || "").trim())
      .filter(Boolean)
  ));
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

async function waitForViewerUi(window) {
  await new Promise((resolve) => window.setTimeout(resolve, 0));
}

function findDataDeclGroup(els, scopeLabel) {
  const expectedScope = String(scopeLabel || "").trim().toUpperCase();
  return Array.from(els.declDescTable.querySelectorAll(".data-decl-group"))
    .find((group) => String(group.getAttribute("data-scope-label") || "").trim().toUpperCase() === expectedScope);
}

function findDataDeclRow(group, declName) {
  const expectedName = String(declName || "").trim().toUpperCase();
  return Array.from(group ? group.querySelectorAll("tbody tr[data-decl-name]") : [])
    .find((row) => String(row.getAttribute("data-decl-name") || "").trim().toUpperCase() === expectedName);
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

module.exports = {
  allowedDir,
  assert,
  assertJsonArtifactsMatchFixtures,
  assertViewerFixtureDirectoriesStayInSync,
  baselineDir,
  cloneTestJson,
  diffJson,
  filterDiffsByAllowedPaths,
  findDataDeclGroup,
  findDataDeclRow,
  findTemplateCellByText,
  findVisibleConfigExportModal,
  findVisibleTemplateConfigPage,
  findVisibleTemplateEditModal,
  fixturesDir,
  getDeclOverrideStorageKeyFromRuntime,
  getTemplateTableRows,
  installVirtualLayoutMock,
  listFixtureFiles,
  loadAllowedPaths,
  normalizeViewerState,
  openTemplateCellDescriptionTab,
  path,
  readJson,
  renderFixture,
  saveTemplateCellDescription,
  settleViewerUi,
  STATEMENT_TEMPLATE_KEYS,
  VIEWER_CONFIG_SECTION_KEYS,
  VIEWER_CONFIG_STORAGE_KEYS,
  waitForViewerUi
};
