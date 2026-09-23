"use strict";

const fs = require("fs");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  allowedDir,
  assert,
  assertViewerFixtureDirectoriesStayInSync,
  baselineDir,
  diffJson,
  filterDiffsByAllowedPaths,
  findVisibleTemplateEditModal,
  findVisibleTemplateConfigPage,
  fixturesDir,
  loadAllowedPaths,
  normalizeViewerState,
  path,
  readJson,
  renderFixture,
  waitForViewerUi
} = require("./helpers/viewer-contract-test-helpers");

async function assertViewerFixture(fileName) {
  const fixturePath = path.join(fixturesDir, fileName);
  const baselinePath = path.join(baselineDir, fileName.replace(/\.abap$/i, ".json"));
  const allowedPath = path.join(allowedDir, fileName.replace(/\.abap$/i, ".json"));
  assert(fs.existsSync(baselinePath), `Missing viewer baseline for ${fileName}. Run npm run build:baselines.`);

  const source = fs.readFileSync(fixturePath, "utf8");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const els = runtime.els;
  const state = runtime.state;
  const actual = normalizeViewerState(window);
  const expected = readJson(baselinePath);
  const diffs = filterDiffsByAllowedPaths(diffJson(expected, actual), loadAllowedPaths(allowedPath));

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
    const techCell = secondRow.querySelector("td:nth-child(2) > div > div");
    assert(techCell, "Expected clickable technical id cell.");
    techCell.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const selectedText = String(els.inputText.value || "").slice(els.inputText.selectionStart, els.inputText.selectionEnd).trim();
    assert.strictEqual(selectedText, "DATA lv_b TYPE i.", "Expected navigation selection to land exactly on the second DATA statement.");

    els.rightTabTemplateBtn.click();
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    const secondDataObject = dataObjects[1];
    const secondTemplateBlock = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
      .find((block) => Number(block.getAttribute("data-template-index")) === state.renderObjects.indexOf(secondDataObject));
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
    assert.strictEqual(Array.isArray(performNode.children) ? performNode.children.length : 0, 0, "Expected PERFORM to remain a leaf statement.");
    const formNode = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
      .find((obj) => obj && obj.objectType === "FORM");
    assert(formNode, "Expected the FORM definition in renderObjects.");

    const ifNode = formNode.children.find((obj) => obj && obj.objectType === "IF");
    assert(ifNode, "Expected IF node inside the source-shaped FORM.");
    assert(ifNode.values && ifNode.values.condition && ifNode.values.condition.decl, "Expected IF condition decl inside FORM.");
    assert.strictEqual(ifNode.values.condition.decl.name, "is_ctx-uname");
    assert.strictEqual(ifNode.values.condition.decl.file, "input.abap");
    assert.strictEqual(ifNode.values.condition.decl.lineStart, 17);

    const assignmentNode = ifNode.children.find((obj) => obj && obj.objectType === "ASSIGNMENT");
    assert(assignmentNode, "Expected assignment inside IF.");
    assert(assignmentNode.values && assignmentNode.values.target && assignmentNode.values.target.decl, "Expected assignment target decl.");
    assert.strictEqual(assignmentNode.values.target.decl.name, "is_ctx-city");
    assert.strictEqual(assignmentNode.values.target.decl.file, "input.abap");
    assert.strictEqual(assignmentNode.values.target.decl.lineStart, 18);
  }

  dom.window.close();
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

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);
  assert(
    String(els.declDescTable.textContent || "").includes("No data loaded."),
    "Expected switching to Data after a failed Render not to restore stale declarations."
  );
  assert(
    String(els.error.textContent || "").includes("JSON parse error"),
    "Expected switching tabs not to erase the Render error."
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

defineFocusedTest(test, "viewer fixture contracts", ["fixtures"], async (t) => {
  const fixtureFiles = assertViewerFixtureDirectoriesStayInSync();

  await t.test("renders every viewer fixture baseline", async (fixtureSuite) => {
    for (const fileName of fixtureFiles) {
      await fixtureSuite.test(`matches ${fileName}`, async () => {
        await assertViewerFixture(fileName);
      });
    }
  });
});

defineFocusedTest(test, "viewer render error contract", ["render-error"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("failed render clears previous result", async () => {
    await assertFailedRenderClearsPreviousResult();
  });
});

defineFocusedTest(test, "viewer template import error contract", ["template-import-error"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template import error is visible", async () => {
    await assertTemplateImportErrorIsVisible();
  });
});
