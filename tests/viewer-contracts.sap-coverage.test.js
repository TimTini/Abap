"use strict";

const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assert,
  findTemplateCellByText,
  getDeclOverrideStorageKeyFromRuntime,
  getTemplateTableRows,
  openTemplateCellDescriptionTab,
  renderFixture,
  saveTemplateCellDescription,
  waitForViewerUi
} = require("./helpers/viewer-contract-test-helpers");

defineFocusedTest(test, "Viewer renders SAP READ TABLE, grouped LOOP and SELECT additions", ["sap-viewer-coverage"], async () => {
  const source = [
    "DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.",
    "DATA lv_package TYPE i.",
    "DATA lv_carrier TYPE string.",
    "DATA lv_skip TYPE string. \"Skip marker",
    "SELECT * INTO TABLE lt_rows PACKAGE SIZE lv_package FROM sflight.",
    "ENDSELECT.",
    "SELECT SINGLE carrid INTO lv_carrier FROM scarr.",
    "SELECT DISTINCT carrid INTO TABLE lt_rows FROM scarr.",
    "READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<line>) WHERE table_line <> lv_skip AND table_line IS NOT INITIAL.",
    "LOOP AT lt_rows INTO DATA(row) GROUP BY ( key = row+0(1) size = GROUP SIZE index = GROUP INDEX ) ASCENDING INTO DATA(group).",
    "  LOOP AT GROUP group INTO DATA(member) WHERE table_line <> lv_skip.",
    "    WRITE member.",
    "  ENDLOOP.",
    "ENDLOOP."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const objects = state.data.objects;
  const read = objects.find((object) => object.objectType === "READ_TABLE");
  const selects = objects.filter((object) => object.objectType === "SELECT");
  const packageSelect = selects.find((object) => /PACKAGE SIZE/i.test(object.raw));
  const groupedLoop = objects.find((object) => object.objectType === "LOOP_AT_ITAB" && /GROUP BY/i.test(object.raw));
  const memberLoop = groupedLoop && groupedLoop.children.find((object) => /LOOP AT GROUP/i.test(object.raw));

  assert(read, "Expected the parser result to contain READ TABLE.");
  assert.equal(read.extras.readTable.whereConditions.length, 2);
  assert.equal(selects.length, 3);
  assert.deepEqual(selects.map((object) => object.values.fields.value), ["*", "carrid", "carrid"]);
  assert(packageSelect, "Expected the parser result to contain the classic PACKAGE SIZE query.");
  assert.equal(packageSelect.values.packageSize.value, "lv_package");
  assert(groupedLoop, "Expected the parser result to contain the group loop.");
  assert.equal(groupedLoop.extras.loopAtItab.groupByRaw, "( key = row+0(1) size = GROUP SIZE index = GROUP INDEX ) ASCENDING");
  assert(memberLoop, "Expected LOOP AT GROUP to remain nested under its group loop.");
  assert.equal(memberLoop.extras.loopAtItab.group, "group");

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  const readTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  const selectTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="SELECT"]'));
  const loopTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="LOOP_AT_ITAB"]'));
  const groupTable = loopTables.find((table) => getTemplateTableRows(table).some((row) => row.join(" ").includes("GROUP BY")));
  const memberTable = loopTables.find((table) => table !== groupTable);
  assert(readTable, "Expected the Viewer to render a READ TABLE Template.");
  assert.equal(selectTables.length, 3, "Expected the Viewer to render all three SELECT variants.");
  assert(groupTable, "Expected the Viewer to render a grouped LOOP Template.");
  assert(memberTable, "Expected the Viewer to render a nested LOOP AT GROUP Template.");

  const readRows = getTemplateTableRows(readTable);
  const packageCells = getTemplateTableRows(selectTables[0]).flat().filter((cell) => cell.includes("lv_package"));
  const groupRows = getTemplateTableRows(groupTable);
  const memberRows = getTemplateTableRows(memberTable);
  assert(readRows.some((row) => row.includes("WHERE")), "Expected READ TABLE's WHERE section in Template.");
  assert(readRows.some((row) => row.includes("table_line") && row.includes("Skip marker")), "Expected editable WHERE operands in Template.");
  assert.equal(packageCells.length, 1, "Expected PACKAGE SIZE's operand to render once, without leaking into SELECT fields.");
  assert(groupRows.some((row) => row.includes("GROUP BY") && row.join(" ").includes("GROUP SIZE")), "Expected group-key syntax in Template.");
  assert(memberRows.some((row) => row.join(" ").includes("LOOP AT GROUP")), "Expected the nested loop's LOOP AT GROUP addition in Template.");
  assert(memberRows.some((row) => row.includes("group")), "Expected the group-result target in the nested loop Template.");
  assert(memberRows.some((row) => row.includes("WHERE")), "Expected the nested member filter clause in Template.");
  assert(memberRows.some((row) => row.join(" ").includes("Skip marker")), "Expected the nested member filter operand in Template.");

  const declaration = read.extras.readTable.whereConditions[0].rightOperandDecl;
  assert.equal(declaration.name, "lv_skip");
  const overrideKey = getDeclOverrideStorageKeyFromRuntime(window, declaration);
  assert(overrideKey, "Expected the WHERE operand to have a stable declaration override key.");
  const cell = findTemplateCellByText(readTable, "Skip marker");
  assert(cell, "Expected the WHERE operand description to be editable from Template.");
  const modal = await openTemplateCellDescriptionTab(window, cell);
  await saveTemplateCellDescription(window, modal, "Skip token");
  assert.equal(state.descOverrides[overrideKey], "Skip token");
  const updatedReadTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert(findTemplateCellByText(updatedReadTable, "Skip token"), "Expected saving a WHERE operand description to refresh Template.");

  const refreshedCell = findTemplateCellByText(updatedReadTable, "Skip token");
  const clearModal = await openTemplateCellDescriptionTab(window, refreshedCell);
  await saveTemplateCellDescription(window, clearModal, "");
  assert.equal(Object.hasOwn(state.descOverrides, overrideKey), false);
  const clearedReadTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert(findTemplateCellByText(clearedReadTable, "Skip marker"), "Expected clearing the override to restore the source description.");
});
