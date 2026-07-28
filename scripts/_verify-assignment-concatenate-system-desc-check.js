"use strict";

const assert = require("assert");
const {
  findTemplateCellByText,
  getDeclOverrideStorageKeyFromRuntime,
  getTemplateTableRows,
  openTemplateCellDescriptionTab,
  renderFixture,
  saveTemplateCellDescription,
  waitForViewerUi
} = require("../tests/helpers/viewer-contract-test-helpers");

function fail(message) {
  console.error(`FAIL: ${message}`);
  process.exit(1);
}

function findTableByObject(els, obj) {
  const idText = `#${String(obj && obj.id || "")}`;
  const block = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
    .find((candidate) => String(candidate.querySelector(".template-block-meta")?.textContent || "").includes(idText));
  return block ? block.querySelector("table.template-preview-table") : null;
}

async function checkAssignmentRows() {
  const source = [
    "DATA a TYPE i.",
    "DATA b TYPE i.",
    "DATA c TYPE i.",
    "DATA d TYPE i.",
    "DATA e TYPE i.",
    "a = b + c + d + e."
  ].join("\n");
  const dom = await renderFixture(source);
  const { els, state } = dom.window.AbapViewerRuntime;
  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // ignore
  }
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(dom.window);
  const multi = (state.renderObjects || []).find((obj) => String(obj && obj.raw || "").trim() === "a = b + c + d + e.");
  assert(multi, "assignment object");
  const rows = getTemplateTableRows(findTableByObject(els, multi));
  assert.deepStrictEqual(rows, [
    ["Đích", "a"],
    ["Nguồn", "b"],
    ["Nguồn", "c"],
    ["Nguồn", "d"],
    ["Nguồn", "e"]
  ]);
  dom.window.close();
  console.log("PASS: assignment multi-operand rows");
}

async function checkConcatenateRows() {
  const source = [
    "DATA lv_a TYPE string.",
    "DATA lv_b TYPE string.",
    "DATA lv_c TYPE string.",
    "DATA lv_d TYPE string.",
    "CONCATENATE lv_a lv_b lv_c INTO lv_d."
  ].join("\n");
  const dom = await renderFixture(source);
  const { els, state } = dom.window.AbapViewerRuntime;
  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // ignore
  }
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(dom.window);
  const obj = (state.renderObjects || []).find((entry) => String(entry && entry.objectType || "") === "CONCATENATE");
  assert(obj, "concatenate object");
  assert.strictEqual(String(obj.values.sources.value || ""), "lv_a lv_b lv_c");
  const rows = getTemplateTableRows(findTableByObject(els, obj));
  assert(rows.some((row) => row[0] === "CONCATENATE" && row[1] === "lv_a"));
  assert(rows.some((row) => row[0] === "CONCATENATE" && row[1] === "lv_b"));
  assert(rows.some((row) => row[0] === "CONCATENATE" && row[1] === "lv_c"));
  assert(rows.some((row) => /INTO/i.test(String(row[0] || "")) && row[1] === "lv_d"));
  dom.window.close();
  console.log("PASS: concatenate multi-source rows");
}

async function checkSyTabixEditable() {
  const source = [
    "DATA lt_tab TYPE TABLE OF string.",
    "DATA ls_row TYPE string.",
    "LOOP AT lt_tab INTO ls_row FROM INDEX sy-tabix.",
    "ENDLOOP."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // ignore
  }
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  const loopTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="LOOP_AT_ITAB"]');
  assert(loopTable, "loop table");
  const fromCell = findTemplateCellByText(loopTable, "sy-tabix")
    || findTemplateCellByText(loopTable, "SY-TABIX")
    || findTemplateCellByText(loopTable, "Current table index");
  assert(fromCell, "sy-tabix cell");
  const systemKey = getDeclOverrideStorageKeyFromRuntime(window, fromCell.__templateCellMeta.declCandidates[0]);
  assert.strictEqual(systemKey, "SYSTEM:SY-TABIX");
  const modal = await openTemplateCellDescriptionTab(window, fromCell);
  await saveTemplateCellDescription(window, modal, "Chi so bang");
  assert.strictEqual(String(state.descOverrides[systemKey] || ""), "Chi so bang");
  els.rightTabDescBtn.click();
  await waitForViewerUi(window);
  assert(/SY-TABIX/i.test(String(els.declDescTable.textContent || "")), "data catalog lists SY-TABIX");
  dom.window.close();
  console.log("PASS: sy-tabix editable");
}

async function main() {
  try {
    await checkAssignmentRows();
    await checkConcatenateRows();
    await checkSyTabixEditable();
    console.log("PASS: verify-assignment-concatenate-system-desc");
  } catch (err) {
    fail(err && err.stack ? err.stack : String(err));
  }
}

main();
