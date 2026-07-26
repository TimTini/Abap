"use strict";

const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assert,
  assertViewerFixtureDirectoriesStayInSync,
  findTemplateCellByText,
  getDeclOverrideStorageKeyFromRuntime,
  getTemplateTableRows,
  openTemplateCellDescriptionTab,
  path,
  renderFixture,
  saveTemplateCellDescription,
  settleViewerUi,
  waitForViewerUi
} = require("./helpers/viewer-contract-test-helpers");

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
  assert.strictEqual(typeof window.AbapViewerRuntime.services.descriptions.getDeclOverrideStorageKey, "function");

  state.settings.nameTemplatesByCode.DS = "PARENT[{{desc}}]";
  state.settings.structDescTemplate = "{{struct}}-{{item}}";
  state.descOverrides[getDeclOverrideStorageKeyFromRuntime(window, parentDecl)] = "Đơn hàng";
  state.descOverrides[getDeclOverrideStorageKeyFromRuntime(window, itemDecl)] = "Mặt hàng";
  state.descOverrides[getDeclOverrideStorageKeyFromRuntime(window, nestedItemDecl)] = "Địa chỉ-Thành phố";

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

  const innerForm = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
    .find((obj) => obj && obj.objectType === "FORM" && obj.extras?.form?.name === "frm_inner");
  assert(innerForm, "Expected source-shaped inner FORM root.");
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
  const tracedAssignment = findDescendantByRaw(innerForm, "cv_inner = ids_inner-item.");
  assert(tracedAssignment, "Expected structure-field assignment inside FORM.");

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

  state.descOverrides[getDeclOverrideStorageKeyFromRuntime(window, constantDecl)] = "Manual maximum";
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
    ["INTO", "ls_row"],
    ["WITH KEY", "="],
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "="],
    ["col1", "=", "gv_a", "AND"],
    ["col2", "=", "gv_b"]
  ]);

  const modifyTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="MODIFY_ITAB"]');
  assert.deepStrictEqual(getTemplateTableRows(modifyTable), [
    ["MODIFY", "lt_rows"],
    ["FROM", "ls_row"],
    ["TRANSPORTING", "col1"],
    ["TRANSPORTING", "col2"],
    ["WHERE", "="],
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "="],
    ["col1", "=", "gv_a", "AND"],
    ["col2", "=", "gv_b"]
  ]);

  const selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert.deepStrictEqual(getTemplateTableRows(selectTable), [
    ["SELECT", "col1"],
    ["SELECT", "col2"],
    ["FROM", "dbtab"],
    ["INTO TABLE", "@lt_rows"],
    ["WHERE", "="],
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "="],
    ["col1", "=", "@gv_a", "AND"],
    ["col2", "=", "@gv_b"],
    ["HAVING", "="],
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "="],
    ["col3", "=", "@gv_a", "OR"],
    ["col4", "=", "@gv_b"]
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
  assert(loopTable, "Expected the source-shaped FORM to render its LOOP template.");
  let modal = await openTemplateCellDescriptionTab(window, findTemplateCellByText(loopTable, "Table local"));
  const modalText = String(modal.textContent || "");
  assert(!modalText.includes("Khong tim thay decl cho o nay."), "Expected the LOOP table row to retain its declaration target.");
  assert(modalText.includes("lt_abc"), "Expected the LOOP table row to target lt_abc.");
  assert(!modalText.includes("ls_abc"), "Expected the LOOP table row not to target the INTO declaration.");

  const tableDecl = (state.data && Array.isArray(state.data.decls) ? state.data.decls : [])
    .find((decl) => String(decl && decl.name || "") === "lt_abc");
  assert(tableDecl, "Expected the parser result to contain the local table declaration.");
  const tableDeclKey = getDeclOverrideStorageKeyFromRuntime(window, tableDecl);

  await saveTemplateCellDescription(window, modal, "Updated list");
  const storedOverride = state.descOverrides[tableDeclKey];
  assert.strictEqual(
    typeof storedOverride === "object" ? String(storedOverride.text || "") : String(storedOverride || ""),
    "Updated list",
    "Expected Save to persist the exact local table override."
  );
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
    `Expected a single PERFORM call chain not to show Description Target select. Keys: ${cellCandidates.map((decl) => getDeclOverrideStorageKeyFromRuntime(window, decl)).join(", ")}`
  );
  assert(String(modal.textContent || "").includes("gv_root_two"), "Expected Description tab to show the root decl label.");

  await saveTemplateCellDescription(window, modal, "Root two edited");
  const rootTwoDecl = cellCandidates.find((decl) => String(decl && decl.name || "").toLowerCase() === "gv_root_two");
  const rootTwoKey = getDeclOverrideStorageKeyFromRuntime(window, rootTwoDecl);
  assert(rootTwoKey, "Expected a storage key for gv_root_two.");
  const storedRoot = state.descOverrides[rootTwoKey];
  assert.strictEqual(
    typeof storedRoot === "object" ? String(storedRoot.text || "") : String(storedRoot || ""),
    "Root two edited",
    "Expected save to persist the root declaration override."
  );
  for (const decl of cellCandidates) {
    assert.strictEqual(
      getDeclOverrideStorageKeyFromRuntime(window, decl),
      rootTwoKey,
      "Expected every cloned declaration in the PERFORM chain to share the scoped override key."
    );
  }
  const globalFormParam = (state.data.decls || []).find((decl) => String(decl && decl.name || "").toLowerCase() === "iv_outer_two");
  const globalFormParamKey = getDeclOverrideStorageKeyFromRuntime(window, globalFormParam);
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
  const sourceThreeKeys = new Set(sourceThreeDecls.map((decl) => getDeclOverrideStorageKeyFromRuntime(window, decl)));
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
    const globalKey = getDeclOverrideStorageKeyFromRuntime(window, globalDecl);
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
  const sourceOneKey = getDeclOverrideStorageKeyFromRuntime(window, clearCell.__templateCellMeta.declCandidates[0]);
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
    `Expected source 3 override to return after source switching. Text=${String(clearCell.textContent || "")} Key=${getDeclOverrideStorageKeyFromRuntime(window, returnedSourceThreeDecl)} Effective=${window.AbapViewerRuntime.api.getEffectiveDeclDesc(returnedSourceThreeDecl)}`
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
    "DATA gv_a TYPE string. \"A",
    "DATA gv_b TYPE string. \"B",
    "DATA lt_rows TYPE TABLE OF string.",
    "DATA ls_row TYPE string.",
    "READ TABLE lt_rows WITH KEY rk1 = gv_a rk2 = gv_b INTO ls_row.",
    "SELECT col1 FROM dbtab INTO TABLE @lt_rows WHERE wk1 = @gv_a AND wk2 = @gv_b."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  let readTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert(readTable, "Expected the READ TABLE condition template.");
  const readLeftRk2 = findTemplateCellByText(readTable, "rk2");
  assert(readLeftRk2, "Expected READ TABLE second-clause left operand.");
  assert.strictEqual(
    readLeftRk2.__templateCellMeta && readLeftRk2.__templateCellMeta.sourcePath,
    "extras.readTable.conditions[1].leftOperandDecl.finalDesc"
  );
  const readLeftRk1 = findTemplateCellByText(readTable, "rk1");
  assert(readLeftRk1, "Expected READ TABLE first-clause left operand.");
  const readKey1 = getDeclOverrideStorageKeyFromRuntime(window, readLeftRk1.__templateCellMeta.declCandidates[0]);
  const readKey2 = getDeclOverrideStorageKeyFromRuntime(window, readLeftRk2.__templateCellMeta.declCandidates[0]);
  assert.notStrictEqual(readKey1, readKey2, "Expected READ TABLE clause left operands to use distinct storage keys.");

  const readModal = await openTemplateCellDescriptionTab(window, readLeftRk2);
  const readModalText = String(readModal.textContent || "");
  assert(readModalText.includes("rk2"), "Expected READ TABLE second row to target rk2.");
  assert(!readModalText.includes("rk1"), "Expected READ TABLE second row not to target rk1.");
  await saveTemplateCellDescription(window, readModal, "rk2 edited");
  assert.strictEqual(String(state.descOverrides[readKey2] || ""), "rk2 edited");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, readKey1), false);
  readTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="READ_TABLE"]');
  assert(findTemplateCellByText(readTable, "rk1"), "Expected READ TABLE first left operand unchanged.");
  assert(findTemplateCellByText(readTable, "rk2 edited"), "Expected only READ TABLE second left operand refreshed.");

  let selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert(selectTable, "Expected the SELECT condition template.");
  const selectLeftWk2 = findTemplateCellByText(selectTable, "wk2");
  assert(selectLeftWk2, "Expected SELECT second WHERE left operand.");
  assert.strictEqual(
    selectLeftWk2.__templateCellMeta && selectLeftWk2.__templateCellMeta.sourcePath,
    "extras.select.whereConditions[1].leftOperandDecl.finalDesc"
  );
  const selectLeftWk1 = findTemplateCellByText(selectTable, "wk1");
  assert(selectLeftWk1, "Expected SELECT first WHERE left operand.");
  const selectKey1 = getDeclOverrideStorageKeyFromRuntime(window, selectLeftWk1.__templateCellMeta.declCandidates[0]);
  const selectKey2 = getDeclOverrideStorageKeyFromRuntime(window, selectLeftWk2.__templateCellMeta.declCandidates[0]);
  assert.notStrictEqual(selectKey1, selectKey2, "Expected SELECT WHERE clause left operands to use distinct storage keys.");
  assert.notStrictEqual(selectKey2, readKey2, "Expected SELECT and READ synthetic condition keys to stay distinct.");

  const selectModal = await openTemplateCellDescriptionTab(window, selectLeftWk2);
  await saveTemplateCellDescription(window, selectModal, "wk2 edited");
  assert.strictEqual(String(state.descOverrides[selectKey2] || ""), "wk2 edited");
  assert.strictEqual(String(state.descOverrides[readKey2] || ""), "rk2 edited", "Expected READ override to stay isolated.");
  selectTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="SELECT"]');
  assert(findTemplateCellByText(selectTable, "wk1"), "Expected SELECT first WHERE left operand unchanged.");
  assert(findTemplateCellByText(selectTable, "wk2 edited"), "Expected only SELECT second WHERE left operand refreshed.");

  const selectRightB = findTemplateCellByText(selectTable, "@B");
  assert(selectRightB, "Expected SELECT WHERE right operand for @gv_b.");
  assert.strictEqual(
    selectRightB.__templateCellMeta && selectRightB.__templateCellMeta.status,
    "editable",
    "Expected SELECT WHERE right operand to stay editable."
  );
  assert.strictEqual(
    selectRightB.__templateCellMeta.sourcePath,
    "extras.select.whereConditions[1].rightOperandDecl.finalDesc"
  );

  dom.window.close();
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
  const realKey = getDeclOverrideStorageKeyFromRuntime(window, realDecl);
  const modal = await openTemplateCellDescriptionTab(window, sourceCell);
  await saveTemplateCellDescription(window, modal, "Real A edited");
  assert.strictEqual(String(state.descOverrides[realKey] || ""), "Real A edited", "Expected a declared APPEND source to retain its real declaration key.");

  dom.window.close();
}

async function assertTemplateAppendUnboundOperandsUseCanonicalTargets() {
  const dom = await renderFixture("APPEND a TO b.");
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const legacySourceKey = "PATH:OBJECTS/OBJECT[1]/VALUES/WHAT/DECL:A";
  state.descOverrides[legacySourceKey] = "Legacy A";
  runtime.api.renderTemplatePreview();

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
  assert.strictEqual(String(state.descOverrides[sourceKey] || ""), "Source A", "Expected APPEND source to save under the canonical PATH_DECL key.");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, legacySourceKey), false, "Expected canonical Save to remove the legacy Template alias.");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, targetKey), false, "Expected editing a not to modify b.");

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
  assert.strictEqual(String(state.descOverrides[targetKey] || ""), "Target B", "Expected APPEND target to save under its own canonical PATH_DECL key.");
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, sourceKey), false, "Expected editing b not to recreate a.");

  dom.window.close();
}

async function assertLegacyPathAliasUsesSourceShapedTemplateIndex() {
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
  const legacySourceKey = "PATH:OBJECTS/OBJECT[2]/VALUES/WHAT/DECL:A";

  const templateService = runtime && runtime.services ? runtime.services.template : null;
  const templateItems = templateService && typeof templateService.getRenderableObjectListForTemplate === "function"
    ? templateService.getRenderableObjectListForTemplate()
    : [];
  const appendIndex = templateItems.findIndex((item) => item && item.obj && item.obj.objectType === "APPEND");
  assert.strictEqual(appendIndex, 1, "Expected APPEND to stay before the later FORM definition in Template order.");

  state.descOverrides[legacySourceKey] = "Legacy shifted A";
  runtime.api.renderTemplatePreview();

  const appendTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="APPEND"]');
  assert(findTemplateCellByText(appendTable, "Legacy shifted A"), "Expected Template to resolve the source-shaped legacy alias.");

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

async function assertTemplateSemanticFallbacksUseStablePaths() {
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
    getDeclOverrideStorageKeyFromRuntime(window, performDecl),
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
    getDeclOverrideStorageKeyFromRuntime(window, callDecl),
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
  assert.strictEqual(getDeclOverrideStorageKeyFromRuntime(window, conditionLeftDecl), conditionLeftKey, "Expected Template to keep the existing condition-operand synthetic key.");
  const conditionModal = await openTemplateCellDescriptionTab(window, conditionLeft);
  await saveTemplateCellDescription(window, conditionModal, "Z edited");
  assert.strictEqual(String(window.AbapViewerRuntime.state.descOverrides[conditionLeftKey] || ""), "Z edited");
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
  assert.strictEqual(getDeclOverrideStorageKeyFromRuntime(window, intoDecl), "PATH:OBJECT:1/VALUES/INTOTABLE/DECL:OUT");

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
      assert.strictEqual(getDeclOverrideStorageKeyFromRuntime(window, decl), expectedKey);
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

  const outputService = runtime && runtime.services ? runtime.services.output : null;
  const originalNormalize = outputService && typeof outputService.normalizeEntryObjectForPath === "function"
    ? outputService.normalizeEntryObjectForPath
    : null;
  const originalWarn = window.console.warn;
  const provenanceWarnings = [];
  outputService.normalizeEntryObjectForPath = () => {
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
    outputService.normalizeEntryObjectForPath = originalNormalize;
    window.console.warn = originalWarn;
    dom.window.close();
  }
}

async function assertAppendVariantsExposeStableExtrasAndDedicatedTemplate() {
  const source = [
    'DATA ls_row TYPE i. "Flight row',
    'DATA lt_source TYPE STANDARD TABLE OF i. "Source rows',
    'DATA lt_target TYPE STANDARD TABLE OF i. "Target rows',
    "FIELD-SYMBOLS <ls_target> TYPE i.",
    "APPEND ls_row TO lt_target.",
    "APPEND INITIAL LINE TO lt_target ASSIGNING <ls_target>.",
    "APPEND LINES OF lt_source FROM 2 TO 4 STEP 2 USING KEY primary_key TO lt_target."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const appendObjects = (state.data && Array.isArray(state.data.objects) ? state.data.objects : [])
    .filter((obj) => obj && obj.objectType === "APPEND");
  assert.strictEqual(appendObjects.length, 3, "Expected all APPEND variants to keep objectType APPEND.");

  const [single, initialLine, linesOf] = appendObjects;
  assert.strictEqual(single.extras.append.variant, "single");
  assert.strictEqual(single.extras.append.source.value, "ls_row");
  assert.strictEqual(single.extras.append.target.value, "lt_target");
  assert.strictEqual(single.extras.append.result.assigning, null);

  assert.strictEqual(initialLine.extras.append.variant, "initialLine");
  assert.strictEqual(initialLine.extras.append.source, null);
  assert.strictEqual(initialLine.extras.append.target.value, "lt_target");
  assert.strictEqual(initialLine.extras.append.result.assigning.value, "<ls_target>");

  assert.strictEqual(linesOf.extras.append.variant, "linesOf");
  assert.strictEqual(single.values.what.value, "ls_row", "Expected legacy values.what compatibility for single-row APPEND.");
  assert.strictEqual(linesOf.values.source.value, "lt_source", "Expected legacy values.source compatibility.");
  assert.deepStrictEqual(
    Array.from(linesOf.values.to || []).map((entry) => entry.value),
    ["4", "lt_target"],
    "Expected the last legacy TO value to remain the target."
  );
  assert.strictEqual(linesOf.extras.append.source, linesOf.values.source);
  assert.strictEqual(linesOf.extras.append.target, linesOf.values.to[1]);
  assert.strictEqual(linesOf.extras.append.range.from.value, "2");
  assert.strictEqual(linesOf.extras.append.range.to.value, "4");
  assert.strictEqual(linesOf.extras.append.range.step.value, "2");
  assert.strictEqual(linesOf.extras.append.range.usingKey.value, "primary_key");
  assert.strictEqual(linesOf.extras.append.source.decl.name, "lt_source");
  assert.strictEqual(linesOf.extras.append.target.decl.name, "lt_target");

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  const linesTable = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="APPEND"]'))
    .find((table) => table.getAttribute("data-template-key") === "APPEND_LINES_OF");
  assert(linesTable, "Expected APPEND LINES OF to resolve the dedicated template key.");
  assert.deepStrictEqual(getTemplateTableRows(linesTable), [
    ["APPEND LINES OF", "Source rows"],
    ["FROM", "2"],
    ["TO", "4"],
    ["STEP", "2"],
    ["USING KEY", "primary_key"],
    ["TO", "Target rows"]
  ]);
  const sourceCell = findTemplateCellByText(linesTable, "Source rows");
  const targetCell = findTemplateCellByText(linesTable, "Target rows");
  assert.strictEqual(sourceCell.__templateCellMeta.declCandidates[0].name, "lt_source");
  assert.strictEqual(targetCell.__templateCellMeta.declCandidates[0].name, "lt_target");

  delete state.templateConfig.templates.APPEND_LINES_OF;
  window.AbapViewerRuntime.api.renderTemplatePreview();
  await waitForViewerUi(window);
  const fallbackAppendTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="APPEND"]'))
    .filter((table) => table.getAttribute("data-template-key") === "APPEND");
  assert.strictEqual(fallbackAppendTables.length, 3, "Expected missing APPEND_LINES_OF config to fall back to APPEND.");

  dom.window.close();
}

async function assertTemplateMultiSelectionAndClipboardSpacing() {
  const source = Array.from({ length: 45 }, (_, index) => `WRITE / 'Row ${String(index + 1).padStart(2, "0")}'.`)
    .join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  assert(els.templateCopySelectedBtn, "Expected Copy Selected toolbar button.");
  assert.strictEqual(els.templateCopySelectedBtn.disabled, true);
  assert.strictEqual(els.templateCopySelectedBtn.textContent.trim(), "Copy Selected (0)");

  const ensureBlock = async (index) => {
    assert.strictEqual(typeof runtime.api.ensureTemplateWindowContainsIndex, "function");
    runtime.api.ensureTemplateWindowContainsIndex(index);
    await waitForViewerUi(window);
    const block = els.templatePreviewOutput.querySelector(`.template-block[data-template-index="${index}"]`);
    assert(block, `Expected virtual Template block ${index}.`);
    return block;
  };
  const clickBlock = async (index, modifiers) => {
    const block = await ensureBlock(index);
    block.dispatchEvent(new window.MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      ...(modifiers || {})
    }));
    await waitForViewerUi(window);
  };
  const selectedIndexes = () => Array.from(state.selectedTemplateIndexes || [])
    .map(String)
    .sort((left, right) => Number(left) - Number(right));

  await clickBlock(1);
  assert.deepStrictEqual(selectedIndexes(), ["1"]);
  assert.strictEqual(state.selectedTemplateIndex, "1");

  await clickBlock(3, { ctrlKey: true });
  assert.deepStrictEqual(selectedIndexes(), ["1", "3"]);

  await clickBlock(5, { shiftKey: true });
  assert.deepStrictEqual(selectedIndexes(), ["3", "4", "5"]);

  await clickBlock(7, { ctrlKey: true, shiftKey: true });
  assert.deepStrictEqual(selectedIndexes(), ["3", "4", "5", "6", "7"]);

  await clickBlock(5, { ctrlKey: true });
  assert.deepStrictEqual(selectedIndexes(), ["3", "4", "6", "7"]);

  await clickBlock(30, { ctrlKey: true });
  assert.deepStrictEqual(selectedIndexes(), ["3", "4", "6", "7", "30"]);
  let block = await ensureBlock(30);
  assert(block.classList.contains("selected"), "Expected offscreen selection to restore after virtual rerender.");
  assert.strictEqual(block.getAttribute("aria-selected"), "true");

  block = await ensureBlock(5);
  assert(!block.classList.contains("selected"), "Expected toggled-off block to stay unselected after virtual rerender.");
  assert.strictEqual(block.getAttribute("aria-selected"), "false");

  assert.strictEqual(els.templateCopySelectedBtn.disabled, false);
  assert.strictEqual(els.templateCopySelectedBtn.textContent.trim(), "Copy Selected (5)");

  const selectionBeforeRerender = selectedIndexes();
  runtime.api.renderTemplatePreview();
  await waitForViewerUi(window);
  assert.deepStrictEqual(selectedIndexes(), selectionBeforeRerender, "Expected config rerender to preserve valid selected indexes.");

  window.__clipboardWrites.length = 0;
  els.templateCopySelectedBtn.click();
  await waitForViewerUi(window);
  const selectedClipboard = window.__clipboardWrites[window.__clipboardWrites.length - 1];
  assert(selectedClipboard && selectedClipboard.type === "text", "Expected Copy Selected clipboard write.");
  const selectedText = selectedClipboard.text;
  for (const expected of ["Row 04", "Row 05", "Row 07", "Row 08", "Row 31"]) {
    assert(selectedText.includes(expected), `Expected selected clipboard text to include ${expected}.`);
  }
  assert(!selectedText.includes("Row 06"), "Expected toggled-off template not to be copied.");
  assert(
    selectedText.indexOf("Row 04") < selectedText.indexOf("Row 31"),
    "Expected clipboard order to follow document order, not click order."
  );

  const virtual = state.templateVirtual;
  const config = virtual.config;
  assert.strictEqual(typeof runtime.api.buildTemplateCollectionCopyPayload, "function");
  const tablePayload = runtime.api.buildTemplateCollectionCopyPayload(virtual.items, [30, 3, 7], config, true);
  const tableHost = window.document.createElement("div");
  tableHost.innerHTML = tablePayload.html;
  assert.strictEqual(tableHost.querySelectorAll("table").length, 1, "Expected one combined table for Excel paste.");
  assert.strictEqual(tableHost.querySelectorAll("tr[data-template-spacer]").length, 2, "Expected N-1 spacer rows.");
  assert(!tableHost.querySelector("tr:last-child").hasAttribute("data-template-spacer"), "Expected no trailing spacer row.");
  assert.strictEqual(tablePayload.text.split("\n\n").length, 3, "Expected one blank plain-text line between templates.");

  const singlePayload = runtime.api.buildTemplateCollectionCopyPayload(virtual.items, [3], config, true);
  const singleHost = window.document.createElement("div");
  singleHost.innerHTML = singlePayload.html;
  assert.strictEqual(singleHost.querySelectorAll("tr[data-template-spacer]").length, 0);

  const fullPayload = runtime.api.buildTemplateCollectionCopyPayload(virtual.items, [3, 7, 30], config, false);
  const fullHost = window.document.createElement("div");
  fullHost.innerHTML = fullPayload.html;
  assert.strictEqual(fullHost.querySelectorAll("[data-template-spacer]").length, 2, "Expected one full-block separator node between templates.");
  assert(!fullHost.lastElementChild.hasAttribute("data-template-spacer"), "Expected no trailing full-block separator.");

  const allIndexes = virtual.items.map((_, index) => index);
  const allPayload = runtime.api.buildTemplateCollectionCopyPayload(virtual.items, allIndexes, config, true);
  const allHost = window.document.createElement("div");
  allHost.innerHTML = allPayload.html;
  assert.strictEqual(allHost.querySelectorAll("tr[data-template-spacer]").length, virtual.items.length - 1, "Expected Copy All builder to use the same spacing rule.");

  window.__clipboardWrites.length = 0;
  els.templateCopyAllBtn.click();
  await waitForViewerUi(window);
  const allClipboard = window.__clipboardWrites[window.__clipboardWrites.length - 1];
  assert(allClipboard && allClipboard.text.includes("Row 01") && allClipboard.text.includes("Row 45"), "Expected Copy All to use the shared collection builder.");

  els.parseBtn.click();
  await waitForViewerUi(window);
  assert.deepStrictEqual(selectedIndexes(), [], "Expected reparse to clear multi-selection.");
  assert.strictEqual(state.selectedTemplateIndex, "");
  assert.strictEqual(els.templateCopySelectedBtn.disabled, true);

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
    "WRITE / 'Carrier'.",
    ""
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  assert(state.templateConfig.templates.MESSAGE, "Expected an explicit MESSAGE default template.");
  assert(state.templateConfig.templates.WRITE, "Expected an explicit WRITE default template.");
  const renderObjects = Array.isArray(state.renderObjects) ? state.renderObjects : [];
  assert.strictEqual(renderObjects.filter((obj) => obj && obj.objectType === "MESSAGE").length, 3);
  assert.strictEqual(renderObjects.filter((obj) => obj && obj.objectType === "WRITE").length, 4);

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
  assert.strictEqual(writeTables.length, 4, "Expected positioned, numeric-position, literal, and newline-only WRITE blocks.");

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
  assert(
    writeRows.some((row) => (row.includes("/") || row.includes("AT")) && row.includes("Column") && !row.includes("Length")),
    "WRITE column position must be its own editable row."
  );
  assert(
    writeRows.some((row) => row.includes("LENGTH") && row.includes("Length") && !row.includes("Column")),
    "WRITE length position must be a separate LENGTH row."
  );
  assert(writeRows.some((row) => row.includes("TO") && row.includes("Destination")));
  assert(writeRows.some((row) => row.includes("NO-GAP")), "Flag formatting must stay visible.");
  assert(writeRows.some((row) => row.includes("CURRENCY") && row.includes("Currency")), "Formatting data operands must retain provenance.");
  assert(writeRows.some((row) => row.includes("USING EDIT MASK") && row.includes("'==XX'")), "Literal format operands must stay visible.");
  const numericPositionRows = getTemplateTableRows(writeTables[1]);
  assert(
    numericPositionRows.some((row) => (row.includes("AT") || row.includes("/")) && row.includes("5") && !row.includes("10")),
    "WRITE numeric column must render on its own row."
  );
  assert(
    numericPositionRows.some((row) => row.includes("LENGTH") && row.includes("10")),
    "WRITE numeric length must render on a LENGTH row."
  );
  assert(numericPositionRows.some((row) => row.includes("WRITE") && row.includes("Output value")));

  const newlineOnlyRows = getTemplateTableRows(writeTables[3]);
  assert(newlineOnlyRows.some((row) => row.includes("WRITE") && row.includes("'Carrier'")));
  assert(
    newlineOnlyRows.some((row) => row[0] === "/" && (row.length === 1 || !row[1])),
    "Newline-only WRITE must not duplicate '/' as the value."
  );
  assert(
    !newlineOnlyRows.some((row) => row[0] === "/" && row[1] === "/"),
    "Newline-only WRITE must not render ['/','/']."
  );
  const messageCell = findTemplateCellByText(messageTables[0], "Message text");
  assert(messageCell, "Expected editable MESSAGE primary cell.");
  const messageCandidates = messageCell.__templateCellMeta && messageCell.__templateCellMeta.declCandidates;
  assert(Array.isArray(messageCandidates) && messageCandidates.length > 0);
  assert.strictEqual(String(messageCandidates[0].name || ""), "lv_message");
  const messageDeclKey = getDeclOverrideStorageKeyFromRuntime(window, messageCandidates[0]);
  let modal = await openTemplateCellDescriptionTab(window, messageCell);
  await saveTemplateCellDescription(window, modal, "Edited message");
  assert.strictEqual(String(state.descOverrides[messageDeclKey] || ""), "Edited message");
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

  dom.window.close();
}

defineFocusedTest(test, "viewer struct field finalDesc contract", ["struct-field-finaldesc"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("struct field final desc normalizes parent only", async () => {
    await assertStructFieldFinalDescNormalizesParentOnly();
  });
});

defineFocusedTest(test, "viewer constant finalDesc contract", ["constant-finaldesc"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("constant initializers and empty table bodies shape final desc", async () => {
    await assertConstantInitializersAndEmptyTableBodiesShapeFinalDesc();
  });
});

defineFocusedTest(test, "viewer condition multi value contract", ["template-multi-value-conditions"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("condition lists expand rows", async () => {
    await assertConditionListsExpandRows();
  });
});

defineFocusedTest(test, "viewer safe raw list contract", ["template-multi-value-safe-lists"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("safe raw lists expand without splitting expressions", async () => {
    await assertSafeRawListsExpandWithoutSplittingExpressions();
  });
});

defineFocusedTest(test, "viewer template row description loop contract", ["template-row-description-loop"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template row description edits local loop decl", async () => {
    await assertTemplateRowDescriptionEditsLocalLoopDecl();
  });
});

defineFocusedTest(test, "viewer template row description perform contracts", ["template-row-description-perform"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template row description keeps nested perform trace", async () => {
    await assertTemplateRowDescriptionKeepsNestedPerformTrace();
  });

  await t.test("template perform source edit does not bleed across sources", async () => {
    await assertTemplatePerformSourceEditDoesNotBleedAcrossSources();
  });
});

defineFocusedTest(test, "viewer template row description condition contract", ["template-row-description-condition"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template row description targets exact condition decls", async () => {
    await assertTemplateRowDescriptionTargetsExactConditionDecls();
  });
});

defineFocusedTest(test, "viewer template provenance contracts", ["template-provenance"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template append declared operands prefer real declarations", async () => {
    await assertTemplateAppendDeclaredOperandsPreferRealDeclarations();
  });

  await t.test("template append unbound operands use canonical targets", async () => {
    await assertTemplateAppendUnboundOperandsUseCanonicalTargets();
  });

  await t.test("legacy path alias uses source shaped template index", async () => {
    await assertLegacyPathAliasUsesSourceShapedTemplateIndex();
  });

  await t.test("template append literal keeps only target editable", async () => {
    await assertTemplateAppendLiteralKeepsOnlyTargetEditable();
  });

  await t.test("template if array provenance is per rendered line", async () => {
    await assertTemplateIfArrayProvenanceIsPerRenderedLine();
  });

  await t.test("template indexed and composite placeholders keep provenance", async () => {
    await assertTemplateIndexedAndCompositePlaceholdersKeepProvenance();
  });

  await t.test("template semantic fallbacks use stable paths", async () => {
    await assertTemplateSemanticFallbacksUseStablePaths();
  });

  await t.test("template direct schema paths stay locked", async () => {
    await assertTemplateDirectSchemaPathsStayLocked();
  });

  await t.test("template fallback allowlist covers existing itab operands", async () => {
    await assertTemplateFallbackAllowlistCoversExistingItabOperands();
  });

  await t.test("template resolver warns once with cell metadata", async () => {
    await assertTemplateResolverWarnsOnceWithCellMetadata();
  });
});

defineFocusedTest(test, "viewer append variant contract", ["append-variants"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("append variants expose stable extras and dedicated template", async () => {
    await assertAppendVariantsExposeStableExtrasAndDedicatedTemplate();
  });
});

defineFocusedTest(test, "viewer template multi select contract", ["template-multi-select"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template multi selection and clipboard spacing", async () => {
    await assertTemplateMultiSelectionAndClipboardSpacing();
  });
});

defineFocusedTest(test, "viewer message and write contract", ["message-write"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("message and write viewer contracts", async () => {
    await assertMessageAndWriteViewerContracts();
  });
});

defineFocusedTest(test, "viewer template Excel round-trip contracts", ["template-excel-roundtrip"], async (t) => {
  await t.test("raw placeholders and supported formatting survive HTML round-trip", async () => {
    const dom = await renderFixture("DATA lv_target TYPE string.\nlv_target = 'X'.");
    const { window } = dom;
    const service = window.AbapViewerRuntime.services.templateExcel;
    assert(service, "Expected the templateExcel service to be registered.");

    const definition = {
      ranges: {
        "A1:B1": {
          text: "{values.target.finalDesc}",
          background: "#dbeef4",
          "font color": "#112233",
          font: "MS PGothic",
          "font size": 11,
          bold: true,
          italic: true,
          underline: true,
          align: "center",
          valign: "middle",
          wrap: true,
          border: "outside-thin",
          merge: true
        },
        A2: { text: "Plain" }
      },
      _options: { hideEmptyRows: true }
    };
    const payload = service.buildClipboardPayload("READ_TABLE", definition);
    assert(payload.html.includes("{values.target.finalDesc}"), "Expected raw placeholder text.");
    assert(payload.html.includes("0.5pt solid"), "Expected the Excel border to be one step thinner.");

    const parsed = service.parseClipboardPayload({
      html: payload.html,
      text: payload.text,
      currentOptions: definition._options
    });
    assert.strictEqual(parsed.source, "html");
    assert.strictEqual(parsed.stats.rows, 2);
    assert.strictEqual(parsed.stats.cols, 2);
    assert.strictEqual(JSON.stringify(parsed.options), JSON.stringify(definition._options));
    assert.strictEqual(parsed.ranges["A1:B1"].text, "{values.target.finalDesc}");
    assert.strictEqual(parsed.ranges["A1:B1"].merge, true);
    assert.strictEqual(parsed.ranges["A1:B1"].border, "outside-thin");
    assert.strictEqual(parsed.ranges.A2.border, undefined, "Expected plain cells to remain borderless.");
  });

  await t.test("text overlays do not shrink an enclosing border range during copy", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    const payload = service.buildClipboardPayload("READ_TABLE", {
      "A1:T1": {
        background: "#dbeef4",
        border: "outside-thin",
        font: "MS PGothic"
      },
      A1: { text: "{rows.keyword}" }
    });
    const doc = new dom.window.DOMParser().parseFromString(payload.html, "text/html");
    const firstCellStyle = doc.querySelector("td").style;
    assert.notStrictEqual(firstCellStyle.borderTopStyle, "none");
    assert.notStrictEqual(firstCellStyle.borderBottomStyle, "none");
    assert.notStrictEqual(firstCellStyle.borderLeftStyle, "none");
    assert.notStrictEqual(
      firstCellStyle.borderRightStyle,
      "solid",
      "A1 must not become a separately boxed cell inside the A1:T1 outline."
    );
    const parsed = service.parseClipboardPayload({ html: payload.html, text: payload.text });
    assert.strictEqual(parsed.ranges.A1.border, undefined);
    assert.strictEqual(parsed.ranges["A1:T1"].border, "outside-thin");
  });

  await t.test("TSV fallback keeps text and reports lost formatting", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    const parsed = service.parseClipboardPayload({
      text: "A\t{values.message.finalDesc}\r\nC\tD",
      currentOptions: { hideRowsWithoutValues: true }
    });
    assert.strictEqual(parsed.source, "tsv");
    assert.strictEqual(parsed.ranges.B1.text, "{values.message.finalDesc}");
    assert.strictEqual(JSON.stringify(parsed.options), JSON.stringify({ hideRowsWithoutValues: true }));
    assert(parsed.warnings.some((warning) => /format|merge|border/i.test(warning)));
  });

  await t.test("Excel CSS specificity and ignored colspan are respected", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    const parsed = service.parseClipboardPayload({
      html: [
        "<style>.excelCell { background-color:#abcdef; } td { background-color:#123456; }</style>",
        "<table><tr>",
        "<td class=\"excelCell\" colspan=\"2\" style=\"mso-ignore:colspan;border:none\">{values.name.finalDesc}</td>",
        "<td style=\"border:none\">Plain</td>",
        "</tr></table>"
      ].join(""),
      currentOptions: {}
    });
    assert.strictEqual(parsed.ranges["A1:B1"].background, "#abcdef");
    assert.strictEqual(parsed.ranges["A1:B1"].merge, undefined, "Expected mso-ignore:colspan not to create a merge.");
    assert.strictEqual(Object.values(parsed.ranges).some((config) => config.border === "outside-thin"), false);
  });

  await t.test("adjacent Excel outlines survive shared-edge border encoding", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    const parsed = service.parseClipboardPayload({
      html: [
        "<table><tr>",
        "<td style=\"border-top:1px solid;border-bottom:1px solid;border-left:1px solid\">A</td>",
        "<td style=\"border-top:1px solid;border-bottom:1px solid;border-right:1px solid\">B</td>",
        "<td style=\"border-top:1px solid;border-bottom:1px solid\">C</td>",
        "<td style=\"border-top:1px solid;border-bottom:1px solid;border-right:1px solid\">D</td>",
        "</tr></table>"
      ].join("")
    });
    const borders = Object.entries(parsed.ranges)
      .filter(([, config]) => config.border === "outside-thin")
      .map(([range]) => range);
    assert.deepStrictEqual(Array.from(borders), ["A1:B1", "C1:D1"]);
  });

  await t.test("benign CSS generated by Excel does not produce unsupported warnings", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    const parsed = service.parseClipboardPayload({
      html: [
        "<table><tr><td style=\"",
        "background-attachment:scroll;background-clip:border-box;background-image:none;",
        "background-origin:padding-box;background-position-x:0%;background-position-y:0%;",
        "background-repeat:repeat;background-size:auto;height:15pt;width:64pt;",
        "padding-left:1px;padding-right:1px;padding-top:1px;",
        "text-decoration-color:#000;text-decoration-style:solid;text-decoration-thickness:auto;",
        "text-wrap-mode:wrap;white-space-collapse:preserve;",
        "border:none\">Text</td></tr></table>"
      ].join("")
    });
    assert.deepStrictEqual(Array.from(parsed.warnings), []);
  });

  await t.test("oversized clipboard is rejected before draft import", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    assert.throws(
      () => service.parseClipboardPayload({ html: `<table><!--${"x".repeat(2000001)}--></table>` }),
      /safe limit/i
    );
  });

  await t.test("repeated Excel cell formatting is compacted into ranges", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const service = dom.window.AbapViewerRuntime.services.templateExcel;
    const cells = Array.from({ length: 8 }, (_, index) => (
      `<td style="background-color:#dbeef4;font-weight:bold;border:none">${index < 2 ? `T${index + 1}` : ""}</td>`
    ));
    const parsed = service.parseClipboardPayload({
      html: `<table><tr>${cells.slice(0, 4).join("")}</tr><tr>${cells.slice(4).join("")}</tr></table>`
    });
    assert(
      Object.keys(parsed.ranges).length <= 3,
      `Expected compact style and text overlays, got ${Object.keys(parsed.ranges).length} ranges.`
    );
    assert.strictEqual(parsed.ranges["A1:D2"].background, "#dbeef4");
  });

  await t.test("Template Form exposes staged Excel actions", async () => {
    const dom = await renderFixture("WRITE 'X'.");
    const { window } = dom;
    window.AbapViewerRuntime.services.template.openTemplateConfigModal();
    await waitForViewerUi(window);
    const page = Array.from(window.document.querySelectorAll(".template-dynamic-page"))
      .find((node) => String(node.textContent || "").includes("Template Form"));
    assert(page, "Expected Template Form page.");
    const labels = Array.from(page.querySelectorAll("button")).map((button) => String(button.textContent || "").trim());
    assert(labels.includes("Copy to Excel"));
    assert(labels.includes("Paste from Excel"));
    assert(page.querySelector(".template-excel-status"), "Expected a dedicated Excel status message area.");

    const keySelect = page.querySelector(".template-builder-key-field select");
    keySelect.value = "READ_TABLE";
    keySelect.dispatchEvent(new window.Event("change", { bubbles: true }));
    await waitForViewerUi(window);
    const getGridCell = (row, col) => page.querySelector(`.template-builder-grid td[data-row="${row}"][data-col="${col}"]`);
    const a1 = getGridCell(1, 1);
    const t1 = getGridCell(1, 20);
    const u1 = getGridCell(1, 21);
    assert.strictEqual(a1.style.borderTopStyle, "solid");
    assert.notStrictEqual(a1.style.borderRightStyle, "solid", "A1 must not be boxed separately.");
    assert.strictEqual(t1.style.borderRightStyle, "solid");
    assert.strictEqual(u1.style.borderLeftStyle, "solid", "The second border range must remain visible.");
  });
});
