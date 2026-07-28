"use strict";

const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assert,
  assertViewerFixtureDirectoriesStayInSync,
  findDataDeclGroup,
  findDataDeclRow,
  getDeclOverrideStorageKeyFromRuntime,
  getTemplateTableRows,
  renderFixture,
  settleViewerUi,
  waitForViewerUi
} = require("./helpers/viewer-contract-test-helpers");

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

async function assertLocalMethodSourceSelectionUsesScopedChains() {
  const source = [
    "DATA gv_first TYPE string.",
    "DATA gv_second TYPE string.",
    "DATA gv_out_first TYPE string.",
    "DATA gv_out_second TYPE string.",
    "DATA gv_count_first TYPE i.",
    "DATA gv_count_second TYPE i.",
    "DATA gv_recv_first TYPE string.",
    "DATA gv_recv_second TYPE string.",
    "CLASS lcl_math DEFINITION.",
    "  PUBLIC SECTION.",
    "    CLASS-METHODS calculate",
    "      IMPORTING iv_input TYPE string",
    "      EXPORTING ev_output TYPE string",
    "      CHANGING cv_count TYPE i",
    "      RETURNING VALUE(rv_result) TYPE string.",
    "ENDCLASS.",
    "CLASS lcl_math IMPLEMENTATION.",
    "  METHOD calculate.",
    "    ev_output = iv_input.",
    "    cv_count = cv_count + 1.",
    "    rv_result = iv_input.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "CALL METHOD lcl_math=>calculate",
    "  EXPORTING iv_input = gv_first",
    "  IMPORTING ev_output = gv_out_first",
    "  CHANGING cv_count = gv_count_first",
    "  RECEIVING rv_result = gv_recv_first.",
    "CALL METHOD lcl_math=>calculate",
    "  EXPORTING iv_input = gv_second",
    "  IMPORTING ev_output = gv_out_second",
    "  CHANGING cv_count = gv_count_second",
    "  RECEIVING rv_result = gv_recv_second."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const targetKey = "METHOD:LCL_MATH=>CALCULATE";
  const registry = state.performSourceRegistry;
  const candidates = registry && registry.candidatesByFormUpper.get(targetKey) || [];

  assert.strictEqual(candidates.length, 2, "Expected one local METHOD source candidate per call site.");
  assert(candidates.every((candidate) => candidate.bindingContext && candidate.bindingContext.chainKind === "METHOD"));
  assert(candidates.every((candidate) => String(candidate.sourceScope || "").includes("CALCULATE")));

  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // JSDOM default viewport is sufficient for this small fixture.
  }
  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const renderedMethods = [];
  const collectMethods = (nodes) => {
    for (const node of Array.isArray(nodes) ? nodes : []) {
      if (String(node && node.objectType || "") === "METHOD") {
        renderedMethods.push(node);
      }
      collectMethods(node && node.children);
    }
  };
  collectMethods(state.renderObjects);
  assert.strictEqual(renderedMethods.filter((node) => /calculate/i.test(String(node && node.raw || ""))).length, 1, "Expected the local METHOD implementation to render once.");
  const sourceSelects = Array.from(els.templatePreviewOutput.querySelectorAll(
    `.perform-source-select[data-perform-form="${targetKey}"]`
  ));
  assert(sourceSelects.length >= 1, "Expected local METHOD subtree to expose its shared source selector.");

  const findAssignmentTable = (raw) => {
    const matchingObject = [];
    const collectMatching = (nodes) => {
      for (const node of Array.isArray(nodes) ? nodes : []) {
        if (String(node && node.raw || "").trim() === raw) {
          matchingObject.push(node);
        }
        collectMatching(node && node.children);
      }
    };
    collectMatching(state.renderObjects);
    const objectId = matchingObject[0] && matchingObject[0].id;
    const block = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
      .find((candidate) => String(candidate.querySelector(".template-block-meta")?.textContent || "").includes(`#${String(objectId || "")}`));
    return block ? block.querySelector("table.template-preview-table") : null;
  };
  const firstInputTable = findAssignmentTable("ev_output = iv_input.");
  assert(firstInputTable, "Expected local METHOD assignment preview.");
  assert.deepStrictEqual(getTemplateTableRows(firstInputTable), [
    ["Đích", "gv_out_first"],
    ["Nguồn", "gv_first"]
  ]);

  sourceSelects[0].value = candidates[1].key;
  sourceSelects[0].dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 4);
  assert.strictEqual(registry.getSelectedCandidate(targetKey).key, candidates[1].key);
  const secondInputTable = findAssignmentTable("ev_output = iv_input.");
  assert.deepStrictEqual(getTemplateTableRows(secondInputTable), [
    ["Đích", "gv_out_second"],
    ["Nguồn", "gv_second"]
  ]);

  const selectedMethodParam = Array.from(secondInputTable.querySelectorAll("td"))
    .find((cell) => String(cell.textContent || "").trim() === "gv_second");
  const methodChainCandidate = selectedMethodParam && selectedMethodParam.__templateCellMeta
    && Array.from(selectedMethodParam.__templateCellMeta.declCandidates || [])
      .find((decl) => /^METHOD_CHAIN:/.test(String(getDeclOverrideStorageKeyFromRuntime(window, decl) || "")));
  assert(methodChainCandidate, "Expected method parameter edits to expose a source-scoped METHOD_CHAIN key.");

  dom.window.close();
}

async function assertNestedLocalMethodSourcesStayScopedAndMapStructComponents() {
  const source = [
    "TYPES: BEGIN OF ty_context,",
    "         city TYPE string,",
    "       END OF ty_context.",
    "DATA gs_first TYPE ty_context.",
    "DATA gs_second TYPE ty_context.",
    "DATA gv_city TYPE string.",
    "CLASS lcl_chain DEFINITION.",
    "  PUBLIC SECTION.",
    "    CLASS-METHODS outer IMPORTING is_context TYPE ty_context.",
    "    CLASS-METHODS inner IMPORTING is_context TYPE ty_context.",
    "ENDCLASS.",
    "CLASS lcl_chain IMPLEMENTATION.",
    "  METHOD outer.",
    "    CALL METHOD lcl_chain=>inner EXPORTING is_context = is_context.",
    "  ENDMETHOD.",
    "  METHOD inner.",
    "    gv_city = is_context-city.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "CALL METHOD lcl_chain=>outer EXPORTING is_context = gs_first.",
    "CALL METHOD lcl_chain=>outer EXPORTING is_context = gs_second."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const registry = state.performSourceRegistry;
  const outerTarget = "METHOD:LCL_CHAIN=>OUTER";
  const innerTarget = "METHOD:LCL_CHAIN=>INNER";
  const outerCandidates = registry.candidatesByFormUpper.get(outerTarget) || [];
  const innerCandidates = registry.candidatesByFormUpper.get(innerTarget) || [];

  assert.strictEqual(outerCandidates.length, 2, "Expected two OUTER call-site candidates.");
  assert.strictEqual(innerCandidates.length, 2, "Expected INNER only through the two scoped OUTER calls.");
  assert(innerCandidates.every((candidate) => candidate.ancestry.length === 1), "Expected every INNER candidate to retain its OUTER ancestry.");
  assert(innerCandidates.every((candidate) => candidate.parentCandidateKey), "Expected nested INNER candidates to have a parent source.");

  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // JSDOM viewport defaults are sufficient for this fixture.
  }
  els.rightTabTemplateBtn.click();
  await settleViewerUi(window, 3);

  const innerSelect = els.templatePreviewOutput.querySelector(`.perform-source-select[data-perform-form="${innerTarget}"]`);
  assert(innerSelect, "Expected INNER METHOD subtree to expose its source selector.");
  assert.strictEqual(
    window.AbapViewerRuntime.services.performSources.selectPerformSourceCandidate(innerTarget, innerCandidates[1].key),
    true,
    "Expected selecting a nested METHOD source to synchronize its OUTER call chain."
  );
  await settleViewerUi(window, 4);

  const cityAssignment = [];
  const collectAssignment = (nodes) => {
    for (const node of Array.isArray(nodes) ? nodes : []) {
      if (String(node && node.raw || "").trim() === "gv_city = is_context-city.") {
        cityAssignment.push(node);
      }
      collectAssignment(node && node.children);
    }
  };
  collectAssignment(state.renderObjects);
  const cityObject = cityAssignment[0];
  assert(cityObject, "Expected INNER structured-param assignment.");
  const cityBlock = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
    .find((block) => String(block.querySelector(".template-block-meta")?.textContent || "").includes(`#${String(cityObject.id)}`));
  const cityTable = cityBlock && cityBlock.querySelector("table.template-preview-table");
  assert(cityTable, "Expected INNER structured-param Template table.");
  assert.deepStrictEqual(getTemplateTableRows(cityTable), [
    ["Đích", "gv_city"],
    ["Nguồn", "gs_second-city"]
  ]);

  dom.window.close();
}

async function assertNestedLocalMethodComponentActualUsesSelectedRoot() {
  const source = [
    "TYPES: BEGIN OF ty_context,",
    "         name TYPE string,",
    "       END OF ty_context.",
    "DATA gs_first TYPE ty_context.",
    "DATA gs_second TYPE ty_context.",
    "DATA gv_name TYPE string.",
    "CLASS lcl_chain DEFINITION.",
    "  PUBLIC SECTION.",
    "    CLASS-METHODS outer IMPORTING is_outer TYPE ty_context.",
    "    CLASS-METHODS inner IMPORTING iv_name TYPE string.",
    "ENDCLASS.",
    "CLASS lcl_chain IMPLEMENTATION.",
    "  METHOD outer.",
    "    CALL METHOD lcl_chain=>inner EXPORTING iv_name = is_outer-name.",
    "  ENDMETHOD.",
    "  METHOD inner.",
    "    gv_name = iv_name.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "CALL METHOD lcl_chain=>outer EXPORTING is_outer = gs_first.",
    "CALL METHOD lcl_chain=>outer EXPORTING is_outer = gs_second."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const registry = state.performSourceRegistry;
  const innerTarget = "METHOD:LCL_CHAIN=>INNER";
  const innerCandidates = registry.candidatesByFormUpper.get(innerTarget) || [];

  assert.strictEqual(innerCandidates.length, 2, "Expected one scoped INNER candidate per OUTER root call.");
  assert.deepStrictEqual(
    Array.from(innerCandidates[0].bindingContext.byParamUpper.get("IV_NAME") || [], (decl) => String(decl && decl.name || "")),
    ["gs_first-name"],
    "Expected the first component actual to resolve through the first OUTER binding."
  );
  assert.deepStrictEqual(
    Array.from(innerCandidates[1].bindingContext.byParamUpper.get("IV_NAME") || [], (decl) => String(decl && decl.name || "")),
    ["gs_second-name"],
    "Expected the second component actual to resolve through the second OUTER binding."
  );

  try {
    Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  } catch {
    // JSDOM viewport defaults are sufficient for this fixture.
  }
  els.rightTabTemplateBtn.click();
  await settleViewerUi(window, 3);
  assert.strictEqual(
    window.AbapViewerRuntime.services.performSources.selectPerformSourceCandidate(innerTarget, innerCandidates[1].key),
    true
  );
  await settleViewerUi(window, 4);

  const assignments = [];
  const collectAssignment = (nodes) => {
    for (const node of Array.isArray(nodes) ? nodes : []) {
      if (String(node && node.raw || "").trim() === "gv_name = iv_name.") {
        assignments.push(node);
      }
      collectAssignment(node && node.children);
    }
  };
  collectAssignment(state.renderObjects);
  const assignment = assignments[0];
  const block = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
    .find((candidate) => String(candidate.querySelector(".template-block-meta")?.textContent || "").includes(`#${String(assignment && assignment.id || "")}`));
  assert.deepStrictEqual(getTemplateTableRows(block.querySelector("table.template-preview-table")), [
    ["Đích", "gv_name"],
    ["Nguồn", "gs_second-name"]
  ]);

  dom.window.close();
}

async function assertTypedLocalInstanceMethodCreatesSourceChain() {
  const source = [
    "DATA gv_input TYPE string.",
    "DATA gv_output TYPE string.",
    "CLASS lcl_worker DEFINITION.",
    "  PUBLIC SECTION.",
    "    METHODS run IMPORTING iv_input TYPE string EXPORTING ev_output TYPE string.",
    "ENDCLASS.",
    "CLASS lcl_worker IMPLEMENTATION.",
    "  METHOD run.",
    "    ev_output = iv_input.",
    "  ENDMETHOD.",
    "ENDCLASS.",
    "DATA lo_worker TYPE REF TO lcl_worker.",
    "CALL METHOD lo_worker->run EXPORTING iv_input = gv_input IMPORTING ev_output = gv_output."
  ].join("\n");
  const dom = await renderFixture(source);
  const { state } = dom.window.AbapViewerRuntime;
  const target = "METHOD:LCL_WORKER=>RUN";
  const candidates = state.performSourceRegistry.candidatesByFormUpper.get(target) || [];

  assert.strictEqual(candidates.length, 1, "Expected a typed local instance call to register a METHOD source.");
  assert.deepStrictEqual(
    Array.from(candidates[0].bindingContext.byParamUpper.get("IV_INPUT") || [], (decl) => String(decl && decl.name || "")),
    ["gv_input"]
  );
  assert.strictEqual(candidates[0].bindingContext.chainKind, "METHOD");

  dom.window.close();
}

async function assertFormSourceTraceUsesRootDeclarations() {
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
  assert(
    performRoots.every((obj) => !Array.isArray(obj.children) || obj.children.length === 0),
    "Expected every PERFORM call to remain a leaf statement."
  );
  const formRoots = (Array.isArray(state.renderObjects) ? state.renderObjects : [])
    .filter((obj) => obj && obj.objectType === "FORM");
  const getFormRoot = (name) => formRoots.find((obj) => String(obj?.extras?.form?.name || "") === name);

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

  const outerForm = getFormRoot("frm_outer");
  const localForm = getFormRoot("frm_local");
  const innerForm = getFormRoot("frm_inner");
  const literalForm = getFormRoot("frm_literal");
  assert(outerForm && localForm && innerForm && literalForm, "Expected every local FORM definition to render once.");
  const firstIf = findByRaw(innerForm, "IF iv_inner IS INITIAL OR is_inner-city IS NOT INITIAL.");
  assert(firstIf, "Expected inner IF inside the source-shaped FORM.");
  assert.deepStrictEqual(getBindingNames(firstIf, "iv_inner"), ["iv_outer", "gv_root"]);
  assert.deepStrictEqual(getBindingNames(firstIf, "is_inner"), ["is_outer", "gs_root"]);
  assert.deepStrictEqual(getBindingNames(firstIf, "cv_inner"), ["cv_outer", "gv_change"]);
  assert.deepStrictEqual(getBindingNames(firstIf, "tt_inner"), ["tt_outer", "gt_root"]);

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
    assert(obj, `Expected source-shaped FORM object for ${raw}`);
    const table = findTemplateTable(obj);
    assert(table, `Expected Template preview table for ${raw}`);
    assert.deepStrictEqual(getTemplateTableRows(table), expectedRows);
  };

  assertTemplateRows(innerForm, "iv_inner = iv_inner && '-x'.", [
    ["Đích", "gv_root"],
    ["Nguồn", "gv_root"],
    ["Nguồn", "'-x'"]
  ]);
  assertTemplateRows(innerForm, "is_inner-city = is_inner-name.", [
    ["Đích", "gs_root-city"],
    ["Nguồn", "gs_root-name"]
  ]);
  assertTemplateRows(innerForm, "cv_inner = iv_inner.", [
    ["Đích", "gv_change"],
    ["Nguồn", "gv_root"]
  ]);
  assertTemplateRows(innerForm, "APPEND iv_inner TO tt_inner.", [
    ["APPEND", "gv_root"],
    ["TO", "gt_root"]
  ]);
  assertTemplateRows(innerForm, "IF iv_inner IS INITIAL OR is_inner-city IS NOT INITIAL.", [
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "Kết nối"],
    ["gv_root", "IS", "INITIAL", "OR"],
    ["gs_root-city", "IS", "NOT INITIAL"]
  ]);
  assertTemplateRows(innerForm, "IF iv_inner = cv_inner AND is_inner-city = is_inner-name.", [
    ["Điều kiện trái", "Toán tử", "Điều kiện phải", "Kết nối"],
    ["gv_root", "=", "gv_change", "AND"],
    ["gs_root-city", "=", "gs_root-name"]
  ]);
  assertTemplateRows(localForm, "CLEAR iv_local.", [["CLEAR", "lv_local"]]);
  assertTemplateRows(literalForm, "CLEAR iv_literal.", [["CLEAR", "iv_literal"]]);

  dom.window.close();
}

async function assertTemplateRendersEachFormOnceInSourceOrder() {
  const source = [
    "DATA gv_first TYPE string.",
    "DATA gv_second TYPE string.",
    "DATA gv_third TYPE string.",
    "PERFORM frm_once USING gv_first.",
    "PERFORM frm_once USING gv_second.",
    "PERFORM frm_once USING gv_third.",
    "FORM frm_once USING iv_value TYPE string.",
    "  IF iv_value IS INITIAL.",
    "    CLEAR iv_value.",
    "  ENDIF.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const renderRoots = Array.isArray(state.renderObjects) ? state.renderObjects : [];
  const performRoots = renderRoots.filter((obj) => obj && obj.objectType === "PERFORM");
  const formRoots = renderRoots.filter((obj) => obj && obj.objectType === "FORM");

  assert.strictEqual(performRoots.length, 3, "Expected every PERFORM call to stay visible.");
  assert(
    performRoots.every((obj) => !Array.isArray(obj.children) || obj.children.length === 0),
    "Expected PERFORM calls not to inline-expand FORM children."
  );
  assert.strictEqual(formRoots.length, 1, "Expected the FORM definition to render exactly once.");
  assert.strictEqual(String(formRoots[0].children?.[0]?.raw || "").trim(), "IF iv_value IS INITIAL.");
  assert.strictEqual(String(formRoots[0].children?.[0]?.children?.[0]?.raw || "").trim(), "CLEAR iv_value.");

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);

  const formTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="FORM"]');
  assert(formTable, "Expected a Template block for the FORM definition.");
  const formBlock = formTable.closest(".template-block");
  const formSourceSelect = formBlock?.querySelector('.perform-source-select[data-perform-form="FRM_ONCE"]');
  assert(formSourceSelect, "Expected source selector on FORM header.");

  const ifTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="IF"]');
  const clearTable = els.templatePreviewOutput.querySelector('.template-preview-table[data-object-type="CLEAR"]');
  const ifSourceSelect = ifTable?.closest(".template-block")
    ?.querySelector('.perform-source-select[data-perform-form="FRM_ONCE"]');
  const clearSourceSelect = clearTable?.closest(".template-block")
    ?.querySelector('.perform-source-select[data-perform-form="FRM_ONCE"]');
  assert(ifSourceSelect, "Expected every child Template block in the FORM to expose its source selector.");
  assert(clearSourceSelect, "Expected nested Template descendants to expose the same FORM source selector.");

  const performTables = Array.from(els.templatePreviewOutput.querySelectorAll('.template-preview-table[data-object-type="PERFORM"]'));
  assert.strictEqual(performTables.length, 3);
  assert(
    performTables.every((table) => !table.closest(".template-block")?.querySelector(".perform-source-select")),
    "Expected PERFORM headers not to repeat the source selector."
  );

  const candidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM_ONCE") || [];
  els.templatePreviewOutput.scrollTop = 80;
  clearSourceSelect.value = candidates[1].key;
  clearSourceSelect.dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 8);
  assert(Math.abs(els.templatePreviewOutput.scrollTop - 80) <= 40, "Expected FORM source switching to preserve the Template viewport.");
  const selectedForm = (state.renderObjects || []).find((obj) => obj && obj.objectType === "FORM");
  const selectedClear = selectedForm?.children?.[0]?.children?.[0];
  const selectedTrace = selectedClear?.__abapPerformTraceBinding?.byParamUpper?.get("IV_VALUE") || [];
  assert.deepStrictEqual(Array.from(selectedTrace, (decl) => String(decl && decl.name || "")), ["gv_second"]);
  const synchronizedSelects = Array.from(
    els.templatePreviewOutput.querySelectorAll('.perform-source-select[data-perform-form="FRM_ONCE"]')
  );
  assert.strictEqual(synchronizedSelects.length, 3, "Expected one synchronized selector on FORM, IF, and CLEAR blocks.");
  assert(
    synchronizedSelects.every((select) => select.value === candidates[1].key),
    "Expected selecting a child block source to update the whole parent/child Template chain."
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

  const getSourceSelect = (container, formUpper) => container.querySelector(
    `.perform-source-select[data-perform-form="${formUpper}"]`
  );
  const getSourceBadge = (container, formUpper) => container.querySelector(
    `.perform-source-badge[data-perform-form="${formUpper}"]`
  );
  els.rightTabTemplateBtn.click();
  await settleViewerUi(window);
  Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  runtime.api.renderTemplatePreview();
  await settleViewerUi(window, 8);
  let templateOuterSelect = getSourceSelect(els.templatePreviewOutput, "FRM_OUTER");
  assert(templateOuterSelect, "Expected Template FORM header source selector.");
  assert.strictEqual(getSourceBadge(els.templatePreviewOutput, "FRM_OUTER")?.textContent, "⇄ 3 nguồn");
  assert.strictEqual(templateOuterSelect.options.length, 3);
  assert.strictEqual(templateOuterSelect.options[0].textContent, "Nguồn 1/3 · line 7 · USING gv_first · CHANGING gv_change_first");
  assert.strictEqual(templateOuterSelect.options[1].textContent, "Nguồn 2/3 · line 8 · USING gv_second · CHANGING gv_change_second");
  assert.strictEqual(templateOuterSelect.value, outerCandidates[0].key);

  state.selectedTemplateIndex = "0";
  state.selectedTemplateIndexes = new window.Set(["0", "2"]);
  state.templateSelectionAnchorIndex = "2";
  els.templatePreviewOutput.scrollTop = 80;

  templateOuterSelect.value = outerCandidates[1].key;
  templateOuterSelect.dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 10);

  assert.strictEqual(registry.selectedKeyByFormUpper.get("FRM_OUTER"), outerCandidates[1].key);
  assert.strictEqual(getSourceSelect(els.templatePreviewOutput, "FRM_OUTER")?.value, outerCandidates[1].key);
  assert.strictEqual(state.selectedTemplateIndex, "0");
  assert.deepStrictEqual(Array.from(state.selectedTemplateIndexes).sort(), ["0", "2"], "Expected source switching to preserve multi-selection.");
  assert(els.templatePreviewOutput.querySelector('.template-block.selected[data-template-index="0"]'), "Expected Template selection anchor to survive rebuild.");
  assert(els.templatePreviewOutput.querySelector('.template-block.selected[data-template-index="2"]'), "Expected secondary Template selection to survive rebuild.");

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
  assert(
    outerPerformRoots.every((obj) => !Array.isArray(obj.children) || obj.children.length === 0),
    "Expected every PERFORM call to remain a leaf statement."
  );
  const outerFormRoot = (state.renderObjects || []).find((obj) => (
    obj && obj.objectType === "FORM" && obj.extras?.form?.name === "frm_outer"
  ));
  assert(outerFormRoot, "Expected one source-shaped FORM root.");
  const firstLocalAssignment = findDescendant(outerFormRoot, (obj) => String(obj && obj.raw || "").trim() === "lv_local = iv_outer.");
  const firstMissingAssignment = findDescendant(outerFormRoot, (obj) => String(obj && obj.raw || "").trim() === "iv_missing = iv_outer.");
  assert(firstLocalAssignment && firstMissingAssignment);
  assert.deepStrictEqual(getBindingNames(firstLocalAssignment, "iv_outer"), ["gv_second"]);
  assert.strictEqual(firstLocalAssignment.values?.target?.decl?.name, "lv_local", "Expected local DATA to stay local.");
  assert.deepStrictEqual(getBindingNames(firstMissingAssignment, "iv_missing"), [], "Expected missing actual argument to use local FORM_PARAM fallback.");

  const findTemplateTable = (obj) => {
    const idText = `#${String(obj && obj.id || "")}`;
    const block = Array.from(els.templatePreviewOutput.querySelectorAll(".template-block"))
      .find((candidate) => String(candidate.querySelector(".template-block-meta")?.textContent || "").includes(idText));
    return block ? block.querySelector("table.template-preview-table") : null;
  };
  assert.deepStrictEqual(getTemplateTableRows(findTemplateTable(firstLocalAssignment)), [
    ["Đích", "lv_local"],
    ["Nguồn", "gv_second"]
  ]);
  assert.deepStrictEqual(getTemplateTableRows(findTemplateTable(firstMissingAssignment)), [
    ["Đích", "iv_missing"],
    ["Nguồn", "gv_second"]
  ]);

  let activeInnerCandidates = registry.getActiveCandidates("FRM_INNER");
  assert.strictEqual(activeInnerCandidates.length, 2, "Expected nested selector candidates only from the selected parent branch.");
  let templateInnerSelect = getSourceSelect(els.templatePreviewOutput, "FRM_INNER");
  assert(templateInnerSelect);
  assert.strictEqual(templateInnerSelect.options.length, 2);
  templateInnerSelect.value = activeInnerCandidates[1].key;
  templateInnerSelect.dispatchEvent(new window.Event("change", { bubbles: true }));
  await settleViewerUi(window, 10);
  assert.strictEqual(registry.selectedKeyByFormUpper.get("FRM_INNER"), activeInnerCandidates[1].key);

  const currentOuterSelect = getSourceSelect(els.templatePreviewOutput, "FRM_OUTER");
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
    (state.renderObjects || []).find((obj) => obj && obj.objectType === "FORM" && obj.extras?.form?.name === "frm_inner"),
    (obj) => String(obj && obj.raw || "").trim() === "CLEAR iv_inner."
  );
  assert(innerClear);
  assert.deepStrictEqual(getBindingNames(innerClear, "iv_inner"), ["iv_outer", "gv_third"]);
  assert.deepStrictEqual(getTemplateTableRows(findTemplateTable(innerClear)), [["CLEAR", "gv_third"]]);

  const literalRoot = (state.renderObjects || []).find((obj) => obj && obj.objectType === "FORM" && obj.extras?.form?.name === "frm_literal");
  const literalClear = findDescendant(literalRoot, (obj) => String(obj && obj.raw || "").trim() === "CLEAR iv_literal.");
  assert(literalClear);
  assert.deepStrictEqual(getBindingNames(literalClear, "iv_literal"), [], "Expected literal actual to keep local fallback.");
  assert.strictEqual(
    getSourceSelect(els.templatePreviewOutput, "FRM_LITERAL"),
    null,
    "Expected a FORM with only one source not to show a redundant selector."
  );
  assert.strictEqual(getSourceBadge(els.templatePreviewOutput, "FRM_LITERAL"), null);

  const externalRoot = (state.renderObjects || []).find((obj) => obj && obj.extras?.performCall?.program === "zother");
  assert(externalRoot);
  assert.strictEqual(Array.isArray(externalRoot.children) ? externalRoot.children.length : 0, 0);
  assert.strictEqual(getSourceSelect(els.templatePreviewOutput, "FRM_EXTERNAL"), null);

  const cycleRoot = (state.renderObjects || []).find((obj) => obj && obj.objectType === "FORM" && obj.extras?.form?.name === "frm_cycle");
  const recursivePerform = findDescendant(cycleRoot, (obj) => obj !== cycleRoot && obj?.extras?.performCall?.form === "frm_cycle");
  assert(recursivePerform, "Expected the recursive call statement itself to remain visible.");
  assert.strictEqual(Array.isArray(recursivePerform.children) ? recursivePerform.children.length : 0, 0, "Expected recursive PERFORM to remain a leaf statement.");

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
  const runtime = window.AbapViewerRuntime;
  const { els, state } = runtime;
  const largeCandidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM_MANY") || [];
  assert.strictEqual(largeCandidates.length, callCount);
  Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  runtime.api.renderTemplatePreview();
  await settleViewerUi(window, 8);
  const visibleSelects = Array.from(els.templatePreviewOutput.querySelectorAll(
    '.perform-source-select[data-perform-form="FRM_MANY"]'
  ));
  assert.strictEqual(
    visibleSelects.length,
    2,
    "Expected the FORM and its child Template block to expose synchronized source selectors."
  );
  assert(
    visibleSelects.every((select) => select.options.length === 1),
    "Expected every large source selector instance to defer option creation."
  );
  visibleSelects[1].dispatchEvent(new window.Event("focus"));
  assert.strictEqual(visibleSelects[1].options.length, callCount, "Expected focusing a child selector to populate all sources on demand.");
  assert.strictEqual(visibleSelects[0].options.length, 1, "Expected lazy options to stay local to each selector instance.");
  largeDom.window.close();
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
    assert.strictEqual(row.querySelectorAll("td").length, 40, "Expected every PERFORM row to keep 20 + 20 cells.");
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

async function assertFormSourceMultiValueRowsUseRootDescriptions() {
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
  const outerForm = (state.renderObjects || []).find((obj) => (
    obj && obj.objectType === "FORM" && obj.extras?.form?.name === "frm_outer"
  ));
  assert(outerForm, "Expected source-shaped outer FORM object.");
  const nestedPerform = (outerForm.children || []).find((obj) => (
    obj
    && obj.objectType === "PERFORM"
    && obj.extras
    && obj.extras.performCall
    && obj.extras.performCall.form === "frm_inner"
  ));
  assert(nestedPerform, "Expected nested PERFORM object inside frm_outer.");

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

async function assertDataCatalogTracesNestedPerformAndEditsSelectedChain() {
  const source = [
    "DATA gv_root TYPE string. \"Root description",
    "PERFORM frm_outer USING gv_root.",
    "FORM frm_outer USING iv_outer TYPE string.",
    "  PERFORM frm_inner USING iv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  CLEAR iv_inner.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);

  let innerGroup = findDataDeclGroup(els, "FORM:FRM_INNER");
  let innerRow = findDataDeclRow(innerGroup, "iv_inner");
  assert(innerRow, "Expected the inner FORM parameter row.");
  assert.strictEqual(
    String(innerRow.querySelector('[data-column="trace"]')?.textContent || "").trim(),
    "iv_inner ← iv_outer ← gv_root"
  );
  assert.strictEqual(
    String(innerRow.querySelector('[data-column="effective-description"]')?.textContent || "").trim(),
    "Root description"
  );

  const chainKey = String(innerRow.getAttribute("data-decl-key") || "");
  assert.match(chainKey, /^PERFORM_CHAIN:/, "Expected traced Data edits to use the selected call-chain key.");
  innerRow.querySelector('button[data-action="edit-description"]').click();
  await waitForViewerUi(window);
  els.editDesc.value = "Inner chain only";
  els.editSaveBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(String(state.descOverrides[chainKey] || ""), "Inner chain only");

  innerGroup = findDataDeclGroup(els, "FORM:FRM_INNER");
  innerRow = findDataDeclRow(innerGroup, "iv_inner");
  assert.strictEqual(
    String(innerRow.querySelector('[data-column="effective-description"]')?.textContent || "").trim(),
    "Inner chain only"
  );

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  assert(
    String(els.templatePreviewOutput.textContent || "").includes("Inner chain only"),
    "Template must resolve the description saved from Data through the same chain key."
  );

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);
  innerGroup = findDataDeclGroup(els, "FORM:FRM_INNER");
  innerRow = findDataDeclRow(innerGroup, "iv_inner");
  innerRow.querySelector('button[data-action="edit-description"]').click();
  await waitForViewerUi(window);
  els.editClearBtn.click();
  await waitForViewerUi(window);
  assert.strictEqual(Object.prototype.hasOwnProperty.call(state.descOverrides, chainKey), false);
  innerRow = findDataDeclRow(findDataDeclGroup(els, "FORM:FRM_INNER"), "iv_inner");
  assert.strictEqual(
    String(innerRow.querySelector('[data-column="effective-description"]')?.textContent || "").trim(),
    "Root description",
    "Clearing the chain override must fall back to the root declaration description."
  );

  dom.window.close();
}

async function assertDataCatalogSourceSelectorStaysSynchronized() {
  const source = [
    "DATA gv_a TYPE string. \"Root A",
    "DATA gv_b TYPE string. \"Root B",
    "PERFORM frm_pick USING gv_a.",
    "PERFORM frm_pick USING gv_b.",
    "FORM frm_pick USING iv_value TYPE string.",
    "  CLEAR iv_value.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;

  els.rightTabDescBtn.click();
  await waitForViewerUi(window);

  const candidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM_PICK") || [];
  assert.strictEqual(candidates.length, 2);
  let group = findDataDeclGroup(els, "FORM:FRM_PICK");
  let selector = group.querySelector("select.data-perform-source-select");
  assert(selector, "Expected a source selector for a FORM with multiple call sites.");
  assert.strictEqual(selector.value, candidates[0].key);
  assert(String(findDataDeclRow(group, "iv_value").textContent || "").includes("gv_a"));

  selector.value = candidates[1].key;
  selector.dispatchEvent(new window.Event("change", { bubbles: true }));
  await waitForViewerUi(window);
  assert.strictEqual(state.performSourceRegistry.getSelectedCandidate("FRM_PICK").key, candidates[1].key);
  group = findDataDeclGroup(els, "FORM:FRM_PICK");
  selector = group.querySelector("select.data-perform-source-select");
  assert.strictEqual(selector.value, candidates[1].key);
  assert(String(findDataDeclRow(group, "iv_value").textContent || "").includes("gv_b"));

  els.rightTabTemplateBtn.click();
  await waitForViewerUi(window);
  const templateSourceSelect = els.templatePreviewOutput.querySelector('select[data-perform-form="FRM_PICK"]');
  assert(templateSourceSelect, "Expected Template to expose the shared FORM source selector.");
  assert.strictEqual(templateSourceSelect.value, candidates[1].key);

  dom.window.close();
}

async function assertPerformSourcePickerRegistryTreeAndSuggestions() {
  const source = [
    "DATA gv_a TYPE string.",
    "DATA gv_b TYPE string.",
    "PERFORM frm_outer USING gv_a.",
    "PERFORM frm_outer USING gv_b.",
    "FORM frm_outer USING iv_outer TYPE string.",
    "  PERFORM frm_inner USING iv_outer.",
    "  PERFORM frm_deep USING iv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  PERFORM frm_leaf USING iv_inner.",
    "ENDFORM.",
    "FORM frm_deep USING iv_deep TYPE string.",
    "  PERFORM frm_mid USING iv_deep.",
    "ENDFORM.",
    "FORM frm_mid USING iv_mid TYPE string.",
    "  PERFORM frm_leaf USING iv_mid.",
    "ENDFORM.",
    "FORM frm_leaf USING iv_leaf TYPE string.",
    "  CLEAR iv_leaf.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const registry = dom.window.AbapViewerRuntime.state.performSourceRegistry;
  const outerCandidates = registry.candidatesByFormUpper.get("FRM_OUTER") || [];
  const innerCandidates = registry.candidatesByFormUpper.get("FRM_INNER") || [];
  const leafCandidates = registry.candidatesByFormUpper.get("FRM_LEAF") || [];

  assert.strictEqual(outerCandidates.length, 2);
  assert.strictEqual(innerCandidates.length, 2);
  assert.strictEqual(leafCandidates.length, 4);
  assert.strictEqual(outerCandidates[0].rootTreeKey, outerCandidates[0].key);
  assert.strictEqual(outerCandidates[0].depth, 0);
  assert.deepStrictEqual(Array.from(outerCandidates[0].callChain), [outerCandidates[0].key]);

  const directLeaf = leafCandidates.find((candidate) => candidate.depth === 2);
  const deepLeaf = leafCandidates.find((candidate) => candidate.depth === 3);
  assert(directLeaf && deepLeaf, "Expected both direct and deep nested leaf candidates.");
  const directParent = registry.candidateByKey.get(directLeaf.parentCandidateKey);
  assert(directParent, "Expected the direct leaf candidate to retain its parent key.");
  assert.strictEqual(directLeaf.rootTreeKey, outerCandidates[0].key);
  assert.deepStrictEqual(Array.from(directLeaf.callChain), [
    outerCandidates[0].key,
    directParent.key,
    directLeaf.key
  ]);
  assert.strictEqual(deepLeaf.rootTreeKey, outerCandidates[0].key);
  assert.strictEqual(deepLeaf.callChain.length, deepLeaf.depth + 1);

  assert.strictEqual(typeof registry.getSuggestedCandidates, "function");
  const suggestions = registry.getSuggestedCandidates("FRM_LEAF", directParent.key);
  assert(suggestions.length <= 3, "Expected at most three source suggestions.");
  assert.strictEqual(
    suggestions[0]?.parentCandidateKey,
    directParent.key,
    "Expected a candidate under the directly changed parent to rank first."
  );
  for (let index = 1; index < suggestions.length; index += 1) {
    const previous = suggestions[index - 1];
    const current = suggestions[index];
    const previousPriority = previous.parentCandidateKey === directParent.key ? 0 : 1;
    const currentPriority = current.parentCandidateKey === directParent.key ? 0 : 1;
    assert(
      previousPriority < currentPriority
        || previousPriority === currentPriority && (
          previous.depth < current.depth
          || previous.depth === current.depth && (
            previous.lineStart < current.lineStart
            || previous.lineStart === current.lineStart && previous.sourceOrder <= current.sourceOrder
          )
        ),
      "Expected source suggestions to rank direct-parent, shallower, then source-order candidates."
    );
  }

  const runtime = dom.window.AbapViewerRuntime;
  runtime.els.rightTabTemplateBtn.click();
  await settleViewerUi(dom.window, 4);
  Object.defineProperty(runtime.els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  runtime.api.renderTemplatePreview();
  await settleViewerUi(dom.window, 4);
  const leafPicker = runtime.els.templatePreviewOutput.querySelector('.perform-source-picker[data-perform-form="FRM_LEAF"]');
  assert(leafPicker, "Expected the nested FORM to expose its source trees.");
  leafPicker.querySelector(".perform-source-trigger").click();
  await settleViewerUi(dom.window, 2);
  const directRow = leafPicker.querySelector(`[data-candidate-key="${directLeaf.key}"]`);
  const deepRow = leafPicker.querySelector(`[data-candidate-key="${deepLeaf.key}"]`);
  assert(directRow && deepRow, "Expected direct and deep sources in the same root tree.");
  assert.strictEqual(
    directRow.style.getPropertyValue("--perform-source-tree-hue"),
    deepRow.style.getPropertyValue("--perform-source-tree-hue")
  );
  assert.notStrictEqual(
    directRow.style.getPropertyValue("--perform-source-tree-lightness"),
    deepRow.style.getPropertyValue("--perform-source-tree-lightness"),
    "Expected deeper branches to use a different tint while retaining the root hue."
  );

  dom.window.close();
}

async function assertPerformSourcePickerKeepsOrFallsBackToActiveSelection() {
  const source = [
    "DATA gv_a TYPE string.",
    "DATA gv_b TYPE string.",
    "DATA gv_direct TYPE string.",
    "PERFORM frm_outer USING gv_a.",
    "PERFORM frm_outer USING gv_b.",
    "PERFORM frm_inner USING gv_direct.",
    "FORM frm_outer USING iv_outer TYPE string.",
    "  PERFORM frm_inner USING iv_outer.",
    "  PERFORM frm_inner USING iv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  CLEAR iv_inner.",
    "ENDFORM."
  ].join("\n");
  const dom = await renderFixture(source);
  const registry = dom.window.AbapViewerRuntime.state.performSourceRegistry;
  const outerCandidates = registry.candidatesByFormUpper.get("FRM_OUTER") || [];
  const innerCandidates = registry.candidatesByFormUpper.get("FRM_INNER") || [];
  const nestedInner = innerCandidates.filter((candidate) => candidate.parentCandidateKey === outerCandidates[0].key);
  const directInner = innerCandidates.find((candidate) => candidate.depth === 0);
  assert.strictEqual(nestedInner.length, 2);
  assert(directInner, "Expected an independent, always-active source candidate.");

  assert.strictEqual(registry.selectCandidate("FRM_INNER", nestedInner[1].key), true);
  assert.strictEqual(registry.selectCandidate("FRM_OUTER", outerCandidates[1].key), true);
  const fallback = registry.getSuggestedCandidates("FRM_INNER", outerCandidates[1].key)[0];
  assert(fallback, "Expected a fallback suggestion for the newly active parent branch.");
  assert.strictEqual(
    registry.selectedKeyByFormUpper.get("FRM_INNER"),
    fallback.key,
    "Expected an inactive child selection to fall back to the best suggestion."
  );

  assert.strictEqual(registry.selectCandidate("FRM_INNER", directInner.key), true);
  assert.strictEqual(registry.selectCandidate("FRM_OUTER", outerCandidates[0].key), true);
  assert.strictEqual(
    registry.selectedKeyByFormUpper.get("FRM_INNER"),
    directInner.key,
    "Expected an independently active child selection to remain selected after a parent change."
  );

  dom.window.close();

  const oneActiveDom = await renderFixture([
    "DATA gv_a TYPE string.",
    "DATA gv_b TYPE string.",
    "PERFORM frm_outer USING gv_a.",
    "PERFORM frm_outer USING gv_b.",
    "FORM frm_outer USING iv_outer TYPE string.",
    "  PERFORM frm_inner USING iv_outer.",
    "ENDFORM.",
    "FORM frm_inner USING iv_inner TYPE string.",
    "  CLEAR iv_inner.",
    "ENDFORM."
  ].join("\n"));
  const oneActiveRuntime = oneActiveDom.window.AbapViewerRuntime;
  const oneActiveRegistry = oneActiveRuntime.state.performSourceRegistry;
  assert.strictEqual(oneActiveRegistry.candidatesByFormUpper.get("FRM_INNER")?.length, 2);
  assert.strictEqual(oneActiveRegistry.getActiveCandidates("FRM_INNER").length, 1);
  oneActiveRuntime.els.rightTabTemplateBtn.click();
  await settleViewerUi(oneActiveDom.window, 4);
  Object.defineProperty(oneActiveRuntime.els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  oneActiveRuntime.api.renderTemplatePreview();
  await settleViewerUi(oneActiveDom.window, 4);
  assert(
    oneActiveRuntime.els.templatePreviewOutput.querySelector('.perform-source-picker[data-perform-form="FRM_INNER"]'),
    "Expected alternative call-chain trees to remain selectable when only one candidate is active."
  );
  oneActiveDom.window.close();
}

async function assertPerformSourcePickerIsSharedSearchableAndLazy() {
  const rootCallCount = 60;
  const callCount = rootCallCount * 2;
  const sourceLines = ["DATA gv_value TYPE string."];
  for (let index = 1; index <= rootCallCount; index += 1) {
    sourceLines.push(`PERFORM frm_outer USING 'ROOT_${String(index).padStart(3, "0")}'.`);
  }
  sourceLines.push(
    "FORM frm_outer USING iv_value TYPE string.",
    "  PERFORM frm_many USING iv_value.",
    "  PERFORM frm_many USING iv_value.",
    "ENDFORM.",
    "FORM frm_many USING iv_value TYPE string.",
    "  CLEAR iv_value.",
    "ENDFORM."
  );
  const dom = await renderFixture(sourceLines.join("\n"));
  const { window } = dom;
  const { els, state } = window.AbapViewerRuntime;
  const candidates = state.performSourceRegistry.candidatesByFormUpper.get("FRM_MANY") || [];
  assert.strictEqual(candidates.length, callCount);

  const assertPicker = (container, label) => {
    const picker = container.querySelector('.perform-source-picker[data-perform-form="FRM_MANY"]');
    assert(picker, `Expected ${label} to use the shared source picker.`);
    const trigger = picker.querySelector(".perform-source-trigger");
    const popup = picker.querySelector(".perform-source-popup");
    assert(trigger && popup, `Expected ${label} picker trigger and popup.`);
    assert.strictEqual(
      popup.querySelectorAll("[data-candidate-key]").length,
      0,
      `Expected ${label} picker to defer building 120 tree rows until opened.`
    );
    return { picker, trigger, popup };
  };

  els.rightTabDescBtn.click();
  await settleViewerUi(window, 8);
  const dataPicker = assertPicker(els.declDescPanel, "Data");

  els.rightTabTemplateBtn.click();
  await settleViewerUi(window, 8);
  Object.defineProperty(els.templatePreviewOutput, "clientHeight", { configurable: true, value: 100000 });
  window.AbapViewerRuntime.api.renderTemplatePreview();
  await settleViewerUi(window, 8);
  let templatePicker = assertPicker(els.templatePreviewOutput, "Template");
  Object.defineProperty(window, "innerWidth", { configurable: true, value: 800 });
  Object.defineProperty(window, "innerHeight", { configurable: true, value: 600 });
  templatePicker.trigger.getBoundingClientRect = () => ({
    left: 730,
    right: 790,
    top: 100,
    bottom: 130,
    width: 60,
    height: 30
  });
  templatePicker.trigger.dispatchEvent(new window.KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
  await settleViewerUi(window, 4);
  assert.strictEqual(templatePicker.popup.hidden, false);
  assert.strictEqual(templatePicker.trigger.getAttribute("aria-expanded"), "true");
  const popupLeft = Number.parseFloat(templatePicker.popup.style.left);
  assert(Number.isFinite(popupLeft) && popupLeft >= 12, "Expected the popup to clamp inside the viewport.");
  assert(templatePicker.popup.style.top, "Expected opening the popup to calculate a viewport-safe vertical position.");
  let search = templatePicker.popup.querySelector(".perform-source-search");
  let suggestions = templatePicker.popup.querySelector(".perform-source-suggestions");
  let tree = templatePicker.popup.querySelector(".perform-source-tree");
  assert(search && suggestions && tree, "Expected popup search, suggestions, and tree sections.");
  assert.strictEqual(window.document.activeElement, search, "Expected opening the picker to focus its search.");
  const suggestedRows = Array.from(suggestions.querySelectorAll('[data-suggested="true"][data-candidate-key]'));
  assert(suggestedRows.length > 0 && suggestedRows.length <= 3, "Expected one to three actionable source suggestions.");
  assert.strictEqual(tree.querySelectorAll("[data-candidate-key]").length, callCount);

  const treeRows = Array.from(tree.querySelectorAll("[data-candidate-key]"));
  const rowsByRoot = new Map();
  for (const row of treeRows) {
    const rootTreeKey = row.getAttribute("data-root-tree-key");
    if (!rowsByRoot.has(rootTreeKey)) {
      rowsByRoot.set(rootTreeKey, []);
    }
    rowsByRoot.get(rootTreeKey).push(row);
  }
  const sameTreeRows = Array.from(rowsByRoot.values()).find((rows) => rows.length === 2);
  const differentTreeRows = Array.from(rowsByRoot.values());
  assert(sameTreeRows && differentTreeRows.length > 1, "Expected nested sources to retain their root trees.");
  assert.strictEqual(
    sameTreeRows[0].style.getPropertyValue("--perform-source-tree-hue"),
    sameTreeRows[1].style.getPropertyValue("--perform-source-tree-hue"),
    "Expected source rows from one root tree to use the same hue."
  );
  assert.notStrictEqual(
    differentTreeRows[0][0].style.getPropertyValue("--perform-source-tree-hue"),
    differentTreeRows[1][0].style.getPropertyValue("--perform-source-tree-hue"),
    "Expected different source trees to use different hues."
  );

  const selectedTreeRow = tree.querySelector(".perform-source-candidate-row.is-selected");
  const selectedRoot = selectedTreeRow?.closest(".perform-source-root");
  const selectedRootToggle = selectedRoot?.querySelector(".perform-source-root-toggle");
  const selectedRootGroup = selectedRoot?.querySelector('[role="group"]');
  assert(selectedTreeRow && selectedRootToggle && selectedRootGroup, "Expected an accessible root treeitem and child group.");
  assert.strictEqual(selectedRootToggle.getAttribute("role"), "treeitem");
  assert.strictEqual(selectedRootToggle.getAttribute("aria-level"), "1");
  assert.strictEqual(selectedTreeRow.getAttribute("role"), "treeitem");
  assert(Number(selectedTreeRow.getAttribute("aria-level")) > 1, "Expected child rows to expose their call depth.");
  selectedTreeRow.focus();
  selectedTreeRow.dispatchEvent(new window.KeyboardEvent("keydown", { key: "ArrowLeft", bubbles: true }));
  assert.strictEqual(window.document.activeElement, selectedRootToggle, "Expected collapsing a tree to preserve keyboard focus.");
  assert.strictEqual(selectedRootToggle.getAttribute("aria-expanded"), "false");
  selectedRootToggle.dispatchEvent(new window.KeyboardEvent("keydown", { key: "ArrowRight", bubbles: true }));
  assert.strictEqual(selectedRootToggle.getAttribute("aria-expanded"), "true");

  const visibleControls = () => Array.from(templatePicker.popup.querySelectorAll(
    ".perform-source-suggestion, .perform-source-root-toggle, .perform-source-candidate-row"
  )).filter((node) => !node.hidden && node.style.display !== "none" && !node.closest("[hidden]"));
  selectedRootToggle.dispatchEvent(new window.KeyboardEvent("keydown", { key: "End", bubbles: true }));
  assert.strictEqual(window.document.activeElement, visibleControls().at(-1));
  window.document.activeElement.dispatchEvent(new window.KeyboardEvent("keydown", { key: "Home", bubbles: true }));
  assert.strictEqual(window.document.activeElement, visibleControls()[0]);
  assert(
    String(templatePicker.trigger.textContent || "").includes("ROOT_001"),
    "Expected the trigger to identify the selected source arguments."
  );

  const firstSuggestedKey = suggestedRows
    .map((row) => row.getAttribute("data-candidate-key"))
    .find((key) => key && key !== state.performSourceRegistry.selectedKeyByFormUpper.get("FRM_MANY"));
  assert(firstSuggestedKey, "Expected a suggestion different from the initial source.");
  suggestions.querySelector(`[data-candidate-key="${firstSuggestedKey}"]`).click();
  await settleViewerUi(window, 6);
  assert.strictEqual(state.performSourceRegistry.selectedKeyByFormUpper.get("FRM_MANY"), firstSuggestedKey);

  els.rightTabDescBtn.click();
  await settleViewerUi(window, 6);
  const syncedDataPicker = els.declDescPanel.querySelector('.perform-source-picker[data-perform-form="FRM_MANY"]');
  assert(syncedDataPicker && syncedDataPicker.isConnected, "Expected Data to rebuild the shared picker after Template selection.");
  assert.strictEqual(
    syncedDataPicker.querySelector(".data-perform-source-select")?.value,
    firstSuggestedKey,
    "Expected Data and Template pickers to share the selected source."
  );

  els.rightTabTemplateBtn.click();
  await settleViewerUi(window, 6);
  templatePicker = assertPicker(els.templatePreviewOutput, "Template after suggestion");
  templatePicker.trigger.click();
  await settleViewerUi(window, 2);
  search = templatePicker.popup.querySelector(".perform-source-search");
  tree = templatePicker.popup.querySelector(".perform-source-tree");
  search.value = "ROOT_060";
  search.dispatchEvent(new window.Event("input", { bubbles: true }));
  await settleViewerUi(window, 2);
  assert.strictEqual(
    window.getComputedStyle(templatePicker.popup.querySelector(".perform-source-suggestions")).display,
    "none",
    "Expected search results to replace unrelated suggestions while a query is active."
  );
  const visibleRows = Array.from(tree.querySelectorAll("[data-candidate-key]")).filter((row) => (
    !row.hidden && row.style.display !== "none"
  ));
  assert(
    visibleRows.length > 0 && visibleRows.length < callCount,
    "Expected source picker search to filter tree rows."
  );
  assert(
    Array.from(tree.querySelectorAll(".perform-source-root[hidden]"))
      .every((section) => window.getComputedStyle(section).display === "none"),
    "Expected search to hide root sections that have no matching source."
  );
  const selectedKey = visibleRows[0].getAttribute("data-candidate-key");
  search.dispatchEvent(new window.KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
  await settleViewerUi(window, 6);
  assert.strictEqual(state.performSourceRegistry.selectedKeyByFormUpper.get("FRM_MANY"), selectedKey);

  templatePicker = assertPicker(els.templatePreviewOutput, "Template after search selection");
  assert(templatePicker.picker.isConnected, "Expected the picker queried after selection to belong to the current render.");
  templatePicker.trigger.click();
  await settleViewerUi(window, 2);
  search = templatePicker.popup.querySelector(".perform-source-search");
  search.dispatchEvent(new window.KeyboardEvent("keydown", { key: "Escape", bubbles: true }));
  await settleViewerUi(window, 2);
  assert.strictEqual(templatePicker.popup.hidden, true, "Expected Escape to close the source picker popup.");

  dom.window.close();
}

defineFocusedTest(test, "viewer if template contract", ["if-template"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("if initial uses condition template", async () => {
    await assertIfInitialUsesConditionTemplate();
  });
});

defineFocusedTest(test, "viewer perform root trace contract", ["perform-root-trace"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("form source trace uses root declarations", async () => {
    await assertFormSourceTraceUsesRootDeclarations();
  });
});

defineFocusedTest(test, "viewer local method source chain contract", ["local-method-source-chain"], async (t) => {
  assertViewerFixtureDirectoriesStayInSync();

  await t.test("local method call sites use independent selected chains", async () => {
    await assertLocalMethodSourceSelectionUsesScopedChains();
  });

  await t.test("nested local methods keep scoped sources and remap struct components", async () => {
    await assertNestedLocalMethodSourcesStayScopedAndMapStructComponents();
  });

  await t.test("nested local method component actual follows selected root", async () => {
    await assertNestedLocalMethodComponentActualUsesSelectedRoot();
  });

  await t.test("typed local instance method creates a source chain", async () => {
    await assertTypedLocalInstanceMethodCreatesSourceChain();
  });
});

defineFocusedTest(test, "viewer perform source selection contracts", ["perform-source-selection"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("template renders each form once in source order", async () => {
    await assertTemplateRendersEachFormOnceInSourceOrder();
  });

  await t.test("global perform source selection", async () => {
    await assertGlobalPerformSourceSelection();
  });

  await t.test("perform sources use source order and lazy large selectors", async () => {
    await assertPerformSourcesUseSourceOrderAndLazyLargeSelectors();
  });
});

defineFocusedTest(test, "viewer perform-call multi value contract", ["template-multi-value-perform-call"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("perform and call multi value rows", async () => {
    await assertPerformAndCallMultiValueRows();
  });
});

defineFocusedTest(test, "viewer perform-root multi value contract", ["template-multi-value-perform-root"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("form source multi value rows use root descriptions", async () => {
    await assertFormSourceMultiValueRowsUseRootDescriptions();
  });
});

defineFocusedTest(test, "viewer data perform trace contracts", ["data-perform-trace"], async (t) => {
assertViewerFixtureDirectoriesStayInSync();

  await t.test("data catalog traces nested perform and edits selected chain", async () => {
    await assertDataCatalogTracesNestedPerformAndEditsSelectedChain();
  });

  await t.test("data catalog source selector stays synchronized", async () => {
    await assertDataCatalogSourceSelectorStaysSynchronized();
  });
});

defineFocusedTest(test, "viewer perform source picker contracts", ["perform-source-picker"], async (t) => {
  assertViewerFixtureDirectoriesStayInSync();

  await t.test("registry records stable source trees and ranks suggestions", async () => {
    await assertPerformSourcePickerRegistryTreeAndSuggestions();
  });

  await t.test("parent changes preserve active children or select the best fallback", async () => {
    await assertPerformSourcePickerKeepsOrFallsBackToActiveSelection();
  });

  await t.test("shared popup is lazy, searchable, tree-based, and keyboard accessible", async () => {
    await assertPerformSourcePickerIsSharedSearchableAndLazy();
  });
});
