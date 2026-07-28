"use strict";

const assert = require("assert");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assertHasObjectTypes,
  findObject,
  findObjects,
  flattenObjects,
  fs,
  getConfigFileNames,
  getValue,
  getValueEntry,
  parse,
  path
} = require("./helpers/parser-test-helpers");

function testMultipleStatementsOnSingleLine() {
  const result = parse("DATA lv_a TYPE i. DATA lv_b TYPE i. lv_a = 1. lv_b = 2.\n");
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");
  const assignmentObjects = findObjects(objects, "ASSIGNMENT");

  assert.strictEqual(dataObjects.length, 2, "Expected two DATA objects from one-line statements.");
  assert.strictEqual(assignmentObjects.length, 2, "Expected two ASSIGNMENT objects from one-line statements.");
  assert.strictEqual(getValue(dataObjects[0].values, "name"), "lv_a");
  assert.strictEqual(getValue(dataObjects[1].values, "name"), "lv_b");
  assert.strictEqual(getValue(assignmentObjects[0].values, "target"), "lv_a");
  assert.strictEqual(getValue(assignmentObjects[1].values, "target"), "lv_b");
  assert.strictEqual(dataObjects[0].lineStart, 1, "First DATA statement should keep its source line.");
  assert.strictEqual(dataObjects[1].lineStart, 1, "Second DATA statement should keep its source line.");
  assert.strictEqual(assignmentObjects[0].lineStart, 1, "First assignment should keep its source line.");
  assert.strictEqual(assignmentObjects[1].lineStart, 1, "Second assignment should keep its source line.");
}

function testSingleLineTrailingCommentAppliesToLastStatementOnly() {
  const result = parse("DATA lv_a TYPE i. DATA lv_b TYPE i. \"last-data-comment\n");
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");

  assert.strictEqual(dataObjects.length, 2, "Expected two DATA objects from one-line statements with comment.");
  assert.strictEqual(dataObjects[0].comment, "");
  assert.strictEqual(dataObjects[1].comment, "last-data-comment");
}

function testDecimalLiteralDoesNotSplitStatement() {
  const result = parse("DATA lv_total TYPE decfloat34.\nlv_total = 1.5.\n");
  const objects = flattenObjects(result.objects);
  const assignmentObjects = findObjects(objects, "ASSIGNMENT");

  assert.strictEqual(assignmentObjects.length, 1, "Expected a single ASSIGNMENT for decimal literal.");
  assert.strictEqual(assignmentObjects[0].raw, "lv_total = 1.5.");
  assert.strictEqual(getValue(assignmentObjects[0].values, "expr"), "1.5");
}

function testChainedDataStatementSingleLine() {
  const result = parse("DATA: lv_a TYPE i, lv_b TYPE i.\n");
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");

  assert.strictEqual(dataObjects.length, 2, "Expected chained DATA statement to split into two DATA objects.");
  assert.strictEqual(getValue(dataObjects[0].values, "name"), "lv_a");
  assert.strictEqual(getValue(dataObjects[1].values, "name"), "lv_b");
}

function testChainedDataStatementAcrossLines() {
  const code = [
    "DATA: lv_a TYPE i,",
    "      lv_b TYPE i.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");

  assert.strictEqual(dataObjects.length, 2, "Expected multi-line chained DATA statement to split into two DATA objects.");
  assert.strictEqual(getValue(dataObjects[0].values, "name"), "lv_a");
  assert.strictEqual(getValue(dataObjects[1].values, "name"), "lv_b");
}

function testChainedDataStatementKeepsCommaInsideTemplateLiteral() {
  const result = parse("DATA: lv_text TYPE string VALUE |A, B|, lv_other TYPE string VALUE |C|.\n");
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");

  assert.strictEqual(dataObjects.length, 2, "Expected commas inside template literals not to break chained DATA splitting.");
  assert.strictEqual(getValue(dataObjects[0].values, "name"), "lv_text");
  assert.strictEqual(getValue(dataObjects[0].values, "value"), "|A, B|");
  assert.strictEqual(getValue(dataObjects[1].values, "name"), "lv_other");
  assert.strictEqual(getValue(dataObjects[1].values, "value"), "|C|");
}

function testChainedConstantsKeepItemCommentsWithoutHeaderLeak() {
  const code = [
    "* Hằng số dùng chung",
    "CONSTANTS: gc_true TYPE abap_bool VALUE abap_true,  \"Giá trị boolean đúng",
    "  gc_status_empty TYPE string VALUE 'EMPTY'.    \"Trạng thái chưa có khách",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = findObjects(flattenObjects(result.objects), "CONSTANTS");

  assert.strictEqual(objects.length, 2, "Expected two chained CONSTANTS objects.");
  assert.strictEqual(objects[0].comment, "Giá trị boolean đúng");
  assert.strictEqual(objects[1].comment, "Trạng thái chưa có khách");
  assert.strictEqual(getValueEntry(objects[0].values, "name").codeDesc, "Giá trị boolean đúng");
  assert.strictEqual(getValueEntry(objects[1].values, "name").codeDesc, "Trạng thái chưa có khách");
}

function testChainedConstantsUseSingleInternalCommentForNextItem() {
  const code = [
    "* Hằng số dùng chung",
    "CONSTANTS:",
    "* Giá trị boolean đúng",
    "  gc_true TYPE abap_bool VALUE abap_true,",
    "  gc_status_empty TYPE string VALUE 'EMPTY'.    \"Trạng thái chưa có khách",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = findObjects(flattenObjects(result.objects), "CONSTANTS");

  assert.strictEqual(objects.length, 2, "Expected two chained CONSTANTS objects.");
  assert.strictEqual(objects[0].comment, "Giá trị boolean đúng");
  assert.strictEqual(objects[1].comment, "Trạng thái chưa có khách");
  assert.strictEqual(getValueEntry(objects[0].values, "name").codeDesc, "Giá trị boolean đúng");
  assert.strictEqual(getValueEntry(objects[1].values, "name").codeDesc, "Trạng thái chưa có khách");
}

function testConstantsCaptureCompleteInitializer() {
  const code = [
    "CONSTANTS: gc_initial TYPE string VALUE IS INITIAL,",
    "  gc_text TYPE string VALUE 'READY'.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = findObjects(flattenObjects(result.objects), "CONSTANTS");

  assert.strictEqual(objects.length, 2, "Expected both chained constants.");
  assert.strictEqual(
    getValue(objects[0].values, "value"),
    "IS INITIAL",
    "Expected the full multi-token constant initializer."
  );
  assert.strictEqual(getValue(objects[1].values, "value"), "'READY'");
}

function testGenericChainedStatementUsesPerItemComment() {
  const code = [
    "* Header must not describe a chained item",
    "CLEAR:",
    "* Clear first target",
    "  lv_first,",
    "  lv_second. \"Clear second target",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = findObjects(flattenObjects(result.objects), "CLEAR");

  assert.strictEqual(objects.length, 2, "Expected CLEAR chain to use the generic chained splitter.");
  assert.strictEqual(getValue(objects[0].values, "target"), "lv_first");
  assert.strictEqual(getValue(objects[1].values, "target"), "lv_second");
  assert.strictEqual(objects[0].comment, "Clear first target");
  assert.strictEqual(objects[1].comment, "Clear second target");
}

function testChainedCommentsRejectBlocksGapsAndUnfinishedItems() {
  const code = [
    "DATA:",
    "* block line one",
    "* block line two",
    "  lv_block TYPE i,",
    "* separated comment",
    "",
    "  lv_gap TYPE i,",
    "* separated by decoration",
    "* -----------------------",
    "  lv_decorated TYPE i,",
    "  lv_unfinished TYPE",
    "* comment inside unfinished item",
    "    i.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = findObjects(flattenObjects(result.objects), "DATA");

  assert.deepStrictEqual(
    objects.map((obj) => obj.comment),
    ["", "", "", ""],
    "Comment blocks, gaps, decorative gaps, and comments inside an unfinished item must not describe chained items."
  );
}

function testChainedStructCommentsUseSegmentMetadata() {
  const code = [
    "* DATA header must not become the root description",
    "DATA:",
    "* Data root description",
    "  BEGIN OF ls_data,",
    "  field_inline TYPE string, \"Inline data field",
    "* Internal data field",
    "  field_internal TYPE i,",
    "  END OF ls_data.",
    "* TYPES header must not become the root description",
    "TYPES:",
    "* Type root description",
    "  BEGIN OF ty_data,",
    "  field_inline TYPE string, \"Inline type field",
    "* Internal type field",
    "  field_internal TYPE i,",
    "  END OF ty_data.",
    "DATA ls_typed TYPE ty_data.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");
  const typeObjects = findObjects(objects, "TYPES");
  const declByName = new Map(result.decls.map((decl) => [String(decl.name || "").toUpperCase(), decl]));

  assert.strictEqual(dataObjects[0].comment, "Data root description");
  assert.strictEqual(dataObjects[1].comment, "Inline data field");
  assert.strictEqual(dataObjects[2].comment, "Internal data field");
  assert.strictEqual(typeObjects[0].comment, "Type root description");
  assert.strictEqual(typeObjects[1].comment, "Inline type field");
  assert.strictEqual(typeObjects[2].comment, "Internal type field");
  assert.strictEqual(declByName.get("LS_DATA").comment, "Data root description");
  assert.strictEqual(declByName.get("LS_DATA-FIELD_INLINE").comment, "Inline data field");
  assert.strictEqual(declByName.get("LS_DATA-FIELD_INTERNAL").comment, "Internal data field");
  assert.strictEqual(declByName.get("TY_DATA").comment, "Type root description");
  assert.strictEqual(declByName.get("LS_TYPED-FIELD_INLINE").comment, "Inline type field");
  assert.strictEqual(declByName.get("LS_TYPED-FIELD_INTERNAL").comment, "Internal type field");
}

function testNonChainedStructKeepsSingleLeadingComment() {
  const code = [
    "* Non-chain data root description",
    "DATA BEGIN OF ls_data,",
    "  field_one TYPE string,",
    "  END OF ls_data.",
    "* Non-chain type root description",
    "TYPES BEGIN OF ty_data,",
    "  field_one TYPE string,",
    "  END OF ty_data.",
    "DATA ls_typed TYPE ty_data.",
    ""
  ].join("\n");

  const result = parse(code);
  const declByName = new Map(result.decls.map((decl) => [String(decl.name || "").toUpperCase(), decl]));
  const dataRoot = declByName.get("LS_DATA");
  const dataField = declByName.get("LS_DATA-FIELD_ONE");
  const typeRoot = declByName.get("TY_DATA");
  const typedField = declByName.get("LS_TYPED-FIELD_ONE");

  assert(dataRoot && dataField && typeRoot && typedField, "Expected non-chain DATA/TYPES struct declarations.");
  assert.strictEqual(dataRoot.comment, "Non-chain data root description");
  assert.strictEqual(dataField.structComment, "Non-chain data root description");
  assert.strictEqual(typeRoot.comment, "Non-chain type root description");
  assert.strictEqual(typedField.structTypeComment, "Non-chain type root description");
}

function testNonChainedStructKeepsLeadingCommentBlock() {
  const code = [
    "* Non-chain data root",
    "* second documentation line",
    "DATA BEGIN OF ls_data_block,",
    "  field_one TYPE string,",
    "  END OF ls_data_block.",
    ""
  ].join("\n");

  const result = parse(code);
  const declByName = new Map(result.decls.map((decl) => [String(decl.name || "").toUpperCase(), decl]));
  const dataRoot = declByName.get("LS_DATA_BLOCK");
  const dataField = declByName.get("LS_DATA_BLOCK-FIELD_ONE");

  assert(dataRoot && dataField, "Expected the non-chain struct declaration and field.");
  assert.strictEqual(dataRoot.comment, "Non-chain data root second documentation line");
  assert.strictEqual(dataField.structComment, "Non-chain data root second documentation line");
}

function testBacktickLiteralKeepsStatementAndInlineComment() {
  const result = parse('DATA lv_text TYPE string VALUE `A.B "C`. DATA lv_other TYPE string.\n');
  const objects = flattenObjects(result.objects);
  const dataObjects = findObjects(objects, "DATA");

  assert.strictEqual(dataObjects.length, 2, "Expected backtick literals to keep the statement intact.");
  assert.strictEqual(getValue(dataObjects[0].values, "name"), "lv_text");
  assert.strictEqual(getValue(dataObjects[0].values, "value"), '`A.B "C`');
  assert.strictEqual(dataObjects[0].comment, "");
  assert.strictEqual(getValue(dataObjects[1].values, "name"), "lv_other");
}

function testInlineCommentInsideSingleQuote() {
  const result = parse("DATA lv TYPE string.\nlv = 'A\"B'.\n");
  const objects = flattenObjects(result.objects);
  const assignment = findObject(objects, "ASSIGNMENT");
  assert(assignment, "Expected ASSIGNMENT object for string containing quote.");
  assert.strictEqual(assignment.raw, "lv = 'A\"B'.");
}

function testInlineCommentInsideTemplate() {
  const result = parse("DATA lv TYPE string.\nlv = |A \" B|.\n");
  const objects = flattenObjects(result.objects);
  const assignment = findObject(objects, "ASSIGNMENT");
  assert(assignment, "Expected ASSIGNMENT object for template containing quote.");
  assert.strictEqual(assignment.raw, "lv = |A \" B|.");
}

function testEscapedSingleQuoteTokenization() {
  const result = parse("DATA lv TYPE string.\nlv = 'a''b'.\n");
  const objects = flattenObjects(result.objects);
  const assignment = findObject(objects, "ASSIGNMENT");
  assert(assignment, "Expected ASSIGNMENT object for escaped quote.");
  assert.strictEqual(getValue(assignment.values, "expr"), "'a''b'");
}

function testAssignmentKeepsFullExpression() {
  const code = [
    "DATA lo TYPE REF TO object.",
    "CALL METHOD lo->m",
    "  EXPORTING",
    "    iv = VALUE string( 'A' ).",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const callMethod = findObject(objects, "CALL_METHOD");
  assert(callMethod && callMethod.extras && callMethod.extras.callMethod, "Expected CALL_METHOD extras.");
  const exporting = callMethod.extras.callMethod.exporting || [];
  assert(exporting.length > 0, "Expected at least one EXPORTING assignment.");
  assert.strictEqual(exporting[0].value, "VALUE string( 'A' )");
}

function testInlineDataReferenceInAssignment() {
  const code = [
    "CALL METHOD lo->m",
    "  IMPORTING",
    "    ev = DATA(lv).",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const callMethod = findObject(objects, "CALL_METHOD");
  assert(callMethod && callMethod.extras && callMethod.extras.callMethod, "Expected CALL_METHOD extras.");
  const importing = callMethod.extras.callMethod.importing || [];
  assert(importing.length > 0, "Expected at least one IMPORTING assignment.");
  assert.strictEqual(importing[0].valueRef, "lv");
}

function testInlineFieldSymbolAssigningBindsDecl() {
  const code = [
    "DATA gt_rows TYPE TABLE OF i.",
    "READ TABLE gt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) INDEX 1.",
    "LOOP AT gt_rows ASSIGNING FIELD-SYMBOL(<ls_loop>).",
    "ENDLOOP.",
    "APPEND INITIAL LINE TO gt_rows ASSIGNING FIELD-SYMBOL(<ls_app>).",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const readTable = findObject(objects, "READ_TABLE");
  const readAssigning = getValueEntry(readTable && readTable.values, "assigning");
  assert(readAssigning, "Expected READ TABLE assigning value.");
  assert.strictEqual(readAssigning.value, "FIELD-SYMBOL(<ls_row>)");
  assert.strictEqual(readAssigning.declRef, "<ls_row>");
  assert(readAssigning.decl, "Expected decl bind for inline FIELD-SYMBOL on READ TABLE.");
  assert.strictEqual(readAssigning.decl.name, "<ls_row>");
  assert.strictEqual(readAssigning.decl.objectType, "INLINE");

  const loop = findObject(objects, "LOOP_AT_ITAB");
  const loopAssigning = getValueEntry(loop && loop.values, "assigning");
  assert(loopAssigning, "Expected LOOP ASSIGNING value.");
  assert.strictEqual(loopAssigning.declRef, "<ls_loop>");
  assert.strictEqual(loopAssigning.decl && loopAssigning.decl.name, "<ls_loop>");

  const append = findObject(objects, "APPEND");
  const appendAssigning = getValueEntry(append && append.values, "assigning");
  assert(appendAssigning, "Expected APPEND ASSIGNING value.");
  assert.strictEqual(appendAssigning.declRef, "<ls_app>");
  assert.strictEqual(appendAssigning.decl && appendAssigning.decl.name, "<ls_app>");
}

function testSelectForAllEntriesCapturesItabAndDecl() {
  const code = [
    "DATA gt_flight_source TYPE TABLE OF sflight.",
    "DATA gt_routes TYPE TABLE OF spfli.",
    "SELECT carrid connid",
    "  FROM spfli",
    "  INTO TABLE gt_routes",
    "  FOR ALL ENTRIES IN gt_flight_source",
    "  WHERE carrid = gt_flight_source-carrid",
    "    AND connid = gt_flight_source-connid.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const select = findObject(objects, "SELECT");
  assert(select, "Expected SELECT object.");

  const forAllEntries = getValueEntry(select.values, "forAllEntries");
  assert(forAllEntries, "Expected values.forAllEntries from FOR ALL ENTRIES IN.");
  assert.strictEqual(forAllEntries.value, "gt_flight_source");
  assert.strictEqual(forAllEntries.declRef, "gt_flight_source");
  assert(forAllEntries.decl, "Expected decl bind for FOR ALL ENTRIES itab.");
  assert.strictEqual(forAllEntries.decl.name, "gt_flight_source");
  assert.strictEqual(forAllEntries.decl.objectType, "DATA");

  assert.strictEqual(getValue(select.values, "intoTable"), "gt_routes");
  assert.strictEqual(getValue(select.values, "from"), "spfli");
  assert(
    select.keywords && select.keywords["for-all-entries-in"],
    "Expected keyword phrase FOR ALL ENTRIES IN."
  );
}

function testSelectForAllEntriesHostEscapeAndCorrespondingTable() {
  const hostCode = [
    "DATA gt_src TYPE TABLE OF i.",
    "DATA gt_dst TYPE TABLE OF i.",
    "SELECT table_line FROM dbtab INTO TABLE @gt_dst",
    "  FOR ALL ENTRIES IN @gt_src",
    "  WHERE table_line = @gt_src-table_line.",
    ""
  ].join("\n");

  const hostResult = parse(hostCode);
  const hostSelect = findObject(flattenObjects(hostResult.objects), "SELECT");
  assert(hostSelect, "Expected SELECT with host escapes.");

  const hostInto = getValueEntry(hostSelect.values, "intoTable");
  assert.strictEqual(hostInto && hostInto.value, "@gt_dst");
  assert.strictEqual(hostInto && hostInto.declRef, "gt_dst");
  assert.strictEqual(hostInto && hostInto.decl && hostInto.decl.name, "gt_dst");

  const hostFae = getValueEntry(hostSelect.values, "forAllEntries");
  assert.strictEqual(hostFae && hostFae.value, "@gt_src");
  assert.strictEqual(hostFae && hostFae.declRef, "gt_src");
  assert.strictEqual(hostFae && hostFae.decl && hostFae.decl.name, "gt_src");

  const whereConditions = hostSelect.extras
    && hostSelect.extras.select
    && Array.isArray(hostSelect.extras.select.whereConditions)
    ? hostSelect.extras.select.whereConditions
    : [];
  assert.strictEqual(whereConditions.length, 1);
  assert.strictEqual(whereConditions[0].rightOperand, "@gt_src-table_line");
  assert.strictEqual(whereConditions[0].rightOperandRef, "gt_src-table_line");

  const correspondingCode = [
    "DATA gt_src TYPE TABLE OF i.",
    "DATA gt_dst TYPE TABLE OF i.",
    "SELECT carrid connid FROM spfli",
    "  INTO CORRESPONDING FIELDS OF TABLE gt_dst",
    "  FOR ALL ENTRIES IN gt_src",
    "  WHERE carrid = gt_src-carrid.",
    ""
  ].join("\n");
  const correspondingSelect = findObject(flattenObjects(parse(correspondingCode).objects), "SELECT");
  assert(correspondingSelect, "Expected SELECT with INTO CORRESPONDING FIELDS OF TABLE.");
  const correspondingInto = getValueEntry(correspondingSelect.values, "intoTable");
  assert.strictEqual(correspondingInto && correspondingInto.value, "gt_dst");
  assert.strictEqual(correspondingInto && correspondingInto.declRef, "gt_dst");
  assert.strictEqual(getValue(correspondingSelect.values, "forAllEntries"), "gt_src");
  assert(
    correspondingSelect.keywords && correspondingSelect.keywords["into-corresponding-fields-of-table"],
    "Expected keyword phrase INTO CORRESPONDING FIELDS OF TABLE."
  );
  assert.strictEqual(getValue(correspondingSelect.values, "into"), "");

  const appendingCode = [
    "DATA gt_src TYPE TABLE OF i.",
    "DATA gt_dst TYPE TABLE OF i.",
    "SELECT carrid FROM spfli",
    "  APPENDING CORRESPONDING FIELDS OF TABLE @gt_dst",
    "  FOR ALL ENTRIES IN @gt_src",
    "  WHERE carrid = @gt_src-carrid.",
    ""
  ].join("\n");
  const appendingSelect = findObject(flattenObjects(parse(appendingCode).objects), "SELECT");
  assert(appendingSelect, "Expected SELECT with APPENDING CORRESPONDING FIELDS OF TABLE.");
  const appendingTable = getValueEntry(appendingSelect.values, "appendingTable");
  assert.strictEqual(appendingTable && appendingTable.value, "@gt_dst");
  assert.strictEqual(appendingTable && appendingTable.declRef, "gt_dst");
  assert.strictEqual(getValue(appendingSelect.values, "forAllEntries"), "@gt_src");
  assert(
    appendingSelect.keywords && appendingSelect.keywords["appending-corresponding-fields-of-table"],
    "Expected keyword phrase APPENDING CORRESPONDING FIELDS OF TABLE."
  );
}

function testCallMethodExpressionWithAssignmentReceiver() {
  const code = [
    "DATA lv_result TYPE syuname.",
    "DATA p_user TYPE syuname.",
    "lv_result = lcl_demo=>get_default( EXPORTING iv_user = p_user ).",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const callMethod = findObject(objects, "CALL_METHOD");
  assert(callMethod && callMethod.extras && callMethod.extras.callMethod, "Expected CALL_METHOD object for expression call.");
  assert.strictEqual(getValue(callMethod.values, "target"), "lcl_demo=>get_default");
  assert.strictEqual(getValue(callMethod.values, "receivingRaw"), "result = lv_result");

  const extras = callMethod.extras.callMethod;
  assert.strictEqual(extras.target, "lcl_demo=>get_default");
  assert.strictEqual(extras.exporting.length, 1);
  assert.strictEqual(extras.exporting[0].name, "iv_user");
  assert.strictEqual(extras.exporting[0].value, "p_user");
  assert.strictEqual(extras.exporting[0].valueRef, "p_user");
  assert(extras.exporting[0].valueDecl, "Expected decl for expression EXPORTING argument.");
  assert.strictEqual(extras.exporting[0].valueDecl.name, "p_user");

  assert.strictEqual(extras.receiving.length, 1);
  assert.strictEqual(extras.receiving[0].value, "lv_result");
  assert.strictEqual(extras.receiving[0].valueRef, "lv_result");
  assert(extras.receiving[0].valueDecl, "Expected decl for expression receiving target.");
  assert.strictEqual(extras.receiving[0].valueDecl.name, "lv_result");
}

function testCallMethodExpressionStandalone() {
  const code = [
    "DATA lo_demo TYPE REF TO object.",
    "DATA p_user TYPE syuname.",
    "lo_demo->do_something( EXPORTING iv_user = p_user ).",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const callMethod = findObject(objects, "CALL_METHOD");
  assert(callMethod && callMethod.extras && callMethod.extras.callMethod, "Expected CALL_METHOD object for standalone expression call.");
  assert.strictEqual(getValue(callMethod.values, "target"), "lo_demo->do_something");
  assert.strictEqual(getValue(callMethod.values, "exportingRaw"), "iv_user = p_user");
  assert.strictEqual(getValue(callMethod.values, "receivingRaw"), "");

  const extras = callMethod.extras.callMethod;
  assert.strictEqual(extras.target, "lo_demo->do_something");
  assert.strictEqual(extras.exporting.length, 1);
  assert.strictEqual(extras.exporting[0].name, "iv_user");
  assert.strictEqual(extras.exporting[0].value, "p_user");
  assert.strictEqual(extras.exporting[0].valueRef, "p_user");
  assert(extras.exporting[0].valueDecl, "Expected decl for standalone expression EXPORTING argument.");
  assert.strictEqual(extras.exporting[0].valueDecl.name, "p_user");
  assert.strictEqual(extras.receiving.length, 0);
}

function testStatementCommentPrefersFirstInline() {
  const code = [
    "PERFORM main",
    "  USING p_user \"first-inline",
    "        p_flag \"second-inline",
    "  CHANGING lv_text.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const perform = findObject(objects, "PERFORM");
  assert(perform, "Expected PERFORM object.");
  assert.strictEqual(perform.comment, "first-inline");

  const formEntry = getValueEntry(perform.values, "form");
  assert(formEntry, "Expected values.form entry.");
  assert.strictEqual(formEntry.codeDesc, "first-inline");
}

function testStatementCommentFallsBackToSingleLeadingLine() {
  const code = [
    "\"leading-comment",
    "PERFORM main USING p_user CHANGING lv_text.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const perform = findObject(objects, "PERFORM");
  assert(perform, "Expected PERFORM object.");
  assert.strictEqual(perform.comment, "leading-comment");

  const formEntry = getValueEntry(perform.values, "form");
  assert(formEntry, "Expected values.form entry.");
  assert.strictEqual(formEntry.codeDesc, "leading-comment");
}

function testStatementCommentIgnoresLeadingCommentBlock() {
  const code = [
    "\"line-1",
    "\"line-2",
    "PERFORM main USING p_user CHANGING lv_text.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const perform = findObject(objects, "PERFORM");
  assert(perform, "Expected PERFORM object.");
  assert.strictEqual(perform.comment, "");

  const formEntry = getValueEntry(perform.values, "form");
  assert(formEntry, "Expected values.form entry.");
  assert.strictEqual(formEntry.codeDesc, "");
}

function testStatementCommentIgnoresLeadingCommentWithBlankGap() {
  const code = [
    "\"leading-comment",
    "",
    "PERFORM main USING p_user CHANGING lv_text.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const perform = findObject(objects, "PERFORM");
  assert(perform, "Expected PERFORM object.");
  assert.strictEqual(perform.comment, "");

  const formEntry = getValueEntry(perform.values, "form");
  assert(formEntry, "Expected values.form entry.");
  assert.strictEqual(formEntry.codeDesc, "");
}

function testElseStartsSiblingBranch() {
  const result = parse([
    "IF lv_flag = abap_true.",
    "  CLEAR lv_then.",
    "ELSE.",
    "  CLEAR lv_else.",
    "ENDIF.",
    "WRITE lv_done."
  ].join("\n"));

  assert.deepStrictEqual(
    result.objects.map((obj) => obj.objectType),
    ["IF", "ELSE", "WRITE"],
    "ELSE must be a sibling of its IF."
  );

  const [ifObject, elseObject] = result.objects;
  assert.deepStrictEqual(ifObject.children.map((obj) => obj.objectType), ["CLEAR"]);
  assert.deepStrictEqual(elseObject.children.map((obj) => obj.objectType), ["CLEAR"]);
  assert.strictEqual(ifObject.children[0].parent, ifObject.id);
  assert.strictEqual(elseObject.children[0].parent, elseObject.id);
  assert.strictEqual(elseObject.block.endRaw, "ENDIF.");
}

function testSupportedStatementSmokeMatrix() {
  const cases = [
    {
      name: "append",
      covers: ["append.json"],
      expectedTypes: ["APPEND"],
      code: "APPEND ls_row TO lt_rows.\n"
    },
    {
      name: "assignment",
      covers: ["assignment.json"],
      expectedTypes: ["ASSIGNMENT"],
      code: "lv_total = gv_total + 1.\n"
    },
    {
      name: "call-function",
      covers: ["call-function.json"],
      expectedTypes: ["CALL_FUNCTION"],
      code: "CALL FUNCTION 'Z_DEMO' EXPORTING iv_user = p_user IMPORTING ev_text = lv_text.\n"
    },
    {
      name: "call-method-expression",
      covers: ["call-method-expression.json"],
      expectedTypes: ["CALL_METHOD"],
      code: "lv_result = lcl_demo=>get_default( EXPORTING iv_user = p_user ).\n"
    },
    {
      name: "call-method-classic",
      covers: ["call-method.json"],
      expectedTypes: ["CALL_METHOD"],
      code: "CALL METHOD lo_demo->run EXPORTING iv_user = p_user IMPORTING ev_text = lv_text.\n"
    },
    {
      name: "call-transaction",
      covers: ["call-transaction.json"],
      expectedTypes: ["CALL_TRANSACTION"],
      code: "CALL TRANSACTION 'SE38'.\n"
    },
    {
      name: "case-when",
      covers: ["case.json", "when.json"],
      expectedTypes: ["CASE", "WHEN"],
      code: "CASE lv_kind. WHEN 'A'. ENDCASE.\n"
    },
    {
      name: "try-catch-cleanup",
      covers: ["try.json", "catch.json", "cleanup.json"],
      expectedTypes: ["TRY", "CATCH", "CLEANUP"],
      code: "TRY. CATCH cx_root INTO DATA(lx_root). CLEANUP. ENDTRY.\n"
    },
    {
      name: "class-data",
      covers: ["class-data.json"],
      expectedTypes: ["CLASS-DATA"],
      code: "CLASS-DATA gv_count TYPE i.\n"
    },
    {
      name: "class-methods",
      covers: ["class-methods.json"],
      expectedTypes: ["CLASS-METHODS"],
      code: "CLASS-METHODS build RETURNING VALUE(rv_text) TYPE string.\n"
    },
    {
      name: "class",
      covers: ["class.json"],
      expectedTypes: ["CLASS"],
      code: "CLASS lcl_demo DEFINITION. ENDCLASS.\n"
    },
    {
      name: "clear",
      covers: ["clear.json"],
      expectedTypes: ["CLEAR"],
      code: "CLEAR lv_text.\n"
    },
    {
      name: "concatenate",
      covers: ["concatenate.json"],
      expectedTypes: ["CONCATENATE"],
      code: "CONCATENATE lv_a lv_b INTO lv_c SEPARATED BY lv_sep.\n"
    },
    {
      name: "constants",
      covers: ["constants.json"],
      expectedTypes: ["CONSTANTS"],
      code: "CONSTANTS gc_flag TYPE abap_bool VALUE abap_true.\n"
    },
    {
      name: "data",
      covers: ["data.json"],
      expectedTypes: ["DATA"],
      code: "DATA lv_text TYPE string.\n"
    },
    {
      name: "delete-itab",
      covers: ["delete-itab.json"],
      expectedTypes: ["DELETE_ITAB"],
      code: "DELETE lt_rows WHERE id = lv_id.\n"
    },
    {
      name: "do",
      covers: ["do.json"],
      expectedTypes: ["DO"],
      code: "DO 2 TIMES. ENDDO.\n"
    },
    {
      name: "if-elseif-else",
      covers: ["if.json", "elseif.json", "else.json"],
      expectedTypes: ["IF", "ELSEIF", "ELSE"],
      code: "IF lv_kind = 'A'. ELSEIF lv_kind = 'B'. ELSE. ENDIF.\n"
    },
    {
      name: "field-symbols",
      covers: ["field-symbols.json"],
      expectedTypes: ["FIELD-SYMBOLS"],
      code: "FIELD-SYMBOLS <ls_row> TYPE any.\n"
    },
    {
      name: "form",
      covers: ["form.json"],
      expectedTypes: ["FORM"],
      code: "FORM main USING p_user TYPE syuname. ENDFORM.\n"
    },
    {
      name: "insert-itab",
      covers: ["insert-itab.json"],
      expectedTypes: ["INSERT_ITAB"],
      code: "INSERT ls_row INTO TABLE lt_rows.\n"
    },
    {
      name: "loop-at-itab",
      covers: ["loop-at-itab.json"],
      expectedTypes: ["LOOP_AT_ITAB"],
      code: "LOOP AT lt_rows INTO ls_row. ENDLOOP.\n"
    },
    {
      name: "method",
      covers: ["method.json"],
      expectedTypes: ["METHOD"],
      code: "METHOD run. ENDMETHOD.\n"
    },
    {
      name: "methods",
      covers: ["methods.json"],
      expectedTypes: ["METHODS"],
      code: "METHODS run IMPORTING iv_user TYPE syuname RETURNING VALUE(rv_text) TYPE string.\n"
    },
    {
      name: "message",
      covers: ["message.json"],
      expectedTypes: ["MESSAGE"],
      code: "MESSAGE 'Saved' TYPE 'S'.\n"
    },
    {
      name: "modify-itab",
      covers: ["modify-itab.json"],
      expectedTypes: ["MODIFY_ITAB"],
      code: "MODIFY lt_rows FROM ls_row TRANSPORTING name WHERE id = lv_id.\n"
    },
    {
      name: "move-corresponding",
      covers: ["move-corresponding.json"],
      expectedTypes: ["MOVE-CORRESPONDING"],
      code: "MOVE-CORRESPONDING ls_src TO ls_dst.\n"
    },
    {
      name: "move",
      covers: ["move.json"],
      expectedTypes: ["MOVE"],
      code: "MOVE lv_src TO lv_dst.\n"
    },
    {
      name: "parameters",
      covers: ["parameters.json"],
      expectedTypes: ["PARAMETERS"],
      code: "PARAMETERS p_user TYPE syuname.\n"
    },
    {
      name: "perform",
      covers: ["perform.json"],
      expectedTypes: ["PERFORM"],
      code: "PERFORM main USING p_user CHANGING lv_text.\n"
    },
    {
      name: "ranges",
      covers: ["ranges.json"],
      expectedTypes: ["RANGES"],
      code: "RANGES lr_user FOR sy-uname.\n"
    },
    {
      name: "read-table",
      covers: ["read-table.json"],
      expectedTypes: ["READ_TABLE"],
      code: "READ TABLE lt_rows WITH KEY id = lv_id INTO ls_row.\n"
    },
    {
      name: "select-options",
      covers: ["select-options.json"],
      expectedTypes: ["SELECT-OPTIONS"],
      code: "SELECT-OPTIONS s_user FOR sy-uname.\n"
    },
    {
      name: "select",
      covers: ["select.json"],
      expectedTypes: ["SELECT"],
      code: "SELECT * FROM usr02 INTO TABLE lt_users WHERE bname = p_user.\n"
    },
    {
      name: "sort-itab",
      covers: ["sort-itab.json"],
      expectedTypes: ["SORT_ITAB"],
      code: "SORT lt_rows BY id.\n"
    },
    {
      name: "statics",
      covers: ["statics.json"],
      expectedTypes: ["STATICS"],
      code: "STATICS sv_count TYPE i.\n"
    },
    {
      name: "types",
      covers: ["types.json"],
      expectedTypes: ["TYPES"],
      code: "TYPES ty_text TYPE string.\n"
    },
    {
      name: "write",
      covers: ["write.json"],
      expectedTypes: ["WRITE"],
      code: "WRITE 'Saved'.\n"
    }
  ];

  const coveredConfigFiles = new Set();
  for (const smokeCase of cases) {
    for (const configFile of smokeCase.covers) {
      coveredConfigFiles.add(configFile);
    }
    const result = parse(smokeCase.code);
    const objects = flattenObjects(result.objects);
    assertHasObjectTypes(objects, smokeCase.expectedTypes, smokeCase.name);
  }

  assert.deepStrictEqual(
    Array.from(coveredConfigFiles).sort(),
    getConfigFileNames(),
    "Smoke matrix must cover every parser config file."
  );
}

function assertWhereClean(select, label) {
  const where = getValue(select.values, "where");
  const whereRaw = select.extras && select.extras.select ? String(select.extras.select.whereRaw || "") : "";
  const text = `${where}\n${whereRaw}`;
  assert(!/\bINTO\b/i.test(text), `${label} whereClean: where must not contain INTO. Got: ${text}`);
  assert(!/\bAPPENDING\b/i.test(text), `${label} whereClean: where must not contain APPENDING. Got: ${text}`);
  const conditions = select.extras && select.extras.select && Array.isArray(select.extras.select.whereConditions)
    ? select.extras.select.whereConditions
    : [];
  for (const clause of conditions) {
    const right = String(clause && clause.rightOperand || "");
    assert(!/\bINTO\b/i.test(right), `${label} whereClean: rightOperand must not contain INTO. Got: ${right}`);
  }
}

function testSelectOpenSqlFieldsWhereIntoFromDeepSample() {
  const samplePath = path.join(__dirname, "..", "examples", "deep_form_demo.abap");
  const sample = fs.readFileSync(samplePath, "utf8");
  const selects = findObjects(flattenObjects(parse(sample).objects), "SELECT");
  assert.ok(selects.length >= 5, `S*: Expected at least 5 SELECT objects from deep sample. Got ${selects.length}`);

  const s1 = selects.find((obj) => /SELECT\s+SINGLE\s+FROM\s+scarr/i.test(String(obj.raw || "")));
  assert(s1, "S1: Expected SELECT SINGLE FROM scarr.");
  assert.strictEqual(getValue(s1.values, "fields"), "carrname", "S1 fieldsOK");
  assertWhereClean(s1, "S1");
  assert.strictEqual(getValue(s1.values, "into"), "@gv_default_carrier_name", "S1 intoOK");
  assert(s1.keywords && s1.keywords.fields, "S1 kwFIELDS");
  const s1Cond = s1.extras.select.whereConditions[0];
  assert.strictEqual(String(s1Cond.rightOperand || ""), "@gs_request-carrid", "S1 rightOperand");

  const cursorSelect = selects.find((obj) =>
    /INNER\s+JOIN/i.test(String(obj.raw || ""))
    && /FROM\s+sflight/i.test(String(obj.raw || ""))
  );
  if (cursorSelect) {
    assert(
      String(getValue(cursorSelect.values, "fields") || "").includes("f~carrid"),
      "S2 fieldsOK"
    );
    assertWhereClean(cursorSelect, "S2");
    assert(!/\bORDER\b/i.test(getValue(cursorSelect.values, "where")), "S2 where must not swallow ORDER");
  }

  const s3 = selects.find((obj) => /gt_sql_summary/i.test(String(obj.raw || "")));
  assert(s3, "S3: Expected aggregate SELECT into gt_sql_summary.");
  assert(
    String(getValue(s3.values, "fields") || "").startsWith("carrid"),
    `S3 fieldsOK. Got: ${getValue(s3.values, "fields")}`
  );
  assertWhereClean(s3, "S3");
  assert(!/\bGROUP\b/i.test(getValue(s3.values, "where")), "S3 where must not swallow GROUP");
  assert.strictEqual(getValue(s3.values, "having").replace(/\s+/g, " ").trim(), "COUNT( * ) > 0", "S3 havingClean");
  assert.strictEqual(getValue(s3.values, "intoTable"), "@gt_sql_summary", "S3 intoOK");

  const s4 = selects.find((obj) => /gt_planetypes/i.test(String(obj.raw || "")));
  assert(s4, "S4: Expected DISTINCT planetype SELECT.");
  assert(
    String(getValue(s4.values, "fields") || "").includes("planetype"),
    `S4 fieldsOK. Got: ${getValue(s4.values, "fields")}`
  );
  assertWhereClean(s4, "S4");
  assert.strictEqual(getValue(s4.values, "intoTable"), "@gt_planetypes", "S4 intoOK");

  const s5 = selects.find((obj) => /gt_active_carriers/i.test(String(obj.raw || "")));
  assert(s5, "S5: Expected EXISTS outer SELECT.");
  assert(
    String(getValue(s5.values, "fields") || "").includes("c~carrid"),
    `S5 fieldsOK. Got: ${getValue(s5.values, "fields")}`
  );
  assert(/\bEXISTS\b/i.test(getValue(s5.values, "where")), "S5 where contains EXISTS");
  assertWhereClean(s5, "S5");
  assert.strictEqual(getValue(s5.values, "intoTable"), "@gt_active_carriers", "S5 intoOK");

  const nestedExists = selects.find((obj) =>
    /FROM\s+sflight\s+AS\s+f/i.test(String(obj.raw || ""))
    && /FIELDS\s+f~carrid/i.test(String(obj.raw || ""))
    && !/gt_active_carriers/i.test(String(obj.raw || ""))
  );
  if (nestedExists) {
    assert(String(getValue(nestedExists.values, "fields") || "").includes("f~carrid"), "S6 fieldsOK");
    assertWhereClean(nestedExists, "S6");
  }

  const s7 = selects.find((obj) => /gt_union_carriers/i.test(String(obj.raw || "")) || /\bUNION\b/i.test(String(obj.raw || "")));
  assert(s7, "S7: Expected UNION SELECT.");
  assert.strictEqual(getValue(s7.values, "fields"), "carrid", "S7 fieldsOK");
  assertWhereClean(s7, "S7");
  assert(!/\bUNION\b/i.test(getValue(s7.values, "where")), "S7 where must stop before UNION");
  assert.strictEqual(getValue(s7.values, "intoTable"), "@gt_union_carriers", "S7 intoOK");
}

function testSelectClassicSingleFieldsBeforeFrom() {
  const code = [
    "DATA gv_name TYPE scarr-carrname.",
    "SELECT SINGLE carrname FROM scarr INTO @gv_name WHERE carrid = 'AA'."
  ].join("\n");
  const select = findObject(flattenObjects(parse(code).objects), "SELECT");
  assert(select, "R2: Expected classic SELECT SINGLE.");
  assert.strictEqual(getValue(select.values, "fields"), "carrname", "R2 fieldsOK");
  assert.strictEqual(getValue(select.values, "into"), "@gv_name", "R2 intoOK");
  assertWhereClean(select, "R2");
}

function testSelectClassicFieldsIntoTableWhere() {
  const code = [
    "DATA gt_rows TYPE TABLE OF t001.",
    "SELECT bukrs butxt FROM t001 INTO TABLE gt_rows WHERE bukrs IN s_bukrs."
  ].join("\n");
  const select = findObject(flattenObjects(parse(code).objects), "SELECT");
  assert(select, "R3: Expected classic SELECT.");
  assert(String(getValue(select.values, "fields") || "").includes("bukrs"), "R3 fieldsOK");
  assert.strictEqual(getValue(select.values, "intoTable"), "gt_rows", "R3 intoTable");
  assertWhereClean(select, "R3");
}

function testSelectOpenSqlInlineIntoDataAndFieldSymbol() {
  const i1 = findObject(flattenObjects(parse(
    "SELECT SINGLE FROM scarr FIELDS carrname WHERE carrid = @lv_id INTO @DATA(lv_carrname)."
  ).objects), "SELECT");
  assert(i1, "I1: Expected SELECT with INTO @DATA.");
  assert.strictEqual(getValue(i1.values, "fields"), "carrname", "I1 fieldsOK");
  assertWhereClean(i1, "I1");
  assert(!/DATA\(/i.test(getValue(i1.values, "where")), "I1 where must not contain DATA(");
  assert.strictEqual(getValue(i1.values, "into"), "@DATA(lv_carrname)", "I1 intoOK");
  const i1Decl = getValueEntry(i1.values, "into");
  assert.strictEqual(String(i1Decl && i1Decl.decl && i1Decl.decl.name || ""), "lv_carrname", "I1 declOK name");

  const i2 = findObject(flattenObjects(parse(
    "SELECT FROM scarr FIELDS carrid, carrname WHERE carrid = @lv_id INTO TABLE @DATA(lt_carriers)."
  ).objects), "SELECT");
  assert(i2, "I2: Expected SELECT INTO TABLE @DATA.");
  assert(String(getValue(i2.values, "fields") || "").includes("carrid"), "I2 fieldsOK");
  assertWhereClean(i2, "I2");
  assert.strictEqual(getValue(i2.values, "intoTable"), "@DATA(lt_carriers)", "I2 intoOK");
  const i2Decl = getValueEntry(i2.values, "intoTable");
  assert.strictEqual(String(i2Decl && i2Decl.decl && i2Decl.decl.name || ""), "lt_carriers", "I2 declOK name");

  const i3 = findObject(flattenObjects(parse(
    "SELECT SINGLE FROM scarr FIELDS carrname WHERE carrid = @lv_id INTO @FIELD-SYMBOL(<lv_carrname>)."
  ).objects), "SELECT");
  assert(i3, "I3: Expected SELECT with INTO @FIELD-SYMBOL.");
  assert.strictEqual(getValue(i3.values, "fields"), "carrname", "I3 fieldsOK");
  assertWhereClean(i3, "I3");
  assert.strictEqual(getValue(i3.values, "into"), "@FIELD-SYMBOL(<lv_carrname>)", "I3 intoOK");
  const i3Decl = getValueEntry(i3.values, "into");
  assert.strictEqual(String(i3Decl && i3Decl.decl && i3Decl.decl.name || ""), "<lv_carrname>", "I3 declOK name");
}

function testReadTableBinarySearchTransportingNoFields() {
  const bothOrders = [
    "READ TABLE lt_tab WITH KEY id = lv_id TRANSPORTING NO FIELDS BINARY SEARCH.",
    "READ TABLE lt_tab WITH KEY id = lv_id BINARY SEARCH TRANSPORTING NO FIELDS."
  ];

  for (const code of bothOrders) {
    const obj = findObject(flattenObjects(parse(code).objects), "READ_TABLE");
    assert(obj, `Expected READ_TABLE for: ${code}`);
    assert.strictEqual(getValue(obj.values, "itab"), "lt_tab", `itab for: ${code}`);
    assert.strictEqual(getValue(obj.values, "withKey"), "id = lv_id", `withKey must stop before flags for: ${code}`);
    assert.strictEqual(getValue(obj.values, "binarySearch"), "X", `binarySearch flag for: ${code}`);
    assert.strictEqual(
      getValue(obj.values, "transportingNoFields"),
      "X",
      `transportingNoFields flag for: ${code}`
    );
    assert(obj.keywords && obj.keywords["binary-search"], `keyword binary-search for: ${code}`);
    assert(
      obj.keywords && obj.keywords["transporting-no-fields"],
      `keyword transporting-no-fields for: ${code}`
    );
    assert.strictEqual(
      obj.extras && obj.extras.readTable && obj.extras.readTable.binarySearch,
      true,
      `extras.binarySearch for: ${code}`
    );
    assert.strictEqual(
      obj.extras && obj.extras.readTable && obj.extras.readTable.transportingNoFields,
      true,
      `extras.transportingNoFields for: ${code}`
    );
  }
}

function testLoopAtFromIndexCapturesIndexValue() {
  const cases = [
    {
      code: "LOOP AT lt_tab INTO ls_row FROM INDEX lv_from.\nENDLOOP.\n",
      from: "lv_from",
      to: ""
    },
    {
      code: "LOOP AT lt_tab INTO ls_row FROM INDEX lv_from TO lv_to.\nENDLOOP.\n",
      from: "lv_from",
      to: "lv_to"
    },
    {
      code: "LOOP AT lt_tab FROM INDEX lv_from INTO ls_row.\nENDLOOP.\n",
      from: "lv_from",
      to: ""
    }
  ];

  for (const item of cases) {
    const obj = findObject(flattenObjects(parse(item.code).objects), "LOOP_AT_ITAB");
    assert(obj, `Expected LOOP_AT_ITAB for: ${item.code}`);
    assert.strictEqual(getValue(obj.values, "from"), item.from, `from for: ${item.code}`);
    assert.notStrictEqual(getValue(obj.values, "from"), "INDEX", `from must not be INDEX keyword for: ${item.code}`);
    if (item.to) {
      assert.strictEqual(getValue(obj.values, "to"), item.to, `to for: ${item.code}`);
    }
    assert.strictEqual(
      obj.extras && obj.extras.loopAtItab && obj.extras.loopAtItab.from,
      item.from,
      `extras.from for: ${item.code}`
    );
  }

  const plainFrom = findObject(flattenObjects(parse(
    "LOOP AT lt_tab INTO ls_row FROM lv_from TO lv_to.\nENDLOOP.\n"
  ).objects), "LOOP_AT_ITAB");
  assert(plainFrom, "Expected LOOP_AT_ITAB for plain FROM idx.");
  assert.strictEqual(getValue(plainFrom.values, "from"), "lv_from");
  assert.strictEqual(getValue(plainFrom.values, "to"), "lv_to");
}

function testConcatenateStatement() {
  const basic = findObject(flattenObjects(parse(
    "CONCATENATE lv_a lv_b INTO lv_c."
  ).objects), "CONCATENATE");
  assert(basic, "Expected CONCATENATE object.");
  assert.strictEqual(getValue(basic.values, "sources"), "lv_a lv_b");
  assert.strictEqual(getValue(basic.values, "into"), "lv_c");

  const separated = findObject(flattenObjects(parse(
    "CONCATENATE rs_flight-cityfrom rs_flight-cityto INTO rs_flight-route_text SEPARATED BY ' -> '."
  ).objects), "CONCATENATE");
  assert(separated, "Expected CONCATENATE with SEPARATED BY.");
  assert.strictEqual(getValue(separated.values, "sources"), "rs_flight-cityfrom rs_flight-cityto");
  assert.strictEqual(getValue(separated.values, "into"), "rs_flight-route_text");
  assert.strictEqual(getValue(separated.values, "separatedBy"), "' -> '");

  const linesOf = findObject(flattenObjects(parse(
    "CONCATENATE LINES OF lt_parts INTO lv_text."
  ).objects), "CONCATENATE");
  assert(linesOf, "Expected CONCATENATE LINES OF.");
  assert.strictEqual(getValue(linesOf.values, "linesOf"), "lt_parts");
  assert.strictEqual(getValue(linesOf.values, "into"), "lv_text");
}

defineFocusedTest(test, "parser statements regression", ["statements"], async (t) => {
  await t.test("multiple statements on single line", () => {
    testMultipleStatementsOnSingleLine();
  });

  await t.test("single line trailing comment applies to last statement only", () => {
    testSingleLineTrailingCommentAppliesToLastStatementOnly();
  });

  await t.test("decimal literal does not split statement", () => {
    testDecimalLiteralDoesNotSplitStatement();
  });

  await t.test("chained data statement single line", () => {
    testChainedDataStatementSingleLine();
  });

  await t.test("chained data statement across lines", () => {
    testChainedDataStatementAcrossLines();
  });

  await t.test("chained data statement keeps comma inside template literal", () => {
    testChainedDataStatementKeepsCommaInsideTemplateLiteral();
  });

  await t.test("chained constants keep item comments without header leak", () => {
    testChainedConstantsKeepItemCommentsWithoutHeaderLeak();
  });

  await t.test("chained constants use single internal comment for next item", () => {
    testChainedConstantsUseSingleInternalCommentForNextItem();
  });

  await t.test("constants capture complete initializer", () => {
    testConstantsCaptureCompleteInitializer();
  });

  await t.test("generic chained statement uses per item comment", () => {
    testGenericChainedStatementUsesPerItemComment();
  });

  await t.test("chained comments reject blocks gaps and unfinished items", () => {
    testChainedCommentsRejectBlocksGapsAndUnfinishedItems();
  });

  await t.test("chained struct comments use segment metadata", () => {
    testChainedStructCommentsUseSegmentMetadata();
  });

  await t.test("non chained struct keeps single leading comment", () => {
    testNonChainedStructKeepsSingleLeadingComment();
  });

  await t.test("non chained struct keeps leading comment block", () => {
    testNonChainedStructKeepsLeadingCommentBlock();
  });

  await t.test("backtick literal keeps statement and inline comment", () => {
    testBacktickLiteralKeepsStatementAndInlineComment();
  });

  await t.test("inline comment inside single quote", () => {
    testInlineCommentInsideSingleQuote();
  });

  await t.test("inline comment inside template", () => {
    testInlineCommentInsideTemplate();
  });

  await t.test("escaped single quote tokenization", () => {
    testEscapedSingleQuoteTokenization();
  });

  await t.test("assignment keeps full expression", () => {
    testAssignmentKeepsFullExpression();
  });

  await t.test("inline data reference in assignment", () => {
    testInlineDataReferenceInAssignment();
  });

  await t.test("inline field-symbol assigning binds decl", () => {
    testInlineFieldSymbolAssigningBindsDecl();
  });

  await t.test("select for all entries captures itab and decl", () => {
    testSelectForAllEntriesCapturesItabAndDecl();
  });

  await t.test("select for all entries host escape and corresponding table", () => {
    testSelectForAllEntriesHostEscapeAndCorrespondingTable();
  });

  await t.test("select open sql fields where into from deep sample", () => {
    testSelectOpenSqlFieldsWhereIntoFromDeepSample();
  });

  await t.test("select classic single fields before from", () => {
    testSelectClassicSingleFieldsBeforeFrom();
  });

  await t.test("select classic fields into table where", () => {
    testSelectClassicFieldsIntoTableWhere();
  });

  await t.test("select open sql inline into data and field-symbol", () => {
    testSelectOpenSqlInlineIntoDataAndFieldSymbol();
  });

  await t.test("call method expression with assignment receiver", () => {
    testCallMethodExpressionWithAssignmentReceiver();
  });

  await t.test("call method expression standalone", () => {
    testCallMethodExpressionStandalone();
  });

  await t.test("statement comment prefers first inline", () => {
    testStatementCommentPrefersFirstInline();
  });

  await t.test("statement comment falls back to single leading line", () => {
    testStatementCommentFallsBackToSingleLeadingLine();
  });

  await t.test("statement comment ignores leading comment block", () => {
    testStatementCommentIgnoresLeadingCommentBlock();
  });

  await t.test("statement comment ignores leading comment with blank gap", () => {
    testStatementCommentIgnoresLeadingCommentWithBlankGap();
  });

  await t.test("else starts sibling branch", () => {
    testElseStartsSiblingBranch();
  });

  await t.test("read table binary search transporting no fields", () => {
    testReadTableBinarySearchTransportingNoFields();
  });

  await t.test("loop at from index captures index value", () => {
    testLoopAtFromIndexCapturesIndexValue();
  });

  await t.test("concatenate statement", () => {
    testConcatenateStatement();
  });

  await t.test("supported statement smoke matrix", () => {
    testSupportedStatementSmokeMatrix();
  });
});
