"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { parseAbapText } = require("./helpers/source-parser");
const { loadConfigs } = require("../cli/config-loader");

const configs = loadConfigs(path.resolve(__dirname, "..", "configs"));
const configsDir = path.resolve(__dirname, "..", "configs");

function parse(code) {
  return parseAbapText(code, configs, "test.abap");
}

function flattenObjects(roots) {
  const out = [];
  const stack = Array.isArray(roots) ? roots.slice().reverse() : [];
  while (stack.length) {
    const node = stack.pop();
    if (!node) {
      continue;
    }
    out.push(node);
    const children = Array.isArray(node.children) ? node.children : [];
    for (let index = children.length - 1; index >= 0; index -= 1) {
      stack.push(children[index]);
    }
  }
  return out;
}

function findObject(objects, objectType) {
  return objects.find((obj) => obj && obj.objectType === objectType) || null;
}

function findObjects(objects, objectType) {
  return objects.filter((obj) => obj && obj.objectType === objectType);
}

function getValue(values, key) {
  if (!values || typeof values !== "object") {
    return "";
  }
  const entryOrList = values[key];
  const entry = Array.isArray(entryOrList) ? entryOrList[0] : entryOrList;
  return entry && typeof entry.value === "string" ? entry.value : "";
}

function getValueEntry(values, key) {
  if (!values || typeof values !== "object") {
    return null;
  }
  const entryOrList = values[key];
  return Array.isArray(entryOrList) ? (entryOrList[0] || null) : (entryOrList || null);
}

function assertHasObjectTypes(objects, expectedTypes, caseName) {
  const actualTypes = new Set(
    (Array.isArray(objects) ? objects : [])
      .map((obj) => (obj && obj.objectType ? obj.objectType : ""))
      .filter(Boolean)
  );

  for (const expectedType of expectedTypes) {
    assert(
      actualTypes.has(expectedType),
      `${caseName}: Expected ${expectedType} object. Got [${Array.from(actualTypes).join(", ")}].`
    );
  }
}

function getConfigFileNames() {
  return fs.readdirSync(configsDir)
    .filter((name) => name.toLowerCase().endsWith(".json"))
    .sort();
}

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

function testReadTableWithKeyConditions() {
  const code = [
    "DATA p_user TYPE syuname.",
    "DATA lv_active TYPE abap_bool.",
    "READ TABLE lt_user WITH KEY uname = p_user active = lv_active INTO ls_user.",
    ""
  ].join("\n");
  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const readTable = findObject(objects, "READ_TABLE");
  assert(readTable && readTable.extras && readTable.extras.readTable, "Expected READ_TABLE extras.");

  const conditions = Array.isArray(readTable.extras.readTable.conditions) ? readTable.extras.readTable.conditions : [];
  assert.strictEqual(conditions.length, 2);
  assert.strictEqual(conditions[0].leftOperand, "uname");
  assert.strictEqual(conditions[0].rightOperand, "p_user");
  assert.strictEqual(conditions[0].comparisonOperator, "=");
  assert.strictEqual(conditions[0].logicalConnector, "AND");
  assert.strictEqual(conditions[0].rightOperandRef, "p_user");
  assert(conditions[0].rightOperandDecl, "Expected decl for right operand p_user.");
  assert.strictEqual(conditions[0].rightOperandDecl.name, "p_user");

  assert.strictEqual(conditions[1].leftOperand, "active");
  assert.strictEqual(conditions[1].rightOperand, "lv_active");
  assert.strictEqual(conditions[1].comparisonOperator, "=");
  assert.strictEqual(conditions[1].logicalConnector, "");
  assert.strictEqual(conditions[1].rightOperandRef, "lv_active");
  assert(conditions[1].rightOperandDecl, "Expected decl for right operand lv_active.");
  assert.strictEqual(conditions[1].rightOperandDecl.name, "lv_active");
}

function testReadTableWithTableKeyConditions() {
  const code = [
    "DATA p_user TYPE syuname.",
    "READ TABLE lt_user WITH TABLE KEY primary_key COMPONENTS uname = p_user OR active = abap_true INTO ls_user.",
    ""
  ].join("\n");
  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const readTable = findObject(objects, "READ_TABLE");
  assert(readTable && readTable.extras && readTable.extras.readTable, "Expected READ_TABLE extras.");

  const extras = readTable.extras.readTable;
  assert.strictEqual(extras.withTableKeyRaw, "primary_key COMPONENTS uname = p_user OR active = abap_true");

  const conditions = Array.isArray(extras.conditions) ? extras.conditions : [];
  assert.strictEqual(conditions.length, 2);
  assert.strictEqual(conditions[0].leftOperand, "uname");
  assert.strictEqual(conditions[0].rightOperand, "p_user");
  assert.strictEqual(conditions[0].comparisonOperator, "=");
  assert.strictEqual(conditions[0].logicalConnector, "OR");
  assert.strictEqual(conditions[0].rightOperandRef, "p_user");
  assert(conditions[0].rightOperandDecl, "Expected decl for right operand p_user.");
  assert.strictEqual(conditions[0].rightOperandDecl.name, "p_user");

  assert.strictEqual(conditions[1].leftOperand, "active");
  assert.strictEqual(conditions[1].rightOperand, "abap_true");
  assert.strictEqual(conditions[1].comparisonOperator, "=");
  assert.strictEqual(conditions[1].logicalConnector, "");
  assert.strictEqual(conditions[1].rightOperandRef, "abap_true");
  assert(conditions[1].rightOperandDecl, "Expected system decl for right operand abap_true.");
  assert.strictEqual(conditions[1].rightOperandDecl.objectType, "SYSTEM");
  assert.strictEqual(conditions[1].rightOperandDecl.name, "ABAP_TRUE");
}

function testWhereConditionExtrasForSimilarStatements() {
  const code = [
    "DATA p_user TYPE syuname.",
    "LOOP AT lt_user INTO ls_user WHERE uname = p_user AND active = abap_true.",
    "ENDLOOP.",
    "MODIFY lt_user FROM ls_user WHERE uname = p_user.",
    "DELETE lt_user WHERE active = abap_false.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const loopAt = findObject(objects, "LOOP_AT_ITAB");
  assert(loopAt && loopAt.extras && loopAt.extras.loopAtItab, "Expected LOOP_AT_ITAB extras.");
  assert.deepStrictEqual(
    loopAt.extras.loopAtItab.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "uname",
        rightOperand: "p_user",
        comparisonOperator: "=",
        logicalConnector: "AND"
      },
      {
        leftOperand: "active",
        rightOperand: "abap_true",
        comparisonOperator: "=",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(loopAt.extras.loopAtItab.conditions[0].rightOperandRef, "p_user");
  assert(loopAt.extras.loopAtItab.conditions[0].rightOperandDecl, "Expected decl for LOOP condition right operand.");
  assert.strictEqual(loopAt.extras.loopAtItab.conditions[0].rightOperandDecl.name, "p_user");

  const modify = findObject(objects, "MODIFY_ITAB");
  assert(modify && modify.extras && modify.extras.modifyItab, "Expected MODIFY_ITAB extras.");
  assert.deepStrictEqual(
    modify.extras.modifyItab.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "uname",
        rightOperand: "p_user",
        comparisonOperator: "=",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(modify.extras.modifyItab.conditions[0].rightOperandRef, "p_user");
  assert(modify.extras.modifyItab.conditions[0].rightOperandDecl, "Expected decl for MODIFY condition right operand.");
  assert.strictEqual(modify.extras.modifyItab.conditions[0].rightOperandDecl.name, "p_user");

  const del = findObject(objects, "DELETE_ITAB");
  assert(del && del.extras && del.extras.deleteItab, "Expected DELETE_ITAB extras.");
  assert.deepStrictEqual(
    del.extras.deleteItab.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "active",
        rightOperand: "abap_false",
        comparisonOperator: "=",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(del.extras.deleteItab.conditions[0].rightOperandRef, "abap_false");
  assert(del.extras.deleteItab.conditions[0].rightOperandDecl, "Expected system decl for DELETE condition right operand.");
  assert.strictEqual(del.extras.deleteItab.conditions[0].rightOperandDecl.objectType, "SYSTEM");
}

function testConditionExtrasForIfElseifSelectAndPerform() {
  const code = [
    "DATA p_user TYPE syuname.",
    "DATA p_flag TYPE abap_bool.",
    "IF p_user = p_user AND p_flag = abap_true.",
    "ELSEIF p_user = p_user OR p_flag = abap_false.",
    "ENDIF.",
    "SELECT bname FROM usr02 WHERE bname = p_user GROUP BY bname HAVING bname = p_user ORDER BY bname.",
    "PERFORM main IF p_user = p_user AND p_flag = abap_true.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const ifObj = findObject(objects, "IF");
  assert(ifObj && ifObj.extras && ifObj.extras.ifCondition, "Expected IF condition extras.");
  assert.strictEqual(ifObj.extras.ifCondition.conditionRaw, "p_user = p_user AND p_flag = abap_true");
  assert.strictEqual(ifObj.extras.ifCondition.conditions.length, 2);
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].leftOperand, "p_user");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperand, "p_user");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].comparisonOperator, "=");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].logicalConnector, "AND");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandRef, "p_user");
  assert(ifObj.extras.ifCondition.conditions[0].rightOperandDecl, "Expected decl for IF right operand.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandDecl.name, "p_user");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].leftOperand, "p_flag");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperand, "abap_true");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].logicalConnector, "");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandRef, "abap_true");
  assert(ifObj.extras.ifCondition.conditions[1].rightOperandDecl, "Expected system decl for IF right operand.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandDecl.objectType, "SYSTEM");

  const ifValue = getValueEntry(ifObj.values, "condition");
  assert(ifValue, "Expected IF values.condition.");
  assert.strictEqual(ifValue.value, "p_user = p_user AND p_flag = abap_true");
  assert.strictEqual(ifValue.declRef, "p_user");

  const elseifObj = findObject(objects, "ELSEIF");
  assert(elseifObj && elseifObj.extras && elseifObj.extras.ifCondition, "Expected ELSEIF condition extras.");
  assert.strictEqual(elseifObj.extras.ifCondition.conditionRaw, "p_user = p_user OR p_flag = abap_false");
  assert.strictEqual(elseifObj.extras.ifCondition.conditions.length, 2);
  assert.strictEqual(elseifObj.extras.ifCondition.conditions[0].logicalConnector, "OR");
  assert.strictEqual(elseifObj.extras.ifCondition.conditions[1].rightOperandRef, "abap_false");
  assert(elseifObj.extras.ifCondition.conditions[1].rightOperandDecl, "Expected system decl for ELSEIF right operand.");
  assert.strictEqual(elseifObj.extras.ifCondition.conditions[1].rightOperandDecl.objectType, "SYSTEM");

  const selectObj = findObject(objects, "SELECT");
  assert(selectObj && selectObj.extras && selectObj.extras.select, "Expected SELECT condition extras.");
  assert.strictEqual(selectObj.extras.select.whereRaw, "bname = p_user");
  assert.strictEqual(selectObj.extras.select.whereConditions.length, 1);
  assert.strictEqual(selectObj.extras.select.whereConditions[0].leftOperand, "bname");
  assert.strictEqual(selectObj.extras.select.whereConditions[0].rightOperand, "p_user");
  assert.strictEqual(selectObj.extras.select.whereConditions[0].rightOperandRef, "p_user");
  assert(selectObj.extras.select.whereConditions[0].rightOperandDecl, "Expected decl for SELECT WHERE right operand.");
  assert.strictEqual(selectObj.extras.select.whereConditions[0].rightOperandDecl.name, "p_user");
  assert.strictEqual(selectObj.extras.select.havingRaw, "bname = p_user");
  assert.strictEqual(selectObj.extras.select.havingConditions.length, 1);
  assert.strictEqual(selectObj.extras.select.havingConditions[0].rightOperandRef, "p_user");
  assert(selectObj.extras.select.havingConditions[0].rightOperandDecl, "Expected decl for SELECT HAVING right operand.");
  assert.strictEqual(selectObj.extras.select.havingConditions[0].rightOperandDecl.name, "p_user");

  const performObj = findObject(objects, "PERFORM");
  assert(performObj && performObj.extras && performObj.extras.performCall, "Expected PERFORM extras.");
  assert.strictEqual(performObj.extras.performCall.ifCondition, "p_user = p_user AND p_flag = abap_true");
  assert.strictEqual(performObj.extras.performCall.ifConditions.length, 2);
  assert.strictEqual(performObj.extras.performCall.ifConditions[0].rightOperandRef, "p_user");
  assert(performObj.extras.performCall.ifConditions[0].rightOperandDecl, "Expected decl for PERFORM IF right operand.");
  assert.strictEqual(performObj.extras.performCall.ifConditions[0].rightOperandDecl.name, "p_user");
  assert.strictEqual(performObj.extras.performCall.ifConditions[1].rightOperandRef, "abap_true");
  assert(performObj.extras.performCall.ifConditions[1].rightOperandDecl, "Expected system decl for PERFORM IF right operand.");
  assert.strictEqual(performObj.extras.performCall.ifConditions[1].rightOperandDecl.objectType, "SYSTEM");
}

function testConditionOperatorMatrix() {
  const code = [
    "DATA lv_a TYPE i.",
    "DATA lv_b TYPE i.",
    "DATA lv_low TYPE i.",
    "DATA lv_high TYPE i.",
    "DATA lv_text TYPE string.",
    "DATA lv_mask TYPE string.",
    "DATA lt_allowed TYPE RANGE OF i.",
    "IF lv_a NE lv_b AND lv_a BT lv_low AND lv_high AND lv_text CP lv_mask AND lv_a IN lt_allowed.",
    "ENDIF.",
    "READ TABLE lt_data WITH KEY col_a NE lv_b col_b BT lv_low AND lv_high col_c CP lv_mask col_d IN lt_allowed INTO ls_data.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const ifObj = findObject(objects, "IF");
  assert(ifObj && ifObj.extras && ifObj.extras.ifCondition, "Expected IF condition extras for operator matrix.");
  assert.deepStrictEqual(
    ifObj.extras.ifCondition.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "lv_a",
        rightOperand: "lv_b",
        comparisonOperator: "NE",
        logicalConnector: "AND"
      },
      {
        leftOperand: "lv_a",
        rightOperand: "lv_low AND lv_high",
        comparisonOperator: "BT",
        logicalConnector: "AND"
      },
      {
        leftOperand: "lv_text",
        rightOperand: "lv_mask",
        comparisonOperator: "CP",
        logicalConnector: "AND"
      },
      {
        leftOperand: "lv_a",
        rightOperand: "lt_allowed",
        comparisonOperator: "IN",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandRef, "lv_b");
  assert(ifObj.extras.ifCondition.conditions[0].rightOperandDecl, "Expected decl for IF NE right operand.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandDecl.name, "lv_b");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandRef, "lv_low");
  assert(ifObj.extras.ifCondition.conditions[1].rightOperandDecl, "Expected decl for IF BT right operand.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandDecl.name, "lv_low");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[2].rightOperandRef, "lv_mask");
  assert(ifObj.extras.ifCondition.conditions[2].rightOperandDecl, "Expected decl for IF CP right operand.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[2].rightOperandDecl.name, "lv_mask");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[3].rightOperandRef, "lt_allowed");
  assert(ifObj.extras.ifCondition.conditions[3].rightOperandDecl, "Expected decl for IF IN right operand.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[3].rightOperandDecl.name, "lt_allowed");

  const readObj = findObject(objects, "READ_TABLE");
  assert(readObj && readObj.extras && readObj.extras.readTable, "Expected READ_TABLE condition extras for operator matrix.");
  assert.deepStrictEqual(
    readObj.extras.readTable.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "col_a",
        rightOperand: "lv_b",
        comparisonOperator: "NE",
        logicalConnector: "AND"
      },
      {
        leftOperand: "col_b",
        rightOperand: "lv_low AND lv_high",
        comparisonOperator: "BT",
        logicalConnector: "AND"
      },
      {
        leftOperand: "col_c",
        rightOperand: "lv_mask",
        comparisonOperator: "CP",
        logicalConnector: "AND"
      },
      {
        leftOperand: "col_d",
        rightOperand: "lt_allowed",
        comparisonOperator: "IN",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperandRef, "lv_b");
  assert(readObj.extras.readTable.conditions[0].rightOperandDecl, "Expected decl for READ NE right operand.");
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperandDecl.name, "lv_b");
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperandRef, "lv_low");
  assert(readObj.extras.readTable.conditions[1].rightOperandDecl, "Expected decl for READ BT right operand.");
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperandDecl.name, "lv_low");
  assert.strictEqual(readObj.extras.readTable.conditions[2].rightOperandRef, "lv_mask");
  assert(readObj.extras.readTable.conditions[2].rightOperandDecl, "Expected decl for READ CP right operand.");
  assert.strictEqual(readObj.extras.readTable.conditions[2].rightOperandDecl.name, "lv_mask");
  assert.strictEqual(readObj.extras.readTable.conditions[3].rightOperandRef, "lt_allowed");
  assert(readObj.extras.readTable.conditions[3].rightOperandDecl, "Expected decl for READ IN right operand.");
  assert.strictEqual(readObj.extras.readTable.conditions[3].rightOperandDecl.name, "lt_allowed");
}

function testIsInitialAndIsNotInitialConditions() {
  const code = [
    "DATA lv_a TYPE string.",
    "DATA lv_b TYPE string.",
    "IF lv_a IS INITIAL OR lv_b IS NOT INITIAL.",
    "ENDIF.",
    "READ TABLE lt_data WITH KEY col_a IS INITIAL col_b IS NOT INITIAL INTO ls_data.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const ifObj = findObject(objects, "IF");
  assert(ifObj && ifObj.extras && ifObj.extras.ifCondition, "Expected IF extras for IS INITIAL patterns.");
  assert.deepStrictEqual(
    ifObj.extras.ifCondition.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "lv_a",
        rightOperand: "INITIAL",
        comparisonOperator: "IS",
        logicalConnector: "OR"
      },
      {
        leftOperand: "lv_b",
        rightOperand: "NOT INITIAL",
        comparisonOperator: "IS",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].leftOperandRef, "lv_a");
  assert(ifObj.extras.ifCondition.conditions[0].leftOperandDecl, "Expected decl for lv_a.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].leftOperandDecl.name, "lv_a");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandRef, undefined);
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandRef, undefined);
  assert(ifObj.extras.ifCondition.conditions[0].rightOperandDecl, "Expected synthetic decl for INITIAL.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandDecl.objectType, "SYSTEM");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandDecl.name, "INITIAL");
  assert(ifObj.extras.ifCondition.conditions[1].rightOperandDecl, "Expected synthetic decl for NOT INITIAL.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandDecl.objectType, "SYSTEM");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperandDecl.name, "NOT INITIAL");

  const readObj = findObject(objects, "READ_TABLE");
  assert(readObj && readObj.extras && readObj.extras.readTable, "Expected READ_TABLE extras for IS INITIAL patterns.");
  assert.deepStrictEqual(
    readObj.extras.readTable.conditions.map((cond) => ({
      leftOperand: cond.leftOperand,
      rightOperand: cond.rightOperand,
      comparisonOperator: cond.comparisonOperator,
      logicalConnector: cond.logicalConnector
    })),
    [
      {
        leftOperand: "col_a",
        rightOperand: "INITIAL",
        comparisonOperator: "IS",
        logicalConnector: "AND"
      },
      {
        leftOperand: "col_b",
        rightOperand: "NOT INITIAL",
        comparisonOperator: "IS",
        logicalConnector: ""
      }
    ]
  );
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperandRef, undefined);
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperandRef, undefined);
  assert(readObj.extras.readTable.conditions[0].rightOperandDecl, "Expected synthetic decl for READ INITIAL.");
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperandDecl.name, "INITIAL");
  assert(readObj.extras.readTable.conditions[1].rightOperandDecl, "Expected synthetic decl for READ NOT INITIAL.");
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperandDecl.name, "NOT INITIAL");
}

function testImplicitSplitOnlyForReadTable() {
  const code = [
    "DATA gv_cnt TYPE i.",
    "PARAMETERS p_flag TYPE abap_bool.",
    "DATA lv_other TYPE abap_bool.",
    "IF gv_cnt > 0 AND p_flag = abap_true lv_other = abap_false.",
    "ENDIF.",
    "READ TABLE lt_data WITH KEY col_a = gv_cnt col_b = p_flag col_c = lv_other INTO ls_data.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const ifObj = findObject(objects, "IF");
  assert(ifObj && ifObj.extras && ifObj.extras.ifCondition, "Expected IF extras for implicit split test.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions.length, 2);
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].leftOperand, "gv_cnt");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperand, "0");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].leftOperand, "p_flag");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].comparisonOperator, "=");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[1].rightOperand, "abap_true lv_other = abap_false");

  const readObj = findObject(objects, "READ_TABLE");
  assert(readObj && readObj.extras && readObj.extras.readTable, "Expected READ_TABLE extras for implicit split test.");
  assert.strictEqual(readObj.extras.readTable.conditions.length, 3);
  assert.strictEqual(readObj.extras.readTable.conditions[0].leftOperand, "col_a");
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperand, "gv_cnt");
  assert.strictEqual(readObj.extras.readTable.conditions[1].leftOperand, "col_b");
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperand, "p_flag");
  assert.strictEqual(readObj.extras.readTable.conditions[2].leftOperand, "col_c");
  assert.strictEqual(readObj.extras.readTable.conditions[2].rightOperand, "lv_other");
}

function testConditionOperandsAlwaysHaveDecl() {
  const code = [
    "DATA gv_cnt TYPE i.",
    "IF gv_cnt > 0.",
    "ENDIF.",
    "READ TABLE lt_data WITH KEY col_a = 0 col_b = 'X' INTO ls_data.",
    ""
  ].join("\n");

  const result = parse(code);
  const objects = flattenObjects(result.objects);

  const ifObj = findObject(objects, "IF");
  assert(ifObj && ifObj.extras && ifObj.extras.ifCondition, "Expected IF extras for literal operand test.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions.length, 1);
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].leftOperandRef, "gv_cnt");
  assert(ifObj.extras.ifCondition.conditions[0].leftOperandDecl, "Expected left decl for IF gv_cnt.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].leftOperandDecl.name, "gv_cnt");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandRef, undefined);
  assert(ifObj.extras.ifCondition.conditions[0].rightOperandDecl, "Expected synthetic right decl for IF literal.");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandDecl.objectType, "CONDITION_VALUE");
  assert.strictEqual(ifObj.extras.ifCondition.conditions[0].rightOperandDecl.name, "0");

  const readObj = findObject(objects, "READ_TABLE");
  assert(readObj && readObj.extras && readObj.extras.readTable, "Expected READ_TABLE extras for literal operand test.");
  assert.strictEqual(readObj.extras.readTable.conditions.length, 2);
  assert.strictEqual(readObj.extras.readTable.conditions[0].leftOperandRef, "col_a");
  assert(readObj.extras.readTable.conditions[0].leftOperandDecl, "Expected synthetic left decl for READ field col_a.");
  assert.strictEqual(readObj.extras.readTable.conditions[0].leftOperandDecl.objectType, "CONDITION_VALUE");
  assert.strictEqual(readObj.extras.readTable.conditions[0].leftOperandDecl.name, "col_a");
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperandRef, undefined);
  assert(readObj.extras.readTable.conditions[0].rightOperandDecl, "Expected synthetic right decl for READ literal 0.");
  assert.strictEqual(readObj.extras.readTable.conditions[0].rightOperandDecl.name, "0");
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperandRef, undefined);
  assert(readObj.extras.readTable.conditions[1].rightOperandDecl, "Expected synthetic right decl for READ literal 'X'.");
  assert.strictEqual(readObj.extras.readTable.conditions[1].rightOperandDecl.name, "'X'");
}

function testMessageStatementModel() {
  const code = [
    "DATA lv_message TYPE string.",
    "DATA lv_id TYPE string.",
    "DATA lv_type TYPE c.",
    "DATA lv_number TYPE string.",
    "DATA lv_first TYPE string.",
    "DATA lv_second TYPE string.",
    "DATA lv_like TYPE c.",
    "DATA lv_target TYPE string.",
    "MESSAGE 'Saved' TYPE 'S'.",
    "MESSAGE lv_message TYPE lv_type WITH lv_first 'fixed' lv_second DISPLAY LIKE lv_like INTO lv_target RAISING static_error.",
    "MESSAGE e001(zclass) WITH lv_first.",
    "MESSAGE ID lv_id TYPE lv_type NUMBER lv_number WITH lv_first lv_second.",
    "MESSAGE e001.",
    ""
  ].join("\n");

  const messages = findObjects(flattenObjects(parse(code).objects), "MESSAGE");
  assert.strictEqual(messages.length, 5, "Expected all practical MESSAGE forms to parse.");

  const direct = messages[0];
  assert.strictEqual(getValue(direct.values, "message"), "'Saved'");
  assert.strictEqual(getValue(direct.values, "messageType"), "'S'");
  assert.strictEqual(direct.extras.message.mode, "direct");
  assert.strictEqual(direct.extras.message.messageRef, undefined, "Literal message text must not get a fake ref.");
  assert.strictEqual(direct.extras.message.messageDecl, undefined, "Literal message text must not get a fake decl.");
  assert.strictEqual(direct.extras.message.messageTypeRef, undefined, "Literal message type must stay static.");

  const dynamic = messages[1];
  const dynamicInfo = dynamic.extras.message;
  assert.strictEqual(dynamicInfo.mode, "direct");
  assert.strictEqual(dynamicInfo.message, "lv_message");
  assert.strictEqual(dynamicInfo.messageRef, "lv_message");
  assert.strictEqual(dynamicInfo.messageDecl.name, "lv_message");
  assert.strictEqual(dynamicInfo.messageTypeRef, "lv_type");
  assert.strictEqual(dynamicInfo.messageTypeDecl.name, "lv_type");
  assert.strictEqual(getValue(dynamic.values, "withRaw"), "lv_first 'fixed' lv_second");
  assert.deepStrictEqual(dynamicInfo.with.map((entry) => entry.value), ["lv_first", "'fixed'", "lv_second"]);
  assert.strictEqual(dynamicInfo.with[0].valueRef, "lv_first");
  assert.strictEqual(dynamicInfo.with[0].valueDecl.name, "lv_first");
  assert.strictEqual(dynamicInfo.with[1].valueRef, undefined, "Literal WITH operand must not get a fake ref.");
  assert.strictEqual(dynamicInfo.with[1].valueDecl, undefined, "Literal WITH operand must not get a fake decl.");
  assert.strictEqual(dynamicInfo.displayLikeRef, "lv_like");
  assert.strictEqual(dynamicInfo.displayLikeDecl.name, "lv_like");
  assert.strictEqual(dynamicInfo.intoRef, "lv_target");
  assert.strictEqual(dynamicInfo.intoDecl.name, "lv_target");
  assert.strictEqual(dynamicInfo.raising, "static_error");
  assert.strictEqual(dynamicInfo.raisingRef, undefined, "RAISING exception is a static name, not a data operand.");
  assert.strictEqual(dynamicInfo.raisingDecl, undefined, "RAISING exception must remain declaration-free.");

  const shorthand = messages[2];
  const shorthandInfo = shorthand.extras.message;
  assert.strictEqual(shorthandInfo.mode, "shorthand");
  assert.strictEqual(getValue(shorthand.values, "message"), "e001(zclass)");
  assert.strictEqual(getValue(shorthand.values, "id"), "zclass");
  assert.strictEqual(getValue(shorthand.values, "messageType"), "E");
  assert.strictEqual(getValue(shorthand.values, "number"), "001");
  for (const key of ["messageRef", "messageDecl", "idRef", "idDecl", "messageTypeRef", "messageTypeDecl", "numberRef", "numberDecl"]) {
    assert.strictEqual(shorthandInfo[key], undefined, `Shorthand static field ${key} must stay unset.`);
  }
  assert.strictEqual(shorthandInfo.with[0].valueDecl.name, "lv_first");

  const explicit = messages[3];
  const explicitInfo = explicit.extras.message;
  assert.strictEqual(explicitInfo.mode, "id-type-number");
  assert.strictEqual(getValue(explicit.values, "message"), "");
  assert.strictEqual(getValue(explicit.values, "id"), "lv_id");
  assert.strictEqual(getValue(explicit.values, "messageType"), "lv_type");
  assert.strictEqual(getValue(explicit.values, "number"), "lv_number");
  assert.strictEqual(explicitInfo.idRef, "lv_id");
  assert.strictEqual(explicitInfo.idDecl.name, "lv_id");
  assert.strictEqual(explicitInfo.messageTypeRef, "lv_type");
  assert.strictEqual(explicitInfo.numberRef, "lv_number");
  assert.strictEqual(explicitInfo.numberDecl.name, "lv_number");
  assert.deepStrictEqual(explicitInfo.with.map((entry) => entry.value), ["lv_first", "lv_second"]);

  const reference = messages[4];
  const referenceInfo = reference.extras.message;
  assert.strictEqual(referenceInfo.mode, "reference");
  assert.strictEqual(getValue(reference.values, "message"), "e001");
  assert.strictEqual(getValue(reference.values, "messageType"), "E");
  assert.strictEqual(getValue(reference.values, "number"), "001");
  for (const key of ["messageRef", "messageDecl", "messageTypeRef", "messageTypeDecl", "numberRef", "numberDecl"]) {
    assert.strictEqual(referenceInfo[key], undefined, `Static MESSAGE reference ${key} must stay unset.`);
  }
}

function testWriteNumericPositionsAndBacktickChains() {
  const code = [
    "DATA lv_output TYPE string.",
    "DATA lv_destination TYPE string.",
    "DATA lv_pos TYPE string.",
    "WRITE 5(10) lv_output.",
    "WRITE: 20 lv_output,",
    "       30(4) lv_destination.",
    "WRITE: `A,B`, |C,D|, 'E,F'.",
    "WRITE lv_pos lv_output.",
    ""
  ].join("\n");
  const writes = findObjects(flattenObjects(parse(code).objects), "WRITE");
  assert.strictEqual(writes.length, 7, "Backtick/template/quoted commas must stay inside three chained WRITE items.");

  assert.strictEqual(getValue(writes[0].values, "position"), "5(10)");
  assert.strictEqual(writes[0].extras.write.position.column, "5");
  assert.strictEqual(writes[0].extras.write.position.length, "10");
  assert.strictEqual(writes[0].extras.write.output, "lv_output");
  assert.strictEqual(writes[0].extras.write.outputDecl.name, "lv_output");

  assert.strictEqual(writes[1].raw, "WRITE 20 lv_output.");
  assert.strictEqual(writes[1].extras.write.position.column, "20");
  assert.strictEqual(writes[1].extras.write.position.length, "");
  assert.strictEqual(writes[1].extras.write.output, "lv_output");
  assert.strictEqual(writes[2].extras.write.position.column, "30");
  assert.strictEqual(writes[2].extras.write.position.length, "4");
  assert.strictEqual(writes[2].extras.write.output, "lv_destination");

  assert.deepStrictEqual(
    writes.slice(3, 6).map((obj) => obj.extras.write.output),
    ["`A,B`", "|C,D|", "'E,F'"],
    "All three literal families must preserve embedded commas during chained splitting."
  );
  for (const literalWrite of writes.slice(3, 6)) {
    assert.strictEqual(literalWrite.extras.write.outputRef, undefined);
    assert.strictEqual(literalWrite.extras.write.outputDecl, undefined);
  }

  assert.strictEqual(writes[6].extras.write.output, "lv_pos", "An arbitrary leading identifier must remain the output operand.");
  assert.deepStrictEqual(writes[6].extras.write.position, { column: "", length: "" });
}

function testWriteStatementModel() {
  const code = [
    "DATA lv_output TYPE string.",
    "DATA lv_destination TYPE string.",
    "DATA lv_column TYPE i.",
    "DATA lv_length TYPE i.",
    "DATA lv_currency TYPE string.",
    "DATA lv_decimals TYPE i.",
    "DATA lv_unit TYPE string.",
    "DATA lv_mask TYPE string.",
    "WRITE lv_output.",
    "WRITE 'literal'.",
    "WRITE: / lv_output, \"first item",
    "* positioned item",
    "       AT /lv_column(lv_length) |Value { lv_output }| TO lv_destination NO-GAP CURRENCY lv_currency DECIMALS lv_decimals USING EDIT MASK lv_mask,",
    "       / 'fixed' UNDER lv_output NO-ZERO.",
    "WRITE AT /5(10) lv_output TO lv_destination LEFT-JUSTIFIED RIGHT-JUSTIFIED CENTERED NO-SIGN NO-GROUPING EXPONENT 2 ROUND 3 UNIT lv_unit.",
    "WRITE AT / lv_output.",
    ""
  ].join("\n");

  const writes = findObjects(flattenObjects(parse(code).objects), "WRITE");
  assert.strictEqual(writes.length, 7, "Expected simple, literal, chained, and positioned WRITE forms.");

  const simple = writes[0];
  assert.strictEqual(getValue(simple.values, "output"), "lv_output");
  assert.strictEqual(simple.extras.write.outputRef, "lv_output");
  assert.strictEqual(simple.extras.write.outputDecl.name, "lv_output");
  assert.strictEqual(simple.extras.write.newLine, false);

  const literal = writes[1];
  assert.strictEqual(getValue(literal.values, "output"), "'literal'");
  assert.strictEqual(literal.extras.write.outputRef, undefined);
  assert.strictEqual(literal.extras.write.outputDecl, undefined, "Literal WRITE output must not get a fake decl.");

  const firstChain = writes[2];
  assert.strictEqual(firstChain.raw, "WRITE / lv_output.");
  assert.strictEqual(firstChain.comment, "first item");
  assert.strictEqual(firstChain.lineStart, 11);
  assert.strictEqual(firstChain.segmentIndex, 0, "Chained WRITE node must retain its source statement segment.");
  assert.strictEqual(firstChain.extras.write.newLine, true);

  const positioned = writes[3];
  const positionedInfo = positioned.extras.write;
  assert.strictEqual(positioned.comment, "positioned item");
  assert.strictEqual(positioned.lineStart, 13);
  assert.strictEqual(positioned.segmentIndex, 0);
  assert.strictEqual(positionedInfo.newLine, true);
  assert.strictEqual(positionedInfo.position.column, "lv_column");
  assert.strictEqual(positionedInfo.position.length, "lv_length");
  assert.strictEqual(positionedInfo.position.columnRef, "lv_column");
  assert.strictEqual(positionedInfo.position.columnDecl.name, "lv_column");
  assert.strictEqual(positionedInfo.position.lengthRef, "lv_length");
  assert.strictEqual(positionedInfo.position.lengthDecl.name, "lv_length");
  assert.strictEqual(getValue(positioned.values, "position"), "/lv_column(lv_length)");
  assert.strictEqual(getValue(positioned.values, "output"), "|Value { lv_output }|");
  assert.strictEqual(positionedInfo.outputRef, undefined, "String template output must stay literal for declaration binding.");
  assert.strictEqual(positionedInfo.destinationRef, "lv_destination");
  assert.strictEqual(positionedInfo.destinationDecl.name, "lv_destination");
  assert.strictEqual(getValue(positioned.values, "destination"), "lv_destination");
  assert.strictEqual(getValue(positioned.values, "formatRaw"), "NO-GAP CURRENCY lv_currency DECIMALS lv_decimals USING EDIT MASK lv_mask");
  assert.deepStrictEqual(
    positionedInfo.format.map((entry) => ({ keyword: entry.keyword, value: entry.value })),
    [
      { keyword: "NO-GAP", value: "" },
      { keyword: "CURRENCY", value: "lv_currency" },
      { keyword: "DECIMALS", value: "lv_decimals" },
      { keyword: "USING EDIT MASK", value: "lv_mask" }
    ]
  );
  assert.strictEqual(positionedInfo.format[0].valueRef, undefined);
  assert.strictEqual(positionedInfo.format[1].valueRef, "lv_currency");
  assert.strictEqual(positionedInfo.format[1].valueDecl.name, "lv_currency");
  assert.strictEqual(positionedInfo.format[2].valueDecl.name, "lv_decimals");
  assert.strictEqual(positionedInfo.format[3].valueDecl.name, "lv_mask");

  const chainedLiteral = writes[4];
  assert.strictEqual(chainedLiteral.extras.write.newLine, true);
  assert.strictEqual(chainedLiteral.extras.write.output, "'fixed'");
  assert.strictEqual(chainedLiteral.extras.write.outputDecl, undefined);
  assert.strictEqual(chainedLiteral.extras.write.format[0].keyword, "UNDER");
  assert.strictEqual(chainedLiteral.extras.write.format[0].valueDecl.name, "lv_output");
  assert.strictEqual(chainedLiteral.extras.write.format[1].keyword, "NO-ZERO");

  const numericPosition = writes[5];
  const numericInfo = numericPosition.extras.write;
  assert.strictEqual(numericInfo.newLine, true);
  assert.deepStrictEqual(
    { column: numericInfo.position.column, length: numericInfo.position.length },
    { column: "5", length: "10" }
  );
  assert.strictEqual(numericInfo.position.columnRef, undefined);
  assert.strictEqual(numericInfo.position.columnDecl, undefined);
  assert.strictEqual(numericInfo.position.lengthRef, undefined);
  assert.strictEqual(numericInfo.position.lengthDecl, undefined);
  assert.deepStrictEqual(
    numericInfo.format.map((entry) => entry.keyword),
    ["LEFT-JUSTIFIED", "RIGHT-JUSTIFIED", "CENTERED", "NO-SIGN", "NO-GROUPING", "EXPONENT", "ROUND", "UNIT"]
  );
  assert.strictEqual(numericInfo.format.find((entry) => entry.keyword === "EXPONENT").valueDecl, undefined);
  assert.strictEqual(numericInfo.format.find((entry) => entry.keyword === "ROUND").valueDecl, undefined);
  assert.strictEqual(numericInfo.format.find((entry) => entry.keyword === "UNIT").valueDecl.name, "lv_unit");

  const atNewLineOnly = writes[6].extras.write;
  assert.strictEqual(atNewLineOnly.newLine, true);
  assert.deepStrictEqual(atNewLineOnly.position, { column: "", length: "" });
  assert.strictEqual(atNewLineOnly.output, "lv_output", "AT / without position must retain the output operand.");
  assert.strictEqual(atNewLineOnly.outputDecl.name, "lv_output");
}

function run() {
  testMultipleStatementsOnSingleLine();
  testSingleLineTrailingCommentAppliesToLastStatementOnly();
  testDecimalLiteralDoesNotSplitStatement();
  testChainedDataStatementSingleLine();
  testChainedDataStatementAcrossLines();
  testChainedDataStatementKeepsCommaInsideTemplateLiteral();
  testChainedConstantsKeepItemCommentsWithoutHeaderLeak();
  testChainedConstantsUseSingleInternalCommentForNextItem();
  testConstantsCaptureCompleteInitializer();
  testGenericChainedStatementUsesPerItemComment();
  testChainedCommentsRejectBlocksGapsAndUnfinishedItems();
  testChainedStructCommentsUseSegmentMetadata();
  testNonChainedStructKeepsSingleLeadingComment();
  testNonChainedStructKeepsLeadingCommentBlock();
  testBacktickLiteralKeepsStatementAndInlineComment();
  testInlineCommentInsideSingleQuote();
  testInlineCommentInsideTemplate();
  testEscapedSingleQuoteTokenization();
  testAssignmentKeepsFullExpression();
  testInlineDataReferenceInAssignment();
  testCallMethodExpressionWithAssignmentReceiver();
  testCallMethodExpressionStandalone();
  testStatementCommentPrefersFirstInline();
  testStatementCommentFallsBackToSingleLeadingLine();
  testStatementCommentIgnoresLeadingCommentBlock();
  testStatementCommentIgnoresLeadingCommentWithBlankGap();
  testReadTableWithKeyConditions();
  testReadTableWithTableKeyConditions();
  testWhereConditionExtrasForSimilarStatements();
  testConditionExtrasForIfElseifSelectAndPerform();
  testConditionOperatorMatrix();
  testIsInitialAndIsNotInitialConditions();
  testImplicitSplitOnlyForReadTable();
  testConditionOperandsAlwaysHaveDecl();
  testMessageStatementModel();
  testWriteStatementModel();
  testWriteNumericPositionsAndBacktickChains();
  testSupportedStatementSmokeMatrix();
  console.log("parser-regression: ok");
}

run();
