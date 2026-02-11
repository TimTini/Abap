"use strict";

const assert = require("assert");
const path = require("path");
const { parseAbapText } = require("../shared/abap-parser");
const { loadConfigs } = require("../cli/config-loader");

const configs = loadConfigs(path.resolve(__dirname, "..", "configs"));

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
  const code = "READ TABLE lt_user WITH KEY uname = p_user active = abap_true INTO ls_user.\n";
  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const readTable = findObject(objects, "READ_TABLE");
  assert(readTable && readTable.extras && readTable.extras.readTable, "Expected READ_TABLE extras.");

  const conditions = Array.isArray(readTable.extras.readTable.conditions) ? readTable.extras.readTable.conditions : [];
  assert.strictEqual(conditions.length, 2);
  assert.deepStrictEqual(conditions[0], {
    leftOperand: "uname",
    rightOperand: "p_user",
    comparisonOperator: "=",
    logicalConnector: "AND"
  });
  assert.deepStrictEqual(conditions[1], {
    leftOperand: "active",
    rightOperand: "abap_true",
    comparisonOperator: "=",
    logicalConnector: ""
  });
}

function testReadTableWithTableKeyConditions() {
  const code = "READ TABLE lt_user WITH TABLE KEY primary_key COMPONENTS uname = p_user OR active = abap_true INTO ls_user.\n";
  const result = parse(code);
  const objects = flattenObjects(result.objects);
  const readTable = findObject(objects, "READ_TABLE");
  assert(readTable && readTable.extras && readTable.extras.readTable, "Expected READ_TABLE extras.");

  const extras = readTable.extras.readTable;
  assert.strictEqual(extras.withTableKeyRaw, "primary_key COMPONENTS uname = p_user OR active = abap_true");

  const conditions = Array.isArray(extras.conditions) ? extras.conditions : [];
  assert.strictEqual(conditions.length, 2);
  assert.deepStrictEqual(conditions[0], {
    leftOperand: "uname",
    rightOperand: "p_user",
    comparisonOperator: "=",
    logicalConnector: "OR"
  });
  assert.deepStrictEqual(conditions[1], {
    leftOperand: "active",
    rightOperand: "abap_true",
    comparisonOperator: "=",
    logicalConnector: ""
  });
}

function testWhereConditionExtrasForSimilarStatements() {
  const code = [
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
  assert.deepStrictEqual(loopAt.extras.loopAtItab.conditions, [
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
  ]);

  const modify = findObject(objects, "MODIFY_ITAB");
  assert(modify && modify.extras && modify.extras.modifyItab, "Expected MODIFY_ITAB extras.");
  assert.deepStrictEqual(modify.extras.modifyItab.conditions, [
    {
      leftOperand: "uname",
      rightOperand: "p_user",
      comparisonOperator: "=",
      logicalConnector: ""
    }
  ]);

  const del = findObject(objects, "DELETE_ITAB");
  assert(del && del.extras && del.extras.deleteItab, "Expected DELETE_ITAB extras.");
  assert.deepStrictEqual(del.extras.deleteItab.conditions, [
    {
      leftOperand: "active",
      rightOperand: "abap_false",
      comparisonOperator: "=",
      logicalConnector: ""
    }
  ]);
}

function run() {
  testInlineCommentInsideSingleQuote();
  testInlineCommentInsideTemplate();
  testEscapedSingleQuoteTokenization();
  testAssignmentKeepsFullExpression();
  testInlineDataReferenceInAssignment();
  testStatementCommentPrefersFirstInline();
  testStatementCommentFallsBackToSingleLeadingLine();
  testStatementCommentIgnoresLeadingCommentBlock();
  testStatementCommentIgnoresLeadingCommentWithBlankGap();
  testReadTableWithKeyConditions();
  testReadTableWithTableKeyConditions();
  testWhereConditionExtrasForSimilarStatements();
  console.log("parser-regression: ok");
}

run();
