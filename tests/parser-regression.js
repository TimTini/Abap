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
