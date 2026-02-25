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
  testConditionExtrasForIfElseifSelectAndPerform();
  testConditionOperatorMatrix();
  testIsInitialAndIsNotInitialConditions();
  testImplicitSplitOnlyForReadTable();
  console.log("parser-regression: ok");
}

run();
