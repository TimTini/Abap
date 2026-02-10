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

function run() {
  testInlineCommentInsideSingleQuote();
  testInlineCommentInsideTemplate();
  testEscapedSingleQuoteTokenization();
  testAssignmentKeepsFullExpression();
  testInlineDataReferenceInAssignment();
  console.log("parser-regression: ok");
}

run();
