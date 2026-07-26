"use strict";

const assert = require("assert");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const {
  assertHasObjectTypes,
  configs,
  findObjects,
  flattenObjects,
  fs,
  parseAbapText,
  path
} = require("./helpers/parser-test-helpers");

function testDefaultFlightDemoContract() {
  const samplePath = path.resolve(__dirname, "..", "examples", "deep_form_demo.abap");
  const source = fs.readFileSync(samplePath, "utf8");
  const lineCount = source.split(/\r?\n/).length;
  assert(lineCount >= 1800 && lineCount <= 2000, `Extended default demo must stay within 1800-2000 lines, got ${lineCount}.`);
  assert.match(source, /^REPORT\s+zflight_operations_overview\b/im);
  for (const marker of [
    "Flight Operations Overview - extended ABAP statement coverage",
    "Classic Open SQL SELECT syntax",
    "OPEN CURSOR",
    "OPEN DATASET",
    "ROLLBACK WORK",
    "START-OF-SELECTION"
  ]) {
    assert(source.includes(marker), `Extended default demo must contain ${marker}.`);
  }

  const parsed = parseAbapText(source, configs, "deep_form_demo.abap");
  const objects = flattenObjects(parsed.objects);
  assertHasObjectTypes(objects, [
    "TYPES",
    "CONSTANTS",
    "DATA",
    "FIELD-SYMBOLS",
    "CLASS",
    "CLASS-METHODS",
    "METHODS",
    "METHOD",
    "ASSIGNMENT",
    "SELECT",
    "SELECT-OPTIONS",
    "PARAMETERS",
    "READ_TABLE",
    "LOOP_AT_ITAB",
    "IF",
    "ELSEIF",
    "ELSE",
    "CASE",
    "WHEN",
    "PERFORM",
    "FORM",
    "CALL_FUNCTION",
    "CALL_METHOD",
    "APPEND",
    "INSERT_ITAB",
    "MODIFY_ITAB",
    "DELETE_ITAB",
    "SORT_ITAB",
    "CLEAR",
    "TRY",
    "CATCH",
    "DO",
    "MESSAGE",
    "WRITE"
  ], "default flight demo");

  const validateCalls = findObjects(objects, "PERFORM").filter((obj) => (
    String(obj && obj.extras && obj.extras.performCall && obj.extras.performCall.form || "").toLowerCase()
      === "frm_validate_request"
  ));
  assert.strictEqual(validateCalls.length, 2, "Expected the main and preview validation sources.");
  const validationRoots = validateCalls.map((obj) => String(obj.extras.performCall.using[0] && obj.extras.performCall.using[0].value || "").toLowerCase());
  assert.deepStrictEqual(validationRoots, ["gs_request", "gs_preview_request"]);

  const nestedFormCalls = new Set(findObjects(objects, "PERFORM").map((obj) => (
    String(obj && obj.extras && obj.extras.performCall && obj.extras.performCall.form || "").toLowerCase()
  )));
  assert(nestedFormCalls.has("frm_load_flights_with_cursor"));
  assert(nestedFormCalls.has("frm_export_dataset"));
  assert(nestedFormCalls.has("frm_demo_database_dml_rollback"));

  const appendVariants = new Set(findObjects(objects, "APPEND").map((obj) => (
    String(obj && obj.extras && obj.extras.append && obj.extras.append.variant || "")
  )));
  assert(appendVariants.has("single"));
  assert(appendVariants.has("initialLine"));
  assert(appendVariants.has("linesOf"));
}

defineFocusedTest(test, "parser default demo regression", ["demo"], async (t) => {
  await t.test("default flight demo contract", () => {
    testDefaultFlightDemoContract();
  });
});
