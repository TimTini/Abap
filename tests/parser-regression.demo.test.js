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
  assert(lineCount >= 500 && lineCount <= 700, `Default demo must stay within 500-700 lines, got ${lineCount}.`);
  assert.match(source, /^REPORT\s+zflight_operations_overview\b/im);
  for (const marker of ["frm_chk_", "CHECK CASE", "v\u00ed d\u1ee5", "deep_chain"]) {
    assert(!source.toLowerCase().includes(marker.toLowerCase()), `Default demo must not contain artificial marker ${marker}.`);
  }

  const parsed = parseAbapText(source, configs, "deep_form_demo.abap");
  const objects = flattenObjects(parsed.objects);
  assertHasObjectTypes(objects, [
    "TYPES",
    "CONSTANTS",
    "DATA",
    "ASSIGNMENT",
    "SELECT",
    "READ_TABLE",
    "LOOP_AT_ITAB",
    "IF",
    "ELSEIF",
    "CASE",
    "PERFORM",
    "CALL_FUNCTION",
    "CALL_METHOD",
    "APPEND",
    "MODIFY_ITAB",
    "DELETE_ITAB",
    "SORT_ITAB",
    "MOVE-CORRESPONDING",
    "CLEAR",
    "TRY",
    "CATCH",
    "MESSAGE",
    "WRITE"
  ], "default flight demo");

  const validateCalls = findObjects(objects, "PERFORM").filter((obj) => (
    String(obj && obj.extras && obj.extras.performCall && obj.extras.performCall.form || "").toLowerCase()
      === "frm_validate_request"
  ));
  assert.strictEqual(validateCalls.length, 3, "Expected exactly three natural validation sources.");
  const validationRoots = validateCalls.map((obj) => String(obj.extras.performCall.using[0] && obj.extras.performCall.using[0].value || "").toLowerCase());
  assert.deepStrictEqual(validationRoots, ["gs_request", "gs_preview_request", "gs_request"]);

  const nestedFormCalls = new Set(findObjects(objects, "PERFORM").map((obj) => (
    String(obj && obj.extras && obj.extras.performCall && obj.extras.performCall.form || "").toLowerCase()
  )));
  assert(nestedFormCalls.has("frm_enrich_flight"));
  assert(nestedFormCalls.has("frm_calculate_availability"));
  assert(nestedFormCalls.has("frm_determine_status"));

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
