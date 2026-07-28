"use strict";

const path = require("path");
const { parseAbapText } = require("../shared/abap-parser.js");
const { loadConfigs } = require("../tests/helpers/config-loader.js");

const configs = loadConfigs(path.join(__dirname, "..", "configs"));

function fail(message) {
  console.error(`FAIL: ${message}`);
  process.exit(1);
}

function first(objects, type) {
  return (objects || []).find((obj) => obj && obj.objectType === type) || null;
}

function valueOf(obj, name) {
  const entries = obj && obj.values ? obj.values[name] : null;
  const entry = Array.isArray(entries) ? entries[0] : entries;
  return entry && entry.value !== undefined ? String(entry.value) : "";
}

const read = first(parseAbapText(
  "READ TABLE lt_tab WITH KEY id = lv_id BINARY SEARCH TRANSPORTING NO FIELDS.",
  configs,
  "check.abap"
).objects, "READ_TABLE");
if (!read) fail("READ_TABLE missing");
if (valueOf(read, "withKey") !== "id = lv_id") fail(`withKey=${valueOf(read, "withKey")}`);
if (valueOf(read, "binarySearch") !== "X") fail("binarySearch flag missing");
if (valueOf(read, "transportingNoFields") !== "X") fail("transportingNoFields flag missing");

const loop = first(parseAbapText(
  "LOOP AT lt_tab INTO ls_row FROM INDEX lv_from TO lv_to.\nENDLOOP.",
  configs,
  "check.abap"
).objects, "LOOP_AT_ITAB");
if (!loop) fail("LOOP_AT_ITAB missing");
if (valueOf(loop, "from") !== "lv_from") fail(`from=${valueOf(loop, "from")}`);
if (valueOf(loop, "to") !== "lv_to") fail(`to=${valueOf(loop, "to")}`);

const concat = first(parseAbapText(
  "CONCATENATE lv_a lv_b INTO lv_c SEPARATED BY ' - '.",
  configs,
  "check.abap"
).objects, "CONCATENATE");
if (!concat) fail("CONCATENATE missing");
if (valueOf(concat, "sources") !== "lv_a lv_b") fail(`sources=${valueOf(concat, "sources")}`);
if (valueOf(concat, "into") !== "lv_c") fail(`into=${valueOf(concat, "into")}`);
if (valueOf(concat, "separatedBy") !== "' - '") fail(`separatedBy=${valueOf(concat, "separatedBy")}`);

console.log("PASS: read-loop-concatenate smoke checks");
