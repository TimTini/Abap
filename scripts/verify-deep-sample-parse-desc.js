"use strict";

/**
 * Smoke + contract checks for examples/deep_form_demo.abap:
 * - parse object coverage
 * - SELECT modern (FIELDS) + classic (list before FROM)
 * - descriptions sourced from parse comments (viewer codeDesc path)
 */
const fs = require("fs");
const path = require("path");
const { parseAbapText } = require("../tests/helpers/source-parser");
const { loadConfigs } = require("../tests/helpers/config-loader");

const repoRoot = path.resolve(__dirname, "..");
const samplePath = path.join(repoRoot, "examples", "deep_form_demo.abap");
const configs = loadConfigs(path.join(repoRoot, "configs"));

function flatten(objects) {
  const out = [];
  const walk = (list) => {
    for (const obj of list || []) {
      if (!obj) continue;
      out.push(obj);
      if (Array.isArray(obj.children)) walk(obj.children);
    }
  };
  walk(objects);
  return out;
}

function getValue(values, key) {
  const entry = values && values[key];
  const first = Array.isArray(entry) ? entry[0] : entry;
  return first && first.value != null ? String(first.value) : "";
}

function whereClean(obj, label) {
  const where = getValue(obj.values, "where");
  if (/\bINTO\b/i.test(where) || /\bAPPENDING\b/i.test(where) || /\bUNION\b/i.test(where)) {
    fail(`${label}: where swallowed later clause: ${where}`);
  }
}

function fail(msg) {
  console.error("FAIL:", msg);
  process.exit(1);
}

function main() {
  const source = fs.readFileSync(samplePath, "utf8");
  const lineCount = source.split(/\r?\n/).length;
  console.log("sample lines:", lineCount);
  if (lineCount < 1800 || lineCount > 2000) {
    fail(`Expected sample ~1868 lines (1800-2000), got ${lineCount}`);
  }
  if (!/Classic Open SQL SELECT syntax/i.test(source)) {
    fail("Expected classic Open SQL SELECT section marker");
  }

  const parsed = parseAbapText(source, configs, "deep_form_demo.abap");
  const objects = flatten(parsed.objects);
  const decls = Array.isArray(parsed.decls) ? parsed.decls : [];

  const byType = {};
  for (const obj of objects) {
    const type = String(obj.objectType || "?");
    byType[type] = (byType[type] || 0) + 1;
  }
  console.log("object counts:", JSON.stringify(byType, null, 2));

  const requiredTypes = [
    "SELECT", "WRITE", "PERFORM", "FORM", "DATA", "TYPES", "IF", "LOOP_AT_ITAB",
    "READ_TABLE", "CALL_FUNCTION", "CALL_METHOD", "MESSAGE"
  ];
  for (const type of requiredTypes) {
    if (!byType[type]) fail(`Missing objectType ${type}`);
  }

  // Parse descriptions live on decl.comment; viewer maps that to codeDesc.
  const declsWithComment = decls.filter((d) => String(d && d.comment || "").trim());
  const objectsWithComment = objects.filter((o) => String(o && o.comment || "").trim());
  console.log("decls total:", decls.length);
  console.log("decls with comment (parse → viewer codeDesc):", declsWithComment.length);
  console.log("objects with statement comment:", objectsWithComment.length);
  if (decls.length < 50) fail("Expected many declarations in sample");
  if (declsWithComment.length < 100) {
    fail(`Expected rich comment-backed descriptions, got ${declsWithComment.length}`);
  }
  if (objectsWithComment.length < 50) {
    fail(`Expected many statement comments, got ${objectsWithComment.length}`);
  }

  const airlineField = declsWithComment.find((d) =>
    /Airline carrier identifier/i.test(String(d.comment || ""))
  );
  if (!airlineField) fail("Missing Airline carrier identifier comment on a decl");
  console.log("sample decl desc:", airlineField.name, "→", airlineField.comment);

  const namedConstant = decls.find((d) => String(d.name || "").toLowerCase() === "gc_status_open");
  if (!namedConstant) fail("Missing gc_status_open decl");
  // Leading CONSTANTS block comment may attach to first items depending on parser;
  // at least the decl must exist for Data panel catalog.
  console.log("constant decl present:", namedConstant.name, "comment=", namedConstant.comment || "(none)");

  const selects = objects.filter((o) => o.objectType === "SELECT");
  console.log("SELECT count:", selects.length);
  if (selects.length < 10) fail(`Expected many SELECTs including classic, got ${selects.length}`);

  const modernSingle = selects.find((o) =>
    /SELECT\s+SINGLE\s+FROM\s+scarr/i.test(String(o.raw || ""))
    && /FIELDS\s+carrname/i.test(String(o.raw || ""))
  );
  if (!modernSingle) fail("Missing modern SELECT SINGLE FROM … FIELDS");
  if (getValue(modernSingle.values, "fields") !== "carrname") {
    fail(`Modern SINGLE fields expected carrname, got ${getValue(modernSingle.values, "fields")}`);
  }
  whereClean(modernSingle, "modernSingle");
  if (getValue(modernSingle.values, "into") !== "@gv_default_carrier_name") {
    fail(`Modern SINGLE into expected @gv_default_carrier_name, got ${getValue(modernSingle.values, "into")}`);
  }

  // Classic samples put INTO before FROM (no FIELDS keyword).
  const classicSingleStar = selects.find((o) =>
    /SELECT\s+SINGLE\s+\*/i.test(String(o.raw || ""))
    && /ls_classic_carrier/i.test(String(o.raw || ""))
    && !/FIELDS/i.test(String(o.raw || ""))
  );
  if (!classicSingleStar) fail("Missing classic SELECT SINGLE * INTO … FROM scarr");
  if (!String(getValue(classicSingleStar.values, "fields") || "").includes("*")) {
    fail(`Classic SINGLE * fields expected *, got ${getValue(classicSingleStar.values, "fields")}`);
  }
  if (getValue(classicSingleStar.values, "into") !== "ls_classic_carrier") {
    fail(`Classic SINGLE into expected ls_classic_carrier, got ${getValue(classicSingleStar.values, "into")}`);
  }
  whereClean(classicSingleStar, "classicSingleStar");

  const classicStarTable = selects.find((o) =>
    /SELECT\s+\*/i.test(String(o.raw || ""))
    && /lt_classic_carriers/i.test(String(o.raw || ""))
    && !/SINGLE/i.test(String(o.raw || ""))
    && !/FIELDS/i.test(String(o.raw || ""))
  );
  if (!classicStarTable) fail("Missing classic SELECT * INTO TABLE lt_classic_carriers");
  if (!String(getValue(classicStarTable.values, "fields") || "").includes("*")) {
    fail(`Classic * table fields expected *, got ${getValue(classicStarTable.values, "fields")}`);
  }
  if (getValue(classicStarTable.values, "intoTable") !== "lt_classic_carriers") {
    fail(`Classic * intoTable expected lt_classic_carriers, got ${getValue(classicStarTable.values, "intoTable")}`);
  }
  whereClean(classicStarTable, "classicStarTable");

  const classicList = selects.find((o) =>
    /SELECT\s+carrid\s+carrname/i.test(String(o.raw || ""))
    && /lt_classic_names/i.test(String(o.raw || ""))
    && /INTO\s+TABLE/i.test(String(o.raw || ""))
    && !/APPENDING/i.test(String(o.raw || ""))
    && !/FIELDS/i.test(String(o.raw || ""))
  );
  if (!classicList) fail("Missing classic SELECT carrid carrname INTO TABLE (no FIELDS)");
  if (!String(getValue(classicList.values, "fields")).includes("carrid")) {
    fail(`Classic list fields should include carrid, got ${getValue(classicList.values, "fields")}`);
  }
  if (getValue(classicList.values, "intoTable") !== "lt_classic_names") {
    fail(`Classic list intoTable expected lt_classic_names, got ${getValue(classicList.values, "intoTable")}`);
  }
  whereClean(classicList, "classicList");

  const classicDistinct = selects.find((o) =>
    /SELECT\s+DISTINCT\s+planetype/i.test(String(o.raw || ""))
    && /lt_classic_planetypes/i.test(String(o.raw || ""))
    && !/FIELDS/i.test(String(o.raw || ""))
  );
  if (!classicDistinct) fail("Missing classic SELECT DISTINCT planetype");
  if (!String(getValue(classicDistinct.values, "fields") || "").includes("planetype")) {
    fail(`Classic DISTINCT fields should include planetype, got ${getValue(classicDistinct.values, "fields")}`);
  }
  whereClean(classicDistinct, "classicDistinct");

  console.log("PASS: deep sample parse + description smoke");
}

main();
