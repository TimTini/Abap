"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { parseAbapText } = require("./source-parser");
const { loadConfigs } = require("./config-loader");

const configs = loadConfigs(path.resolve(__dirname, "..", "..", "configs"));
const configsDir = path.resolve(__dirname, "..", "..", "configs");

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

module.exports = {
  assertHasObjectTypes,
  configs,
  findObject,
  findObjects,
  flattenObjects,
  fs,
  getConfigFileNames,
  getValue,
  getValueEntry,
  parse,
  parseAbapText,
  path
};
