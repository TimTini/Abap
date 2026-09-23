"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { test } = require("node:test");
const { parseAbapText } = require("../shared/abap-parser");
const { loadConfigs } = require("./helpers/config-loader");
const { defineFocusedTest } = require("./helpers/test-focus");

const root = path.resolve(__dirname, "..");
const configsDir = path.join(root, "configs");
const inventoryPath = path.join(__dirname, "fixtures", "parser", "sap-latest-coverage.json");
const sapBase = "https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/";

function flatten(objects) {
  return objects.flatMap((object) => [object, ...flatten(object.children || [])]);
}

defineFocusedTest(test, "SAP syntax inventory covers each configured parser matcher", ["sap-coverage"], () => {
  const inventory = JSON.parse(fs.readFileSync(inventoryPath, "utf8"));
  const configFiles = fs.readdirSync(configsDir).filter((name) => name.endsWith(".json")).sort();
  const documentedFiles = inventory.forms.flatMap((form) => form.configFiles || []).sort();
  assert.deepEqual([...new Set(documentedFiles)], configFiles);
  assert.equal(inventory.forms.length, new Set(inventory.forms.map((form) => form.id)).size);

  const configs = loadConfigs(configsDir);
  const configsByFile = new Map(configs.map((config) => [config._sourceFile, config]));
  assert.equal(new Set(configs.map((config) => config.object)).size, 45);

  for (const form of inventory.forms) {
    assert.match(form.sapUrl, /^https:\/\/help\.sap\.com\/doc\/abapdocu_latest_index_htm\/latest\/en-US\/[^\s]+\.html$/);
    assert.ok(form.source && form.source.trim(), `${form.id}: missing source`);
    assert.ok(form.expectedType, `${form.id}: missing expected type`);
    assert.ok(form.assertions && Object.keys(form.assertions).length, `${form.id}: missing field assertions`);
    assert.ok(form.dialect, `${form.id}: missing dialect annotation`);

    assert.ok(form.configFiles && form.configFiles.length, `${form.id}: missing config file mapping`);
    const formConfigs = form.configFiles.map((file) => {
      const config = configsByFile.get(file);
      assert.ok(config, `${form.id}: missing config ${file}`);
      assert.equal(config.object, form.expectedType, `${form.id}: ${file} maps to the wrong object type`);
      const matcher = config.match || {};
      const startPhrase = matcher.startPhrase || matcher.startKeyword || (matcher.startTokens || []).join(" ");
      if (startPhrase) {
        const normalizedSource = form.source.trim().toUpperCase().replace(/\s+/g, " ");
        const normalizedMatcher = String(startPhrase).toUpperCase().replace(/\s+/g, " ");
        assert.ok(
          normalizedSource.includes(normalizedMatcher),
          `${form.id}: source does not contain ${file} matcher ${startPhrase}`
        );
      } else {
        assert.ok(
          ["assignment", "methodcallexpr"].includes(String(matcher.type || "").toLowerCase()),
          `${form.id}: ${file} has no supported matcher`
        );
      }
      return config;
    });
    const configOrders = form.checkReversedConfigs
      ? [formConfigs, configs, configs.slice().reverse()]
      : [formConfigs];
    for (const orderedConfigs of configOrders) {
      const objects = flatten(parseAbapText(form.source, orderedConfigs, `${form.id}.abap`).objects);
      const object = objects.find((entry) => entry.objectType === form.expectedType);
      assert.ok(object, `${form.id}: ${form.expectedType} missing; got ${objects.map((entry) => entry.objectType)}`);
      for (const [key, expected] of Object.entries(form.assertions)) {
        const actual = key.split(".").reduce((value, part) => value && value[part], object);
        assert.deepEqual(actual, expected, `${form.id}: ${key}`);
      }
    }
  }
});
