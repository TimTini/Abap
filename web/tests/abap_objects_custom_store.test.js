"use strict";

const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function createMemoryStorage() {
  const store = new Map();
  return {
    getItem: (key) => (store.has(String(key)) ? store.get(String(key)) : null),
    setItem: (key, value) => {
      store.set(String(key), String(value));
    },
    removeItem: (key) => {
      store.delete(String(key));
    },
    clear: () => {
      store.clear();
    },
  };
}

function loadScript(relPath) {
  const absPath = path.join(__dirname, "..", relPath);
  const code = fs.readFileSync(absPath, "utf8");
  vm.runInThisContext(code, { filename: absPath });
}

function setupCustomStoreOnly() {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/abap_objects/custom_store.js");

  return global.window.AbapFlow;
}

function setupWithLoader() {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/abap_objects/custom_store.js");
  loadScript("js/abap_objects/schema.js");
  loadScript("js/abap_objects/parsers.js");
  loadScript("js/abap_objects/loader.js");

  return global.window.AbapFlow;
}

test("abap_objects custom_store merges overrides + disabled", () => {
  const ns = setupCustomStoreOnly();
  const storeApi = ns.abapObjects.customStore;

  const base = {
    schema: "abapflow-abap-objects-master-config",
    version: 1,
    parserConfig: { version: 1 },
    objects: [
      { id: "a", kind: "statement", label: "A", parse: { kind: "regex", regex: "^A$" }, builder: { kind: "mapping", fields: {} } },
      { id: "b", kind: "statement", label: "B", parse: { kind: "regex", regex: "^B$" }, builder: { kind: "mapping", fields: {} } },
    ],
  };

  storeApi.upsertObjectDef({ id: "a", kind: "statement", label: "A (override)", parse: { kind: "regex", regex: "^A$" }, builder: { kind: "mapping", fields: {} } });
  storeApi.upsertObjectDef({ id: "c", kind: "statement", label: "C (custom)", parse: { kind: "regex", regex: "^C$" }, builder: { kind: "mapping", fields: {} } });
  storeApi.setObjectDisabled("b", true);

  const effective = storeApi.getEffectiveConfig(base).config;
  const ids = new Set((effective.objects || []).map((o) => String(o?.id || "")));

  assert.equal(effective.objects.find((o) => o.id === "a").label, "A (override)");
  assert.equal(ids.has("b"), false);
  assert.equal(ids.has("c"), true);
});

test("abap_objects loader uses custom_store effective config when no options.config", async () => {
  const ns = setupWithLoader();

  ns.abapObjectsMasterConfig = {
    schema: "abapflow-abap-objects-master-config",
    version: 1,
    parserConfig: { version: 1 },
    objects: [
      { id: "echo", kind: "statement", label: "ECHO", parse: { kind: "regex", regex: "^ECHO$" }, builder: { kind: "mapping", fields: {} } },
      { id: "other", kind: "statement", label: "OTHER", parse: { kind: "regex", regex: "^OTHER$" }, builder: { kind: "mapping", fields: {} } },
    ],
  };

  ns.abapObjects.customStore.upsertObjectDef({
    id: "echo",
    kind: "statement",
    label: "ECHO (override)",
    parse: { kind: "regex", regex: "^ECHO$" },
    builder: { kind: "mapping", fields: {} },
  });
  ns.abapObjects.customStore.setObjectDisabled("other", true);

  await ns.abapObjects.init();
  const registry = ns.abapObjects.getRegistry();

  assert.equal(registry.objectsById.get("echo").label, "ECHO (override)");
  assert.equal(registry.objectsById.has("other"), false);
});

