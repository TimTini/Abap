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

function setupParserWithMasterConfig() {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/doc.js");
  loadScript("js/model.js");
  loadScript("js/graph.js");
  loadScript("abap_objects.config.js");

  const ns = global.window.AbapFlow;
  ns.parserConfig = ns.abapObjectsMasterConfig?.parserConfig || { version: 1 };

  loadScript("js/parser.js");
  return ns;
}

test("parser extracts PARAMETERS selection-screen declarations", () => {
  const ns = setupParserWithMasterConfig();

  const abap = `
* User name
PARAMETERS p_user TYPE syuname DEFAULT sy-uname. " Current user
PARAMETERS p_text(10) TYPE c. " Text
PARAMETERS p_len LENGTH 5. " Len only

PARAMETERS: p_a TYPE i DEFAULT 1, " A
            p_b TYPE i. " B

START-OF-SELECTION.
  WRITE: / p_user.
`;

  const model = ns.parser.parseProgram(abap);
  const params = (model.globalData || []).filter((d) => String(d?.declKind || "").toUpperCase() === "PARAMETERS");

  assert.equal(params.length, 5);

  const byName = new Map(params.map((p) => [String(p.variableName || ""), p]));
  assert.equal(byName.get("p_user").dataType, "syuname");
  assert.equal(byName.get("p_user").value, "sy-uname");
  assert.equal(byName.get("p_user").description, "Current user");

  assert.equal(byName.get("p_text").dataType, "c LENGTH 10");
  assert.equal(byName.get("p_text").description, "Text");

  assert.equal(byName.get("p_len").dataType, "c LENGTH 5");
  assert.equal(byName.get("p_len").description, "Len only");

  assert.equal(byName.get("p_a").dataType, "i");
  assert.equal(byName.get("p_a").value, "1");
  assert.equal(byName.get("p_a").description, "A");

  assert.equal(byName.get("p_b").dataType, "i");
  assert.equal(byName.get("p_b").description, "B");
});
