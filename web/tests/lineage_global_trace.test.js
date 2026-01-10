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

function setupLineageWithParser() {
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
  loadScript("js/lineage.js");
  return ns;
}

function collectRoutineKeys(node, out = new Set()) {
  if (!node) return out;
  out.add(String(node.routineKey || ""));
  for (const c of node.calls || []) collectRoutineKeys(c?.child, out);
  return out;
}

test("lineage.traceGlobalVariable prunes unrelated routines but keeps call paths", () => {
  const ns = setupLineageWithParser();

  const abap = `
REPORT ztest.

DATA gv_x TYPE i.
DATA gv_y TYPE i.

START-OF-SELECTION.
  PERFORM run.
  PERFORM b.

FORM run.
  PERFORM a.
ENDFORM.

FORM a.
  PERFORM c USING gv_x.
  PERFORM d.
ENDFORM.

FORM b.
  DATA lv TYPE i.
  lv = 1.
ENDFORM.

FORM c USING iv_x TYPE i.
  gv_y = iv_x.
ENDFORM.

FORM d.
  gv_x = 3.
ENDFORM.
`;

  const model = ns.parser.parseProgram(abap);
  const res = ns.lineage.traceGlobalVariable(model, "gv_x", { maxNodes: 200 });

  assert.ok(res.root);
  const keys = collectRoutineKeys(res.root);

  assert.ok(keys.has("PROGRAM"));
  assert.ok(keys.has("EVENT:START-OF-SELECTION"));
  assert.ok(keys.has("FORM:run"));
  assert.ok(keys.has("FORM:a"));
  assert.ok(keys.has("FORM:c"));
  assert.ok(keys.has("FORM:d"));
  assert.equal(keys.has("FORM:b"), false);
});

