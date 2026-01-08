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

function setupAbapFlow() {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/notes.js");
  loadScript("js/ui_core.js");
  loadScript("js/ui_anno.js");
  loadScript("js/ui_templates_store.js");
  loadScript("js/desc_resolver.js");

  return global.window.AbapFlow;
}

test("utils.safeJsonParse returns ok/value or ok/error", () => {
  const ns = setupAbapFlow();
  const okRes = ns.utils.safeJsonParse('{"a":1}');
  assert.equal(okRes.ok, true);
  assert.deepEqual(okRes.value, { a: 1 });

  const badRes = ns.utils.safeJsonParse("{");
  assert.equal(badRes.ok, false);
  assert.equal(typeof badRes.error, "string");
  assert.ok(badRes.error.length > 0);
});

test("utils.nowIso returns ISO string", () => {
  const ns = setupAbapFlow();
  const stamp = ns.utils.nowIso();
  assert.equal(typeof stamp, "string");
  assert.ok(stamp.includes("T"));
  assert.ok(!Number.isNaN(Date.parse(stamp)));
});

test("utils.extractSource slices 1-based lines and trims", () => {
  const ns = setupAbapFlow();
  const model = { lines: ["  A", "B  ", " C "] };
  assert.equal(ns.utils.extractSource(model, { startLine: 1, endLine: 2 }), "A\nB");
  assert.equal(ns.utils.extractSource(model, { startLine: 2, endLine: 2 }), "B");
  assert.equal(ns.utils.extractSource(model, { startLine: 3 }), "C");
  assert.equal(ns.utils.extractSource(null, { startLine: 1, endLine: 1 }), "");
});

test("ui.renderInlineAnnoEditorHtml renders escaped HTML and data-* attrs", () => {
  const ns = setupAbapFlow();
  const html = ns.ui.renderInlineAnnoEditorHtml({
    title: "Edit <notes>",
    style: "padding-left:14px",
    codeDesc: "",
    userDesc: "<x>",
    userNote: "a & b",
    annoType: "decl",
    annoKey: "DECL:PROGRAM:DATA:lv_a",
    attrs: {
      "data-scope-key": "PROGRAM",
      "data-decl-kind": "DATA",
      "data-var-name": "lv_<a>",
    },
  });

  assert.ok(html.includes('style="padding-left:14px"'));
  assert.ok(html.includes("Edit &lt;notes&gt;"));
  assert.ok(html.includes('data-anno-type="decl"'));
  assert.ok(html.includes('data-anno-key="DECL:PROGRAM:DATA:lv_a"'));
  assert.ok(html.includes('data-scope-key="PROGRAM"'));
  assert.ok(html.includes('data-var-name="lv_&lt;a&gt;"'));
  assert.ok(html.includes("anno-code--empty"));
  assert.ok(html.includes("&lt;x&gt;"));
  assert.ok(html.includes("a &amp; b"));
});

test("notes.importJson rejects invalid JSON", () => {
  const ns = setupAbapFlow();
  const res = ns.notes.importJson("{", { mode: "merge" });
  assert.equal(res.ok, false);
});

test("notes.importJson accepts normalized store", () => {
  const ns = setupAbapFlow();
  const payload = JSON.stringify({
    schema: "abapflow-notes",
    version: 1,
    updatedAt: "2020-01-01T00:00:00.000Z",
    programs: {
      default: {
        updatedAt: "2020-01-01T00:00:00.000Z",
        routines: {
          PROGRAM: { description: "d", note: "n", updatedAt: "2020-01-01T00:00:00.000Z" },
        },
      },
    },
  });
  const res = ns.notes.importJson(payload, { mode: "merge" });
  assert.equal(res.ok, true);
  assert.equal(res.mode, "merge");
});

