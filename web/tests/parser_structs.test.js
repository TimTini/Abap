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

function setupParser() {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/notes.js");
  loadScript("js/doc.js");
  loadScript("js/model.js");
  loadScript("js/graph.js");
  loadScript("js/lineage.js");
  loadScript("js/desc_resolver.js");

  loadScript("abap_objects.config.js");
  const ns = global.window.AbapFlow;
  ns.parserConfig = ns.abapObjectsMasterConfig?.parserConfig || { version: 1 };
  loadScript("js/parser.js");

  return ns;
}

test("parser extracts DATA BEGIN OF struct fields (global)", () => {
  const ns = setupParser();

  const abap = `
REPORT z.
* Struct description
DATA: BEGIN OF ls_struct,
        item_1 TYPE i, " Item desc 1
        item_2 TYPE string,
        BEGIN OF sub, " Sub desc
          sub_1 TYPE i, " Sub1 desc
        END OF sub,
      END OF ls_struct.
`;

  const model = ns.parser.parseProgram(abap);
  const names = new Set((model.globalData || []).map((d) => String(d?.variableName || "")));

  assert.ok(names.has("ls_struct"));
  assert.ok(names.has("ls_struct-item_1"));
  assert.ok(names.has("ls_struct-item_2"));
  assert.ok(names.has("ls_struct-sub"));
  assert.ok(names.has("ls_struct-sub-sub_1"));

  const byName = new Map((model.globalData || []).map((d) => [String(d?.variableName || ""), d]));
  assert.equal(byName.get("ls_struct").description, "Struct description");
  assert.equal(byName.get("ls_struct-item_1").description, "Item desc 1");
  assert.equal(byName.get("ls_struct-sub").description, "Sub desc");
  assert.equal(byName.get("ls_struct-sub-sub_1").description, "Sub1 desc");
});

test("parser stores TYPES BEGIN OF struct and expands typed DATA into virtual fields", () => {
  const ns = setupParser();

  const abap = `
REPORT z.
TYPES: BEGIN OF ty_struct,
         item_1 TYPE i, " Item1
         BEGIN OF sub, " Sub
           sub_1 TYPE string, " Sub1
         END OF sub,
       END OF ty_struct.

DATA ls TYPE ty_struct. " Struct var
`;

  const model = ns.parser.parseProgram(abap);

  const typeDef = model.typeDefs.get("PROGRAM|ty_struct");
  assert.ok(typeDef);
  assert.equal(typeDef.name, "ty_struct");
  assert.ok(typeDef.fields.get("item_1"));
  assert.ok(typeDef.fields.get("sub"));
  assert.ok(typeDef.fields.get("sub-sub_1"));
  assert.equal(typeDef.fields.get("item_1").description, "Item1");
  assert.equal(typeDef.fields.get("sub").description, "Sub");
  assert.equal(typeDef.fields.get("sub-sub_1").description, "Sub1");

  const byName = new Map((model.globalData || []).map((d) => [String(d?.variableName || ""), d]));
  assert.equal(byName.get("ls").dataType, "ty_struct");
  assert.equal(byName.get("ls").description, "Struct var");

  assert.ok(byName.get("ls-item_1"));
  assert.ok(byName.get("ls-sub"));
  assert.ok(byName.get("ls-sub-sub_1"));

  assert.equal(byName.get("ls-item_1").isVirtual, true);
  assert.equal(byName.get("ls-item_1").description, "Item1");
  assert.deepEqual(byName.get("ls-item_1").virtualOrigin, {
    kind: "typeField",
    typeScopeKey: "PROGRAM",
    typeName: "ty_struct",
    fieldPath: "item_1",
  });
});

test("desc_resolver describes struct paths as desc_root-desc_field and supports TYPEFIELD notes", () => {
  const ns = setupParser();

  const abap = `
REPORT z.
TYPES: BEGIN OF ty_struct,
         item_1 TYPE i, " Item1
       END OF ty_struct.

DATA ls TYPE ty_struct. " Struct var

START-OF-SELECTION.
`;

  const model = ns.parser.parseProgram(abap);

  const res1 = ns.desc.describeExpressionWithOrigin(model, "EVENT:START-OF-SELECTION", "ls-item_1", {});
  assert.equal(res1.text, "Struct var-Item1");
  assert.equal(res1.originKey, "TYPEFIELD:PROGRAM:ty_struct:item_1");

  ns.notes.setEntry("TYPEFIELD:PROGRAM:ty_struct:item_1", { description: "Custom item" });
  ns.notes.applyToModel(model);

  const res2 = ns.desc.describeExpressionWithOrigin(model, "EVENT:START-OF-SELECTION", "ls-item_1", {});
  assert.equal(res2.text, "Struct var-Custom item");
});

test("desc_resolver exposes origins for multi-token expressions and struct segments", () => {
  const ns = setupParser();

  const abap = `
REPORT z.

* Global config (BEGIN OF struct)
DATA: BEGIN OF gs_conf,
        counter TYPE i, " Counter
        BEGIN OF meta, " Meta
          depth TYPE i, " Depth
        END OF meta,
      END OF gs_conf.

PARAMETERS p_a TYPE i.
PARAMETERS p_b TYPE i.

START-OF-SELECTION.
`;

  const model = ns.parser.parseProgram(abap);

  const expr = ns.desc.describeExpressionWithOrigin(model, "EVENT:START-OF-SELECTION", "p_a + p_b", {});
  const keys = new Set((expr.origins || []).map((o) => String(o?.key || "")));
  assert.ok(keys.has(ns.notes.makeDeclKey("PROGRAM", "PARAMETERS", "p_a")));
  assert.ok(keys.has(ns.notes.makeDeclKey("PROGRAM", "PARAMETERS", "p_b")));

  const structExpr = ns.desc.describeExpressionWithOrigin(model, "EVENT:START-OF-SELECTION", "gs_conf-meta-depth", {});
  const structKeys = new Set((structExpr.origins || []).map((o) => String(o?.key || "")));
  assert.ok(structKeys.has(ns.notes.makeDeclKey("PROGRAM", "DATA", "gs_conf")));
  assert.ok(structKeys.has(ns.notes.makeDeclKey("PROGRAM", "DATA", "gs_conf-meta")));
  assert.ok(structKeys.has(ns.notes.makeDeclKey("PROGRAM", "DATA", "gs_conf-meta-depth")));
});
