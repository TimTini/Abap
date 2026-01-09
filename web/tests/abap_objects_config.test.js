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

function setupAbapFlowWithAbapObjects() {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/doc.js");
  loadScript("js/model.js");
  loadScript("js/logic.js");
  loadScript("js/lineage.js");
  loadScript("js/notes.js");
  loadScript("js/desc_resolver.js");
  loadScript("js/template_converter.js");

  loadScript("abap_objects.config.js");
  loadScript("js/abap_objects/schema.js");
  loadScript("js/abap_objects/parsers.js");
  loadScript("js/abap_objects/loader.js");

  return global.window.AbapFlow;
}

test("abap_objects master config validates", () => {
  const ns = setupAbapFlowWithAbapObjects();
  const res = ns.abapObjects.schema.validateMasterConfig(ns.abapObjectsMasterConfig);
  assert.equal(res.ok, true);
  assert.deepEqual(res.errors, []);
});

test("abap_objects loader registers templates from master config", async () => {
  const ns = setupAbapFlowWithAbapObjects();

  await ns.abapObjects.init({
    config: ns.abapObjectsMasterConfig,
    loadScript: async (url) => {
      loadScript(url);
      return true;
    },
  });

  const templates = ns.templateRegistry?.templates || {};
  assert.ok(templates["perform.excel-like-table"]);
  assert.ok(templates["assignment.excel-like-table"]);
  assert.ok(templates["if.excel-like-table"]);
  assert.ok(templates["message.excel-like-table"]);
  assert.ok(templates["itabRead.excel-like-table"]);
  assert.ok(templates["itabCollect.excel-like-table"]);
  assert.ok(templates["itabModify.excel-like-table"]);
  assert.ok(templates["itabDelete.excel-like-table"]);
  assert.ok(templates["append.excel-like-table"]);
});

test("template_converter fills excel-like-table binds", async () => {
  const ns = setupAbapFlowWithAbapObjects();
  await ns.abapObjects.init({
    config: ns.abapObjectsMasterConfig,
    loadScript: async (url) => {
      loadScript(url);
      return true;
    },
  });

  const tplCfg = ns.templateRegistry.templates["assignment.excel-like-table"].config;
  const ctx = {
    item: { description: "Counter" },
    value: { description: "Counter + 1" },
  };

  const expanded = ns.templateConverter.expandExcelLikeTableTemplate(tplCfg, ctx);
  const filled = ns.templateConverter.compactExcelLikeTableConfig(ns.templateConverter.fillTemplateConfig(expanded, ctx));

  const byAddr = new Map(filled.cells.map((c) => [String(c.addr || "").toUpperCase(), c]));
  assert.equal(byAddr.get("A2").text, "Counter");
  assert.equal(byAddr.get("H2").text, "Counter + 1");
  assert.equal(byAddr.get("A2").bind, "item.description");
  assert.equal(byAddr.get("H2").bind, "value.description");
});

test("new ABAP Object can be added by config + template only", async () => {
  global.window = { AbapFlow: {} };
  global.localStorage = createMemoryStorage();

  loadScript("js/namespace.js");
  loadScript("js/utils.js");
  loadScript("js/template_converter.js");
  loadScript("js/abap_objects/schema.js");
  loadScript("js/abap_objects/parsers.js");
  loadScript("js/abap_objects/loader.js");

  const ns = global.window.AbapFlow;

  const master = {
    schema: "abapflow-abap-objects-master-config",
    version: 1,
    parserConfig: { version: 1 },
    objects: [
      {
        id: "echo",
        kind: "statement",
        label: "ECHO",
        parse: { kind: "regex", regex: /^ECHO\s+(.+)$/i, fields: { value: 1 } },
        builder: { kind: "mapping", fields: { value: { type: "expr", from: "value" } } },
        templates: [{ id: "echo.excel", label: "ECHO template", auto: true, file: "__inline__/echo.excel.js" }],
      },
    ],
  };

  await ns.abapObjects.init({
    config: master,
    loadScript: async (url) => {
      if (url === "__inline__/echo.excel.js") {
        ns.abapObjects.defineTemplate("echo.excel", {
          type: "excel-like-table",
          grid: { rows: 1, cols: 4, colWidths: { A: 200 }, rowHeights: { 1: 30 } },
          css: { cell: "border:1px solid #222;" },
          merges: [{ start: "A1", rowspan: 1, colspan: 4 }],
          cells: [{ addr: "A1", text: "{value.description}", class: ["cell"] }],
        });
        return true;
      }
      throw new Error(`Unexpected template url: ${url}`);
    },
  });

  const registry = ns.abapObjects.getRegistry();
  const items = registry.parseStatementItems(null, { key: "FORM:MAIN", kind: "FORM", name: "main" }, "ECHO lv_demo", { startLine: 1, endLine: 1 }, null);
  assert.equal(items.length, 1);
  assert.equal(items[0].objectId, "echo");

  const objDef = registry.objectsById.get("echo");
  const ctx = registry.buildContext(objDef, null, { key: "FORM:MAIN", kind: "FORM", name: "main" }, items[0].payload, { callPath: [] });
  assert.equal(String(ctx?.value?.description || ""), "lv_demo");

  const tplCfg = ns.templateRegistry.templates["echo.excel"].config;
  const filled = ns.templateConverter.compactExcelLikeTableConfig(ns.templateConverter.fillTemplateConfig(tplCfg, ctx));
  assert.equal(filled.cells[0].text, "lv_demo");
  assert.equal(filled.cells[0].bind, "value.description");
});
