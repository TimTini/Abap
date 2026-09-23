"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const { loadConfigs } = require("./helpers/config-loader");
const { parseAbapTextDetailed } = require("../shared/abap-parser");

const repoRoot = path.resolve(__dirname, "..");
const inventoryPath = path.resolve(repoRoot, "docs/ABAP_SYNTAX_INVENTORY.md");
const statuses = new Set(["supported", "misclassified", "missing", "structural"]);

function loadSyntaxInventory(filePath) {
  const lines = fs.readFileSync(filePath, "utf8").split(/\r?\n/);
  const formsHeading = lines.findIndex((line) => /^## Forms\s*$/.test(line));
  assert.notEqual(formsHeading, -1, "inventory must contain a Forms section");
  const coverageHeading = lines.findIndex((line, index) => index > formsHeading && /^## SAP latest syntax probes\s*$/.test(line));
  const section = lines.slice(formsHeading + 1, coverageHeading === -1 ? undefined : coverageHeading);
  const table = section.filter((line) => /^\|/.test(line));
  const headers = table[0].split("|").slice(1, -1).map((cell) => cell.trim());
  return table.slice(2).map((line) => {
    const cells = line.split("|").slice(1, -1).map((cell) => cell.trim());
    return Object.fromEntries(headers.map((header, index) => [header, cells[index]]));
  }).map((row) => ({ ...row, line: Number(row.line) }));
}

function flattenObjects(objects) {
  return objects.flatMap((object) => [object, ...flattenObjects(object.children || [])]);
}

function canonicalKind(value) {
  return String(value || "").toUpperCase().replace(/[^A-Z0-9]+/g, "_").replace(/^_|_$/g, "");
}

function findAbapSources(directory) {
  return fs.readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const target = path.join(directory, entry.name);
    return entry.isDirectory() ? findAbapSources(target)
      : entry.isFile() && entry.name.endsWith(".abap") ? [target] : [];
  });
}

defineFocusedTest(test, "ABAP grammar inventory contract", ["grammar-inventory"], async (t) => {
  await t.test("rows have valid schema, source locations, status, and authoritative provenance", () => {
    const markdown = fs.readFileSync(inventoryPath, "utf8");
    const rows = loadSyntaxInventory(inventoryPath);
    assert.ok(rows.length > 0, "inventory must contain at least one syntax form");
    const inventoriedSources = new Set(rows.map((row) => row.source));
    const corpusSources = [...findAbapSources(path.join(repoRoot, "examples")),
      ...findAbapSources(path.join(repoRoot, "tests", "fixtures"))]
      .map((source) => path.relative(repoRoot, source).replaceAll(path.sep, "/"));
    assert.deepEqual([...inventoriedSources].sort(), corpusSources.sort(),
      "inventory must represent every committed ABAP example and fixture file");
    assert.match(markdown, /02481ae352dab5c659cf39cff1dae93173dd2709/);
    assert.match(markdown, /69700003ec1f01bd720c055d85e7fbef456fc506/);
    assert.match(markdown, /https:\/\/help\.sap\.com\/doc\/abapdocu_latest_index_htm\/latest\/en-US\/ABENABAP\.html/);
    const configs = loadConfigs(path.resolve(repoRoot, "configs"));
    const parsedBySource = new Map();
    const documentedKindsBySource = new Map();

    for (const row of rows) {
      assert.ok(row.source && row.line > 0 && row.rawForm, "source, positive line, and form are required");
      assert.ok(row.family && row.expectedKind, "grammar family and expected node kind are required");
      assert.ok(statuses.has(row.status), `unknown inventory status: ${row.status}`);
      assert.ok(row.dialect, "record source dialect provenance, or use 'not-gated'");
      assert.match(row.documentation, /^https:\/\//, "documentation must be a source URL");
      const sourcePath = path.resolve(repoRoot, row.source);
      assert.ok(sourcePath.startsWith(`${repoRoot}${path.sep}`), `source must be inside the repo: ${row.source}`);
      assert.ok(fs.existsSync(sourcePath), `missing source file: ${row.source}`);
      const sourceLines = fs.readFileSync(sourcePath, "utf8").split(/\r?\n/);
      assert.ok(row.line <= sourceLines.length, `line ${row.line} exceeds ${row.source}`);
      assert.ok(sourceLines[row.line - 1].toUpperCase().includes(row.rawForm.toUpperCase()),
        `${row.source}:${row.line} does not contain the recorded form: ${row.rawForm}`);
      if (!parsedBySource.has(row.source)) {
        parsedBySource.set(row.source, flattenObjects(parseAbapTextDetailed(
          fs.readFileSync(sourcePath, "utf8"), configs, row.source
        ).objects));
      }
      const actualKinds = parsedBySource.get(row.source)
        .filter((object) => object.lineStart === row.line)
        .map((object) => canonicalKind(object.objectType));
      assert.ok(actualKinds.includes(canonicalKind(row.expectedKind)),
        `${row.source}:${row.line} (${row.rawForm}) expected ${row.expectedKind}, got ${actualKinds.join(", ") || "no parser node"}`);
      if (!documentedKindsBySource.has(row.source)) documentedKindsBySource.set(row.source, new Set());
      documentedKindsBySource.get(row.source).add(canonicalKind(row.expectedKind));
    }

    for (const [source, objects] of parsedBySource) {
      const documentedKinds = documentedKindsBySource.get(source);
      const missingKinds = [...new Set(objects.map((object) => canonicalKind(object.objectType)))]
        .filter((kind) => !documentedKinds.has(kind));
      assert.deepEqual(missingKinds, [], `${source} has parser node kinds missing from the inventory`);
    }
  });
});
