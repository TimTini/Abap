"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { parseAbapText } = require("../shared/abap-parser");
const { loadConfigs } = require("../cli/config-loader");
const {
  diffJson,
  filterDiffsByAllowedPaths,
  listFixtureFiles,
  normalizeParserResult,
  readJson
} = require("./helpers/contracts");

const repoRoot = path.resolve(__dirname, "..");
const configs = loadConfigs(path.resolve(repoRoot, "configs"));
const fixturesDir = path.resolve(__dirname, "fixtures", "parser");
const baselineDir = path.resolve(__dirname, "baselines", "parser");
const allowedDir = path.resolve(__dirname, "allowed-deltas", "parser");

function assertMultiStatementSegmentIndexFixture() {
  const fixturePath = path.resolve(__dirname, "fixtures", "viewer", "multi-statement-navigation.abap");
  assert(fs.existsSync(fixturePath), "Missing multi-statement viewer fixture for parser segment-index check.");
  const source = fs.readFileSync(fixturePath, "utf8");
  const actual = parseAbapText(source, configs, "multi-statement-navigation.abap");
  const dataObjects = Array.isArray(actual && actual.objects)
    ? actual.objects.filter((obj) => obj && obj.objectType === "DATA")
    : [];
  const segmentIndexes = Array.from(dataObjects, (obj) => Number(obj && obj.segmentIndex));
  const lineStarts = Array.from(dataObjects, (obj) => Number(obj && obj.lineStart));
  const raws = Array.from(dataObjects, (obj) => String((obj && obj.raw) || "").trim());
  assert.deepStrictEqual(
    segmentIndexes,
    [0, 1],
    "Expected the parser to preserve segment indexes for both DATA statements on the same line."
  );
  assert.deepStrictEqual(
    lineStarts,
    [3, 3],
    "Expected both DATA statements to stay on the same source line."
  );
  assert.deepStrictEqual(
    raws,
    ["DATA lv_a TYPE i.", "DATA lv_b TYPE i."],
    "Expected the parser to split the two DATA statements into separate raw nodes."
  );
}

function assertParserFixture(fileName) {
  const fixturePath = path.join(fixturesDir, fileName);
  const baselinePath = path.join(baselineDir, fileName.replace(/\.abap$/i, ".json"));
  const allowedPath = path.join(allowedDir, fileName.replace(/\.abap$/i, ".json"));
  assert(fs.existsSync(baselinePath), `Missing parser baseline for ${fileName}. Run npm run build:baselines.`);
  assert(fs.existsSync(allowedPath), `Missing parser allowed-delta manifest for ${fileName}.`);

  const source = fs.readFileSync(fixturePath, "utf8");
  const actual = normalizeParserResult(parseAbapText(source, configs, fileName));
  const expected = readJson(baselinePath);
  const allowed = readJson(allowedPath);
  const diffs = filterDiffsByAllowedPaths(diffJson(expected, actual), allowed.allowedPaths);

  assert.deepStrictEqual(diffs, [], `${fileName}: parser contract mismatch.\n${JSON.stringify(diffs.slice(0, 20), null, 2)}`);
}

for (const fileName of listFixtureFiles(fixturesDir)) {
  assertParserFixture(fileName);
}

assertMultiStatementSegmentIndexFixture();

console.log("parser-contracts: ok");
