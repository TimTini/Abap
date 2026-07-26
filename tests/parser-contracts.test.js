"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { test } = require("node:test");
const { parseAbapText } = require("../shared/abap-parser");
const { loadConfigs } = require("./helpers/config-loader");
const {
  assertJsonArtifactsMatchFixtures,
  diffJson,
  filterDiffsByAllowedPaths,
  listFixtureFiles,
  loadAllowedPaths,
  normalizeParserResult,
  readJson
} = require("./helpers/contracts");
const { defineFocusedTest } = require("./helpers/test-focus");

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
  assert.deepStrictEqual(segmentIndexes, [0, 1]);
  assert.deepStrictEqual(lineStarts, [3, 3]);
  assert.deepStrictEqual(raws, ["DATA lv_a TYPE i.", "DATA lv_b TYPE i."]);
}

function assertParserFixture(fileName) {
  const fixturePath = path.join(fixturesDir, fileName);
  const baselinePath = path.join(baselineDir, fileName.replace(/\.abap$/i, ".json"));
  const allowedPath = path.join(allowedDir, fileName.replace(/\.abap$/i, ".json"));
  assert(fs.existsSync(baselinePath), `Missing parser baseline for ${fileName}. Run npm run build:baselines.`);

  const source = fs.readFileSync(fixturePath, "utf8");
  const actual = normalizeParserResult(parseAbapText(source, configs, fileName));
  const expected = readJson(baselinePath);
  const diffs = filterDiffsByAllowedPaths(diffJson(expected, actual), loadAllowedPaths(allowedPath));

  assert.deepStrictEqual(diffs, [], `${fileName}: parser contract mismatch.\n${JSON.stringify(diffs.slice(0, 20), null, 2)}`);
}

defineFocusedTest(test, "parser contracts", ["parser-contracts"], async (t) => {
  const fixtureFiles = listFixtureFiles(fixturesDir);

  await t.test("baseline and allowed-delta fixtures stay in sync", () => {
    assertJsonArtifactsMatchFixtures(baselineDir, fixtureFiles, "parser baseline");
    assertJsonArtifactsMatchFixtures(allowedDir, fixtureFiles, "parser allowed-delta", { optional: true });
  });

  await t.test("every parser fixture matches its baseline contract", async (fixtureSuite) => {
    for (const fileName of fixtureFiles) {
      await fixtureSuite.test(`matches ${fileName}`, () => {
        assertParserFixture(fileName);
      });
    }
  });

  await t.test("multi-statement segment index fixture stays stable", () => {
    assertMultiStatementSegmentIndexFixture();
  });
});
