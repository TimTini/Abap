"use strict";

const fs = require("fs");
const path = require("path");
const { parseAbapText } = require("../shared/abap-parser");
const { loadConfigs } = require("../cli/config-loader");
const {
  listFixtureFiles,
  normalizeParserResult,
  normalizeViewerState,
  writeJson
} = require("./helpers/contracts");
const { renderFixture } = require("./helpers/viewer-harness");

const repoRoot = path.resolve(__dirname, "..");
const configs = loadConfigs(path.resolve(repoRoot, "configs"));

async function buildParserBaselines() {
  const fixturesDir = path.resolve(__dirname, "fixtures", "parser");
  const baselineDir = path.resolve(__dirname, "baselines", "parser");
  for (const fileName of listFixtureFiles(fixturesDir)) {
    const fullPath = path.join(fixturesDir, fileName);
    const content = fs.readFileSync(fullPath, "utf8");
    const parsed = parseAbapText(content, configs, fileName);
    writeJson(
      path.join(baselineDir, fileName.replace(/\.abap$/i, ".json")),
      normalizeParserResult(parsed)
    );
  }
}

async function buildViewerBaselines() {
  const fixturesDir = path.resolve(__dirname, "fixtures", "viewer");
  const baselineDir = path.resolve(__dirname, "baselines", "viewer");
  for (const fileName of listFixtureFiles(fixturesDir)) {
    const fullPath = path.join(fixturesDir, fileName);
    const content = fs.readFileSync(fullPath, "utf8");
    const dom = await renderFixture(content);
    writeJson(
      path.join(baselineDir, fileName.replace(/\.abap$/i, ".json")),
      normalizeViewerState(dom.window)
    );
    dom.window.close();
  }
}

async function main() {
  await buildParserBaselines();
  await buildViewerBaselines();
  console.log("regression-baselines: ok");
}

main().catch((err) => {
  console.error(err && err.stack ? err.stack : err);
  process.exitCode = 1;
});
