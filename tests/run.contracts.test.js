"use strict";

const assert = require("assert");
const { spawnSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { test } = require("node:test");
const { defineFocusedTest } = require("./helpers/test-focus");
const { entries, suiteNames } = require("./registry");

const repoRoot = path.resolve(__dirname, "..");
const runnerPath = path.resolve(__dirname, "run.js");

function runRunner(args) {
  return spawnSync(process.execPath, [runnerPath, ...args], {
    cwd: repoRoot,
    encoding: "utf8"
  });
}

defineFocusedTest(test, "runner contracts", ["runner"], async (t) => {
  await t.test("registry covers every domain test and keeps focus labels unique per suite", () => {
    const registeredFiles = entries.map((entry) => entry.file).sort();
    const discoveredFiles = fs.readdirSync(path.resolve(__dirname))
      .filter((fileName) => fileName.endsWith(".test.js"))
      .map((fileName) => `tests/${fileName}`)
      .sort();
    assert.deepStrictEqual(registeredFiles, discoveredFiles);

    for (const entry of entries) {
      const source = fs.readFileSync(path.resolve(repoRoot, entry.file), "utf8");
      for (const suite of entry.suites) {
        assert(suiteNames.includes(suite), `${entry.file}: unknown registered suite ${suite}`);
      }
      for (const label of entry.labels) {
        assert(source.includes(`"${label}"`), `${entry.file}: focus label ${label} is not declared in the test source`);
      }
    }

    for (const suite of suiteNames) {
      const labels = entries
        .filter((entry) => entry.suites.includes(suite))
        .flatMap((entry) => entry.labels);
      assert.strictEqual(new Set(labels).size, labels.length, `${suite}: focus labels must be unique`);
    }
  });

  await t.test("exits nonzero for an unknown suite", () => {
    const result = runRunner(["unknown-suite"]);
    assert.notStrictEqual(result.status, 0);
    assert.match(result.stderr || result.stdout, /Unknown suite "unknown-suite"/);
  });

  await t.test("exits nonzero for an unknown focus", () => {
    const result = runRunner(["viewer", "unknown-focus"]);
    assert.notStrictEqual(result.status, 0);
    assert.match(result.stderr || result.stdout, /Unknown focus "unknown-focus"/);
  });

  await t.test("exits nonzero for a focus prefix that is not registered", () => {
    const result = runRunner(["viewer", "scroll"]);
    assert.notStrictEqual(result.status, 0);
    assert.match(result.stderr || result.stdout, /Unknown focus "scroll"/);
  });
});
