"use strict";

const { spawnSync } = require("child_process");
const { getEntriesForSuite, resolveFocus, suiteNames, toAbsoluteFile } = require("./registry");

function fail(message) {
  console.error(message);
  process.exit(2);
}

function parseArguments(argv) {
  const positional = [];
  let suite = "";
  let focus = "";

  for (let index = 0; index < argv.length; index += 1) {
    const arg = String(argv[index] || "").trim();
    if (arg.startsWith("--suite=")) {
      suite = arg.slice("--suite=".length).trim();
      continue;
    }
    if (arg === "--suite") {
      suite = String(argv[index + 1] || "").trim();
      index += 1;
      continue;
    }
    if (arg.startsWith("--focus=")) {
      focus = arg.slice("--focus=".length).trim();
      continue;
    }
    if (arg === "--focus") {
      focus = String(argv[index + 1] || "").trim();
      index += 1;
      continue;
    }
    if (arg.startsWith("--")) {
      fail(`Unknown option "${arg}".`);
    }
    positional.push(arg);
  }

  if (!suite && positional.length) {
    suite = positional.shift();
  }
  if (!focus && positional.length) {
    focus = positional.shift();
  }
  if (positional.length) {
    fail(`Unexpected argument "${positional[0]}".`);
  }
  return { suite, focus };
}

function main() {
  const { suite, focus: requestedFocus } = parseArguments(process.argv.slice(2));

  if (!suite || !suiteNames.includes(suite)) {
    fail(`Unknown suite "${suite}". Expected one of: ${suiteNames.join(", ")}`);
  }

  const suiteEntries = getEntriesForSuite(suite);
  const resolution = resolveFocus(suiteEntries, requestedFocus);
  if (resolution.error) {
    fail(resolution.error);
  }

  for (const entry of resolution.matches) {
    const env = { ...process.env };
    if (resolution.resolvedFocus) {
      env.ABAP_TEST_FOCUS = resolution.resolvedFocus;
    } else {
      delete env.ABAP_TEST_FOCUS;
    }

    const result = spawnSync(
      process.execPath,
      ["--test", toAbsoluteFile(entry)],
      {
        cwd: require("path").resolve(__dirname, ".."),
        env,
        stdio: "inherit"
      }
    );

    if (result.status !== 0) {
      process.exit(result.status || 1);
    }
  }
}

main();
