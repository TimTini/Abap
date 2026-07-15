"use strict";

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "..", "..");
const partPaths = [
  "shared/abap-parser/01-context.js",
  "shared/abap-parser/02-config.js",
  "shared/abap-parser/03-statements.js",
  "shared/abap-parser/04-parse-core.js",
  "shared/abap-parser/05-extras.js",
  "shared/abap-parser/06-conditions.js",
  "shared/abap-parser/07-declarations.js",
  "shared/abap-parser/08-helpers.js",
  "shared/abap-parser/09-public-api.js"
];

const source = partPaths
  .map((partPath) => fs.readFileSync(path.resolve(repoRoot, partPath), "utf8"))
  .join("\n");
const parserModule = { exports: {} };
const loadSourceParser = new Function("module", "exports", "require", source);
loadSourceParser(parserModule, parserModule.exports, require);

module.exports = parserModule.exports;
