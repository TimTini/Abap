"use strict";

const fs = require("fs");
const path = require("path");
const { normalizeConfig } = require("../../shared/abap-parser.js");

function loadConfigs(configDir) {
  const files = fs.readdirSync(configDir)
    .filter((fileName) => fileName.endsWith(".json"))
    .sort((left, right) => left.localeCompare(right));

  return files.map((fileName) => {
    const raw = fs.readFileSync(path.join(configDir, fileName), "utf8");
    const config = JSON.parse(raw);
    return normalizeConfig({ ...config, _sourceFile: fileName });
  });
}

module.exports = {
  loadConfigs
};
