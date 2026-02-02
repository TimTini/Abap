const fs = require("fs");
const path = require("path");
const { loadConfigs } = require("./config-loader");
const { parseAbapText } = require("../shared/abap-parser");

function main() {
  const targetFile = process.argv[2];
  if (!targetFile) {
    console.log("Usage: node cli/parse.js <abap-file>");
    process.exit(1);
  }

  const configDir = path.resolve(__dirname, "..", "configs");
  const configs = loadConfigs(configDir);

  const content = fs.readFileSync(targetFile, "utf8");
  const fileName = path.basename(targetFile);
  const result = parseAbapText(content, configs, fileName);

  console.log(JSON.stringify(result, null, 2));
}

main();
