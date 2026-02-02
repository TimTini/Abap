const fs = require("fs");
const path = require("path");

function loadConfigs(configDir) {
  const files = fs.readdirSync(configDir).filter((f) => f.endsWith(".json"));
  return files.map((file) => {
    const raw = fs.readFileSync(path.join(configDir, file), "utf8");
    const config = JSON.parse(raw);
    return normalizeConfig(config, file);
  });
}

function normalizeConfig(config, fileName) {
  const normalized = {
    ...config,
    _sourceFile: fileName,
    match: config.match || {},
    block: config.block || null,
    keywordLabels: normalizeMapKeys(config.keywordLabels),
    keywordPhrases: normalizeMapKeys(config.keywordPhrases),
    captureRules: normalizeCaptureRules(config.captureRules),
    valueDescriptions: normalizeValueDescriptions(config.valueDescriptions)
  };

  if (normalized.match.startKeyword) {
    normalized.match.startKeyword = normalized.match.startKeyword.toUpperCase();
  }

  if (normalized.match.startPhrase) {
    normalized.match.startTokens = String(normalized.match.startPhrase)
      .trim()
      .toUpperCase()
      .split(/\s+/)
      .filter(Boolean);
  } else {
    normalized.match.startTokens = [];
  }

  if (normalized.block && normalized.block.endKeyword) {
    normalized.block = {
      ...normalized.block,
      endKeyword: String(normalized.block.endKeyword).toUpperCase()
    };
  }

  return normalized;
}

function normalizeMapKeys(map) {
  if (!map) {
    return {};
  }

  const output = {};
  for (const [key, value] of Object.entries(map)) {
    output[key.toUpperCase()] = value;
  }
  return output;
}

function normalizeCaptureRules(rules) {
  if (!Array.isArray(rules)) {
    return [];
  }

  return rules.map((rule) => {
    const after = rule.after || "";
    const afterTokens = after
      .trim()
      .toUpperCase()
      .split(/\s+/)
      .filter(Boolean);

    const stopTokensUpper = Array.isArray(rule.stopTokens)
      ? rule.stopTokens
          .map((token) => String(token).trim().toUpperCase())
          .filter(Boolean)
      : [];

    return {
      ...rule,
      afterTokens,
      capture: rule.capture || "next",
      stopTokensUpper
    };
  });
}

function normalizeValueDescriptions(desc) {
  if (!desc) {
    return {};
  }

  const output = {};
  for (const [key, map] of Object.entries(desc)) {
    output[key] = normalizeMapKeys(map);
  }
  return output;
}

module.exports = {
  loadConfigs
};
