#!/usr/bin/env node
"use strict";

const fs = require("fs");
const path = require("path");

const DEFAULT_TAIL_COLS = 20;

function printUsage() {
  const usage = [
    "Usage:",
    "  node scripts/template-tool.js [options]",
    "",
    "Options:",
    "  --input <file>        Read tool script from file (default: stdin)",
    "  --output <file>       Write JSON output to file (default: stdout)",
    "  --base <file>         Base template JSON to merge into (optional)",
    "  --template <key>      Force template key (e.g. ASSIGNMENT)",
    "  --tail-cols <n>       Extra columns for row tail segment (default: 20)",
    "  --map-only            Output only template map (not full {version,templates})",
    "  --help                Show this help",
    "",
    "Tool line format:",
    "  [ASSIGNMENT]",
    "  A1.#dbeef4 = Append",
    "  H1 = {values.what.decl.finalDesc}",
    "",
    "Notes:",
    "  - Range expands from each anchor to the column before the next anchor on the same row.",
    "  - Last anchor on a row expands with --tail-cols.",
    "  - Auto-fix: unbalanced placeholder braces are balanced automatically.",
    ""
  ].join("\n");
  process.stderr.write(`${usage}\n`);
}

function parseArgs(argv) {
  const args = {
    input: "",
    output: "",
    base: "",
    template: "",
    tailCols: DEFAULT_TAIL_COLS,
    mapOnly: false,
    help: false
  };

  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (token === "--help" || token === "-h") {
      args.help = true;
      continue;
    }
    if (token === "--map-only") {
      args.mapOnly = true;
      continue;
    }
    if (token === "--input") {
      args.input = String(argv[i + 1] || "").trim();
      i += 1;
      continue;
    }
    if (token === "--output") {
      args.output = String(argv[i + 1] || "").trim();
      i += 1;
      continue;
    }
    if (token === "--base") {
      args.base = String(argv[i + 1] || "").trim();
      i += 1;
      continue;
    }
    if (token === "--template") {
      args.template = String(argv[i + 1] || "").trim().toUpperCase();
      i += 1;
      continue;
    }
    if (token === "--tail-cols") {
      const value = Number(argv[i + 1]);
      args.tailCols = Number.isFinite(value) && value > 0 ? Math.floor(value) : DEFAULT_TAIL_COLS;
      i += 1;
      continue;
    }

    throw new Error(`Unknown argument: ${token}`);
  }

  return args;
}

function normalizeAliasToken(value) {
  return String(value || "")
    .trim()
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .replace(/\s+/g, " ");
}

function parseCellRef(cellRef) {
  const raw = String(cellRef || "").trim().toUpperCase();
  const match = /^([A-Z]+)([1-9][0-9]*)$/.exec(raw);
  if (!match) {
    throw new Error(`Invalid cell ref "${cellRef}". Expected A1 style.`);
  }

  const letters = match[1];
  const row = Number(match[2]) || 0;
  let col = 0;
  for (let i = 0; i < letters.length; i += 1) {
    col = (col * 26) + (letters.charCodeAt(i) - 64);
  }

  if (!row || !col) {
    throw new Error(`Invalid cell ref "${cellRef}".`);
  }

  return { row, col, raw };
}

function colToLetters(colNumber) {
  let value = Number(colNumber) || 0;
  if (value < 1) {
    return "";
  }

  let out = "";
  while (value > 0) {
    const rem = (value - 1) % 26;
    out = String.fromCharCode(65 + rem) + out;
    value = Math.floor((value - 1) / 26);
  }
  return out;
}

function parseTemplateKeyLine(lineText) {
  const line = String(lineText || "").trim();
  if (!line) {
    return "";
  }

  const bracket = /^\[([A-Za-z0-9_-]+)\]$/.exec(line);
  if (bracket && bracket[1]) {
    return bracket[1].toUpperCase();
  }

  const named = /^(?:TEMPLATE|OBJECT|TYPE)\s*:?\s*([A-Za-z0-9_-]+)$/i.exec(line);
  if (named && named[1]) {
    return named[1].toUpperCase();
  }

  return "";
}

function parseAnchorLine(lineText, lineNumber) {
  const line = String(lineText || "").trim();
  const eqIndex = line.indexOf("=");
  if (eqIndex <= 0) {
    return { error: `Line ${lineNumber}: expected format CELL[.COLOR] = TEXT` };
  }

  const left = line.slice(0, eqIndex).trim();
  const right = line.slice(eqIndex + 1).trim();
  if (!left || !right) {
    return { error: `Line ${lineNumber}: missing cell or text value.` };
  }

  const leftMatch = /^([A-Za-z]+[1-9][0-9]*)(?:\.(\S+))?$/.exec(left);
  if (!leftMatch) {
    return { error: `Line ${lineNumber}: invalid anchor "${left}".` };
  }

  const cell = String(leftMatch[1] || "").toUpperCase();
  let row = 0;
  let col = 0;
  try {
    const cellRef = parseCellRef(cell);
    row = cellRef.row;
    col = cellRef.col;
  } catch (err) {
    return { error: `Line ${lineNumber}: ${err && err.message ? err.message : String(err)}` };
  }

  const background = String(leftMatch[2] || "").trim() || "default";
  return {
    row,
    col,
    cell,
    background,
    text: right,
    lineNo: lineNumber
  };
}

function createDiagnostics() {
  return { errors: [], warnings: [], infos: [] };
}

function pushDiag(diag, level, message) {
  const text = String(message || "").trim();
  if (!text) {
    return;
  }
  if (level === "error") {
    diag.errors.push(text);
    return;
  }
  if (level === "warning") {
    diag.warnings.push(text);
    return;
  }
  diag.infos.push(text);
}

function formatDiagnostics(diag) {
  const out = [];
  if (diag.errors.length) {
    out.push(`Errors (${diag.errors.length})`);
    for (const line of diag.errors) {
      out.push(`- ${line}`);
    }
  }
  if (diag.warnings.length) {
    out.push(`Warnings (${diag.warnings.length})`);
    for (const line of diag.warnings) {
      out.push(`- ${line}`);
    }
  }
  if (diag.infos.length) {
    out.push(`Info (${diag.infos.length})`);
    for (const line of diag.infos) {
      out.push(`- ${line}`);
    }
  }
  return out.join("\n").trim();
}

function autoFixBraceBalance(text) {
  const raw = String(text || "");
  let balance = 0;
  for (let i = 0; i < raw.length; i += 1) {
    const ch = raw[i];
    if (ch === "{") {
      balance += 1;
    } else if (ch === "}") {
      balance -= 1;
    }
  }

  if (balance === 0) {
    return { value: raw, fixed: false };
  }
  if (balance > 0) {
    return { value: `${raw}${"}".repeat(balance)}`, fixed: true };
  }
  return { value: `${"{".repeat(Math.abs(balance))}${raw}`, fixed: true };
}

function normalizeBackgroundToken(token, lineNo, diag) {
  const raw = String(token || "").trim();
  if (!raw) {
    return "default";
  }

  const normalized = normalizeAliasToken(raw);
  if (!normalized || normalized === "default") {
    return "default";
  }
  if (normalized === "mau xanh nhat") {
    return "mau xanh nhat";
  }
  if (normalized === "den") {
    return "den";
  }

  if (/^#(?:[0-9a-f]{3}|[0-9a-f]{6})$/i.test(raw)) {
    return raw.toLowerCase();
  }
  if (/^rgba?\(/i.test(raw)) {
    return raw;
  }
  if (/^[a-z][a-z0-9-]*$/i.test(raw)) {
    pushDiag(diag, "warning", `Line ${lineNo}: background "${raw}" treated as CSS color token.`);
    return raw;
  }

  pushDiag(diag, "warning", `Line ${lineNo}: uncommon background token "${raw}", keeping raw.`);
  return raw;
}

function createBaseStyle(background) {
  return {
    background: String(background || "default"),
    border: "outside-thin",
    font: "Calibri",
    "font color": "#111111",
    "font size": 10,
    "font family": "default",
    bold: false,
    italic: false,
    underline: false,
    merge: false,
    align: "left",
    valign: "top",
    wrap: false
  };
}

function compileToolScript(rawText, options) {
  const opts = options || {};
  const diag = createDiagnostics();
  const tailCols = Number.isFinite(Number(opts.tailCols))
    ? Math.max(1, Math.floor(Number(opts.tailCols)))
    : DEFAULT_TAIL_COLS;
  const forcedTemplate = String(opts.templateKey || "").trim().toUpperCase();

  const lines = String(rawText || "").split(/\r?\n/);
  let templateKey = forcedTemplate || "DEFAULT";
  const anchors = [];

  for (let i = 0; i < lines.length; i += 1) {
    const lineNo = i + 1;
    const line = String(lines[i] || "").trim();
    if (!line || line.startsWith("//") || line.startsWith(";")) {
      continue;
    }

    const candidateKey = parseTemplateKeyLine(line);
    if (candidateKey && !anchors.length && !forcedTemplate) {
      templateKey = candidateKey;
      pushDiag(diag, "info", `Template key set to ${templateKey}.`);
      continue;
    }

    const anchor = parseAnchorLine(line, lineNo);
    if (anchor.error) {
      pushDiag(diag, "error", anchor.error);
      continue;
    }

    const fixed = autoFixBraceBalance(anchor.text);
    if (fixed.fixed) {
      pushDiag(diag, "warning", `Line ${lineNo}: auto-fixed unbalanced braces for ${anchor.cell}.`);
    }
    anchor.text = fixed.value;
    anchor.background = normalizeBackgroundToken(anchor.background, lineNo, diag);

    anchors.push(anchor);
  }

  if (!anchors.length) {
    pushDiag(diag, "error", "No valid tool lines found.");
    return { templateKey, templateMap: null, diagnostics: diag };
  }

  const byRow = new Map();
  for (const anchor of anchors) {
    if (!byRow.has(anchor.row)) {
      byRow.set(anchor.row, []);
    }
    byRow.get(anchor.row).push(anchor);
  }

  const templateMap = {};
  for (const [row, rowAnchors] of byRow.entries()) {
    const dedupByCol = new Map();
    for (const anchor of rowAnchors) {
      if (dedupByCol.has(anchor.col)) {
        const prev = dedupByCol.get(anchor.col);
        pushDiag(diag, "warning", `Row ${row}: duplicate anchor at ${anchor.cell} (replace ${prev.cell}).`);
      }
      dedupByCol.set(anchor.col, anchor);
    }

    const originalOrder = Array.from(dedupByCol.values());
    const sorted = originalOrder.slice().sort((a, b) => a.col - b.col);
    const reordered = originalOrder.some((item, idx) => sorted[idx] && sorted[idx].cell !== item.cell);
    if (reordered) {
      pushDiag(diag, "warning", `Row ${row}: anchors auto-sorted left-to-right.`);
    }

    for (let i = 0; i < sorted.length; i += 1) {
      const current = sorted[i];
      const next = sorted[i + 1] || null;
      const endCol = next ? Math.max(current.col, next.col - 1) : (current.col + tailCols);
      const endLetters = colToLetters(endCol);
      if (!endLetters) {
        pushDiag(diag, "error", `Row ${row}: cannot build range end for ${current.cell}.`);
        continue;
      }

      const rangeKey = `${current.cell}:${endLetters}${row}`;
      templateMap[rangeKey] = createBaseStyle(current.background);
      templateMap[current.cell] = { text: current.text };
    }
  }

  return { templateKey, templateMap, diagnostics: diag };
}

function loadJsonFile(filePath) {
  const abs = path.resolve(filePath);
  const raw = fs.readFileSync(abs, "utf8");
  return JSON.parse(raw);
}

function buildOutputConfig(compiled, options) {
  const opts = options || {};
  if (!compiled || !compiled.templateMap) {
    return null;
  }

  if (opts.mapOnly) {
    return compiled.templateMap;
  }

  let baseConfig = { version: 1, templates: {} };
  if (opts.basePath) {
    const parsed = loadJsonFile(opts.basePath);
    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
      throw new Error("Base config must be a JSON object.");
    }
    const templates = parsed.templates && typeof parsed.templates === "object" && !Array.isArray(parsed.templates)
      ? parsed.templates
      : {};
    baseConfig = {
      ...parsed,
      version: 1,
      templates: { ...templates }
    };
  }

  baseConfig.templates[compiled.templateKey || "DEFAULT"] = compiled.templateMap;
  return baseConfig;
}

function readInputText(inputPath) {
  if (inputPath) {
    return fs.readFileSync(path.resolve(inputPath), "utf8");
  }
  return fs.readFileSync(0, "utf8");
}

function writeOutputText(outputPath, text) {
  if (outputPath) {
    fs.writeFileSync(path.resolve(outputPath), text, "utf8");
    return;
  }
  process.stdout.write(text);
}

function main() {
  let args;
  try {
    args = parseArgs(process.argv.slice(2));
  } catch (err) {
    process.stderr.write(`${err.message}\n\n`);
    printUsage();
    process.exitCode = 2;
    return;
  }

  if (args.help) {
    printUsage();
    return;
  }

  let rawInput = "";
  try {
    rawInput = readInputText(args.input);
  } catch (err) {
    process.stderr.write(`Failed to read input: ${err && err.message ? err.message : String(err)}\n`);
    process.exitCode = 2;
    return;
  }

  const compiled = compileToolScript(rawInput, {
    templateKey: args.template,
    tailCols: args.tailCols
  });

  const diagnosticText = formatDiagnostics(compiled.diagnostics);
  if (diagnosticText) {
    process.stderr.write(`${diagnosticText}\n`);
  }

  if (compiled.diagnostics.errors.length) {
    process.exitCode = 1;
    return;
  }

  let outputObj;
  try {
    outputObj = buildOutputConfig(compiled, {
      mapOnly: args.mapOnly,
      basePath: args.base
    });
  } catch (err) {
    process.stderr.write(`Failed to build output config: ${err && err.message ? err.message : String(err)}\n`);
    process.exitCode = 2;
    return;
  }

  const outputJson = `${JSON.stringify(outputObj, null, 2)}\n`;
  try {
    writeOutputText(args.output, outputJson);
  } catch (err) {
    process.stderr.write(`Failed to write output: ${err && err.message ? err.message : String(err)}\n`);
    process.exitCode = 2;
  }
}

main();
