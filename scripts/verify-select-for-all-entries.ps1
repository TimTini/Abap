#Requires -Version 5.1
<#
.SYNOPSIS
  Verify SELECT FOR ALL ENTRIES variants: plain, @host, CORRESPONDING table.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-select-for-all-entries.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== parser unit test (SELECT FOR ALL ENTRIES variants) =="
$testOut = & node --test tests/parser-regression.statements.test.js 2>&1 | Out-String
if ($LASTEXITCODE -ne 0) {
  Write-Host $testOut
  Write-Host "FAIL: parser-regression.statements.test.js"
  exit 1
}
if ($testOut -notmatch "select for all entries captures itab and decl") {
  Write-Host $testOut
  Write-Host "FAIL: expected base FAE subtest not found"
  exit 1
}
if ($testOut -notmatch "select for all entries host escape and corresponding table") {
  Write-Host $testOut
  Write-Host "FAIL: expected host/CORRESPONDING subtest not found"
  exit 1
}
if ($testOut -match "fail [1-9]") {
  Write-Host $testOut
  Write-Host "FAIL: suite reported failures"
  exit 1
}
Write-Host "PASS: statements suite green (FAE + @host + CORRESPONDING)"

Write-Host "== live parse smoke =="
$smokeJs = @'
const path = require("path");
const repoRoot = path.resolve(__dirname, "..");
const { parseAbapText } = require(path.join(repoRoot, "shared", "abap-parser.js"));
const { loadConfigs } = require(path.join(repoRoot, "tests", "helpers", "config-loader.js"));
const configs = loadConfigs(path.join(repoRoot, "configs"));

function mustSelect(code) {
  const result = parseAbapText(code, configs, "smoke.abap");
  const select = (result.objects || []).find((o) => o && o.objectType === "SELECT");
  if (!select) {
    throw new Error("missing SELECT");
  }
  return select;
}

const plain = mustSelect([
  "DATA gt_src TYPE TABLE OF i.",
  "DATA gt_dst TYPE TABLE OF i.",
  "SELECT table_line FROM gt_dummy INTO TABLE gt_dst",
  "  FOR ALL ENTRIES IN gt_src",
  "  WHERE table_line = gt_src-table_line."
].join("\n"));
if (!plain.values.forAllEntries || plain.values.forAllEntries.declRef !== "gt_src") {
  console.error("FAIL plain FAE", plain.values.forAllEntries);
  process.exit(1);
}

const host = mustSelect([
  "DATA gt_src TYPE TABLE OF i.",
  "DATA gt_dst TYPE TABLE OF i.",
  "SELECT table_line FROM dbtab INTO TABLE @gt_dst",
  "  FOR ALL ENTRIES IN @gt_src",
  "  WHERE table_line = @gt_src-table_line."
].join("\n"));
const hostCond = host.extras.select.whereConditions[0];
if (host.values.forAllEntries.declRef !== "gt_src" || hostCond.rightOperandRef !== "gt_src-table_line") {
  console.error("FAIL host", host.values.forAllEntries, hostCond);
  process.exit(1);
}

const corr = mustSelect([
  "DATA gt_src TYPE TABLE OF i.",
  "DATA gt_dst TYPE TABLE OF i.",
  "SELECT carrid FROM spfli",
  "  INTO CORRESPONDING FIELDS OF TABLE gt_dst",
  "  FOR ALL ENTRIES IN gt_src",
  "  WHERE carrid = gt_src-carrid."
].join("\n"));
if (corr.values.intoTable.value !== "gt_dst" || corr.values.into) {
  console.error("FAIL corresponding", corr.values);
  process.exit(1);
}

const app = mustSelect([
  "DATA gt_src TYPE TABLE OF i.",
  "DATA gt_dst TYPE TABLE OF i.",
  "SELECT carrid FROM spfli",
  "  APPENDING CORRESPONDING FIELDS OF TABLE @gt_dst",
  "  FOR ALL ENTRIES IN @gt_src",
  "  WHERE carrid = @gt_src-carrid."
].join("\n"));
if (app.values.appendingTable.value !== "@gt_dst" || app.values.appendingTable.declRef !== "gt_dst") {
  console.error("FAIL appending corresponding", app.values.appendingTable);
  process.exit(1);
}

console.log("PASS smoke", {
  plainFae: plain.values.forAllEntries.value,
  hostPathRef: hostCond.rightOperandRef,
  corrInto: corr.values.intoTable.value,
  appInto: app.values.appendingTable.value
});
'@
$smokePath = Join-Path $repoRoot "scripts\_tmp-fae-smoke.js"
Set-Content -Path $smokePath -Value $smokeJs -Encoding UTF8
try {
  & node $smokePath
  if ($LASTEXITCODE -ne 0) {
    Write-Host "FAIL: live parse smoke"
    exit 1
  }
} finally {
  Remove-Item -Force -ErrorAction SilentlyContinue $smokePath
}

Write-Host "PASS: verify-select-for-all-entries"
exit 0
