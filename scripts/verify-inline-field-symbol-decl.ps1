#Requires -Version 5.1
<#
.SYNOPSIS
  Repro / verify inline FIELD-SYMBOL ASSIGNING binds decl correctly.

.DESCRIPTION
  Root cause was extractFirstIdentifier treating FIELD-SYMBOL(...) as a
  structure field path (FIELD-SYMBOL) before inline-decl extraction.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-inline-field-symbol-decl.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== parser unit test (inline FIELD-SYMBOL) =="
$testOut = & node --test tests/parser-regression.statements.test.js 2>&1 | Out-String
if ($LASTEXITCODE -ne 0) {
  Write-Host $testOut
  Write-Host "FAIL: parser-regression.statements.test.js"
  exit 1
}
if ($testOut -notmatch "inline field-symbol assigning binds decl") {
  Write-Host $testOut
  Write-Host "FAIL: expected focused subtest not found in output"
  exit 1
}
if ($testOut -match "fail [1-9]") {
  Write-Host $testOut
  Write-Host "FAIL: suite reported failures"
  exit 1
}
Write-Host "PASS: statements suite green (includes inline FIELD-SYMBOL decl bind)"

Write-Host "== live parse smoke =="
$smokeJs = @'
const path = require("path");
const repoRoot = path.resolve(__dirname, "..");
const { parseAbapText } = require(path.join(repoRoot, "shared", "abap-parser.js"));
const { loadConfigs } = require(path.join(repoRoot, "tests", "helpers", "config-loader.js"));
const configs = loadConfigs(path.join(repoRoot, "configs"));
const code = [
  "DATA gt TYPE TABLE OF i.",
  "READ TABLE gt ASSIGNING FIELD-SYMBOL(<ls_x>) INDEX 1."
].join("\n");
const result = parseAbapText(code, configs, "smoke.abap");
const read = (result.objects || []).find((o) => o && o.objectType === "READ_TABLE");
const assigning = read && read.values && read.values.assigning;
if (!assigning || assigning.declRef !== "<ls_x>" || !assigning.decl || assigning.decl.name !== "<ls_x>") {
  console.error("FAIL smoke", assigning);
  process.exit(1);
}
console.log("PASS smoke", { value: assigning.value, declRef: assigning.declRef, declType: assigning.decl.objectType });
'@
$smokePath = Join-Path $repoRoot "scripts\_tmp-inline-fs-smoke.js"
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

Write-Host "PASS: verify-inline-field-symbol-decl"
exit 0
