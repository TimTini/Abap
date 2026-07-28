#Requires -Version 5.1
<#
.SYNOPSIS
  Verify Open SQL SELECT FIELDS / WHERE…INTO parse (deep sample + inline DATA/FIELD-SYMBOL).

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-select-open-sql-fields.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== Open SQL SELECT fields/where/into contracts =="
$env:ABAP_TEST_FOCUS = "statements"
$testOut = & node --test tests/parser-regression.statements.test.js 2>&1 | Out-String
$exitCode = $LASTEXITCODE
Remove-Item Env:ABAP_TEST_FOCUS -ErrorAction SilentlyContinue

if ($exitCode -ne 0) {
  Write-Host $testOut
  Write-Host "FAIL: parser statements suite"
  exit 1
}

$required = @(
  "select open sql fields where into from deep sample",
  "select classic single fields before from",
  "select classic fields into table where",
  "select open sql inline into data and field-symbol",
  "select for all entries captures itab and decl",
  "select for all entries host escape and corresponding table"
)

foreach ($name in $required) {
  if ($testOut -notmatch [regex]::Escape($name)) {
    Write-Host $testOut
    Write-Host "FAIL: expected subtest not found: $name"
    exit 1
  }
}

if ($testOut -match "fail [1-9]") {
  Write-Host $testOut
  Write-Host "FAIL: suite reported failures"
  exit 1
}

Write-Host "PASS: Open SQL SELECT checklist (deep sample + classic + inline INTO)"
exit 0
