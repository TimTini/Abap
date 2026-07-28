#Requires -Version 5.1
<#
.SYNOPSIS
  Re-verify default deep sample: sync check, demo contract, Open SQL matrix, parse+desc smoke.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-deep-sample-parse-desc.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== Sync SAMPLE_ABAP check =="
& node scripts/sync-default-sample.js --check
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: SAMPLE_ABAP out of sync with examples/deep_form_demo.abap"
  exit 1
}

Write-Host "== Demo regression contract =="
$env:ABAP_TEST_FOCUS = "demo"
$demoOut = & node --test tests/parser-regression.demo.test.js 2>&1 | Out-String
$demoExit = $LASTEXITCODE
Remove-Item Env:ABAP_TEST_FOCUS -ErrorAction SilentlyContinue
if ($demoExit -ne 0) {
  Write-Host $demoOut
  Write-Host "FAIL: demo regression"
  exit 1
}
if ($demoOut -notmatch [regex]::Escape("default flight demo contract")) {
  Write-Host $demoOut
  Write-Host "FAIL: expected demo subtest not found"
  exit 1
}

Write-Host "== Open SQL deep-sample + classic contracts =="
$env:ABAP_TEST_FOCUS = "statements"
$stOut = & node --test tests/parser-regression.statements.test.js 2>&1 | Out-String
$stExit = $LASTEXITCODE
Remove-Item Env:ABAP_TEST_FOCUS -ErrorAction SilentlyContinue
if ($stExit -ne 0) {
  Write-Host $stOut
  Write-Host "FAIL: statements suite"
  exit 1
}
foreach ($name in @(
  "select open sql fields where into from deep sample",
  "select classic single fields before from",
  "select classic fields into table where"
)) {
  if ($stOut -notmatch [regex]::Escape($name)) {
    Write-Host $stOut
    Write-Host "FAIL: expected subtest not found: $name"
    exit 1
  }
}

Write-Host "== Parse + description smoke (JS) =="
& node scripts/verify-deep-sample-parse-desc.js
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: parse+desc smoke"
  exit 1
}

Write-Host "PASS: deep sample parse + description verification"
exit 0
