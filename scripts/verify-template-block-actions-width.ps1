#Requires -Version 5.1
<#
.SYNOPSIS
  Verify Template title-row icon actions stay width-capped and .template-block-actions is gone.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-template-block-actions-width.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

$cssPath = Join-Path $repoRoot "viewer\styles\viewer.css"
$css = Get-Content -Raw -LiteralPath $cssPath

if ($css -match '\.template-block-actions\s*\{') {
  Write-Host "FAIL: .template-block-actions still present in viewer.css"
  exit 1
}
if ($css -notmatch '\.template-block-title-actions\s*\{[^}]*max-width:\s*100%') {
  Write-Host "FAIL: .template-block-title-actions missing max-width: 100%"
  exit 1
}
if ($css -notmatch '\.template-block-title-actions\s*\{[^}]*min-width:\s*0') {
  Write-Host "FAIL: .template-block-title-actions missing min-width: 0"
  exit 1
}
if ($css -notmatch '\.perform-source-picker\s*\{[^}]*min-width:\s*0') {
  Write-Host "FAIL: .perform-source-picker missing min-width: 0"
  exit 1
}

Write-Host "== perform-source-picker CSS width contract =="
$env:ABAP_TEST_FOCUS = "perform-source-picker"
$testOut = & node --test tests/viewer-contracts.perform.test.js 2>&1 | Out-String
$code = $LASTEXITCODE
Remove-Item Env:ABAP_TEST_FOCUS -ErrorAction SilentlyContinue

if ($code -ne 0) {
  Write-Host $testOut
  Write-Host "FAIL: perform-source-picker contract"
  exit 1
}
if ($testOut -notmatch "template block title actions CSS caps intrinsic width") {
  Write-Host $testOut
  Write-Host "FAIL: expected title-actions width subtest not found"
  exit 1
}
if ($testOut -match "fail [1-9]") {
  Write-Host $testOut
  Write-Host "FAIL: suite reported failures"
  exit 1
}

Write-Host "PASS: template-block-title-actions width caps (CSS + perform-source-picker)"
exit 0
