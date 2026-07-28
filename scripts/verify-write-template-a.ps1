#Requires -Version 5.1
<#
.SYNOPSIS
  Verify WRITE template approach A: split AT/LENGTH rows and clean newline-only /.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-write-template-a.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== WRITE template message-write contract =="
$env:ABAP_TEST_FOCUS = "message-write"
$testOut = & node --test tests/viewer-contracts.template.test.js 2>&1 | Out-String
$code = $LASTEXITCODE
Remove-Item Env:ABAP_TEST_FOCUS -ErrorAction SilentlyContinue

if ($code -ne 0) {
  Write-Host $testOut
  Write-Host "FAIL: message-write contract"
  exit 1
}
if ($testOut -notmatch "message and write viewer contracts") {
  Write-Host $testOut
  Write-Host "FAIL: expected message-write subtest not found"
  exit 1
}
if ($testOut -match "fail [1-9]") {
  Write-Host $testOut
  Write-Host "FAIL: suite reported failures"
  exit 1
}

Write-Host "PASS: WRITE template A (split AT/LENGTH + clean /)"
exit 0
