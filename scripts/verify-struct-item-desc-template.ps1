# Verify struct item descriptions survive PERFORM template remap
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-struct-item-desc-template.ps1
#
# PASS = exit 0; FAIL = exit 1

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== verify-struct-item-desc-template =="

Write-Host "[1/2] Focused viewer contract"
node tests/run.js viewer struct-field-finaldesc
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: struct-field-finaldesc"
  exit 1
}

Write-Host "[2/2] Smoke remapped WRITE keeps item comment"
node scripts/_verify-struct-item-desc-template-check.js
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: smoke check"
  exit 1
}

Write-Host "PASS: struct item desc in template"
exit 0
