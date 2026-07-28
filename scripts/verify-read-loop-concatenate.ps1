# Verify READ / LOOP FROM INDEX / CONCATENATE parse fixes
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-read-loop-concatenate.ps1
#
# PASS = exit 0; FAIL = exit 1

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== verify-read-loop-concatenate =="

Write-Host "[1/3] Node syntax check shared/abap-parser.js"
node --check shared/abap-parser.js
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: node --check shared/abap-parser.js"
  exit 1
}

Write-Host "[2/3] Parser statements suite"
node tests/run.js parser statements
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: parser statements"
  exit 1
}

Write-Host "[3/3] Smoke fixture script"
node scripts/_verify-read-loop-concatenate-check.js
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: smoke fixture"
  exit 1
}

Write-Host "PASS: READ flags + LOOP FROM INDEX + CONCATENATE"
exit 0
