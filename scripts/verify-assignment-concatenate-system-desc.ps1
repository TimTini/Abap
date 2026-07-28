#Requires -Version 5.1
param()
$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot
Set-Location $root

Write-Host "Running assignment/concatenate/sy-tabix check..."
node "$root\scripts\_verify-assignment-concatenate-system-desc-check.js"
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: node check"
  exit 1
}

Write-Host "Running npm run test:fast..."
npm run test:fast
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: test:fast"
  exit 1
}

Write-Host "Checking inline viewer freshness..."
uv run python "$root\scripts\build-inline-viewer.py" --check
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: inline viewer check"
  exit 1
}

Write-Host "PASS: verify-assignment-concatenate-system-desc.ps1"
exit 0
