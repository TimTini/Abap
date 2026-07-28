# Verify bare itab components bind as STRUCT_FIELD (SORT/READ/LOOP/MODIFY)
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-itab-component-desc.ps1
#
# PASS = exit 0; FAIL = exit 1

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

Write-Host "== verify-itab-component-desc =="

Write-Host "[1/3] Condition lists bind itab components"
node tests/run.js viewer template-multi-value-conditions
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: template-multi-value-conditions"
  exit 1
}

Write-Host "[2/3] SORT BY / TRANSPORTING raw lists editable"
node tests/run.js viewer template-multi-value-safe-lists
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: template-multi-value-safe-lists"
  exit 1
}

Write-Host "[3/3] READ condition description targets distinct itab fields"
node tests/run.js viewer template-row-description-condition
if ($LASTEXITCODE -ne 0) {
  Write-Host "FAIL: template-row-description-condition"
  exit 1
}

Write-Host "PASS: itab component descriptions"
exit 0
