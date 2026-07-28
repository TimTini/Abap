#Requires -Version 5.1
<#
.SYNOPSIS
  Verify review remediation: LOOP FROM INDEX pairing, catalog STRUCT_FIELD no-dupe, builds.

.DESCRIPTION
  Re-runnable PASS/FAIL checks for slices A–C from the review remediation plan.
  Run from repo root:
    powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-review-from-index-catalog.ps1
#>
$ErrorActionPreference = "Stop"
$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

function Write-Step([string]$Name) {
  Write-Host ""
  Write-Host "=== $Name ===" -ForegroundColor Cyan
}

function Assert-ExitZero([string]$Label, [scriptblock]$Command) {
  Write-Step $Label
  & $Command
  if ($LASTEXITCODE -ne 0) {
    Write-Host "FAIL: $Label (exit $LASTEXITCODE)" -ForegroundColor Red
    exit $LASTEXITCODE
  }
  Write-Host "PASS: $Label" -ForegroundColor Green
}

Assert-ExitZero "build-viewer-configs --check" {
  node scripts/build-viewer-configs.js --check
}

Assert-ExitZero "parser statements: loop from index + read flags + concatenate" {
  node --test --test-name-pattern "loop at from index|read table binary search|concatenate statement" tests/parser-regression.statements.test.js
}

Assert-ExitZero "viewer: from-index pairing + remapped struct-field comment" {
  node --test --test-name-pattern "loop from index template pairs|perform remapped struct field" tests/viewer-contracts.template.test.js
}

Assert-ExitZero "build-inline-viewer --check" {
  uv run python scripts/build-inline-viewer.py --check
}

Assert-ExitZero "npm run test:fast" {
  npm run test:fast
}

Write-Host ""
Write-Host "ALL PASS: scripts/verify-review-from-index-catalog.ps1" -ForegroundColor Green
exit 0
