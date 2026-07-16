# Verify: Description Target hide + PERFORM source + CHECK sample
# Usage (from repo root):
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts\verify-perform-desc-target.ps1

$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot
Set-Location $root

function Invoke-Step([string]$Label, [scriptblock]$Action) {
  Write-Host "==> $Label"
  & $Action
  if ($LASTEXITCODE -ne 0 -and $null -ne $LASTEXITCODE) {
    Write-Host "FAIL: $Label (exit $LASTEXITCODE)"
    exit $LASTEXITCODE
  }
  Write-Host "PASS: $Label"
}

Invoke-Step "sync SAMPLE_ABAP" { node scripts/sync-default-sample.js }
Invoke-Step "build-runtime-bundles" { node scripts/build-runtime-bundles.js }

$python = @(
  "$env:USERPROFILE\.local\bin\python3.12.exe",
  "$env:USERPROFILE\.local\bin\python3.11.exe",
  "python"
) | Where-Object { $_ -eq "python" -or (Test-Path $_) } | Select-Object -First 1

if (-not $python) {
  Write-Host "FAIL: Python not found for build-inline-viewer.py"
  exit 1
}

Invoke-Step "build-inline-viewer ($python)" { & $python scripts/build-inline-viewer.py }
Invoke-Step "node --check viewer/app.js" { node --check viewer/app.js }
Invoke-Step "node --check shared/abap-parser.js" { node --check shared/abap-parser.js }
Invoke-Step "parser-regression" { node tests/parser-regression.js }
Invoke-Step "viewer-contracts" { node tests/viewer-contracts.js }

Write-Host ""
Write-Host "Manual QA (viewer inputText sample):"
Write-Host "  1. Open viewer/index.html — sample has CHECK block (frm_chk_*)."
Write-Host "  2. Template: CHK Root once — Description has NO Target select."
Write-Host "  3. PERFORM frm_chk_multi — badge ⇄ 2 nguồn."
Write-Host "  4. IF CHK Left/Right — Target select may appear (left vs right)."
Write-Host "ALL PASS"
