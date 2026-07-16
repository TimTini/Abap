# Verify: natural flight sample, source-scoped PERFORM descriptions, APPEND, and copy
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

Invoke-Step "sync SAMPLE_ABAP" { rtk node scripts/sync-default-sample.js }
Invoke-Step "build-viewer-configs" { rtk node scripts/build-viewer-configs.js }
Invoke-Step "build-runtime-bundles" { rtk node scripts/build-runtime-bundles.js }

$python = @(
  "$env:USERPROFILE\.local\bin\python3.12.exe",
  "$env:USERPROFILE\.local\bin\python3.11.exe",
  "$env:USERPROFILE\.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe",
  "python"
) | Where-Object { $_ -eq "python" -or (Test-Path $_) } | Select-Object -First 1

if (-not $python) {
  Write-Host "FAIL: Python not found for build-inline-viewer.py"
  exit 1
}

Invoke-Step "build-inline-viewer ($python)" { & $python scripts/build-inline-viewer.py }
Invoke-Step "node --check viewer/app.js" { rtk node --check viewer/app.js }
Invoke-Step "node --check shared/abap-parser.js" { rtk node --check shared/abap-parser.js }
Invoke-Step "parser-regression" { rtk node tests/parser-regression.js }
Invoke-Step "viewer-contracts" { rtk node tests/viewer-contracts.js }
Invoke-Step "git diff --check" { rtk git diff --check }

Write-Host ""
Write-Host "Manual QA (viewer default flight sample):"
Write-Host "  1. Hard reload viewer/index.html and parse REPORT zflight_operations_overview."
Write-Host "  2. FORM frm_validate_request must show exactly 3 sources."
Write-Host "  3. Select source 3 and edit the request description to 'Source 3 only'."
Write-Host "  4. Switch to sources 1 and 2: neither description may change."
Write-Host "  5. Return to source 3: its override must be restored; Clear removes only source 3."
Write-Host "  6. Inspect nested frm_enrich_flight calls for availability and status ancestry."
Write-Host "  7. APPEND LINES OF must show source, FROM, range TO, STEP, USING KEY, target TO once each."
Write-Host "  8. Ctrl/Shift-select 2-3 Template blocks, Copy Selected, and paste into Excel."
Write-Host "     Confirm exactly one worksheet row is blank between adjacent templates."
Write-Host "ALL PASS"
