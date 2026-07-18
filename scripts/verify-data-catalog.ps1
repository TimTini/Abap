param(
  [string]$PythonPath = ""
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot

function Invoke-CheckedStep {
  param(
    [string]$Label,
    [scriptblock]$Action
  )

  Write-Host "[RUN] $Label"
  & $Action
  if ($LASTEXITCODE -ne 0) {
    throw "$Label failed with exit code $LASTEXITCODE."
  }
  Write-Host "[PASS] $Label"
}

function Resolve-PythonExecutable {
  if ($PythonPath) {
    return $PythonPath
  }

  $pythonCommand = Get-Command python -ErrorAction SilentlyContinue
  if ($pythonCommand -and $pythonCommand.Source -notlike "*WindowsApps*") {
    return $pythonCommand.Source
  }

  $bundledPython = Join-Path $env:USERPROFILE ".cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe"
  if (Test-Path -LiteralPath $bundledPython) {
    return $bundledPython
  }

  throw "Python executable not found. Pass -PythonPath <path-to-python.exe>."
}

Push-Location $repoRoot
try {
  Invoke-CheckedStep "Build runtime bundles" { node scripts/build-runtime-bundles.js }
  $python = Resolve-PythonExecutable
  Invoke-CheckedStep "Build inline viewer" { & $python scripts/build-inline-viewer.py }
  Invoke-CheckedStep "Data catalog contracts" { node tests/viewer-contracts.js data-catalog }
  Invoke-CheckedStep "Data PERFORM trace contracts" { node tests/viewer-contracts.js data-perform-trace }
  Invoke-CheckedStep "Parser regression" { node tests/parser-regression.js }
  Invoke-CheckedStep "Runtime bundle contracts" { node tests/runtime-bundle-contracts.js }
  Invoke-CheckedStep "Parser syntax" { node --check shared/abap-parser.js }
  Invoke-CheckedStep "Viewer syntax" { node --check viewer/app.js }

  Write-Host ""
  Write-Host "Manual smoke checklist:"
  Write-Host "  1. Hard reload viewer/index.html and parse the default sample."
  Write-Host "  2. Open Data; verify Global and Subroutine / FORM groups."
  Write-Host "  3. Verify a nested FORM parameter shows local <- intermediate <- root."
  Write-Host "  4. Edit a traced row, open Template, and verify the same description."
  Write-Host "  5. Switch PERFORM source; verify chain-specific overrides do not bleed."
  Write-Host ""
  Write-Host "verify-data-catalog: PASS"
}
finally {
  Pop-Location
}
