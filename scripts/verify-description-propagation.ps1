#Requires -Version 5.1
<#
.SYNOPSIS
  Verify scope-safe description propagation and TYPE item fan-out.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-description-propagation.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

function Invoke-CheckedStep {
  param(
    [Parameter(Mandatory = $true)]
    [string]$Label,

    [Parameter(Mandatory = $true)]
    [scriptblock]$Command
  )

  Write-Host "== $Label =="
  & $Command
  if ($LASTEXITCODE -ne 0) {
    throw "$Label failed with exit code $LASTEXITCODE."
  }
}

Invoke-CheckedStep "Parser description propagation" {
  node tests/run.js parser --focus description-propagation
}

Invoke-CheckedStep "Viewer DATA and TYPE fan-out" {
  node tests/run.js viewer --focus data-catalog
}

Invoke-CheckedStep "Viewer local METHOD chains" {
  node tests/run.js viewer --focus local-method-source-chain
}

Invoke-CheckedStep "Template canonical description keys" {
  node tests/run.js viewer --focus template-provenance
}

Invoke-CheckedStep "Parser syntax" {
  node --check shared/abap-parser.js
}

Invoke-CheckedStep "Viewer source syntax" {
  $viewerFiles = @(
    "viewer/app.js",
    "viewer/app/descriptions/01-normalize-and-desc.js",
    "viewer/app/parser/01-parser-controller.js",
    "viewer/app/perform/01-perform-sources.js",
    "viewer/app/template/01-path-resolver.js"
  )
  foreach ($viewerFile in $viewerFiles) {
    node --check $viewerFile
    if ($LASTEXITCODE -ne 0) {
      throw "Syntax check failed for $viewerFile."
    }
  }
}

Invoke-CheckedStep "Generated configs freshness" {
  node scripts/build-viewer-configs.js --check
}

Invoke-CheckedStep "Inline Viewer freshness" {
  uv run python scripts/build-inline-viewer.py --check
}

Write-Host "PASS: description propagation verification"
