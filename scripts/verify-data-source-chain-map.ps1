#Requires -Version 5.1
<#
.SYNOPSIS
  Smoke-check Data Source Chain Map demo (files + full Viewer sample + fixture).

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-data-source-chain-map.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

function Fail([string]$Message) {
  Write-Host "FAIL: $Message"
  exit 1
}

Write-Host "== Data Source Chain Map demo =="

$htmlPath = Join-Path $repoRoot "demo\data-source-chain-map.html"
$fixturePath = Join-Path $repoRoot "demo\data-source-chain-map.fixture.js"
$samplePath = Join-Path $repoRoot "demo\data-source-chain-map.sample.js"
$specPath = Join-Path $repoRoot "docs\superpowers\specs\2026-07-27-data-source-chain-map-demo-design.md"
$syncScript = Join-Path $repoRoot "scripts\sync-data-source-chain-map-sample.js"
$buildScript = Join-Path $repoRoot "scripts\build-data-source-chain-map-fixture.js"
$checkScript = Join-Path $repoRoot "scripts\verify-data-source-chain-map-check.js"

if (-not (Test-Path -LiteralPath $htmlPath)) { Fail "missing $htmlPath" }
if (-not (Test-Path -LiteralPath $fixturePath)) { Fail "missing $fixturePath" }
if (-not (Test-Path -LiteralPath $samplePath)) {
  Fail "missing $samplePath - run node scripts/sync-data-source-chain-map-sample.js"
}
if (-not (Test-Path -LiteralPath $specPath)) { Fail "missing $specPath" }
if (-not (Test-Path -LiteralPath $syncScript)) { Fail "missing $syncScript" }
if (-not (Test-Path -LiteralPath $buildScript)) { Fail "missing $buildScript" }
if (-not (Test-Path -LiteralPath $checkScript)) { Fail "missing $checkScript" }

& node $syncScript --check
if ($LASTEXITCODE -ne 0) {
  Fail "sample.js stale vs examples/deep_form_demo.abap"
}

& node $buildScript --check
if ($LASTEXITCODE -ne 0) {
  Fail "fixture.js stale vs parser graph - run node scripts/build-data-source-chain-map-fixture.js"
}

$html = Get-Content -LiteralPath $htmlPath -Raw -Encoding UTF8
if ($html -notmatch "data-source-chain-map\.sample\.js") {
  Fail "HTML does not load sample script"
}
if ($html -notmatch "data-source-chain-map\.fixture\.js") {
  Fail "HTML does not load fixture script"
}
if ($html -notmatch "id=`"mapSvg`"") {
  Fail "HTML missing mapSvg"
}
if ($html -notmatch "code-token") {
  Fail "HTML missing code-token interaction"
}

$out = & node $checkScript 2>&1 | Out-String
if ($LASTEXITCODE -ne 0) {
  Write-Host $out
  Fail "fixture/sample node check"
}

Write-Host $out.Trim()
Write-Host "PASS: Data Source Chain Map demo"
exit 0
