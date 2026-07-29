#Requires -Version 5.1
<#
.SYNOPSIS
  Smoke-check Chain Dot Popover demo (files + fixture shape + HTML wiring).

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-chain-dot-popover.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

function Fail([string]$Message) {
  Write-Host "FAIL: $Message"
  exit 1
}

Write-Host "== Chain Dot Popover demo =="

$htmlPath = Join-Path $repoRoot "demo\chain-dot-popover.html"
$fixturePath = Join-Path $repoRoot "demo\chain-dot-popover.fixture.js"
$specPath = Join-Path $repoRoot "docs\superpowers\specs\2026-07-28-chain-dot-popover-demo-design.md"
$planPath = Join-Path $repoRoot "docs\superpowers\plans\2026-07-28-chain-dot-popover-demo.md"

if (-not (Test-Path -LiteralPath $htmlPath)) { Fail "missing $htmlPath" }
if (-not (Test-Path -LiteralPath $fixturePath)) { Fail "missing $fixturePath" }
if (-not (Test-Path -LiteralPath $specPath)) { Fail "missing $specPath" }
if (-not (Test-Path -LiteralPath $planPath)) { Fail "missing $planPath" }

$html = Get-Content -LiteralPath $htmlPath -Raw -Encoding UTF8
if ($html -notmatch 'chain-dot-popover\.fixture\.js') {
  Fail "HTML does not load chain-dot-popover.fixture.js"
}
if ($html -notmatch 'chain-token') {
  Fail "HTML missing chain-token class"
}
if ($html -notmatch 'hop-dot') {
  Fail "HTML missing hop-dot class"
}
if ($html -notmatch 'chain-edges') {
  Fail "HTML missing chain-edges SVG connectors"
}
if ($html -notmatch 'computeBranchLayout|applyBranchLayout') {
  Fail "HTML missing branch layout to separate sibling dots"
}
if ($html -notmatch 'id="inputPanel"') {
  Fail "HTML missing Input-like panel shell"
}

$checkJs = @"
const fs = require('fs');
const path = require('path');
const vm = require('vm');

const fixturePath = path.join(process.cwd(), 'demo', 'chain-dot-popover.fixture.js');
const code = fs.readFileSync(fixturePath, 'utf8');
const sandbox = { window: {}, globalThis: {} };
sandbox.window = sandbox;
sandbox.globalThis = sandbox;
vm.runInNewContext(code, sandbox);

const fixture = sandbox.ChainDotPopoverFixture;
if (!fixture) throw new Error('ChainDotPopoverFixture missing');
if (typeof fixture.sourceText !== 'string' || !fixture.sourceText.includes('gs_request')) {
  throw new Error('sourceText missing gs_request');
}
if (!Array.isArray(fixture.chains) || fixture.chains.length < 1) {
  throw new Error('chains empty');
}
const gs = fixture.chains.find((c) => c.rootId === 'gs_request');
if (!gs) throw new Error('chain gs_request missing');
if (!Array.isArray(gs.nodes) || gs.nodes.length < 2) {
  throw new Error('gs_request needs root + hops');
}
const root = gs.nodes.find((n) => n.role === 'root');
const hops = gs.nodes.filter((n) => n.role === 'hop');
if (!root) throw new Error('gs_request root missing');
if (hops.length < 1) throw new Error('gs_request hops missing');
if (!Array.isArray(gs.tokenRanges) || gs.tokenRanges.length < 1) {
  throw new Error('gs_request tokenRanges missing');
}
const cycleLink = gs.nodes.some((n) => Array.isArray(n.links) && n.links.some((l) => l.kind === 'cycle'));
if (!cycleLink) throw new Error('gs_request should demo a cycle link');
const branchKids = gs.nodes.filter((n) => n.parentId === root.id);
if (branchKids.length < 2) throw new Error('gs_request root should branch to 2+ hops');
console.log('PASS fixture: chains=' + fixture.chains.length + ' gs_request hops=' + hops.length + ' branches=' + branchKids.length);
"@

$tmpCheck = Join-Path $env:TEMP "verify-chain-dot-popover-check.js"
Set-Content -LiteralPath $tmpCheck -Value $checkJs -Encoding UTF8
try {
  & node $tmpCheck
  if ($LASTEXITCODE -ne 0) {
    Fail "fixture node check failed (exit $LASTEXITCODE)"
  }
}
finally {
  Remove-Item -LiteralPath $tmpCheck -Force -ErrorAction SilentlyContinue
}

Write-Host "PASS: Chain Dot Popover demo files + fixture OK"
Write-Host "Open: file:///$($htmlPath.Replace('\','/'))"
exit 0
