#Requires -Version 5.1
<#
.SYNOPSIS
  Smoke-check all chain-lineage UX demos (split / drawer / chips / hybrid + common).

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-chain-lineage-ux.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

function Fail([string]$Message) {
  Write-Host "FAIL: $Message"
  exit 1
}

Write-Host "== Chain Lineage UX demos =="

$files = @(
  "demo\chain-dot-popover.fixture.js",
  "demo\chain-lineage-common.js",
  "demo\chain-lineage-index.html",
  "demo\chain-lineage-split.html",
  "demo\chain-lineage-drawer.html",
  "demo\chain-lineage-chips.html",
  "demo\chain-lineage-hybrid.html"
)

foreach ($rel in $files) {
  $path = Join-Path $repoRoot $rel
  if (-not (Test-Path -LiteralPath $path)) {
    Fail "missing $path"
  }
}

$htmlChecks = @{
  "demo\chain-lineage-split.html" = @("chain-lineage-common.js", "dagSvg", "workspace")
  "demo\chain-lineage-drawer.html" = @("chain-lineage-common.js", "drawer", "dagSvg")
  "demo\chain-lineage-chips.html" = @("chain-lineage-common.js", "inline-chain", "chip")
  "demo\chain-lineage-hybrid.html" = @("chain-lineage-common.js", "chainComplexity", "sheet", "popover")
  "demo\chain-lineage-index.html" = @("chain-lineage-split.html", "chain-lineage-drawer.html", "chain-lineage-chips.html", "chain-lineage-hybrid.html")
}

foreach ($rel in $htmlChecks.Keys) {
  $path = Join-Path $repoRoot $rel
  $html = Get-Content -LiteralPath $path -Raw -Encoding UTF8
  foreach ($needle in $htmlChecks[$rel]) {
    if ($html -notmatch [regex]::Escape($needle)) {
      Fail "$rel missing expected marker: $needle"
    }
  }
}

$checkJs = @"
const fs = require('fs');
const path = require('path');
const vm = require('vm');

const sandbox = { window: {}, globalThis: {} };
sandbox.window = sandbox;
sandbox.globalThis = sandbox;
vm.runInNewContext(fs.readFileSync(path.join('demo', 'chain-dot-popover.fixture.js'), 'utf8'), sandbox);
vm.runInNewContext(fs.readFileSync(path.join('demo', 'chain-lineage-common.js'), 'utf8'), sandbox);

const L = sandbox.ChainLineage;
const fx = L.fixture();
if (!fx) throw new Error('fixture missing');
if (!L.renderDagSvg || !L.pathChipsHtml || !L.chainComplexity) {
  throw new Error('common API incomplete');
}
const gs = L.findChain('gs_request');
const cx = L.chainComplexity(gs);
if (cx.isSimple) throw new Error('gs_request should be complex');
if (cx.branchKids < 2) throw new Error('gs_request should branch');
const carrier = L.findChain('gv_default_carrier_name');
const cx2 = L.chainComplexity(carrier);
if (!cx2.isSimple) throw new Error('gv_default_carrier_name should be simple');
console.log('PASS common: chains=' + fx.chains.length + ' gs.branches=' + cx.branchKids + ' carrier.simple=' + cx2.isSimple);
"@

$tmp = Join-Path $env:TEMP "verify-chain-lineage-ux-check.js"
Set-Content -LiteralPath $tmp -Value $checkJs -Encoding UTF8
try {
  & node $tmp
  if ($LASTEXITCODE -ne 0) { Fail "node check failed" }
}
finally {
  Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue
}

Write-Host "PASS: Chain Lineage UX demos OK"
Write-Host "Open: file:///$($repoRoot.Replace('\','/'))/demo/chain-lineage-index.html"
exit 0
