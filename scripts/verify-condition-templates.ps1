#Requires -Version 5.1
<#
.SYNOPSIS
  Verify condition templates render left/right editable columns for SELECT/READ/LOOP/MODIFY/DELETE.

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/verify-condition-templates.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

function Invoke-FocusedViewerTest {
  param(
    [Parameter(Mandatory = $true)][string]$Focus,
    [Parameter(Mandatory = $true)][string]$File,
    [Parameter(Mandatory = $true)][string]$MustContain,
    [Parameter(Mandatory = $true)][string]$Label
  )
  Write-Host "== $Label =="
  $env:ABAP_TEST_FOCUS = $Focus
  $out = & node --test $File 2>&1 | Out-String
  $code = $LASTEXITCODE
  Remove-Item Env:ABAP_TEST_FOCUS -ErrorAction SilentlyContinue
  if ($code -ne 0) {
    Write-Host $out
    Write-Host "FAIL: $Label"
    exit 1
  }
  if ($out -notmatch [regex]::Escape($MustContain)) {
    Write-Host $out
    Write-Host "FAIL: expected subtest not found ($MustContain)"
    exit 1
  }
  if ($out -match "fail [1-9]") {
    Write-Host $out
    Write-Host "FAIL: suite reported failures ($Label)"
    exit 1
  }
  Write-Host "PASS: $Label"
}

Invoke-FocusedViewerTest `
  -Focus "template-multi-value-conditions" `
  -File "tests/viewer-contracts.template.test.js" `
  -MustContain "condition lists expand rows" `
  -Label "condition layout rows"

Invoke-FocusedViewerTest `
  -Focus "template-row-description-condition" `
  -File "tests/viewer-contracts.template.test.js" `
  -MustContain "template row description targets exact condition decls" `
  -Label "condition left/right edit"

Invoke-FocusedViewerTest `
  -Focus "template-configs" `
  -File "tests/viewer-contracts.config.test.js" `
  -MustContain "statement specific twenty cell templates" `
  -Label "hybrid defaults / 80-col"

Write-Host "ALL PASS: condition templates"
exit 0
