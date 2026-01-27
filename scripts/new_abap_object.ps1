Param(
  [string]$Id,
  [string]$Label,
  [ValidateSet("statement", "callEdge")] [string]$Kind = "statement",
  [string]$ParseKind = "regex",
  [string]$Regex,
  [string]$Flags = "i",
  [string]$PrimaryField = "value",
  [string]$TemplateId,
  [switch]$InlineTemplate,
  [string]$TemplateFile,
  [switch]$Insert
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Read-IfEmpty([string]$value, [string]$prompt, [string]$defaultValue = "") {
  if ($value -and $value.Trim()) { return $value.Trim() }
  $suffix = ""
  if ($defaultValue) { $suffix = " (default: $defaultValue)" }
  $input = Read-Host "$prompt$suffix"
  if (-not $input) { return $defaultValue }
  return $input.Trim()
}

function Escape-JsString([string]$s) {
  $out = $s -replace "\\", "\\\\"
  $out = $out -replace "`"", "\\`""
  return $out
}

function Ensure-Dir([string]$path) {
  if (-not (Test-Path -LiteralPath $path)) {
    New-Item -ItemType Directory -Path $path | Out-Null
  }
}

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..") | Select-Object -ExpandProperty Path

$Id = Read-IfEmpty $Id "Object id"
if (-not $Id) { throw "Object id is required." }

$Label = Read-IfEmpty $Label "Label" $Id

if (-not $TemplateId) { $TemplateId = "$Id.excel-like-table" }

$Kind = Read-IfEmpty $Kind "Kind (statement/callEdge)" $Kind

if ($Kind -eq "statement") {
  $ParseKind = Read-IfEmpty $ParseKind "Parse kind (regex/assignment/conditional/message/itabOp)" $ParseKind
  if ($ParseKind -eq "regex") {
    $defaultRegex = "^$($Id.ToUpper())\s+(.+)$"
    $Regex = Read-IfEmpty $Regex "Regex pattern" $defaultRegex
    $Flags = Read-IfEmpty $Flags "Regex flags" $Flags
    $PrimaryField = Read-IfEmpty $PrimaryField "Primary field name (capture group #1)" $PrimaryField
  }
} else {
  $ParseKind = ""
}

$useInline = $InlineTemplate.IsPresent
$useFile = $false

if (-not $InlineTemplate.IsPresent -and -not $TemplateFile) {
  $ans = Read-Host "Use inline template config? (Y/n)"
  if ($ans -match "^(n|no)$") { $useFile = $true } else { $useInline = $true }
} elseif ($TemplateFile) {
  $useFile = $true
  $useInline = $false
}

$safeTemplateFile = $TemplateFile
if ($useFile -and -not $safeTemplateFile) {
  $safeName = ($TemplateId -replace "[^A-Za-z0-9_.-]", "_")
  $safeTemplateFile = "web/js/abap_objects/templates/$safeName.js"
}

$escapedLabel = Escape-JsString $Label
$escapedTemplateId = Escape-JsString $TemplateId
$escapedPrimaryField = Escape-JsString $PrimaryField
$escapedRegex = Escape-JsString $Regex
$escapedFlags = Escape-JsString $Flags

$templateConfigLines = @(
"      type: `"excel-like-table`",",
"      compact: { removeEmptyRows: true },",
"      grid: { rows: 1, cols: 2, colWidths: { A: 160, B: 640 }, rowHeights: { 1: 30 } },",
"      css: {",
"        header: `"background:#9dc3e6;font-weight:700;color:#111;`",",
"        cell: `"border:1px solid #222;padding:6px 8px;vertical-align:middle;background:#fff;color:#111;`",",
"        wrap: `"white-space:normal;line-height:1.25;`",",
"      },",
"      cells: [",
"        { addr: `"A1`", text: `"$escapedLabel`", class: [`"cell`", `"header`"] },",
"        { addr: `"B1`", text: `"{${escapedPrimaryField}.description}`", class: [`"cell`", `"wrap`"] },",
"      ],"
)

$objectLines = New-Object System.Collections.Generic.List[string]
$objectLines.Add("      {")
$objectLines.Add("        id: `"$Id`",")
$objectLines.Add("        kind: `"$Kind`",")
$objectLines.Add("        label: `"$escapedLabel`",")

if ($Kind -eq "statement") {
  if ($ParseKind) {
    if ($ParseKind -eq "regex") {
      $objectLines.Add("        parse: { kind: `"regex`", regex: `"$escapedRegex`", flags: `"$escapedFlags`", fields: { `"$escapedPrimaryField`": 1 } },")
    } else {
      $objectLines.Add("        parse: { kind: `"$ParseKind`" },")
    }
  }
  $objectLines.Add("        builder: { kind: `"mapping`", fields: { `"$escapedPrimaryField`": { type: `"expr`", from: `"$escapedPrimaryField`" } } },")
} else {
  $prefix = Read-IfEmpty "" "match.toKeyPrefix (ex: FORM:)" "FORM:"
  $builder = Read-IfEmpty "" "builder kind (performCall)" "performCall"
  $objectLines.Add("        match: { toKeyPrefix: `"$prefix`" },")
  $objectLines.Add("        builder: { kind: `"$builder`" },")
}

$objectLines.Add("        templates: [")
$objectLines.Add("          {")
$objectLines.Add("            id: `"$escapedTemplateId`",")
$objectLines.Add("            label: `"$escapedLabel template`",")
$objectLines.Add("            auto: true,")

if ($useInline) {
  $objectLines.Add("            config: {")
  foreach ($line in $templateConfigLines) { $objectLines.Add("              $line") }
  $objectLines.Add("            },")
} else {
  $objectLines.Add("            file: `"$safeTemplateFile`",")
}

$objectLines.Add("          },")
$objectLines.Add("        ],")
$objectLines.Add("      },")

$objectSnippet = ($objectLines -join "`r`n")

Write-Host ""
Write-Host "=== Object snippet ==="
Write-Host $objectSnippet
Write-Host ""

if (Get-Command Set-Clipboard -ErrorAction SilentlyContinue) {
  Set-Clipboard -Value $objectSnippet
  Write-Host "Snippet copied to clipboard."
}

if ($useFile) {
  $templateAbs = Join-Path $repoRoot $safeTemplateFile
  $templateDir = Split-Path -Parent $templateAbs
  Ensure-Dir $templateDir

  if (Test-Path -LiteralPath $templateAbs) {
    Write-Warning "Template file already exists: $safeTemplateFile"
  } else {
    $templateContent = @"
(function (ns) {
  `"use strict`";

  const cfg = {
$(($templateConfigLines | ForEach-Object { "    $_" }) -join "`r`n")
  };

  ns.abapObjects?.defineTemplate?.(`"$escapedTemplateId`", cfg);
})(window.AbapFlow);
"@
    Set-Content -Path $templateAbs -Value $templateContent -NoNewline
    Write-Host "Template file created: $safeTemplateFile"
  }
}

if (-not $PSBoundParameters.ContainsKey("Insert")) {
  $ans = Read-Host "Insert snippet into web/abap_objects.config.js? (y/N)"
  if ($ans -match "^(y|yes)$") { $Insert = $true }
}

if ($Insert) {
  $configPath = Join-Path $repoRoot "web/abap_objects.config.js"
  if (-not (Test-Path -LiteralPath $configPath)) {
    Write-Warning "Config not found: web/abap_objects.config.js"
    exit 0
  }

  $marker = "// @abapflow-objects-insert"
  $raw = Get-Content -Path $configPath -Raw
  if ($raw -notmatch [regex]::Escape($marker)) {
    Write-Warning "Marker not found in web/abap_objects.config.js. Paste snippet manually."
    exit 0
  }

  $insertText = $objectSnippet.TrimEnd() + "`r`n      "
  $updated = $raw -replace [regex]::Escape($marker), ($insertText + $marker)
  Set-Content -Path $configPath -Value $updated -NoNewline
  Write-Host "Snippet inserted into web/abap_objects.config.js"
}
