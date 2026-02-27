param(
  [int]$Top = 80,
  [string[]]$Extensions = @("*.js", "*.html", "*.py", "*.ps1", "*.bas", "*.md")
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $repoRoot

$gitFiles = git ls-files
if (-not $gitFiles) {
  Write-Host "No tracked files found."
  exit 0
}

$patterns = @{}
foreach ($ext in $Extensions) {
  $patterns[$ext.ToLowerInvariant()] = $true
}

$rows = foreach ($file in $gitFiles) {
  if (-not (Test-Path $file)) {
    continue
  }

  $name = [System.IO.Path]::GetFileName($file)
  $matched = $false
  foreach ($ext in $patterns.Keys) {
    if ($name -like $ext) {
      $matched = $true
      break
    }
  }
  if (-not $matched) {
    continue
  }

  $lineCount = (Get-Content $file | Measure-Object -Line).Lines
  [PSCustomObject]@{
    Lines = $lineCount
    File  = $file
  }
}

$rows |
  Sort-Object Lines -Descending |
  Select-Object -First $Top |
  Format-Table -AutoSize
