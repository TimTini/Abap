Param(
    [string]$Workbook = "excel\\AbapFlowDiagram.xlsm",
    [string]$VbaDir = "vba",
    [switch]$Export,
    [switch]$Import,
    [switch]$EnableVBOM
)

$ErrorActionPreference = "Stop"

if (-not $Import -and -not $Export) {
    $Import = $true
}

$venvPython = ".venv\\Scripts\\python.exe"
if (-not (Test-Path $venvPython)) {
    throw "Missing $venvPython. Run scripts\\setup.ps1 first."
}

if ($EnableVBOM) {
    & powershell -ExecutionPolicy Bypass -File scripts\\enable_vbom.ps1
}

$argsList = @("scripts\\sync_vba.py", "--workbook", $Workbook, "--vba-dir", $VbaDir)
if ($Import) { $argsList += "--import" }
if ($Export) { $argsList += "--export" }

& $venvPython @argsList
