Param(
    [string]$VenvPath = ".venv",
    [string]$PythonCmd = "python"
)

$ErrorActionPreference = "Stop"

$pythonExe = $null
$pythonArgs = @()

if (Get-Command $PythonCmd -ErrorAction SilentlyContinue) {
    $pythonExe = $PythonCmd
} elseif (Get-Command "py" -ErrorAction SilentlyContinue) {
    $pythonExe = "py"
    $pythonArgs = @("-3")
} else {
    throw "Python not found. Install Python 3.x and ensure it is on PATH."
}

if (-not (Test-Path $VenvPath)) {
    Write-Host "Creating venv at $VenvPath ..."
    & $pythonExe @pythonArgs -m venv $VenvPath
}

$venvPython = Join-Path $VenvPath "Scripts\\python.exe"
if (-not (Test-Path $venvPython)) {
    throw "Venv python not found at $venvPython"
}

Write-Host "Installing dependencies ..."
& $venvPython -m pip install --upgrade pip
& $venvPython -m pip install -r requirements.txt

Write-Host ""
Write-Host "Done."
Write-Host "Next:"
Write-Host "  $venvPython scripts\\sync_vba.py --import"
