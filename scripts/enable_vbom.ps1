Param(
    [switch]$AlsoSetPolicies
)

$ErrorActionPreference = "Stop"

function Set-AccessVbom([string]$baseKey) {
    if (-not (Test-Path $baseKey)) {
        return $false
    }

    Get-ChildItem $baseKey -ErrorAction SilentlyContinue |
        Where-Object { $_.PSChildName -match '^\d+\.\d+$' } |
        ForEach-Object {
            $version = $_.PSChildName
            $securityPath = Join-Path $_.PsPath "Excel\\Security"
            New-Item -Path $securityPath -Force | Out-Null
            New-ItemProperty -Path $securityPath -Name AccessVBOM -PropertyType DWord -Value 1 -Force | Out-Null
            Write-Host "Enabled AccessVBOM for Office $version ($securityPath)"
        }

    return $true
}

Write-Host "Enabling 'Trust access to the VBA project object model' (AccessVBOM=1) for current user..."
Write-Host "Close all Excel instances first for best results."
Write-Host ""

$ok = $false
$ok = (Set-AccessVbom "HKCU:\\Software\\Microsoft\\Office") -or $ok

if ($AlsoSetPolicies) {
    Write-Host ""
    Write-Host "Also attempting Policies hive (may be overridden by your organization)..."
    $ok = (Set-AccessVbom "HKCU:\\Software\\Policies\\Microsoft\\Office") -or $ok
}

if (-not $ok) {
    Write-Host "No Office registry keys found under HKCU. Excel may not be installed for this user yet."
}

