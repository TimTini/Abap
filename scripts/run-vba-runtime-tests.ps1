param(
    [switch]$SkipIfExcelMissing
)

$ErrorActionPreference = "Stop"

function Assert-Equal {
    param(
        [string]$Name,
        [string]$Actual,
        [string]$Expected
    )

    if ($Actual -ne $Expected) {
        throw "[FAIL] $Name`n  expected: '$Expected'`n  actual:   '$Actual'"
    }
}

function Assert-SequenceEqual {
    param(
        [string]$Name,
        [string[]]$Actual,
        [string[]]$Expected
    )

    $a = @($Actual)
    $e = @($Expected)

    if ($a.Count -ne $e.Count) {
        throw "[FAIL] $Name`n  expected count: $($e.Count)`n  actual count:   $($a.Count)`n  expected: $($e -join ', ')`n  actual:   $($a -join ', ')"
    }

    for ($i = 0; $i -lt $e.Count; $i++) {
        if ([string]$a[$i] -ne [string]$e[$i]) {
            throw "[FAIL] $Name at index $i`n  expected: '$($e[$i])'`n  actual:   '$($a[$i])'"
        }
    }
}

function New-ExcelApp {
    try {
        return New-Object -ComObject Excel.Application
    } catch {
        if ($SkipIfExcelMissing) {
            Write-Host "[SKIP] Excel COM is not available on this machine."
            exit 0
        }
        throw "Cannot create Excel COM object. Install Excel Desktop, or rerun with -SkipIfExcelMissing."
    }
}

function Find-MarkerCell {
    param(
        $Worksheet,
        [string]$MarkerText
    )

    try {
        return $Worksheet.Cells.Find($MarkerText)
    } catch {
        throw "Cannot find marker '$MarkerText'. Original: $($_.Exception.Message)"
    }
}

function Set-TemplateOptions {
    param(
        $Worksheet,
        [string]$TemplateKey,
        [string[]]$Options
    )

    $tl = Find-MarkerCell -Worksheet $Worksheet -MarkerText "__ABAPFLOW_TPL_${TemplateKey}_TL__"
    $tr = Find-MarkerCell -Worksheet $Worksheet -MarkerText "__ABAPFLOW_TPL_${TemplateKey}_TR__"
    if ($null -eq $tl -or $null -eq $tr) {
        throw "Template markers TL/TR not found for '$TemplateKey'."
    }

    $col = $tl.Column + 1
    foreach ($opt in $Options) {
        if ($col -ge $tr.Column) {
            break
        }
        $Worksheet.Cells($tl.Row, $col).Value2 = $opt
        $col++
    }
}

function Configure-TemplateRows {
    param(
        $Worksheet,
        [string]$TemplateKey,
        [hashtable[]]$Rows
    )

    $tl = Find-MarkerCell -Worksheet $Worksheet -MarkerText "__ABAPFLOW_TPL_${TemplateKey}_TL__"
    if ($null -eq $tl) {
        throw "Template marker TL not found for '$TemplateKey'."
    }

    $baseRow = [int]$tl.Row + 1
    $baseCol = [int]$tl.Column + 1
    $maxRows = 24

    $Worksheet.Range(
        $Worksheet.Cells($baseRow, $baseCol),
        $Worksheet.Cells($baseRow + $maxRows - 1, $baseCol + 1)
    ).ClearContents() | Out-Null

    $Worksheet.Cells($baseRow, $baseCol).Value2 = "path"
    $Worksheet.Cells($baseRow, $baseCol + 1).Value2 = "value"

    $rowIndex = 2
    foreach ($row in $Rows) {
        if ($rowIndex -gt $maxRows) {
            break
        }
        $Worksheet.Cells($baseRow + $rowIndex - 1, $baseCol).Value2 = [string]$row.path
        $Worksheet.Cells($baseRow + $rowIndex - 1, $baseCol + 1).Value2 = [string]$row.value
        $rowIndex++
    }
}

function Write-XmlToInputSheet {
    param(
        $Worksheet,
        [string]$XmlText
    )

    $normalized = [string]$XmlText
    $normalized = $normalized -replace "`r`n", "`n"
    $normalized = $normalized -replace "`r", "`n"
    $lines = $normalized -split "`n"

    $lastRow = [int]$Worksheet.Cells($Worksheet.Rows.Count, 1).End(-4162).Row # xlUp
    if ($lastRow -lt 2) { $lastRow = 2 }
    $Worksheet.Range("A2:A$lastRow").ClearContents() | Out-Null

    for ($i = 0; $i -lt $lines.Count; $i++) {
        $Worksheet.Cells(2 + $i, 1).Value2 = $lines[$i]
    }
}

function Find-HeaderRow {
    param(
        $Worksheet,
        [string]$ObjectType
    )

    $lastRow = [int]$Worksheet.Cells($Worksheet.Rows.Count, 1).End(-4162).Row
    if ($lastRow -lt 1) { $lastRow = 1 }

    for ($r = 1; $r -le $lastRow; $r++) {
        $text = [string]$Worksheet.Cells($r, 1).Value2
        if ([string]::IsNullOrWhiteSpace($text)) { continue }
        if ($text -match " $([Regex]::Escape($ObjectType))(\s|\||$)") {
            return $r
        }
    }

    throw "Header row for object type '$ObjectType' not found in Template sheet."
}

function Get-PathValuesFromBlock {
    param(
        $Worksheet,
        [int]$HeaderRow,
        [string]$Path
    )

    $values = New-Object System.Collections.Generic.List[string]
    for ($r = $HeaderRow + 1; $r -le $HeaderRow + 80; $r++) {
        $k = [string]$Worksheet.Cells($r, 1).Value2
        if ([string]::IsNullOrWhiteSpace($k)) { continue }
        if ($k -eq $Path) {
            $values.Add([string]$Worksheet.Cells($r, 2).Value2)
        }
    }
    return $values.ToArray()
}

function Build-TestXml {
    @'
<abapflowObjects>
  <file>runtime-test.abap</file>
  <objects>
    <object>
      <id>1</id>
      <parent/>
      <objectType>FORM</objectType>
      <lineStart>10</lineStart>
      <raw>FORM main USING iv_a TYPE i iv_b TYPE i CHANGING cv_text TYPE string RAISING cx_root cx_demo.</raw>
      <extras>
        <form>
          <name>main</name>
          <nameFromComment>MAIN</nameFromComment>
          <params>
            <item>
              <section>USING</section>
              <name>iv_a</name>
              <typing>
                <kind>TYPE</kind>
                <value>i</value>
              </typing>
            </item>
            <item>
              <section>USING</section>
              <name>iv_b</name>
              <typing>
                <kind>TYPE</kind>
                <value>i</value>
              </typing>
            </item>
          </params>
          <exceptions>
            <item><name>cx_root</name></item>
            <item><name>cx_demo</name></item>
          </exceptions>
        </form>
      </extras>
    </object>
    <object>
      <id>2</id>
      <parent/>
      <objectType>PERFORM</objectType>
      <lineStart>20</lineStart>
      <raw>PERFORM main USING lv_a lv_b lv_c CHANGING lv_out TABLES lt_tab.</raw>
      <extras>
        <performCall>
          <form>main</form>
          <program>sy-repid</program>
          <ifCondition>lv_a &gt; 0</ifCondition>
          <using>
            <item>
              <value>lv_a</value>
              <valueRef>lv_a</valueRef>
              <valueDecl><name>LV_A</name></valueDecl>
            </item>
            <item>
              <value>lv_b</value>
              <valueRef>lv_b</valueRef>
              <valueDecl><name>LV_B</name></valueDecl>
            </item>
            <item>
              <value>lv_c</value>
              <valueRef>lv_c</valueRef>
              <valueDecl><name>LV_C</name></valueDecl>
            </item>
          </using>
          <usingDirect>
            <value>du_a</value>
            <value>du_b</value>
          </usingDirect>
          <changing>
            <item><value>lv_out</value></item>
          </changing>
          <tables>
            <item><value>lt_tab</value></item>
          </tables>
        </performCall>
      </extras>
    </object>
    <object>
      <id>3</id>
      <parent/>
      <objectType>CALL_FUNCTION</objectType>
      <lineStart>30</lineStart>
      <raw>CALL FUNCTION 'Z_DEMO' EXPORTING iv_a = lv_a iv_b = lv_b IMPORTING ev_x = lv_x EXCEPTIONS cx_root = 1 OTHERS = 2.</raw>
      <extras>
        <callFunction>
          <name>'Z_DEMO'</name>
          <destination></destination>
          <exporting>
            <item><name>iv_a</name><value>lv_a</value></item>
            <item><name>iv_b</name><value>lv_b</value></item>
          </exporting>
          <importing>
            <item><name>ev_x</name><value>lv_x</value></item>
          </importing>
          <changing/>
          <tables/>
          <exceptions>
            <item><name>cx_root</name><value>1</value></item>
            <item><name>OTHERS</name><value>2</value></item>
          </exceptions>
        </callFunction>
      </extras>
    </object>
  </objects>
</abapflowObjects>
'@
}

$repoRoot = Split-Path -Parent $PSScriptRoot
$modulePath = Join-Path $repoRoot "excel\modAbapTemplateTool.bas"
if (-not (Test-Path $modulePath)) {
    throw "Cannot find VBA module at: $modulePath"
}

$excel = $null
$workbook = $null

try {
    $excel = New-ExcelApp
    $excel.Visible = $false
    $excel.DisplayAlerts = $false
    $excel.EnableEvents = $false
    $excel.AutomationSecurity = 3 # msoAutomationSecurityForceDisable

    $workbook = $excel.Workbooks.Add()

    try {
        $null = $workbook.VBProject.VBComponents.Import($modulePath)
    } catch {
        throw "Cannot import VBA module. Enable 'Trust access to the VBA project object model' in Excel. Original: $($_.Exception.Message)"
    }

    $null = $excel.Run("'$($workbook.Name)'!SetupAbapTemplateTool")

    $wsInput = $workbook.Worksheets.Item("Input")
    $wsCfg = $workbook.Worksheets.Item("Template config")
    $wsTpl = $workbook.Worksheets.Item("Template")

    Set-TemplateOptions -Worksheet $wsCfg -TemplateKey "FORM" -Options @("expandArrayRows=true")
    Set-TemplateOptions -Worksheet $wsCfg -TemplateKey "PERFORM" -Options @("expandArrayRows=true")
    Set-TemplateOptions -Worksheet $wsCfg -TemplateKey "CALL_FUNCTION" -Options @("expandArrayRows=true")

    Configure-TemplateRows -Worksheet $wsCfg -TemplateKey "FORM" -Rows @(
        @{ path = "extras.form.name"; value = "{extras.form.name}" },
        @{ path = "extras.form.params.name"; value = "{extras.form.params.name}" },
        @{ path = "extras.form.params.typing.value"; value = "{extras.form.params.typing.value}" },
        @{ path = "extras.form.exceptions.name"; value = "{extras.form.exceptions.name}" }
    )
    Configure-TemplateRows -Worksheet $wsCfg -TemplateKey "PERFORM" -Rows @(
        @{ path = "extras.performCall.form"; value = "{extras.performCall.form}" },
        @{ path = "extras.performCall.using.value"; value = "{extras.performCall.using.value}" },
        @{ path = "drop.when.empty"; value = "{extras.performCall.__no_such_value__}" },
        @{ path = "extras.performCall.usingDirect.value"; value = "{extras.performCall.usingDirect.value}" },
        @{ path = "extras.performCall.using.valueDecl.name"; value = "{extras.performCall.using.valueDecl.name}" },
        @{ path = "extras.performCall.changing.value"; value = "{extras.performCall.changing.value}" },
        @{ path = "extras.performCall.tables.value"; value = "{extras.performCall.tables.value}" },
        @{ path = "tail.marker"; value = "KEEP_TAIL" }
    )
    Configure-TemplateRows -Worksheet $wsCfg -TemplateKey "CALL_FUNCTION" -Rows @(
        @{ path = "extras.callFunction.name"; value = "{extras.callFunction.name}" },
        @{ path = "extras.callFunction.exporting.name"; value = "{extras.callFunction.exporting.name}" },
        @{ path = "extras.callFunction.exporting.value"; value = "{extras.callFunction.exporting.value}" },
        @{ path = "extras.callFunction.importing.name"; value = "{extras.callFunction.importing.name}" },
        @{ path = "extras.callFunction.importing.value"; value = "{extras.callFunction.importing.value}" },
        @{ path = "extras.callFunction.exceptions.name"; value = "{extras.callFunction.exceptions.name}" }
    )

    Write-XmlToInputSheet -Worksheet $wsInput -XmlText (Build-TestXml)

    $null = $excel.Run("'$($workbook.Name)'!LoadObjectListFromXml")
    $null = $excel.Run("'$($workbook.Name)'!GenerateAllTemplatesFromXml")

    $formHeader = Find-HeaderRow -Worksheet $wsTpl -ObjectType "FORM"
    $performHeader = Find-HeaderRow -Worksheet $wsTpl -ObjectType "PERFORM"
    $callFnHeader = Find-HeaderRow -Worksheet $wsTpl -ObjectType "CALL_FUNCTION"

    Assert-SequenceEqual -Name "FORM extras.form.params.name expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $formHeader -Path "extras.form.params.name") `
        -Expected @("iv_a", "iv_b")

    Assert-SequenceEqual -Name "FORM extras.form.exceptions.name expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $formHeader -Path "extras.form.exceptions.name") `
        -Expected @("cx_root", "cx_demo")

    Assert-SequenceEqual -Name "PERFORM extras.performCall.using.value expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $performHeader -Path "extras.performCall.using.value") `
        -Expected @("lv_a", "lv_b", "lv_c")

    Assert-SequenceEqual -Name "PERFORM placeholder-empty row is removed without deleting expanded rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $performHeader -Path "drop.when.empty") `
        -Expected @()

    Assert-SequenceEqual -Name "PERFORM extras.performCall.usingDirect.value expands repeated child tags" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $performHeader -Path "extras.performCall.usingDirect.value") `
        -Expected @("du_a", "du_b")

    Assert-SequenceEqual -Name "PERFORM extras.performCall.using.valueDecl.name expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $performHeader -Path "extras.performCall.using.valueDecl.name") `
        -Expected @("LV_A", "LV_B", "LV_C")

    Assert-SequenceEqual -Name "PERFORM rows after array expansion are preserved (no overwrite)" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $performHeader -Path "tail.marker") `
        -Expected @("KEEP_TAIL")

    Assert-SequenceEqual -Name "CALL_FUNCTION extras.callFunction.exporting.name expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $callFnHeader -Path "extras.callFunction.exporting.name") `
        -Expected @("iv_a", "iv_b")

    Assert-SequenceEqual -Name "CALL_FUNCTION extras.callFunction.exporting.value expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $callFnHeader -Path "extras.callFunction.exporting.value") `
        -Expected @("lv_a", "lv_b")

    Assert-SequenceEqual -Name "CALL_FUNCTION extras.callFunction.exceptions.name expands to rows" `
        -Actual (Get-PathValuesFromBlock -Worksheet $wsTpl -HeaderRow $callFnHeader -Path "extras.callFunction.exceptions.name") `
        -Expected @("cx_root", "OTHERS")

    $singleCellArray = [string]$wsTpl.Cells($performHeader + 3, 2).Value2
    if ($singleCellArray -like "*`n*" -or $singleCellArray -like "*`r*") {
        throw "[FAIL] PERFORM array value still contains newline in a single cell. Expected row expansion."
    }

    Write-Host "[PASS] VBA runtime tests completed successfully."
    exit 0
}
finally {
    if ($workbook -ne $null) {
        try { $workbook.Close($false) | Out-Null } catch {}
    }
    if ($excel -ne $null) {
        try { $excel.Quit() } catch {}
        try { [System.Runtime.InteropServices.Marshal]::ReleaseComObject($excel) | Out-Null } catch {}
    }
    [GC]::Collect()
    [GC]::WaitForPendingFinalizers()
}
