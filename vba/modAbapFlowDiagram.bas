Attribute VB_Name = "modAbapFlowDiagram"
Option Explicit

' Entry point
Public Sub BuildAbapFlowDiagram()
    Const INPUT_SHEET As String = "Sheet1"
    Const OUTPUT_SHEET As String = "Sheet2"
    Const REPORT_SHEET As String = "Objects"

    Dim prevScreenUpdating As Boolean
    Dim prevEnableEvents As Boolean
    Dim prevCalculation As XlCalculation
    Dim stepName As String

    Dim hadError As Boolean
    Dim errNum As Long
    Dim errDesc As String
    Dim errSrc As String

    On Error GoTo ErrHandler

    prevScreenUpdating = Application.ScreenUpdating
    prevEnableEvents = Application.EnableEvents
    prevCalculation = Application.Calculation

    Application.ScreenUpdating = False
    Application.EnableEvents = False
    Application.Calculation = xlCalculationManual

    stepName = "Init sheets"
    Dim wsIn As Worksheet
    Dim wsOut As Worksheet
    Dim wsReport As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, INPUT_SHEET)
    Set wsOut = GetOrCreateWorksheet(ThisWorkbook, OUTPUT_SHEET)
    Set wsReport = GetOrCreateWorksheet(ThisWorkbook, REPORT_SHEET)

    stepName = "Read ABAP code"
    Dim abapCode As String
    abapCode = ReadAbapCodeFromSheet(wsIn)
    If Len(Trim$(abapCode)) = 0 Then
        Err.Raise vbObjectError + 1000, "BuildAbapFlowDiagram", "Sheet1 khong co du lieu ABAP."
    End If

    stepName = "Parse ABAP program"
    Dim isDefined As Object
    Dim orderKeys As Collection
    Dim globalData As Collection
    Dim globalConstants As Collection
    Dim subsDict As Object
    Set subsDict = ParseAbapProgram(abapCode, isDefined, orderKeys, globalData, globalConstants)

    stepName = "Build CalledBy index"
    BuildCalledByIndex subsDict

    stepName = "Detect cycles"
    Dim cycleEdges As Object
    Dim cycleNodes As Object
    Set cycleEdges = FindCycleEdges(subsDict, cycleNodes)

    stepName = "Compute hierarchy depths"
    Dim depths As Object
    Set depths = ComputeHierarchyDepths(subsDict, cycleEdges)

    stepName = "Draw diagram"
    DrawFlowDiagram wsOut, subsDict, depths, cycleEdges, cycleNodes, isDefined, orderKeys
    stepName = "Write objects report"
    WriteObjectsReport wsReport, subsDict, depths, cycleNodes, isDefined, orderKeys, globalData, globalConstants

CleanExit:
    Application.ScreenUpdating = prevScreenUpdating
    Application.EnableEvents = prevEnableEvents
    Application.Calculation = prevCalculation

    If hadError Then
        Dim msg As String
        msg = "Step: " & stepName & vbCrLf & "Err " & CStr(errNum) & " (" & errSrc & "): " & errDesc

        ' Prevent infinite loop: Err.Raise would jump back to ErrHandler.
        On Error GoTo 0
        If Application.Visible And Application.Interactive Then
            MsgBox msg, vbExclamation, "ABAP Flow Diagram"
            Exit Sub
        End If
        Err.Raise errNum, errSrc, msg
    End If
    Exit Sub

ErrHandler:
    hadError = True
    errNum = Err.Number
    errDesc = Err.Description
    errSrc = Err.Source
    Resume CleanExit
End Sub
