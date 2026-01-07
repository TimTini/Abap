Attribute VB_Name = "modAbapTraceUi"
Option Explicit

Private Const TRACE_SHEET As String = "Trace"
Private Const TRACE_COL_SUBLIST As Long = 8  ' H
Private Const TRACE_COL_VARLIST As Long = 9  ' I

Public Sub SetupTraceSheet()
    On Error GoTo ErrHandler

    Dim ws As Worksheet
    Set ws = GetOrCreateWorksheet(ThisWorkbook, TRACE_SHEET)

    ws.Parent.Activate
    ws.Activate

    ws.Cells.Clear
    DeleteAllShapes ws

    ws.Range("A1").Value = "ABAP Data Trace"
    ws.Range("A1").Font.Bold = True
    ws.Range("A1").Font.Size = 14

    ws.Range("A3").Value = "Subroutine Key"
    ws.Range("A4").Value = "Variable"
    ws.Range("A3:A4").Font.Bold = True

    ws.Range("B3").Value = vbNullString
    ws.Range("B4").Value = vbNullString

    ws.Columns("A:A").ColumnWidth = 18
    ws.Columns("B:B").ColumnWidth = 40
    ws.Columns("C:C").ColumnWidth = 2
    ws.Columns("D:D").ColumnWidth = 18
    ws.Columns("E:E").ColumnWidth = 18
    ws.Columns("F:F").ColumnWidth = 18
    ws.Columns("G:G").ColumnWidth = 50

    ws.Columns(TRACE_COL_SUBLIST).Hidden = True
    ws.Columns(TRACE_COL_VARLIST).Hidden = True

    If Not HasAbapModel() Then
        ws.Range("A6").Value = "Chua co model. Hay chay BuildAbapFlowDiagram truoc."
        Exit Sub
    End If

    WriteSubroutineList ws
    ApplySubroutineValidation ws

    AddButton ws, "Refresh Vars", "Trace_RefreshVariables", 360, 48
    AddButton ws, "Trace Origin", "Trace_Run", 360, 88

    If gOrderKeys.Count > 0 Then
        ws.Range("B3").Value = CStr(gOrderKeys(1))
        Trace_RefreshVariables
    End If

    Exit Sub

ErrHandler:
    Dim n As Long
    Dim d As String
    n = Err.Number
    d = Err.Description

    ' Bubble up to the main macro (or automation) to avoid hanging on MsgBox.
    On Error GoTo 0
    Err.Raise n, "SetupTraceSheet", "SetupTraceSheet error: " & d
End Sub

Public Sub Trace_RefreshVariables()
    On Error GoTo ErrHandler

    If Not HasAbapModel() Then
        MsgBox "Chua co model. Hay chay BuildAbapFlowDiagram truoc.", vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If

    Dim ws As Worksheet
    Set ws = ThisWorkbook.Worksheets(TRACE_SHEET)
    ws.Parent.Activate
    ws.Activate

    Dim subKey As String
    subKey = UCase$(Trim$(CStr(ws.Range("B3").Value2)))
    If Len(subKey) = 0 Then Exit Sub
    If Not gSubsDict.Exists(subKey) Then
        MsgBox "Khong tim thay subroutine key: " & subKey, vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If

    Dim subr As clsSubroutine
    Set subr = gSubsDict(subKey)

    Dim vars As Collection
    Set vars = New Collection

    Dim v As Variant
    For Each v In subr.Writes
        AddUnique vars, UCase$(CStr(v))
    Next v

    Dim p As clsParameter
    For Each p In subr.Parameters
        Dim t As String
        t = UCase$(Trim$(p.paramType))
        If t = "CHANGING" Or t = "TABLES" Then
            AddUnique vars, UCase$(p.name)
        End If
    Next p

    Dim startRow As Long
    startRow = 2

    ws.Range(ws.Cells(startRow, TRACE_COL_VARLIST), ws.Cells(startRow + 2000, TRACE_COL_VARLIST)).ClearContents

    Dim i As Long
    For i = 1 To vars.Count
        ws.Cells(startRow + i - 1, TRACE_COL_VARLIST).Value = CStr(vars(i))
    Next i

    ApplyVariableValidation ws, startRow, vars.Count
    Exit Sub

ErrHandler:
    HandleUiError "Trace_RefreshVariables", Err.Number, Err.Description
End Sub

Public Sub Trace_Run()
    On Error GoTo ErrHandler

    If Not HasAbapModel() Then
        MsgBox "Chua co model. Hay chay BuildAbapFlowDiagram truoc.", vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If

    Dim ws As Worksheet
    Set ws = ThisWorkbook.Worksheets(TRACE_SHEET)
    ws.Parent.Activate
    ws.Activate

    Dim subKey As String
    subKey = UCase$(Trim$(CStr(ws.Range("B3").Value2)))
    Dim varName As String
    varName = UCase$(Trim$(CStr(ws.Range("B4").Value2)))

    If Len(subKey) = 0 Or Len(varName) = 0 Then
        MsgBox "Hay chon Subroutine va Variable.", vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If
    If Not gSubsDict.Exists(subKey) Then
        MsgBox "Khong tim thay subroutine key: " & subKey, vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If

    Dim globalIndex As Object
    Set globalIndex = BuildGlobalDeclarationIndex(gGlobalData, gGlobalConstants)

    Dim results As Collection
    Set results = TraceVariableOrigins(gSubsDict, globalIndex, subKey, varName)

    Dim writers As Object
    Set writers = BuildWritersIndex(gSubsDict)

    ws.Range("A6:K2000").ClearContents

    Dim outRow As Long
    outRow = 6

    ws.Cells(outRow, 1).Value = "Selected Subroutine"
    ws.Cells(outRow, 2).Value = subKey
    outRow = outRow + 1

    ws.Cells(outRow, 1).Value = "Selected Variable"
    ws.Cells(outRow, 2).Value = varName
    outRow = outRow + 1

    ws.Cells(outRow, 1).Value = "Selected Description"
    ws.Cells(outRow, 2).Value = GetVariableDescriptionInContext(gSubsDict(subKey), globalIndex, varName)
    outRow = outRow + 2

    ws.Cells(outRow, 1).Value = "Writers (same var name)"
    ws.Cells(outRow, 1).Font.Bold = True
    outRow = outRow + 1

    If writers.Exists(varName) Then
        Dim col As Collection
        Set col = writers(varName)

        Dim w As Variant
        For Each w In col
            ws.Cells(outRow, 1).Value = CStr(w)
            outRow = outRow + 1
        Next w
    Else
        ws.Cells(outRow, 1).Value = "(none)"
        outRow = outRow + 1
    End If

    outRow = outRow + 1

    ws.Cells(outRow, 1).Value = "Origin Results"
    ws.Cells(outRow, 1).Font.Bold = True
    outRow = outRow + 1

    ws.Range(ws.Cells(outRow, 1), ws.Cells(outRow, 6)).Value = Array("OriginKind", "OriginSubKey", "OriginVar", "DataType", "Description", "Path")
    ws.Range(ws.Cells(outRow, 1), ws.Cells(outRow, 6)).Font.Bold = True
    outRow = outRow + 1

    If results.Count = 0 Then
        ws.Cells(outRow, 1).Value = "(not found)"
        Exit Sub
    End If

    Dim r As Variant
    For Each r In results
        ws.Cells(outRow, 1).Value = CStr(r("OriginKind"))
        ws.Cells(outRow, 2).Value = CStr(r("OriginSubKey"))
        ws.Cells(outRow, 3).Value = CStr(r("OriginVar"))
        ws.Cells(outRow, 4).Value = CStr(r("DataType"))
        ws.Cells(outRow, 5).Value = CStr(r("Description"))
        ws.Cells(outRow, 6).Value = CStr(r("Path"))
        outRow = outRow + 1
    Next r

    ws.Columns("A:G").EntireColumn.AutoFit
    Exit Sub

ErrHandler:
    HandleUiError "Trace_Run", Err.Number, Err.Description
End Sub

Public Sub AbapTrace_SelectFromDiagram()
    On Error GoTo ErrHandler

    If Not HasAbapModel() Then
        MsgBox "Chua co model. Hay chay BuildAbapFlowDiagram truoc.", vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If

    Dim shName As String
    shName = CStr(Application.Caller)
    If Len(shName) = 0 Then Exit Sub

    Dim sh As Shape
    Set sh = ActiveSheet.Shapes(shName)

    Dim key As String
    key = UCase$(Trim$(sh.AlternativeText))
    If Len(key) = 0 Then Exit Sub

    SetupTraceSheet

    Dim ws As Worksheet
    Set ws = ThisWorkbook.Worksheets(TRACE_SHEET)
    ws.Range("B3").Value = key
    Trace_RefreshVariables

    ws.Activate
    ws.Range("B4").Select
    Exit Sub

ErrHandler:
    HandleUiError "AbapTrace_SelectFromDiagram", Err.Number, Err.Description
End Sub

Private Sub WriteSubroutineList(ByVal ws As Worksheet)
    Dim startRow As Long
    startRow = 2

    ws.Range(ws.Cells(startRow, TRACE_COL_SUBLIST), ws.Cells(startRow + 5000, TRACE_COL_SUBLIST)).ClearContents

    Dim i As Long
    For i = 1 To gOrderKeys.Count
        ws.Cells(startRow + i - 1, TRACE_COL_SUBLIST).Value = CStr(gOrderKeys(i))
    Next i
End Sub

Private Sub ApplySubroutineValidation(ByVal ws As Worksheet)
    Dim startRow As Long
    startRow = 2

    Dim lastRow As Long
    lastRow = startRow + gOrderKeys.Count - 1
    If lastRow < startRow Then lastRow = startRow

    Dim addr As String
    addr = "='" & ws.Name & "'!" & ws.Range(ws.Cells(startRow, TRACE_COL_SUBLIST), ws.Cells(lastRow, TRACE_COL_SUBLIST)).Address

    On Error Resume Next
    ws.Range("B3").Validation.Delete
    On Error GoTo 0

    ws.Range("B3").Validation.Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, Operator:=xlBetween, Formula1:=addr
End Sub

Private Sub ApplyVariableValidation(ByVal ws As Worksheet, ByVal startRow As Long, ByVal count As Long)
    On Error Resume Next
    ws.Range("B4").Validation.Delete
    On Error GoTo 0

    If count <= 0 Then Exit Sub

    Dim lastRow As Long
    lastRow = startRow + count - 1

    Dim addr As String
    addr = "='" & ws.Name & "'!" & ws.Range(ws.Cells(startRow, TRACE_COL_VARLIST), ws.Cells(lastRow, TRACE_COL_VARLIST)).Address
    ws.Range("B4").Validation.Add Type:=xlValidateList, AlertStyle:=xlValidAlertStop, Operator:=xlBetween, Formula1:=addr
End Sub

Private Sub AddButton(ByVal ws As Worksheet, ByVal caption As String, ByVal macroName As String, ByVal left As Single, ByVal top As Single)
    Dim sh As Shape
    Set sh = ws.Shapes.AddShape(msoShapeRoundedRectangle, left, top, 150, 28)
    sh.TextFrame.Characters.Text = caption
    sh.TextFrame.Characters.Font.Color = RGB(0, 0, 0)
    sh.Fill.ForeColor.RGB = RGB(221, 235, 247)
    sh.Line.ForeColor.RGB = RGB(60, 60, 60)
    sh.OnAction = macroName
    sh.Placement = xlFreeFloating
End Sub

Private Sub DeleteAllShapes(ByVal ws As Worksheet)
    Dim i As Long
    For i = ws.Shapes.Count To 1 Step -1
        ws.Shapes(i).Delete
    Next i
End Sub

Private Sub AddUnique(ByVal col As Collection, ByVal value As String)
    If col Is Nothing Then Exit Sub
    Dim key As String
    key = UCase$(Trim$(value))
    If Len(key) = 0 Then Exit Sub
    On Error Resume Next
    col.Add value, key
    On Error GoTo 0
End Sub

Private Sub HandleUiError(ByVal procName As String, ByVal errNum As Long, ByVal errDesc As String)
    Dim msg As String
    msg = procName & " error: " & errDesc

    On Error GoTo 0
    If Application.Visible And Application.Interactive Then
        MsgBox msg, vbExclamation, "ABAP Flow Diagram"
        Exit Sub
    End If

    Err.Raise errNum, procName, msg
End Sub

Private Function GetVariableDescriptionInContext(ByVal subr As clsSubroutine, ByVal globalIndex As Object, ByVal varKey As String) As String
    If Len(Trim$(varKey)) = 0 Then Exit Function

    If Not subr Is Nothing Then
        Dim p As clsParameter
        For Each p In subr.Parameters
            If UCase$(Trim$(p.name)) = varKey Then
                GetVariableDescriptionInContext = p.Description
                Exit Function
            End If
        Next p

        Dim d As clsDataDeclaration
        For Each d In subr.LocalData
            If UCase$(Trim$(d.VariableName)) = varKey Then
                GetVariableDescriptionInContext = d.Description
                Exit Function
            End If
        Next d

        For Each d In subr.LocalConstants
            If UCase$(Trim$(d.VariableName)) = varKey Then
                GetVariableDescriptionInContext = d.Description
                Exit Function
            End If
        Next d
    End If

    If Not globalIndex Is Nothing Then
        If globalIndex.Exists(varKey) Then
            Dim decl As clsDataDeclaration
            Set decl = globalIndex(varKey)
            GetVariableDescriptionInContext = decl.Description
        End If
    End If
End Function
