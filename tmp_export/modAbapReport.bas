Attribute VB_Name = "modAbapReport"
Option Explicit

' -----------------------------
' Objects report (Sheet "Objects")
' -----------------------------

Public Sub WriteObjectsReport(ByVal ws As Worksheet, ByVal subsDict As Object, ByVal depths As Object, ByVal cycleNodes As Object, ByVal isDefined As Object, ByVal orderKeys As Collection, ByVal globalData As Collection, ByVal globalConstants As Collection)
    On Error GoTo ErrHandler

    ws.Cells.Clear

    Dim i As Long
    For i = ws.Shapes.Count To 1 Step -1
        ws.Shapes(i).Delete
    Next i

    Dim rowIdx As Long
    rowIdx = 1

    ws.Cells(rowIdx, 1).value = "ABAP Objects Report"
    ws.Cells(rowIdx, 1).Font.Bold = True
    rowIdx = rowIdx + 2

    ' Global declarations
    ws.Cells(rowIdx, 1).value = "Global Declarations"
    ws.Cells(rowIdx, 1).Font.Bold = True
    rowIdx = rowIdx + 1

    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 4)).value = Array("DeclarationType", "Name", "DataType", "Value")
    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 4)).Font.Bold = True
    rowIdx = rowIdx + 1

    Dim decl As clsDataDeclaration
    For Each decl In globalData
        ws.Cells(rowIdx, 1).value = decl.DeclarationType
        ws.Cells(rowIdx, 2).value = decl.VariableName
        ws.Cells(rowIdx, 3).value = decl.dataType
        ws.Cells(rowIdx, 4).value = decl.value
        rowIdx = rowIdx + 1
    Next decl

    For Each decl In globalConstants
        ws.Cells(rowIdx, 1).value = decl.DeclarationType
        ws.Cells(rowIdx, 2).value = decl.VariableName
        ws.Cells(rowIdx, 3).value = decl.dataType
        ws.Cells(rowIdx, 4).value = decl.value
        rowIdx = rowIdx + 1
    Next decl

    rowIdx = rowIdx + 2

    ' Objects table
    ws.Cells(rowIdx, 1).value = "Objects"
    ws.Cells(rowIdx, 1).Font.Bold = True
    rowIdx = rowIdx + 1

    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 11)).value = Array( _
        "Kind", "Name", "Depth", "Params", "LocalData", "LocalConst", "Defined", "InCycle", _
        "InfoType", "InfoValue", "Description" _
    )
    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 11)).Font.Bold = True
    rowIdx = rowIdx + 1

    Dim idx As Long
    For idx = 1 To orderKeys.Count
        Dim key As String
        key = CStr(orderKeys(idx))
        If Not subsDict.Exists(key) Then GoTo ContinueKey

        Dim subr As clsSubroutine
        Set subr = subsDict(key)

        Dim kind As String
        kind = UCase$(Trim$(subr.kind))
        If Len(kind) = 0 Then kind = "FORM"

        Dim depth As Long
        depth = 0
        If depths.Exists(key) Then depth = CLng(depths(key))

        Dim defined As Boolean
        defined = isDefined.Exists(key)

        Dim inCycle As Boolean
        inCycle = cycleNodes.Exists(key)

        Dim paramCount As Long
        Dim localDataCount As Long
        Dim localConstCount As Long
        paramCount = subr.Parameters.Count
        localDataCount = subr.LocalData.Count
        localConstCount = subr.LocalConstants.Count

        Dim descValues As Collection
        Dim descDescs As Collection
        Set descValues = New Collection
        Set descDescs = GetTextLines(subr.Description)

        If descDescs.Count > 0 Then
            Dim lineItem As Variant
            For Each lineItem In descDescs
                descValues.Add vbNullString
            Next lineItem

            WriteObjectInfoRows ws, rowIdx, kind, subr.name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Description", descValues, descDescs
        End If

        Dim paramValues As Collection
        Dim paramDescs As Collection
        BuildParameterInfo subr.Parameters, paramValues, paramDescs
        WriteObjectInfoRows ws, rowIdx, kind, subr.name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Parameters", paramValues, paramDescs

        WriteObjectInfoRows ws, rowIdx, kind, subr.name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Local DATA", GetDeclarationLines(subr.LocalData, False)
        WriteObjectInfoRows ws, rowIdx, kind, subr.name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Local CONSTANTS", GetDeclarationLines(subr.LocalConstants, True)
        WriteObjectInfoRows ws, rowIdx, kind, subr.name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Callings", GetVariantLines(subr.Callings)
        WriteObjectInfoRows ws, rowIdx, kind, subr.name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "CalledBy", GetVariantLines(subr.CalledBy)

ContinueKey:
    Next idx

    ws.Columns("A:A").ColumnWidth = 12
    ws.Columns("B:B").ColumnWidth = 32
    ws.Columns("C:C").ColumnWidth = 7
    ws.Columns("D:D").ColumnWidth = 7
    ws.Columns("E:E").ColumnWidth = 10
    ws.Columns("F:F").ColumnWidth = 10
    ws.Columns("G:G").ColumnWidth = 9
    ws.Columns("H:H").ColumnWidth = 9
    ws.Columns("I:I").ColumnWidth = 16
    ws.Columns("J:J").ColumnWidth = 45
    ws.Columns("K:K").ColumnWidth = 55
    ws.Rows("1:" & CStr(rowIdx)).EntireRow.AutoFit
    ws.Rows("1:1").RowHeight = 20

    Exit Sub

ErrHandler:
    ' Keep the main macro running even if report formatting fails
End Sub

Private Sub WriteObjectInfoRows(ByVal ws As Worksheet, ByRef rowIdx As Long, ByVal kind As String, ByVal name As String, ByVal depth As Long, ByVal paramsCount As Long, ByVal localDataCount As Long, ByVal localConstCount As Long, ByVal defined As Boolean, ByVal inCycle As Boolean, ByVal infoType As String, ByVal values As Collection, Optional ByVal descriptions As Collection)
    If values Is Nothing Then
        WriteObjectInfoRow ws, rowIdx, kind, name, depth, paramsCount, localDataCount, localConstCount, defined, inCycle, infoType, vbNullString, vbNullString
        rowIdx = rowIdx + 1
        Exit Sub
    End If

    If values.Count = 0 Then
        WriteObjectInfoRow ws, rowIdx, kind, name, depth, paramsCount, localDataCount, localConstCount, defined, inCycle, infoType, vbNullString, vbNullString
        rowIdx = rowIdx + 1
        Exit Sub
    End If

    Dim v As Variant
    Dim itemIdx As Long
    itemIdx = 0
    For Each v In values
        itemIdx = itemIdx + 1

        Dim desc As String
        desc = vbNullString
        If Not descriptions Is Nothing Then
            If itemIdx <= descriptions.Count Then desc = CStr(descriptions(itemIdx))
        End If

        WriteObjectInfoRow ws, rowIdx, kind, name, depth, paramsCount, localDataCount, localConstCount, defined, inCycle, infoType, CStr(v), desc
        rowIdx = rowIdx + 1
    Next v
End Sub

Private Sub WriteObjectInfoRow(ByVal ws As Worksheet, ByVal rowIdx As Long, ByVal kind As String, ByVal name As String, ByVal depth As Long, ByVal paramsCount As Long, ByVal localDataCount As Long, ByVal localConstCount As Long, ByVal defined As Boolean, ByVal inCycle As Boolean, ByVal infoType As String, ByVal infoValue As String, ByVal rowDescription As String)
    ws.Cells(rowIdx, 1).value = kind
    ws.Cells(rowIdx, 2).value = name
    ws.Cells(rowIdx, 3).value = depth
    ws.Cells(rowIdx, 4).value = paramsCount
    ws.Cells(rowIdx, 5).value = localDataCount
    ws.Cells(rowIdx, 6).value = localConstCount
    ws.Cells(rowIdx, 7).value = IIf(defined, "Yes", "No")
    ws.Cells(rowIdx, 8).value = IIf(inCycle, "Yes", "No")
    ws.Cells(rowIdx, 9).value = infoType
    ws.Cells(rowIdx, 10).value = infoValue
    ws.Cells(rowIdx, 11).value = rowDescription

    Dim rowRange As Range
    Set rowRange = ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 11))

    If inCycle Then
        rowRange.Interior.Color = RGB(255, 199, 206)
    ElseIf Not defined Then
        rowRange.Interior.Color = RGB(217, 217, 217)
    ElseIf kind = "EVENT" Then
        rowRange.Interior.Color = RGB(226, 239, 218)
    End If
End Sub

Private Function GetVariantLines(ByVal col As Collection) As Collection
    Dim result As Collection
    Set result = New Collection

    If col Is Nothing Then
        Set GetVariantLines = result
        Exit Function
    End If

    Dim v As Variant
    For Each v In col
        result.Add CStr(v)
    Next v

    Set GetVariantLines = result
End Function

Private Sub BuildParameterInfo(ByVal params As Collection, ByRef values As Collection, ByRef descriptions As Collection)
    Set values = New Collection
    Set descriptions = New Collection

    If params Is Nothing Then Exit Sub

    Dim p As clsParameter
    For Each p In params
        Dim part As String
        part = UCase$(Trim$(p.paramType)) & " " & p.name
        If Len(Trim$(p.dataType)) > 0 Then part = part & " TYPE " & p.dataType
        values.Add part
        descriptions.Add p.Description
    Next p
End Sub

Private Function GetDeclarationLines(ByVal decls As Collection, ByVal includeValue As Boolean) As Collection
    Dim result As Collection
    Set result = New Collection

    If decls Is Nothing Then
        Set GetDeclarationLines = result
        Exit Function
    End If

    Dim decl As clsDataDeclaration
    For Each decl In decls
        Dim part As String
        part = decl.VariableName
        If Len(Trim$(decl.dataType)) > 0 Then part = part & " TYPE " & decl.dataType
        If includeValue And Len(Trim$(decl.value)) > 0 Then part = part & " VALUE " & decl.value
        result.Add part
    Next decl

    Set GetDeclarationLines = result
End Function

Private Function GetTextLines(ByVal text As String) As Collection
    Dim result As Collection
    Set result = New Collection

    Dim t As String
    t = NormalizeLineBreaks(text)

    Dim parts() As String
    parts = Split(t, vbLf)

    Dim i As Long
    For i = LBound(parts) To UBound(parts)
        Dim line As String
        line = Trim$(CStr(parts(i)))
        If Len(line) > 0 Then result.Add line
    Next i

    Set GetTextLines = result
End Function

