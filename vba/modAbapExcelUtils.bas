Attribute VB_Name = "modAbapExcelUtils"
Option Explicit

Public Function GetOrCreateWorksheet(ByVal wb As Workbook, ByVal sheetName As String) As Worksheet
    On Error Resume Next
    Set GetOrCreateWorksheet = wb.Worksheets(sheetName)
    On Error GoTo 0

    If GetOrCreateWorksheet Is Nothing Then
        Set GetOrCreateWorksheet = wb.Worksheets.Add(After:=wb.Worksheets(wb.Worksheets.Count))
        GetOrCreateWorksheet.Name = sheetName
    End If
End Function

Public Function ReadAbapCodeFromSheet(ByVal ws As Worksheet) As String
    Dim rng As Range
    Set rng = ws.UsedRange
    If rng Is Nothing Then Exit Function

    Dim v As Variant
    v = rng.Value2

    Dim outText As String
    outText = vbNullString

    Dim r As Long, c As Long
    If IsArray(v) Then
        For r = 1 To UBound(v, 1)
            For c = 1 To UBound(v, 2)
                Dim s As String
                s = CStr(v(r, c))
                If Len(s) > 0 Then outText = outText & s & vbCrLf
            Next c
        Next r
    Else
        outText = CStr(v)
    End If

    ReadAbapCodeFromSheet = outText
End Function

