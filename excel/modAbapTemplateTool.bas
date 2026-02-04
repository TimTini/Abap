Attribute VB_Name = "modAbapTemplateTool"
Option Explicit

Private Const SHEET_INPUT As String = "Input"
Private Const SHEET_TEMPLATE_CONFIG As String = "Template config"
Private Const SHEET_TEMPLATE As String = "Template"

Private Const MARKER_PREFIX As String = "__ABAPFLOW_TPL_"
Private Const MARKER_SUFFIX_TL As String = "_TL__"
Private Const MARKER_SUFFIX_TR As String = "_TR__"
Private Const MARKER_SUFFIX_BL As String = "_BL__"
Private Const MARKER_SUFFIX_BR As String = "_BR__"

Private Const INDENT_COLS_PER_DEPTH As Long = 2
Private Const BLOCK_GAP_ROWS As Long = 2
Private Const MAX_HEADER_TEXT As Long = 240
Private Const MAX_LIST_ORIGINAL As Long = 140

Private Const STATUS_CELL As String = "C2"

Private Const LIST_HEADER_ROW As Long = 4
Private Const LIST_FIRST_ROW As Long = 5
Private Const LIST_COL_SELECT As Long = 2
Private Const LIST_COL_INDEX As Long = 3
Private Const LIST_COL_OBJECT_ID As Long = 4
Private Const LIST_COL_TEMPLATE_ID As Long = 5
Private Const LIST_COL_KIND As Long = 6
Private Const LIST_COL_ROUTINE As Long = 7
Private Const LIST_COL_LINE As Long = 8
Private Const LIST_COL_ORIGINAL As Long = 9
Private Const LIST_COL_DEPTH As Long = 10
Private Const LIST_COL_LAST As Long = 10

Public Sub Auto_Open()
    On Error Resume Next
    SetupAbapTemplateTool
End Sub

Public Sub SetupAbapTemplateTool()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    Dim wsCfg As Worksheet
    Set wsCfg = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE_CONFIG)

    Dim wsTpl As Worksheet
    Set wsTpl = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE)

    SetupInputSheet wsIn
    SetupTemplateConfigSheet wsCfg
    SetupTemplateSheet wsTpl
End Sub

Private Sub SetupInputSheet(ByVal wsIn As Worksheet)
    If Len(CStr(wsIn.Range("A1").Value2)) = 0 Then
        wsIn.Range("A1").Value2 = U("D\u00E1n XML export t\u1EEB ABAP Parser Viewer (web) v\u00E0o c\u1ED9t A t\u1EEB \u00F4 A2 (c\u00F3 th\u1EC3 d\u00E1n to\u00E0n b\u1ED9 XML v\u00E0o A2; Excel s\u1EBD t\u1EF1 t\u00E1ch d\u00F2ng).")
        wsIn.Range("A1").Font.Bold = True
        wsIn.Range("A1").WrapText = True
    End If

    wsIn.Columns("A").ColumnWidth = 110
    wsIn.Columns("B").ColumnWidth = 8
    wsIn.Columns("C").ColumnWidth = 8
    wsIn.Columns("D").ColumnWidth = 26
    wsIn.Columns("E").ColumnWidth = 26
    wsIn.Columns("F").ColumnWidth = 12
    wsIn.Columns("G").ColumnWidth = 26
    wsIn.Columns("H").ColumnWidth = 10
    wsIn.Columns("I").ColumnWidth = 80
    wsIn.Columns("J").ColumnWidth = 6

    wsIn.Columns("A").WrapText = False

    wsIn.Range("B1:B2").ClearContents

    wsIn.Range("C1").Value2 = U("Tr\u1EA1ng th\u00E1i")
    wsIn.Range("C1").Font.Bold = True
    If Len(CStr(wsIn.Range(STATUS_CELL).Value2)) = 0 Then wsIn.Range(STATUS_CELL).Value2 = U("S\u1EB5n s\u00E0ng")

    EnsureObjectListHeader wsIn

    EnsureButton wsIn, "btnLoadXml", U("N\u1EA1p XML"), "LoadXmlFromFile", 320, 32
    EnsureButton wsIn, "btnListObjects", U("Danh s\u00E1ch \u0111\u1ED1i t\u01B0\u1EE3ng"), "LoadObjectListFromXml", 320, 72
    EnsureButton wsIn, "btnGenerateAll", U("T\u1EA1o to\u00E0n b\u1ED9/\u0111\u00E3 ch\u1ECDn"), "GenerateAllTemplatesFromXml", 320, 152
    EnsureButton wsIn, "btnResetTplCfg", U("Kh\u00F4i ph\u1EE5c template"), "ResetTemplateConfig", 320, 192
    EnsureButton wsIn, "btnSelectAll", U("Ch\u1ECDn t\u1EA5t c\u1EA3"), "SelectAllObjects", 320, 232
    EnsureButton wsIn, "btnClearSelected", U("B\u1ECF ch\u1ECDn"), "ClearSelectedObjects", 320, 272
    RemoveButton wsIn, "btnGenerate"
    RemoveButton wsIn, "btnGenerateOne"
End Sub

Private Sub SetupTemplateSheet(ByVal wsTpl As Worksheet)
    If wsTpl Is Nothing Then Exit Sub

    On Error Resume Next
    wsTpl.Cells.Font.Name = "Meiryo UI"
    On Error GoTo 0

    wsTpl.Cells.Font.Size = 9
    wsTpl.Cells.WrapText = True
    wsTpl.Cells.VerticalAlignment = xlTop
    wsTpl.Cells.HorizontalAlignment = xlLeft
    wsTpl.Cells.NumberFormat = "@"

    Dim c As Long
    For c = 1 To 60
        If (c Mod 2) = 1 Then
            wsTpl.Columns(c).ColumnWidth = 22
        Else
            wsTpl.Columns(c).ColumnWidth = 34
        End If
    Next c
End Sub

Private Sub SetupTemplateConfigSheet(ByVal wsCfg As Worksheet)
    Dim found As Range
    On Error Resume Next
    Set found = wsCfg.Cells.Find(What:=MARKER_PREFIX, LookIn:=xlValues, LookAt:=xlPart, MatchCase:=False)
    On Error GoTo 0

    If found Is Nothing Then
        wsCfg.Cells.Clear
        SeedDefaultTemplateConfig wsCfg
    End If
End Sub

Public Sub LoadXmlFromFile()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    Dim filePath As Variant
    filePath = Application.GetOpenFilename("XML Files (*.xml),*.xml,All Files (*.*),*.*", , U("Ch\u1ECDn file XML export ABAP Objects"))
    If VarType(filePath) = vbBoolean And filePath = False Then Exit Sub

    WriteXmlInputToSheet wsIn, ReadTextFile(CStr(filePath))
    LoadObjectListFromXml
End Sub

Public Sub ResetTemplateConfig()
    Dim wsCfg As Worksheet
    Set wsCfg = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE_CONFIG)

    If MsgBox(U("Kh\u00F4i ph\u1EE5c c\u1EA5u h\u00ECnh template v\u1EC1 m\u1EB7c \u0111\u1ECBnh? Thao t\u00E1c s\u1EBD x\u00F3a n\u1ED9i dung sheet."), vbQuestion + vbYesNo) <> vbYes Then Exit Sub

    wsCfg.Cells.Clear
    SeedDefaultTemplateConfig wsCfg

    MsgBox U("\u0110\u00E3 kh\u00F4i ph\u1EE5c c\u1EA5u h\u00ECnh template."), vbInformation
End Sub

Private Function ReadXmlInputFromSheet(ByVal wsIn As Worksheet) As String
    Dim lastRow As Long
    lastRow = wsIn.Cells(wsIn.Rows.Count, "A").End(xlUp).Row
    If lastRow < 2 Then Exit Function

    Dim rng As Range
    Set rng = wsIn.Range(wsIn.Cells(2, 1), wsIn.Cells(lastRow, 1))

    Dim vals As Variant
    vals = rng.Value2

    If Not IsArray(vals) Then
        If IsEmpty(vals) Then Exit Function
        ReadXmlInputFromSheet = CStr(vals)
        Exit Function
    End If

    Dim rowCount As Long
    rowCount = UBound(vals, 1)
    If rowCount <= 0 Then Exit Function

    Dim lines() As String
    ReDim lines(0 To rowCount - 1) As String

    Dim i As Long
    For i = 1 To rowCount
        If IsError(vals(i, 1)) Then
            lines(i - 1) = ""
        ElseIf IsEmpty(vals(i, 1)) Then
            lines(i - 1) = ""
        Else
            lines(i - 1) = CStr(vals(i, 1))
        End If
    Next i

    ReadXmlInputFromSheet = Join(lines, vbCrLf)
End Function

Private Sub WriteXmlInputToSheet(ByVal wsIn As Worksheet, ByVal xmlText As String)
    Dim normalized As String
    normalized = CStr(xmlText)
    normalized = Replace(normalized, vbCrLf, vbLf)
    normalized = Replace(normalized, vbCr, vbLf)

    Dim parts() As String
    parts = Split(normalized, vbLf)

    Dim lastRow As Long
    lastRow = wsIn.Cells(wsIn.Rows.Count, "A").End(xlUp).Row
    If lastRow < 2 Then lastRow = 2
    wsIn.Range("A2:A" & CStr(lastRow)).ClearContents

    Dim i As Long
    For i = 0 To UBound(parts)
        wsIn.Cells(2 + i, 1).Value2 = parts(i)
    Next i
End Sub

Private Sub EnsureObjectListHeader(ByVal wsIn As Worksheet)
    Dim r As Long
    r = LIST_HEADER_ROW

    wsIn.Cells(r, LIST_COL_SELECT).Value2 = U("Ch\u1ECDn")
    wsIn.Cells(r, LIST_COL_INDEX).Value2 = "STT"
    wsIn.Cells(r, LIST_COL_OBJECT_ID).Value2 = "Object ID"
    wsIn.Cells(r, LIST_COL_TEMPLATE_ID).Value2 = "Template ID"
    wsIn.Cells(r, LIST_COL_KIND).Value2 = U("Lo\u1EA1i")
    wsIn.Cells(r, LIST_COL_ROUTINE).Value2 = "Routine"
    wsIn.Cells(r, LIST_COL_LINE).Value2 = U("D\u00F2ng")
    wsIn.Cells(r, LIST_COL_ORIGINAL).Value2 = U("C\u00E2u l\u1EC7nh")
    wsIn.Cells(r, LIST_COL_DEPTH).Value2 = "Depth"

    Dim c As Long
    For c = LIST_COL_SELECT To LIST_COL_LAST
        wsIn.Cells(r, c).Font.Bold = True
    Next c
End Sub

Private Sub ClearObjectList(ByVal wsIn As Worksheet, Optional ByVal clearHeader As Boolean = False)
    Dim lastRow As Long
    lastRow = wsIn.Cells(wsIn.Rows.Count, LIST_COL_INDEX).End(xlUp).Row
    If lastRow < LIST_HEADER_ROW Then lastRow = LIST_HEADER_ROW

    Dim startRow As Long
    If clearHeader Then
        startRow = LIST_HEADER_ROW
    Else
        startRow = LIST_FIRST_ROW
    End If

    wsIn.Range(wsIn.Cells(startRow, LIST_COL_SELECT), wsIn.Cells(lastRow, LIST_COL_LAST)).ClearContents
End Sub

Private Sub UpdateStatus(ByVal wsIn As Worksheet, ByVal text As String)
    If wsIn Is Nothing Then Exit Sub
    wsIn.Range(STATUS_CELL).Value2 = text
    DoEvents
End Sub

Private Function NormalizeSingleLine(ByVal text As String, ByVal maxLen As Long) As String
    Dim s As String
    s = CStr(text)
    s = Replace(s, vbCrLf, " ")
    s = Replace(s, vbCr, " ")
    s = Replace(s, vbLf, " ")
    s = Trim$(s)

    If maxLen > 0 And Len(s) > maxLen Then
        If maxLen > 3 Then
            s = Left$(s, maxLen - 3) & "..."
        Else
            s = Left$(s, maxLen)
        End If
    End If

    NormalizeSingleLine = s
End Function

Public Sub LoadObjectListFromXml()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    EnsureObjectListHeader wsIn

    Dim inputText As String
    inputText = ReadXmlInputFromSheet(wsIn)
    If Len(Trim$(inputText)) = 0 Then
        MsgBox U("XML tr\u1ED1ng. D\u00E1n XML export v\u00E0o c\u1ED9t A t\u1EEB A2 ho\u1EB7c b\u1EA5m 'N\u1EA1p XML'."), vbExclamation
        Exit Sub
    End If

    Dim doc As Object
    Dim rootObjects As Object
    If Not TryLoadObjectsExportXml(inputText, doc, rootObjects) Then Exit Sub

    Dim flatNodes As Collection
    Dim flatDepths As Collection
    Set flatNodes = New Collection
    Set flatDepths = New Collection
    FlattenObjects rootObjects, flatNodes, flatDepths

    ClearObjectList wsIn

    Dim i As Long
    For i = 1 To flatNodes.Count
        Dim objNode As Object
        Set objNode = flatNodes.Item(i)
        If objNode Is Nothing Then GoTo NextRow

        Dim row As Long
        row = LIST_FIRST_ROW + (i - 1)

        Dim objectId As String
        objectId = SafeChildText(objNode, "id")

        Dim objectType As String
        objectType = SafeChildText(objNode, "objectType")

        Dim lineStart As String
        lineStart = SafeChildText(objNode, "lineStart")

        Dim originalText As String
        originalText = SafeChildText(objNode, "raw")

        wsIn.Cells(row, LIST_COL_INDEX).Value2 = i
        wsIn.Cells(row, LIST_COL_OBJECT_ID).Value2 = objectId
        wsIn.Cells(row, LIST_COL_TEMPLATE_ID).Value2 = objectType
        wsIn.Cells(row, LIST_COL_KIND).Value2 = objectType
        wsIn.Cells(row, LIST_COL_ROUTINE).Value2 = GetRoutineKeyFromObjectNode(objNode)
        wsIn.Cells(row, LIST_COL_LINE).Value2 = lineStart
        wsIn.Cells(row, LIST_COL_ORIGINAL).Value2 = NormalizeSingleLine(originalText, MAX_LIST_ORIGINAL)
        wsIn.Cells(row, LIST_COL_DEPTH).Value2 = CLng(flatDepths.Item(i))

NextRow:
    Next i

    UpdateStatus wsIn, U("\u0110\u00E3 t\u1EA3i ") & CStr(flatNodes.Count) & U(" \u0111\u1ED1i t\u01B0\u1EE3ng.")
End Sub

Public Sub SelectAllObjects()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    Dim lastRow As Long
    lastRow = wsIn.Cells(wsIn.Rows.Count, LIST_COL_INDEX).End(xlUp).Row
    If lastRow < LIST_FIRST_ROW Then
        UpdateStatus wsIn, U("Ch\u01B0a c\u00F3 danh s\u00E1ch \u0111\u1EC3 ch\u1ECDn.")
        Exit Sub
    End If

    wsIn.Range(wsIn.Cells(LIST_FIRST_ROW, LIST_COL_SELECT), wsIn.Cells(lastRow, LIST_COL_SELECT)).Value2 = "x"
    UpdateStatus wsIn, U("\u0110\u00E3 ch\u1ECDn t\u1EA5t c\u1EA3.")
End Sub

Public Sub ClearSelectedObjects()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    Dim lastRow As Long
    lastRow = wsIn.Cells(wsIn.Rows.Count, LIST_COL_INDEX).End(xlUp).Row
    If lastRow < LIST_FIRST_ROW Then
        UpdateStatus wsIn, U("Ch\u01B0a c\u00F3 danh s\u00E1ch \u0111\u1EC3 b\u1ECF ch\u1ECDn.")
        Exit Sub
    End If

    wsIn.Range(wsIn.Cells(LIST_FIRST_ROW, LIST_COL_SELECT), wsIn.Cells(lastRow, LIST_COL_SELECT)).ClearContents
    UpdateStatus wsIn, U("\u0110\u00E3 b\u1ECF ch\u1ECDn.")
End Sub

Private Function IsSelectedFlag(ByVal value As Variant) As Boolean
    If IsEmpty(value) Then Exit Function
    If VarType(value) = vbBoolean Then
        IsSelectedFlag = (value = True)
        Exit Function
    End If

    Dim s As String
    s = LCase$(Trim$(CStr(value)))

    If s = "x" Or s = "1" Or s = "y" Or s = "yes" Or s = "true" Or s = "chon" Or s = U("ch\u1ECDn") Then
        IsSelectedFlag = True
    End If
End Function

Private Sub AddUniqueLong(ByRef col As Collection, ByVal value As Long)
    Dim v As Variant
    For Each v In col
        If CLng(v) = value Then Exit Sub
    Next v
    col.Add value
End Sub

Private Function GetSelectedObjectIndexes(ByVal wsIn As Worksheet) As Collection
    Dim out As New Collection
    Dim lastRow As Long
    lastRow = wsIn.Cells(wsIn.Rows.Count, LIST_COL_INDEX).End(xlUp).Row
    If lastRow < LIST_FIRST_ROW Then
        Set GetSelectedObjectIndexes = out
        Exit Function
    End If

    Dim r As Long
    For r = LIST_FIRST_ROW To lastRow
        If IsSelectedFlag(wsIn.Cells(r, LIST_COL_SELECT).Value2) Then
            Dim idx As Long
            idx = CLng(Val(CStr(wsIn.Cells(r, LIST_COL_INDEX).Value2)))
            If idx > 0 Then AddUniqueLong out, idx
        End If
    Next r

    Set GetSelectedObjectIndexes = out
End Function

Private Function BuildObjectIndexArray(ByVal objectCount As Long, ByVal selected As Collection) As Variant
    Dim arr() As Long
    Dim i As Long

    If selected Is Nothing Or selected.Count = 0 Then
        If objectCount < 1 Then
            BuildObjectIndexArray = Array()
            Exit Function
        End If
        ReDim arr(1 To objectCount)
        For i = 1 To objectCount
            arr(i) = i
        Next i
    Else
        ReDim arr(1 To selected.Count)
        i = 1
        Dim v As Variant
        For Each v In selected
            arr(i) = CLng(v)
            i = i + 1
        Next v
    End If

    BuildObjectIndexArray = arr
End Function

Public Sub GenerateAllTemplatesFromXml()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    Dim wsCfg As Worksheet
    Set wsCfg = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE_CONFIG)

    Dim wsTpl As Worksheet
    Set wsTpl = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE)

    Dim inputText As String
    inputText = ReadXmlInputFromSheet(wsIn)
    If Len(Trim$(inputText)) = 0 Then
        MsgBox U("XML tr\u1ED1ng. D\u00E1n XML export v\u00E0o c\u1ED9t A t\u1EEB A2 ho\u1EB7c b\u1EA5m 'N\u1EA1p XML'."), vbExclamation
        Exit Sub
    End If

    Dim doc As Object
    Dim rootObjects As Object
    If Not TryLoadObjectsExportXml(inputText, doc, rootObjects) Then Exit Sub

    Dim flatNodes As Collection
    Dim flatDepths As Collection
    Set flatNodes = New Collection
    Set flatDepths = New Collection
    FlattenObjects rootObjects, flatNodes, flatDepths

    Dim selectedIdx As Collection
    Set selectedIdx = GetSelectedObjectIndexes(wsIn)

    Dim indexList As Variant
    indexList = BuildObjectIndexArray(flatNodes.Count, selectedIdx)

    Dim totalCount As Long
    On Error Resume Next
    totalCount = UBound(indexList) - LBound(indexList) + 1
    On Error GoTo 0
    If totalCount <= 0 Then
        MsgBox U("Kh\u00F4ng c\u00F3 \u0111\u1ED1i t\u01B0\u1EE3ng \u0111\u1EC3 t\u1EA1o."), vbExclamation
        Exit Sub
    End If

    Dim prevCalc As XlCalculation
    prevCalc = Application.Calculation

    On Error GoTo Cleanup
    Application.ScreenUpdating = False
    Application.EnableEvents = False
    Application.Calculation = xlCalculationManual

    wsTpl.Cells.Clear
    SetupTemplateSheet wsTpl
    UpdateStatus wsIn, U("\u0110ang t\u1EA1o ") & CStr(totalCount) & U(" \u0111\u1ED1i t\u01B0\u1EE3ng...")

    Dim rowPtr As Long
    rowPtr = 1

    Dim sampleCache As Object
    Set sampleCache = CreateObject("Scripting.Dictionary")
    sampleCache.CompareMode = 1 ' TextCompare

    Dim idxLower As Long
    Dim idxUpper As Long
    idxLower = LBound(indexList)
    idxUpper = UBound(indexList)

    Dim i As Long
    Dim stepIndex As Long
    Dim processed As Long

    For i = idxLower To idxUpper
        stepIndex = i - idxLower + 1

        Dim objIndex As Long
        objIndex = CLng(indexList(i))
        If objIndex < 1 Or objIndex > flatNodes.Count Then GoTo NextObj

        Dim objNode As Object
        Set objNode = flatNodes.Item(objIndex)
        If objNode Is Nothing Then GoTo NextObj

        processed = processed + 1

        Dim templateKey As String
        templateKey = Trim$(SafeChildText(objNode, "objectType"))

        Dim depth As Long
        depth = CLng(flatDepths.Item(objIndex))
        If depth < 0 Then depth = 0

        Dim colPtr As Long
        colPtr = 1 + depth * INDENT_COLS_PER_DEPTH
        If colPtr < 1 Then colPtr = 1

        Dim routineKey As String
        routineKey = GetRoutineKeyFromObjectNode(objNode)

        Dim originalText As String
        originalText = SafeChildText(objNode, "raw")
        originalText = Replace(originalText, vbCrLf, " ")
        originalText = Replace(originalText, vbCr, " ")
        originalText = Replace(originalText, vbLf, " ")
        originalText = Trim$(originalText)

        Dim header As String
        header = "#" & CStr(objIndex) & " depth=" & CStr(depth) & " " & templateKey
        If Len(routineKey) > 0 Then header = header & " | " & routineKey
        If Len(originalText) > 0 Then header = header & " | " & originalText
        If Len(header) > MAX_HEADER_TEXT Then header = Left$(header, MAX_HEADER_TEXT - 3) & "..."

        Dim headerRange As Range
        Set headerRange = wsTpl.Range(wsTpl.Cells(rowPtr, colPtr), wsTpl.Cells(rowPtr, colPtr + 1))
        headerRange.Merge
        headerRange.Value2 = header
        headerRange.Font.Bold = True
        headerRange.Interior.Color = RGB(242, 242, 242)
        headerRange.Font.Color = RGB(0, 0, 0)
        headerRange.HorizontalAlignment = xlLeft
        headerRange.VerticalAlignment = xlCenter
        headerRange.Borders.LineStyle = xlContinuous
        headerRange.Borders.Weight = xlThin
        headerRange.Borders.Color = RGB(200, 200, 200)
        wsTpl.Rows(rowPtr).RowHeight = 20
        rowPtr = rowPtr + 1

        Dim sample As Range
        If sampleCache.Exists(templateKey) Then
            Set sample = sampleCache(templateKey)
        Else
            Set sample = GetTemplateSampleRange(wsCfg, templateKey)
            If sample Is Nothing Then Set sample = GetTemplateSampleRange(wsCfg, "DEFAULT")
            If Not sample Is Nothing Then sampleCache.Add templateKey, sample
        End If

        If sample Is Nothing Then
            wsTpl.Cells(rowPtr, colPtr).Value2 = U("[Thi\u1EBFu c\u1EA5u h\u00ECnh template] ") & templateKey
            rowPtr = rowPtr + BLOCK_GAP_ROWS
            GoTo NextObj
        End If

        Dim dest As Range
        Set dest = wsTpl.Cells(rowPtr, colPtr)
        CopyRangeWithRowHeights sample, dest

        Dim outRange As Range
        Set outRange = wsTpl.Range(dest, dest.Offset(sample.Rows.Count - 1, sample.Columns.Count - 1))
        Dim removeEmpty As Boolean
        removeEmpty = ShouldRemoveEmptyRows(wsCfg, templateKey)
        Dim removeAdv As Boolean
        removeAdv = ShouldRemoveEmptyRowsAdvanced(wsCfg, templateKey)

        Dim rowInfo As Object
        If removeAdv Then Set rowInfo = CreateObject("Scripting.Dictionary")

        FillRangePlaceholders outRange, objNode, rowInfo

        Dim removed As Long
        If removeAdv Then
            removed = RemoveEmptyRowsInRangeAdvanced(outRange, rowInfo)
        ElseIf removeEmpty Then
            removed = RemoveEmptyRowsInRange(outRange)
        Else
            removed = 0
        End If

        rowPtr = rowPtr + (sample.Rows.Count - removed) + BLOCK_GAP_ROWS

NextObj:
        If stepIndex Mod 20 = 0 Or stepIndex = totalCount Then
            UpdateStatus wsIn, U("\u0110ang t\u1EA1o ") & CStr(stepIndex) & "/" & CStr(totalCount) & "..."
        End If
    Next i

    Application.CutCopyMode = False

    If selectedIdx Is Nothing Or selectedIdx.Count = 0 Then
        UpdateStatus wsIn, U("\u0110\u00E3 t\u1EA1o ") & CStr(processed) & U(" \u0111\u1ED1i t\u01B0\u1EE3ng.")
    Else
        UpdateStatus wsIn, U("\u0110\u00E3 t\u1EA1o ") & CStr(processed) & "/" & CStr(totalCount) & U(" \u0111\u1ED1i t\u01B0\u1EE3ng \u0111\u00E3 ch\u1ECDn.")
    End If

Cleanup:
    Application.Calculation = prevCalc
    Application.EnableEvents = True
    Application.ScreenUpdating = True

    If Err.Number <> 0 Then
        MsgBox U("T\u1EA1o to\u00E0n b\u1ED9 th\u1EA5t b\u1EA1i: ") & Err.Description, vbCritical
    End If
End Sub

Private Function TryLoadObjectsExportXml(ByVal xmlText As String, ByRef outDoc As Object, ByRef outObjectNodes As Object) As Boolean
    On Error GoTo Fail

    Dim doc As Object
    Set doc = CreateObject("MSXML2.DOMDocument.6.0")

    doc.async = False
    doc.validateOnParse = False
    doc.resolveExternals = False
    doc.setProperty "SelectionLanguage", "XPath"

    If Not doc.LoadXML(xmlText) Then Err.Raise vbObjectError + 3000, , U("XML kh\u00F4ng h\u1EE3p l\u1EC7.")

    Dim rootName As String
    rootName = LCase$(CStr(doc.documentElement.nodeName))
    If rootName <> "abapflowobjects" Then Err.Raise vbObjectError + 3001, , U("Sai ph\u1EA7n t\u1EED g\u1ED1c: ") & doc.documentElement.nodeName

    Dim nodes As Object
    Set nodes = doc.SelectNodes("/abapflowObjects/objects/object")
    If nodes Is Nothing Or nodes.Length = 0 Then Err.Raise vbObjectError + 3002, , U("Kh\u00F4ng t\u00ECm th\u1EA5y n\u00FAt <objects>/<object>.")

    Set outDoc = doc
    Set outObjectNodes = nodes
    TryLoadObjectsExportXml = True
    Exit Function

Fail:
    MsgBox U("Kh\u00F4ng \u0111\u1ECDc \u0111\u01B0\u1EE3c XML: ") & Err.Description, vbCritical
    TryLoadObjectsExportXml = False
End Function

Private Function PickObjectNode(ByVal objectNodes As Object, ByVal index1 As Long) As Object
    On Error Resume Next
    If objectNodes Is Nothing Then Exit Function

    Dim idx As Long
    idx = index1
    If idx < 1 Then idx = 1
    If idx > objectNodes.Length Then idx = objectNodes.Length

    Set PickObjectNode = objectNodes.Item(idx - 1)
End Function

Private Function SafeAttr(ByVal node As Object, ByVal attrName As String) As String
    On Error Resume Next
    SafeAttr = CStr(node.getAttribute(attrName))
    On Error GoTo 0
End Function

Private Function SafeChildText(ByVal node As Object, ByVal childName As String) As String
    On Error Resume Next
    Dim c As Object
    Set c = node.selectSingleNode(childName)
    If c Is Nothing Then
        SafeChildText = vbNullString
    Else
        SafeChildText = CStr(c.text)
    End If
    On Error GoTo 0
End Function

Private Sub FlattenObjects(ByVal rootObjects As Object, ByRef outFlatNodes As Collection, ByRef outDepths As Collection)
    If rootObjects Is Nothing Then Exit Sub
    If outFlatNodes Is Nothing Then Exit Sub
    If outDepths Is Nothing Then Exit Sub

    Dim i As Long
    On Error Resume Next
    For i = 0 To rootObjects.Length - 1
        Dim n As Object
        Set n = rootObjects.Item(i)
        If Not n Is Nothing Then
            FlattenObjectNode n, 0, outFlatNodes, outDepths
        End If
    Next i
    On Error GoTo 0
End Sub

Private Sub FlattenObjectNode(ByVal objNode As Object, ByVal depth As Long, ByRef outFlatNodes As Collection, ByRef outDepths As Collection)
    If objNode Is Nothing Then Exit Sub

    outFlatNodes.Add objNode
    outDepths.Add depth

    Dim childrenNode As Object
    Set childrenNode = objNode.selectSingleNode("children")
    If childrenNode Is Nothing Then Exit Sub

    Dim child As Object
    For Each child In childrenNode.childNodes
        If Not child Is Nothing Then
            If child.nodeType = 1 Then
                FlattenObjectNode child, depth + 1, outFlatNodes, outDepths
            End If
        End If
    Next child
End Sub

Private Function GetRoutineKeyFromObjectNode(ByVal objNode As Object) As String
    On Error Resume Next
    If objNode Is Nothing Then Exit Function

    ' 1) Try common fields in values: name/target/form (new map shape)
    Dim v1 As String

    v1 = Trim$(SafeChildText(objNode, "values/name/value"))
    If Len(v1) = 0 Then v1 = Trim$(SafeChildText(objNode, "values/name/item[1]/value"))
    If Len(v1) > 0 Then
        GetRoutineKeyFromObjectNode = v1
        Exit Function
    End If

    v1 = Trim$(SafeChildText(objNode, "values/target/value"))
    If Len(v1) = 0 Then v1 = Trim$(SafeChildText(objNode, "values/target/item[1]/value"))
    If Len(v1) > 0 Then
        GetRoutineKeyFromObjectNode = v1
        Exit Function
    End If

    v1 = Trim$(SafeChildText(objNode, "values/form/value"))
    If Len(v1) = 0 Then v1 = Trim$(SafeChildText(objNode, "values/form/item[1]/value"))
    If Len(v1) > 0 Then
        GetRoutineKeyFromObjectNode = v1
        Exit Function
    End If

    ' 1b) Backward compatible: old array-ish iteration (also works for new shape when not nested arrays)
    Dim valuesNode As Object
    Set valuesNode = objNode.selectSingleNode("values")
    If Not valuesNode Is Nothing Then
        Dim item As Object
        For Each item In valuesNode.childNodes
            If item.nodeType <> 1 Then GoTo NextValueItem

            Dim k As String
            k = LCase$(Trim$(SafeChildText(item, "name")))

            If k = "name" Or k = "target" Or k = "form" Then
                Dim v As String
                v = Trim$(SafeChildText(item, "value"))
                If Len(v) > 0 Then
                    GetRoutineKeyFromObjectNode = v
                    Exit Function
                End If
            End If

NextValueItem:
        Next item
    End If

    ' 2) Fallback by extras
    Dim extras As Object
    Set extras = objNode.selectSingleNode("extras")

    If Not extras Is Nothing Then
        Dim cf As Object
        Set cf = extras.selectSingleNode("callFunction")
        If Not cf Is Nothing Then
            GetRoutineKeyFromObjectNode = Trim$(SafeChildText(cf, "name"))
            If Len(GetRoutineKeyFromObjectNode) > 0 Then Exit Function
        End If

        Dim cm As Object
        Set cm = extras.selectSingleNode("callMethod")
        If Not cm Is Nothing Then
            GetRoutineKeyFromObjectNode = Trim$(SafeChildText(cm, "target"))
            If Len(GetRoutineKeyFromObjectNode) > 0 Then Exit Function
        End If

        Dim pc As Object
        Set pc = extras.selectSingleNode("performCall")
        If Not pc Is Nothing Then
            GetRoutineKeyFromObjectNode = Trim$(SafeChildText(pc, "form"))
            If Len(GetRoutineKeyFromObjectNode) > 0 Then Exit Function
        End If

        Dim f As Object
        Set f = extras.selectSingleNode("form")
        If Not f Is Nothing Then
            GetRoutineKeyFromObjectNode = Trim$(SafeChildText(f, "name"))
            If Len(GetRoutineKeyFromObjectNode) > 0 Then Exit Function
        End If

        Dim ms As Object
        Set ms = extras.selectSingleNode("methodSignature")
        If Not ms Is Nothing Then
            GetRoutineKeyFromObjectNode = Trim$(SafeChildText(ms, "name"))
            If Len(GetRoutineKeyFromObjectNode) > 0 Then Exit Function
        End If
    End If

    On Error GoTo 0
End Function

Private Function MakeMarker(ByVal templateId As String, ByVal corner As String) As String
    MakeMarker = MARKER_PREFIX & CStr(templateId) & "_" & UCase$(CStr(corner)) & "__"
End Function

Private Function GetTemplateSampleRange(ByVal wsCfg As Worksheet, ByVal templateKey As String) As Range
    Dim tlMarker As String, trMarker As String, blMarker As String, brMarker As String
    tlMarker = MakeMarker(templateKey, "TL")
    trMarker = MakeMarker(templateKey, "TR")
    blMarker = MakeMarker(templateKey, "BL")
    brMarker = MakeMarker(templateKey, "BR")

    Dim tl As Range, tr As Range, bl As Range, br As Range
    Set tl = FindMarkerCell(wsCfg, tlMarker)
    Set tr = FindMarkerCell(wsCfg, trMarker)
    Set bl = FindMarkerCell(wsCfg, blMarker)
    Set br = FindMarkerCell(wsCfg, brMarker)

    If tl Is Nothing Or tr Is Nothing Or bl Is Nothing Or br Is Nothing Then Exit Function

    If tr.Row <> tl.Row Then Exit Function
    If bl.Column <> tl.Column Then Exit Function
    If br.Row <> bl.Row Then Exit Function
    If br.Column <> tr.Column Then Exit Function

    If br.Row - tl.Row < 2 Then Exit Function
    If br.Column - tl.Column < 2 Then Exit Function

    Set GetTemplateSampleRange = wsCfg.Range(tl.Offset(1, 1), br.Offset(-1, -1))
End Function

Private Function FindMarkerCell(ByVal ws As Worksheet, ByVal markerText As String) As Range
    On Error Resume Next
    Set FindMarkerCell = ws.Cells.Find(What:=markerText, LookIn:=xlValues, LookAt:=xlWhole, MatchCase:=False)
    On Error GoTo 0
End Function

Private Sub CopyRangeWithDimensions(ByVal src As Range, ByVal dstTopLeft As Range)
    src.Copy dstTopLeft

    Dim c As Long
    For c = 1 To src.Columns.Count
        dstTopLeft.Worksheet.Columns(dstTopLeft.Column + c - 1).ColumnWidth = src.Worksheet.Columns(src.Column + c - 1).ColumnWidth
    Next c

    Dim r As Long
    For r = 1 To src.Rows.Count
        dstTopLeft.Worksheet.Rows(dstTopLeft.Row + r - 1).RowHeight = src.Worksheet.Rows(src.Row + r - 1).RowHeight
    Next r
End Sub

Private Sub CopyRangeWithRowHeights(ByVal src As Range, ByVal dstTopLeft As Range)
    src.Copy dstTopLeft

    Dim r As Long
    For r = 1 To src.Rows.Count
        dstTopLeft.Worksheet.Rows(dstTopLeft.Row + r - 1).RowHeight = src.Worksheet.Rows(src.Row + r - 1).RowHeight
    Next r
End Sub

Private Sub FillRangePlaceholders(ByVal targetRange As Range, ByVal contextNode As Object, Optional ByVal rowInfo As Object = Nothing)
    Dim cell As Range
    For Each cell In targetRange.Cells
        If VarType(cell.Value2) <> vbString Then GoTo NextCell

        Dim raw As String
        raw = CStr(cell.Value2)

        Dim hasPlaceholder As Boolean
        hasPlaceholder = (InStr(1, raw, "{", vbBinaryCompare) > 0 And InStr(1, raw, "}", vbBinaryCompare) > 0)

        Dim tokenHasValue As Boolean
        tokenHasValue = False

        If hasPlaceholder Then
            Dim replaced As String
            replaced = ReplacePlaceholders(raw, contextNode, tokenHasValue)
            If replaced <> raw Then cell.Value2 = replaced
        End If

        If Not rowInfo Is Nothing Then
            UpdateRowInfo rowInfo, cell.Row, hasPlaceholder, tokenHasValue
        End If

NextCell:
    Next cell
End Sub

Private Function ShouldRemoveEmptyRows(ByVal wsCfg As Worksheet, ByVal templateKey As String) As Boolean
    ShouldRemoveEmptyRows = True

    Dim tlMarker As String, trMarker As String
    tlMarker = MakeMarker(templateKey, "TL")
    trMarker = MakeMarker(templateKey, "TR")

    Dim tl As Range, tr As Range
    Set tl = FindMarkerCell(wsCfg, tlMarker)
    Set tr = FindMarkerCell(wsCfg, trMarker)
    If tl Is Nothing Or tr Is Nothing Then Exit Function
    If tl.Row <> tr.Row Then Exit Function

    Dim c As Long
    For c = tl.Column + 1 To tr.Column - 1
        Dim v As Variant
        v = wsCfg.Cells(tl.Row, c).Value2
        If VarType(v) = vbString Then
            Dim flag As Boolean
            If ParseBoolOption(CStr(v), "removeEmptyRows", flag) Then
                ShouldRemoveEmptyRows = flag
                Exit Function
            End If
            If ParseBoolOption(CStr(v), "compact.removeEmptyRows", flag) Then
                ShouldRemoveEmptyRows = flag
                Exit Function
            End If
        End If
    Next c
End Function

Private Function ShouldRemoveEmptyRowsAdvanced(ByVal wsCfg As Worksheet, ByVal templateKey As String) As Boolean
    ShouldRemoveEmptyRowsAdvanced = True

    Dim tlMarker As String, trMarker As String
    tlMarker = MakeMarker(templateKey, "TL")
    trMarker = MakeMarker(templateKey, "TR")

    Dim tl As Range, tr As Range
    Set tl = FindMarkerCell(wsCfg, tlMarker)
    Set tr = FindMarkerCell(wsCfg, trMarker)
    If tl Is Nothing Or tr Is Nothing Then Exit Function
    If tl.Row <> tr.Row Then Exit Function

    Dim c As Long
    For c = tl.Column + 1 To tr.Column - 1
        Dim v As Variant
        v = wsCfg.Cells(tl.Row, c).Value2
        If VarType(v) = vbString Then
            Dim flag As Boolean
            If ParseBoolOption(CStr(v), "removeEmptyRowsAdvanced", flag) Then
                ShouldRemoveEmptyRowsAdvanced = flag
                Exit Function
            End If
            If ParseBoolOption(CStr(v), "removeEmptyRowsAdv", flag) Then
                ShouldRemoveEmptyRowsAdvanced = flag
                Exit Function
            End If
            If ParseBoolOption(CStr(v), "compact.removeEmptyRowsAdvanced", flag) Then
                ShouldRemoveEmptyRowsAdvanced = flag
                Exit Function
            End If
            If ParseBoolOption(CStr(v), "compact.removeEmptyRowsAdv", flag) Then
                ShouldRemoveEmptyRowsAdvanced = flag
                Exit Function
            End If
        End If
    Next c
End Function

Private Function ParseBoolOption(ByVal text As String, ByVal key As String, ByRef outValue As Boolean) As Boolean
    Dim s As String
    s = LCase$(Trim$(CStr(text)))
    If Len(s) = 0 Then Exit Function

    Dim k As String
    k = LCase$(Trim$(CStr(key)))
    If Len(k) = 0 Then Exit Function

    If s = k Then
        outValue = True
        ParseBoolOption = True
        Exit Function
    End If

    If Left$(s, Len(k)) <> k Then Exit Function

    Dim rest As String
    rest = Trim$(Mid$(s, Len(k) + 1))
    If Len(rest) = 0 Then
        outValue = True
        ParseBoolOption = True
        Exit Function
    End If

    Dim firstCh As String
    firstCh = Left$(rest, 1)
    If firstCh = "=" Or firstCh = ":" Then rest = Trim$(Mid$(rest, 2))

    If IsTruthy(rest) Then
        outValue = True
        ParseBoolOption = True
        Exit Function
    End If
    If IsFalsy(rest) Then
        outValue = False
        ParseBoolOption = True
        Exit Function
    End If
End Function

Private Function IsTruthy(ByVal text As String) As Boolean
    Dim s As String
    s = LCase$(Trim$(CStr(text)))
    If s = "1" Or s = "true" Or s = "yes" Or s = "y" Or s = "on" Then IsTruthy = True
End Function

Private Function IsFalsy(ByVal text As String) As Boolean
    Dim s As String
    s = LCase$(Trim$(CStr(text)))
    If s = "0" Or s = "false" Or s = "no" Or s = "n" Or s = "off" Then IsFalsy = True
End Function

Private Function RemoveEmptyRowsInRange(ByVal targetRange As Range) As Long
    Dim removed As Long
    Dim r As Long
    Dim deleteRange As Range

    For r = targetRange.Rows.Count To 1 Step -1
        Dim rowRange As Range
        Set rowRange = targetRange.Rows(r)
        If IsRowBlank(rowRange) Then
            If deleteRange Is Nothing Then
                Set deleteRange = rowRange.EntireRow
            Else
                Set deleteRange = Union(deleteRange, rowRange.EntireRow)
            End If
            removed = removed + 1
        End If
    Next r

    If Not deleteRange Is Nothing Then deleteRange.Delete
    RemoveEmptyRowsInRange = removed
End Function

Private Function RemoveEmptyRowsInRangeAdvanced(ByVal targetRange As Range, ByVal rowInfo As Object) As Long
    Dim removed As Long
    Dim r As Long
    Dim deleteRange As Range

    For r = targetRange.Rows.Count To 1 Step -1
        Dim rowRange As Range
        Set rowRange = targetRange.Rows(r)

        Dim removable As Boolean
        removable = IsRowBlank(rowRange)

        If Not removable And Not rowInfo Is Nothing Then
            Dim key As String
            key = CStr(rowRange.Row)
            If rowInfo.Exists(key) Then
                Dim info As Object
                Set info = rowInfo(key)
                If info.Exists("hasPlaceholder") Then
                    If info("hasPlaceholder") = True Then
                        If Not info.Exists("hasValue") Or info("hasValue") <> True Then
                            removable = True
                        End If
                    End If
                End If
            End If
        End If

        If removable Then
            If deleteRange Is Nothing Then
                Set deleteRange = rowRange.EntireRow
            Else
                Set deleteRange = Union(deleteRange, rowRange.EntireRow)
            End If
            removed = removed + 1
        End If
    Next r

    If Not deleteRange Is Nothing Then deleteRange.Delete
    RemoveEmptyRowsInRangeAdvanced = removed
End Function

Private Function IsRowBlank(ByVal rowRange As Range) As Boolean
    Dim cell As Range
    For Each cell In rowRange.Cells
        Dim v As Variant
        v = cell.Value2
        If IsError(v) Then
            IsRowBlank = False
            Exit Function
        End If
        If IsEmpty(v) Then GoTo NextCell
        If VarType(v) = vbString Then
            If Len(Trim$(CStr(v))) > 0 Then
                IsRowBlank = False
                Exit Function
            End If
        Else
            IsRowBlank = False
            Exit Function
        End If
NextCell:
    Next cell
    IsRowBlank = True
End Function

Private Function ReplacePlaceholders(ByVal text As String, ByVal contextNode As Object, Optional ByRef anyTokenValue As Boolean = False) As String
    Dim s As String
    s = CStr(text)

    Dim out As String
    out = vbNullString

    Dim pos As Long
    pos = 1

    Do While pos <= Len(s)
        Dim openPos As Long
        openPos = InStr(pos, s, "{", vbBinaryCompare)
        If openPos = 0 Then
            out = out & Mid$(s, pos)
            Exit Do
        End If

        out = out & Mid$(s, pos, openPos - pos)

        Dim closePos As Long
        closePos = InStr(openPos + 1, s, "}", vbBinaryCompare)
        If closePos = 0 Then
            out = out & Mid$(s, openPos)
            Exit Do
        End If

        Dim token As String
        token = Trim$(Mid$(s, openPos + 1, closePos - openPos - 1))

        Dim tokenValue As String
        If Len(token) > 0 Then
            If LCase$(token) = "__dump__" Then
                tokenValue = BuildXmlPlaceholderDump(contextNode)
                If Len(Trim$(CStr(tokenValue))) > 0 Then anyTokenValue = True
                out = out & tokenValue
            ElseIf TryResolveXmlPath(contextNode, token, tokenValue) Then
                If Len(Trim$(CStr(tokenValue))) > 0 Then anyTokenValue = True
                out = out & tokenValue
            Else
                out = out & vbNullString
            End If
        End If

        pos = closePos + 1
    Loop

    ReplacePlaceholders = out
End Function

Private Function BuildXmlPlaceholderDump(ByVal contextNode As Object) As String
    On Error GoTo Fail

    If contextNode Is Nothing Then Exit Function

    Dim lines As New Collection
    CollectXmlPaths contextNode, vbNullString, lines

    If lines.Count = 0 Then Exit Function

    Dim arr() As String
    ReDim arr(1 To lines.Count) As String

    Dim i As Long
    For i = 1 To lines.Count
        arr(i) = CStr(lines.Item(i))
    Next i

    BuildXmlPlaceholderDump = Join(arr, vbCrLf)
    Exit Function

Fail:
    BuildXmlPlaceholderDump = vbNullString
End Function

Private Sub CollectXmlPaths(ByVal node As Object, ByVal pathPrefix As String, ByRef outLines As Collection)
    If node Is Nothing Then Exit Sub
    If outLines Is Nothing Then Exit Sub

    Dim hasElementChildren As Boolean
    hasElementChildren = False

    Dim child As Object
    For Each child In node.childNodes
        If child.nodeType = 1 Then
            hasElementChildren = True
            Exit For
        End If
    Next child

    If Not hasElementChildren Then
        If Len(Trim$(pathPrefix)) = 0 Then Exit Sub
        outLines.Add pathPrefix & " = " & NormalizeSingleLine(CStr(node.text), 220)
        Exit Sub
    End If

    If IsArrayContainer(node) Then
        Dim idx As Long
        idx = 0
        For Each child In node.childNodes
            If child.nodeType = 1 Then
                Dim itemPrefix As String
                If Len(pathPrefix) > 0 Then
                    itemPrefix = pathPrefix & "[" & CStr(idx) & "]"
                Else
                    itemPrefix = "[" & CStr(idx) & "]"
                End If
                CollectXmlPaths child, itemPrefix, outLines
                idx = idx + 1
            End If
        Next child
        Exit Sub
    End If

    For Each child In node.childNodes
        If child.nodeType <> 1 Then GoTo NextChild

        Dim childName As String
        childName = CStr(child.nodeName)
        If Len(Trim$(childName)) = 0 Then GoTo NextChild

        Dim childPrefix As String
        If Len(pathPrefix) = 0 Then
            childPrefix = childName
        Else
            childPrefix = pathPrefix & "." & childName
        End If

        CollectXmlPaths child, childPrefix, outLines

NextChild:
    Next child
End Sub

Private Function IsArrayContainer(ByVal node As Object) As Boolean
    On Error Resume Next

    Dim firstName As String
    firstName = vbNullString

    Dim count As Long
    count = 0

    Dim child As Object
    For Each child In node.childNodes
        If child.nodeType = 1 Then
            If Len(firstName) = 0 Then
                firstName = LCase$(CStr(child.nodeName))
            ElseIf LCase$(CStr(child.nodeName)) <> firstName Then
                IsArrayContainer = False
                Exit Function
            End If
            count = count + 1
        End If
    Next child

    If count = 0 Then Exit Function
    If firstName <> "item" And firstName <> "object" Then Exit Function

    IsArrayContainer = True
End Function

Private Sub UpdateRowInfo(ByVal rowInfo As Object, ByVal rowIndex As Long, ByVal hasPlaceholder As Boolean, ByVal tokenHasValue As Boolean)
    If rowInfo Is Nothing Then Exit Sub

    Dim key As String
    key = CStr(rowIndex)

    Dim info As Object
    If rowInfo.Exists(key) Then
        Set info = rowInfo(key)
    Else
        Set info = CreateObject("Scripting.Dictionary")
        rowInfo.Add key, info
    End If

    If hasPlaceholder Then info("hasPlaceholder") = True
    If tokenHasValue Then info("hasValue") = True
End Sub

Private Function TryResolveXmlPath(ByVal baseNode As Object, ByVal path As String, ByRef outValue As String) As Boolean
    On Error GoTo Fail

    Dim raw As String
    raw = Trim$(CStr(path))
    If Len(raw) = 0 Then Exit Function

    Dim cur As Object
    Set cur = baseNode
    If cur Is Nothing Then Exit Function

    Dim parts() As String
    parts = Split(raw, ".")

    Dim i As Long
    For i = LBound(parts) To UBound(parts)
        Dim part As String
        part = Trim$(CStr(parts(i)))
        If Len(part) = 0 Then GoTo Fail

        Dim name As String
        Dim idxs As Collection
        If Not ParsePathPart(part, name, idxs) Then GoTo Fail

        Set cur = FindChildElementByName(cur, name)
        If cur Is Nothing Then GoTo Fail

        Dim j As Long
        For j = 1 To idxs.Count
            Set cur = NthElementChild(cur, CLng(idxs(j)))
            If cur Is Nothing Then GoTo Fail
        Next j
    Next i

    outValue = CStr(cur.text)
    TryResolveXmlPath = True
    Exit Function

Fail:
    TryResolveXmlPath = False
End Function

Private Function ParsePathPart(ByVal part As String, ByRef outName As String, ByRef outIndices As Collection) As Boolean
    Dim s As String
    s = Trim$(CStr(part))
    If Len(s) = 0 Then Exit Function

    Dim bracketPos As Long
    bracketPos = InStr(1, s, "[", vbBinaryCompare)

    If bracketPos = 0 Then
        outName = s
        Set outIndices = New Collection
        ParsePathPart = True
        Exit Function
    End If

    Dim name As String
    name = Trim$(Left$(s, bracketPos - 1))
    If Len(name) = 0 Then Exit Function

    Dim rest As String
    rest = Mid$(s, bracketPos)

    Dim idxs As New Collection
    Do While Len(rest) > 0
        If Left$(rest, 1) <> "[" Then Exit Function

        Dim endPos As Long
        endPos = InStr(2, rest, "]", vbBinaryCompare)
        If endPos = 0 Then Exit Function

        Dim idxText As String
        idxText = Mid$(rest, 2, endPos - 2)
        If Len(Trim$(idxText)) = 0 Then Exit Function

        Dim idx As Long
        idx = CLng(Val(idxText))
        If idx < 0 Then Exit Function

        idxs.Add idx
        rest = Mid$(rest, endPos + 1)
    Loop

    outName = name
    Set outIndices = idxs
    ParsePathPart = True
End Function

Private Function FindChildElementByName(ByVal parentNode As Object, ByVal childName As String) As Object
    On Error Resume Next

    Dim want As String
    want = LCase$(Trim$(CStr(childName)))
    If Len(want) = 0 Then Exit Function

    Dim child As Object
    For Each child In parentNode.childNodes
        If child.nodeType = 1 Then
            If LCase$(CStr(child.nodeName)) = want Then
                Set FindChildElementByName = child
                Exit Function
            End If
        End If
    Next child
End Function

Private Function NthElementChild(ByVal parentNode As Object, ByVal index0 As Long) As Object
    On Error Resume Next

    Dim child As Object
    Dim i As Long
    i = 0
    For Each child In parentNode.childNodes
        If child.nodeType = 1 Then
            If i = index0 Then
                Set NthElementChild = child
                Exit Function
            End If
            i = i + 1
        End If
    Next child
End Function

Private Function JoinTemplateKeys(ByVal wsCfg As Worksheet) As String
    Dim dict As Object
    Set dict = CreateObject("Scripting.Dictionary")
    dict.CompareMode = 1 ' TextCompare

    Dim cell As Range
    For Each cell In wsCfg.UsedRange.Cells
        If VarType(cell.Value2) <> vbString Then GoTo NextCell

        Dim s As String
        s = Trim$(CStr(cell.Value2))
        If Len(s) = 0 Then GoTo NextCell

        If LCase$(Left$(s, Len(MARKER_PREFIX))) <> LCase$(MARKER_PREFIX) Then GoTo NextCell

        Dim key As String
        key = ExtractKeyFromMarker(s)
        If Len(key) > 0 Then
            If Not dict.Exists(key) Then dict.Add key, True
        End If

NextCell:
    Next cell

    If dict.Count = 0 Then Exit Function

    Dim k As Variant
    Dim out As String
    out = vbNullString
    For Each k In dict.Keys
        If Len(out) > 0 Then out = out & ", "
        out = out & CStr(k)
    Next k

    JoinTemplateKeys = out
End Function

Private Function ExtractKeyFromMarker(ByVal marker As String) As String
    Dim s As String
    s = Trim$(CStr(marker))

    If LCase$(Left$(s, Len(MARKER_PREFIX))) <> LCase$(MARKER_PREFIX) Then Exit Function

    Dim suffixLen As Long
    If Right$(s, Len(MARKER_SUFFIX_TL)) = MARKER_SUFFIX_TL Then
        suffixLen = Len(MARKER_SUFFIX_TL)
    ElseIf Right$(s, Len(MARKER_SUFFIX_TR)) = MARKER_SUFFIX_TR Then
        suffixLen = Len(MARKER_SUFFIX_TR)
    ElseIf Right$(s, Len(MARKER_SUFFIX_BL)) = MARKER_SUFFIX_BL Then
        suffixLen = Len(MARKER_SUFFIX_BL)
    ElseIf Right$(s, Len(MARKER_SUFFIX_BR)) = MARKER_SUFFIX_BR Then
        suffixLen = Len(MARKER_SUFFIX_BR)
    Else
        Exit Function
    End If

    ExtractKeyFromMarker = Mid$(s, Len(MARKER_PREFIX) + 1, Len(s) - Len(MARKER_PREFIX) - suffixLen)
End Function

Private Sub ApplyTemplateConfigSheetDefaults(ByVal wsCfg As Worksheet)
    If wsCfg Is Nothing Then Exit Sub

    On Error Resume Next
    wsCfg.Cells.Font.Name = "Meiryo UI"
    On Error GoTo 0

    wsCfg.Cells.Font.Size = 9
    wsCfg.Cells.WrapText = True
    wsCfg.Cells.VerticalAlignment = xlTop
    wsCfg.Cells.HorizontalAlignment = xlLeft
    wsCfg.Cells.NumberFormat = "@"

    wsCfg.Columns(1).ColumnWidth = 2
    wsCfg.Columns(2).ColumnWidth = 34
    wsCfg.Columns(3).ColumnWidth = 54
    wsCfg.Columns(4).ColumnWidth = 2
End Sub

Private Sub SeedDefaultTemplateConfig(ByVal wsCfg As Worksheet)
    ApplyTemplateConfigSheetDefaults wsCfg

    Dim startRow As Long
    startRow = 1

    Dim startCol As Long
    startCol = 1

    Dim templateKeys As Variant
    ' Avoid "Too many line continuations" (VBA limit per statement)
    Dim keysCsv As String
    keysCsv = "DEFAULT,DATA,CONSTANTS,PARAMETERS,SELECT-OPTIONS,RANGES,TYPES,STATICS,CLASS-DATA,FIELD-SYMBOLS"
    keysCsv = keysCsv & ",FORM,PERFORM,CALL_FUNCTION,CALL_METHOD,CALL_TRANSACTION"
    keysCsv = keysCsv & ",ASSIGNMENT,MOVE,MOVE-CORRESPONDING,CLEAR"
    keysCsv = keysCsv & ",APPEND,INSERT_ITAB,MODIFY_ITAB,READ_TABLE,DELETE_ITAB,SORT_ITAB,LOOP_AT_ITAB"
    keysCsv = keysCsv & ",IF,ELSEIF,ELSE,CASE,WHEN,DO,TRY,CATCH,CLEANUP,SELECT"
    keysCsv = keysCsv & ",CLASS,METHODS,CLASS-METHODS,METHOD"
    templateKeys = Split(keysCsv, ",")

    Dim key As Variant
    For Each key In templateKeys
        Dim inner As Range
        Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, CStr(key), 24, 2)
        SeedGenericTemplate inner
        startRow = startRow + 24 + 4
    Next key
End Sub

Private Sub SeedGenericTemplate(ByVal inner As Range)
    ' Generic template for ABAP Parser Viewer XML (JS structure)
    ' Column A: placeholder path (for you to edit later)
    ' Column B: resolved value (placeholder)

    SetTemplateCell inner, "A1", "path"
    SetTemplateCell inner, "B1", "value"

    SetTemplateCell inner, "A2", "objectType"
    SetTemplateCell inner, "B2", "{objectType}"

    SetTemplateCell inner, "A3", "id"
    SetTemplateCell inner, "B3", "{id}"

    SetTemplateCell inner, "A4", "parent"
    SetTemplateCell inner, "B4", "{parent}"

    SetTemplateCell inner, "A5", "file"
    SetTemplateCell inner, "B5", "{file}"

    SetTemplateCell inner, "A6", "lineStart"
    SetTemplateCell inner, "B6", "{lineStart}"

    SetTemplateCell inner, "A7", "comment"
    SetTemplateCell inner, "B7", "{comment}"

    SetTemplateCell inner, "A8", "raw"
    SetTemplateCell inner, "B8", "{raw}"

    SetTemplateCell inner, "A9", "keywords.stmt.text"
    SetTemplateCell inner, "B9", "{keywords.stmt.text}"

    SetTemplateCell inner, "A10", "keywords.stmt.label"
    SetTemplateCell inner, "B10", "{keywords.stmt.label}"

    SetTemplateCell inner, "A11", "values.name.value"
    SetTemplateCell inner, "B11", "{values.name.value}"

    SetTemplateCell inner, "A12", "values.name.label"
    SetTemplateCell inner, "B12", "{values.name.label}"

    SetTemplateCell inner, "A13", "values.name.codeDesc"
    SetTemplateCell inner, "B13", "{values.name.codeDesc}"

    SetTemplateCell inner, "A14", "values.name.decl.name"
    SetTemplateCell inner, "B14", "{values.name.decl.name}"

    SetTemplateCell inner, "A15", "values.name.decl.desc"
    SetTemplateCell inner, "B15", "{values.name.decl.desc}"

    SetTemplateCell inner, "A16", "values.name.decl.scopeLabel"
    SetTemplateCell inner, "B16", "{values.name.decl.scopeLabel}"

    SetTemplateCell inner, "A17", "values.type.value"
    SetTemplateCell inner, "B17", "{values.type.value}"

    SetTemplateCell inner, "A18", "extras.performCall.form"
    SetTemplateCell inner, "B18", "{extras.performCall.form}"

    SetTemplateCell inner, "A19", "extras.callFunction.name"
    SetTemplateCell inner, "B19", "{extras.callFunction.name}"

    SetTemplateCell inner, "A20", "extras.callMethod.target"
    SetTemplateCell inner, "B20", "{extras.callMethod.target}"

    SetTemplateCell inner, "A21", "extras.form.name"
    SetTemplateCell inner, "B21", "{extras.form.name}"

    SetTemplateCell inner, "A22", "children[0].objectType"
    SetTemplateCell inner, "B22", "{children[0].objectType}"

    SetTemplateCell inner, "A23", "__DUMP__"
    SetTemplateCell inner, "B23", "{__DUMP__}"

    ApplyJapanesePathValueTableStyle inner
End Sub

Private Sub ApplyJapanesePathValueTableStyle(ByVal inner As Range)
    If inner Is Nothing Then Exit Sub

    On Error Resume Next
    inner.Font.Name = "Meiryo UI"
    On Error GoTo 0

    inner.Font.Size = 9
    inner.NumberFormat = "@"
    inner.WrapText = True
    inner.VerticalAlignment = xlTop
    inner.HorizontalAlignment = xlLeft
    inner.Interior.Color = RGB(255, 255, 255)

    inner.RowHeight = 18
    inner.Rows(1).RowHeight = 20

    If inner.Columns.Count >= 1 Then inner.Columns(1).Interior.Color = RGB(248, 248, 248)
    If inner.Columns.Count >= 2 Then inner.Columns(2).Interior.Color = RGB(255, 255, 242)

    With inner.Rows(1)
        .Interior.Color = RGB(242, 242, 242)
        .Font.Bold = True
        .HorizontalAlignment = xlCenter
        .VerticalAlignment = xlCenter
    End With

    With inner.Borders
        .LineStyle = xlContinuous
        .Weight = xlThin
        .Color = RGB(200, 200, 200)
    End With

    inner.Borders(xlEdgeLeft).Weight = xlMedium
    inner.Borders(xlEdgeTop).Weight = xlMedium
    inner.Borders(xlEdgeRight).Weight = xlMedium
    inner.Borders(xlEdgeBottom).Weight = xlMedium
    inner.Rows(1).Borders(xlEdgeBottom).Weight = xlMedium
End Sub

Private Sub SeedItabTemplate(ByVal inner As Range)
    SetTemplateCell inner, "A1", U("B\u1EA3ng n\u1ED9i b\u1ED9")
    SetTemplateCell inner, "B1", "{table.description}"

    SetTemplateCell inner, "A2", "{labels.target}"
    SetTemplateCell inner, "B2", "{target.description}"

    SetTemplateCell inner, "A3", "{labels.conditions}"

    SetTemplateCell inner, "A4", "{labels.condItem1}"
    SetTemplateCell inner, "B4", "{labels.condOperator}"
    SetTemplateCell inner, "C4", "{labels.condItem2}"
    SetTemplateCell inner, "D4", "{labels.condAssoc}"

    SetTemplateCell inner, "A5", "{conditions[0].item1.description}"
    SetTemplateCell inner, "B5", "{conditions[0].operator}"
    SetTemplateCell inner, "C5", "{conditions[0].item2.description}"
    SetTemplateCell inner, "D5", "{conditions[0].association}"

    SetTemplateCell inner, "A6", "{conditions[1].item1.description}"
    SetTemplateCell inner, "B6", "{conditions[1].operator}"
    SetTemplateCell inner, "C6", "{conditions[1].item2.description}"
    SetTemplateCell inner, "D6", "{conditions[1].association}"

    SetTemplateCell inner, "A7", "{labels.binarySearch}"
End Sub

Private Sub SeedPerformTemplate(ByVal inner As Range)
    SetTemplateCell inner, "A1", U("Th\u1EF1c hi\u1EC7n")
    SetTemplateCell inner, "B1", "{perform.name}"
    SetTemplateCell inner, "C1", "{perform.description}"

    SetTemplateCell inner, "A2", "{labels.tables}"
    SetTemplateCell inner, "B2", "{tables[0].actual}"
    SetTemplateCell inner, "C2", "{tables[0].description}"

    SetTemplateCell inner, "A3", ""
    SetTemplateCell inner, "B3", "{tables[1].actual}"
    SetTemplateCell inner, "C3", "{tables[1].description}"

    SetTemplateCell inner, "A4", "{labels.using}"
    SetTemplateCell inner, "B4", "{using[0].actual}"
    SetTemplateCell inner, "C4", "{using[0].description}"

    SetTemplateCell inner, "A5", ""
    SetTemplateCell inner, "B5", "{using[1].actual}"
    SetTemplateCell inner, "C5", "{using[1].description}"

    SetTemplateCell inner, "A6", "{labels.changing}"
    SetTemplateCell inner, "B6", "{changing[0].actual}"
    SetTemplateCell inner, "C6", "{changing[0].description}"

    SetTemplateCell inner, "A7", ""
    SetTemplateCell inner, "B7", "{changing[1].actual}"
    SetTemplateCell inner, "C7", "{changing[1].description}"

    SetTemplateCell inner, "A8", "{labels.raising}"
    SetTemplateCell inner, "B8", "{raising[0].name}"
    SetTemplateCell inner, "C8", "{raising[0].description}"

    SetTemplateCell inner, "A9", ""
    SetTemplateCell inner, "B9", "{raising[1].name}"
    SetTemplateCell inner, "C9", "{raising[1].description}"
End Sub

Private Function CreateTemplateBlock(ByVal ws As Worksheet, ByVal startRow As Long, ByVal startCol As Long, ByVal templateId As String, ByVal gridRows As Long, ByVal gridCols As Long) As Range
    Dim tl As Range, tr As Range, bl As Range, br As Range
    Set tl = ws.Cells(startRow, startCol)
    Set tr = ws.Cells(startRow, startCol + gridCols + 1)
    Set bl = ws.Cells(startRow + gridRows + 1, startCol)
    Set br = ws.Cells(startRow + gridRows + 1, startCol + gridCols + 1)

    tl.Value2 = MakeMarker(templateId, "TL")
    tr.Value2 = MakeMarker(templateId, "TR")
    bl.Value2 = MakeMarker(templateId, "BL")
    br.Value2 = MakeMarker(templateId, "BR")

    Dim innerTopLeft As Range
    Set innerTopLeft = tl.Offset(1, 1)

    Dim inner As Range
    Set inner = ws.Range(innerTopLeft, innerTopLeft.Offset(gridRows - 1, gridCols - 1))

    inner.ClearContents
    inner.WrapText = True
    inner.NumberFormat = "@"

    On Error Resume Next
    inner.Font.Name = "Meiryo UI"
    On Error GoTo 0
    inner.Font.Size = 9
    inner.VerticalAlignment = xlTop
    inner.HorizontalAlignment = xlLeft

    ' Hide marker cells (keep values for parsing)
    tl.Font.Color = RGB(255, 255, 255)
    tr.Font.Color = RGB(255, 255, 255)
    bl.Font.Color = RGB(255, 255, 255)
    br.Font.Color = RGB(255, 255, 255)
    tl.Font.Size = 1
    tr.Font.Size = 1
    bl.Font.Size = 1
    br.Font.Size = 1

    ws.Rows(tl.Row).RowHeight = 4
    ws.Rows(bl.Row).RowHeight = 4

    With inner.Borders
        .LineStyle = xlContinuous
        .Weight = xlThin
        .Color = RGB(200, 200, 200)
    End With

    Set CreateTemplateBlock = inner
End Function

Private Sub SetTemplateCell(ByVal inner As Range, ByVal addr As String, ByVal text As String)
    Dim rr As Long, cc As Long
    If Not AddrToRowCol(addr, rr, cc) Then Exit Sub
    inner.Cells(rr, cc).Value2 = text
End Sub

Private Function AddrToRowCol(ByVal addr As String, ByRef outRow As Long, ByRef outCol As Long) As Boolean
    Dim s As String
    s = Trim$(CStr(addr))
    If Len(s) = 0 Then Exit Function

    Dim letters As String
    letters = vbNullString
    Dim digits As String
    digits = vbNullString

    Dim i As Long
    For i = 1 To Len(s)
        Dim ch As String
        ch = Mid$(s, i, 1)
        If ch Like "[A-Za-z]" Then
            If Len(digits) > 0 Then Exit Function
            letters = letters & ch
        ElseIf ch Like "[0-9]" Then
            digits = digits & ch
        Else
            Exit Function
        End If
    Next i

    If Len(letters) = 0 Or Len(digits) = 0 Then Exit Function

    outCol = ColLettersToIndex(letters)
    outRow = CLng(Val(digits))
    If outCol <= 0 Or outRow <= 0 Then Exit Function

    AddrToRowCol = True
End Function

Private Function ColLettersToIndex(ByVal letters As String) As Long
    Dim s As String
    s = UCase$(Trim$(CStr(letters)))
    If Len(s) = 0 Then Exit Function

    Dim n As Long
    n = 0

    Dim i As Long
    For i = 1 To Len(s)
        Dim ch As Integer
        ch = Asc(Mid$(s, i, 1))
        If ch < 65 Or ch > 90 Then Exit Function
        n = n * 26 + (ch - 64)
    Next i

    ColLettersToIndex = n
End Function

Private Sub EnsureButton(ByVal ws As Worksheet, ByVal name As String, ByVal caption As String, ByVal actionMacro As String, ByVal left As Double, ByVal top As Double)
    Dim btn As Button
    On Error Resume Next
    Set btn = ws.Buttons(name)
    On Error GoTo 0

    If btn Is Nothing Then
        Set btn = ws.Buttons.Add(left, top, 160, 28)
        btn.name = name
    End If

    btn.caption = caption
    btn.OnAction = actionMacro
End Sub

Private Sub RemoveButton(ByVal ws As Worksheet, ByVal name As String)
    On Error Resume Next
    ws.Buttons(name).Delete
    On Error GoTo 0
End Sub

Private Function GetOrCreateWorksheet(ByVal wb As Workbook, ByVal sheetName As String) As Worksheet
    On Error Resume Next
    Set GetOrCreateWorksheet = wb.Worksheets(sheetName)
    On Error GoTo 0

    If GetOrCreateWorksheet Is Nothing Then
        Set GetOrCreateWorksheet = wb.Worksheets.Add(After:=wb.Worksheets(wb.Worksheets.Count))
        GetOrCreateWorksheet.name = sheetName
    End If
End Function

Private Function ReadTextFile(ByVal filePath As String) As String
    On Error GoTo Fallback

    Dim stm As Object
    Set stm = CreateObject("ADODB.Stream")
    stm.Type = 2 ' text
    stm.Charset = "utf-8"
    stm.Open
    stm.LoadFromFile filePath
    ReadTextFile = stm.ReadText
    stm.Close
    Exit Function

Fallback:
    On Error Resume Next
    Dim f As Integer
    f = FreeFile
    Open filePath For Input As #f
    ReadTextFile = Input$(LOF(f), #f)
    Close #f
End Function


Private Function U(ByVal s As String) As String
    Dim out As String
    Dim i As Long
    i = 1
    Do While i <= Len(s)
        If Mid$(s, i, 2) = "\u" And i + 5 <= Len(s) Then
            Dim hexText As String
            hexText = Mid$(s, i + 2, 4)
            If IsHex4(hexText) Then
                out = out & ChrW(CLng("&H" & hexText))
                i = i + 6
                GoTo NextChar
            End If
        End If
        out = out & Mid$(s, i, 1)
        i = i + 1
NextChar:
    Loop
    U = out
End Function

Private Function IsHex4(ByVal s As String) As Boolean
    Dim i As Long
    If Len(s) <> 4 Then Exit Function
    For i = 1 To 4
        Dim ch As Integer
        ch = Asc(Mid$(s, i, 1))
        If (ch < 48 Or ch > 57) And (ch < 65 Or ch > 70) And (ch < 97 Or ch > 102) Then Exit Function
    Next i
    IsHex4 = True
End Function
