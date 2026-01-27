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
End Sub

Private Sub SetupInputSheet(ByVal wsIn As Worksheet)
    If Len(CStr(wsIn.Range("A1").Value2)) = 0 Then
        wsIn.Range("A1").Value2 = "Paste ABAP Objects export XML here starting at A2 (one XML line per row)."
        wsIn.Range("A1").Font.Bold = True
        wsIn.Range("A1").WrapText = True
    End If

    wsIn.Columns("A").ColumnWidth = 110
    wsIn.Columns("B").ColumnWidth = 16
    wsIn.Columns("C").ColumnWidth = 50

    wsIn.Columns("A").WrapText = False

    wsIn.Range("B1").Value2 = "Object #"
    wsIn.Range("B1").Font.Bold = True
    If Len(CStr(wsIn.Range("B2").Value2)) = 0 Then wsIn.Range("B2").Value2 = 1

    wsIn.Range("C1").Value2 = "Selected"
    wsIn.Range("C1").Font.Bold = True

    EnsureButton wsIn, "btnLoadXml", "Load XML file", "LoadXmlFromFile", 320, 32
    RemoveButton wsIn, "btnGenerate"
    EnsureButton wsIn, "btnResetTplCfg", "Reset Template config", "ResetTemplateConfig", 320, 112
    EnsureButton wsIn, "btnGenerateAll", "Generate all", "GenerateAllTemplatesFromXml", 320, 152
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
    filePath = Application.GetOpenFilename("XML Files (*.xml),*.xml,All Files (*.*),*.*", , "Select ABAP Objects export XML")
    If VarType(filePath) = vbBoolean And filePath = False Then Exit Sub

    WriteXmlInputToSheet wsIn, ReadTextFile(CStr(filePath))
End Sub

Public Sub ResetTemplateConfig()
    Dim wsCfg As Worksheet
    Set wsCfg = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE_CONFIG)

    If MsgBox("Reset Template config to defaults? This will clear the sheet.", vbQuestion + vbYesNo) <> vbYes Then Exit Sub

    wsCfg.Cells.Clear
    SeedDefaultTemplateConfig wsCfg

    MsgBox "Template config reset.", vbInformation
End Sub

Public Sub GenerateTemplateFromXml()
    Dim wsIn As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, SHEET_INPUT)

    Dim wsCfg As Worksheet
    Set wsCfg = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE_CONFIG)

    Dim wsTpl As Worksheet
    Set wsTpl = GetOrCreateWorksheet(ThisWorkbook, SHEET_TEMPLATE)

    Dim inputText As String
    inputText = ReadXmlInputFromSheet(wsIn)
    If Len(Trim$(inputText)) = 0 Then
        MsgBox "Input XML is empty. Paste export XML into Input column A (starting at A2), or click 'Load XML file'.", vbExclamation
        Exit Sub
    End If

    Dim index1 As Long
    index1 = CLng(Val(CStr(wsIn.Range("B2").Value2)))
    If index1 <= 0 Then index1 = 1

    Dim doc As Object
    Dim objectNodes As Object
    If Not TryLoadObjectsExportXml(inputText, doc, objectNodes) Then Exit Sub

    Dim objNode As Object
    Set objNode = PickObjectNode(objectNodes, index1)
    If objNode Is Nothing Then
        MsgBox "No object found at index #" & CStr(index1) & ".", vbExclamation
        Exit Sub
    End If

    Dim templateId As String
    templateId = SafeAttr(objNode, "templateId")

    Dim objectId As String
    objectId = SafeAttr(objNode, "objectId")

    Dim templateKey As String
    templateKey = Trim$(templateId)
    If Len(templateKey) = 0 Then templateKey = Trim$(objectId)

    If Len(templateKey) = 0 Then
        MsgBox "Selected object has no templateId/objectId.", vbExclamation
        Exit Sub
    End If

    Dim sample As Range
    Set sample = GetTemplateSampleRange(wsCfg, templateKey)
    If sample Is Nothing Then
        Dim available As String
        available = JoinTemplateKeys(wsCfg)
        MsgBox "Template config not found for: " & templateKey & IIf(Len(available) > 0, vbCrLf & vbCrLf & "Available: " & available, ""), vbExclamation
        Exit Sub
    End If

    Dim ctxNode As Object
    Set ctxNode = objNode.selectSingleNode("context")
    If ctxNode Is Nothing Then
        MsgBox "Selected object has no <context> node.", vbExclamation
        Exit Sub
    End If

    wsTpl.Cells.Clear

    Dim dest As Range
    Set dest = wsTpl.Range("A1")
    CopyRangeWithDimensions sample, dest

    Dim outRange As Range
    Set outRange = wsTpl.Range(dest, dest.Offset(sample.Rows.Count - 1, sample.Columns.Count - 1))
    FillRangePlaceholders outRange, ctxNode

    wsIn.Range("C2").Value2 = "templateId=" & templateId & " objectId=" & objectId & " resultId=" & SafeAttr(objNode, "resultId")

    MsgBox "Done. Template written to '" & SHEET_TEMPLATE & "'.", vbInformation
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
        MsgBox "Input XML is empty. Paste export XML into Input column A (starting at A2), or click 'Load XML file'.", vbExclamation
        Exit Sub
    End If

    Dim doc As Object
    Dim objectNodes As Object
    If Not TryLoadObjectsExportXml(inputText, doc, objectNodes) Then Exit Sub

    Dim prevCalc As XlCalculation
    prevCalc = Application.Calculation

    On Error GoTo Cleanup
    Application.ScreenUpdating = False
    Application.EnableEvents = False
    Application.Calculation = xlCalculationManual

    wsTpl.Cells.Clear

    Dim rowPtr As Long
    rowPtr = 1

    Dim sampleCache As Object
    Set sampleCache = CreateObject("Scripting.Dictionary")
    sampleCache.CompareMode = 1 ' TextCompare

    Dim i As Long
    For i = 0 To objectNodes.Length - 1
        Dim objNode As Object
        Set objNode = objectNodes.Item(i)
        If objNode Is Nothing Then GoTo NextObj

        Dim templateId As String
        templateId = SafeAttr(objNode, "templateId")

        Dim objectId As String
        objectId = SafeAttr(objNode, "objectId")

        Dim templateKey As String
        templateKey = Trim$(templateId)
        If Len(templateKey) = 0 Then templateKey = Trim$(objectId)

        Dim depth As Long
        depth = CLng(Val(SafeAttr(objNode, "depth")))
        If depth < 0 Then depth = 0

        Dim colPtr As Long
        colPtr = 1 + depth * INDENT_COLS_PER_DEPTH
        If colPtr < 1 Then colPtr = 1

        Dim routineKey As String
        routineKey = SafeChildText(objNode, "routineKey")

        Dim originalText As String
        originalText = SafeChildText(objNode, "original")
        originalText = Replace(originalText, vbCrLf, " ")
        originalText = Replace(originalText, vbCr, " ")
        originalText = Replace(originalText, vbLf, " ")
        originalText = Trim$(originalText)

        Dim header As String
        header = "#" & CStr(i + 1) & " depth=" & CStr(depth) & " " & templateKey
        If Len(routineKey) > 0 Then header = header & " | " & routineKey
        If Len(originalText) > 0 Then header = header & " | " & originalText
        If Len(header) > MAX_HEADER_TEXT Then header = Left$(header, MAX_HEADER_TEXT - 3) & "..."

        With wsTpl.Cells(rowPtr, colPtr)
            .Value2 = header
            .Font.Bold = True
        End With
        rowPtr = rowPtr + 1

        If Len(templateKey) = 0 Then
            wsTpl.Cells(rowPtr, colPtr).Value2 = "[Missing templateId/objectId]"
            rowPtr = rowPtr + BLOCK_GAP_ROWS
            GoTo NextObj
        End If

        Dim sample As Range
        If sampleCache.Exists(templateKey) Then
            Set sample = sampleCache(templateKey)
        Else
            Set sample = GetTemplateSampleRange(wsCfg, templateKey)
            If Not sample Is Nothing Then sampleCache.Add templateKey, sample
        End If

        If sample Is Nothing Then
            wsTpl.Cells(rowPtr, colPtr).Value2 = "[Missing Template config] " & templateKey
            rowPtr = rowPtr + BLOCK_GAP_ROWS
            GoTo NextObj
        End If

        Dim ctxNode As Object
        Set ctxNode = objNode.selectSingleNode("context")
        If ctxNode Is Nothing Then
            wsTpl.Cells(rowPtr, colPtr).Value2 = "[Missing <context>]"
            rowPtr = rowPtr + BLOCK_GAP_ROWS
            GoTo NextObj
        End If

        Dim dest As Range
        Set dest = wsTpl.Cells(rowPtr, colPtr)
        CopyRangeWithRowHeights sample, dest

        Dim outRange As Range
        Set outRange = wsTpl.Range(dest, dest.Offset(sample.Rows.Count - 1, sample.Columns.Count - 1))
        FillRangePlaceholders outRange, ctxNode

        rowPtr = rowPtr + sample.Rows.Count + BLOCK_GAP_ROWS

NextObj:
    Next i

    Application.CutCopyMode = False

    wsIn.Range("C2").Value2 = "Generated " & CStr(objectNodes.Length) & " object(s)."

Cleanup:
    Application.Calculation = prevCalc
    Application.EnableEvents = True
    Application.ScreenUpdating = True

    If Err.Number <> 0 Then
        MsgBox "Generate all failed: " & Err.Description, vbCritical
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

    If Not doc.LoadXML(xmlText) Then Err.Raise vbObjectError + 3000, , "Invalid XML."

    Dim rootName As String
    rootName = LCase$(CStr(doc.documentElement.nodeName))
    If rootName <> "abapflowobjects" Then Err.Raise vbObjectError + 3001, , "Unexpected root element: " & doc.documentElement.nodeName

    Dim nodes As Object
    Set nodes = doc.SelectNodes("/abapflowObjects/object")
    If nodes Is Nothing Or nodes.Length = 0 Then Err.Raise vbObjectError + 3002, , "No <object> nodes found."

    Set outDoc = doc
    Set outObjectNodes = nodes
    TryLoadObjectsExportXml = True
    Exit Function

Fail:
    MsgBox "XML load failed: " & Err.Description, vbCritical
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

Private Sub FillRangePlaceholders(ByVal targetRange As Range, ByVal contextNode As Object)
    Dim cell As Range
    For Each cell In targetRange.Cells
        If VarType(cell.Value2) <> vbString Then GoTo NextCell

        Dim raw As String
        raw = CStr(cell.Value2)

        If InStr(1, raw, "{", vbBinaryCompare) = 0 Or InStr(1, raw, "}", vbBinaryCompare) = 0 Then GoTo NextCell

        Dim replaced As String
        replaced = ReplacePlaceholders(raw, contextNode)

        If replaced <> raw Then cell.Value2 = replaced

NextCell:
    Next cell
End Sub

Private Function ReplacePlaceholders(ByVal text As String, ByVal contextNode As Object) As String
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
        If Len(token) > 0 And TryResolveXmlPath(contextNode, token, tokenValue) Then
            out = out & tokenValue
        Else
            out = out & "{" & token & "}"
        End If

        pos = closePos + 1
    Loop

    ReplacePlaceholders = out
End Function

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

Private Sub SeedDefaultTemplateConfig(ByVal wsCfg As Worksheet)
    Dim startRow As Long
    startRow = 1

    Dim startCol As Long
    startCol = 1

    Dim inner As Range

    ' append.excel-like-table (2x4)
    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "append.excel-like-table", 2, 4)
    SetTemplateCell inner, "A1", "Append"
    SetTemplateCell inner, "B1", "{line.description}"
    SetTemplateCell inner, "C1", "TO"
    SetTemplateCell inner, "D1", "{itab.description}"
    SetTemplateCell inner, "A2", "{labels.sortedBy}"
    SetTemplateCell inner, "B2", "{sortedBy.description}"
    startRow = startRow + 2 + 4

    ' assignment.excel-like-table (2x2)
    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "assignment.excel-like-table", 2, 2)
    SetTemplateCell inner, "A1", "Item"
    SetTemplateCell inner, "B1", "Value"
    SetTemplateCell inner, "A2", "{item.description}"
    SetTemplateCell inner, "B2", "{value.description}"
    startRow = startRow + 2 + 4

    ' if.excel-like-table (3x4)
    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "if.excel-like-table", 3, 4)
    SetTemplateCell inner, "A1", "Item 1"
    SetTemplateCell inner, "B1", "Operator"
    SetTemplateCell inner, "C1", "Item 2"
    SetTemplateCell inner, "D1", "Associations"
    SetTemplateCell inner, "A2", "{conditions[0].item1.description}"
    SetTemplateCell inner, "B2", "{conditions[0].operator}"
    SetTemplateCell inner, "C2", "{conditions[0].item2.description}"
    SetTemplateCell inner, "D2", "{conditions[0].association}"
    SetTemplateCell inner, "A3", "{conditions[1].item1.description}"
    SetTemplateCell inner, "B3", "{conditions[1].operator}"
    SetTemplateCell inner, "C3", "{conditions[1].item2.description}"
    SetTemplateCell inner, "D3", "{conditions[1].association}"
    startRow = startRow + 3 + 4

    ' itabRead.excel-like-table (7x4) - base for other itab templates
    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "itabRead.excel-like-table", 7, 4)
    SeedItabTemplate inner
    startRow = startRow + 7 + 4

    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "itabModify.excel-like-table", 7, 4)
    SeedItabTemplate inner
    startRow = startRow + 7 + 4

    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "itabDelete.excel-like-table", 7, 4)
    SeedItabTemplate inner
    startRow = startRow + 7 + 4

    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "itabCollect.excel-like-table", 7, 4)
    SeedItabTemplate inner
    startRow = startRow + 7 + 4

    ' message.excel-like-table (8x6)
    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "message.excel-like-table", 8, 6)
    SetTemplateCell inner, "A1", "{labels.msgClass}"
    SetTemplateCell inner, "B1", "{msgClass.description}"
    SetTemplateCell inner, "C1", "{labels.msgNo}"
    SetTemplateCell inner, "D1", "{msgNo.description}"
    SetTemplateCell inner, "E1", "{labels.displayLike}"
    SetTemplateCell inner, "F1", "{displayLike.description}"
    SetTemplateCell inner, "A2", "{labels.messageText}"
    SetTemplateCell inner, "B2", "{messageText.description}"
    SetTemplateCell inner, "A3", "{labels.with1}"
    SetTemplateCell inner, "B3", "{with[0].description}"
    SetTemplateCell inner, "A4", "{labels.with2}"
    SetTemplateCell inner, "B4", "{with[1].description}"
    SetTemplateCell inner, "A5", "{labels.with3}"
    SetTemplateCell inner, "B5", "{with[2].description}"
    SetTemplateCell inner, "A6", "{labels.with4}"
    SetTemplateCell inner, "B6", "{with[3].description}"
    SetTemplateCell inner, "A7", "{labels.into}"
    SetTemplateCell inner, "B7", "{into.description}"
    SetTemplateCell inner, "A8", "{labels.raising}"
    SetTemplateCell inner, "B8", "{raising.description}"
    startRow = startRow + 8 + 4

    ' perform.excel-like-table (9x3)
    Set inner = CreateTemplateBlock(wsCfg, startRow, startCol, "perform.excel-like-table", 9, 3)
    SeedPerformTemplate inner
End Sub

Private Sub SeedItabTemplate(ByVal inner As Range)
    SetTemplateCell inner, "A1", "Internal table"
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
    SetTemplateCell inner, "A1", "Perform"
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

    With inner.Borders
        .LineStyle = xlContinuous
        .Weight = xlThin
        .ColorIndex = 1
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
