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

    On Error GoTo ErrHandler

    prevScreenUpdating = Application.ScreenUpdating
    prevEnableEvents = Application.EnableEvents
    prevCalculation = Application.Calculation

    Application.ScreenUpdating = False
    Application.EnableEvents = False
    Application.Calculation = xlCalculationManual

    Dim wsIn As Worksheet
    Dim wsOut As Worksheet
    Dim wsReport As Worksheet
    Set wsIn = GetOrCreateWorksheet(ThisWorkbook, INPUT_SHEET)
    Set wsOut = GetOrCreateWorksheet(ThisWorkbook, OUTPUT_SHEET)
    Set wsReport = GetOrCreateWorksheet(ThisWorkbook, REPORT_SHEET)

    Dim abapCode As String
    abapCode = ReadAbapCodeFromSheet(wsIn)
    If Len(Trim$(abapCode)) = 0 Then
        Err.Raise vbObjectError + 1000, "BuildAbapFlowDiagram", "Sheet1 khong co du lieu ABAP."
    End If

    Dim isDefined As Object
    Dim orderKeys As Collection
    Dim globalData As Collection
    Dim globalConstants As Collection
    Dim subsDict As Object
    Set subsDict = ParseAbapProgram(abapCode, isDefined, orderKeys, globalData, globalConstants)

    BuildCalledByIndex subsDict

    Dim cycleEdges As Object
    Dim cycleNodes As Object
    Set cycleEdges = FindCycleEdges(subsDict, cycleNodes)

    Dim depths As Object
    Set depths = ComputeHierarchyDepths(subsDict, cycleEdges)

    DrawFlowDiagram wsOut, subsDict, depths, cycleEdges, cycleNodes, isDefined, orderKeys
    WriteObjectsReport wsReport, subsDict, depths, cycleNodes, isDefined, orderKeys, globalData, globalConstants

CleanExit:
    Application.ScreenUpdating = prevScreenUpdating
    Application.EnableEvents = prevEnableEvents
    Application.Calculation = prevCalculation
    Exit Sub

ErrHandler:
    MsgBox "Loi: " & Err.Description, vbExclamation, "ABAP Flow Diagram"
    Resume CleanExit
End Sub

' -----------------------------
' Parsing
' -----------------------------

Private Function ParseAbapProgram(ByVal abapCode As String, ByRef isDefined As Object, ByRef orderKeys As Collection, ByRef globalData As Collection, ByRef globalConstants As Collection) As Object
    Dim subsDict As Object
    Set subsDict = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = clsSubroutine

    Set isDefined = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = True
    Set orderKeys = New Collection ' UCase(name) in order of first encounter
    Set globalData = New Collection
    Set globalConstants = New Collection

    Dim codeText As String
    codeText = NormalizeLineBreaks(abapCode)

    Dim lines() As String
    lines = Split(codeText, vbLf)

    ' Strip comments first to simplify statement collection
    Dim i As Long
    For i = LBound(lines) To UBound(lines)
        lines(i) = StripAbapComments(lines(i))
    Next i

    i = LBound(lines)
    Do While i <= UBound(lines)
        Dim line As String
        line = lines(i)

        If StartsWithWord(line, "FORM") Then
            Dim headerEnd As Long
            Dim headerStmt As String
            headerStmt = CollectStatement(lines, i, headerEnd)

            Dim formName As String
            formName = ExtractFormName(headerStmt)
            If Len(formName) = 0 Then
                i = headerEnd + 1
                GoTo ContinueLoop
            End If

            Dim key As String
            key = UCase$(formName)

            Dim subr As clsSubroutine
            Set subr = GetOrCreateSubroutine(subsDict, orderKeys, key, formName)
            subr.Kind = "FORM"
            isDefined(key) = True

            ParseFormHeader headerStmt, subr

            Dim endFormIdx As Long
            endFormIdx = FindEndFormIndex(lines, headerEnd + 1)
            If endFormIdx < 0 Then endFormIdx = UBound(lines)

            ParseFormBody lines, headerEnd + 1, endFormIdx - 1, subr, subsDict, orderKeys

            i = endFormIdx + 1
        ElseIf StartsWithWord(line, "DATA") And Not StartsWithInlineData(line) Then
            Dim stmtEndG1 As Long
            Dim stmtG1 As String
            stmtG1 = CollectStatement(lines, i, stmtEndG1)
            ParseGlobalDeclarationStatement stmtG1, "DATA", globalData, globalConstants
            i = stmtEndG1 + 1
        ElseIf StartsWithWord(line, "CONSTANTS") Then
            Dim stmtEndG2 As Long
            Dim stmtG2 As String
            stmtG2 = CollectStatement(lines, i, stmtEndG2)
            ParseGlobalDeclarationStatement stmtG2, "CONSTANTS", globalData, globalConstants
            i = stmtEndG2 + 1
        Else
            Dim headerEnd2 As Long
            Dim headerStmt2 As String
            headerStmt2 = CollectStatement(lines, i, headerEnd2)

            Dim eventName As String
            If TryParseEventHeader(headerStmt2, eventName) Then
                Dim eventKey As String
                eventKey = "EVENT:" & UCase$(eventName)

                Dim evt As clsSubroutine
                Set evt = GetOrCreateSubroutine(subsDict, orderKeys, eventKey, eventName)
                evt.Kind = "EVENT"
                isDefined(eventKey) = True

                Dim nextIdx As Long
                nextIdx = FindNextTopLevelBlockIndex(lines, headerEnd2 + 1)
                If nextIdx < 0 Then nextIdx = UBound(lines) + 1

                ParseFormBody lines, headerEnd2 + 1, nextIdx - 1, evt, subsDict, orderKeys
                i = nextIdx
            Else
                i = i + 1
            End If
        End If

ContinueLoop:
    Loop

    Set ParseAbapProgram = subsDict
End Function

Private Function FindNextTopLevelBlockIndex(ByRef lines() As String, ByVal startIdx As Long) As Long
    Dim i As Long
    For i = startIdx To UBound(lines)
        Dim line As String
        line = lines(i)

        If StartsWithWord(line, "FORM") Then
            FindNextTopLevelBlockIndex = i
            Exit Function
        End If

        If Len(Trim$(line)) = 0 Then GoTo ContinueLoop

        Dim headerEnd As Long
        Dim headerStmt As String
        headerStmt = CollectStatement(lines, i, headerEnd)

        Dim eventName As String
        If TryParseEventHeader(headerStmt, eventName) Then
            FindNextTopLevelBlockIndex = i
            Exit Function
        End If

ContinueLoop:
    Next i
    FindNextTopLevelBlockIndex = UBound(lines) + 1
End Function

Private Function TryParseEventHeader(ByVal headerStmt As String, ByRef eventName As String) As Boolean
    Dim s As String
    s = NormalizeSpaces(Replace(headerStmt, vbLf, " "))
    s = NormalizeSpaces(Replace(s, vbCr, " "))
    s = Trim$(s)
    If Right$(s, 1) = "." Then s = Left$(s, Len(s) - 1)
    s = Trim$(s)
    If Len(s) = 0 Then Exit Function

    Dim u As String
    u = UCase$(s)

    If u = "LOAD-OF-PROGRAM" _
        Or u = "INITIALIZATION" _
        Or u = "START-OF-SELECTION" _
        Or u = "END-OF-SELECTION" _
        Or u = "TOP-OF-PAGE" _
        Or Left$(u, Len("TOP-OF-PAGE ")) = "TOP-OF-PAGE " _
        Or u = "END-OF-PAGE" _
        Or u = "AT LINE-SELECTION" _
        Or u = "AT USER-COMMAND" _
        Or Left$(u, Len("AT SELECTION-SCREEN")) = "AT SELECTION-SCREEN" Then
        eventName = u
        TryParseEventHeader = True
    End If
End Function

Private Function ExtractFormName(ByVal headerStmt As String) As String
    Dim stmt As String
    stmt = NormalizeSpaces(Replace(headerStmt, vbLf, " "))
    stmt = NormalizeSpaces(Replace(stmt, vbCr, " "))
    stmt = Trim$(stmt)
    If Right$(stmt, 1) = "." Then stmt = Left$(stmt, Len(stmt) - 1)
    If Not StartsWithWord(stmt, "FORM") Then Exit Function

    Dim afterForm As String
    afterForm = Trim$(Mid$(stmt, Len("FORM") + 1))
    ExtractFormName = ExtractFirstIdentifier(afterForm)
End Function

Private Sub ParseFormHeader(ByVal headerStmt As String, ByVal subr As clsSubroutine)
    Dim stmt As String
    stmt = NormalizeSpaces(Replace(headerStmt, vbLf, " "))
    stmt = NormalizeSpaces(Replace(stmt, vbCr, " "))
    stmt = NormalizeSpaces(stmt)
    stmt = Trim$(stmt)

    If Right$(stmt, 1) = "." Then stmt = Left$(stmt, Len(stmt) - 1)
    If Not StartsWithWord(stmt, "FORM") Then Exit Sub

    Dim afterForm As String
    afterForm = Trim$(Mid$(stmt, Len("FORM") + 1))

    Dim formName As String
    formName = ExtractFirstIdentifier(afterForm)
    If Len(formName) = 0 Then Exit Sub

    subr.Name = formName

    Dim posName As Long
    posName = InStr(1, afterForm, formName, vbTextCompare)
    If posName <= 0 Then Exit Sub

    Dim rest As String
    rest = Trim$(Mid$(afterForm, posName + Len(formName)))
    If Len(rest) = 0 Then Exit Sub

    ParseParameterSections rest, subr
End Sub

Private Sub ParseParameterSections(ByVal rest As String, ByVal subr As clsSubroutine)
    Dim t As String
    t = NormalizeSpaces(rest)
    If Len(t) = 0 Then Exit Sub

    Dim tokens() As String
    tokens = Split(t, " ")

    Dim curType As String
    Dim seg As String
    curType = vbNullString
    seg = vbNullString

    Dim i As Long
    For i = LBound(tokens) To UBound(tokens)
        Dim w As String
        w = UCase$(CleanToken(tokens(i)))

        If w = "TABLES" Or w = "USING" Or w = "CHANGING" Or w = "RAISING" Then
            If Len(curType) > 0 Then
                ParseParameterSegment curType, seg, subr
            End If
            curType = w
            seg = vbNullString
        ElseIf Len(curType) > 0 Then
            If Len(seg) > 0 Then seg = seg & " "
            seg = seg & tokens(i)
        End If
    Next i

    If Len(curType) > 0 Then
        ParseParameterSegment curType, seg, subr
    End If
End Sub

Private Sub ParseParameterSegment(ByVal paramType As String, ByVal segment As String, ByVal subr As clsSubroutine)
    Dim t As String
    t = NormalizeSpaces(segment)
    If Len(t) = 0 Then Exit Sub

    Dim tokens() As String
    tokens = Split(t, " ")

    Dim idx As Long
    idx = LBound(tokens)
    Do While idx <= UBound(tokens)
        Dim tok As String
        tok = CleanToken(tokens(idx))

        If Len(tok) = 0 Then
            idx = idx + 1
        Else
            Dim uTok As String
            uTok = UCase$(tok)

            If uTok = "TYPE" Or uTok = "LIKE" Or uTok = "OPTIONAL" Or uTok = "DEFAULT" Then
                idx = idx + 1
            Else
                Dim paramName As String
                paramName = ExtractParameterName(tok)

                If Len(paramName) = 0 Then
                    idx = idx + 1
                Else
                    Dim dataType As String
                    dataType = vbNullString

                    If idx + 2 <= UBound(tokens) Then
                        If UCase$(CleanToken(tokens(idx + 1))) = "TYPE" Then
                            dataType = ExtractIdentifier(CleanToken(tokens(idx + 2)))
                            idx = idx + 3
                        Else
                            idx = idx + 1
                        End If
                    Else
                        idx = idx + 1
                    End If

                    Dim p As clsParameter
                    Set p = New clsParameter
                    p.ParamType = paramType
                    p.Name = paramName
                    p.DataType = dataType
                    subr.AddParameter p
                End If
            End If
        End If
    Loop
End Sub

Private Sub ParseFormBody(ByRef lines() As String, ByVal startIdx As Long, ByVal endIdx As Long, ByVal subr As clsSubroutine, ByVal subsDict As Object, ByVal orderKeys As Collection)
    If startIdx > endIdx Then Exit Sub

    Dim i As Long
    i = startIdx
    Do While i <= endIdx
        Dim line As String
        line = Trim$(lines(i))

        If Len(line) = 0 Then
            i = i + 1
        ElseIf StartsWithWord(line, "DATA") And Not StartsWithInlineData(line) Then
            Dim stmtEnd As Long
            Dim stmt As String
            stmt = CollectStatement(lines, i, stmtEnd)
            ParseDeclarationStatement stmt, "DATA", subr
            i = stmtEnd + 1
        ElseIf StartsWithWord(line, "CONSTANTS") Then
            Dim stmtEnd2 As Long
            Dim stmt2 As String
            stmt2 = CollectStatement(lines, i, stmtEnd2)
            ParseDeclarationStatement stmt2, "CONSTANTS", subr
            i = stmtEnd2 + 1
        ElseIf StartsWithWord(line, "PERFORM") Then
            Dim stmtEnd3 As Long
            Dim stmt3 As String
            stmt3 = CollectStatement(lines, i, stmtEnd3)
            AddPerformCallings stmt3, subr, subsDict, orderKeys
            i = stmtEnd3 + 1
        Else
            AddPerformCallings line, subr, subsDict, orderKeys
            i = i + 1
        End If
    Loop
End Sub

Private Sub AddPerformCallings(ByVal stmt As String, ByVal subr As clsSubroutine, ByVal subsDict As Object, ByVal orderKeys As Collection)
    Dim s As String
    s = NormalizeSpaces(Replace(stmt, vbLf, " "))
    s = NormalizeSpaces(Replace(s, vbCr, " "))
    s = Trim$(s)
    If Len(s) = 0 Then Exit Sub

    Dim searchPos As Long
    searchPos = 1

    Do While searchPos <= Len(s)
        Dim pos As Long
        pos = FindWordOutsideQuotesFrom(s, "PERFORM", searchPos)
        If pos <= 0 Then Exit Do

        Dim idx As Long
        idx = pos + Len("PERFORM")
        Do While idx <= Len(s) And Mid$(s, idx, 1) = " "
            idx = idx + 1
        Loop

        Dim callee As String
        callee = ReadIdentifierFrom(s, idx)
        If Len(callee) > 0 Then
            Dim calleeKey As String
            calleeKey = UCase$(callee)
            subr.AddCalling callee
            Call GetOrCreateSubroutine(subsDict, orderKeys, calleeKey, callee)
        End If

        searchPos = idx + 1
    Loop
End Sub

Private Sub ParseDeclarationStatement(ByVal stmt As String, ByVal declType As String, ByVal subr As clsSubroutine)
    Dim decls As Collection
    Set decls = ParseDeclarationsFromStatement(stmt, declType)

    Dim decl As clsDataDeclaration
    For Each decl In decls
        If UCase$(declType) = "DATA" Then
            subr.AddLocalData decl
        Else
            subr.AddLocalConstant decl
        End If
    Next decl
End Sub

Private Sub ParseGlobalDeclarationStatement(ByVal stmt As String, ByVal declType As String, ByVal globalData As Collection, ByVal globalConstants As Collection)
    Dim decls As Collection
    Set decls = ParseDeclarationsFromStatement(stmt, declType)

    Dim decl As clsDataDeclaration
    For Each decl In decls
        If UCase$(declType) = "DATA" Then
            globalData.Add decl
        Else
            globalConstants.Add decl
        End If
    Next decl
End Sub

Private Function ParseDeclarationsFromStatement(ByVal stmt As String, ByVal declType As String) As Collection
    Dim result As Collection
    Set result = New Collection

    Dim s As String
    s = NormalizeSpaces(Replace(stmt, vbLf, " "))
    s = NormalizeSpaces(Replace(s, vbCr, " "))
    s = Trim$(s)

    If Right$(s, 1) = "." Then s = Left$(s, Len(s) - 1)
    s = Trim$(s)
    If Len(s) = 0 Then
        Set ParseDeclarationsFromStatement = result
        Exit Function
    End If

    Dim prefix As String
    prefix = UCase$(Trim$(declType))
    If Len(prefix) = 0 Then
        Set ParseDeclarationsFromStatement = result
        Exit Function
    End If

    Dim u As String
    u = UCase$(s)
    If Left$(u, Len(prefix)) <> prefix Then
        Set ParseDeclarationsFromStatement = result
        Exit Function
    End If

    s = Trim$(Mid$(s, Len(prefix) + 1))
    If Left$(s, 1) = ":" Then s = Trim$(Mid$(s, 2))
    If Len(s) = 0 Then
        Set ParseDeclarationsFromStatement = result
        Exit Function
    End If

    ' Bo qua cac khai bao phuc tap (BEGIN OF / END OF / INCLUDE TYPE ...)
    Dim sUpper As String
    sUpper = UCase$(s)
    If StartsWithWord(sUpper, "BEGIN") Or StartsWithWord(sUpper, "END") Or StartsWithWord(sUpper, "INCLUDE") Then
        Set ParseDeclarationsFromStatement = result
        Exit Function
    End If

    Dim parts As Variant
    parts = SplitByCommaOutsideQuotes(s)

    Dim i As Long
    For i = LBound(parts) To UBound(parts)
        Dim part As String
        part = Trim$(CStr(parts(i)))
        If Len(part) = 0 Then GoTo ContinuePart

        Dim varName As String
        varName = ExtractFirstIdentifier(part)
        If Len(varName) = 0 Then GoTo ContinuePart

        Dim dataType As String
        dataType = ExtractTypeAfterKeyword(part, "TYPE")

        Dim valueText As String
        valueText = vbNullString
        If UCase$(prefix) = "CONSTANTS" Then
            valueText = ExtractValueAfterKeyword(part, "VALUE")
        End If

        Dim decl As clsDataDeclaration
        Set decl = New clsDataDeclaration
        decl.VariableName = varName
        decl.DataType = dataType
        decl.DeclarationType = UCase$(prefix)
        decl.Value = valueText

        result.Add decl

ContinuePart:
    Next i

    Set ParseDeclarationsFromStatement = result
End Function

Private Sub BuildCalledByIndex(ByVal subsDict As Object)
    Dim callerKey As Variant
    For Each callerKey In subsDict.Keys
        Dim caller As clsSubroutine
        Set caller = subsDict(callerKey)

        Dim callee As Variant
        For Each callee In caller.Callings
            Dim calleeKey As String
            calleeKey = UCase$(CStr(callee))
            If subsDict.Exists(calleeKey) Then
                subsDict(calleeKey).AddCalledBy caller.Name
            End If
        Next callee
    Next callerKey
End Sub

' -----------------------------
' Cycle detection & hierarchy
' -----------------------------

Private Function FindCycleEdges(ByVal subsDict As Object, ByRef cycleNodes As Object) As Object
    Dim cycleEdges As Object
    Set cycleEdges = CreateObject("Scripting.Dictionary") ' key = "CALLER->CALLEE"

    Set cycleNodes = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = True

    ResetProcessedFlags subsDict

    Dim inStack As Object
    Set inStack = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = True

    Dim stack As Collection
    Set stack = New Collection ' DFS path (UCase(name))

    Dim key As Variant
    For Each key In subsDict.Keys
        If Not subsDict(key).IsProcessed Then
            DfsFindCycles CStr(key), subsDict, inStack, stack, cycleEdges, cycleNodes
        End If
    Next key

    Set FindCycleEdges = cycleEdges
End Function

Private Sub DfsFindCycles(ByVal nodeKey As String, ByVal subsDict As Object, ByVal inStack As Object, ByVal stack As Collection, ByVal cycleEdges As Object, ByVal cycleNodes As Object)
    If subsDict(nodeKey).IsProcessed Then Exit Sub

    inStack(nodeKey) = True
    stack.Add nodeKey

    Dim callee As Variant
    For Each callee In subsDict(nodeKey).Callings
        Dim calleeKey As String
        calleeKey = UCase$(CStr(callee))
        If Not subsDict.Exists(calleeKey) Then GoTo ContinueCallee

        If inStack.Exists(calleeKey) Then
            cycleEdges(EdgeKey(nodeKey, calleeKey)) = True
            MarkCycleNodes stack, calleeKey, cycleNodes
        ElseIf Not subsDict(calleeKey).IsProcessed Then
            DfsFindCycles calleeKey, subsDict, inStack, stack, cycleEdges, cycleNodes
        End If

ContinueCallee:
    Next callee

    If inStack.Exists(nodeKey) Then inStack.Remove nodeKey
    If stack.Count > 0 Then stack.Remove stack.Count
    subsDict(nodeKey).IsProcessed = True
End Sub

Private Function ComputeHierarchyDepths(ByVal subsDict As Object, ByVal cycleEdges As Object) As Object
    Dim depth As Object
    Set depth = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = Long

    Dim inDeg As Object
    Set inDeg = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = Long

    Dim key As Variant
    For Each key In subsDict.Keys
        depth(key) = 0
        inDeg(key) = 0
    Next key

    ' Build indegree excluding cycle edges (back edges)
    For Each key In subsDict.Keys
        Dim caller As clsSubroutine
        Set caller = subsDict(key)

        Dim callee As Variant
        For Each callee In caller.Callings
            Dim calleeKey As String
            calleeKey = UCase$(CStr(callee))
            If subsDict.Exists(calleeKey) Then
                If Not cycleEdges.Exists(EdgeKey(CStr(key), calleeKey)) Then
                    inDeg(calleeKey) = CLng(inDeg(calleeKey)) + 1
                End If
            End If
        Next callee
    Next key

    Dim q As Collection
    Set q = New Collection

    For Each key In subsDict.Keys
        If CLng(inDeg(key)) = 0 Then q.Add CStr(key)
    Next key

    Dim processed As Long
    Do While q.Count > 0
        Dim cur As String
        cur = q(1)
        q.Remove 1
        processed = processed + 1

        Dim curDepth As Long
        curDepth = CLng(depth(cur))

        Dim caller2 As clsSubroutine
        Set caller2 = subsDict(cur)

        Dim callee2 As Variant
        For Each callee2 In caller2.Callings
            Dim calleeKey2 As String
            calleeKey2 = UCase$(CStr(callee2))
            If subsDict.Exists(calleeKey2) Then
                If Not cycleEdges.Exists(EdgeKey(cur, calleeKey2)) Then
                    If CLng(depth(calleeKey2)) < curDepth + 1 Then depth(calleeKey2) = curDepth + 1

                    inDeg(calleeKey2) = CLng(inDeg(calleeKey2)) - 1
                    If CLng(inDeg(calleeKey2)) = 0 Then q.Add calleeKey2
                End If
            End If
        Next callee2
    Loop

    ' Fallback: neu vi ly do nao do van con node chua duoc topo-sort, giu depth = 0 de van ve duoc.
    Set ComputeHierarchyDepths = depth
End Function

' -----------------------------
' Rendering (Sheet2)
' -----------------------------

Private Sub DrawFlowDiagram(ByVal ws As Worksheet, ByVal subsDict As Object, ByVal depths As Object, ByVal cycleEdges As Object, ByVal cycleNodes As Object, ByVal isDefined As Object, ByVal orderKeys As Collection)
    ClearSheet ws

    Dim maxDepth As Long
    maxDepth = 0

    Dim key As Variant
    For Each key In subsDict.Keys
        If depths.Exists(key) Then
            If CLng(depths(key)) > maxDepth Then maxDepth = CLng(depths(key))
        End If
    Next key

    Dim shapesByKey As Object
    Set shapesByKey = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = Shape

    Const START_X As Single = 20
    Const START_Y As Single = 20
    Const BOX_W As Single = 220
    Const BOX_H As Single = 70
    Const H_GAP As Single = 90
    Const V_GAP As Single = 20

    Dim d As Long
    For d = 0 To maxDepth
        Dim x As Single
        x = START_X + d * (BOX_W + H_GAP)

        Dim y As Single
        y = START_Y

        ' Uu tien ve theo thu tu xuat hien trong code
        Dim idx As Long
        For idx = 1 To orderKeys.Count
            Dim nodeKey As String
            nodeKey = CStr(orderKeys(idx))

            If shapesByKey.Exists(nodeKey) Then GoTo ContinueNode
            If Not depths.Exists(nodeKey) Then GoTo ContinueNode
            If CLng(depths(nodeKey)) <> d Then GoTo ContinueNode

            Dim subr As clsSubroutine
            Set subr = subsDict(nodeKey)

            Dim defined As Boolean
            defined = isDefined.Exists(nodeKey)

            Dim inCycle As Boolean
            inCycle = cycleNodes.Exists(nodeKey)

            Set shapesByKey(nodeKey) = AddSubroutineShape(ws, subr, x, y, BOX_W, BOX_H, defined, inCycle)
            y = y + BOX_H + V_GAP

ContinueNode:
        Next idx

        ' Bo sung node khong co trong orderKeys (hiem)
        For Each key In subsDict.Keys
            nodeKey = CStr(key)
            If shapesByKey.Exists(nodeKey) Then GoTo ContinueNode2
            If Not depths.Exists(nodeKey) Then GoTo ContinueNode2
            If CLng(depths(nodeKey)) <> d Then GoTo ContinueNode2

            Set subr = subsDict(nodeKey)
            defined = isDefined.Exists(nodeKey)
            inCycle = cycleNodes.Exists(nodeKey)

            Set shapesByKey(nodeKey) = AddSubroutineShape(ws, subr, x, y, BOX_W, BOX_H, defined, inCycle)
            y = y + BOX_H + V_GAP

ContinueNode2:
        Next key
    Next d

    ' Draw arrows (calling relationships)
    Dim callerKey As Variant
    For Each callerKey In subsDict.Keys
        Dim callerShape As Shape
        If Not shapesByKey.Exists(callerKey) Then GoTo ContinueCaller
        Set callerShape = shapesByKey(callerKey)

        Dim caller As clsSubroutine
        Set caller = subsDict(callerKey)

        Dim callee As Variant
        For Each callee In caller.Callings
            Dim calleeKey As String
            calleeKey = UCase$(CStr(callee))
            If Not shapesByKey.Exists(calleeKey) Then GoTo ContinueCallee

            Dim calleeShape As Shape
            Set calleeShape = shapesByKey(calleeKey)

            Dim conn As Shape
            Set conn = ws.Shapes.AddConnector(msoConnectorElbow, 0, 0, 0, 0)
            conn.ConnectorFormat.BeginConnect callerShape, 2
            conn.ConnectorFormat.EndConnect calleeShape, 4
            conn.RerouteConnections
            conn.Line.EndArrowheadStyle = msoArrowheadTriangle
            conn.ZOrder msoSendToBack
            conn.Placement = xlFreeFloating

            If cycleEdges.Exists(EdgeKey(CStr(callerKey), calleeKey)) Then
                conn.Line.ForeColor.RGB = RGB(192, 0, 0)
                conn.Line.Weight = 2
                conn.Line.DashStyle = msoLineDash
            Else
                conn.Line.ForeColor.RGB = RGB(90, 90, 90)
                conn.Line.Weight = 1.25
            End If

ContinueCallee:
        Next callee

ContinueCaller:
    Next callerKey
End Sub

Private Function AddSubroutineShape(ByVal ws As Worksheet, ByVal subr As clsSubroutine, ByVal left As Single, ByVal top As Single, ByVal width As Single, ByVal height As Single, ByVal isDefined As Boolean, ByVal inCycle As Boolean) As Shape
    Dim sh As Shape
    Set sh = ws.Shapes.AddShape(msoShapeRoundedRectangle, left, top, width, height)
    sh.Placement = xlFreeFloating

    Dim title As String
    Dim kind As String
    kind = UCase$(Trim$(subr.Kind))
    If Len(kind) = 0 Then kind = "FORM"

    If kind = "EVENT" Then
        title = subr.Name
    Else
        title = "FORM " & subr.Name
    End If

    Dim meta As String
    meta = "Params: " & CStr(subr.Parameters.Count) & " | DATA: " & CStr(subr.LocalData.Count)

    Dim meta2 As String
    meta2 = "CONST: " & CStr(subr.LocalConstants.Count)

    sh.TextFrame.Characters.Text = title & vbCrLf & meta & vbCrLf & meta2
    sh.TextFrame.HorizontalAlignment = xlHAlignCenter
    sh.TextFrame.VerticalAlignment = xlVAlignCenter
    sh.TextFrame.MarginLeft = 6
    sh.TextFrame.MarginRight = 6
    sh.TextFrame.MarginTop = 6
    sh.TextFrame.MarginBottom = 6
    sh.TextFrame.Characters.Font.Color = RGB(0, 0, 0)

    sh.Line.ForeColor.RGB = RGB(60, 60, 60)
    sh.Line.Weight = 1

    If inCycle Then
        sh.Fill.ForeColor.RGB = RGB(255, 199, 206) ' light red
    ElseIf Not isDefined Then
        sh.Fill.ForeColor.RGB = RGB(217, 217, 217) ' gray for missing/external
    ElseIf kind = "EVENT" Then
        sh.Fill.ForeColor.RGB = RGB(226, 239, 218) ' light green
    Else
        sh.Fill.ForeColor.RGB = RGB(221, 235, 247) ' light blue
    End If
    sh.Fill.Solid

    Set AddSubroutineShape = sh
End Function

Private Sub ClearSheet(ByVal ws As Worksheet)
    Dim i As Long
    For i = ws.Shapes.Count To 1 Step -1
        ws.Shapes(i).Delete
    Next i
    ws.Cells.Clear
End Sub

' -----------------------------
' Text report (Objects sheet)
' -----------------------------

Private Sub WriteObjectsReport(ByVal ws As Worksheet, ByVal subsDict As Object, ByVal depths As Object, ByVal cycleNodes As Object, ByVal isDefined As Object, ByVal orderKeys As Collection, ByVal globalData As Collection, ByVal globalConstants As Collection)
    On Error GoTo ErrHandler

    ws.Cells.Clear

    Dim i As Long
    For i = ws.Shapes.Count To 1 Step -1
        ws.Shapes(i).Delete
    Next i

    Dim rowIdx As Long
    rowIdx = 1

    ws.Cells(rowIdx, 1).Value = "ABAP Objects Report"
    ws.Cells(rowIdx, 1).Font.Bold = True
    rowIdx = rowIdx + 2

    ' Global declarations
    ws.Cells(rowIdx, 1).Value = "Global Declarations"
    ws.Cells(rowIdx, 1).Font.Bold = True
    rowIdx = rowIdx + 1

    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 4)).Value = Array("DeclarationType", "Name", "DataType", "Value")
    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 4)).Font.Bold = True
    rowIdx = rowIdx + 1

    Dim decl As clsDataDeclaration
    For Each decl In globalData
        ws.Cells(rowIdx, 1).Value = decl.DeclarationType
        ws.Cells(rowIdx, 2).Value = decl.VariableName
        ws.Cells(rowIdx, 3).Value = decl.DataType
        ws.Cells(rowIdx, 4).Value = decl.Value
        rowIdx = rowIdx + 1
    Next decl

    For Each decl In globalConstants
        ws.Cells(rowIdx, 1).Value = decl.DeclarationType
        ws.Cells(rowIdx, 2).Value = decl.VariableName
        ws.Cells(rowIdx, 3).Value = decl.DataType
        ws.Cells(rowIdx, 4).Value = decl.Value
        rowIdx = rowIdx + 1
    Next decl

    rowIdx = rowIdx + 2

    ' Objects table
    ws.Cells(rowIdx, 1).Value = "Objects"
    ws.Cells(rowIdx, 1).Font.Bold = True
    rowIdx = rowIdx + 1

    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 10)).Value = Array( _
        "Kind", "Name", "Depth", "Params", "LocalData", "LocalConst", "Defined", "InCycle", _
        "InfoType", "InfoValue" _
    )
    ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 10)).Font.Bold = True
    rowIdx = rowIdx + 1

    Dim idx As Long
    For idx = 1 To orderKeys.Count
        Dim key As String
        key = CStr(orderKeys(idx))
        If Not subsDict.Exists(key) Then GoTo ContinueKey

        Dim subr As clsSubroutine
        Set subr = subsDict(key)

        Dim kind As String
        kind = UCase$(Trim$(subr.Kind))
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

        WriteObjectInfoRows ws, rowIdx, kind, subr.Name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Parameters", GetParameterLines(subr.Parameters)
        WriteObjectInfoRows ws, rowIdx, kind, subr.Name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Local DATA", GetDeclarationLines(subr.LocalData, False)
        WriteObjectInfoRows ws, rowIdx, kind, subr.Name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Local CONSTANTS", GetDeclarationLines(subr.LocalConstants, True)
        WriteObjectInfoRows ws, rowIdx, kind, subr.Name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "Callings", GetVariantLines(subr.Callings)
        WriteObjectInfoRows ws, rowIdx, kind, subr.Name, depth, paramCount, localDataCount, localConstCount, defined, inCycle, "CalledBy", GetVariantLines(subr.CalledBy)

ContinueKey:
    Next idx

    ws.Columns("A:J").EntireColumn.AutoFit
    ws.Columns("J:J").WrapText = True
    ws.Columns("J:J").ColumnWidth = 60
    ws.Rows("1:" & CStr(rowIdx)).EntireRow.AutoFit
    ws.Rows("1:1").RowHeight = 20

    Exit Sub

ErrHandler:
    ' Keep the main macro running even if report formatting fails
End Sub

Private Sub WriteObjectInfoRows(ByVal ws As Worksheet, ByRef rowIdx As Long, ByVal kind As String, ByVal name As String, ByVal depth As Long, ByVal paramsCount As Long, ByVal localDataCount As Long, ByVal localConstCount As Long, ByVal defined As Boolean, ByVal inCycle As Boolean, ByVal infoType As String, ByVal values As Collection)
    If values Is Nothing Then
        WriteObjectInfoRow ws, rowIdx, kind, name, depth, paramsCount, localDataCount, localConstCount, defined, inCycle, infoType, vbNullString
        rowIdx = rowIdx + 1
        Exit Sub
    End If

    If values.Count = 0 Then
        WriteObjectInfoRow ws, rowIdx, kind, name, depth, paramsCount, localDataCount, localConstCount, defined, inCycle, infoType, vbNullString
        rowIdx = rowIdx + 1
        Exit Sub
    End If

    Dim v As Variant
    For Each v In values
        WriteObjectInfoRow ws, rowIdx, kind, name, depth, paramsCount, localDataCount, localConstCount, defined, inCycle, infoType, CStr(v)
        rowIdx = rowIdx + 1
    Next v
End Sub

Private Sub WriteObjectInfoRow(ByVal ws As Worksheet, ByVal rowIdx As Long, ByVal kind As String, ByVal name As String, ByVal depth As Long, ByVal paramsCount As Long, ByVal localDataCount As Long, ByVal localConstCount As Long, ByVal defined As Boolean, ByVal inCycle As Boolean, ByVal infoType As String, ByVal infoValue As String)
    ws.Cells(rowIdx, 1).Value = kind
    ws.Cells(rowIdx, 2).Value = name
    ws.Cells(rowIdx, 3).Value = depth
    ws.Cells(rowIdx, 4).Value = paramsCount
    ws.Cells(rowIdx, 5).Value = localDataCount
    ws.Cells(rowIdx, 6).Value = localConstCount
    ws.Cells(rowIdx, 7).Value = IIf(defined, "Yes", "No")
    ws.Cells(rowIdx, 8).Value = IIf(inCycle, "Yes", "No")
    ws.Cells(rowIdx, 9).Value = infoType
    ws.Cells(rowIdx, 10).Value = infoValue

    Dim rowRange As Range
    Set rowRange = ws.Range(ws.Cells(rowIdx, 1), ws.Cells(rowIdx, 10))

    If inCycle Then
        rowRange.Interior.Color = RGB(255, 199, 206)
    ElseIf Not defined Then
        rowRange.Interior.Color = RGB(217, 217, 217)
    ElseIf kind = "EVENT" Then
        rowRange.Interior.Color = RGB(226, 239, 218)
    End If
End Sub

Private Function JoinVariantCollection(ByVal col As Collection, ByVal delimiter As String) As String
    Dim out As String
    out = vbNullString

    Dim v As Variant
    For Each v In col
        If Len(out) > 0 Then out = out & delimiter
        out = out & CStr(v)
    Next v

    JoinVariantCollection = out
End Function

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

Private Function GetParameterLines(ByVal params As Collection) As Collection
    Dim result As Collection
    Set result = New Collection

    If params Is Nothing Then
        Set GetParameterLines = result
        Exit Function
    End If

    Dim p As clsParameter
    For Each p In params
        Dim part As String
        part = UCase$(Trim$(p.ParamType)) & " " & p.Name
        If Len(Trim$(p.DataType)) > 0 Then part = part & " TYPE " & p.DataType
        result.Add part
    Next p

    Set GetParameterLines = result
End Function

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
        If Len(Trim$(decl.DataType)) > 0 Then part = part & " TYPE " & decl.DataType
        If includeValue And Len(Trim$(decl.Value)) > 0 Then part = part & " VALUE " & decl.Value
        result.Add part
    Next decl

    Set GetDeclarationLines = result
End Function

Private Function FormatParametersText(ByVal params As Collection) As String
    Dim out As String
    out = vbNullString

    Dim p As clsParameter
    For Each p In params
        Dim part As String
        part = UCase$(Trim$(p.ParamType)) & " " & p.Name
        If Len(Trim$(p.DataType)) > 0 Then part = part & " TYPE " & p.DataType

        If Len(out) > 0 Then out = out & vbLf
        out = out & part
    Next p

    FormatParametersText = out
End Function

Private Function FormatDeclarationsText(ByVal decls As Collection, ByVal includeValue As Boolean) As String
    Dim out As String
    out = vbNullString

    Dim decl As clsDataDeclaration
    For Each decl In decls
        Dim part As String
        part = decl.VariableName
        If Len(Trim$(decl.DataType)) > 0 Then part = part & " TYPE " & decl.DataType
        If includeValue And Len(Trim$(decl.Value)) > 0 Then part = part & " VALUE " & decl.Value

        If Len(out) > 0 Then out = out & vbLf
        out = out & part
    Next decl

    FormatDeclarationsText = out
End Function

' -----------------------------
' Utilities
' -----------------------------

Private Function GetOrCreateWorksheet(ByVal wb As Workbook, ByVal sheetName As String) As Worksheet
    On Error Resume Next
    Set GetOrCreateWorksheet = wb.Worksheets(sheetName)
    On Error GoTo 0

    If GetOrCreateWorksheet Is Nothing Then
        Set GetOrCreateWorksheet = wb.Worksheets.Add(After:=wb.Worksheets(wb.Worksheets.Count))
        GetOrCreateWorksheet.Name = sheetName
    End If
End Function

Private Function ReadAbapCodeFromSheet(ByVal ws As Worksheet) As String
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

Private Function NormalizeLineBreaks(ByVal text As String) As String
    Dim s As String
    s = Replace(text, vbCrLf, vbLf)
    s = Replace(s, vbCr, vbLf)
    NormalizeLineBreaks = s
End Function

Private Function StripAbapComments(ByVal line As String) As String
    Dim s As String
    s = Replace(line, vbTab, " ")

    Dim t As String
    t = LTrim$(s)
    If Len(t) > 0 Then
        If Left$(t, 1) = "*" Then
            StripAbapComments = vbNullString
            Exit Function
        End If
    End If

    Dim pos As Long
    pos = InStr(1, s, """")
    If pos > 0 Then s = Left$(s, pos - 1)

    StripAbapComments = s
End Function

Private Function CollectStatement(ByRef lines() As String, ByVal startIdx As Long, ByRef endIdx As Long) As String
    Dim stmt As String
    stmt = vbNullString

    Dim i As Long
    For i = startIdx To UBound(lines)
        Dim line As String
        line = Trim$(lines(i))
        If Len(line) = 0 Then GoTo ContinueLine

        Dim dotPos As Long
        dotPos = InStr(1, line, ".")
        If dotPos > 0 Then
            If Len(stmt) > 0 Then stmt = stmt & " "
            stmt = stmt & Left$(line, dotPos)
            endIdx = i
            CollectStatement = stmt
            Exit Function
        Else
            If Len(stmt) > 0 Then stmt = stmt & " "
            stmt = stmt & line
        End If

ContinueLine:
    Next i

    endIdx = UBound(lines)
    CollectStatement = stmt
End Function

Private Function FindEndFormIndex(ByRef lines() As String, ByVal startIdx As Long) As Long
    Dim i As Long
    For i = startIdx To UBound(lines)
        If IsEndformLine(lines(i)) Then
            FindEndFormIndex = i
            Exit Function
        End If
    Next i
    FindEndFormIndex = -1
End Function

Private Function NormalizeSpaces(ByVal text As String) As String
    Dim s As String
    s = Replace(text, vbTab, " ")
    s = Trim$(s)
    Do While InStr(1, s, "  ") > 0
        s = Replace(s, "  ", " ")
    Loop
    NormalizeSpaces = s
End Function

Private Function StartsWithKeyword(ByVal text As String, ByVal keyword As String) As Boolean
    Dim t As String
    t = LCase$(Trim$(text))

    Dim k As String
    k = LCase$(Trim$(keyword))

    If Len(t) < Len(k) Then Exit Function

    If Len(t) = Len(k) Then
        StartsWithKeyword = (t = k)
        Exit Function
    End If

    StartsWithKeyword = (Left$(t, Len(k) + 1) = k & " ")
End Function

Private Function StartsWithWord(ByVal text As String, ByVal word As String) As Boolean
    Dim t As String
    t = LCase$(LTrim$(text))

    Dim w As String
    w = LCase$(Trim$(word))

    If Len(t) < Len(w) Then Exit Function
    If Left$(t, Len(w)) <> w Then Exit Function

    If Len(t) = Len(w) Then
        StartsWithWord = True
        Exit Function
    End If

    Dim nextCh As String
    nextCh = Mid$(t, Len(w) + 1, 1)
    StartsWithWord = (nextCh = " " Or nextCh = ":" Or nextCh = ".")
End Function

Private Function StartsWithInlineData(ByVal line As String) As Boolean
    Dim t As String
    t = LCase$(Trim$(line))
    StartsWithInlineData = (Left$(t, 5) = "data(")
End Function

Private Function IsEndformLine(ByVal line As String) As Boolean
    IsEndformLine = (UCase$(Trim$(line)) = "ENDFORM.")
End Function

Private Function CleanToken(ByVal token As String) As String
    Dim s As String
    s = Trim$(token)
    s = Replace(s, ".", "")
    s = Replace(s, ",", "")
    s = Replace(s, ";", "")
    CleanToken = s
End Function

Private Function ExtractParameterName(ByVal token As String) As String
    Dim s As String
    s = CleanToken(token)

    If UCase$(Left$(s, 6)) = "VALUE(" Then
        Dim inside As String
        inside = Mid$(s, 7)
        If Right$(inside, 1) = ")" Then inside = Left$(inside, Len(inside) - 1)
        ExtractParameterName = ExtractIdentifier(inside)
        Exit Function
    End If

    ExtractParameterName = ExtractIdentifier(s)
End Function

Private Function ExtractIdentifier(ByVal token As String) As String
    Dim s As String
    s = Trim$(token)
    If IsAbapIdentifier(s) Then
        ExtractIdentifier = s
    Else
        ExtractIdentifier = vbNullString
    End If
End Function

Private Function IsAbapIdentifier(ByVal s As String) As Boolean
    If Len(s) = 0 Then Exit Function

    Dim ch As String
    ch = Mid$(s, 1, 1)
    If Not (ch Like "[A-Za-z_]") Then Exit Function

    Dim i As Long
    For i = 2 To Len(s)
        ch = Mid$(s, i, 1)
        If Not (ch Like "[A-Za-z0-9_]") Then Exit Function
    Next i

    IsAbapIdentifier = True
End Function

Private Function IsIdentifierChar(ByVal ch As String) As Boolean
    If Len(ch) = 0 Then Exit Function
    IsIdentifierChar = (ch Like "[A-Za-z0-9_]")
End Function

Private Function ReadIdentifierFrom(ByVal text As String, ByVal startAt As Long) As String
    If startAt <= 0 Or startAt > Len(text) Then Exit Function

    Dim ch As String
    ch = Mid$(text, startAt, 1)
    If Not (ch Like "[A-Za-z_]") Then Exit Function

    Dim i As Long
    i = startAt
    Do While i <= Len(text)
        ch = Mid$(text, i, 1)
        If Not IsIdentifierChar(ch) Then Exit Do
        i = i + 1
    Loop

    ReadIdentifierFrom = Mid$(text, startAt, i - startAt)
End Function

Private Function FindWordOutsideQuotesFrom(ByVal text As String, ByVal word As String, ByVal startAt As Long) As Long
    Dim s As String
    s = text

    Dim w As String
    w = UCase$(Trim$(word))
    If Len(w) = 0 Then Exit Function

    If startAt < 1 Then startAt = 1
    If startAt > Len(s) Then Exit Function

    Dim inQuote As Boolean
    inQuote = False

    Dim i As Long
    i = startAt
    Do While i <= Len(s)
        Dim ch As String
        ch = Mid$(s, i, 1)

        If ch = "'" Then
            ' ABAP escape in string: '' (2 single quotes)
            If inQuote And i < Len(s) Then
                If Mid$(s, i + 1, 1) = "'" Then
                    i = i + 2
                    GoTo ContinueLoop
                End If
            End If

            inQuote = Not inQuote
            i = i + 1
            GoTo ContinueLoop
        End If

        If Not inQuote Then
            If i + Len(w) - 1 <= Len(s) Then
                If UCase$(Mid$(s, i, Len(w))) = w Then
                    Dim prevOk As Boolean
                    Dim nextOk As Boolean

                    If i = 1 Then
                        prevOk = True
                    Else
                        prevOk = Not IsIdentifierChar(Mid$(s, i - 1, 1))
                    End If

                    If i + Len(w) > Len(s) Then
                        nextOk = True
                    Else
                        nextOk = Not IsIdentifierChar(Mid$(s, i + Len(w), 1))
                    End If

                    If prevOk And nextOk Then
                        FindWordOutsideQuotesFrom = i
                        Exit Function
                    End If
                End If
            End If
        End If

        i = i + 1
ContinueLoop:
    Loop
End Function

Private Function ExtractFirstIdentifier(ByVal text As String) As String
    Dim tokens() As String
    tokens = Split(NormalizeSpaces(text), " ")
    If UBound(tokens) < 0 Then Exit Function

    Dim i As Long
    For i = LBound(tokens) To UBound(tokens)
        Dim id As String
        id = ExtractIdentifier(CleanToken(tokens(i)))
        If Len(id) > 0 Then
            ExtractFirstIdentifier = id
            Exit Function
        End If
    Next i
End Function

Private Function ExtractTypeAfterKeyword(ByVal text As String, ByVal keyword As String) As String
    Dim pos As Long
    pos = FindWordOutsideQuotesFrom(text, keyword, 1)
    If pos <= 0 Then Exit Function

    Dim idx As Long
    idx = pos + Len(keyword)
    Do While idx <= Len(text) And Mid$(text, idx, 1) = " "
        idx = idx + 1
    Loop

    ExtractTypeAfterKeyword = ReadIdentifierFrom(text, idx)
End Function

Private Function ExtractValueAfterKeyword(ByVal text As String, ByVal keyword As String) As String
    Dim pos As Long
    pos = FindWordOutsideQuotesFrom(text, keyword, 1)
    If pos <= 0 Then Exit Function

    Dim idx As Long
    idx = pos + Len(keyword)
    Do While idx <= Len(text) And Mid$(text, idx, 1) = " "
        idx = idx + 1
    Loop

    If idx <= Len(text) Then
        ExtractValueAfterKeyword = Trim$(Mid$(text, idx))
    End If
End Function

Private Function SplitByCommaOutsideQuotes(ByVal text As String) As Variant
    Dim parts As Collection
    Set parts = New Collection

    Dim buf As String
    buf = vbNullString

    Dim inQuote As Boolean
    inQuote = False

    Dim i As Long
    For i = 1 To Len(text)
        Dim ch As String
        ch = Mid$(text, i, 1)

        If ch = "'" Then
            ' ABAP escape trong string: '' (2 single quotes)
            If inQuote And i < Len(text) Then
                If Mid$(text, i + 1, 1) = "'" Then
                    buf = buf & "''"
                    i = i + 1
                    GoTo ContinueChar
                End If
            End If

            inQuote = Not inQuote
            buf = buf & ch
        ElseIf ch = "," And Not inQuote Then
            parts.Add buf
            buf = vbNullString
        Else
            buf = buf & ch
        End If

ContinueChar:
    Next i
    parts.Add buf

    Dim arr() As String
    ReDim arr(0 To parts.Count - 1) As String

    For i = 1 To parts.Count
        arr(i - 1) = CStr(parts(i))
    Next i

    SplitByCommaOutsideQuotes = arr
End Function

Private Function GetOrCreateSubroutine(ByVal subsDict As Object, ByVal orderKeys As Collection, ByVal key As String, ByVal displayName As String) As clsSubroutine
    If subsDict.Exists(key) Then
        Set GetOrCreateSubroutine = subsDict(key)
        Exit Function
    End If

    Dim subr As clsSubroutine
    Set subr = New clsSubroutine
    subr.Name = displayName

    subsDict.Add key, subr
    orderKeys.Add key

    Set GetOrCreateSubroutine = subr
End Function

Private Function EdgeKey(ByVal callerKey As String, ByVal calleeKey As String) As String
    EdgeKey = UCase$(callerKey) & "->" & UCase$(calleeKey)
End Function

Private Sub ResetProcessedFlags(ByVal subsDict As Object)
    Dim key As Variant
    For Each key In subsDict.Keys
        subsDict(key).IsProcessed = False
    Next key
End Sub

Private Sub MarkCycleNodes(ByVal stack As Collection, ByVal startKey As String, ByVal cycleNodes As Object)
    Dim startIndex As Long
    startIndex = FindInStack(stack, startKey)
    If startIndex <= 0 Then Exit Sub

    Dim i As Long
    For i = startIndex To stack.Count
        cycleNodes(CStr(stack(i))) = True
    Next i
End Sub

Private Function FindInStack(ByVal stack As Collection, ByVal key As String) As Long
    Dim i As Long
    For i = 1 To stack.Count
        If CStr(stack(i)) = key Then
            FindInStack = i
            Exit Function
        End If
    Next i
    FindInStack = 0
End Function
