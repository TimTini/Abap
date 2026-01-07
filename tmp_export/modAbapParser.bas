Attribute VB_Name = "modAbapParser"
Option Explicit

' -----------------------------
' Parsing
' -----------------------------

Public Function ParseAbapProgram(ByVal abapCode As String, ByRef isDefined As Object, ByRef orderKeys As Collection, ByRef globalData As Collection, ByRef globalConstants As Collection) As Object
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
    Dim rawLines As Variant
    rawLines = lines

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
            subr.kind = "FORM"
            isDefined(key) = True

            Dim paramDesc As Object
            Set paramDesc = CreateObject("Scripting.Dictionary") ' key = UCase(paramName), value = description
            subr.Description = ParseLeadingCommentDescription(rawLines, i, paramDesc)

            ParseFormHeader headerStmt, subr
            ApplyParameterDescriptions subr.Parameters, paramDesc

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
                evt.kind = "EVENT"
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
    If Right$(s, 1) = "." Then s = left$(s, Len(s) - 1)
    s = Trim$(s)
    If Len(s) = 0 Then Exit Function

    Dim u As String
    u = UCase$(s)

    If u = "LOAD-OF-PROGRAM" _
        Or u = "INITIALIZATION" _
        Or u = "START-OF-SELECTION" _
        Or u = "END-OF-SELECTION" _
        Or u = "TOP-OF-PAGE" _
        Or left$(u, Len("TOP-OF-PAGE ")) = "TOP-OF-PAGE " _
        Or u = "END-OF-PAGE" _
        Or u = "AT LINE-SELECTION" _
        Or u = "AT USER-COMMAND" _
        Or left$(u, Len("AT SELECTION-SCREEN")) = "AT SELECTION-SCREEN" Then
        eventName = u
        TryParseEventHeader = True
    End If
End Function

Private Function ExtractFormName(ByVal headerStmt As String) As String
    Dim stmt As String
    stmt = NormalizeSpaces(Replace(headerStmt, vbLf, " "))
    stmt = NormalizeSpaces(Replace(stmt, vbCr, " "))
    stmt = Trim$(stmt)
    If Right$(stmt, 1) = "." Then stmt = left$(stmt, Len(stmt) - 1)
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

    If Right$(stmt, 1) = "." Then stmt = left$(stmt, Len(stmt) - 1)
    stmt = Trim$(stmt)

    Dim formName As String
    formName = ExtractFormName(stmt)
    If Len(formName) > 0 Then subr.name = formName

    Dim afterName As String
    Dim posName As Long
    posName = InStr(1, stmt, formName, vbTextCompare)
    If posName > 0 Then
        afterName = Trim$(Mid$(stmt, posName + Len(formName)))
    Else
        afterName = vbNullString
    End If

    ParseParameterSections afterName, subr
End Sub

Private Sub ParseParameterSections(ByVal rest As String, ByVal subr As clsSubroutine)
    Dim s As String
    s = NormalizeSpaces(rest)
    If Len(s) = 0 Then Exit Sub

    Dim u As String
    u = UCase$(s)

    Dim posTables As Long, posUsing As Long, posChanging As Long, posRaising As Long
    posTables = FindWordOutsideQuotesFrom(u, "TABLES", 1)
    posUsing = FindWordOutsideQuotesFrom(u, "USING", 1)
    posChanging = FindWordOutsideQuotesFrom(u, "CHANGING", 1)
    posRaising = FindWordOutsideQuotesFrom(u, "RAISING", 1)

    Dim positions(1 To 4) As Long
    positions(1) = posTables
    positions(2) = posUsing
    positions(3) = posChanging
    positions(4) = posRaising

    Dim names(1 To 4) As String
    names(1) = "TABLES"
    names(2) = "USING"
    names(3) = "CHANGING"
    names(4) = "RAISING"

    Dim i As Long
    For i = 1 To 4
        If positions(i) > 0 Then
            Dim startPos As Long
            startPos = positions(i) + Len(names(i))

            Dim endPos As Long
            endPos = Len(s) + 1

            Dim j As Long
            For j = 1 To 4
                If j <> i Then
                    If positions(j) > 0 And positions(j) > positions(i) Then
                        If positions(j) < endPos Then endPos = positions(j)
                    End If
                End If
            Next j

            Dim seg As String
            Dim segStart As Long
            Dim segEnd As Long
            segStart = startPos + 1
            segEnd = endPos - 1

            seg = vbNullString
            If segStart <= Len(s) Then
                If segEnd >= segStart Then
                    seg = Trim$(Mid$(s, segStart, segEnd - segStart + 1))
                End If
            End If
            If Len(seg) > 0 Then
                ParseParameterSegment names(i), seg, subr
            End If
        End If
    Next i
End Sub

Private Sub ParseParameterSegment(ByVal paramType As String, ByVal segment As String, ByVal subr As clsSubroutine)
    Dim tokens() As String
    tokens = Split(NormalizeSpaces(segment), " ")

    If UBound(tokens) < 0 Then Exit Sub

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

            ' Stop words in parameter list
            If uTok = "OPTIONAL" Or uTok = "VALUE" Or uTok = "DEFAULT" Or uTok = "LIKE" Or uTok = "TYPE" Or uTok = "STRUCTURE" Then
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
                    p.paramType = paramType
                    p.name = paramName
                    p.dataType = dataType
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

    If Right$(s, 1) = "." Then s = left$(s, Len(s) - 1)
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
    If left$(u, Len(prefix)) <> prefix Then
        Set ParseDeclarationsFromStatement = result
        Exit Function
    End If

    s = Trim$(Mid$(s, Len(prefix) + 1))
    If left$(s, 1) = ":" Then s = Trim$(Mid$(s, 2))
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
        decl.dataType = dataType
        decl.DeclarationType = UCase$(prefix)
        decl.value = valueText

        result.Add decl

ContinuePart:
    Next i

    Set ParseDeclarationsFromStatement = result
End Function
