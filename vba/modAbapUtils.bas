Attribute VB_Name = "modAbapUtils"
Option Explicit

' -----------------------------
' Text utilities
' -----------------------------

Public Function NormalizeLineBreaks(ByVal text As String) As String
    Dim s As String
    s = Replace(text, vbCrLf, vbLf)
    s = Replace(s, vbCr, vbLf)
    NormalizeLineBreaks = s
End Function

Public Function NormalizeSpaces(ByVal text As String) As String
    Dim s As String
    s = Replace(text, vbTab, " ")
    s = Trim$(s)
    Do While InStr(1, s, "  ") > 0
        s = Replace(s, "  ", " ")
    Loop
    NormalizeSpaces = s
End Function

Public Function StripAbapComments(ByVal line As String) As String
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

Public Function CollectStatement(ByRef lines() As String, ByVal startIdx As Long, ByRef endIdx As Long) As String
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

Public Function FindEndFormIndex(ByRef lines() As String, ByVal startIdx As Long) As Long
    Dim i As Long
    For i = startIdx To UBound(lines)
        If IsEndformLine(lines(i)) Then
            FindEndFormIndex = i
            Exit Function
        End If
    Next i
    FindEndFormIndex = -1
End Function

Public Function StartsWithWord(ByVal text As String, ByVal word As String) As Boolean
    Dim t As String
    t = UCase$(Trim$(text))
    Dim w As String
    w = UCase$(Trim$(word))
    If Len(w) = 0 Then Exit Function
    If Left$(t, Len(w)) <> w Then Exit Function
    If Len(t) = Len(w) Then
        StartsWithWord = True
        Exit Function
    End If

    Dim nextCh As String
    nextCh = Mid$(t, Len(w) + 1, 1)
    StartsWithWord = (nextCh = " " Or nextCh = ":" Or nextCh = ".")
End Function

Public Function StartsWithInlineData(ByVal line As String) As Boolean
    Dim t As String
    t = LCase$(Trim$(line))
    StartsWithInlineData = (Left$(t, 5) = "data(")
End Function

' -----------------------------
' Token/identifier helpers
' -----------------------------

Public Function CleanToken(ByVal token As String) As String
    Dim s As String
    s = Trim$(token)
    s = Replace(s, ".", "")
    s = Replace(s, ",", "")
    s = Replace(s, ";", "")
    CleanToken = s
End Function

Public Function ExtractParameterName(ByVal token As String) As String
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

Public Function ExtractIdentifier(ByVal token As String) As String
    Dim s As String
    s = Trim$(token)
    If IsAbapIdentifier(s) Then
        ExtractIdentifier = s
    Else
        ExtractIdentifier = vbNullString
    End If
End Function

Public Function ReadIdentifierFrom(ByVal text As String, ByVal startAt As Long) As String
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

Public Function FindWordOutsideQuotesFrom(ByVal text As String, ByVal word As String, ByVal startAt As Long) As Long
    Dim t As String
    t = text
    Dim w As String
    w = UCase$(word)
    If Len(w) = 0 Then Exit Function
    If startAt <= 0 Then startAt = 1

    Dim inQuotes As Boolean
    inQuotes = False

    Dim beforeOk As Boolean
    Dim afterOk As Boolean

    Dim i As Long
    For i = startAt To Len(t) - Len(w) + 1
        Dim ch As String
        ch = Mid$(t, i, 1)

        If ch = "'" Then
            ' handle escaped '' inside string literal
            If inQuotes Then
                If i < Len(t) And Mid$(t, i + 1, 1) = "'" Then
                    i = i + 1
                Else
                    inQuotes = False
                End If
            Else
                inQuotes = True
            End If
        ElseIf Not inQuotes Then
            beforeOk = (i = 1)
            If Not beforeOk Then beforeOk = Not IsIdentifierChar(Mid$(t, i - 1, 1))

            If beforeOk Then
                If UCase$(Mid$(t, i, Len(w))) = w Then
                    afterOk = (i + Len(w) > Len(t))
                    If Not afterOk Then afterOk = Not IsIdentifierChar(Mid$(t, i + Len(w), 1))

                    If afterOk Then
                        FindWordOutsideQuotesFrom = i
                        Exit Function
                    End If
                End If
            End If
        End If
    Next i
End Function

Public Function ExtractFirstIdentifier(ByVal text As String) As String
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

Public Function ExtractTypeAfterKeyword(ByVal text As String, ByVal keyword As String) As String
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

Public Function ExtractValueAfterKeyword(ByVal text As String, ByVal keyword As String) As String
    Dim pos As Long
    pos = FindWordOutsideQuotesFrom(text, keyword, 1)
    If pos <= 0 Then Exit Function

    Dim idx As Long
    idx = pos + Len(keyword)
    Do While idx <= Len(text) And Mid$(text, idx, 1) = " "
        idx = idx + 1
    Loop

    Dim out As String
    out = Mid$(text, idx)
    out = Trim$(out)
    If Len(out) = 0 Then Exit Function

    ExtractValueAfterKeyword = out
End Function

Public Function SplitByCommaOutsideQuotes(ByVal text As String) As Variant
    Dim parts As Collection
    Set parts = New Collection

    Dim s As String
    s = text

    Dim startPos As Long
    startPos = 1

    Dim inQuotes As Boolean
    inQuotes = False

    Dim i As Long
    For i = 1 To Len(s)
        Dim ch As String
        ch = Mid$(s, i, 1)

        If ch = "'" Then
            If inQuotes Then
                If i < Len(s) And Mid$(s, i + 1, 1) = "'" Then
                    i = i + 1
                Else
                    inQuotes = False
                End If
            Else
                inQuotes = True
            End If
        ElseIf ch = "," And Not inQuotes Then
            parts.Add Mid$(s, startPos, i - startPos)
            startPos = i + 1
        End If
    Next i

    parts.Add Mid$(s, startPos)

    Dim arr() As String
    ReDim arr(0 To parts.Count - 1)

    Dim idx As Long
    For idx = 1 To parts.Count
        arr(idx - 1) = CStr(parts(idx))
    Next idx

    SplitByCommaOutsideQuotes = arr
End Function

' -----------------------------
' Subroutine factory
' -----------------------------

Public Function GetOrCreateSubroutine(ByVal subsDict As Object, ByVal orderKeys As Collection, ByVal key As String, ByVal displayName As String) As clsSubroutine
    If subsDict.Exists(key) Then
        Set GetOrCreateSubroutine = subsDict(key)
        Exit Function
    End If

    Dim subr As clsSubroutine
    Set subr = New clsSubroutine
    subr.name = displayName

    ' Store object reference in Scripting.Dictionary: use Add/Set to avoid default-property coercion (Err 438)
    subsDict.Add key, subr
    orderKeys.Add key

    Set GetOrCreateSubroutine = subr
End Function

' -----------------------------
' Private helpers
' -----------------------------

Private Function IsEndformLine(ByVal line As String) As Boolean
    IsEndformLine = (UCase$(Trim$(line)) = "ENDFORM.")
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
