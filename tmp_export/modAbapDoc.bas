Attribute VB_Name = "modAbapDoc"
Option Explicit

' -----------------------------
' Comment/Description parsing
' -----------------------------

Public Function ParseLeadingCommentDescription(ByRef rawLines As Variant, ByVal startLineIdx As Long, ByVal paramDesc As Object) As String
    Dim commentLines As Collection
    Set commentLines = CollectLeadingCommentLines(rawLines, startLineIdx)

    Dim descLines As Collection
    Set descLines = New Collection

    If paramDesc Is Nothing Then Set paramDesc = CreateObject("Scripting.Dictionary")

    Dim v As Variant
    For Each v In commentLines
        Dim t As String
        t = CleanFullLineCommentText(CStr(v))
        If Len(t) = 0 Then GoTo ContinueLine

        If IsParamDescriptionLine(t) Then
            ParseParamDescriptionLine t, paramDesc
        ElseIf Not IsSeparatorCommentLine(t) Then
            descLines.Add t
        End If

ContinueLine:
    Next v

    ParseLeadingCommentDescription = JoinVariantCollection(descLines, vbLf)
End Function

Public Sub ApplyParameterDescriptions(ByVal params As Collection, ByVal paramDesc As Object)
    If params Is Nothing Then Exit Sub
    If paramDesc Is Nothing Then Exit Sub

    Dim p As clsParameter
    For Each p In params
        Dim key As String
        key = UCase$(Trim$(p.name))
        If Len(key) = 0 Then GoTo ContinueParam

        If paramDesc.Exists(key) Then
            p.Description = CStr(paramDesc(key))
        End If

ContinueParam:
    Next p
End Sub

Private Function CollectLeadingCommentLines(ByRef rawLines As Variant, ByVal startLineIdx As Long) As Collection
    Dim temp As Collection
    Set temp = New Collection

    Dim started As Boolean
    started = False

    Dim idx As Long
    idx = startLineIdx - 1

    Do While idx >= LBound(rawLines)
        Dim line As String
        line = CStr(rawLines(idx))

        If Len(Trim$(line)) = 0 Then
            If started Then Exit Do
        ElseIf IsFullLineComment(line) Then
            started = True
            temp.Add line
        Else
            Exit Do
        End If

        idx = idx - 1
    Loop

    Dim result As Collection
    Set result = New Collection

    Dim i As Long
    For i = temp.Count To 1 Step -1
        result.Add temp(i)
    Next i

    Set CollectLeadingCommentLines = result
End Function

Private Function IsFullLineComment(ByVal line As String) As Boolean
    Dim t As String
    t = LTrim$(line)
    If Len(t) = 0 Then Exit Function
    IsFullLineComment = (left$(t, 1) = "*" Or left$(t, 1) = """")
End Function

Private Function CleanFullLineCommentText(ByVal line As String) As String
    Dim t As String
    t = LTrim$(line)
    If Len(t) = 0 Then Exit Function

    If left$(t, 1) = "*" Or left$(t, 1) = """" Then t = Mid$(t, 2)
    t = LTrim$(t)
    If left$(t, 1) = "&" Then t = Mid$(t, 2)
    t = Trim$(t)

    If Len(t) > 0 Then
        If Right$(t, 1) = "*" Then t = RTrim$(left$(t, Len(t) - 1))
    End If

    CleanFullLineCommentText = NormalizeSpaces(t)
End Function

Private Function IsParamDescriptionLine(ByVal text As String) As Boolean
    Dim t As String
    t = LTrim$(text)
    IsParamDescriptionLine = (left$(t, 3) = "-->" Or left$(t, 3) = "<--")
End Function

Private Sub ParseParamDescriptionLine(ByVal text As String, ByVal paramDesc As Object)
    If paramDesc Is Nothing Then Exit Sub

    Dim t As String
    t = Trim$(text)
    If Len(t) < 3 Then Exit Sub

    If left$(t, 3) = "-->" Or left$(t, 3) = "<--" Then
        t = Trim$(Mid$(t, 4))
    End If
    If Len(t) = 0 Then Exit Sub

    Dim paramName As String
    paramName = ExtractFirstIdentifier(t)
    If Len(paramName) = 0 Then Exit Sub

    Dim posName As Long
    posName = InStr(1, t, paramName, vbTextCompare)

    Dim desc As String
    desc = Trim$(Mid$(t, posName + Len(paramName)))
    If Len(desc) = 0 Then Exit Sub

    paramDesc(UCase$(paramName)) = desc
End Sub

Private Function IsSeparatorCommentLine(ByVal text As String) As Boolean
    Dim s As String
    s = text
    s = Replace(s, " ", vbNullString)
    s = Replace(s, vbTab, vbNullString)
    s = Replace(s, "-", vbNullString)
    s = Replace(s, "*", vbNullString)
    s = Replace(s, "&", vbNullString)
    s = Replace(s, "|", vbNullString)
    s = Replace(s, "=", vbNullString)
    s = Replace(s, "_", vbNullString)
    s = Replace(s, ".", vbNullString)
    s = Replace(s, "(", vbNullString)
    s = Replace(s, ")", vbNullString)
    IsSeparatorCommentLine = (Len(s) = 0)
End Function

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

