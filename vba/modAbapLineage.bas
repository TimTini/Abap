Attribute VB_Name = "modAbapLineage"
Option Explicit

' -----------------------------
' Data lineage (origin + writers)
' -----------------------------

Public Function BuildGlobalDeclarationIndex(ByVal globalData As Collection, ByVal globalConstants As Collection) As Object
    Dim dict As Object
    Set dict = CreateObject("Scripting.Dictionary") ' key = UCase(varName), value = clsDataDeclaration

    Dim decl As clsDataDeclaration

    If Not globalData Is Nothing Then
        For Each decl In globalData
            If Len(Trim$(decl.VariableName)) > 0 Then dict(UCase$(decl.VariableName)) = decl
        Next decl
    End If

    If Not globalConstants Is Nothing Then
        For Each decl In globalConstants
            If Len(Trim$(decl.VariableName)) > 0 Then dict(UCase$(decl.VariableName)) = decl
        Next decl
    End If

    Set BuildGlobalDeclarationIndex = dict
End Function

Public Function BuildWritersIndex(ByVal subsDict As Object) As Object
    Dim writers As Object
    Set writers = CreateObject("Scripting.Dictionary") ' key = UCase(varName), value = Collection(subKey)

    If subsDict Is Nothing Then
        Set BuildWritersIndex = writers
        Exit Function
    End If

    Dim subKey As Variant
    For Each subKey In subsDict.Keys
        Dim subr As clsSubroutine
        Set subr = subsDict(CStr(subKey))

        Dim v As Variant
        For Each v In subr.Writes
            Dim varKey As String
            varKey = UCase$(Trim$(CStr(v)))
            If Len(varKey) = 0 Then GoTo ContinueVar

            Dim col As Collection
            If writers.Exists(varKey) Then
                Set col = writers(varKey)
            Else
                Set col = New Collection
                writers(varKey) = col
            End If

            AddUniqueString col, CStr(subKey)

ContinueVar:
        Next v
    Next subKey

    Set BuildWritersIndex = writers
End Function

Public Function TraceVariableOrigins(ByVal subsDict As Object, ByVal globalIndex As Object, ByVal startSubKey As String, ByVal varName As String) As Collection
    Dim results As Collection
    Set results = New Collection

    If subsDict Is Nothing Then
        Set TraceVariableOrigins = results
        Exit Function
    End If

    Dim sKey As String
    sKey = UCase$(Trim$(startSubKey))
    If Len(sKey) = 0 Then
        Set TraceVariableOrigins = results
        Exit Function
    End If

    Dim vKey As String
    vKey = UCase$(Trim$(varName))
    If Len(vKey) = 0 Then
        Set TraceVariableOrigins = results
        Exit Function
    End If

    Dim visited As Object
    Set visited = CreateObject("Scripting.Dictionary") ' key = "SUB|VAR"

    Dim path As String
    path = sKey & "." & vKey

    TraceVarRecursive subsDict, globalIndex, sKey, vKey, path, visited, results

    Set TraceVariableOrigins = results
End Function

Private Sub TraceVarRecursive(ByVal subsDict As Object, ByVal globalIndex As Object, ByVal curSubKey As String, ByVal varKey As String, ByVal path As String, ByVal visited As Object, ByVal results As Collection)
    Dim visitKey As String
    visitKey = curSubKey & "|" & varKey
    If visited.Exists(visitKey) Then Exit Sub
    visited(visitKey) = True

    If Not subsDict.Exists(curSubKey) Then Exit Sub

    Dim subr As clsSubroutine
    Set subr = subsDict(curSubKey)

    Dim decl As clsDataDeclaration
    If TryFindDeclInCollection(subr.LocalData, varKey, decl) Then
        results.Add BuildOriginResult("Local DATA", curSubKey, decl.VariableName, decl.dataType, decl.Description, path)
        Exit Sub
    End If

    If TryFindDeclInCollection(subr.LocalConstants, varKey, decl) Then
        results.Add BuildOriginResult("Local CONSTANTS", curSubKey, decl.VariableName, decl.dataType, decl.Description, path)
        Exit Sub
    End If

    If Not globalIndex Is Nothing Then
        If globalIndex.Exists(varKey) Then
            Set decl = globalIndex(varKey)
            results.Add BuildOriginResult("Global " & decl.DeclarationType, "GLOBAL", decl.VariableName, decl.dataType, decl.Description, path)
            Exit Sub
        End If
    End If

    Dim param As clsParameter
    Dim paramType As String
    Dim paramIndex As Long
    If TryFindParameterInfo(subr, varKey, param, paramType, paramIndex) Then
        Dim mapped As Boolean
        mapped = False

        Dim caller As Variant
        For Each caller In subr.CalledBy
            Dim callerKey As String
            callerKey = UCase$(Trim$(CStr(caller)))
            If Len(callerKey) = 0 Then GoTo ContinueCaller
            If Not subsDict.Exists(callerKey) Then GoTo ContinueCaller

            Dim callerSub As clsSubroutine
            Set callerSub = subsDict(callerKey)

            Dim edge As clsCallEdge
            For Each edge In callerSub.CallEdges
                If UCase$(Trim$(edge.calleeKey)) = curSubKey Then
                    Dim argName As String
                    argName = GetEdgeArgument(edge, paramType, paramIndex)
                    If Len(argName) > 0 Then
                        mapped = True
                        Dim nextPath As String
                        nextPath = path & " <= " & callerKey & "." & UCase$(argName)
                        TraceVarRecursive subsDict, globalIndex, callerKey, UCase$(argName), nextPath, visited, results
                    End If
                End If
            Next edge

ContinueCaller:
        Next caller

        If Not mapped Then
            results.Add BuildOriginResult("Parameter " & paramType, curSubKey, param.name, param.dataType, param.Description, path)
        End If
    End If
End Sub

Private Function GetEdgeArgument(ByVal edge As clsCallEdge, ByVal paramType As String, ByVal paramIndex As Long) As String
    If edge Is Nothing Then Exit Function
    If paramIndex <= 0 Then Exit Function

    Dim t As String
    t = UCase$(Trim$(paramType))

    Dim col As Collection
    Select Case t
        Case "TABLES"
            Set col = edge.tablesArgs
        Case "USING"
            Set col = edge.usingArgs
        Case "CHANGING"
            Set col = edge.changingArgs
        Case Else
            Exit Function
    End Select

    If col Is Nothing Then Exit Function
    If paramIndex > col.Count Then Exit Function
    GetEdgeArgument = CStr(col(paramIndex))
End Function

Private Function TryFindDeclInCollection(ByVal decls As Collection, ByVal varKey As String, ByRef decl As clsDataDeclaration) As Boolean
    If decls Is Nothing Then Exit Function

    Dim d As clsDataDeclaration
    For Each d In decls
        If UCase$(Trim$(d.VariableName)) = varKey Then
            Set decl = d
            TryFindDeclInCollection = True
            Exit Function
        End If
    Next d
End Function

Private Function TryFindParameterInfo(ByVal subr As clsSubroutine, ByVal varKey As String, ByRef param As clsParameter, ByRef paramType As String, ByRef paramIndex As Long) As Boolean
    If subr Is Nothing Then Exit Function
    If subr.Parameters Is Nothing Then Exit Function

    Dim idxTables As Long, idxUsing As Long, idxChanging As Long
    idxTables = 0
    idxUsing = 0
    idxChanging = 0

    Dim p As clsParameter
    For Each p In subr.Parameters
        Dim t As String
        t = UCase$(Trim$(p.paramType))

        Select Case t
            Case "TABLES"
                idxTables = idxTables + 1
            Case "USING"
                idxUsing = idxUsing + 1
            Case "CHANGING"
                idxChanging = idxChanging + 1
        End Select

        If UCase$(Trim$(p.name)) = varKey Then
            Set param = p
            paramType = t
            Select Case t
                Case "TABLES"
                    paramIndex = idxTables
                Case "USING"
                    paramIndex = idxUsing
                Case "CHANGING"
                    paramIndex = idxChanging
            End Select
            TryFindParameterInfo = True
            Exit Function
        End If
    Next p
End Function

Private Function BuildOriginResult(ByVal originKind As String, ByVal originSubKey As String, ByVal originVar As String, ByVal dataType As String, ByVal description As String, ByVal path As String) As Object
    Dim d As Object
    Set d = CreateObject("Scripting.Dictionary")
    d("OriginKind") = originKind
    d("OriginSubKey") = originSubKey
    d("OriginVar") = originVar
    d("DataType") = dataType
    d("Description") = description
    d("Path") = path
    Set BuildOriginResult = d
End Function

Private Sub AddUniqueString(ByVal col As Collection, ByVal value As String)
    If col Is Nothing Then Exit Sub
    Dim key As String
    key = UCase$(Trim$(value))
    If Len(key) = 0 Then Exit Sub
    On Error Resume Next
    col.Add value, key
    On Error GoTo 0
End Sub

