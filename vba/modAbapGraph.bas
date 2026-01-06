Attribute VB_Name = "modAbapGraph"
Option Explicit

' -----------------------------
' Graph index + cycle detection + hierarchy
' -----------------------------

Public Sub BuildCalledByIndex(ByVal subsDict As Object)
    Dim callerKey As Variant
    For Each callerKey In subsDict.Keys
        Dim caller As clsSubroutine
        Set caller = subsDict(callerKey)

        Dim callee As Variant
        For Each callee In caller.Callings
            Dim calleeKey As String
            calleeKey = UCase$(CStr(callee))
            If subsDict.Exists(calleeKey) Then
                subsDict(calleeKey).AddCalledBy caller.name
            End If
        Next callee
    Next callerKey
End Sub

Public Function FindCycleEdges(ByVal subsDict As Object, ByRef cycleNodes As Object) As Object
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

Public Function ComputeHierarchyDepths(ByVal subsDict As Object, ByVal cycleEdges As Object) As Object
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

Public Function EdgeKey(ByVal callerKey As String, ByVal calleeKey As String) As String
    EdgeKey = UCase$(callerKey) & "->" & UCase$(calleeKey)
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

Private Sub ResetProcessedFlags(ByVal subsDict As Object)
    Dim key As Variant
    For Each key In subsDict.Keys
        subsDict(key).IsProcessed = False
    Next key
End Sub

Private Sub MarkCycleNodes(ByVal stack As Collection, ByVal startKey As String, ByVal cycleNodes As Object)
    Dim pos As Long
    pos = FindInStack(stack, startKey)
    If pos <= 0 Then Exit Sub

    Dim i As Long
    For i = pos To stack.Count
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
End Function

