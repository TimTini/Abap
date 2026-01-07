Attribute VB_Name = "modAbapDiagram"
Option Explicit

' -----------------------------
' Rendering (Sheet2)
' -----------------------------

Public Sub DrawFlowDiagram(ByVal ws As Worksheet, ByVal subsDict As Object, ByVal depths As Object, ByVal cycleEdges As Object, ByVal cycleNodes As Object, ByVal isDefined As Object, ByVal orderKeys As Collection)
    ' Some Shape/Connector operations require the target sheet to be active,
    ' otherwise Excel may throw: "Expecting object to be local" (Err 1004).
    On Error Resume Next
    ws.Activate
    On Error GoTo 0

    ClearSheet ws

    Dim key As Variant

    Dim shapesByKey As Object
    Set shapesByKey = CreateObject("Scripting.Dictionary") ' key = UCase(name), value = Shape

    Const START_X As Single = 20
    Const START_Y As Single = 20
    Const BOX_W As Single = 220
    Const BOX_H As Single = 70
    Const H_GAP As Single = 90
    Const V_GAP As Single = 20

    Dim depthRowCount As Object
    Set depthRowCount = CreateObject("Scripting.Dictionary") ' key = depth, value = count

    Dim idx As Long
    For idx = 1 To orderKeys.Count
        Dim nodeKey As String
        nodeKey = CStr(orderKeys(idx))

        Dim subr As clsSubroutine
        Set subr = subsDict(nodeKey)

        Dim depth As Long
        depth = 0
        If depths.Exists(nodeKey) Then depth = CLng(depths(nodeKey))

        Dim rowIdx As Long
        If depthRowCount.Exists(CStr(depth)) Then
            rowIdx = CLng(depthRowCount(CStr(depth))) + 1
        Else
            rowIdx = 1
        End If
        depthRowCount(CStr(depth)) = rowIdx

        Dim left As Single
        Dim top As Single
        left = START_X + depth * (BOX_W + H_GAP)
        top = START_Y + (rowIdx - 1) * (BOX_H + V_GAP)

        Dim defined As Boolean
        defined = isDefined.Exists(nodeKey)

        Dim inCycle As Boolean
        inCycle = cycleNodes.Exists(nodeKey)

        Dim sh As Shape
        Set sh = AddSubroutineShape(ws, subr, left, top, BOX_W, BOX_H, defined, inCycle)
        Set shapesByKey(nodeKey) = sh
    Next idx

    Dim callerKey As Variant
    For Each callerKey In subsDict.Keys
        If Not shapesByKey.Exists(CStr(callerKey)) Then GoTo ContinueCaller

        Dim caller As clsSubroutine
        Set caller = subsDict(callerKey)

        Dim callerShape As Shape
        Set callerShape = shapesByKey(CStr(callerKey))

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
            conn.line.EndArrowheadStyle = msoArrowheadTriangle
            conn.ZOrder msoSendToBack
            conn.Placement = xlFreeFloating

            If cycleEdges.Exists(EdgeKey(CStr(callerKey), calleeKey)) Then
                conn.line.ForeColor.RGB = RGB(192, 0, 0)
                conn.line.Weight = 2
                conn.line.DashStyle = msoLineDash
            Else
                conn.line.ForeColor.RGB = RGB(90, 90, 90)
                conn.line.Weight = 1.25
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
    kind = UCase$(Trim$(subr.kind))
    If Len(kind) = 0 Then kind = "FORM"

    If kind = "EVENT" Then
        title = subr.name
    Else
        title = "FORM " & subr.name
    End If

    Dim meta As String
    meta = "Params: " & CStr(subr.Parameters.Count) & " | DATA: " & CStr(subr.LocalData.Count)

    Dim meta2 As String
    meta2 = "CONST: " & CStr(subr.LocalConstants.Count)

    sh.TextFrame.Characters.text = title & vbCrLf & meta & vbCrLf & meta2
    sh.TextFrame.HorizontalAlignment = xlHAlignCenter
    sh.TextFrame.VerticalAlignment = xlVAlignCenter
    sh.TextFrame.MarginLeft = 6
    sh.TextFrame.MarginRight = 6
    sh.TextFrame.MarginTop = 6
    sh.TextFrame.MarginBottom = 6
    sh.TextFrame.Characters.Font.Color = RGB(0, 0, 0)

    sh.line.ForeColor.RGB = RGB(60, 60, 60)
    sh.line.Weight = 1

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
