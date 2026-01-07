Attribute VB_Name = "modAbapModel"
Option Explicit

' -----------------------------
' In-memory cache of the last parsed ABAP model (for UI actions)
' -----------------------------

Public gSubsDict As Object
Public gIsDefined As Object
Public gOrderKeys As Collection
Public gGlobalData As Collection
Public gGlobalConstants As Collection
Public gCycleEdges As Object
Public gCycleNodes As Object
Public gDepths As Object

Public Sub CacheAbapModel(ByVal subsDict As Object, ByVal isDefined As Object, ByVal orderKeys As Collection, ByVal globalData As Collection, ByVal globalConstants As Collection, ByVal cycleEdges As Object, ByVal cycleNodes As Object, ByVal depths As Object)
    Set gSubsDict = subsDict
    Set gIsDefined = isDefined
    Set gOrderKeys = orderKeys
    Set gGlobalData = globalData
    Set gGlobalConstants = globalConstants
    Set gCycleEdges = cycleEdges
    Set gCycleNodes = cycleNodes
    Set gDepths = depths
End Sub

Public Sub ClearAbapModel()
    Set gSubsDict = Nothing
    Set gIsDefined = Nothing
    Set gOrderKeys = Nothing
    Set gGlobalData = Nothing
    Set gGlobalConstants = Nothing
    Set gCycleEdges = Nothing
    Set gCycleNodes = Nothing
    Set gDepths = Nothing
End Sub

Public Function HasAbapModel() As Boolean
    HasAbapModel = Not (gSubsDict Is Nothing)
End Function

