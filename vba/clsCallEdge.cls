VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsCallEdge"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mCalleeKey As String
Private mCalleeName As String
Private mTablesArgs As Collection
Private mUsingArgs As Collection
Private mChangingArgs As Collection
Private mRaw As String

Private Sub Class_Initialize()
    Set mTablesArgs = New Collection
    Set mUsingArgs = New Collection
    Set mChangingArgs = New Collection
End Sub

Public Property Get calleeKey() As String
    calleeKey = mCalleeKey
End Property

Public Property Let calleeKey(ByVal value As String)
    mCalleeKey = value
End Property

Public Property Get calleeName() As String
    calleeName = mCalleeName
End Property

Public Property Let calleeName(ByVal value As String)
    mCalleeName = value
End Property

Public Property Get tablesArgs() As Collection
    Set tablesArgs = mTablesArgs
End Property

Public Property Get usingArgs() As Collection
    Set usingArgs = mUsingArgs
End Property

Public Property Get changingArgs() As Collection
    Set changingArgs = mChangingArgs
End Property

Public Property Get rawText() As String
    rawText = mRaw
End Property

Public Property Let rawText(ByVal value As String)
    mRaw = value
End Property

Public Sub AddTablesArg(ByVal value As String)
    AddUnique mTablesArgs, value
End Sub

Public Sub AddUsingArg(ByVal value As String)
    AddUnique mUsingArgs, value
End Sub

Public Sub AddChangingArg(ByVal value As String)
    AddUnique mChangingArgs, value
End Sub

Private Sub AddUnique(ByVal col As Collection, ByVal value As String)
    If col Is Nothing Then Exit Sub
    Dim key As String
    key = UCase$(Trim$(value))
    If Len(key) = 0 Then Exit Sub

    On Error Resume Next
    col.Add value, key
    On Error GoTo 0
End Sub

