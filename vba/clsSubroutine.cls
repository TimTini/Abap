VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsSubroutine"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mName As String
Private mKind As String
Private mDescription As String
Private mCallings As Collection
Private mCalledBy As Collection
Private mParameters As Collection
Private mLocalData As Collection
Private mLocalConstants As Collection
Private mIsProcessed As Boolean

Private Sub Class_Initialize()
    Set mCallings = New Collection
    Set mCalledBy = New Collection
    Set mParameters = New Collection
    Set mLocalData = New Collection
    Set mLocalConstants = New Collection
    mKind = "FORM"
    mIsProcessed = False
End Sub

Public Property Get name() As String
    name = mName
End Property

Public Property Let name(ByVal value As String)
    mName = value
End Property

Public Property Get kind() As String
    kind = mKind
End Property

Public Property Let kind(ByVal value As String)
    mKind = value
End Property

Public Property Get Description() As String
    Description = mDescription
End Property

Public Property Let Description(ByVal value As String)
    mDescription = value
End Property

Public Property Get Callings() As Collection
    Set Callings = mCallings
End Property

Public Property Get CalledBy() As Collection
    Set CalledBy = mCalledBy
End Property

Public Property Get Parameters() As Collection
    Set Parameters = mParameters
End Property

Public Property Get LocalData() As Collection
    Set LocalData = mLocalData
End Property

Public Property Get LocalConstants() As Collection
    Set LocalConstants = mLocalConstants
End Property

Public Property Get IsProcessed() As Boolean
    IsProcessed = mIsProcessed
End Property

Public Property Let IsProcessed(ByVal value As Boolean)
    mIsProcessed = value
End Property

Public Sub AddCalling(ByVal subroutineName As String)
    Dim key As String
    key = UCase$(Trim$(subroutineName))
    If Len(key) = 0 Then Exit Sub

    On Error Resume Next
    mCallings.Add subroutineName, key
    On Error GoTo 0
End Sub

Public Sub AddCalledBy(ByVal callerName As String)
    Dim key As String
    key = UCase$(Trim$(callerName))
    If Len(key) = 0 Then Exit Sub

    On Error Resume Next
    mCalledBy.Add callerName, key
    On Error GoTo 0
End Sub

Public Sub AddParameter(ByVal param As clsParameter)
    If param Is Nothing Then Exit Sub
    mParameters.Add param
End Sub

Public Sub AddLocalData(ByVal decl As clsDataDeclaration)
    If decl Is Nothing Then Exit Sub
    mLocalData.Add decl
End Sub

Public Sub AddLocalConstant(ByVal decl As clsDataDeclaration)
    If decl Is Nothing Then Exit Sub
    mLocalConstants.Add decl
End Sub
