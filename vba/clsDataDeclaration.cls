VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsDataDeclaration"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mVariableName As String
Private mDataType As String
Private mDeclarationType As String
Private mValue As String

Public Property Get VariableName() As String
    VariableName = mVariableName
End Property

Public Property Let VariableName(ByVal value As String)
    mVariableName = value
End Property

Public Property Get DataType() As String
    DataType = mDataType
End Property

Public Property Let DataType(ByVal value As String)
    mDataType = value
End Property

Public Property Get DeclarationType() As String
    DeclarationType = mDeclarationType
End Property

Public Property Let DeclarationType(ByVal value As String)
    mDeclarationType = value
End Property

Public Property Get Value() As String
    Value = mValue
End Property

Public Property Let Value(ByVal value As String)
    mValue = value
End Property

