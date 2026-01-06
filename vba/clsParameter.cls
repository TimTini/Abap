VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsParameter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mParamType As String
Private mName As String
Private mDataType As String

Public Property Get ParamType() As String
    ParamType = mParamType
End Property

Public Property Let ParamType(ByVal value As String)
    mParamType = value
End Property

Public Property Get Name() As String
    Name = mName
End Property

Public Property Let Name(ByVal value As String)
    mName = value
End Property

Public Property Get DataType() As String
    DataType = mDataType
End Property

Public Property Let DataType(ByVal value As String)
    mDataType = value
End Property
