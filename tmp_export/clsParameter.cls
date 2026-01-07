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
Private mDescription As String

Public Property Get paramType() As String
    paramType = mParamType
End Property

Public Property Let paramType(ByVal value As String)
    mParamType = value
End Property

Public Property Get name() As String
    name = mName
End Property

Public Property Let name(ByVal value As String)
    mName = value
End Property

Public Property Get dataType() As String
    dataType = mDataType
End Property

Public Property Let dataType(ByVal value As String)
    mDataType = value
End Property

Public Property Get Description() As String
    Description = mDescription
End Property

Public Property Let Description(ByVal value As String)
    mDescription = value
End Property
