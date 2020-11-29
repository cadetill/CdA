object RESTMdl: TRESTMdl
  OldCreateOrder = False
  Height = 182
  Width = 311
  object RESTClient1: TRESTClient
    Params = <>
    Left = 47
    Top = 38
  end
  object RESTResponse1: TRESTResponse
    Left = 215
    Top = 38
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    OnAfterExecute = RESTRequest1AfterExecute
    SynchronizedEvents = False
    Left = 127
    Top = 38
  end
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    FieldDefs = <>
    Response = RESTResponse1
    OnBeforeOpenDataSet = RESTResponseDataSetAdapter1BeforeOpenDataSet
    Left = 127
    Top = 94
  end
end
