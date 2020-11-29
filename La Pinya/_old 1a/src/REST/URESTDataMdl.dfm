object RESTDataMdl: TRESTDataMdl
  OldCreateOrder = False
  Height = 190
  Width = 306
  object RESTClient1: TRESTClient
    Params = <>
    HandleRedirects = True
    Left = 47
    Top = 22
  end
  object RESTResponse1: TRESTResponse
    Left = 215
    Top = 22
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    OnAfterExecute = RESTRequest1AfterExecute
    SynchronizedEvents = False
    Left = 127
    Top = 22
  end
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    FieldDefs = <>
    Response = RESTResponse1
    OnBeforeOpenDataSet = RESTResponseDataSetAdapter1BeforeOpenDataSet
    Left = 71
    Top = 110
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 200
    Top = 112
  end
end
