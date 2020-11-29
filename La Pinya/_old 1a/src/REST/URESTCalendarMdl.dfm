object RESTCalendarMdl: TRESTCalendarMdl
  OldCreateOrder = False
  Height = 217
  Width = 271
  object cdsCalendars: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 40
    Top = 56
  end
  object cdsCalendar: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 56
  end
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    Dataset = cdsItems
    FieldDefs = <>
    Response = RESTResponse1
    Left = 71
    Top = 126
  end
  object cdsItems: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 192
    Top = 56
  end
  object RESTResponse1: TRESTResponse
    Left = 200
    Top = 128
  end
end
