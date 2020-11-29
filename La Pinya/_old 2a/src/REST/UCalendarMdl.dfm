object CalendarMdl: TCalendarMdl
  OldCreateOrder = False
  Height = 238
  Width = 315
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    Dataset = cdsItems
    FieldDefs = <>
    Response = RESTResponse1
    Left = 71
    Top = 30
  end
  object RESTResponse1: TRESTResponse
    Left = 207
    Top = 30
  end
  object cdsItems: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 71
    Top = 112
  end
  object cdsTemporades: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 143
    Top = 112
  end
  object cdsTmpEvents: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'Idx'
        Fields = 'datai;id_google'
      end>
    Params = <>
    StoreDefs = True
    Left = 71
    Top = 160
    object cdsTmpEventsid_cal: TIntegerField
      FieldName = 'id_cal'
    end
    object cdsTmpEventsid_google: TStringField
      FieldName = 'id_google'
      Size = 100
    end
    object cdsTmpEventsstatus: TStringField
      FieldName = 'status'
      Size = 30
    end
    object cdsTmpEventssummary: TStringField
      FieldName = 'summary'
      Size = 100
    end
    object cdsTmpEventsany: TIntegerField
      FieldName = 'any'
    end
    object cdsTmpEventsdatai: TStringField
      FieldName = 'datai'
    end
    object cdsTmpEventsdataf: TStringField
      FieldName = 'dataf'
    end
    object cdsTmpEventshorai: TStringField
      FieldName = 'horai'
      LookupDataSet = cdsEvents
    end
    object cdsTmpEventshoraf: TStringField
      FieldName = 'horaf'
    end
    object cdsTmpEventslocation: TStringField
      FieldName = 'location'
      Size = 200
    end
  end
  object cdsEvents: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 143
    Top = 160
  end
  object cdsResult: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 207
    Top = 160
  end
end
