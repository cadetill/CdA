object HomeMdl: THomeMdl
  OldCreateOrder = False
  Height = 150
  Width = 215
  object cdsResult: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 64
    Top = 56
  end
  object cdsNews: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 128
    Top = 56
    object cdsNewsid: TStringField
      FieldName = 'id'
      Size = 11
    end
    object cdsNewstitol: TStringField
      FieldName = 'titol'
      Size = 100
    end
    object cdsNewscontingut: TMemoField
      FieldName = 'contingut'
      BlobType = ftMemo
    end
    object cdsNewspublicar: TStringField
      FieldName = 'publicar'
      Size = 10
    end
    object cdsNewshora: TStringField
      FieldName = 'hora'
      Size = 8
    end
    object cdsNewsusera: TStringField
      FieldName = 'usera'
      Size = 11
    end
    object cdsNewsdatea: TStringField
      FieldName = 'datea'
      Size = 30
    end
  end
end
