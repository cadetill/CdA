inherited InsResultMdl: TInsResultMdl
  OldCreateOrder = True
  inherited cdsResult: TClientDataSet
    object cdsResultstatus: TStringField
      FieldName = 'status'
      Size = 2
    end
    object cdsResultid: TStringField
      FieldName = 'id'
      Size = 15
    end
  end
end
