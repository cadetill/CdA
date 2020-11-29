unit USocisMdl;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, FMX.ListBox,
  UClasses;

type
  TSocisMdl = class(TDataModule)
    cdsResult: TClientDataSet;
  private
  public
    function GetSocisName(ActCodes: string): Boolean;
    function GetEventsSoci(ActCodes, Any: string): Boolean;

    class function GetInfoEventFromSoci(ActCodes, IdEvent: string): TAssistencia;
    class function UpdateInfoEventFromSoci(Assist: TAssistencia): Boolean;

    class function DesvinculaSoci(ActCodes, Id: string): Boolean;
    class procedure AddUsersToCombobox(ActCodes: string; cbSoci: TComboBox);
  end;

var
  SocisMdl: TSocisMdl;

implementation

uses
  System.JSON,
  URESTMdl;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TSocisMdl }

class procedure TSocisMdl.AddUsersToCombobox(ActCodes: string;
  cbSoci: TComboBox);
var
  L: TStringList;
  Mdl: TSocisMdl;
begin
  cbSoci.Clear;

  L := nil;
  Mdl := nil;
  try
    L := TStringList.Create;
    Mdl := TSocisMdl.Create(nil);

    L.CommaText := ActCodes;
    Mdl.GetSocisName(L.CommaText);
    if Mdl.cdsResult.Active then
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          i: Integer;
          Soci: TSoci;
        begin
          for i := 0 to L.Count - 1 do
          begin
            if Mdl.cdsResult.Locate('actcode', L[i], []) then
            begin
              Soci := TSoci.Create;
              Soci.ActCode := Mdl.cdsResult.FieldByName('actcode').AsString;
              Soci.Nom := Mdl.cdsResult.FieldByName('nom').AsString;
              cbSoci.Items.AddObject(Mdl.cdsResult.FieldByName('nom').AsString, Soci)
            end;
          end;

          if cbSoci.Count > 0 then
            cbSoci.ItemIndex := 0;
        end);
  finally
    FreeAndNil(Mdl);
    FreeAndNil(L);
  end;
end;

class function TSocisMdl.DesvinculaSoci(ActCodes, Id: string): Boolean;
var
  Mdl: TSocisMdl;
  Obj: TJSONObject;
begin
  Mdl := TSocisMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('DesvinculaSoci'));
      Obj.AddPair('actcode', TJSONString.Create(ActCodes));
      Obj.AddPair('id', TJSONString.Create(Id));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    Result := False;
    if Mdl.cdsResult.Active then
      if Mdl.cdsResult.RecordCount > 0 then
        if SameStr(Mdl.cdsResult.FieldByName('status').AsString, 'ok') then
          Result := True;
  finally
    FreeAndNil(Mdl);
  end;
end;

function TSocisMdl.GetEventsSoci(ActCodes, Any: string): Boolean;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('GetAssistencia'));
    Obj.AddPair('actcode', TJSONString.Create(ActCodes));
    Obj.AddPair('any', TJSONString.Create(Any));

    TRESTMdl.GetRESTResponse('', cdsResult, Obj);
  finally
    FreeAndNil(Obj);
  end;

  Result := cdsResult.Active and (cdsResult.RecordCount > 0);
end;

class function TSocisMdl.GetInfoEventFromSoci(ActCodes, IdEvent: string): TAssistencia;
var
  Obj: TJSONObject;
  Mdl: TSocisMdl;
begin
  Mdl := nil;
  Obj := nil;
  try
    Mdl := TSocisMdl.Create(nil);
    Obj := TJSONObject.Create;

    Obj.AddPair('func', TJSONString.Create('GetInfoEvent'));
    Obj.AddPair('actcode', TJSONString.Create(ActCodes));
    Obj.AddPair('idevent', TJSONString.Create(IdEvent));

    TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    Result.Clear;
    if Mdl.cdsResult.RecordCount > 0 then
    begin
      Result.IdSoci := Mdl.cdsResult.FieldByName('id_soci').AsString;
      Result.IdEvent := Mdl.cdsResult.FieldByName('id_event').AsString;
      Result.Bus := Mdl.cdsResult.FieldByName('bus').AsString;
      Result.Assist := Mdl.cdsResult.FieldByName('assist').AsString;
    end;
  finally
    FreeAndNil(Obj);
    FreeAndNil(Mdl);
  end;
end;

function TSocisMdl.GetSocisName(ActCodes: string): Boolean;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('GetSociName'));
    Obj.AddPair('actcode', TJSONString.Create(ActCodes));

    TRESTMdl.GetRESTResponse('', cdsResult, Obj);
  finally
    FreeAndNil(Obj);
  end;

  Result := cdsResult.Active and (cdsResult.RecordCount > 0);
end;

class function TSocisMdl.UpdateInfoEventFromSoci(Assist: TAssistencia): Boolean;
var
  Mdl: TSocisMdl;
  Obj: TJSONObject;
begin
  Mdl := TSocisMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('UpdateInfoEventSoci'));
      Obj.AddPair('idsoci', TJSONString.Create(Assist.IdSoci));
      Obj.AddPair('idevent', TJSONString.Create(Assist.IdEvent));
      Obj.AddPair('bus', TJSONString.Create(Assist.Bus));
      Obj.AddPair('assist', TJSONString.Create(Assist.Assist));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    Result := False;
    if Mdl.cdsResult.Active then
      if Mdl.cdsResult.RecordCount > 0 then
        if SameStr(Mdl.cdsResult.FieldByName('status').AsString, 'ok') then
          Result := True;
  finally
    FreeAndNil(Mdl);
  end;
end;

end.
