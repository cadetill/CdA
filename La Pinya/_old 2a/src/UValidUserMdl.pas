unit UValidUserMdl;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient,
  UClasses;

type
  TValidUserMdl = class(TDataModule)
    cdsResult: TClientDataSet;
  private
  public
    class function GetDeviceId(UUID, SO: string): string;
    class function GetValidationCode(id, mail, mobil, data_naix: string): string;
    class function GetUserRol(ActCode: string; var User: TUser): string;
  end;

var
  ValidUserMdl: TValidUserMdl;

implementation

uses
  System.JSON,
  URESTMdl, USets;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TConfigMdl }

class function TValidUserMdl.GetDeviceId(UUID, SO: string): string;
var
  Mdl: TValidUserMdl;
  Obj: TJSONObject;
begin
  Result := '-1';
  Mdl := TValidUserMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('AddDispositiu'));
      Obj.AddPair('uuid', TJSONString.Create(UUID));
      Obj.AddPair('so', TJSONString.Create(SO));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    if Mdl.cdsResult.Active then
      Result := Mdl.cdsResult.FieldByName('id').AsString;
  finally
    FreeAndNil(Mdl);
  end;
end;

class function TValidUserMdl.GetUserRol(ActCode: string; var User: TUser): string;
var
  Mdl: TValidUserMdl;
  Obj: TJSONObject;
begin
  Result := '-1';
  Mdl := TValidUserMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('GetRols'));
      Obj.AddPair('actcode', TJSONString.Create(ActCode));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    if Mdl.cdsResult.Active then
    begin
      User.Clear;
      User.ActCode := ActCode;
      while not Mdl.cdsResult.Eof do
      begin
        User.Id := Mdl.cdsResult.FieldByName('id_rol').AsInteger;
        case Mdl.cdsResult.FieldByName('id_rol').AsInteger of
          1: User.Rols := User.Rols + [rAdmin];
          2: User.Rols := User.Rols + [rJunta];
          3: User.Rols := User.Rols + [rTecnica];
          4: User.Rols := User.Rols + [rCasteller];
          5: User.Rols := User.Rols + [rSanitari];
          6: User.Rols := User.Rols + [rConvidat];
          7: User.Rols := User.Rols + [rComunicacio];
        end;
        Mdl.cdsResult.Next;
      end;
    end;
  finally
    FreeAndNil(Mdl);
  end;
end;

class function TValidUserMdl.GetValidationCode(id, mail, mobil,
  data_naix: string): string;
var
  Mdl: TValidUserMdl;
  Obj: TJSONObject;
begin
  Result := '';
  Mdl := TValidUserMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('GetUserCode'));
      Obj.AddPair('id', TJSONString.Create(id));
      Obj.AddPair('mail', TJSONString.Create(mail));
      Obj.AddPair('mobil', TJSONString.Create(mobil));
      Obj.AddPair('data_naix', TJSONString.Create(data_naix));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    if Mdl.cdsResult.Active and Assigned(Mdl.cdsResult.FindField('actcode')) then
      Result := Mdl.cdsResult.FieldByName('actcode').AsString;
  finally
    FreeAndNil(Mdl);
  end;
end;

end.
