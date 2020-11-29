unit UHomeMdl;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient;

type
  THomeMdl = class(TDataModule)
    cdsResult: TClientDataSet;
    cdsNews: TClientDataSet;
    cdsNewsid: TStringField;
    cdsNewstitol: TStringField;
    cdsNewscontingut: TMemoField;
    cdsNewspublicar: TStringField;
    cdsNewshora: TStringField;
    cdsNewsusera: TStringField;
    cdsNewsdatea: TStringField;
  private
  public
    function GetNews(Start, Count: Integer; IsComunica: Boolean): Boolean;

    class function DelNews(Id: string): Boolean;
    class function AddNews(Title, Content, PubDate, PubTime, UserId: string): string;
    class function EditNews(Id, Title, Content, PubDate, PubTime, UserId: string): Boolean;
  end;

var
  HomeMdl: THomeMdl;

implementation

uses
  System.JSON,
  URESTMdl, UClasses;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ THomeMdl }

class function THomeMdl.AddNews(Title, Content, PubDate, PubTime,
  UserId: string): string;
var
  Mdl: THomeMdl;
  Obj: TJSONObject;
begin
  Result := '';

  Mdl := THomeMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('AddNews'));
      Obj.AddPair('title', TJSONString.Create(TGenFunc.StringToURL(Title)));
      Obj.AddPair('body', TJSONString.Create(TGenFunc.StringToURL(Content)));
      Obj.AddPair('pubdate', TJSONString.Create(PubDate));
      Obj.AddPair('hora', TJSONString.Create(PubTime));
      Obj.AddPair('user', TJSONString.Create(UserId));
      Obj.AddPair('json', TJSONString.Create('X'));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    if Mdl.cdsResult.Active then
    begin
      if SameStr(Mdl.cdsResult.FieldByName('status').AsString, 'ok') then
        Result := Mdl.cdsResult.FieldByName('id').AsString;
    end;
  finally
    FreeAndNil(Mdl);
  end;
end;

class function THomeMdl.DelNews(Id: string): Boolean;
var
  Mdl: THomeMdl;
  Obj: TJSONObject;
begin
  Mdl := THomeMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('DelNews'));
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

class function THomeMdl.EditNews(Id, Title, Content, PubDate, PubTime,
  UserId: string): Boolean;
var
  Mdl: THomeMdl;
  Obj: TJSONObject;
begin
  Result := False;
  Mdl := THomeMdl.Create(nil);
  try
    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('EditNews'));
      Obj.AddPair('id', TJSONString.Create(Id));
      Obj.AddPair('title', TJSONString.Create(TGenFunc.StringToURL(Title)));
      Obj.AddPair('body', TJSONString.Create(TGenFunc.StringToURL(Content)));
      Obj.AddPair('pubdate', TJSONString.Create(PubDate));
      Obj.AddPair('hora', TJSONString.Create(PubTime));
      Obj.AddPair('user', TJSONString.Create(UserId));

      TRESTMdl.GetRESTResponse('', Mdl.cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    if Mdl.cdsResult.Active then
    begin
      Result := SameStr(Mdl.cdsResult.FieldByName('status').AsString, 'ok');
    end;
  finally
    FreeAndNil(Mdl);
  end;
end;

function THomeMdl.GetNews(Start, Count: Integer; IsComunica: Boolean): Boolean;
var
  Comunica: string;
  Obj: TJSONObject;
begin
  if IsComunica then Comunica := 'X'
  else Comunica := '';

  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('GetNews'));
    Obj.AddPair('start', TJSONNumber.Create(Start));
    Obj.AddPair('count', TJSONNumber.Create(Count));
    Obj.AddPair('iscomunica', TJSONString.Create(Comunica));

    TRESTMdl.GetRESTResponse('', cdsNews, Obj);
  finally
    FreeAndNil(Obj);
  end;

  Result := cdsNews.Active and (cdsNews.RecordCount > 0);
end;

end.
