unit uRESTMdl;

interface

uses
  System.SysUtils, System.Classes, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, IPPeerClient, System.JSON,
  uBaseJson, uResultRequest;

const
  cUrlBase = 'http://www.castellersandorra.com/ca/';
  cUrlCalendars = 'http://www.castellersandorra.com/ca/ca_api.php?func=GetCalendars';
  cUrlSeasons = 'http://www.castellersandorra.com/ca/ca_api.php?func=GetTemporades';
  cUrlEvents = 'http://www.castellersandorra.com/ca/ca_api.php?func=GetEvents&any=%s';
  cUrlGoogleCal = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?key=%s&timeMin=%s-01-01T00:00:00-01:00&timeMax=%s-12-31T00:00:00-01:00';
  cUrlColles = 'http://www.castellersandorra.com/ca/ca_api.php?func=GetColles';
  cUrlSocis = 'http://www.castellersandorra.com/ca/ca_api.php?func=GetSocis';
  cUrlRols = 'http://www.castellersandorra.com/ca/ca_api.php?func=GetRols';

type
  TTypeClass = (tcGetCal, tcGetSea, tcGetGoogleCal, tcGetEvents, tcGetColles,
      tcGetSocis, tcGetRols);

  TRESTMdl = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
  private
  protected
    function LoadData(TypeClass: TTypeClass; ExtraData: string = ''; FileName: string = ''): TResultRequest;
    function ExecuteUpdate(Params: TJSONObject): TResultRequest;
  public
    function GetHTML(Url: string; out HTML: string): Boolean;

    class function GetUpdateResponse(const Params: TJSONObject): TResultRequest;
    class function GetData(TypeClass: TTypeClass; ExtraData: string = ''; FileName: string = ''): TResultRequest;
  end;

var
  RESTMdl: TRESTMdl;

implementation

uses
  System.IOUtils, System.Net.HttpClientComponent, System.Net.HttpClient,
  uCalendars, uGenFunc;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TRESTMdl }

function TRESTMdl.ExecuteUpdate(Params: TJSONObject): TResultRequest;
var
  TmpStr: string;
begin
  if not TGenFunc.IsConnected then
  begin
    Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"","Error":"Sense connexió","Json":""}');
    Exit;
  end;

  RESTClient1.BaseURL := cUrlBase;
  RESTRequest1.Resource := 'ca_api.php';
  RESTRequest1.Method := TRESTRequestMethod.rmPOST;
  RESTRequest1.Body.Add(Params);

  try
    RESTRequest1.Execute;

    if RESTResponse1.StatusCode <> 200 then
    begin
      Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"X","Error":"Error obtenint la informació","Json":""}');
      Exit;
    end;

    TmpStr := RESTResponse1.JSONText;
    TmpStr := StringReplace(TmpStr, '[', '', []);
    TmpStr := StringReplace(TmpStr, ']', '', []);
    TmpStr := TGenFunc.DeNormalizeString(TmpStr);
    Result := TResultRequest.FromJsonString<TResultRequest>(TmpStr);
  except
    on E: Exception do
    begin
      Result := TResultRequest.Create;
      Result.Error := E.Message;
    end;
  end;
end;

class function TRESTMdl.GetData(TypeClass: TTypeClass; ExtraData,
  FileName: string): TResultRequest;
var
  Mdl: TRESTMdl;
begin
  Mdl := TRESTMdl.Create(nil);
  try
    Result := Mdl.LoadData(TypeClass, ExtraData, FileName);
  finally
    FreeAndNil(Mdl);
  end;
end;

function TRESTMdl.GetHTML(Url: string; out HTML: string): Boolean;
var
  Client: TNetHTTPClient;
  Request: TNetHTTPRequest;
  Resp: IHTTPResponse;
begin
  HTML := '';
  Client := nil;
  Request := nil;
  try
    Client := TNetHTTPClient.Create(nil);
    Request := TNetHTTPRequest.Create(nil);

    Request.Client := Client;
    Request.URL := Url;
    Request.MethodString := 'GET';
    Resp := Request.Execute;
    Result := Resp.StatusCode = 200;
    if Result then
      HTML := Resp.ContentAsString;
  finally
    FreeAndNil(Client);
    FreeAndNil(Request);
  end;
end;

class function TRESTMdl.GetUpdateResponse(const Params: TJSONObject): TResultRequest;
var
  Mdl: TRESTMdl;
begin
  Mdl := TRESTMdl.Create(nil);
  try
    Result := Mdl.ExecuteUpdate(Params);
  finally
    FreeAndNil(Mdl);
  end;
end;

function TRESTMdl.LoadData(TypeClass: TTypeClass; ExtraData: string;
  FileName: string): TResultRequest;
var
  Url: string;
  TmpStr: string;
  L: TStringList;
begin
  if not TGenFunc.IsConnected then
  begin
    Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"","Error":"Sense connexió","Json":""}');
    Exit;
  end;

  Url := '';
  case TypeClass of
    tcGetCal: Url := cUrlCalendars;
    tcGetSea: Url := cUrlSeasons;
    tcGetColles: Url := cUrlColles;
    tcGetSocis: Url := cUrlSocis;
    tcGetRols: Url := cUrlRols;
    tcGetGoogleCal:
      begin
        if ExtraData = '' then
        begin
          Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"X","Error":"Falta ExtraData","Json":""}');
          Exit;
        end;
        Url := ExtraData;
      end;
    tcGetEvents:
      begin
        if ExtraData = '' then
        begin
          Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"X","Error":"Falta ExtraData","Json":""}');
          Exit;
        end;
        Url := Format(cUrlEvents, [ExtraData]);
      end;
  end;

  RESTClient1.BaseURL := Url;
  RESTRequest1.Method := TRESTRequestMethod.rmGET;
  RESTRequest1.Execute;

  if RESTResponse1.StatusCode <> 200 then
  begin
    Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"X","Error":"Error obtenint la informació","Json":""}');
    Exit;
  end;

  TmpStr := RESTResponse1.JSONText;
  {$IFDEF MSWINDOWS}
  if TmpStr[1] = '[' then
  {$ELSE}
  if TmpStr[0] = '[' then
  {$ENDIF}
    TmpStr := '{"Items":' + TmpStr + '}';
  TmpStr := TGenFunc.DeNormalizeString(TmpStr);
  Result := TResultRequest.FromJsonString<TResultRequest>('{"status":"","id":"0","affected":"0","Connected":"X","Error":"","Json":""}');
  Result.Json := TmpStr;

  // si tenim nom de fitxer, gravem les dades
  if FileName <> '' then
  begin
    L := TStringList.Create;
    try
      L.Text := TmpStr;
      L.SaveToFile(FileName);
    finally
      FreeAndNil(L);
    end;
  end;
end;

end.
