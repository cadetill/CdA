unit URESTDataMdl;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Response.Adapter,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, Data.DB,
  FMX.StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP;

const
  BaseUrl = 'http://www.castellersandorra.com/ca/ca_api.php?';

type
  TRESTDataMdl = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTResponse1: TRESTResponse;
    RESTRequest1: TRESTRequest;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    IdHTTP1: TIdHTTP;
    procedure RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
    procedure RESTResponseDataSetAdapter1BeforeOpenDataSet(Sender: TObject);
  private
    FJSON: string;

    procedure ResetRESTComponentsToDefaults;
  public
    class procedure GetRESTResponse(Params: string; DataSet: TDataSet; OnTerminateEvent: TNotifyEvent;
      AniInd: TAniIndicator; Button: TCustomButton; FilterField: string = ''; Modify: Boolean = False);

    property JSON: string read FJSON;
  end;

  // classe thread per a la no "congelació" de l'aplicació
  TRESTThread = class(TThread)
  private
    FDataSet: TDataSet;
    FUrl: string;
    FJSONText: string;
    FAniInd: TAniIndicator;
    FButton: TCustomButton;
    FFilterField: string;
    FModify: Boolean;
  protected
    procedure Execute; override;

    procedure StartLoading;
    procedure EndLoading;

    procedure DoSelect;
    procedure DoModify;

    property DataSet: TDataSet read FDataSet;
    property Url: string read FUrl;
    property FilterField: string read FFilterField;
  public
    constructor Create(CreateSuspended: Boolean; DataSet: TDataSet; Params: string;
      AniInd: TAniIndicator; Button: TCustomButton; FilterField: string; Modify: Boolean); virtual;

    property JSONText: string read FJSONText;
    property Button: TCustomButton read FButton;
  end;
var
  RESTDataMdl: TRESTDataMdl;

implementation

uses
  REST.Json, REST.Types, Datasnap.DBClient;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TRESTDataMdl }

class procedure TRESTDataMdl.GetRESTResponse(Params: string; DataSet: TDataSet;
  OnTerminateEvent: TNotifyEvent; AniInd: TAniIndicator; Button: TCustomButton;
  FilterField: string; Modify: Boolean);
begin
  inherited;

  with TRESTThread.Create(True, DataSet, Params, AniInd, Button, FilterField, Modify) do
  begin
    OnTerminate := OnTerminateEvent;
    Sleep(100);
    FreeOnTerminate := True;
    Sleep(100);
    Start;
  end;
end;

procedure TRESTDataMdl.ResetRESTComponentsToDefaults;
begin
  RESTRequest1.ResetToDefaults;
  RESTClient1.ResetToDefaults;
  RESTResponse1.ResetToDefaults;
  RESTResponseDataSetAdapter1.ResetToDefaults;
end;

procedure TRESTDataMdl.RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
begin
  if assigned(RESTResponse1.JSONValue) then
    FJSON := TJson.Format(RESTResponse1.JSONValue)
  else
    FJSON := RESTResponse1.Content;
end;

procedure TRESTDataMdl.RESTResponseDataSetAdapter1BeforeOpenDataSet(
  Sender: TObject);
begin
  if not RESTResponseDataSetAdapter1.Dataset.Active then
    if RESTResponseDataSetAdapter1.Dataset is TClientDataSet then
      (RESTResponseDataSetAdapter1.Dataset as TClientDataSet).CreateDataSet
    else
      RESTResponseDataSetAdapter1.Dataset.Active := True;
end;

{ TRESTThread }

constructor TRESTThread.Create(CreateSuspended: Boolean; DataSet: TDataSet;
  Params: string; AniInd: TAniIndicator; Button: TCustomButton;
  FilterField: string; Modify: Boolean);
begin
  inherited Create(CreateSuspended);

  FDataSet := DataSet;
  if Pos('http', Params) > 0 then
    FUrl := Params
  else
    FUrl := BaseUrl + Params;
  FAniInd := AniInd;
  FButton := Button;
  FFilterField := FilterField;
  FModify := Modify;
end;

procedure TRESTThread.DoModify;
var
  Mdl: TRESTDataMdl;
  lStream: TStringStream;
begin
  Mdl := TRESTDataMdl.Create(nil);
  lStream := TStringStream.Create(FJSONText);
  try
    with Mdl do
    begin
      IdHTTP1.Get(FUrl, lStream);
      lStream.Position := 0;
      FJSONText := lStream.ReadString(lStream.Size);
    end;
  finally
    FreeAndNil(lStream);
    FreeAndNil(Mdl);
  end;
end;

procedure TRESTThread.DoSelect;
var
  Mdl: TRESTDataMdl;
begin
  if Assigned(FDataSet) and FDataSet.Active then FDataSet.Close;

  Mdl := TRESTDataMdl.Create(nil);
  try
    with Mdl do
    begin
      ResetRESTComponentsToDefaults;
      RESTResponseDataSetAdapter1.Dataset := FDataSet;
      RESTResponseDataSetAdapter1.RootElement := FFilterField;

      RESTClient1.BaseURL := FUrl;
      RESTRequest1.Resource := '';
      RESTRequest1.Execute;

      FJSONText := JSON;
    end;
  finally
    FreeAndNil(Mdl);
  end;
end;

procedure TRESTThread.EndLoading;
begin
  if Assigned(FAniInd) then
  begin
    FAniInd.Visible := False;
    FAniInd.Enabled := False;
  end;

  if Assigned(FButton) then
    FButton.Enabled := True;
end;

procedure TRESTThread.Execute;
begin
  inherited;

  Synchronize(StartLoading);

  try
    if not FModify then
      DoSelect
    else
      DoModify;
  finally
    Synchronize(EndLoading);
  end;
end;

procedure TRESTThread.StartLoading;
begin
  if Assigned(FAniInd) then
  begin
    FAniInd.Visible := True;
    FAniInd.Enabled := True;
  end;

  if Assigned(FButton) then
    FButton.Enabled := False;
end;

end.
