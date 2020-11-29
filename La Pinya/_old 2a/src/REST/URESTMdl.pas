// @include(..\..\docs\URESTMdl.txt)
unit URESTMdl;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Response.Adapter,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, FMX.StdCtrls,
  Data.DB, System.JSON, REST.Types;

const
  // @include(..\..\docs\URESTMdl.BaseUrl.txt)
  BaseUrl = 'http://www.castellersandorra.com/ca/';

type
  // @include(..\..\docs\URESTMdl.TRESTMdl.txt)
  TRESTMdl = class(TDataModule)
    {@exclude} RESTClient1: TRESTClient;
    {@exclude} RESTResponse1: TRESTResponse;
    {@exclude} RESTRequest1: TRESTRequest;
    {@exclude} RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    {@exclude} procedure RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
    {@exclude} procedure RESTResponseDataSetAdapter1BeforeOpenDataSet(Sender: TObject);
  private
    FDataSet: TDataSet;
    FUrl: string;
    FFilterField: string;
    FAValue: TJSONObject;

    FJSON: string;
    procedure ResetRESTComponentsToDefaults;
  protected
  public
    // @include(..\..\docs\URESTMdl.TRESTMdl.Execute.txt)
    procedure Execute;

    // @include(..\..\docs\URESTMdl.TRESTMdl.Create.txt)
    constructor Create(AOwner: TComponent; DataSet: TDataSet; Params: string;
      FilterField: string; const AValue: TJSONObject); reintroduce; virtual;

    // @include(..\..\docs\URESTMdl.TRESTMdl.GetRESTResponse.txt)
    class function GetRESTResponse(Params: string; DataSet: TDataSet; const AValue: TJSONObject; FilterField: string = ''): string;

    // @include(..\..\docs\URESTMdl.TRESTMdl.JSON.txt)
    property JSON: string read FJSON;
  end;

var
  RESTMdl: TRESTMdl;

implementation

uses
  REST.Json, Datasnap.DBClient;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TRESTMdl }

constructor TRESTMdl.Create(AOwner: TComponent; DataSet: TDataSet;
  Params: string; FilterField: string; const AValue: TJSONObject);
begin
  inherited Create(AOwner);

  FDataSet := DataSet;
  if Pos('http', Params) > 0 then
    FUrl := Params
  else
    FUrl := BaseUrl + Params;
  FFilterField := FilterField;
  FAValue := AValue;
end;

procedure TRESTMdl.Execute;
begin
  if Assigned(FDataSet) and FDataSet.Active then FDataSet.Close;

  ResetRESTComponentsToDefaults;
  RESTResponseDataSetAdapter1.Dataset := FDataSet;
  RESTResponseDataSetAdapter1.RootElement := FFilterField;

  RESTClient1.BaseURL := FUrl;
  if FUrl <> BaseUrl then  // petició GET
  begin
    RESTRequest1.Resource := '';
    RESTRequest1.Method := TRESTRequestMethod.rmGET;
    RESTRequest1.Body.ClearBody;
  end
  else  // petició POST
  begin
    RESTRequest1.Resource := 'ca_api.php';
    RESTRequest1.Method := TRESTRequestMethod.rmPOST;
    RESTRequest1.Body.Add(FAValue);
  end;

  RESTRequest1.Execute;
end;

class function TRESTMdl.GetRESTResponse(Params: string; DataSet: TDataSet;
  const AValue: TJSONObject; FilterField: string): string;
var
  Mdl: TRESTMdl;
begin
  Mdl := TRESTMdl.Create(nil, DataSet, Params, FilterField, AValue);
  try
    Mdl.Execute;
    Result := Mdl.JSON;
  finally
    FreeAndNil(Mdl);
  end;
end;

procedure TRESTMdl.ResetRESTComponentsToDefaults;
begin
  RESTRequest1.ResetToDefaults;
  RESTClient1.ResetToDefaults;
  RESTResponse1.ResetToDefaults;
  RESTResponseDataSetAdapter1.ResetToDefaults;
end;

procedure TRESTMdl.RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
begin
  if Assigned(RESTResponse1.JSONValue) then
    FJSON := TJSON.Format(RESTResponse1.JSONValue)
  else
    FJSON := RESTResponse1.Content;
end;

procedure TRESTMdl.RESTResponseDataSetAdapter1BeforeOpenDataSet(
  Sender: TObject);
begin
  if not RESTResponseDataSetAdapter1.Dataset.Active then
    if RESTResponseDataSetAdapter1.Dataset is TClientDataSet then
      (RESTResponseDataSetAdapter1.Dataset as TClientDataSet).CreateDataSet
    else
      RESTResponseDataSetAdapter1.Dataset.Active := True;
end;

end.
