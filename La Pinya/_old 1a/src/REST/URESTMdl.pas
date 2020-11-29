// @include(..\..\docs\URESTMdl.txt)
unit URESTMdl;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Response.Adapter,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, FMX.StdCtrls,
  Data.DB;

const
  // @include(..\..\docs\URESTMdl.BaseUrl.txt)
  BaseUrl = 'http://www.castellersandorra.com/ca/ca_api.php?';

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
    FButton: TCustomButton;
    FAniInd: TAniIndicator;
    FFilterField: string;
    FModify: Boolean;

    FJSON: string;
    procedure ResetRESTComponentsToDefaults;
  protected
    // @include(..\..\docs\URESTMdl.TRESTMdl.StartLoading.txt)
    procedure StartLoading;
    // @include(..\..\docs\URESTMdl.TRESTMdl.EndLoading.txt)
    procedure EndLoading;

    // @include(..\..\docs\URESTMdl.TRESTMdl.DoSelect.txt)
    procedure DoSelect;
    // @include(..\..\docs\URESTMdl.TRESTMdl.DoModify.txt)
    procedure DoModify;
  public
    // @include(..\..\docs\URESTMdl.TRESTMdl.Execute.txt)
    procedure Execute;

    // @include(..\..\docs\URESTMdl.TRESTMdl.Create.txt)
    constructor Create(AOwner: TComponent; DataSet: TDataSet; Params: string;
      AniInd: TAniIndicator; Button: TCustomButton; FilterField: string;
      Modify: Boolean); reintroduce; virtual;

    // @include(..\..\docs\URESTMdl.TRESTMdl.GetRESTResponse.txt)
    class procedure GetRESTResponse(Params: string; DataSet: TDataSet;
      AniInd: TAniIndicator; Button: TCustomButton; FilterField: string = ''; Modify: Boolean = False);

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
  Params: string; AniInd: TAniIndicator; Button: TCustomButton;
  FilterField: string; Modify: Boolean);
begin
  inherited Create(AOwner);

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

procedure TRESTMdl.DoModify;
begin

end;

procedure TRESTMdl.DoSelect;
begin
  if Assigned(FDataSet) and FDataSet.Active then FDataSet.Close;

  ResetRESTComponentsToDefaults;
  RESTResponseDataSetAdapter1.Dataset := FDataSet;
  RESTResponseDataSetAdapter1.RootElement := FFilterField;

  RESTClient1.BaseURL := FUrl;
  RESTRequest1.Resource := '';
  RESTRequest1.Execute;
end;

procedure TRESTMdl.EndLoading;
begin
  if Assigned(FAniInd) then
  begin
    FAniInd.Visible := False;
    FAniInd.Enabled := False;
  end;

  if Assigned(FButton) then
    FButton.Enabled := True;
end;

procedure TRESTMdl.Execute;
begin
  StartLoading;

  try
    if not FModify then
      DoSelect
    else
      DoModify;
  finally
    EndLoading;
  end;
end;

class procedure TRESTMdl.GetRESTResponse(Params: string; DataSet: TDataSet;
  AniInd: TAniIndicator; Button: TCustomButton; FilterField: string;
  Modify: Boolean);
var
  Mdl: TRESTMdl;
begin
  Mdl := TRESTMdl.Create(nil, DataSet, Params, AniInd, Button, FilterField, Modify);
  try
    Mdl.Execute;
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
    FJSON := TJson.Format(RESTResponse1.JSONValue)
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

procedure TRESTMdl.StartLoading;
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
