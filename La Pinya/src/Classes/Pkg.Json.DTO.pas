{
  @abstract(unit with base class for Json results)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 11, 2020)
  @lastmod(October 11, 2020)

  The Pkg.Json.DTO unit contains the definition and implementation of @link(TJsonDTO) class. This class is the base class for REST responses.

  This file is autogenerated by https://jsontodelphi.com/ website with some modifications.

  Change List @br
  @unorderedList(
    @item(11/10/2020 : first version)
  )
}
unit Pkg.Json.DTO;

interface

uses
  System.Classes, System.Json, Rest.Json, Rest.JsonReflect;

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.txt)
  TJsonDTO = class
  private
    FOptions: TJsonOptions;
    FErrParam: string;
    FErrField: string;
    FErrFunc: string;
    FError: string;
    FErrRecordCount: string;
    FErrTable: string;

    function GetAsJson: string;
    procedure SetAsJson(aValue: string);

    class procedure PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
    class procedure PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer = 0); overload;
    class procedure PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
  public
    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.Create.txt)
    constructor Create; virtual;

    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.GetJson.txt)
    function GetJson(Params: TJSONObject): string;

    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.PrettyPrintJSON.txt)
    class function PrettyPrintJSON(aJson: string): string; overload;

    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.AsJson.txt)
    property AsJson: string read GetAsJson write SetAsJson;

    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.ErrField.txt)
    property ErrField: string read FErrField write FErrField;
    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.ErrFunc.txt)
    property ErrFunc: string read FErrFunc write FErrFunc;
    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.ErrParam.txt)
    property ErrParam: string read FErrParam write FErrParam;
    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.ErrRecordCount.txt)
    property ErrRecordCount: string read FErrRecordCount write FErrRecordCount;
    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.ErrTable.txt)
    property ErrTable: string read FErrTable write FErrTable;
    // @include(..\..\docs\help\Pkg.Json.DTO.TJsonDTO.Error.txt)
    property Error: string read FError write FError;
  end;

  TJsonDTOClass = class of TJsonDTO;

  { -------------------------------------------------------------------------- }
  // @exclude
  GenericListReflectAttribute = class(JsonReflectAttribute)
  public
    constructor Create;
  end;

implementation

uses
  System.Sysutils, System.JSONConsts, System.Rtti, System.Generics.Collections,
  REST.Client, REST.Types, IpPeerClient,
  uGenFunc;

const
  cUrlBase = 'http://www.castellersandorra.com/ca/';

{ TJsonDTO }

constructor TJsonDTO.Create;
begin
  inherited;

  FOptions := [joDateIsUTC, joDateFormatISO8601];
end;

function TJsonDTO.GetAsJson: string;
begin
  Result := TJson.ObjectToJsonString(Self, FOptions);
end;

function TJsonDTO.GetJson(Params: TJSONObject): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  if not TGenFunc.IsConnected then
  begin
    Result := '{"error": "not connected"}';
    Exit;
  end;

  RESTClient := nil;
  RESTRequest := nil;
  RESTResponse := nil;
  try
    RESTClient := TRESTClient.Create(nil);
    RESTRequest := TRESTRequest.Create(nil);
    RESTResponse := TRESTResponse.Create(nil);

    RESTClient.Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
    RESTClient.AcceptCharset := 'UTF-8, *;q=0.8';
    RESTClient.ContentType := 'application/json';
    RESTClient.RaiseExceptionOn500 := True;
    RESTClient.BaseURL := cUrlBase;

    RESTResponse.ContentType := 'application/json';

    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Accept := 'application/json';
    RESTRequest.Resource := 'ca_api.php';
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Body.Add(Params);

    try
      RESTRequest.Execute;

      if RESTResponse.StatusCode <> 200 then
      begin
        Result := '{"error": "error getting data"}';
        Exit;
      end;

      Result := RESTResponse.JSONText;
      if Result[1] = '[' then
        Result := '{"Items":' + Result + '}';
    except
      on E: Exception do
        Result := '{"error": "' + E.Message + '"}';
    end;
  finally
    RESTClient.DisposeOf;
    RESTRequest.DisposeOf;
    RESTResponse.DisposeOf;
  end;
end;

const
  INDENT_SIZE = 2;
class procedure TJsonDTO.PrettyPrintJSON(aJSONValue: TJsonValue; aOutputStrings: TStrings; Indent: Integer);
var
  i: Integer;
  Ident: Integer;
begin
  Ident := Indent + INDENT_SIZE;
  i := 0;

  if aJSONValue is TJSONObject then
  begin
    aOutputStrings.Add(StringOfChar(' ', Ident) + '{');
    for i := 0 to TJSONObject(aJSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(aJSONValue).Pairs[i], aOutputStrings, i = TJSONObject(aJSONValue).Count - 1, Ident);

    aOutputStrings.Add(StringOfChar(' ', Ident) + '}');
  end
  else if aJSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(aJSONValue), aOutputStrings, i = TJSONObject(aJSONValue).Count - 1, Ident)
  else
    aOutputStrings.Add(StringOfChar(' ', Ident) + aJSONValue.ToString);
end;

class procedure TJsonDTO.PrettyPrintArray(aJSONValue: TJSONArray; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
var
  i: Integer;
begin
  aOutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE) + '[');

  for i := 0 to aJSONValue.Count - 1 do
  begin
    PrettyPrintJSON(aJSONValue.Items[i], aOutputStrings, Indent);
    if i < aJSONValue.Count - 1 then
      aOutputStrings[aOutputStrings.Count - 1] := aOutputStrings[aOutputStrings.Count - 1] + ',';
  end;

  aOutputStrings.Add(StringOfChar(' ', Indent + INDENT_SIZE - 2) + ']');
end;

class function TJsonDTO.PrettyPrintJSON(aJson: string): string;
var
  StringList: TStringlist;
  JSONValue: TJsonValue;
begin
  StringList := TStringlist.Create;
  try
    JSONValue := TJSONObject.ParseJSONValue(aJson);
    try
      if JSONValue <> nil then
        PrettyPrintJSON(JSONValue, StringList);
    finally
      JSONValue.Free;
    end;

    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

class procedure TJsonDTO.PrettyPrintPair(aJSONValue: TJSONPair; aOutputStrings: TStrings; Last: Boolean; Indent: Integer);
const
  TEMPLATE = '%s:%s';
var
  Line: string;
  NewList: TStringlist;
begin
  NewList := TStringlist.Create;
  try
    PrettyPrintJSON(aJSONValue.JSONValue, NewList, Indent);
    Line := Format(TEMPLATE, [aJSONValue.JsonString.ToString, Trim(NewList.Text)]);
  finally
    NewList.Free;
  end;

  Line := StringOfChar(' ', Indent + INDENT_SIZE) + Line;
  if not Last then
    Line := Line + ',';
  aOutputStrings.Add(Line);
end;

procedure TJsonDTO.SetAsJson(aValue: string);
var
  JSONValue: TJsonValue;
  JSONObject: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(aValue);
  try
    if not Assigned(JSONValue) then
      Exit;

    if (JSONValue is TJSONArray) then
    begin
      with TJSONUnMarshal.Create do
        try
          SetFieldArray(Self, 'Items', (JSONValue as TJSONArray));
        finally
          Free;
        end;

      Exit;
    end;

    if (JSONValue is TJSONObject) then
      JSONObject := JSONValue as TJSONObject
    else
    begin
      aValue := aValue.Trim;
      if (aValue = '') and not Assigned(JSONValue) or (aValue <> '') and Assigned(JSONValue) and JSONValue.Null then
        Exit
      else
        raise EConversionError.Create(SCannotCreateObject);
    end;

    TJson.JsonToObject(Self, JSONObject, FOptions);
  finally
    JSONValue.Free;
  end;
end;

type
  TGenericListFieldInterceptor = class(TJSONInterceptor)
  public
    function ObjectsConverter(Data: TObject; Field: string): TListOfObjects; override;
  end;

  { TListFieldInterceptor }

function TGenericListFieldInterceptor.ObjectsConverter(Data: TObject; Field: string): TListOfObjects;
var
  ctx: TRttiContext;
  List: TList<TObject>;
begin
  List := TList<TObject>(ctx.GetType(Data.ClassInfo).GetField(Field).GetValue(Data).AsObject);
  Result := TListOfObjects(List.List);
  SetLength(Result, List.Count);
end;

constructor GenericListReflectAttribute.Create;
begin
  inherited Create(ctObjects, rtObjects, TGenericListFieldInterceptor, nil, false);
end;

end.
