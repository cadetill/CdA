unit uBaseJson;

interface

type
  TBaseJson = class;
  TBaseJsonClass = class of TBaseJson;

  TBaseJson = class
  private
  public
    procedure SaveToFile(FileName: string); virtual;
    procedure Compare(FileName: string); virtual;
    function ToJsonString: string;

    class function FromJsonString<T: class, constructor>(AJsonString: string): T;
    class function LoadJsonFromFile(JsonFile: string): string;
  end;

implementation

uses
  Rest.Json, REST.Json.Types,
  System.IOUtils, System.Classes, System.SysUtils,
  uGenFunc;

{ TBaseJson }

procedure TBaseJson.Compare(FileName: string);
begin

end;

class function TBaseJson.FromJsonString<T>(AJsonString: string): T;
begin
  Result := TJson.JsonToObject<T>(AJsonString);
end;

class function TBaseJson.LoadJsonFromFile(JsonFile: string): string;
var
  L: TStringList;
begin
  Result := '';
  if TFile.Exists(TGenFunc.GetBaseFolder + JsonFile) then
  begin
    L := TStringList.Create;
    try
      L.LoadFromFile(TGenFunc.GetBaseFolder + JsonFile);
      Result := L.Text;
    finally
      FreeAndNil(L);
    end;
  end;
end;

procedure TBaseJson.SaveToFile(FileName: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Text := ToJsonString;
    L.SaveToFile(FileName);
  finally
    FreeAndNil(L);
  end;
end;

function TBaseJson.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.
