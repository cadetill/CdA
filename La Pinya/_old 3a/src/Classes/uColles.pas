unit uColles;

interface

uses
  uBaseJson, uResultRequest;

const
  cJsonColles = 'Colles.json';

type
  TColla = class(TBaseJson)
  private
    FColla: string;
    FId: string;
  public
    property colla: string read FColla write FColla;
    property id: string read FId write FId;
  end;

  TColles = class(TBaseJson)
  private
    FItems: TArray<TColla>;
    function GetCount: Integer;
  public
    destructor Destroy; override;

    function IndexOf(Id: string): Integer;

    class function GetColles(var Res: TResultRequest): TColles;

    property Items: TArray<TColla> read FItems write FItems;
    property Count: Integer read GetCount;
  end;

implementation

uses
  Generics.Collections, Rest.Json,
  System.SysUtils, System.Classes, System.IOUtils,
  uRESTMdl, uGenFunc;

{ TColles }

destructor TColles.Destroy;
var
  LItemsItem: TColla;
begin
 for LItemsItem in FItems do
   LItemsItem.Free;

  inherited;
end;

class function TColles.GetColles(var Res: TResultRequest): TColles;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetColles, '', TGenFunc.GetBaseFolder + cJsonColles);
  if Res.Error <> '' then // si hi ha error, carreguem dades locals
    Res.Json := TColles.LoadJsonFromFile(cJsonColles);
  if Res.Json <> '' then
    Result := TColles.FromJsonString<TColles>(Res.Json);
end;

function TColles.GetCount: Integer;
begin
  Result := High(FItems);
end;

function TColles.IndexOf(Id: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count do
    if SameText(FItems[i].Id, Id) then
    begin
      Result := i;
      Break;
    end;
end;

end.
