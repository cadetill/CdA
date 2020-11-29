unit uRols;

interface

uses
  uBaseJson, uResultRequest;

const
  cJsonRols = 'Rols.json';

type
  TRol = class(TBaseJson)
  private
    FDescrip: string;
    FId: string;
  public
    property descrip: string read FDescrip write FDescrip;
    property id: string read FId write FId;
  end;

  TRols = class(TBaseJson)
  private
    FItems: TArray<TRol>;
    function GetCount: Integer;
  public
    destructor Destroy; override;

    function IndexOf(Id: string): Integer;

    class function GetRols(var Res: TResultRequest): TRols;

    property Items: TArray<TRol> read FItems write FItems;
    property Count: Integer read GetCount;
  end;

implementation

uses
  Generics.Collections, Rest.Json,
  System.SysUtils,
  uRESTMdl, uGenFunc;

{ TRols }

destructor TRols.Destroy;
var
  LItemsItem: TRol;
begin
 for LItemsItem in FItems do
   LItemsItem.Free;

  inherited;
end;

function TRols.GetCount: Integer;
begin
  Result := High(FItems);
end;

class function TRols.GetRols(var Res: TResultRequest): TRols;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetRols, '', TGenFunc.GetBaseFolder + cJsonRols);
  if Res.Error <> '' then // si hi ha error, carreguem dades locals
    Res.Json := TRols.LoadJsonFromFile(cJsonRols);
  if Res.Json <> '' then
    Result := TRols.FromJsonString<TRols>(Res.Json);
end;

function TRols.IndexOf(Id: string): Integer;
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
