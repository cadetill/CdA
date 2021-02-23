unit uRoles;

interface

uses
  Pkg.Json.DTO, System.Generics.Collections, REST.Json.Types;

{$M+}

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uRoles.TRole.txt)
  TRole = class
  private
    FDescrip: string;
    FId: string;
  published
    // @include(..\..\docs\help\uRoles.TRole.Descrip.txt)
    property Descrip: string read FDescrip write FDescrip;
    // @include(..\..\docs\help\uRoles.TRole.Id.txt)
    property Id: string read FId write FId;
  end;
  
  TRoles = class(TJsonDTO)
  private
    [JSONName('Items')]
    FItemsArray: TArray<TRole>;
    [GenericListReflect]
    FItems: TObjectList<TRole>;

    function GetItems: TObjectList<TRole>;
  published
    destructor Destroy; override;

    class function GetRoles: TRoles;

    property Items: TObjectList<TRole> read GetItems;
  end;

implementation

uses
  System.JSON;

{ TRoles }

destructor TRoles.Destroy;
begin
  GetItems.Free;
  inherited;
end;

function TRoles.GetItems: TObjectList<TRole>;
begin
  if not Assigned(FItems) then
  begin
    FItems := TObjectList<TRole>.Create;
    FItems.AddRange(FItemsArray);
  end;
  Result := FItems;
end;

class function TRoles.GetRoles: TRoles;
var
  Json: string;
  Obj: TJSONObject;
begin
  Result := TRoles.Create;
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('getRoles'));

    Json := Result.GetJson(Obj);
    Result.AsJson := Json;
  finally
    Obj.DisposeOf;
  end;
end;

end.
