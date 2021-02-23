{
  @abstract(Unit to manage the members (Socis) from the colla castellera)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 06, 2020)
  @lastmod(October 06, 2020)

  The uMembers unit contains the definition and implementation of @link(TMembers) and @link(TMember) classes. This classes manage the members of the colla castellera.

  @bold(Change List) @br
  @unorderedList(
    @item(09/12/2020 : first version)
  )
}
unit uMembers;

interface

uses
  System.Generics.Collections, System.Classes, REST.Json.Types,
  Pkg.Json.DTO, uResultRequest, uRoles;

{$M+}

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uMembers.TMember.txt)
  TMember = class
  private
    FAdresa: string;
    FAlt_Esp: string;
    FAlt_Ma: string;
    FCp: string;
    FData_Alta: string;
    FData_Baixa: string;
    FData_Naix: string;
    FId: string;
    FIs_Casteller: string;
    FIs_Graller: string;
    FMail: string;
    FMalnom: string;
    FMobil: string;
    FNom: string;
    FPais: string;
    FPoblacio: string;
    FSexe: string;
    [JSONName('Rols')]
    FRolsArray: TArray<TRole>;
    [GenericListReflect]
    FRols: TObjectList<TRole>;

    FOnChange: TNotifyEvent;
    function GetRols: TObjectList<TRole>;
  public
    // @include(..\..\docs\help\uMembers.TMember.Destroy.txt)
    destructor Destroy; override;

    // @include(..\..\docs\help\uMembers.TMember.Adresa.txt)
    property Adresa: string read FAdresa write FAdresa;
    // @include(..\..\docs\help\uMembers.TMember.Alt_Esp.txt)
    property Alt_Esp: string read FAlt_Esp write FAlt_Esp;
    // @include(..\..\docs\help\uMembers.TMember.Alt_Ma.txt)
    property Alt_Ma: string read FAlt_Ma write FAlt_Ma;
    // @include(..\..\docs\help\uMembers.TMember.Cp.txt)
    property Cp: string read FCp write FCp;
    // @include(..\..\docs\help\uMembers.TMember.Data_Alta.txt)
    property Data_Alta: string read FData_Alta write FData_Alta;
    // @include(..\..\docs\help\uMembers.TMember.Data_Baixa.txt)
    property Data_Baixa: string read FData_Baixa write FData_Baixa;
    // @include(..\..\docs\help\uMembers.TMember.Data_Naix.txt)
    property Data_Naix: string read FData_Naix write FData_Naix;
    // @include(..\..\docs\help\uMembers.TMember.Id.txt)
    property Id: string read FId write FId;
    // @include(..\..\docs\help\uMembers.TMember.Is_Casteller.txt)
    property Is_Casteller: string read FIs_Casteller write FIs_Casteller;
    // @include(..\..\docs\help\uMembers.TMember.Is_Graller.txt)
    property Is_Graller: string read FIs_Graller write FIs_Graller;
    // @include(..\..\docs\help\uMembers.TMember.Mail.txt)
    property Mail: string read FMail write FMail;
    // @include(..\..\docs\help\uMembers.TMember.Malnom.txt)
    property Malnom: string read FMalnom write FMalnom;
    // @include(..\..\docs\help\uMembers.TMember.Mobil.txt)
    property Mobil: string read FMobil write FMobil;
    // @include(..\..\docs\help\uMembers.TMember.Nom.txt)
    property Nom: string read FNom write FNom;
    // @include(..\..\docs\help\uMembers.TMember.Pais.txt)
    property Pais: string read FPais write FPais;
    // @include(..\..\docs\help\uMembers.TMember.Poblacio.txt)
    property Poblacio: string read FPoblacio write FPoblacio;
    // @include(..\..\docs\help\uMembers.TMember.Sexe.txt)
    property Sexe: string read FSexe write FSexe;
    // @include(..\..\docs\help\uMembers.TMember.Rols.txt)
    property Rols: TObjectList<TRole> read GetRols;

    // @include(..\..\docs\help\uMembers.TMember.OnChange.txt)
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uMembers.TMembers.txt)
  TMembers = class(TJsonDTO)
  private
    [JSONName('Items')]
    FItemsArray: TArray<TMember>;
    [GenericListReflect]
    FItems: TObjectList<TMember>;

    function GetItems: TObjectList<TMember>;
  public
    // @include(..\..\docs\help\uMembers.TMembers.Destroy.txt)
    destructor Destroy; override;

    // @include(..\..\docs\help\uMembers.TMembers.IndexOf.txt)
    function IndexOf(Id: string): Integer;
    // @include(..\..\docs\help\uMembers.TMembers.Add.txt)
    function Add(Id, Nom: string): Integer;

    // @include(..\..\docs\help\uMembers.TMembers.GetMembers.txt)
    class function GetMembers: TMembers;
    // @include(..\..\docs\help\uMembers.TMembers.AddMember.txt)
    class function AddMember(const Nom: string): TResultRequest;
    // @include(..\..\docs\help\uMembers.TMembers.EditMember.txt)
    class function EditMember(const Item: TMember): TResultRequest;
    // @include(..\..\docs\help\uMembers.TMembers.DelMember.txt)
    class function DelMember(const Id: string): TResultRequest;

    // @include(..\..\docs\help\uMembers.TMembers.Items.txt)
    property Items: TObjectList<TMember> read GetItems;
  end;

implementation

uses
  System.JSON, System.SysUtils;

{ TMembers }

function TMembers.Add(Id, Nom: string): Integer;
begin
  Result := Items.Add(TMember.Create);
  Items[Result].Id := Id;
  Items[Result].Nom := Nom;
end;

class function TMembers.AddMember(const Nom: string): TResultRequest;
var
  Json: string;
  Obj: TJSONObject;
begin
  Result := TResultRequest.Create;
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('addMember'));
    Obj.AddPair('nom', TJSONString.Create(Nom));

    Json := Result.GetJson(Obj);
    Result.AsJson := Json;
  finally
    Obj.DisposeOf;
  end;
end;

class function TMembers.DelMember(const Id: string): TResultRequest;
begin

end;

destructor TMembers.Destroy;
begin
  GetItems.Free;

  inherited;
end;

class function TMembers.EditMember(const Item: TMember): TResultRequest;
var
  Json: string;
  Obj: TJSONObject;
begin
  Result := TResultRequest.Create;
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('editMember'));
    Obj.AddPair('id', TJSONString.Create(Item.Id));
    Obj.AddPair('nom', TJSONString.Create(Item.Nom));
    Obj.AddPair('malnom', TJSONString.Create(Item.Malnom));
    Obj.AddPair('data_naix', TJSONString.Create(Item.Data_Naix));
    Obj.AddPair('data_alta', TJSONString.Create(Item.Data_Alta));
    Obj.AddPair('data_baixa', TJSONString.Create(Item.Data_Baixa));
    Obj.AddPair('mail', TJSONString.Create(Item.Mail));
    Obj.AddPair('mobil', TJSONString.Create(Item.Mobil));
    Obj.AddPair('is_casteller', TJSONString.Create(Item.Is_Casteller));
    Obj.AddPair('is_graller', TJSONString.Create(Item.Is_Graller));
    Obj.AddPair('sexe', TJSONString.Create(Item.Sexe));
    Obj.AddPair('alt_esp', TJSONString.Create(Item.Alt_Esp));
    Obj.AddPair('alt_ma', TJSONString.Create(Item.Alt_Ma));
    Obj.AddPair('adresa', TJSONString.Create(Item.Adresa));
    Obj.AddPair('poblacio', TJSONString.Create(Item.Poblacio));
    Obj.AddPair('cp', TJSONString.Create(Item.Cp));
    Obj.AddPair('pais', TJSONString.Create(Item.Pais));

    Json := Result.GetJson(Obj);
    Result.AsJson := Json;
  finally
    Obj.DisposeOf;
  end;
end;

function TMembers.GetItems: TObjectList<TMember>;
begin
  if not Assigned(FItems) then
  begin
    FItems := TObjectList<TMember>.Create;
    FItems.AddRange(FItemsArray);
  end;
  Result := FItems;
end;

class function TMembers.GetMembers: TMembers;
var
  Json: string;
  Obj: TJSONObject;
begin
  Result := TMembers.Create;
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('getMembers'));

    Json := Result.GetJson(Obj);
    Result.AsJson := Json;
  finally
    Obj.DisposeOf;
  end;
end;

function TMembers.IndexOf(Id: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FItems.Count - 1 do
    if SameText(FItems[i].Id, Id) then
    begin
      Result := i;
      Break;
    end;
end;

{ TMember }

destructor TMember.Destroy;
begin
  GetRols.Free;

  inherited;
end;

function TMember.GetRols: TObjectList<TRole>;
begin
  if not Assigned(FRols) then
  begin
    FRols := TObjectList<TRole>.Create;
    FRols.AddRange(FRolsArray);
  end;
  Result := FRols;
end;

end.
