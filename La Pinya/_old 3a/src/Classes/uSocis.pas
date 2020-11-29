unit uSocis;

interface

uses
  System.Classes,
  uBaseJson, uRols, uResultRequest;

const
  cJsonSocis = 'Socis.json';

type
  TSoci = class(TBaseJson)
  private
    FAdresa: string;
    FAlt_esp: string;
    FAlt_ma: string;
    FCp: string;
    FData_alta: string;
    FData_baixa: string;
    FData_naix: string;
    FId: string;
    FIs_casteller: string;
    FIs_graller: string;
    FMail: string;
    FMalnom: string;
    FMobil: string;
    FNom: string;
    FPais: string;
    FPoblacio: string;
    FRols: string;
    FSexe: string;
    FItems: TArray<TRol>;
    FOnChange: TNotifyEvent;
    FRolsText: string;
    FVinculats: string;
    procedure SetRols(const Value: string);
    function GetCount: Integer;
    procedure SetRolsText(const Value: string);
    procedure SetNom(const Value: string);
    procedure SetMalnom(const Value: string);
  public
    procedure ClearRols;
    function AddRol(Id, Descrip: string): Integer;
    function GetRols: string;

    property adresa: string read FAdresa write FAdresa;
    property alt_esp: string read FAlt_esp write FAlt_esp;
    property alt_ma: string read FAlt_ma write FAlt_ma;
    property cp: string read FCp write FCp;
    property data_alta: string read FData_alta write FData_alta;
    property data_baixa: string read FData_baixa write FData_baixa;
    property data_naix: string read FData_naix write FData_naix;
    property id: string read FId write FId;
    property is_casteller: string read FIs_casteller write FIs_casteller;
    property is_graller: string read FIs_graller write FIs_graller;
    property mail: string read FMail write FMail;
    property malnom: string read FMalnom write SetMalnom;
    property mobil: string read FMobil write FMobil;
    property nom: string read FNom write SetNom;
    property pais: string read FPais write FPais;
    property poblacio: string read FPoblacio write FPoblacio;
    property rols: string read FRols write SetRols;
    property vinculats: string read FVinculats write FVinculats;
    property sexe: string read FSexe write FSexe;
    property Items: TArray<TRol> read FItems write FItems;
    property Count: Integer read GetCount;

    property RolsText: string read FRolsText write SetRolsText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSocis = class(TBaseJson)
  private
    FItems: TArray<TSoci>;
    function GetCount: Integer;
  public
    destructor Destroy; override;

    function IndexOf(Id: string): Integer;
    function Add(const Id, Nom, Mail: string): Integer;

    class function GetSocis(var Res: TResultRequest): TSocis;
    class function EditSoci(const Item: TSoci): TResultRequest;
    class function AddSoci(const Item: TSoci): TResultRequest;
    class function DelSoci(const Id: string): TResultRequest;

    property Items: TArray<TSoci> read FItems write FItems;
    property Count: Integer read GetCount;
  end;

implementation

uses
  Generics.Collections, Rest.Json,
  System.Json, System.SysUtils,
  uRESTMdl, uGenFunc;

{ TSocis }

function TSocis.Add(const Id, Nom, Mail: string): Integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := TSoci.Create;
  FItems[High(FItems)].Id := Id;
  FItems[High(FItems)].nom := Nom;
  FItems[High(FItems)].mail := Mail;
  Result := High(FItems);
end;

class function TSocis.AddSoci(const Item: TSoci): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('AddSoci'));
    Obj.AddPair('nom', TJSONString.Create(Item.nom));
    Obj.AddPair('malnom', TJSONString.Create(Item.malnom));
    Obj.AddPair('data_naix', TJSONString.Create(FormatDateTime('yyyy-mm-dd',  TGenFunc.StringToDate(Item.data_naix))));
    Obj.AddPair('data_alta', TJSONString.Create(FormatDateTime('yyyy-mm-dd',  TGenFunc.StringToDate(Item.data_alta))));
    Obj.AddPair('data_baixa', TJSONString.Create(FormatDateTime('yyyy-mm-dd',  TGenFunc.StringToDate(Item.data_baixa))));
    Obj.AddPair('mail', TJSONString.Create(Item.mail));
    Obj.AddPair('mobil', TJSONString.Create(Item.mobil));
    Obj.AddPair('is_casteller', TJSONString.Create(Item.is_casteller));
    Obj.AddPair('is_graller', TJSONString.Create(Item.is_graller));
    Obj.AddPair('sexe', TJSONString.Create(Item.sexe));
    Obj.AddPair('alt_esp', TJSONString.Create(Item.alt_esp));
    Obj.AddPair('alt_ma', TJSONString.Create(Item.alt_ma));
    Obj.AddPair('adresa', TJSONString.Create(Item.adresa));
    Obj.AddPair('poblacio', TJSONString.Create(Item.poblacio));
    Obj.AddPair('cp', TJSONString.Create(Item.cp));
    Obj.AddPair('pais', TJSONString.Create(Item.pais));
    Obj.AddPair('rols', TJSONString.Create(Item.GetRols));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

class function TSocis.DelSoci(const Id: string): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('DelSoci'));
    Obj.AddPair('id', TJSONString.Create(Id));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

destructor TSocis.Destroy;
var
  LItemsItem: TSoci;
begin
 for LItemsItem in FItems do
   LItemsItem.Free;

  inherited;
end;

class function TSocis.EditSoci(const Item: TSoci): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('EditSoci'));
    Obj.AddPair('id', TJSONString.Create(Item.id));
    Obj.AddPair('nom', TJSONString.Create(Item.nom));
    Obj.AddPair('malnom', TJSONString.Create(Item.malnom));
    Obj.AddPair('data_naix', TJSONString.Create(FormatDateTime('yyyy-mm-dd',  TGenFunc.StringToDate(Item.data_naix))));
    Obj.AddPair('data_alta', TJSONString.Create(FormatDateTime('yyyy-mm-dd',  TGenFunc.StringToDate(Item.data_alta))));
    Obj.AddPair('data_baixa', TJSONString.Create(FormatDateTime('yyyy-mm-dd',  TGenFunc.StringToDate(Item.data_baixa))));
    Obj.AddPair('mail', TJSONString.Create(Item.mail));
    Obj.AddPair('mobil', TJSONString.Create(Item.mobil));
    Obj.AddPair('is_casteller', TJSONString.Create(Item.is_casteller));
    Obj.AddPair('is_graller', TJSONString.Create(Item.is_graller));
    Obj.AddPair('sexe', TJSONString.Create(Item.sexe));
    Obj.AddPair('alt_esp', TJSONString.Create(Item.alt_esp));
    Obj.AddPair('alt_ma', TJSONString.Create(Item.alt_ma));
    Obj.AddPair('adresa', TJSONString.Create(Item.adresa));
    Obj.AddPair('poblacio', TJSONString.Create(Item.poblacio));
    Obj.AddPair('cp', TJSONString.Create(Item.cp));
    Obj.AddPair('pais', TJSONString.Create(Item.pais));
    Obj.AddPair('rols', TJSONString.Create(Item.GetRols));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

function TSocis.GetCount: Integer;
begin
  Result := High(FItems);
end;

class function TSocis.GetSocis(var Res: TResultRequest): TSocis;
var
  i: Integer;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetSocis, '', TGenFunc.GetBaseFolder + cJsonSocis);
  if Res.Error <> '' then // si hi ha error, carreguem dades locals
    Res.Json := TSocis.LoadJsonFromFile(cJsonSocis);
  if Res.Json <> '' then
    Result := TSocis.FromJsonString<TSocis>(Res.Json);

  for i := 0 to Result.Count do
    Result.Items[i].rols := Result.Items[i].rols;
end;

function TSocis.IndexOf(Id: string): Integer;
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

{ TSoci }

function TSoci.AddRol(Id, Descrip: string): Integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := TRol.Create;
  FItems[High(FItems)].Id := Id;
  FItems[High(FItems)].Descrip := Descrip;
  Result := High(FItems);
end;

procedure TSoci.ClearRols;
var
  i: Integer;
begin
  for i := 0 to Count do
    Items[i].Free;
  SetLength(FItems, 0);
end;

function TSoci.GetCount: Integer;
begin
  Result := High(FItems);
end;

function TSoci.GetRols: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count do
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + Items[i].id;
  end;
end;

procedure TSoci.SetMalnom(const Value: string);
begin
  FMalnom := Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoci.SetNom(const Value: string);
begin
  FNom := Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSoci.SetRols(const Value: string);
var
  L: TStringList;
  i: Integer;
begin
  // esborrem rols anteriors
  ClearRols;

  // afegim nous rols
  FRols := Value;
  L := TStringList.Create;
  try
    L.CommaText := FRols;
    for i := 0 to L.Count - 1 do
      AddRol(TGenFunc.GetField(L[i], 1, ';'), TGenFunc.GetField(L[i], 2, ';'));
  finally
    FreeAndNil(L);
  end;
end;

procedure TSoci.SetRolsText(const Value: string);
begin
  FRolsText := Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
