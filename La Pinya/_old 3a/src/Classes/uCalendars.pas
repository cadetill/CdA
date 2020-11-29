unit uCalendars;

interface

uses
  System.Classes, System.Generics.Collections, System.JSON.Serializers,
  uBaseJson, uResultRequest;

const
  cJsonCalendars = 'Calendars.json';

type
  TCalendar = class(TBaseJson)
  private
    FId: string;
    FIdcalendar: string;
    FKey: string;
    FNom: string;
    FNom_curt: string;
    FOnChange: TNotifyEvent;
  public
    property id: string read FId write FId;
    property idcalendar: string read FIdcalendar write FIdcalendar;
    property key: string read FKey write FKey;
    property nom: string read FNom write FNom;
    property nom_curt: string read FNom_curt write FNom_curt;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCalendars = class(TBaseJson)
  private
    FItems: TArray<TCalendar>;

    function GetCount: Integer;
  public
    destructor Destroy; override;

    function IndexOf(Id: string): Integer;
    function Add(const Id, IdCalendari, Key: string): Integer;

    class function GetCalendar(var Res: TResultRequest): TCalendars;
    class function EditCalendar(const Item: TCalendar): TResultRequest;
    class function AddCalendar(const IdCalendari, Key: string): TResultRequest;
    class function DelCalendar(const Id: string): TResultRequest;

    property Items: TArray<TCalendar> read FItems write FItems;
    property Count: Integer read GetCount;
  end;

implementation

uses
  System.JSON, System.SysUtils, System.IOUtils,
  Rest.Json,
  uRESTMdl, uGenFunc;

{ TCalendars }

function TCalendars.Add(const Id, IdCalendari, Key: string): Integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := TCalendar.Create;
  FItems[High(FItems)].Id := Id;
  FItems[High(FItems)].idcalendar := IdCalendari;
  FItems[High(FItems)].Key := Key;
  FItems[High(FItems)].nom := IdCalendari;
  Result := High(FItems);
end;

class function TCalendars.AddCalendar(const IdCalendari, Key: string): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('AddCalendar'));
    Obj.AddPair('idcalendar', TJSONString.Create(IdCalendari));
    Obj.AddPair('key', TJSONString.Create(Key));
    Obj.AddPair('nom', TJSONString.Create(IdCalendari));
    Obj.AddPair('nom_curt', TJSONString.Create(''));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

class function TCalendars.DelCalendar(const Id: string): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('DelCalendar'));
    Obj.AddPair('id', TJSONString.Create(Id));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

destructor TCalendars.Destroy;
var
  LItemsItem: TCalendar;
begin
  for LItemsItem in FItems do
    LItemsItem.Free;

  inherited;
end;

class function TCalendars.EditCalendar(const Item: TCalendar): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('EditCalendar'));
    Obj.AddPair('id', TJSONString.Create(Item.id));
    Obj.AddPair('idcalendar', TJSONString.Create(Item.idcalendar));
    Obj.AddPair('key', TJSONString.Create(Item.key));
    Obj.AddPair('nom', TJSONString.Create(Item.nom));
    Obj.AddPair('nom_curt', TJSONString.Create(Item.nom_curt));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

class function TCalendars.GetCalendar(var Res: TResultRequest): TCalendars;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetCal, '', TGenFunc.GetBaseFolder + cJsonCalendars);
  if Res.Error <> '' then // si hi ha error, carreguem dades locals
    Res.Json := TCalendars.LoadJsonFromFile(cJsonCalendars);
  if Res.Json <> '' then // si tenim dades, creem objecte de sortida
    Result := TCalendars.FromJsonString<TCalendars>(Res.Json);
end;

function TCalendars.GetCount: Integer;
begin
  Result := High(FItems);
end;

function TCalendars.IndexOf(Id: string): Integer;
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

