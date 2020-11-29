unit uEvents;

interface

uses
  System.Classes,
  uBaseJson, uResultRequest;

const
  cJsonEvents = 'Events.json';

type
  TEvent = class(TBaseJson)
  private
    FAny: string;
    FDataf: string;
    FDatai: string;
    FDescrip: string;
    FHoraf: string;
    FHorai: string;
    FId: string;
    FId_cal: string;
    FId_google: string;
    FLloc: string;
    FModif: string;
    FNom_curt: string;
    FStatus: string;
    FColles: string;
    FQuedard: string;
    FQuedar: string;
    FQuedarh: string;
    FInfo: string;
    FOnChange: TNotifyEvent;
    FNameColles: string;
    procedure SetNameColles(const Value: string);
  public
    function IsEqualTo(Event: TEvent): Boolean;

    property Status: string read FStatus write FStatus;    // Google Calendar => indica si el event ha estat anulat o es vigent
    property any: string read FAny write FAny;
    property dataf: string read FDataf write FDataf;
    property datai: string read FDatai write FDatai;
    property descrip: string read FDescrip write FDescrip;
    property horaf: string read FHoraf write FHoraf;
    property horai: string read FHorai write FHorai;
    property id: string read FId write FId;
    property id_cal: string read FId_cal write FId_cal;
    property id_google: string read FId_google write FId_google;
    property lloc: string read FLloc write FLloc;
    property modif: string read FModif write FModif;
    property nom_curt: string read FNom_curt write FNom_curt;
    property colles: string read FColles write FColles;
    property quedar: string read FQuedar write FQuedar;
    property quedard: string read FQuedard write FQuedard;
    property quedarh: string read FQuedarh write FQuedarh;
    property info: string read FInfo write FInfo;

    property NameColles: string read FNameColles write SetNameColles;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

 TEvents = class(TBaseJson)
  private
    FItems: TArray<TEvent>;
    function GetCount: Integer;
  public
    destructor Destroy; override;

    function IndexOf(Id: string): Integer; overload;
    function IndexOf(GoogleId, DataI: string): Integer; overload;
    function Add(const Id, Any, IdCal, IdGoogle, Descrip, Location, Status, HoraI,
      HoraF, DataI, DataF: string): Integer;

    class function GetEvents(var Res: TResultRequest; const Any: string): TEvents;
    class function EditEvent(const Item: TEvent; ModifOption: Integer): TResultRequest;
    class function AddEvent(const Item: TEvent): TResultRequest;
    class function DelEvent(const Id: string): TResultRequest;

    property Items: TArray<TEvent> read FItems write FItems;
    property Count: Integer read GetCount;
  end;

implementation

uses
  System.SysUtils, System.JSON,
  uGenFunc, uRESTMdl;

{ TEvents }

function TEvents.Add(const Id, Any, IdCal, IdGoogle, Descrip, Location, Status,
  HoraI, HoraF, DataI, DataF: string): Integer;
begin
  SetLength(FItems, Length(FItems) + 1);

  FItems[High(FItems)] := TEvent.Create;
  FItems[High(FItems)].Id := Id;
  FItems[High(FItems)].any := Any;
  FItems[High(FItems)].id_cal := IdCal;
  FItems[High(FItems)].id_google := IdGoogle;
  FItems[High(FItems)].descrip := Descrip;
  FItems[High(FItems)].Status := Status;
  FItems[High(FItems)].datai := DataI;
  FItems[High(FItems)].dataf := DataF;
  FItems[High(FItems)].horai := HoraI;
  FItems[High(FItems)].horaf := HoraF;
  FItems[High(FItems)].lloc := Location;

  Result := High(FItems);
end;

class function TEvents.AddEvent(const Item: TEvent): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('UpdateEvents'));
    Obj.AddPair('id', TJSONString.Create(Item.id));
    Obj.AddPair('id_cal', TJSONString.Create(Item.id_cal));
    Obj.AddPair('id_google', TJSONString.Create(Item.id_google));
    Obj.AddPair('any', TJSONString.Create(Item.any));
    Obj.AddPair('descrip', TJSONString.Create(TGenFunc.NormalizeString(Item.descrip)));
    Obj.AddPair('datai', TJSONString.Create(Item.datai));
    Obj.AddPair('dataf', TJSONString.Create(Item.dataf));
    Obj.AddPair('horai', TJSONString.Create(Item.horai));
    Obj.AddPair('horaf', TJSONString.Create(Item.horaf));
    Obj.AddPair('lloc', TJSONString.Create(TGenFunc.NormalizeString(Item.lloc)));
    Obj.AddPair('modif', TJSONString.Create('1'));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

class function TEvents.DelEvent(const Id: string): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('UpdateEvents'));
    Obj.AddPair('id', TJSONString.Create(Id));
    Obj.AddPair('id_cal', TJSONString.Create(''));
    Obj.AddPair('id_google', TJSONString.Create(''));
    Obj.AddPair('any', TJSONString.Create(''));
    Obj.AddPair('descrip', TJSONString.Create(''));
    Obj.AddPair('datai', TJSONString.Create(''));
    Obj.AddPair('dataf', TJSONString.Create(''));
    Obj.AddPair('horai', TJSONString.Create(''));
    Obj.AddPair('horaf', TJSONString.Create(''));
    Obj.AddPair('lloc', TJSONString.Create(''));
    Obj.AddPair('modif', TJSONString.Create('3'));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

destructor TEvents.Destroy;
var
  LItemsItem: TEvent;
begin
 for LItemsItem in FItems do
   LItemsItem.Free;

  inherited;
end;

class function TEvents.EditEvent(const Item: TEvent; ModifOption: Integer): TResultRequest;
var
  Obj: TJSONObject;
begin
  {
    valors possibles per ModifOption
      2: modificarà les dades provinents de Google Calendar
      4: modificarà la resta de dades (les que es poden canviar des de l'APP
  }

  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('UpdateEvents'));
    Obj.AddPair('id', TJSONString.Create(Item.id));
    Obj.AddPair('id_cal', TJSONString.Create(Item.id_cal));
    Obj.AddPair('id_google', TJSONString.Create(Item.id_google));
    Obj.AddPair('any', TJSONString.Create(Item.any));
    Obj.AddPair('descrip', TJSONString.Create(Item.descrip));
    Obj.AddPair('datai', TJSONString.Create(Item.datai));
    Obj.AddPair('dataf', TJSONString.Create(Item.dataf));
    Obj.AddPair('horai', TJSONString.Create(Item.horai));
    Obj.AddPair('horaf', TJSONString.Create(Item.horaf));
    Obj.AddPair('lloc', TJSONString.Create(Item.lloc));
    Obj.AddPair('quedar', TJSONString.Create(Item.quedar));
    Obj.AddPair('quedard', TJSONString.Create(Item.quedard));
    Obj.AddPair('quedarh', TJSONString.Create(Item.quedarh));
    Obj.AddPair('info', TJSONString.Create(Item.info));
    Obj.AddPair('colles', TJSONString.Create(Item.colles));
    Obj.AddPair('modif', TJSONString.Create(ModifOption.ToString));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

function TEvents.GetCount: Integer;
begin
  Result := High(FItems);
end;

class function TEvents.GetEvents(var Res: TResultRequest; const Any: string): TEvents;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetEvents, Any, TGenFunc.GetBaseFolder + cJsonEvents);
  if Res.Error <> '' then // si hi ha error, carreguem dades locals
    Res.Json := TEvents.LoadJsonFromFile(cJsonEvents);
  if Res.Json <> '' then
    Result := TEvents.FromJsonString<TEvents>(Res.Json);
end;

function TEvents.IndexOf(GoogleId, DataI: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count do
    if SameText(FItems[i].id_google, GoogleId) and SameText(FItems[i].datai, DataI) then
    begin
      Result := i;
      Break;
    end;
end;

function TEvents.IndexOf(Id: string): Integer;
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

{ TEvent }

function TEvent.IsEqualTo(Event: TEvent): Boolean;
begin
  Result := (FAny = Event.any) and  // si és de la mateixa temporada
            SameText(FId_google, Event.id_google) and  // si té el mateix Id de Google
            (FDatai = Event.datai) and
            (FDataf = Event.dataf) and
            (FHorai = Event.horai) and
            (FHoraf = Event.horaf) and
            SameText(FDescrip, Event.descrip) and // si té la mateixa descripció
            SameText(FLloc, Event.lloc);  // si te la mateixa localització
end;

procedure TEvent.SetNameColles(const Value: string);
begin
  FNameColles := Value;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
