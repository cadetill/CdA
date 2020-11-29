unit uSeasons;

interface

uses
  System.Classes,
  uBaseJson, uResultRequest,
  uCalendars, uGoogleCal, uEvents;

const
  cFreqYearly = 'YEARLY';
  cFreqMontly = 'MONTHLY';
  cFreqWeekly = 'WEEKLY';
  cFreqDaily = 'DAILY';
  cFreqHourly = 'HOURLY';
  cFreqMinutely = 'MINUTELY';
  cNodeRecFREQ = 'FREQ';
  cNodeRecCOUNT = 'COUNT';
  cNodeRecUNTIL = 'UNTIL';
  cNodeRecBYDAY = 'BYDAY';
  cNodeRecINTERVAL = 'INTERVAL';
  cStatusConfirmed = 'confirmed';
  cStatusCancelled = 'cancelled';

  cJsonSeasons = 'Seasons.json';

type
  TSeason = class(TBaseJson)
  private
    FAny: String;
    FDescripcio: String;
    FOnChange: TNotifyEvent;
  public
    property any: String read FAny write FAny;
    property descripcio: String read FDescripcio write FDescripcio;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSeasons = class(TBaseJson)
  private
    FItems: TArray<TSeason>;

    function GetCount: Integer;
  public
    destructor Destroy; override;

    function IndexOf(Any: string): Integer;
    function Add(const Any, Descripcio: string): Integer;

    class function GetSeasons(var Res: TResultRequest): TSeasons;
    class function EditSeason(const Item: TSeason): TResultRequest;
    class function AddSeason(const Year, Description: string): TResultRequest;
    class function DelSeason(const Year: string): TResultRequest;
    class function SyncSeasons(const Season: TSeason; const Cal: TCalendars): TResultRequest;

    property Items: TArray<TSeason> read FItems write FItems;
    property Count: Integer read GetCount;
  end;

  TFreq = (fYearly, fMontly, fWeekly, fDaily, fHourly, fMinutely, fUndefined);

  TRRule = record
  private
    class var
      FByDay: string;
      FUntilDate: string;
      FCount: string;
      FFreq: string;
      FInterval: string;

      FFreqs: TFreq;
      FCountI: Integer;
      FByDayA: array[1..7] of Boolean;

    class procedure SetFreq(const Value: string); static;
    class procedure SetByDay(const Value: string); static;
  public
    class property Freq: string read FFreq write SetFreq;
    class property Count: string read FCount write FCount;
    class property UntilDate: string read FUntilDate write FUntilDate;
    class property ByDay: string read FByDay write SetByDay;
    class property Interval: string read FInterval write FInterval;

    class procedure Clear; static;
    class function HaveRRule: Boolean; static;
    class function NextDate(DateI: TDate): TDate; static;
    class function IsFinished(DateI: TDate): Boolean; static;
  end;

  TSeasonsFunc = record
  public
    class procedure ProcessGoogleCalendar(const Season: TSeason; const Cal: TCalendar;
      const Data: TGoogleCal; var Events: TEvents); static;
    class procedure InsertIntoEvents(GoogleItem: TGoogleItem; Any: string;
      DI, DF: TDate; HI, HF: TTime; var Events: TEvents; const Cal: TCalendar); static;
    class function GetRules(RRule: string): TRRule; static;
  end;

implementation

uses
  System.SysUtils, System.JSON, System.DateUtils,
  uGenFunc, uRESTMdl;

{ TSeasons }

function TSeasons.Add(const Any, Descripcio: string): Integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := TSeason.Create;
  FItems[High(FItems)].Any := Any;
  FItems[High(FItems)].descripcio := Descripcio;
  Result := High(FItems);
end;

class function TSeasons.AddSeason(const Year,
  Description: string): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('AddTemporada'));
    Obj.AddPair('any', TJSONString.Create(Year));
    Obj.AddPair('descripcio', TJSONString.Create(Description));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

class function TSeasons.DelSeason(const Year: string): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('DelTemporada'));
    Obj.AddPair('any', TJSONString.Create(Year));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

destructor TSeasons.Destroy;
var
  LItemsItem: TSeason;
begin
  for LItemsItem in FItems do
    LItemsItem.Free;

  inherited;
end;

class function TSeasons.EditSeason(const Item: TSeason): TResultRequest;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('EditTemporada'));
    Obj.AddPair('any', TJSONString.Create(Item.any));
    Obj.AddPair('descripcio', TJSONString.Create(Item.descripcio));

    Result := TRESTMdl.GetUpdateResponse(Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

function TSeasons.GetCount: Integer;
begin
  Result := High(FItems);
end;

class function TSeasons.GetSeasons(var Res: TResultRequest): TSeasons;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetSea, '', TGenFunc.GetBaseFolder + cJsonSeasons);
  if Res.Error <> '' then // si hi ha error, carreguem dades locals
    Res.Json := TSeasons.LoadJsonFromFile(cJsonSeasons);
  if Res.Json <> '' then
    Result := TSeasons.FromJsonString<TSeasons>(Res.Json);
end;

function TSeasons.IndexOf(Any: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count do
    if SameText(FItems[i].Any, Any) then
    begin
      Result := i;
      Break;
    end;
end;

class function TSeasons.SyncSeasons(const Season: TSeason; const Cal: TCalendars): TResultRequest;
var
  Datas: array of TGoogleCal;
  i: Integer;
  TmpEvent: TEvents;
  Events: TEvents;
  Idx: Integer;
begin
  SetLength(Datas, Cal.Count+1);

  Result := TResultRequest.Create;
  // recuperem events de Google Calendar
  for i := 0 to Cal.Count do
  begin
    Datas[i] := TGoogleCal.GetGoogleCal(Result, Cal.Items[i].idcalendar, Cal.Items[i].key, Season.any);
    if Result.connected = '' then // no hi ha connexió
      Exit;
  end;

  // recuperem Events de la web
  Events := TEvents.GetEvents(Result, Season.any);
  if Result.connected = '' then // no hi ha connexió
    Exit;

  // transformem events de Google Calendar a TEvents
  TmpEvent := TEvents.Create;
  try
    for i := 0 to Cal.Count do
      TSeasonsFunc.ProcessGoogleCalendar(Season, Cal.Items[i], Datas[i], TmpEvent);

    // confrontem les dos llistes de TEvents i actualitzem servidor
    for i := 0 to TmpEvent.Count do
    begin
      Idx := Events.IndexOf(TmpEvent.Items[i].id_google, TmpEvent.Items[i].datai);
      if Idx < 0 then // si no existeix l'event
      begin
        // si és cancelació no fem res. Si no, cal afegir-lo
        if SameText(TmpEvent.Items[i].Status, cStatusConfirmed) then
        begin
          Result := TEvents.AddEvent(TmpEvent.Items[i]);
          if Result.connected = '' then // no hi ha connexió
            Exit;
        end;
      end
      else // ja existeix l'event
      begin
        // si está cancelat, l'esborrem
        if SameText(TmpEvent.Items[i].Status, cStatusCancelled) then
        begin
          Result := TEvents.DelEvent(Events.Items[Idx].id);
          if Result.connected = '' then // no hi ha connexió
            Exit;
        end
        else
        begin
          // si no són igual, modifiquem
          if not Events.Items[Idx].IsEqualTo(TmpEvent.Items[i]) then
          begin
            TmpEvent.Items[i].id := Events.Items[Idx].id;
            Result := TEvents.EditEvent(TmpEvent.Items[i], 2);
            if Result.connected = '' then // no hi ha connexió
              Exit;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(TmpEvent);
  end;
end;

{ TSeasonsFunc }

class function TSeasonsFunc.GetRules(RRule: string): TRRule;
var
  L: TStringList;
  i: Integer;
begin
  // el format de RRULE es de parelles de valos separats per ;
  //  per tant, posarem en un TStringList cada parella de valors
  L := TStringList.Create;
  L.Delimiter := ';';
  L.DelimitedText := RRule;
  L.Text := UpperCase(L.Text);

  Result.Clear;
  // ara recorrem el TStringList i en cada posició mirem quina opció tenim
  for i := 0 to L.Count - 1 do
  begin
    if Pos(cNodeRecFREQ, L[i]) > 0 then
      Result.Freq := TGenFunc.GetField(L[i], 2, '=');
    if Pos(cNodeRecCOUNT, L[i]) > 0 then
      Result.Count := TGenFunc.GetField(L[i], 2, '=');
    if Pos(cNodeRecUNTIL, L[i]) > 0 then
      Result.UntilDate := TGenFunc.GetField(L[i], 2, '=');
    if Pos(cNodeRecBYDAY, L[i]) > 0 then
      Result.ByDay := TGenFunc.GetField(L[i], 2, '=');
    if Pos(cNodeRecINTERVAL, L[i]) > 0 then
      Result.Interval := TGenFunc.GetField(L[i], 2, '=');
  end;
end;

class procedure TSeasonsFunc.InsertIntoEvents(GoogleItem: TGoogleItem; Any: string;
  DI, DF: TDate; HI, HF: TTime; var Events: TEvents; const Cal: TCalendar);
var
  ToSearch: string;
  Idx: Integer;
begin
  if GoogleItem.recurringEventId = '' then
    ToSearch := GoogleItem.id
  else
    ToSearch := GoogleItem.recurringEventId;

  // si no existeix (que no deuria), l'afegim
  Idx := Events.IndexOf(ToSearch, FormatDateTime('yyyy-mm-dd', DI));
  if Idx = -1 then
    Events.Add('0', // Id
               Any, // Any = IdTemporada
               Cal.Id, // IdCalendari
               GoogleItem.id, // IDGoogle
               GoogleItem.summary,
               GoogleItem.location,
               GoogleItem.status,
               FormatDateTime('hh:nn:ss', HI),
               FormatDateTime('hh:nn:ss', HF),
               FormatDateTime('yyyy-mm-dd', DI),
               FormatDateTime('yyyy-mm-dd', DF)
               )
  else
  begin
    Events.Items[Idx].Status := GoogleItem.status;
    Events.Items[Idx].descrip := GoogleItem.summary;
    Events.Items[Idx].datai := FormatDateTime('yyyy-mm-dd', DI);
    Events.Items[Idx].dataf := FormatDateTime('yyyy-mm-dd', DF);
    Events.Items[Idx].horai := FormatDateTime('hh:nn:ss', HI);
    Events.Items[Idx].horaf := FormatDateTime('hh:nn:ss', HF);
    Events.Items[Idx].lloc := GoogleItem.location;
  end;
end;

class procedure TSeasonsFunc.ProcessGoogleCalendar(const Season: TSeason;
  const Cal: TCalendar; const Data: TGoogleCal; var Events: TEvents);
var
  Rule: TRRule;
  i: Integer;
  DI, DF: TDate;
  HI, HF: TTime;
  DaysB: Integer;
begin
  if not Assigned(Season) or not Assigned(Cal) or not Assigned(Data) then
    Exit;

  for i := 0 to Data.Count do
  begin
    // desglossem RRULE si n'hi ha
    Rule.Clear;
    if Assigned(Data.items[i].recurrence) then
      Rule := TSeasonsFunc.GetRules(Data.items[i].recurrence[0]);

    // transformem camps data i hora
    if SameText(Data.items[i].status, cStatusConfirmed) then
    begin
      DI := TGenFunc.StringToDate(Data.items[i].start.dateTime + Data.items[i].start.date);    // formats possibles: 2019-08-03  o  2019-02-05T21:30:00+01:00
      DF := TGenFunc.StringToDate(Data.items[i].EndDate.dateTime + Data.items[i].EndDate.date);
      if Length(Data.items[i].EndDate.dateTime + Data.items[i].start.date) = 10 then
        DF := IncDay(DF, -1);
      DaysB := DaysBetween(DI, DF);
      HI := TGenFunc.StringToTime(Data.items[i].start.dateTime);
      HF := TGenFunc.StringToTime(Data.items[i].EndDate.dateTime);
    end
    else
    begin
      DI := TGenFunc.StringToDate(Data.items[i].originalStartTime.dateTime + Data.items[i].originalStartTime.date);
      DF := DI;
      DaysB := 0;
      HI := TGenFunc.StringToTime(Data.items[i].originalStartTime.dateTime);
      HF := HI;
    end;

    // si no hi ha RRULE, només cal controlar 1 dia
    if not Rule.HaveRRule then
      TSeasonsFunc.InsertIntoEvents(Data.items[i], Season.any, DI, DF, HI, HF, Events, Cal)
    else // hi ha RRULE
    begin
      while not Rule.IsFinished(DI) do
      begin
        TSeasonsFunc.InsertIntoEvents(Data.items[i], Season.any, DI, DF, HI, HF, Events, Cal);

        DI := Rule.NextDate(DI);
        DF := IncDay(DI, DaysB);
      end;
    end;
  end;
end;

{ TRRule }

class procedure TRRule.Clear;
var
  i: Integer;
begin
  FByDay := '';
  FUntilDate := '';
  FCount := '';
  FFreq := '';
  FInterval := '';

  FCountI := 0;
  FFreqs := fUndefined;
  for i := 1 to 7 do
    FByDayA[i] := False;
end;

class function TRRule.HaveRRule: Boolean;
begin
  Result := (FFreq <> '');
end;

class function TRRule.IsFinished(DateI: TDate): Boolean;
begin
  if FCount <> '' then // va per repeticions
    Result := FCountI > StrToInt(FCount)
  else // va per data
    Result := DateI > TGenFunc.StringToDate(FUntilDate, False);
end;

class function TRRule.NextDate(DateI: TDate): TDate;
  function GetNewIndex(aDate: TDate): Integer;
  var
    IdxWeek: Integer;
    i: Integer;
  begin
    // agafem el dia de la setmana
    IdxWeek := DayOfTheWeek(aDate);
    // mirem si en la setmana algun altre dia, posterior a aquest, està actiu
    Result := 0;
    for i := IdxWeek+1 to 7 do
      if FByDayA[i] then
      begin
        Result := i;
        Break;
      end;
  end;
var
  NewIdx: Integer;
begin
  case FFreqs of
    fYearly: Result := IncYear(DateI);
    fMontly: Result := IncMonth(DateI);
    fWeekly:
      begin
        if FByDay = '' then Result := IncWeek(DateI)
        else
        begin
          NewIdx := GetNewIndex(DateI); // busquem següent dia de la setmana activat
          // si NewIdx <> 0 vol dir que queda algun dia de la mateixa setmana per fer
          if NewIdx <> 0 then
            Result := IncDay(DateI, NewIdx - DayOfTheWeek(DateI))
          else // cal saltar a la setmana següent i agafar el primer dia activat d'aquesta
          begin
            DateI := StartOfTheWeek(IncWeek(DateI)); // anem al 1er dia de la següent setmana (dilluns següent, vamos)
            NewIdx := GetNewIndex(DateI);  // busquem primer dia de la setmana activat
            // si NewIdx <> 0 vol dir que queda algun dia de la mateixa setmana per fer
            if NewIdx <> 0 then
              Result := IncDay(DateI, NewIdx - DayOfTheWeek(DateI))
            else // seria un ERROR!!
              Result := IncDay(DateI);
          end;
        end;
      end;
    fDaily: Result := IncDay(DateI);
    fHourly: Result := DateI;
    fMinutely: Result := DateI;
    else Result := DateI;
  end;
  Inc(FCountI);
end;

class procedure TRRule.SetByDay(const Value: string);
var
  L: TStringList;
  i: Integer;
begin
  if FByDay = Value then
    Exit;
  FByDay := Value;

  if FByDay = '' then
    Exit;

  L := TStringList.Create;
  try
    L.CommaText := FByDay;
    for i := 0 to L.Count - 1 do
    begin
      if SameText(L[i], 'MO') then FByDayA[1] := True;
      if SameText(L[i], 'TU') then FByDayA[2] := True;
      if SameText(L[i], 'WE') then FByDayA[3] := True;
      if SameText(L[i], 'TH') then FByDayA[4] := True;
      if SameText(L[i], 'FR') then FByDayA[5] := True;
      if SameText(L[i], 'SA') then FByDayA[6] := True;
      if SameText(L[i], 'SU') then FByDayA[7] := True;
    end;
  finally
    FreeAndNil(L);
  end;
end;

class procedure TRRule.SetFreq(const Value: string);
begin
  if FFreq = Value then
    Exit;

  FFreq := Value;

  FFreqs := fUndefined;
  if SameText(FFreq, cFreqYearly) then
    FFreqs := fYearly;
  if SameText(FFreq, cFreqMontly) then
    FFreqs := fMontly;
  if SameText(FFreq, cFreqWeekly) then
    FFreqs := fWeekly;
  if SameText(FFreq, cFreqDaily) then
    FFreqs := fDaily;
  if SameText(FFreq, cFreqHourly) then
    FFreqs := fHourly;
  if SameText(FFreq, cFreqMinutely) then
    FFreqs := fMinutely;
end;

end.
