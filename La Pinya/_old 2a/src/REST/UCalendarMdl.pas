unit UCalendarMdl;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Client,
  REST.Response.Adapter,

  UClasses;

const
  cNodeItems = 'items';
  cNodeId = 'id';
  cNodeStatus = 'status';
  cStatusConfirmed = 'confirmed';
  cStatusCancelled = 'cancelled';
  cNodeSummary = 'summary';
  cNodeLocation = 'location';
  cNodeStart = 'start';
  cNodeEnd = 'end';
  cNodeRecurrence = 'recurrence';
  cNodeRecFREQ = 'FREQ';
  cNodeRecCOUNT = 'COUNT';
  cNodeRecUNTIL = 'UNTIL';
  cNodeRecBYDAY = 'BYDAY';
  cNodeRecINTERVAL = 'INTERVAL';
  cNodeRecurringEventId = 'recurringEventId';
  cNodeOriginalStartTime = 'originalStartTime';
  cNodeDateTime = 'dateTime';
  cNodeDate = 'date';

type
  TCalendarMdl = class(TDataModule)
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    RESTResponse1: TRESTResponse;
    cdsItems: TClientDataSet;
    cdsTemporades: TClientDataSet;
    cdsTmpEvents: TClientDataSet;
    cdsEvents: TClientDataSet;
    cdsTmpEventsid_cal: TIntegerField;
    cdsTmpEventsid_google: TStringField;
    cdsTmpEventsany: TIntegerField;
    cdsTmpEventsdatai: TStringField;
    cdsTmpEventshorai: TStringField;
    cdsTmpEventsstatus: TStringField;
    cdsTmpEventssummary: TStringField;
    cdsTmpEventslocation: TStringField;
    cdsTmpEventsdataf: TStringField;
    cdsTmpEventshoraf: TStringField;
    cdsResult: TClientDataSet;
  private
    FJSON: string;

    procedure ParseJSON(IdCal, Any: string);
    procedure ManageGoogleEvent(GEvent: TGoogleEvent; IdCal, Any: string);
    function GetRules(RRule: string): TRRule;
  public
    procedure GetCalendars;
    procedure DelCalendar(Id: string);
    procedure GetCalendarItems(IdCal, IdGoogle, Key, Any: string);
    procedure GetTemporades;
    procedure GetEventsBySeason(Season: string);
    procedure MergeEvents;

    property JSON: string read FJSON;
  end;

var
  CalendarMdl: TCalendarMdl;

implementation

uses
  System.JSON, System.DateUtils, System.Variants,
  URESTMdl, UMessage;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TCalendarMdl }

procedure TCalendarMdl.DelCalendar(Id: string);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('DelCalendar'));
    Obj.AddPair('id', TJSONString.Create(Id));

    TRESTMdl.GetRESTResponse('', cdsItems, Obj);
  finally
    FreeAndNil(Obj);
  end;
end;

procedure TCalendarMdl.GetCalendarItems(IdCal, IdGoogle, Key, Any: string);
const
  cGoogleCal = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?key=%s&timeMin=%s-01-01T00:00:00-01:00&timeMax=%s-12-31T00:00:00-01:00';
  cTimeMin = '&timeMin=%sT00:00:00-01:00';
  cTimeMax = '&timeMax=%sT00:00:00-01:00';
var
  Url: string;
begin
  Url := Format(cGoogleCal, [ IdGoogle, Key, Any, Any ]);

  FJSON := TRESTMdl.GetRESTResponse(Url, cdsItems, nil, '');
  ParseJSON(IdCal, Any);
end;

procedure TCalendarMdl.GetCalendars;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('GetCalendars'));

    TRESTMdl.GetRESTResponse('', cdsItems, Obj);
  finally
    FreeAndNil(Obj);
  end;
{const
  cParams = 'func=GetCalendars';
begin
  TRESTMdl.GetRESTResponse(cParams, cdsItems, nil);}
end;

procedure TCalendarMdl.GetEventsBySeason(Season: string);
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('GetEvents'));
    Obj.AddPair('any', TJSONString.Create(Season));

    TRESTMdl.GetRESTResponse('', cdsEvents, Obj);
  finally
    FreeAndNil(Obj);
  end;
{const
  cParams = 'func=GetEvents&any=%s';
begin
  TRESTMdl.GetRESTResponse(Format(cParams, [Season]), cdsEvents, nil);}

  if not cdsEvents.Active then
  begin
    cdsEvents.FieldDefs.Add('id', Data.DB.ftString, 10);
    cdsEvents.FieldDefs.Add('id_cal', Data.DB.ftString, 10);
    cdsEvents.FieldDefs.Add('id_google', Data.DB.ftString, 100);
    cdsEvents.FieldDefs.Add('descrip', Data.DB.ftString, 100);
    cdsEvents.FieldDefs.Add('any', Data.DB.ftString, 10);
    cdsEvents.FieldDefs.Add('datai', Data.DB.ftString, 20);
    cdsEvents.FieldDefs.Add('dataf', Data.DB.ftString, 20);
    cdsEvents.FieldDefs.Add('horai', Data.DB.ftString, 20);
    cdsEvents.FieldDefs.Add('horaf', Data.DB.ftString, 20);
    cdsEvents.FieldDefs.Add('lloc', Data.DB.ftString, 200);
    cdsEvents.FieldDefs.Add('modif', Data.DB.ftString, 1);
    cdsEvents.CreateDataSet;
  end;
end;

function TCalendarMdl.GetRules(RRule: string): TRRule;
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

procedure TCalendarMdl.GetTemporades;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('GetTemporades'));

    TRESTMdl.GetRESTResponse('', cdsTemporades, Obj);
  finally
    FreeAndNil(Obj);
  end;
{const
  cParams = 'func=GetTemporades';
begin
  TRESTMdl.GetRESTResponse(cParams, cdsTemporades, nil);}
end;

procedure TCalendarMdl.ManageGoogleEvent(GEvent: TGoogleEvent; IdCal, Any: string);
  procedure InsertIntoTmpEvents(Event: TGoogleEvent; IdCal, Any: string; DataI, DataF: TDate; HoraI, HoraF: TTime);
  begin
    // event actiu? afegim 1 registre
    if GEvent.Status = cStatusConfirmed then
    begin
      // si no existeix (que no deuria), l'afegim
      if not cdsTmpEvents.Locate('id_google;datai', VarArrayOf([Event.Id, DataI]), [loCaseInsensitive]) then
        cdsTmpEvents.AppendRecord([IdCal,
                                   Event.Id,
                                   Event.Status,
                                   Event.Summary,
                                   Any,
                                   FormatDateTime('yyyy-mm-dd', DataI),
                                   FormatDateTime('yyyy-mm-dd', DataF),
                                   FormatDateTime('hh:nn:ss', HoraI),
                                   FormatDateTime('hh:nn:ss', HoraF),
                                   Event.Location]);
    end
    else // event anulat?
    begin
      // si existeix (que hauria) el donem de baixa
      if cdsTmpEvents.Locate('id_google;datai', VarArrayOf([Event.RecurringEventId, DataI]), [loCaseInsensitive]) then
      begin
        cdsTmpEvents.Edit;
        cdsTmpEventsstatus.AsString := Event.Status;
        cdsTmpEvents.Post;
      end;
    end;
  end;
var
  Rule: TRRule;
  DI, DF: TDate;
  HI, HF: TTime;
  DaysB: Integer;
begin
  if not cdsTmpEvents.Active then
    cdsTmpEvents.CreateDataSet;

  // desglossem RRULE si n'hi ha
  Rule.Clear;
  if GEvent.RRule <> '' then
    Rule := GetRules(GEvent.RRule);

  // transformem camps data i hora
  DI := TGenFunc.StringToDate(GEvent.StartDate);
  DF := TGenFunc.StringToDate(GEvent.EndDate);
  if Length(GEvent.EndDate) = 10 then
    DF := IncDay(DF, -1);
  DaysB := DaysBetween(DI, DF);
  HI := TGenFunc.StringToTime(GEvent.StartDate);
  HF := TGenFunc.StringToTime(GEvent.EndDate);

  // si no hi ha RRULE, només cal controlar 1 dia
  if not Rule.HaveRRule then
  begin
    InsertIntoTmpEvents(GEvent, IdCal, Any, DI, DF, HI, HF);
  end
  else // hi ha RRULE
  begin
    while not Rule.IsFinished(DI) do
    begin
      InsertIntoTmpEvents(GEvent, IdCal, Any, DI, DF, HI, HF);

      DI := Rule.NextDate(DI);
      DF := IncDay(DI, DaysB);
    end;
  end;

  cdsTmpEvents.IndexName := 'Idx';
end;

procedure TCalendarMdl.MergeEvents;
var
  Obj: TJSONObject;
begin
  if not cdsEvents.Active or not cdsTmpEvents.Active then Exit;

  // primer mirem events de Google Calendar => CdA
  cdsTmpEvents.First;
  while not cdsTmpEvents.Eof do
  begin
    if not cdsEvents.Locate('id_google;datai', VarArrayOf([cdsTmpEventsid_google.AsString, cdsTmpEventsdatai.AsString]), [loCaseInsensitive]) then
    begin // afegim.... si cal
      if cdsTmpEventsstatus.AsString = cStatusConfirmed then
      begin
        cdsEvents.Append;
        cdsEvents.FieldByName('modif').AsString := '1';
      end;
    end
    else  // està en CdA, per tant => modificar o esborrar.... si cal
    begin
      if cdsTmpEventsstatus.AsString = cStatusCancelled then
      begin // cal esborrar
        cdsEvents.Edit;
        cdsEvents.FieldByName('modif').AsString := '3';
      end
      else
      begin  // cal modificar
        if (cdsEvents.FieldByName('id_cal').AsString <> cdsTmpEventsid_cal.AsString) or
           (cdsEvents.FieldByName('id_google').AsString <> cdsTmpEventsid_google.AsString) or
           (cdsEvents.FieldByName('any').AsString <> cdsTmpEventsany.AsString) or
           (cdsEvents.FieldByName('descrip').AsString <> cdsTmpEventssummary.AsString) or
           (cdsEvents.FieldByName('datai').AsString <> cdsTmpEventsdatai.AsString) or
           (cdsEvents.FieldByName('dataf').AsString <> cdsTmpEventsdataf.AsString) or
           (cdsEvents.FieldByName('horai').AsString <> cdsTmpEventshorai.AsString) or
           (cdsEvents.FieldByName('horaf').AsString <> cdsTmpEventshoraf.AsString) or
           (cdsEvents.FieldByName('lloc').AsString <> cdsTmpEventslocation.AsString) then
        begin
          cdsEvents.Edit;
          cdsEvents.FieldByName('modif').AsString := '2';
        end;
      end;
    end;

    if cdsEvents.State in [dsEdit, dsInsert] then
    begin
      cdsEvents.FieldByName('id_cal').AsString := cdsTmpEventsid_cal.AsString;
      cdsEvents.FieldByName('id_google').AsString := cdsTmpEventsid_google.AsString;
      cdsEvents.FieldByName('any').AsString := cdsTmpEventsany.AsString;
      cdsEvents.FieldByName('descrip').AsString := cdsTmpEventssummary.AsString;
      cdsEvents.FieldByName('datai').AsString := cdsTmpEventsdatai.AsString;
      cdsEvents.FieldByName('dataf').AsString := cdsTmpEventsdataf.AsString;
      cdsEvents.FieldByName('horai').AsString := cdsTmpEventshorai.AsString;
      cdsEvents.FieldByName('horaf').AsString := cdsTmpEventshoraf.AsString;
      cdsEvents.FieldByName('lloc').AsString := cdsTmpEventslocation.AsString;
      cdsEvents.Post;
    end;

    cdsTmpEvents.Next;
  end;

  // Ara events de CdA => Google Calendar
  cdsEvents.First;
  while not cdsEvents.Eof do
  begin
    // només cal mirar si s'ha esborrat, perquè afegir i modificar ja s'ha fet en el bucle anterior
    if not cdsTmpEvents.Locate('id_google;datai', VarArrayOf([cdsEvents.FieldByName('id_google').AsString, cdsEvents.FieldByName('datai').AsString]), [loCaseInsensitive]) then
    begin
      cdsEvents.Edit;
      cdsEvents.FieldByName('modif').AsString := '3';
      cdsEvents.Post;
    end;

    cdsEvents.Next;
  end;

  // I ara actualitzem servidor amb les dades corresponents
  cdsEvents.First;
  while not cdsEvents.Eof do
  begin

    Obj := TJSONObject.Create;
    try
      Obj.AddPair('func', TJSONString.Create('UpdateEvents'));
      Obj.AddPair('id', TJSONString.Create(cdsEvents.FieldByName('id').AsString));
      Obj.AddPair('id_cal', TJSONString.Create(cdsEvents.FieldByName('id_cal').AsString));
      Obj.AddPair('id_google', TJSONString.Create(cdsEvents.FieldByName('id_google').AsString));
      Obj.AddPair('any', TJSONString.Create(cdsEvents.FieldByName('any').AsString));
      Obj.AddPair('descrip', TJSONString.Create(cdsEvents.FieldByName('descrip').AsString));
      Obj.AddPair('datai', TJSONString.Create(cdsEvents.FieldByName('datai').AsString));
      Obj.AddPair('dataf', TJSONString.Create(cdsEvents.FieldByName('dataf').AsString));
      Obj.AddPair('horai', TJSONString.Create(cdsEvents.FieldByName('horai').AsString));
      Obj.AddPair('horaf', TJSONString.Create(cdsEvents.FieldByName('horaf').AsString));
      Obj.AddPair('lloc', TJSONString.Create(cdsEvents.FieldByName('lloc').AsString));
      Obj.AddPair('modif', TJSONString.Create(cdsEvents.FieldByName('modif').AsString));

      TRESTMdl.GetRESTResponse('', cdsResult, Obj);
    finally
      FreeAndNil(Obj);
    end;

    cdsEvents.Next;
  end;
end;

procedure TCalendarMdl.ParseJSON(IdCal, Any: string);
  function GetDateFromJSON(const JObject: TJSONObject; Node: string): string;
  var
    JVal: TJSONValue;
    JObj: TJSONObject;
  begin
    Result := '';
    JVal := JObject.GetValue(Node);
    JObj := JVal as TJSONObject;
    JVal := JObj.GetValue(cNodeDateTime);
    if Assigned(JVal) then
      Result := JVal.Value;
    JVal := JObj.GetValue(cNodeDate);
    if Assigned(JVal) then
      Result := JVal.Value;
  end;
var
  JO: TJSONObject;
  JA: TJSONArray;
  JV1, JV2: TJSONValue;
  GEvent: TGoogleEvent;
begin
  GEvent := TGoogleEvent.Create;

  try
    JO := TJSONObject.ParseJSONValue(BytesOf(UTF8Encode(FJSON)), 0) as TJSONObject;

    // agafem l'array "items"
    JA := JO.GetValue(cNodeItems) as TJSONArray;

    // recorrem array
    for JV1 in JA do
    begin
      GEvent.Clear;

      JO := JV1 as TJSONObject;
      // de cada posició de l'array, ens interessen les següents posicions:
      // - id
      JV2 := JO.GetValue(cNodeId);
      GEvent.Id := JV2.Value;
      // - status
      JV2 := JO.GetValue(cNodeStatus);
      GEvent.Status := JV2.Value;

      if JV2.Value = cStatusConfirmed then
      begin
        // - summary
        JV2 := JO.GetValue(cNodeSummary);
        GEvent.Summary := JV2.Value;
        // - location
        JV2 := JO.GetValue(cNodeLocation);
        if Assigned(JV2) then
        GEvent.Location := JV2.Value;
        // - start
        GEvent.StartDate := GetDateFromJSON(JO, cNodeStart);
        // - end
        GEvent.EndDate := GetDateFromJSON(JO, cNodeEnd);

        // - recurrence
        JV2 := JO.GetValue(cNodeRecurrence);
        if Assigned(JV2) then
          GEvent.RRule := TGenFunc.GetField((JV2 as TJSONArray).Items[0].Value, 2, ':');
      end
      else
      begin
        // - recurringEventId
        JV2 := JO.GetValue(cNodeRecurringEventId);
        GEvent.RecurringEventId := JV2.Value;

        // - originalStartTime
        GEvent.StartDate := GetDateFromJSON(JO, cNodeOriginalStartTime);
        GEvent.EndDate := GEvent.StartDate;
      end;

      ManageGoogleEvent(GEvent, IdCal, Any);
    end;
  finally
    FreeAndNil(GEvent);
  end;
end;

end.
