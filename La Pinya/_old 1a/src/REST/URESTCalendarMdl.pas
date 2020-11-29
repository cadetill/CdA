unit URESTCalendarMdl;
//https://developers.google.com/google-apps/calendar/v3/reference/events/list
interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, FMX.StdCtrls,
  REST.Response.Adapter, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Client;

type
  TRESTCalendarMdl = class(TDataModule)
    cdsCalendars: TClientDataSet;
    cdsCalendar: TClientDataSet;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    cdsItems: TClientDataSet;
    RESTResponse1: TRESTResponse;
  private
    FCalendarGet: Boolean;
    FFinish: Boolean;

    procedure OnThreadTerminate(Sender: TObject);
    procedure OnThreadCalTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    class procedure GetCalendar(DataSet: TDataSet; OnTerminateEvent: TNotifyEvent;
      AniInd: TAniIndicator; Button: TCustomButton; MinDay, MaxDay: TDate; MaxResults: Integer = 0);

    property CalendarsGet: Boolean read FCalendarGet;
    property Finish: Boolean read FFinish write FFinish;
  end;

  TRESTCalThread = class(TThread)
  private
    FDataSet: TDataSet;
    FAniInd: TAniIndicator;
    FButton: TCustomButton;
    FMaxResults: Integer;
    FMinDay: TDate;
    FMaxDay: TDate;
  protected
    procedure Execute; override;

    procedure StartLoading;
    procedure EndLoading;
    procedure InitializeDataSet;
    procedure FillDataSet(Origen, Calendars: TDataSet);
    function GetDateTime(JSON: string; Start: Boolean = False): TDateTime;
    function GetField(S: string; FieldIndex: integer; Delimiter: Char): string;
  public
    constructor Create(CreateSuspended: Boolean; DataSet: TDataSet; AniInd: TAniIndicator;
      Button: TCustomButton; MinDay, MaxDay: TDate; MaxResults: Integer = 0); virtual;

    property Button: TCustomButton read FButton;
  end;

var
  RESTCalendarMdl: TRESTCalendarMdl;

implementation

uses
  System.JSON, System.DateUtils,
  URESTDataMdl;

const
  ctConfirmed = 'confirmed';

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TRESTCalendarMdl }

constructor TRESTCalendarMdl.Create(AOwner: TComponent);
begin
  inherited;

  FCalendarGet := False;
  FFinish := False;
end;

class procedure TRESTCalendarMdl.GetCalendar(DataSet: TDataSet;
  OnTerminateEvent: TNotifyEvent; AniInd: TAniIndicator; Button: TCustomButton;
  MinDay, MaxDay: TDate; MaxResults: Integer = 0);
begin
  with TRESTCalThread.Create(True, DataSet, AniInd, Button, MinDay, MaxDay, MaxResults) do
  begin
    OnTerminate := OnTerminateEvent;
    Sleep(100);
    FreeOnTerminate := True;
    Sleep(100);
    Start;
  end;
end;

procedure TRESTCalendarMdl.OnThreadCalTerminate(Sender: TObject);
begin
  // tindrem el calendari
  FFinish := True;
end;

procedure TRESTCalendarMdl.OnThreadTerminate(Sender: TObject);
begin
  // tindrem els calendaris
  FCalendarGet := True;
end;

{ TRESTCalThread }

constructor TRESTCalThread.Create(CreateSuspended: Boolean; DataSet: TDataSet;
  AniInd: TAniIndicator; Button: TCustomButton; MinDay, MaxDay: TDate;
  MaxResults: Integer);
begin
  inherited Create(CreateSuspended);

  FDataSet := DataSet;
  FAniInd := AniInd;
  FButton := Button;
  FMaxResults := MaxResults;
  FMinDay := MinDay;
  FMaxDay := MaxDay;
end;

procedure TRESTCalThread.EndLoading;
begin
  if not Assigned(FAniInd) then Exit;

  FAniInd.Visible := False;
  FAniInd.Enabled := False;
end;

procedure TRESTCalThread.Execute;
const
  cGoogleCal = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?key=%s';
  cTimeMin = '&timeMin=%sT00:00:00-01:00';
  cTimeMax = '&timeMax=%sT00:00:00-01:00';
  cResults = '&maxResults=%s';
var
  Mdl: TRESTCalendarMdl;
  Url: string;
begin
  inherited;

  Synchronize(StartLoading);

  InitializeDataSet;

  Mdl := TRESTCalendarMdl.Create(nil);
  try
    TRESTDataMdl.GetRESTResponse('func=get_calendars', Mdl.cdsCalendars , Mdl.OnThreadTerminate, nil, FButton);
    repeat
      Sleep(10);
      // do nothing!! Like Windows :-P   jejejeje
      // simplement esperem a que acabi el Thread que recupera els calendaris
    until Mdl.CalendarsGet;

    Mdl.cdsCalendars.First;
    while not Mdl.cdsCalendars.Eof do
    begin
      Mdl.cdsCalendar.Close;
      Url := Format(cGoogleCal, [Mdl.cdsCalendars.FieldByName('idcalendar').AsString,
                                 Mdl.cdsCalendars.FieldByName('key').AsString]);
      if FMinDay > EncodeDate(2015,1,1) then
        Url := Url + Format(cTimeMin, [FormatDateTime('yyyy-mm-dd', FMinDay)]);
      if (FMaxDay > EncodeDate(2015,1,1)) and (FMaxDay > FMinDay) then
        Url := Url + Format(cTimeMax, [FormatDateTime('yyyy-mm-dd', FMaxDay)]);
      if FMaxResults > 0 then
        Url := Url + Format(cResults, [IntToStr(FMaxResults)]);

      Mdl.FFinish := False;
      TRESTDataMdl.GetRESTResponse(Url, Mdl.cdsCalendar, Mdl.OnThreadCalTerminate, nil, nil, 'items');
      repeat
        Sleep(10);
        // do nothing!! Like Windows :-P   jejejeje
        // simplement esperem a que acabi el Thread que recupera els calendaris
      until Mdl.Finish;

      FillDataSet(Mdl.cdsCalendar, Mdl.cdsCalendars);

      Mdl.cdsCalendars.Next;
    end;
  finally
    FreeAndNil(Mdl);
  end;

  Synchronize(EndLoading);
end;

procedure TRESTCalThread.FillDataSet(Origen, Calendars: TDataSet);
begin
  Origen.First;
  while not Origen.Eof do
  begin
    if SameText(Origen.FieldByName('status').AsString, ctConfirmed) then
    begin
      FDataSet.Append;
      FDataSet.FieldByName('idcal').AsInteger := Calendars.FieldByName('id').AsInteger;
      FDataSet.FieldByName('status').AsString := Origen.FieldByName('status').AsString;
      FDataSet.FieldByName('etag').AsString := Origen.FieldByName('etag').AsString;
      FDataSet.FieldByName('id').AsString := Origen.FieldByName('id').AsString;
      FDataSet.FieldByName('htmlLink').AsString := Origen.FieldByName('htmlLink').AsString;
      FDataSet.FieldByName('summary').AsString := Origen.FieldByName('summary').AsString;
      FDataSet.FieldByName('start').AsDateTime := GetDateTime(Origen.FieldByName('start').AsString, True);
      FDataSet.FieldByName('end').AsDateTime := GetDateTime(Origen.FieldByName('end').AsString);
      FDataSet.Post;
    end;

    Origen.Next;
  end;
end;

function TRESTCalThread.GetDateTime(JSON: string; Start: Boolean): TDateTime;
var
  Field: string;
  Value: string;
begin
  Result := EncodeDate(1900,1,1);

  JSON := StringReplace(JSON, '{', '', [rfReplaceAll, rfIgnoreCase]);
  JSON := StringReplace(JSON, '}', '', [rfReplaceAll, rfIgnoreCase]);
  JSON := StringReplace(JSON, '"', '', [rfReplaceAll, rfIgnoreCase]);

  Field := GetField(JSON, 1, ':');
  Value := GetField(JSON, 2, ':');
  if SameText(Field, 'datetime') then
  begin
    Value := Copy(JSON, Length('datetime:')+1, Length(JSON)-Length('datetime:'));

    Result := EncodeDate(StrToInt(GetField(GetField(Value, 1, 'T'), 1, '-')),
                         StrToInt(GetField(GetField(Value, 1, 'T'), 2, '-')),
                         StrToInt(GetField(GetField(Value, 1, 'T'), 3, '-')));

    Value := GetField(Value, 2, 'T');
    Value := GetField(Value, 1, '+');

    Result := Result + StrToTime(Value);

    Exit;
  end;

  if SameText(Field, 'date') then
  begin
    Result := EncodeDate(StrToInt(GetField(Value, 1, '-')), StrToInt(GetField(Value, 2, '-')), StrToInt(GetField(Value, 3, '-')));
    if Start then Result := IncDay(Result);

    Exit;
  end;
end;

function TRESTCalThread.GetField(S: string; FieldIndex: integer;
  Delimiter: Char): string;
var
  DelimiterPos: integer;
  loopCount: integer;
  sRecord,
  sField: string;
begin
  loopCount := 1;
  sRecord := S;
  while loopCount <= FieldIndex do
  begin
    DelimiterPos := Pos(Delimiter, sRecord);
    if DelimiterPos <> 0 then
    begin
      sField := Copy(sRecord, 1, DelimiterPos - 1);
      Delete(sRecord, 1, DelimiterPos);
    end
    else sField := sRecord;
    loopCount := loopCount + 1;
  end;
  Result := sField;
end;

procedure TRESTCalThread.InitializeDataSet;
begin
  if FDataSet is TClientDataSet then
  begin
    with TClientDataSet(FDataSet) do
    begin
      if Active then Close;
      if IndexDefs.Count <> 0 then IndexDefs.Clear;
      if FieldDefs.Count <> 0 then FieldDefs.Clear;
      FieldDefs.Add('idcal', TFieldType.ftInteger, 0);
      FieldDefs.Add('etag', TFieldType.ftString, 30);
      FieldDefs.Add('id', TFieldType.ftString, 50);
      FieldDefs.Add('status', TFieldType.ftString, 15);
      FieldDefs.Add('htmlLink', TFieldType.ftString, 200);
      FieldDefs.Add('summary', TFieldType.ftString, 100);
      FieldDefs.Add('start', TFieldType.ftDateTime, 0);
      FieldDefs.Add('end', TFieldType.ftDateTime, 0);
      CreateDataSet;
      IndexFieldNames := 'start';
    end;
  end
  else
  begin
    if FDataSet.Active then
    begin
      FDataSet.First;
      while not FDataSet.Eof do FDataSet.Delete;
    end;

    FDataSet.Open;
  end;
end;

procedure TRESTCalThread.StartLoading;
begin
  if not Assigned(FAniInd) then Exit;

  FAniInd.Visible := True;
  FAniInd.Enabled := True;
end;

end.
