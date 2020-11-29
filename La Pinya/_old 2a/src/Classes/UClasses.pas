unit UClasses;

interface

uses
  FMX.Objects, FMX.StdCtrls, FMX.Effects, FMX.Types, FMX.Graphics,
  System.Classes, System.UITypes,
  USets;

const
  cFreqYearly = 'YEARLY';
  cFreqMontly = 'MONTHLY';
  cFreqWeekly = 'WEEKLY';
  cFreqDaily = 'DAILY';
  cFreqHourly = 'HOURLY';
  cFreqMinutely = 'MINUTELY';

type
  TUser = record
  private
    class var
      FActCode: string;
      FId: Integer;
      FRols: TRols;
  public
    class procedure Clear; static;
    class function IsAdmin: Boolean; static;
    class function IsTecnica: Boolean; static;
    class function IsJunta: Boolean; static;
    class function IsCasteller: Boolean; static;
    class function IsConvidat: Boolean; static;
    class function IsSanitari: Boolean; static;
    class function IsCommunication: Boolean; static;

    class property ActCode: string read FActCode write FActCode;
    class property Id: Integer read FId write FId;
    class property Rols: TRols read FRols write FRols;
  end;

  TGenFunc = record
  public
    class function GetField(S: string; FieldIndex: integer; Delimiter: Char): string; static;

    class function StringToDate(Value: string; WithSeparator: Boolean = True): TDate; static;
    class function StringToTime(Value: string): TTime; static;
    class function StringToURL(Value: string): string; static;

    class function GetIniName: string; static;

    class function GetDeviceId: string; static;
    class function GetOperatingSystem: string; static;

    class function MeasureTextHeight(const AFont: TFont; const AText: string): Single; static;
  end;

  TAnimation = class(TRectangle)
    FAniIndicator: TAniIndicator;
  private
    FShadow: TShadowEffect;
    FAnimate: Boolean;
    FText: TLabel;

    procedure SetAnimate(const Value: Boolean);
  public
    property Animate: Boolean read FAnimate write SetAnimate;
    property Text: TLabel read FText write FText;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TItemCal = class
  private
    FKey: string;
    FIdCalendar: string;
    FNom: string;
    FId: string;
  public
    property Id: string read FId write FId;
    property IdCalendar: string read FIdCalendar write FIdCalendar;
    property Key: string read FKey write FKey;
    property Nom: string read FNom write FNom;
  end;

  TAssistencia = record
  private
    class var FIdEvent: string;
    class var FAssist: string;
    class var FIdSoci: string;
    class var FBus: string;
    class procedure SetAssist(const Value: string); static;
    class procedure SetBus(const Value: string); static;
    class procedure SetIdEvent(const Value: string); static;
    class procedure SetIdSoci(const Value: string); static;
  public
    class procedure Clear; static;

    class property IdSoci: string read FIdSoci write SetIdSoci;
    class property IdEvent: string read FIdEvent write SetIdEvent;
    class property Bus: string read FBus write SetBus;
    class property Assist: string read FAssist write SetAssist;
  end;

  TSoci = class;

  TItemEvent = class
  private
    FDatai: string;
    FModif: string;
    FHorai: string;
    FCotxe: string;
    FDataf: string;
    FBusd: string;
    FDescrip: string;
    FId: string;
    FHoraf: string;
    FBuslloc: string;
    FQuedar: string;
    FQuedarh: string;
    FLloc: string;
    FId_google: string;
    FId_cal: string;
    FNom_curt: string;
    FBus: string;
    FBush: string;
    FAny: string;
    FColles: string;
    FSoci: TSoci;
  public
    procedure Clear;
    function BusdToDate: TDate;
    function BushToTime: TTime;
    function DataiToDate: TDate;
    function DatafToDate: TDate;
    function HoraiToTime: TTime;
    function HorafToTime: TTime;
    function QuedarhToTime: TTime;
    function IdCalToInt: Integer;

    constructor Create; virtual;
    destructor Destroy; override;

    property Id: string read FId write FId;
    property Id_cal: string read FId_cal write FId_cal;
    property Id_google: string read FId_google write FId_google;
    property Any: string read FAny write FAny;
    property Descrip: string read FDescrip write FDescrip;
    property Cotxe: string read FCotxe write FCotxe;
    property Bus: string read FBus write FBus;
    property Busd: string read FBusd write FBusd;
    property Bush: string read FBush write FBush;
    property Buslloc: string read FBuslloc write FBuslloc;
    property Datai: string read FDatai write FDatai;
    property Dataf: string read FDataf write FDataf;
    property Horai: string read FHorai write FHorai;
    property Horaf: string read FHoraf write FHoraf;
    property Lloc: string read FLloc write FLloc;
    property Quedar: string read FQuedar write FQuedar;
    property Quedarh: string read FQuedarh write FQuedarh;
    property Nom_curt: string read FNom_curt write FNom_curt;
    property Modif: string read FModif write FModif;
    property Colles: string read FColles write FColles;
    property Soci: TSoci read FSoci write FSoci;
  end;

  TItemNews = class;

  TOnChangeItemNews = procedure (Sender: TItemNews) of object;

  TItemNews = class
  private
    FTitol: string;
    FHora: string;
    FPublicar: string;
    FId: string;
    FContingut: string;
    FPubDate: string;
    FIndexLB: Integer;
    FOnChange: TOnChangeItemNews;
    procedure SetPublicar(const Value: string);
  public
    constructor Create(OnChangeEvent: TOnChangeItemNews); virtual;

    property Id: string read FId write FId;
    property Titol: string read FTitol write FTitol;
    property Contingut: string read FContingut write FContingut;
    property Publicar: string read FPublicar write SetPublicar;
    property Hora: string read FHora write FHora;
    property PubDate: string read FPubDate;
    property IndexLB: Integer read FIndexLB write FIndexLB;
    property OnChange: TOnChangeItemNews read FOnChange write FOnChange;
  end;

  TItemConfig = class;

  TOnChangeItemConfig = procedure (Sender: TItemConfig) of object;

  TItemConfig = class
  private
    FOnChange: TOnChangeItemConfig;
    FActCode: string;
  public
    constructor Create(OnChangeEvent: TOnChangeItemConfig); virtual;

    property OnChange: TOnChangeItemConfig read FOnChange write FOnChange;
    property ActCode: string read FActCode write FActCode;
  end;

  TSoci = class
  private
    FNom: string;
    FActCode: string;
  public
    procedure Assign(Source: TObject); virtual;

    property ActCode: string read FActCode write FActCode;
    property Nom: string read FNom write FNom;
  end;

  TTemporada = class
  private
    FDescripcio: string;
    FAny: string;
  public
    property Any: string read FAny write FAny;
    property Descripcio: string read FDescripcio write FDescripcio;
  end;

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

  TGoogleEvent = class
  private
    FEndDate: string;
    FId: string;
    FStatus: string;
    FStartDate: string;
    FRecurringEventId: string;
    FStartHour: string;
    FEndHour: string;
    FSummary: string;
    FLocation: string;
    FRRule: string;
  public
    function ToStr: string;
    procedure Clear;

    property Id: string read FId write FId;
    property Status: string read FStatus write FStatus;
    property Summary: string read FSummary write FSummary;
    property Location: string read FLocation write FLocation;
    property StartDate: string read FStartDate write FStartDate;
    property StartHour: string read FStartHour write FStartHour;
    property EndDate: string read FEndDate write FEndDate;
    property EndHour: string read FEndHour write FEndHour;
    property RRule: string read FRRule write FRRule;
    property RecurringEventId: string read FRecurringEventId write FRecurringEventId;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, System.IOUtils,
  FMX.TextLayout;

{ TAnimation }

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAniIndicator := TAniIndicator.Create(Self);
  FAniIndicator.Align := TAlignLayout.Client;
  FAniIndicator.StyleLookup := 'aniindicatorstyle';
  FAniIndicator.Parent := Self;

  FText := TLabel.Create(Self);
  FText.Align := TAlignLayout.Bottom;
  FText.Text := 'Carregant...';
  FText.TextAlign := TTextAlign.Center;
  FText.Parent := Self;

  Width := 150;
  Height := 100;
  Stroke.Thickness := 0;
  Fill.Color := 4292927712;

  Padding.Left := 5;
  Padding.Bottom := 5;
  Padding.Top := 5;
  Padding.Right := 5;

  XRadius := 10;
  YRadius := 10;

  FShadow := TShadowEffect.Create(Self);
  FShadow.Distance := 1;
  FShadow.Direction := 90;
  FShadow.Softness := 0.100000001490116100;
  FShadow.Opacity := 0.699999988079071100;
  FShadow.ShadowColor := TAlphaColorRec.Black;
  FShadow.Parent := Self;
  FShadow.Enabled := True;
end;

destructor TAnimation.Destroy;
begin
  FShadow.DisposeOf;
  FAniIndicator.DisposeOf;
  FText.DisposeOf;

  inherited;
end;

procedure TAnimation.SetAnimate(const Value: Boolean);
begin
  FAnimate := Value;
  FAniIndicator.Enabled := Value;
  FAniIndicator.Visible := Value;
  Self.BringToFront;
end;

{ TGoogleEvent }

procedure TGoogleEvent.Clear;
begin
  FId := '';
  FStatus := '';
  FSummary := '';
  FLocation := '';
  FStartDate := '';
  FStartHour := '';
  FEndDate := '';
  FEndHour := '';
  FRRule := '';
  FRecurringEventId := '';
end;

function TGoogleEvent.ToStr: string;
const
  cPar = '%s;%s;%s;%s;%s;%s;%s;%s;%s;%s';
begin
  Result := Format(cPar, [FId,
                          FStatus,
                          FSummary,
                          FLocation,
                          FStartDate,
                          FStartHour,
                          FEndDate,
                          FEndHour,
                          FRRule,
                          FRecurringEventId]);
end;

{ TRRule }

class procedure TRRule.Clear;
begin
  FByDay := '';
  FUntilDate := '';
  FCount := '';
  FFreq := '';
  FInterval := '';

  FCountI := 0;
  FFreqs := fUndefined;
  FByDayA[1] := False;
  FByDayA[2] := False;
  FByDayA[3] := False;
  FByDayA[4] := False;
  FByDayA[5] := False;
  FByDayA[6] := False;
  FByDayA[7] := False;
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
  if FByDay = Value then Exit;
  FByDay := Value;

  if FByDay = '' then Exit;

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
  if FFreq = Value then Exit;

  FFreq := Value;

  FFreqs := fUndefined;
  if SameStr(FFreq, cFreqYearly) then
    FFreqs := fYearly;
  if SameStr(FFreq, cFreqMontly) then
    FFreqs := fMontly;
  if SameStr(FFreq, cFreqWeekly) then
    FFreqs := fWeekly;
  if SameStr(FFreq, cFreqDaily) then
    FFreqs := fDaily;
  if SameStr(FFreq, cFreqHourly) then
    FFreqs := fHourly;
  if SameStr(FFreq, cFreqMinutely) then
    FFreqs := fMinutely;
end;

{ TGenFunc }

class function TGenFunc.GetDeviceId: string;
var
  Guid: TGUID;
begin
  if CreateGUID(Guid) <> 0 then
     Result := FormatDateTime('yyyymmddhhnnss', Now)
  else
     Result := GUIDToString(Guid);
end;

class function TGenFunc.GetField(S: string; FieldIndex: integer;
  Delimiter: Char): string;
var
  DelimiterPos: Integer;
  loopCount: Integer;
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

class function TGenFunc.GetIniName: string;
begin
  {$IFDEF MSWINDOWS}
    Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(ParamStr(0))) + 'Config.ini';
  {$ELSE}
    {$IFDEF ANDROID}
      Result := TPath.Combine(TPath.GetDocumentsPath, IncludeTrailingPathDelimiter('CdAPP') + 'Config.ini');
    {$ELSE}
      Result := TPath.Combine(TPath.GetSharedDownloadsPath, IncludeTrailingPathDelimiter('CdAPP') + 'Config.ini');
    {$ENDIF}
  {$ENDIF}
  if not TDirectory.Exists(TPath.GetDirectoryName(Result)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(Result))
end;

class function TGenFunc.GetOperatingSystem: string;
begin
  Result := TOSVersion.ToString;
end;

class function TGenFunc.MeasureTextHeight(const AFont: TFont;
  const AText: string): Single;
var
  LLayout: TTextLayout;
begin
  LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    try
      LLayout.WordWrap := False;
      LLayout.Font.Assign(AFont);
      LLayout.Text := AText;
    finally
      LLayout.EndUpdate;
    end;
    Result := LLayout.TextHeight;
  finally
    LLayout.Free;
  end;
end;

class function TGenFunc.StringToDate(Value: string; WithSeparator: Boolean): TDate;
var
  TmpI: Integer;
begin
  Result := EncodeDate(1900,1,1);

  if Value = '' then Exit;

  TmpI := 10;
  if not WithSeparator then TmpI := 8;
  if Length(Value) < TmpI then Exit;

  Value := Copy(Value, 1, TmpI);

  if WithSeparator then
    Result := EncodeDate(StrToInt(Copy(Value, 1, 4)), StrToInt(Copy(Value, 6, 2)), StrToInt(Copy(Value, 9, 2)))
  else
    Result := EncodeDate(StrToInt(Copy(Value, 1, 4)), StrToInt(Copy(Value, 5, 2)), StrToInt(Copy(Value, 7, 2)));
end;

class function TGenFunc.StringToTime(Value: string): TTime;
begin
  Result := EncodeTime(0,0,0,0);
  if Value = '' then Exit;
  case Length(Value) of
    19:
    begin
      Value := Copy(Value, 12, 8);
      Result := EncodeTime(StrToInt(GetField(Value, 1, ':')), StrToInt(GetField(Value, 2, ':')), StrToInt(GetField(Value, 3, ':')), 0);
    end;
    8:
    begin
      Result := EncodeTime(StrToInt(GetField(Value, 1, ':')), StrToInt(GetField(Value, 2, ':')), StrToInt(GetField(Value, 3, ':')), 0);
    end;
  end;
end;

class function TGenFunc.StringToURL(Value: string): string;
begin
  Result := StringReplace(Value, ' ', '%20', [rfReplaceAll]);
  Result := StringReplace(Value, '''', '´', [rfReplaceAll]);
end;

{ TUser }

class procedure TUser.Clear;
begin
  FActCode := '';
  FRols := [];
end;

class function TUser.IsAdmin: Boolean;
begin
  Result := rAdmin in FRols;
end;

class function TUser.IsCasteller: Boolean;
begin
  Result := rCasteller in FRols;
end;

class function TUser.IsCommunication: Boolean;
begin
  Result := rComunicacio in FRols;
end;

class function TUser.IsConvidat: Boolean;
begin
  Result := rConvidat in FRols;
end;

class function TUser.IsJunta: Boolean;
begin
  Result := rJunta in FRols;
end;

class function TUser.IsSanitari: Boolean;
begin
  Result := rSanitari in FRols;
end;

class function TUser.IsTecnica: Boolean;
begin
  Result := rTecnica in FRols;
end;

{ TItemNews }

constructor TItemNews.Create(OnChangeEvent: TOnChangeItemNews);
begin
  FIndexLB := -1;
  FOnChange := OnChangeEvent;
end;

procedure TItemNews.SetPublicar(const Value: string);
begin
  if FPublicar = Value then Exit;

  FPublicar := Value;
  FPubDate := '';
  if Length(FPublicar) = 10 then
    FPubDate := Copy(FPublicar, 9, 2) + '/' + Copy(FPublicar, 6, 2) + '/' + Copy(FPublicar, 1, 4);
end;

{ TItemConfig }

constructor TItemConfig.Create(OnChangeEvent: TOnChangeItemConfig);
begin
  FOnChange := OnChangeEvent;
  FActCode := '';
end;

{ TItemEvent }

function TItemEvent.BusdToDate: TDate;
begin
  Result := TGenFunc.StringToDate(FBusd);
end;

function TItemEvent.BushToTime: TTime;
begin
  Result := TGenFunc.StringToTime(FBush);
end;

procedure TItemEvent.Clear;
begin
  FDatai := '';
  FModif := '';
  FHorai := '';
  FCotxe := '';
  FDataf := '';
  FBusd := '';
  FDescrip := '';
  FId := '';
  FHoraf := '';
  FBuslloc := '';
  FQuedar := '';
  FQuedarh := '';
  FLloc := '';
  FId_google := '';
  FId_cal := '';
  FNom_curt := '';
  FBus := '';
  FBush := '';
  FAny := '';
  FColles := '';
  FSoci.ActCode := '';
  FSoci.Nom := '';
end;

constructor TItemEvent.Create;
begin
  FSoci := TSoci.Create;
  Clear;
end;

function TItemEvent.DatafToDate: TDate;
begin
  Result := TGenFunc.StringToDate(FDataf);
end;

function TItemEvent.DataiToDate: TDate;
begin
  Result := TGenFunc.StringToDate(FDatai);
end;

destructor TItemEvent.Destroy;
begin
  if Assigned(FSoci) then
    FreeAndNil(FSoci);

  inherited;
end;

function TItemEvent.HorafToTime: TTime;
begin
  Result := TGenFunc.StringToTime(FHoraf);
end;

function TItemEvent.HoraiToTime: TTime;
begin
  Result := TGenFunc.StringToTime(FHorai);
end;

function TItemEvent.IdCalToInt: Integer;
begin
  if not TryStrToInt(FId_cal, Result) then
    Result := -1;
end;

function TItemEvent.QuedarhToTime: TTime;
begin
  Result := TGenFunc.StringToTime(FQuedarh);
end;

{ TSoci }

procedure TSoci.Assign(Source: TObject);
begin
  if Source is TSoci then
  begin
    ActCode := TSoci(Source).ActCode;
    Nom := TSoci(Source).Nom;
  end;
end;

{ TAssistencia }

class procedure TAssistencia.Clear;
begin
  IdSoci := '';
  IdEvent := '';
  Bus := '';
  Assist := '';
end;

class procedure TAssistencia.SetAssist(const Value: string);
begin
  FAssist := Value;
end;

class procedure TAssistencia.SetBus(const Value: string);
begin
  FBus := Value;
end;

class procedure TAssistencia.SetIdEvent(const Value: string);
begin
  FIdEvent := Value;
end;

class procedure TAssistencia.SetIdSoci(const Value: string);
begin
  FIdSoci := Value;
end;

end.
