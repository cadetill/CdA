unit UEventsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.SearchBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts, UFEvent, FMX.Objects,
  uInterfaces, uEvents, uColles;

type
  TEventsFrm = class(TForm, IChildren)
    lbEvents: TListBox;
    ListBoxItem1: TListBoxItem;
    FEvent1: TFEvent;
    rHeader: TRectangle;
    cbSeasons: TComboBox;
    cbUser: TComboBox;
    cbCalendars: TComboBox;
    rCalendars: TRectangle;
    rSeasons: TRectangle;
    rUser: TRectangle;
    procedure cbSeasonsChange(Sender: TObject);
    procedure cbCalendarsChange(Sender: TObject);
  private
    FEvents: TEvents;
    FColles: TColles;

    procedure FilterItems;
    procedure PosToItem;

    procedure LoadData;
    function FillSeasons: Boolean;
    function FillCalendars: Boolean;
    procedure CreateItems;
    procedure CreateItem(Event: TEvent);
    procedure CreateGroupHeader(Month: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SetCaption: string;
    function ShowOkButton: Boolean;
    function ShowBackButton: Boolean;
    function AcceptForm: Boolean;
    procedure AfterShow;
  end;

var
  EventsFrm: TEventsFrm;

implementation

uses
  System.DateUtils,
  uMessage, uGenFunc, uSeasons, uCalendars, uResultRequest;

{$R *.fmx}

{ TEventsFrm }

function TEventsFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TEventsFrm.AfterShow;
begin

end;

procedure TEventsFrm.cbCalendarsChange(Sender: TObject);
begin
  FilterItems;
  PosToItem;
end;

procedure TEventsFrm.cbSeasonsChange(Sender: TObject);
begin
  LoadData;
end;

constructor TEventsFrm.Create(AOwner: TComponent);
begin
  inherited;

  FEvents := TEvents.Create;

  cbSeasons.Clear;
  cbUser.Clear;
  cbCalendars.Clear;
  lbEvents.Clear;
  LoadData;
end;

procedure TEventsFrm.CreateGroupHeader(Month: Integer);
var
  lbHeader: TListBoxGroupHeader;
begin
  lbHeader := TListBoxGroupHeader.Create(lbEvents);
  lbHeader.Parent := lbEvents;
  lbHeader.StyledSettings := lbHeader.StyledSettings - [TStyledSetting.FontColor];
  lbHeader.StyledSettings := lbHeader.StyledSettings - [TStyledSetting.Size];
  lbHeader.StyledSettings := lbHeader.StyledSettings - [TStyledSetting.Style];
  lbHeader.TextSettings.Font.Style := lbHeader.TextSettings.Font.Style + [TFontStyle.fsBold];
  lbHeader.TextSettings.Font.Size := 10;
  lbHeader.TextSettings.FontColor := TAlphaColorRec.Crimson;
  case Month of
    1: lbHeader.Text := #13'GENER DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    2: lbHeader.Text := #13'FEBRER DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    3: lbHeader.Text := #13'MARÇ DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    4: lbHeader.Text := #13'ABRIL DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    5: lbHeader.Text := #13'MAIG DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    6: lbHeader.Text := #13'JUNY DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    7: lbHeader.Text := #13'JULIOL DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    8: lbHeader.Text := #13'AGOST DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    9: lbHeader.Text := #13'SETEMBRE DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    10: lbHeader.Text := #13'OCTUBRE DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    11: lbHeader.Text := #13'NOVENBRE DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
    12: lbHeader.Text := #13'DECEMBRE DE ' + TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any;
  end;
  lbHeader.Height := 50;
  lbEvents.AddObject(lbHeader);
end;

procedure TEventsFrm.CreateItem(Event: TEvent);
var
  lbItem: TListBoxItem;
  Frame: TFEvent;
  L: TStringList;
  i: Integer;
  Idx: Integer;
begin
  //Event.OnChange := OnChangeEvent;

  lbItem := TListBoxItem.Create(lbEvents);
  lbItem.Parent := lbEvents;
  lbItem.TagString := Event.id;
  lbItem.TagObject := Event;
  lbEvents.AddObject(lbItem);

  Frame := TFEvent.Create(Self);
  lbItem.Height := Frame.Height;
  Frame.Parent := lbItem;
  Frame.Name := 'Frame' + Event.id;
  Frame.Align := TAlignLayout.Client;
  Frame.lType.Text := Event.nom_curt;
  Frame.lDesc.Text := Event.descrip;
  Frame.lDay.Text := FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(Event.datai));
  if Event.datai <> Event.dataf then
    Frame.lDay.Text := Frame.lDay.Text + ' a ' + FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(Event.dataf));
  Frame.lTime.Text := FormatDateTime('hh:mm', TGenFunc.StringToTime(Event.horai));
  if Event.horai <> Event.horaf then
    Frame.lTime.Text := Frame.lTime.Text + ' a ' + FormatDateTime('hh:mm', TGenFunc.StringToTime(Event.horaf));
  Frame.lPlace.Text := Event.lloc;
  L := TStringList.Create;
  try
    L.CommaText := Event.colles;
    for i := 0 to L.Count - 1 do
    begin
      Idx := FColles.IndexOf(L[i]);
      if Idx <> -1 then
        L[i] := FColles.Items[Idx].colla;
    end;
    Frame.lColles.Text := L.CommaText;
  finally
    FreeAndNil(L);
  end;
  Frame.IdEvent := Event.id;
  Frame.Event := Event;
end;

procedure TEventsFrm.CreateItems;
var
  i: Integer;
  Mes: Integer;
begin
  if not Assigned(FEvents) then
    Exit;

  lbEvents.BeginUpdate;
  lbEvents.Clear;
  Mes := 0;
  for i := 0 to FEvents.Count do
  begin
    if Mes <> MonthOf(TGenFunc.StringToDate(FEvents.Items[i].datai)) then
    begin
      Mes := MonthOf(TGenFunc.StringToDate(FEvents.Items[i].datai));
      CreateGroupHeader(Mes);
    end;
    CreateItem(FEvents.Items[i]);
  end;
  lbEvents.EndUpdate;

  FilterItems;
  PosToItem;
end;

destructor TEventsFrm.Destroy;
begin
  if Assigned(FEvents) then
    FreeAndNil(FEvents);
  if Assigned(FColles) then
    FreeAndNil(FColles);

  inherited;
end;

function TEventsFrm.FillCalendars: Boolean;
var
  Cal: TCalendars;
  Res: TResultRequest;
begin
  Res := TResultRequest.Create;
  try
    cbCalendars.Clear;
    Result := True;
    Cal := TCalendars.GetCalendar(Res);
    if Res.Error <> '' then
    begin
      TMessage.Show(Res.Error);
      if not Assigned(Cal) then
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    FreeAndNil(Res);
  end;

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      i: Integer;
    begin
      cbCalendars.Items.Add('Tots els calendaris');
      for i := 0 to Cal.Count do
        cbCalendars.Items.AddObject(Cal.Items[i].nom, Cal.Items[i]);

      cbCalendars.OnChange := nil;
      cbCalendars.ItemIndex := 0;
      cbCalendars.OnChange := cbCalendarsChange;
    end);
end;

function TEventsFrm.FillSeasons: Boolean;
var
  Seasons: TSeasons;
  Res: TResultRequest;
begin
  Res := TResultRequest.Create;
  try
    cbSeasons.Clear;
    Result := True;
    Seasons := TSeasons.GetSeasons(Res);
    if Res.Error <> '' then
    begin
      TMessage.Show(Res.Error);
      if not Assigned(Seasons) then
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    FreeAndNil(Res);
  end;

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      i: Integer;
    begin
      // carreguem temporades
      for i := 0 to Seasons.Count do
        cbSeasons.Items.AddObject(Seasons.Items[i].descripcio, Seasons.Items[i]);

      // si existeix la temporada de l'any en curs, ens posicionem en ella
      cbSeasons.OnChange := nil;
      for i := 0 to cbSeasons.Count - 1 do
        if SameText(TSeason(cbSeasons.Items.Objects[i]).any, YearOf(Date).ToString) then
          cbSeasons.ItemIndex := i;
      cbSeasons.OnChange := cbSeasonsChange;
    end);
end;

procedure TEventsFrm.FilterItems;
var
  i: Integer;
begin
//  lbEvents.BeginUpdate;
  for i := 0 to lbEvents.Count - 1 do
  begin
    if lbEvents.ItemByIndex(i) is TListBoxGroupHeader then
      Continue;

    if not Assigned(cbCalendars.Items.Objects[cbCalendars.ItemIndex]) then
      lbEvents.ItemByIndex(i).Visible := True
    else
      lbEvents.ItemByIndex(i).Visible := SameText(TCalendar(cbCalendars.Items.Objects[cbCalendars.ItemIndex]).id,
                                                  TEvent(lbEvents.ItemByIndex(i).TagObject).id_cal);
  end;
//  lbEvents.EndUpdate;
end;

procedure TEventsFrm.LoadData;
var
  Intf: IMainMenu;
begin
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  var
    Res: TResultRequest;
  begin
    Res := TResultRequest.Create;
    try
      cbCalendars.Items.Clear;
      if not FillCalendars then
        Exit;
      cbSeasons.Items.Clear;
      if not FillSeasons then
        Exit;

      if Assigned(FColles) then
        FreeAndNil(FColles);
      FColles := TColles.GetColles(Res);
      if Res.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(Res.Error);
          end);
      end;

      if Assigned(FEvents) then
        FreeAndNil(FEvents);
      FEvents := TEvents.GetEvents(Res, TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any);
      if Res.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(Res.Error);
          end);
      end;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          CreateItems;
        end);
    finally
      FreeAndNil(Res);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Supports(Owner, IMainMenu, Intf)  then
            Intf.ShowAni(False);
        end);
    end;
  end
  ).Start;
end;

procedure TEventsFrm.PosToItem;
var
  i: Integer;
  lbItem: TListBoxItem;
begin
  if not SameText(TSeason(cbSeasons.Items.Objects[cbSeasons.ItemIndex]).any, YearOf(Date).ToString) then
  begin
    lbEvents.ScrollToItem(lbEvents.ItemByIndex(0));
    Exit;
  end;

  for i := 0 to lbEvents.Count - 1 do
  begin
    lbItem := lbEvents.ItemByIndex(i);
    if not Assigned(lbItem.TagObject) or not (lbItem.TagObject is TEvent) or not lbItem.Visible then
      Continue;

    if TGenFunc.StringToDate(TEvent(lbItem.TagObject).datai) >= Date then
    begin
      lbEvents.ScrollToItem(lbItem);
      Break
    end;
  end;
end;

function TEventsFrm.SetCaption: string;
begin
  Result := 'Calendari de la colla';
end;

function TEventsFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TEventsFrm.ShowOkButton: Boolean;
begin
  Result := False;
end;

end.
