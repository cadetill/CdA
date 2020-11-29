unit UCalendarFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseFrm, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation,
  FMX.MultiResBitmap, FMX.Objects,
  UCalendarMdl, UInterfaces, UHelpers, UBaseModalFrm;

const
  cCaption = 'Calendari';
  cErrNotSelectedSeasson = 'Cal especificar una temporada.';

type
  TCalendarFrm = class(TBaseFrm, IChildren)
    pFilter: TPanel;
    bDiades: TCornerButton;
    bAssajos: TCornerButton;
    bEvents: TCornerButton;
    pTempUser: TPanel;
    cbTemporades: TComboBox;
    cbSoci: TComboBox;
    lbSeason: TListBox;
    ListBoxItem1: TListBoxItem;
    recCal: TCalloutRectangle;
    pImages: TPanel;
    imgCalendar: TImage;
    imgTime: TImage;
    imgPlace: TImage;
    imgMeeting: TImage;
    imgBus: TImage;
    imgColles: TImage;
    pData: TPanel;
    llCalendar: TLabel;
    lTime: TLabel;
    lPlace: TLabel;
    lMeeting: TLabel;
    lBus: TLabel;
    lColles: TLabel;
    pTitre: TPanel;
    lType: TLabel;
    lDesc: TLabel;
    lnCal: TLine;
    procedure cbTemporadesChange(Sender: TObject);
    procedure cbSociChange(Sender: TObject);
    procedure bDiadesClick(Sender: TObject);
    procedure bAssajosClick(Sender: TObject);
    procedure bEventsClick(Sender: TObject);
  private
    FCalMdl: TCalendarMdl;

    procedure GetTemporades;
    procedure GetEventsTemporada;
    procedure GetAssistencia;

    procedure FilterItems(Button: TObject);

    procedure CreateNewListBoxItem(bCalendar, bClock, bMarker, bMeeting, bBus, bColles: TCustomBitmapItem);
    procedure ListBoxItemOnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SetCaption: string;
    function DeactivateAll: Boolean;
    function AcceptForm: Boolean;
    function EnabledBackButton: Boolean;
    function DefaultStateAcceptButton: Boolean;
    procedure AfterShow;
  end;

var
  CalendarFrm: TCalendarFrm;

implementation

uses
  System.DateUtils,
  UClasses, UIniFiles, USocisMdl, UMessage, UImagesMdl, UCalFrm, UAssistenciaFrm;

{$R *.fmx}

{ TCalendarFrm }

function TCalendarFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TCalendarFrm.AfterShow;
var
  L: TStringList;
begin
  lbSeason.Clear;

  Ani(True);
  TThread.CreateAnonymousThread(procedure
    begin
      // afegim usuaris
      TFileIni.SetFileIni(TGenFunc.GetIniName);
      L := nil;
      try
        L := TStringList.Create;
        TFileIni.GetSection('USERS', L);
        TSocisMdl.AddUsersToCombobox(L.CommaText, cbSoci);
      finally
        FreeAndNil(L);
      end;

      // afegim temporades
      GetTemporades;

      // carreguem events de la temporada
      GetEventsTemporada;

      // carreguem assist�ncia usuari seleccionat i pintem segons sigui
      GetAssistencia;

      // treiem animaci�
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Ani(False);
        end);
    end).Start;
end;

procedure TCalendarFrm.bAssajosClick(Sender: TObject);
begin
  inherited;

  FilterItems(Sender);
end;

procedure TCalendarFrm.bDiadesClick(Sender: TObject);
begin
  inherited;

  FilterItems(Sender);
end;

procedure TCalendarFrm.bEventsClick(Sender: TObject);
begin
  inherited;

  FilterItems(Sender);
end;

procedure TCalendarFrm.cbSociChange(Sender: TObject);
begin
  inherited;

  if (cbSoci.ItemIndex = -1) or (cbTemporades.ItemIndex = -1) or (lbSeason.Items.Count = 0) then
    Exit;

  Ani(True);
  TThread.CreateAnonymousThread(procedure
    begin
      // carreguem assist�ncia usuari seleccionat i pintem segons sigui
      GetAssistencia;

      // treiem animaci�
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Ani(False);
        end);
    end).Start;
end;

procedure TCalendarFrm.cbTemporadesChange(Sender: TObject);
begin
  inherited;

  if (cbSoci.ItemIndex = -1) or (cbTemporades.ItemIndex = -1) or (lbSeason.Items.Count = 0) then
    Exit;

  Ani(True);
  TThread.CreateAnonymousThread(procedure
    begin
      // carreguem events de la temporada
      GetEventsTemporada;

      // carreguem assist�ncia usuari seleccionat i pintem segons sigui
      GetAssistencia;

      // treiem animaci�
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Ani(False);
        end);
    end).Start;
end;

constructor TCalendarFrm.Create(AOwner: TComponent);
begin
  inherited;

  FCalMdl := TCalendarMdl.Create(Self);
end;

procedure TCalendarFrm.CreateNewListBoxItem(bCalendar, bClock, bMarker,
  bMeeting, bBus, bColles: TCustomBitmapItem);
var
  lbItem: TListBoxItem;
  CalFrm: TCalFrm;
begin
  lbItem := TListBoxItem.Create(lbSeason);
  lbItem.Size.PlatformDefault := False;
  lbItem.Size.Height := 337;
  lbItem.Name := 'itCal' + FCalMdl.cdsEvents.FieldByName('id').AsString;
  lbItem.Text := '';
  lbItem.Width := lbSeason.Width;
  lbItem.OnClick := ListBoxItemOnClick;

  lbItem.TagObject := TItemEvent.Create;
  TItemEvent(lbItem.TagObject).Id := FCalMdl.cdsEvents.FieldByName('Id').AsString;
  TItemEvent(lbItem.TagObject).Id_cal := FCalMdl.cdsEvents.FieldByName('Id_cal').AsString;
  TItemEvent(lbItem.TagObject).Id_google := FCalMdl.cdsEvents.FieldByName('Id_google').AsString;
  TItemEvent(lbItem.TagObject).Any := FCalMdl.cdsEvents.FieldByName('Any').AsString;
  TItemEvent(lbItem.TagObject).Descrip := FCalMdl.cdsEvents.FieldByName('Descrip').AsString;
  TItemEvent(lbItem.TagObject).Cotxe := FCalMdl.cdsEvents.FieldByName('Cotxe').AsString;
  TItemEvent(lbItem.TagObject).Bus := FCalMdl.cdsEvents.FieldByName('Bus').AsString;
  TItemEvent(lbItem.TagObject).Busd := FCalMdl.cdsEvents.FieldByName('Busd').AsString;
  TItemEvent(lbItem.TagObject).Bush := FCalMdl.cdsEvents.FieldByName('Bush').AsString;
  TItemEvent(lbItem.TagObject).Buslloc := FCalMdl.cdsEvents.FieldByName('Buslloc').AsString;
  TItemEvent(lbItem.TagObject).Datai := FCalMdl.cdsEvents.FieldByName('Datai').AsString;
  TItemEvent(lbItem.TagObject).Dataf := FCalMdl.cdsEvents.FieldByName('Dataf').AsString;
  TItemEvent(lbItem.TagObject).Horai := FCalMdl.cdsEvents.FieldByName('Horai').AsString;
  TItemEvent(lbItem.TagObject).Horaf := FCalMdl.cdsEvents.FieldByName('Horaf').AsString;
  TItemEvent(lbItem.TagObject).Lloc := FCalMdl.cdsEvents.FieldByName('Lloc').AsString;
  TItemEvent(lbItem.TagObject).Quedar := FCalMdl.cdsEvents.FieldByName('Quedar').AsString;
  TItemEvent(lbItem.TagObject).Quedarh := FCalMdl.cdsEvents.FieldByName('Quedarh').AsString;
  TItemEvent(lbItem.TagObject).Nom_curt := FCalMdl.cdsEvents.FieldByName('Nom_curt').AsString;
  TItemEvent(lbItem.TagObject).Modif := FCalMdl.cdsEvents.FieldByName('Modif').AsString;
  TItemEvent(lbItem.TagObject).Colles := FCalMdl.cdsEvents.FieldByName('Colles').AsString;

  CalFrm := TCalFrm.Create(Self);
  CalFrm.Parent := lbItem;
  CalFrm.Align := TAlignLayout.Client;
  CalFrm.Name := lbItem.Name + 'c';
  CalFrm.Id := FCalMdl.cdsEvents.FieldByName('id').AsString;

  CalFrm.lType.Text := FCalMdl.cdsEvents.FieldByName('nom_curt').AsString + ':';
  CalFrm.lDesc.Text := FCalMdl.cdsEvents.FieldByName('descrip').AsString + FCalMdl.cdsEvents.FieldByName('Id').AsString;
  CalFrm.imgEdit.Visible := False;

  if FCalMdl.cdsEvents.FieldByName('datai').AsString <> '' then
    CalFrm.lCalendar.Text := FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(FCalMdl.cdsEvents.FieldByName('datai').AsString));
  if FCalMdl.cdsEvents.FieldByName('busd').AsString <> '' then
    CalFrm.lBus.Text := FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(FCalMdl.cdsEvents.FieldByName('busd').AsString));
  if FCalMdl.cdsEvents.FieldByName('buslloc').AsString <> '' then
    if CalFrm.lBus.Text <> '' then
      CalFrm.lBus.Text := CalFrm.lBus.Text + ' - ' + FCalMdl.cdsEvents.FieldByName('buslloc').AsString
    else
      CalFrm.lBus.Text := FCalMdl.cdsEvents.FieldByName('buslloc').AsString;
  CalFrm.lMeeting.Text := FCalMdl.cdsEvents.FieldByName('quedar').AsString;
  CalFrm.lPlace.Text := FCalMdl.cdsEvents.FieldByName('lloc').AsString;
  if (FCalMdl.cdsEvents.FieldByName('horai').AsString <> '') and (FCalMdl.cdsEvents.FieldByName('horai').AsString <> '00:00:00') then
    CalFrm.lTime.Text := FormatDateTime('hh''h''nn', TGenFunc.StringToTime(FCalMdl.cdsEvents.FieldByName('horai').AsString));
  CalFrm.lColles.Text := FCalMdl.cdsEvents.FieldByName('Colles').AsString;

  lbItem.Height := CalFrm.GetTotalHeight;

  lbSeason.AddObject(lbItem);
end;

function TCalendarFrm.DeactivateAll: Boolean;
begin
  Result := False;
end;

function TCalendarFrm.DefaultStateAcceptButton: Boolean;
begin
  Result := False;
end;

destructor TCalendarFrm.Destroy;
begin
  if Assigned(FCalMdl) then
    FreeAndNil(FCalMdl);

  inherited;
end;

function TCalendarFrm.EnabledBackButton: Boolean;
begin
  Result := True;
end;

procedure TCalendarFrm.FilterItems(Button: TObject);
var
  i: Integer;
  Vis: Boolean;
  lbItem: TListBoxItem;
begin
  if not (Button is TCornerButton) then
    Exit;

  Vis := not TCornerButton(Button).IsPressed;

  for i := 0 to lbSeason.Count - 1 do
  begin
    lbItem := TListBoxItem(lbSeason.ItemByIndex(i));
    if TItemEvent(lbItem.TagObject).IdCalToInt = TCornerButton(Button).Tag then
      lbItem.Visible := Vis;
  end;
end;

procedure TCalendarFrm.GetAssistencia;
var
  lbItem: TListBoxItem;
  Comp: TComponent;
  Mdl: TSocisMdl;
begin
  // inicialitzem a negre
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      i: Integer;
    begin
      for i := 0 to lbSeason.Count - 1 do
      begin
        lbItem := TListBoxItem(lbSeason.ItemByIndex(i));
        Comp := FindComponent(lbItem.Name + 'c');
        if Assigned(Comp) and (Comp is TCalFrm) then
          TCalFrm(Comp).SetColorStroke(TAlphaColorRec.Black);
      end;
    end);

  Mdl := TSocisMdl.Create(Self);
  try
    // consultem assist�ncia
    Mdl.GetEventsSoci(TSoci(cbSoci.Items.Objects[cbSoci.ItemIndex]).ActCode, TTemporada(cbTemporades.Items.Objects[cbTemporades.ItemIndex]).Any);
    if not Mdl.cdsResult.Active or (Mdl.cdsResult.RecordCount = 0) then
      Exit;

    // actualitzem colors assist�ncia
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      var
        i: Integer;
      begin
        for i := 0 to lbSeason.Count - 1 do
        begin
          lbItem := TListBoxItem(lbSeason.ItemByIndex(i));

          if Mdl.cdsResult.Locate('id_event', TItemEvent(lbItem.TagObject).Id, []) then
          begin
            Comp := FindComponent(lbItem.Name + 'c');
            if Assigned(Comp) and (Comp is TCalFrm) then
            begin
              if Mdl.cdsResult.FieldByName('assist').AsString = '1' then
                TCalFrm(Comp).SetColorStroke(TAlphaColorRec.Green);
              if Mdl.cdsResult.FieldByName('assist').AsString = '2' then
                TCalFrm(Comp).SetColorStroke(TAlphaColorRec.Yellow);
              if Mdl.cdsResult.FieldByName('assist').AsString = '3' then
                TCalFrm(Comp).SetColorStroke(TAlphaColorRec.Red);
            end;
          end;
        end;
      end);
  finally
    FreeAndNil(Mdl);
  end;
end;

procedure TCalendarFrm.GetEventsTemporada;
var
  MdlImg: TImagesMdl;
  bmCalendar: TCustomBitmapItem;
  bmClock: TCustomBitmapItem;
  bmMarker: TCustomBitmapItem;
  bmMeeting: TCustomBitmapItem;
  bmBus: TCustomBitmapItem;
  bmColles: TCustomBitmapItem;
  Size: TSize;
  ActYear: Integer;
  i: Integer;
  lbItem: TListBoxItem;
begin
  lbSeason.Clear;

  if cbTemporades.ItemIndex = -1 then
  begin
    TMessage.Show(cErrNotSelectedSeasson);
    Exit;
  end;

  FCalMdl.GetEventsBySeason(TTemporada(cbTemporades.Items.Objects[cbTemporades.ItemIndex]).Any);

  MdlImg := TImagesMdl.Create(Self);
  try
    MdlImg.ilImages.BitmapItemByName('calendar32', bmCalendar, Size);
    MdlImg.ilImages.BitmapItemByName('clock32', bmClock, Size);
    MdlImg.ilImages.BitmapItemByName('marker32', bmMarker, Size);
    MdlImg.ilImages.BitmapItemByName('meeting32', bmMeeting, Size);
    MdlImg.ilImages.BitmapItemByName('bus32', bmBus, Size);
    MdlImg.ilImages.BitmapItemByName('colles32', bmColles, Size);

    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        while not FCalMdl.cdsEvents.Eof do
        begin
          CreateNewListBoxItem(
                               bmCalendar,
                               bmClock,
                               bmMarker,
                               bmMeeting,
                               bmBus,
                               bmColles
                              );

          FCalMdl.cdsEvents.Next;
        end;
      end);
  finally
    FreeAndNil(MdlImg);
  end;

  if TryStrToInt(TTemporada(cbTemporades.Items.Objects[cbTemporades.ItemIndex]).Any, ActYear) and (YearOf(Date) = ActYear) then
  begin
    for i := 0 to lbSeason.Count - 1 do
    begin
      lbItem := TListBoxItem(lbSeason.ItemByIndex(i));
      if not Assigned(lbItem.TagObject) or not (lbItem.TagObject is TItemEvent) then
        Continue;

      if TItemEvent(lbItem.TagObject).DataiToDate >= Date then
      begin
        lbSeason.ScrollToItem(lbItem);
        Break
      end;
    end;
  end;
end;

procedure TCalendarFrm.GetTemporades;
begin
  FCalMdl.GetTemporades;

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      Temp: TTemporada;
    begin
      cbTemporades.Clear;

      while not FCalMdl.cdsTemporades.Eof do
      begin
        Temp := TTemporada.Create;
        Temp.Any := FCalMdl.cdsTemporades.FieldByName('any').AsString;
        Temp.Descripcio := FCalMdl.cdsTemporades.FieldByName('descripcio').AsString;

        cbTemporades.Items.AddObject(Temp.Descripcio, Temp);

        FCalMdl.cdsTemporades.Next;
      end;

      if cbTemporades.Count > 0 then
        cbTemporades.ItemIndex := 0;
    end);
end;

procedure TCalendarFrm.ListBoxItemOnClick(Sender: TObject);
var
  Intf: IMainMenu;
begin
  if not (Sender is TListBoxItem) or not (TListBoxItem(Sender).TagObject is TItemEvent) then
    Exit;

  // assignem el soci seleccionat
  TItemEvent(TListBoxItem(Sender).TagObject).Soci.Assign(TSoci(cbSoci.Items.Objects[cbSoci.ItemIndex]));

  // si es pot, creem formulari d'assist�ncia
  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TAssistenciaFrm, TItemEvent(TListBoxItem(Sender).TagObject));
end;

function TCalendarFrm.SetCaption: string;
begin
  Result := cCaption;
end;

end.
