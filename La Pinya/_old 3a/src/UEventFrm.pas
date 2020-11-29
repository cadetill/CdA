unit UEventFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ListBox, FMX.Layouts,
  UInterfaces, uColles, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  FMX.DateTimeCtrls;

type
  TEventFrm = class(TForm, IChildren)
    rDate: TRectangle;
    rHour: TRectangle;
    rLocation: TRectangle;
    eDate: TLabel;
    eHour: TLabel;
    eLocation: TLabel;
    rColles: TRectangle;
    cbColles: TComboBox;
    lbColles: TListBox;
    mInfo: TMemo;
    bAddColla: TSpeedButton;
    rDescrip: TRectangle;
    eDescrip: TLabel;
    imgDate: TImage;
    imgTime: TImage;
    imgPlace: TImage;
    tcPagines: TTabControl;
    tiGeneralData: TTabItem;
    tiInfo: TTabItem;
    tiTest: TTabItem;
    sbGenData: TScrollBox;
    lnSep1: TLine;
    lnSep2: TLine;
    lRondes: TLabel;
    rMeeting: TRectangle;
    imgMeeting: TImage;
    rMeetingDate: TRectangle;
    eMeetingDate: TDateEdit;
    rMeetingHour: TRectangle;
    eMeetingHour: TTimeEdit;
    imgMeetingHour: TImage;
    imgMeetingDate: TImage;
    eMeeting: TEdit;
    rHeader: TRectangle;
    imgYes: TImage;
    rYes: TRectangle;
    rNo: TRectangle;
    imgNo: TImage;
    rUnknow: TRectangle;
    imgUnknow: TImage;
    rOptions: TRectangle;
    cbUsers: TComboBox;
    procedure rLocationClick(Sender: TObject);
    procedure bAddCollaClick(Sender: TObject);
    procedure rMeetingClick(Sender: TObject);
  private
    FColles: TColles;
    FThreadEnd: Boolean;

    procedure AddColles(Colles: string);
    procedure AddColla(Colla: TColla);
    procedure OnClickBDel(Sender: TObject);
    procedure ThreadTerminated(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function SetCaption: string;
    function ShowOkButton: Boolean;
    function ShowBackButton: Boolean;
    function AcceptForm: Boolean;
    procedure AfterShow;
  end;

var
  EventFrm: TEventFrm;

implementation

uses
  uEvents, uGenFunc, uMessage, uResultRequest, uMaps;

{$R *.fmx}

{ TEventFrm }

function TEventFrm.AcceptForm: Boolean;
var
  i: Integer;
  TmpStr: string;
  TmpStrColles: string;
  Intf: IMainMenu;
  Thrd: TThread;
  RR: TResultRequest;
begin
  Result := True;

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TEvent) then Exit;

  // assignem dades
  TEvent(TagObject).quedar := eMeeting.Text;
  TEvent(TagObject).quedard := FormatDateTime('yyyy-mm-dd', eMeetingDate.Date);
  TEvent(TagObject).quedarh := FormatDateTime('hh:mm:00', eMeetingHour.Time);
  TEvent(TagObject).info := mInfo.Text;

  TmpStr := '';
  TmpStrColles := '';
  for i := 0 to lbColles.Count - 1 do
  begin
    if TmpStr <> '' then
    begin
      TmpStr := TmpStr + ',';
      TmpStrColles := TmpStrColles + ', ';
    end;

    TmpStr := TmpStr + lbColles.ItemByIndex(i).TagString;
    TmpStrColles := TmpStrColles + lbColles.ItemByIndex(i).Text;
  end;
  TEvent(TagObject).colles := TmpStr;
  TEvent(TagObject).NameColles := TmpStrColles;

  // actualitzem dades
  FThreadEnd := False;
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);
  Thrd := TThread.CreateAnonymousThread(procedure
  begin
    try
      RR := TEvents.EditEvent(TEvent(TagObject), 4);
    finally
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Supports(Owner, IMainMenu, Intf)  then
            Intf.ShowAni(False);
        end);
    end;
  end
  );
  Thrd.OnTerminate := ThreadTerminated;
  Thrd.Start;

  repeat
    Sleep(50);
    Application.ProcessMessages;
  until (FThreadEnd);

  if Assigned(RR) and (RR.Error <> '') then
    TMessage.MsjErr('Error actualitzant event. Missatge d''error: %s', [RR.Error])
  else
    TMessage.Show('Event actualitzat.');
end;

procedure TEventFrm.AddColla(Colla: TColla);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
begin
  lbItem := TListBoxItem.Create(lbColles);
  lbItem.Text := Colla.colla;
  lbItem.TagString := Colla.id;
  lbColles.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.OnClick := OnClickBDel;
end;

procedure TEventFrm.AddColles(Colles: string);
var
  L: TStringList;
  i: Integer;
  Idx: Integer;
begin
  L := TStringList.Create;
  try
    L.CommaText := Colles;
    for i := 0 to L.Count - 1 do
    begin
      Idx := FColles.IndexOf(L[i]);
      if Idx <> -1 then
       AddColla(FColles.Items[Idx]);
    end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TEventFrm.AfterShow;
var
  Intf: IMainMenu;
begin
  // carreguem dades provinents de TEvent
  eDate.Text := '';
  eHour.Text := '';
  eLocation.Text := '';
  eDescrip.Text := '';

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TEvent) then Exit;

  eDate.Text := FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(TEvent(TagObject).datai));
  if TEvent(TagObject).datai <> TEvent(TagObject).dataf then
    eDate.Text := eDate.Text + ' a ' + FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(TEvent(TagObject).dataf));
  eHour.Text := FormatDateTime('hh:mm', TGenFunc.StringToTime(TEvent(TagObject).horai));
  if TEvent(TagObject).horai <> TEvent(TagObject).horaf then
    eHour.Text := eHour.Text + ' a ' + FormatDateTime('hh:mm', TGenFunc.StringToTime(TEvent(TagObject).horaf));
  eLocation.Text := TEvent(TagObject).lloc;
  eDescrip.Text := UpperCase(TEvent(TagObject).descrip);
  eMeeting.Text := TEvent(TagObject).quedar;
  eMeetingDate.Text := FormatDateTime('dd/mm/yyyy', TGenFunc.StringToDate(TEvent(TagObject).quedard));
  eMeetingHour.Time := TGenFunc.StringToTime(TEvent(TagObject).quedarh);
  mInfo.Text := TEvent(TagObject).info;

  // carreguem dades provinents del servidor
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  var
    Res: TResultRequest;
  begin
    Res := TResultRequest.Create;
    try
      FColles := TColles.GetColles(Res);
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
          AddColles(TEvent(TagObject).colles);
        end);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          i: Integer;
        begin
          cbColles.Clear;
          if Assigned(FColles) then
            for i := 0 to FColles.Count do
              cbColles.Items.AddObject(FColles.Items[i].colla, FColles.Items[i]);
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

procedure TEventFrm.bAddCollaClick(Sender: TObject);
begin
  if cbColles.ItemIndex = -1 then
    Exit;

  if lbColles.Items.IndexOf(TColla(cbColles.Items.Objects[cbColles.ItemIndex]).colla) <> -1 then
    Exit;

  AddColla(TColla(cbColles.Items.Objects[cbColles.ItemIndex]));
end;

constructor TEventFrm.Create(aOwner: TComponent);
begin
  inherited;

  tcPagines.ActiveTab := tiGeneralData;
end;

destructor TEventFrm.Destroy;
begin
  if Assigned(FColles) then
    FreeAndNil(FColles);

  inherited;
end;

procedure TEventFrm.OnClickBDel(Sender: TObject);
begin
  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol treure la colla "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    begin
      lbColles.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

procedure TEventFrm.rLocationClick(Sender: TObject);
begin
  if TEvent(TagObject).lloc <> '' then
    TMaps.OpenNavigation(TEvent(TagObject).lloc);
end;

procedure TEventFrm.rMeetingClick(Sender: TObject);
begin
  if eMeeting.Text <> '' then
    TMaps.OpenNavigation(eMeeting.Text);
end;

function TEventFrm.SetCaption: string;
begin
  Result := 'Event';
end;

function TEventFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TEventFrm.ShowOkButton: Boolean;
begin
  Result := True;
end;

procedure TEventFrm.ThreadTerminated(Sender: TObject);
begin
  FThreadEnd := True;
end;

end.
