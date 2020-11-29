unit UCalendarsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.SearchBox, FMX.ListBox, FMX.Layouts,
  uInterfaces, uCalendars;

type
  TCalendarsFrm = class(TForm, IChildren)
    lbCalendars: TListBox;
    ListBoxItem1: TListBoxItem;
    lbiSearch: TSearchBox;
    bAdd: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure bAddClick(Sender: TObject);
  private
    FCal: TCalendars;

    procedure CreateItems;
    procedure CreateItem(Cal: TCalendar);
    procedure OnClickBEdit(Sender: TObject);
    procedure OnClickBDel(Sender: TObject);
    procedure OnChangeCalendar(Sender: TObject);
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
  CalendarsFrm: TCalendarsFrm;

implementation

uses
  FMX.DialogService,
  uMessage, UCalendarFrm, {uResultRequest,} uGenFunc;

{$R *.fmx}

{ TCalendarsFrm }

function TCalendarsFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TCalendarsFrm.AfterShow;
var
  Intf: IMainMenu;
begin
  lbCalendars.Clear;
  lbiSearch.Text := '';

  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  begin
    try
      FCal := TCalendars.GetCalendar;
      if FCal.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(FCal.Error);
          end);
      end;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          CreateItems;
        end);
    finally
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

constructor TCalendarsFrm.Create(AOwner: TComponent);
begin
  inherited;

  FCal := TCalendars.Create;
end;

procedure TCalendarsFrm.CreateItem(Cal: TCalendar);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
  BEdit: TSpeedButton;
begin
  Cal.OnChange := OnChangeCalendar;

  lbItem := TListBoxItem.Create(lbCalendars);
  lbItem.Text := Cal.Nom;
  lbItem.TagString := Cal.Id;
  lbItem.ItemData.Detail := 'Id.Calendari: ' + Cal.Idcalendar;
  lbCalendars.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.TagString := Cal.Id;
  BDel.OnClick := OnClickBDel;

  BEdit := TSpeedButton.Create(lbItem);
  BEdit.Align := TAlignLayout.Right;
  BEdit.Width := 40;
  BEdit.StyleLookup := 'composetoolbutton';
  BEdit.Parent := lbItem;
  BEdit.TagString := Cal.Id;
  BEdit.OnClick := OnClickBEdit;
end;

procedure TCalendarsFrm.CreateItems;
var
  i: Integer;
begin
  if not Assigned(FCal) then
    Exit;
    
  lbCalendars.BeginUpdate;
  try
    for i := 0 to FCal.Items.Count - 1 do
      CreateItem(FCal.Items[i]);
  finally
    lbCalendars.EndUpdate;
  end;
end;

destructor TCalendarsFrm.Destroy;
begin
  if Assigned(FCal) then
    FCal.DisposeOf;

  inherited;
end;

procedure TCalendarsFrm.OnChangeCalendar(Sender: TObject);
var
  Idx: Integer;
  i: Integer;
begin
  if not (Sender is TCalendar) then
    Exit;

  Idx := FCal.IndexOf(TCalendar(Sender).id);
  if Idx < 0 then
    Exit;

  for i := 0 to lbCalendars.Count - 1 do
  begin
    if SameText(lbCalendars.ItemByIndex(i).TagString, TCalendar(Sender).id) then
    begin
      lbCalendars.ItemIndex := i;
      lbCalendars.ItemByIndex(i).Text := TCalendar(Sender).nom;
      lbCalendars.ItemByIndex(i).ItemData.Detail := 'Id.Calendari: ' + TCalendar(Sender).idcalendar;
      Break;
    end;
  end;
end;

procedure TCalendarsFrm.OnClickBDel(Sender: TObject);
begin
{
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol esborrar el calendari "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    var
      Intf: IMainMenu;
    begin
      if Supports(Owner, IMainMenu, Intf)  then
        Intf.ShowAni(True);

      TThread.CreateAnonymousThread(procedure
      begin
        TCalendars.DelCalendar(TSpeedButton(Sender).TagString);

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            if Supports(Owner, IMainMenu, Intf)  then
              Intf.ShowAni(False);
          end);
      end
      ).Start;

      lbCalendars.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
}
end;

procedure TCalendarsFrm.OnClickBEdit(Sender: TObject);
var
  Intf: IMainMenu;
  Idx: Integer;
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  if not (Sender is TSpeedButton) then
    Exit;

  Idx := FCal.IndexOf(TSpeedButton(Sender).TagString);
  if Idx < 0 then
    Exit;

  // si es pot, creem formulari d'assistència
  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TCalendarFrm, FCal.Items[Idx]);
end;

function TCalendarsFrm.SetCaption: string;
begin
  Result := 'Calendaris de la colla';
end;

function TCalendarsFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TCalendarsFrm.ShowOkButton: Boolean;
begin
  Result := False;
end;

procedure TCalendarsFrm.bAddClick(Sender: TObject);
begin
{
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  TDialogService.InputQuery('Nou Calendari', ['Id.Calendari', 'Key'], ['', ''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    var
      Resp: TResultRequest;
      TmpI: Integer;
    begin
      if AResult <> mrOk then
        Exit;

      if (Trim(AValues[0]) = '') or (Trim(AValues[1]) = '') then
        Exit;

      Resp := TCalendars.AddCalendar(Trim(AValues[0]), Trim(AValues[1]));

      if TryStrToInt(Resp.id, TmpI) and (TmpI > 0) and (Resp.affected = '1') then
      begin
        TmpI := FCal.Add(Resp.id, Trim(AValues[0]), Trim(AValues[1]));
        FCal.SaveToFile(TGenFunc.GetBaseFolder + uCalendars.cJsonCalendars);
        CreateItem(FCal.Items[TmpI]);
      end
      else
        TMessage.MsjErr('Error creant calendari', []);
    end);
}
end;

end.
