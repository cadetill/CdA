unit USeasonsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.SearchBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts,
  uInterfaces, uSeasons, uCalendars;

type
  TSeasonsFrm = class(TForm, IChildren)
    lbSeasons: TListBox;
    ListBoxItem1: TListBoxItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    lbiSearch: TSearchBox;
    bAdd: TSpeedButton;
    procedure bAddClick(Sender: TObject);
  private
    FSeason: TSeasons;
    FCal: TCalendars;

    procedure CreateItems;
    procedure CreateItem(Season: TSeason);
    procedure OnClickBSync(Sender: TObject);
    procedure OnClickBEdit(Sender: TObject);
    procedure OnClickBDel(Sender: TObject);
    procedure OnChangeSeason(Sender: TObject);
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
  SeasonsFrm: TSeasonsFrm;

implementation

uses
  FMX.DialogService,
  uRESTMdl, uMessage, USeasonFrm, uResultRequest, uGenFunc;

{$R *.fmx}

{ TSeasonsFrm }

function TSeasonsFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TSeasonsFrm.AfterShow;
var
  Intf: IMainMenu;
begin
  lbSeasons.Clear;

  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  var
    Res: TResultRequest;
  begin
    Res := TResultRequest.Create;
    try
      FSeason := TSeasons.GetSeasons(Res);
      if Res.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(Res.Error);
          end);
        Exit;
      end;

      FCal := TCalendars.GetCalendar(Res);
      if Res.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(Res.Error);
          end);
        Exit;
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

procedure TSeasonsFrm.bAddClick(Sender: TObject);
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  TDialogService.InputQuery('Nova Temporada', ['Any', 'Descripció'], ['', ''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    var
      Resp: TResultRequest;
      TmpI: Integer;
    begin
      if AResult <> mrOk then
        Exit;

      if (Trim(AValues[0]) = '') or (Trim(AValues[1]) = '') then
        Exit;

      if not TryStrToInt(AValues[0], TmpI) or (TmpI < 2000) or (TmpI > 2100) then
        Exit;

      Resp := TSeasons.AddSeason(Trim(AValues[0]), Trim(AValues[1]));

      if Resp.affected = '1' then
      begin
        TmpI := FSeason.Add(Trim(AValues[0]), Trim(AValues[1]));
        FSeason.SaveToFile(TGenFunc.GetBaseFolder + uSeasons.cJsonSeasons);
        CreateItem(FSeason.Items[TmpI]);
      end
      else
        TMessage.MsjErr('Error creant temporada', []);
    end);
end;

constructor TSeasonsFrm.Create(AOwner: TComponent);
begin
  inherited;

  FSeason := TSeasons.Create;
end;

procedure TSeasonsFrm.CreateItem(Season: TSeason);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
  BEdit: TSpeedButton;
  BSync: TSpeedButton;
begin
  Season.OnChange := OnChangeSeason;

  lbItem := TListBoxItem.Create(lbSeasons);
  lbItem.Text := Season.descripcio;
  lbItem.TagString := Season.any;
  lbItem.ItemData.Detail := 'Any: ' + Season.any;
  lbSeasons.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.TagString := Season.any;
  BDel.OnClick := OnClickBDel;

  BEdit := TSpeedButton.Create(lbItem);
  BEdit.Align := TAlignLayout.Right;
  BEdit.Width := 40;
  BEdit.StyleLookup := 'composetoolbutton';
  BEdit.Parent := lbItem;
  BEdit.TagString := Season.any;
  BEdit.OnClick := OnClickBEdit;

  BSync := TSpeedButton.Create(lbItem);
  BSync.Align := TAlignLayout.Right;
  BSync.Width := 40;
  BSync.StyleLookup := 'refreshtoolbutton';
  BSync.Parent := lbItem;
  BSync.TagString := Season.any;
  BSync.OnClick := OnClickBSync;
end;

procedure TSeasonsFrm.CreateItems;
var
  i: Integer;
begin
  if not Assigned(FSeason) then
    Exit;

  lbSeasons.BeginUpdate;
  for i := 0 to FSeason.Count do
    CreateItem(FSeason.Items[i]);
  lbSeasons.EndUpdate;
end;

destructor TSeasonsFrm.Destroy;
begin
  if Assigned(FSeason) then
    FreeAndNil(FSeason);

  inherited;
end;

procedure TSeasonsFrm.OnChangeSeason(Sender: TObject);
var
  Idx: Integer;
  i: Integer;
begin
  if not (Sender is TSeason) then
    Exit;

  Idx := FSeason.IndexOf(TSeason(Sender).any);
  if Idx < 0 then
    Exit;

  for i := 0 to lbSeasons.Count - 1 do
  begin
    if SameText(lbSeasons.ItemByIndex(i).TagString, TSeason(Sender).any) then
    begin
      lbSeasons.ItemIndex := i;
      lbSeasons.ItemByIndex(i).Text := TSeason(Sender).descripcio;
      lbSeasons.ItemByIndex(i).ItemData.Detail := 'Id.Calendari: ' + TSeason(Sender).any;
      Break;
    end;
  end;
end;

procedure TSeasonsFrm.OnClickBDel(Sender: TObject);
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol esborrar la temporada "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    var
      Intf: IMainMenu;
    begin
      if Supports(Owner, IMainMenu, Intf)  then
        Intf.ShowAni(True);

      TThread.CreateAnonymousThread(procedure
      begin
        TSeasons.DelSeason(TSpeedButton(Sender).TagString);

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            if Supports(Owner, IMainMenu, Intf)  then
              Intf.ShowAni(False);
          end);
      end
      ).Start;

      lbSeasons.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

procedure TSeasonsFrm.OnClickBEdit(Sender: TObject);
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

  Idx := FSeason.IndexOf(TSpeedButton(Sender).TagString);
  if Idx < 0 then
    Exit;

  // si es pot, creem formulari d'assistència
  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TSeasonFrm, FSeason.Items[Idx]);
end;

procedure TSeasonsFrm.OnClickBSync(Sender: TObject);
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Sincronitzar temporada "%s" desde Google Calendars?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    var
      Intf: IMainMenu;
      Idx: Integer;
    begin
      Idx := FSeason.IndexOf(TSpeedButton(Sender).TagString);
      if Idx = -1 then
        Exit;

      if Supports(Owner, IMainMenu, Intf)  then
        Intf.ShowAni(True);

      TThread.CreateAnonymousThread(procedure
      var
        Res: TResultRequest;
      begin
        try
          Res := TSeasons.SyncSeasons(FSeason.Items[Idx], FCal);
          if Res.Error <> '' then
          begin
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                TMessage.Show(Res.Error);
              end);
          end;
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
    end);
end;

function TSeasonsFrm.SetCaption: string;
begin
  Result := 'Temporades';
end;

function TSeasonsFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TSeasonsFrm.ShowOkButton: Boolean;
begin
  Result := False;
end;

end.
