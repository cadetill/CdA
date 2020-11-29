unit USocisFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.SearchBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts,
  uInterfaces, uSocis;

type
  TSocisFrm = class(TForm, IChildren)
    lbSocis: TListBox;
    ListBoxItem1: TListBoxItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lbiSearch: TSearchBox;
    bAdd: TSpeedButton;
    procedure bAddClick(Sender: TObject);
  private
    FSocis: TSocis;

    procedure CreateItems;
    procedure CreateItem(Soci: TSoci);
    procedure OnClickBEdit(Sender: TObject);
    procedure OnClickBDel(Sender: TObject);

    procedure OnChangeSoci(Sender: TObject);
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
  SocisFrm: TSocisFrm;

implementation

uses
  FMX.DialogService,
  uMessage, USociFrm, uResultRequest, uGenFunc;

{$R *.fmx}

{ TSocisFrm }

function TSocisFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TSocisFrm.AfterShow;
begin

end;

procedure TSocisFrm.bAddClick(Sender: TObject);
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  TDialogService.InputQuery('Nou Soci', ['Nom', 'Mail'], ['', ''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    var
      Resp: TResultRequest;
      TmpI: Integer;
      Soci: TSoci;
    begin
      if AResult <> mrOk then
        Exit;

      if (Trim(AValues[0]) = '') or (Trim(AValues[1]) = '') then
        Exit;

      Soci := TSoci.Create;
      try
        Soci.nom := Trim(AValues[0]);
        Soci.mail := Trim(AValues[1]);
        Resp := TSocis.AddSoci(Soci);
      finally
        FreeAndNil(Soci);
      end;

      if TryStrToInt(Resp.id, TmpI) and (TmpI > 0) and (Resp.affected = '1') then
      begin
        TmpI := FSocis.Add(Resp.id, Trim(AValues[0]), Trim(AValues[1]));
        FSocis.SaveToFile(TGenFunc.GetBaseFolder + uSocis.cJsonSocis);
        CreateItem(FSocis.Items[TmpI]);
      end
      else
        TMessage.MsjErr('Error creant Soci', []);
    end);
end;

constructor TSocisFrm.Create(AOwner: TComponent);
var
  Intf: IMainMenu;
begin
  inherited;

  FSocis := TSocis.Create;

  lbSocis.Clear;
  lbiSearch.Text := '';

  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  var
    Res: TResultRequest;
  begin
    Res := TResultRequest.Create;
    try
      FSocis := TSocis.GetSocis(Res);
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

procedure TSocisFrm.CreateItem(Soci: TSoci);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
  BEdit: TSpeedButton;
  TmpStr: string;
  i: Integer;
begin
  Soci.OnChange := OnChangeSoci;

  TmpStr := '';
  for i := 0 to Soci.Count do
  begin
    if TmpStr <> '' then
      TmpStr := TmpStr + ', ';
    TmpStr := TmpStr + Soci.Items[i].descrip;
  end;

  lbItem := TListBoxItem.Create(lbSocis);
  lbItem.Text := Soci.nom;
  lbItem.TagString := Soci.id;
  lbItem.ItemData.Detail := 'Malnom: ' + Soci.malnom + '  -  Rols: ' + TmpStr;
  lbSocis.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.TagString := Soci.id;
  BDel.OnClick := OnClickBDel;

  BEdit := TSpeedButton.Create(lbItem);
  BEdit.Align := TAlignLayout.Right;
  BEdit.Width := 40;
  BEdit.StyleLookup := 'composetoolbutton';
  BEdit.Parent := lbItem;
  BEdit.TagString := Soci.id;
  BEdit.OnClick := OnClickBEdit;
end;

procedure TSocisFrm.CreateItems;
var
  i: Integer;
begin
  lbSocis.BeginUpdate;
  for i := 0 to FSocis.Count do
    CreateItem(FSocis.Items[i]);
  lbSocis.EndUpdate;
end;

destructor TSocisFrm.Destroy;
begin
  if Assigned(FSocis) then
    FreeAndNil(FSocis);

  inherited;
end;

procedure TSocisFrm.OnChangeSoci(Sender: TObject);
var
  i: Integer;
begin
  if not (Sender is TSoci) then
    Exit;

  for i := 0 to lbSocis.Count - 1 do
  begin
    if lbSocis.ItemByIndex(i).TagString = TSoci(Sender).id then
    begin
      lbSocis.ItemByIndex(i).Text := TSoci(Sender).nom;
      lbSocis.ItemByIndex(i).ItemData.Detail := 'Malnom: ' + TSoci(Sender).malnom + '  -  Rols: ' + TSoci(Sender).RolsText;
      break;
    end;
  end;
end;

procedure TSocisFrm.OnClickBDel(Sender: TObject);
begin
  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol esborrar el soci "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    var
      Intf: IMainMenu;
    begin
      if Supports(Owner, IMainMenu, Intf)  then
        Intf.ShowAni(True);

      TThread.CreateAnonymousThread(procedure
      begin
        TSocis.DelSoci(TSpeedButton(Sender).TagString);

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            if Supports(Owner, IMainMenu, Intf)  then
              Intf.ShowAni(False);
          end);
      end
      ).Start;

      lbSocis.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

procedure TSocisFrm.OnClickBEdit(Sender: TObject);
var
  Intf: IMainMenu;
  Idx: Integer;
begin
  if not (Sender is TSpeedButton) then
    Exit;

  Idx := FSocis.IndexOf(TSpeedButton(Sender).TagString);
  if Idx < 0 then
    Exit;

  // si es pot, creem formulari d'assistència
  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TSociFrm, FSocis.Items[Idx]);
end;

function TSocisFrm.SetCaption: string;
begin
  Result := 'Llistat de Socis';
end;

function TSocisFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TSocisFrm.ShowOkButton: Boolean;
begin
  Result := False;
end;

end.
