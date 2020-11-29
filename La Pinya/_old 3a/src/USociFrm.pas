unit USociFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  FMX.TabControl, FMX.DateTimeCtrls, FMX.ListBox, FMX.EditBox, FMX.NumberBox,
  uInterfaces, uRols, uSocis;

type
  TSociFrm = class(TForm, IChildren)
    sbGeneral: TScrollBox;
    rNom: TRectangle;
    lNom: TLabel;
    eNom: TEdit;
    rMalNom: TRectangle;
    lMalNom: TLabel;
    eMalNom: TEdit;
    tcPages: TTabControl;
    tiGeneral: TTabItem;
    rDataNaix: TRectangle;
    lDataNaix: TLabel;
    eDataNaix: TDateEdit;
    tiDadesCastelleres: TTabItem;
    sbCastelleres: TScrollBox;
    rDataBaixa: TRectangle;
    lDataBaixa: TLabel;
    eDataBaixa: TDateEdit;
    rDataAlta: TRectangle;
    lDataAlta: TLabel;
    eDataAlta: TDateEdit;
    rMail: TRectangle;
    lMail: TLabel;
    eMail: TEdit;
    rMobil: TRectangle;
    lMobil: TLabel;
    eMobil: TEdit;
    rIsCasteller: TRectangle;
    cbIsCasteller: TCheckBox;
    rIsGraller: TRectangle;
    cbIsGraller: TCheckBox;
    rSexe: TRectangle;
    lSexe: TLabel;
    cbSexe: TComboBox;
    tiDadesTecniques: TTabItem;
    sbTecniques: TScrollBox;
    rAltEsp: TRectangle;
    lAltEsp: TLabel;
    eAltEsp: TNumberBox;
    rAltMa: TRectangle;
    lAltMa: TLabel;
    eAltMa: TNumberBox;
    rRolsSelect: TRectangle;
    lRols: TLabel;
    cbRols: TComboBox;
    rRols: TRectangle;
    lbRols: TListBox;
    bAddRol: TSpeedButton;
    rAdresa: TRectangle;
    lAdresa: TLabel;
    eAdresa: TEdit;
    rPoblacio: TRectangle;
    lPoblacio: TLabel;
    ePoblacio: TEdit;
    rCP: TRectangle;
    lCP: TLabel;
    eCP: TEdit;
    rPais: TRectangle;
    lPais: TLabel;
    ePais: TEdit;
    rVinculats: TRectangle;
    rVinculatsSelect: TRectangle;
    lVinculats: TLabel;
    cbVinculats: TComboBox;
    bVinculats: TSpeedButton;
    lbVinculats: TListBox;
    procedure bAddRolClick(Sender: TObject);
    procedure bVinculatsClick(Sender: TObject);
  private
    FSocis: TSocis;
    FRols: TRols;
    FThreadEnd: Boolean;

    procedure ThreadTerminated(Sender: TObject);
    procedure GetRols;
    procedure GetSocis;
    procedure AddItemRol(Rol: TRol);
    procedure AddItemVinculat(Soci: TSoci);
    procedure OnClickBDel(Sender: TObject);
    procedure OnClickBDelVinculat(Sender: TObject);
  public
    function SetCaption: string;
    function ShowOkButton: Boolean;
    function ShowBackButton: Boolean;
    function AcceptForm: Boolean;
    procedure AfterShow;
  end;

var
  SociFrm: TSociFrm;

implementation

uses
  uGenFunc, uMessage, uResultRequest;

{$R *.fmx}

{ TSociFrm }

function TSociFrm.AcceptForm: Boolean;
var
  Intf: IMainMenu;
  Thrd: TThread;
  i: Integer;
  TmpStr: string;
  TmpStrName: string;
  RR: TResultRequest;
begin
  Result := True;

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TSoci) then Exit;

  // assignem dades generals
  TSoci(TagObject).nom := eNom.Text;
  TSoci(TagObject).malnom := eMalNom.Text;
  TSoci(TagObject).data_naix := FormatDateTime('yyyy-mm-dd', eDataNaix.Date);
  TSoci(TagObject).mail := eMail.Text;
  TSoci(TagObject).mobil := eMobil.Text;
  TSoci(TagObject).adresa := eAdresa.Text;
  TSoci(TagObject).poblacio := ePoblacio.Text;
  TSoci(TagObject).cp := eCP.Text;
  TSoci(TagObject).pais := ePais.Text;
  if cbSexe.ItemIndex = 0 then
    TSoci(TagObject).sexe := 'H'
  else
    TSoci(TagObject).sexe := 'D';

  // assignem dades castelleres
  TSoci(TagObject).data_alta := FormatDateTime('yyyy-mm-dd', eDataAlta.Date);
  TSoci(TagObject).data_baixa := FormatDateTime('yyyy-mm-dd', eDataBaixa.Date);
  TmpStr := '';
  TmpStrName := '';
  for i := 0 to lbRols.Count - 1 do
  begin
    if TmpStr <> '' then
    begin
      TmpStr := TmpStr + ',';
      TmpStrName := TmpStrName + ', ';
    end;

    TmpStr := TmpStr + lbRols.ItemByIndex(i).TagString + ';' + lbRols.ItemByIndex(i).Text;
    TmpStrName := TmpStrName + lbRols.ItemByIndex(i).Text;
  end;
  TSoci(TagObject).rols := TmpStr;
  TSoci(TagObject).RolsText := TmpStrName;
  TmpStr := '';
  for i := 0 to lbVinculats.Count - 1 do
  begin
    if TmpStr <> '' then
      TmpStr := TmpStr + ',';

    TmpStr := TmpStr + lbVinculats.ItemByIndex(i).TagString;
  end;
  TSoci(TagObject).vinculats := TmpStr;

  // assignem dades tècniques
  TSoci(TagObject).alt_esp := eAltEsp.Text;
  TSoci(TagObject).alt_ma := eAltMa.Text;
  if cbIsCasteller.IsChecked then
    TSoci(TagObject).is_casteller := '1'
  else
    TSoci(TagObject).is_casteller := '0';
  if cbIsGraller.IsChecked then
    TSoci(TagObject).is_graller := '1'
  else
    TSoci(TagObject).is_graller := '0';

  RR := nil;
  // actualitzem dades
  FThreadEnd := False;
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);
  Thrd := TThread.CreateAnonymousThread(procedure
  begin
    try
      RR := TSocis.EditSoci(TSoci(TagObject));
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
    TMessage.MsjErr('Error actualitzant soci. Missatge d''error: %s', [RR.Error])
  else
    TMessage.Show('Soci actualitzat.');
end;

procedure TSociFrm.AddItemRol(Rol: TRol);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
begin
  lbItem := TListBoxItem.Create(lbRols);
  lbItem.Text := Rol.descrip;
  lbItem.TagString := Rol.id;
  lbRols.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.OnClick := OnClickBDel;
end;

procedure TSociFrm.AddItemVinculat(Soci: TSoci);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
begin
  lbItem := TListBoxItem.Create(lbVinculats);
  lbItem.Text := Soci.nom;
  lbItem.TagString := Soci.id;
  lbVinculats.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.OnClick := OnClickBDelVinculat;
end;

procedure TSociFrm.AfterShow;
var
  i: Integer;
begin
  if not Assigned(TagObject) then Exit;
  if not (TagObject is TSoci) then Exit;

  tcPages.ActiveTab := tiGeneral;

  // Carreguem Socis
  GetSocis;

  // Carreguem Rols
  GetRols;

  // dades generals
  eNom.Text := TSoci(TagObject).nom;
  eMalNom.Text := TSoci(TagObject).malnom;
  eDataNaix.Date := TGenFunc.StringToDate(TSoci(TagObject).data_naix);
  eMail.Text := TSoci(TagObject).mail;
  eMobil.Text := TSoci(TagObject).mobil;
  eAdresa.Text := TSoci(TagObject).adresa;
  ePoblacio.Text := TSoci(TagObject).poblacio;
  eCP.Text := TSoci(TagObject).cp;
  ePais.Text := TSoci(TagObject).pais;
  if SameText(TSoci(TagObject).sexe, 'D') then
    cbSexe.ItemIndex := 1
  else
    cbSexe.ItemIndex := 0;

  // dades castelleres
  eDataAlta.Date := TGenFunc.StringToDate(TSoci(TagObject).data_alta);
  eDataBaixa.Date := TGenFunc.StringToDate(TSoci(TagObject).data_baixa);
  for i := 0 to TSoci(TagObject).Count do
    AddItemRol(TSoci(TagObject).Items[i]);

  // dades tècniques
  eAltEsp.Text := TSoci(TagObject).alt_esp;
  eAltMa.Text := TSoci(TagObject).alt_ma;
  cbIsCasteller.IsChecked := TSoci(TagObject).is_casteller = '1';
  cbIsGraller.IsChecked := TSoci(TagObject).is_graller = '1';
end;

procedure TSociFrm.bAddRolClick(Sender: TObject);
begin
  if cbRols.ItemIndex = -1 then
    Exit;

  if lbRols.Items.IndexOf(TRol(cbRols.Items.Objects[cbRols.ItemIndex]).descrip) <> -1 then
    Exit;

  AddItemRol(TRol(cbRols.Items.Objects[cbRols.ItemIndex]));
end;

procedure TSociFrm.bVinculatsClick(Sender: TObject);
begin
  if cbVinculats.ItemIndex = -1 then
    Exit;

  if lbVinculats.Items.IndexOf(TSoci(cbVinculats.Items.Objects[cbVinculats.ItemIndex]).nom) <> -1 then
    Exit;

  AddItemVinculat(TSoci(cbVinculats.Items.Objects[cbVinculats.ItemIndex]));
end;

procedure TSociFrm.GetRols;
var
  Intf: IMainMenu;
begin
  // Carreguem Rols
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  var
    Res: TResultRequest;
  begin
    Res := TResultRequest.Create;
    try
      FRols := TRols.GetRols(Res);
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
        var
          i: Integer;
        begin
          cbRols.Clear;
          for i := 0 to FRols.Count do
            cbRols.Items.AddObject(FRols.Items[i].descrip, FRols.Items[i]);
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

procedure TSociFrm.GetSocis;
var
  Intf: IMainMenu;
begin
  // Carreguem Rols
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
        var
          i: Integer;
        begin
          cbVinculats.Clear;
          for i := 0 to FSocis.Count do
            if TSoci(TagObject).id <> FSocis.Items[i].id then
              cbVinculats.Items.AddObject(FSocis.Items[i].nom, FSocis.Items[i]);
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

procedure TSociFrm.OnClickBDel(Sender: TObject);
begin
  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol treure el rol "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    begin
      lbRols.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

procedure TSociFrm.OnClickBDelVinculat(Sender: TObject);
begin
  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol desvincular el soci "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    begin
      lbVinculats.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

function TSociFrm.SetCaption: string;
begin
  Result := 'Gestió del Soci';
end;

function TSociFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TSociFrm.ShowOkButton: Boolean;
begin
  Result := True;
end;

procedure TSociFrm.ThreadTerminated(Sender: TObject);
begin
  FThreadEnd := True;
end;

end.
