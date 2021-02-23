{
  @abstract(Unit for show or edit a specific @link(TMember))
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(February 7, 2021)
  @lastmod(February 7, 2021)

  The UMemberFrm unit contains the information of an individual @link(TMember). You can modify this information.

  Change List @br
  @unorderedList(
    @item(02/07/2020 : first version)
  )
}
unit UMemberFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseGesFrm, FMX.Layouts, FMX.EditBox, FMX.NumberBox, FMX.ListBox,
  FMX.DateTimeCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Objects,
  FMX.TabControl,
  uMembers, uRoles;

type
  { -------------------------------------------------------------------------- }
  // @include(..\docs\help\UMemberFrm.TMemberFrm.txt)
  TMemberFrm = class(TBaseGesFrm)
    // @exclude
    tcPages: TTabControl;
    // @exclude
    tiGeneral: TTabItem;
    // @exclude
    sbGeneral: TScrollBox;
    // @exclude
    rNom: TRectangle;
    // @exclude
    lNom: TLabel;
    // @exclude
    eNom: TEdit;
    // @exclude
    rMalNom: TRectangle;
    // @exclude
    lMalNom: TLabel;
    // @exclude
    eMalNom: TEdit;
    // @exclude
    rDataNaix: TRectangle;
    // @exclude
    lDataNaix: TLabel;
    // @exclude
    eDataNaix: TDateEdit;
    // @exclude
    rMail: TRectangle;
    // @exclude
    lMail: TLabel;
    // @exclude
    eMail: TEdit;
    // @exclude
    rMobil: TRectangle;
    // @exclude
    lMobil: TLabel;
    // @exclude
    eMobil: TEdit;
    // @exclude
    rSexe: TRectangle;
    // @exclude
    lSexe: TLabel;
    // @exclude
    cbSexe: TComboBox;
    // @exclude
    rAdresa: TRectangle;
    // @exclude
    lAdresa: TLabel;
    // @exclude
    eAdresa: TEdit;
    // @exclude
    rPoblacio: TRectangle;
    // @exclude
    lPoblacio: TLabel;
    // @exclude
    ePoblacio: TEdit;
    // @exclude
    rCP: TRectangle;
    // @exclude
    lCP: TLabel;
    // @exclude
    eCP: TEdit;
    // @exclude
    rPais: TRectangle;
    // @exclude
    lPais: TLabel;
    // @exclude
    ePais: TEdit;
    // @exclude
    tiDadesCastelleres: TTabItem;
    // @exclude
    sbCastelleres: TScrollBox;
    // @exclude
    rDataBaixa: TRectangle;
    // @exclude
    lDataBaixa: TLabel;
    // @exclude
    eDataBaixa: TDateEdit;
    // @exclude
    rDataAlta: TRectangle;
    // @exclude
    lDataAlta: TLabel;
    // @exclude
    eDataAlta: TDateEdit;
    // @exclude
    rRols: TRectangle;
    // @exclude
    rRolsSelect: TRectangle;
    // @exclude
    lRols: TLabel;
    // @exclude
    cbRols: TComboBox;
    // @exclude
    bAddRol: TSpeedButton;
    // @exclude
    lbRols: TListBox;
    // @exclude
    rVinculats: TRectangle;
    // @exclude
    rVinculatsSelect: TRectangle;
    // @exclude
    lVinculats: TLabel;
    // @exclude
    cbVinculats: TComboBox;
    // @exclude
    bVinculats: TSpeedButton;
    // @exclude
    lbVinculats: TListBox;
    // @exclude
    tiDadesTecniques: TTabItem;
    // @exclude
    sbTecniques: TScrollBox;
    // @exclude
    rAltEsp: TRectangle;
    // @exclude
    lAltEsp: TLabel;
    // @exclude
    eAltEsp: TNumberBox;
    // @exclude
    rAltMa: TRectangle;
    // @exclude
    lAltMa: TLabel;
    // @exclude
    eAltMa: TNumberBox;
    // @exclude
    rIsCasteller: TRectangle;
    // @exclude
    cbIsCasteller: TCheckBox;
    // @exclude
    rIsGraller: TRectangle;
    // @exclude
    cbIsGraller: TCheckBox;
  private
    FMembers: TMembers;
    FRoles: TRoles;

    procedure AddItemRol(Role: TRole);
    procedure OnClickBDel(Sender: TObject);

    procedure GetRols;
    procedure GetSocis;
  public
    // @include(..\docs\help\UBaseGesFrm.TBaseGesFrm.SetCaption.txt)
    function SetCaption: string; override;
    // @include(..\docs\help\UBaseGesFrm.TBaseGesFrm.AcceptForm.txt)
    function AcceptForm: Boolean; override;
    // @include(..\docs\help\UBaseGesFrm.TBaseGesFrm.AfterShow.txt)
    procedure AfterShow; override;
  end;

var
  MemberFrm: TMemberFrm;

implementation

uses
  uInterfaces, uGenFunc, uMessage, uResultRequest;

{$R *.fmx}

{ TMemberFrm }

function TMemberFrm.AcceptForm: Boolean;
begin

end;

procedure TMemberFrm.AddItemRol(Role: TRole);
var
  lbItem: TListBoxItem;
  BDel: TSpeedButton;
begin
  lbItem := TListBoxItem.Create(lbRols);
  lbItem.Text := Role.Descrip;
  lbItem.TagString := Role.Id;
  lbRols.AddObject(lbItem);

  BDel := TSpeedButton.Create(lbItem);
  BDel.Align := TAlignLayout.Right;
  BDel.Width := 40;
  BDel.StyleLookup := 'trashtoolbutton';
  BDel.Parent := lbItem;
  BDel.OnClick := OnClickBDel;
end;

procedure TMemberFrm.AfterShow;
var
  i: Integer;
begin
  if not Assigned(TagObject) then Exit;
  if not (TagObject is TMember) then Exit;

  tcPages.ActiveTab := tiGeneral;

  // Carreguem Socis
  GetSocis;

  // Carreguem Rols
  GetRols;

  // dades generals
  eNom.Text := TMember(TagObject).Nom;
  eMalNom.Text := TMember(TagObject).Malnom;
  eDataNaix.Date := TGenFunc.StringToDate(TMember(TagObject).data_naix);
  eMail.Text := TMember(TagObject).Mail;
  eMobil.Text := TMember(TagObject).Mobil;
  eAdresa.Text := TMember(TagObject).Adresa;
  ePoblacio.Text := TMember(TagObject).Poblacio;
  eCP.Text := TMember(TagObject).Cp;
  ePais.Text := TMember(TagObject).Pais;
  if SameText(TMember(TagObject).Sexe, 'D') then
    cbSexe.ItemIndex := 1
  else
    cbSexe.ItemIndex := 0;

  // dades castelleres
  eDataAlta.Date := TGenFunc.StringToDate(TMember(TagObject).Data_Alta);
  eDataBaixa.Date := TGenFunc.StringToDate(TMember(TagObject).Data_Baixa);
  for i := 0 to TMember(TagObject).Rols.Count - 1 do
    AddItemRol(TMember(TagObject).Rols[i]);

  // dades tècniques
  eAltEsp.Text := TMember(TagObject).Alt_Esp;
  eAltMa.Text := TMember(TagObject).Alt_Ma;
  cbIsCasteller.IsChecked := TMember(TagObject).Is_Casteller = '1';
  cbIsGraller.IsChecked := TMember(TagObject).Is_Graller = '1';
end;

procedure TMemberFrm.GetRols;
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
      FRoles := TRoles.GetRoles;
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
          for i := 0 to FRoles.Items.Count - 1 do
            cbRols.Items.AddObject(FRoles.Items[i].Descrip, FRoles.Items[i]);
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

procedure TMemberFrm.GetSocis;
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
      FMembers := TMembers.GetMembers;
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
          for i := 0 to FMembers.Items.Count - 1 do
            if TMember(TagObject).Id <> FMembers.Items[i].Id then
              cbVinculats.Items.AddObject(FMembers.Items[i].Nom, FMembers.Items[i]);
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

procedure TMemberFrm.OnClickBDel(Sender: TObject);
begin
  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol treure el rol "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    begin
      lbRols.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

function TMemberFrm.SetCaption: string;
begin
  Result := 'Definició del Membre';
end;

end.
