unit UValidUserFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.DateTimeCtrls,
  FMX.TabControl,
  UBaseModalFrm, UInterfaces;

const
  cErrGetingId = 'Error obtenint ID dispositiu vàlid.';
  cErrGettingValidCode = 'Error obtenint el Codi de Validació.';
  cErrValCode = 'Codi de Validació introduit erroni.';
  cErrMailNotDefined = 'Falta correu electrònic.';
  cErrDateNotDefined = 'Falta data de naixement.';
  cErrMobileNotDefined = 'Falta número de telèfon.';
  cErrIncorrectData = 'Les dades no corresponen a cap usuari. Contacti amb el responsable de la colla.';
  cInfoNewId = 'ID del nou dispositiu: %s';
  cSeeMail = 'Revisa el correu per saber el Codi d''Activació';
  cCaption = 'Benvingut/da a la CdAPP';

type
  TValidUserFrm = class(TBaseModalFrm, IChildren)
    lHeader1: TLabel;
    lHeader2: TLabel;
    lHeader3: TLabel;
    lMail: TLabel;
    eMail: TEdit;
    lDate: TLabel;
    eDate: TDateEdit;
    lMobile: TLabel;
    eMobile: TEdit;
    bCode: TButton;
    lNoCode: TLabel;
    tcFooter: TTabControl;
    tiButton: TTabItem;
    tiEdit: TTabItem;
    eValCode: TEdit;
    lValCode: TLabel;
    procedure bCodeClick(Sender: TObject);
  private
    FValidCode: string;

    function CreateDefaultIni(MsgCtrl: Boolean): Boolean;
    function CheckIdValue: Boolean;

    procedure SetValidCode(const Value: string);
  protected
    property ValidCode: string read FValidCode write SetValidCode;
  public
    constructor Create(AOwner: TComponent); override;

    function SetCaption: string;
    function DeactivateAll: Boolean;
    function AcceptForm: Boolean;
    function EnabledBackButton: Boolean;
    function DefaultStateAcceptButton: Boolean;
    procedure AfterShow;
  end;

var
  ValidUserFrm: TValidUserFrm;

implementation

uses
  UClasses, UIniFiles, UValidUserMdl, UMessage;

{$R *.fmx}

{ TConfigFrm }

function TValidUserFrm.AcceptForm: Boolean;
var
  L: TStringList;
  Intf: IMainMenu;
begin
  Result := False;

  // inicialitzem ActCode de l'objecte passat si s'escau
  if (TagObject <> nil) and (TagObject is TItemConfig) then
    TItemConfig(TagObject).ActCode := '';

  if FValidCode <> eValCode.Text then
  begin
    TMessage.Show(cErrValCode);
    eValCode.Text := '';
    eValCode.SetFocus;
  end
  else
  begin
    // grabem usuari (el seu Valid Code que ha de ser únic)
    TFileIni.SetFileIni(TGenFunc.GetIniName);
    L := TStringList.Create;
    try
      TFileIni.GetSection('USERS', L);
      TFileIni.SetStrValue('USERS', IntToStr(L.Count), FValidCode);

      // si és el primer usuari creat, establim menú i opcions d'avís per defecte
      if L.Count = 0 then // no hi ha usuari definit
      begin
        TFileIni.SetBolValue('NOTIFICACIONS', 'DIADES', True);
        TFileIni.SetBolValue('NOTIFICACIONS', 'ASSAJOS', True);
        TFileIni.SetBolValue('NOTIFICACIONS', 'EVENTS', True);
        TFileIni.SetBolValue('NOTIFICACIONS', 'NOTICIES', True);

        if Supports(Owner, IMainMenu, Intf) then
          Intf.MenuControl;
      end;
    finally
      FreeAndNil(L);
    end;

    if (TagObject <> nil) and (TagObject is TItemConfig) then
    begin
      TItemConfig(TagObject).ActCode := eValCode.Text;
      if Assigned(TItemConfig(TagObject).OnChange) then
        TItemConfig(TagObject).OnChange(TItemConfig(TagObject));
    end;

    Result := True;
  end;
end;

procedure TValidUserFrm.AfterShow;
begin
  lNoCode.Visible := not Assigned(TagObject);
end;

procedure TValidUserFrm.bCodeClick(Sender: TObject);
var
  Id: string;
  TmpI: Integer;
  Intf: IMainMenu;
begin
  inherited;

  if eMail.Text = '' then
  begin
    TMessage.Show(cErrMailNotDefined);
    eMail.SetFocus;
    Exit;
  end;

  if eDate.Text = '' then
  begin
    TMessage.Show(cErrDateNotDefined);
    eDate.SetFocus;
    Exit;
  end;

  if eMobile.Text = '' then
  begin
    TMessage.Show(cErrMobileNotDefined);
    eMobile.SetFocus;
    Exit;
  end;

  // agafem ID dispositiu
  TFileIni.SetFileIni(TGenFunc.GetIniName);
  Id := TFileIni.GetStrValue('GENERAL', 'ID');

  Ani(True);
  TThread.CreateAnonymousThread(procedure
  begin
    // comprovem que l'ID sigui vàlid
    if not TryStrToInt(Id, TmpI) or (TmpI = -1) then
    begin  // no és vàlid
      if not CreateDefaultIni(True) then  // intentem demanar-lo novament. Si no es pot aconseguir, mostrem error i marxem
      begin
        TMessage.Show(cErrGetingId);
        Exit;
      end;
    end;

    // si arriba aquí, tenim un ID vàlid, per tant, comprovem usuari
    try
      ValidCode := TValidUserMdl.GetValidationCode(Id, eMail.Text, eMobile.Text, FormatDateTime('yyyy-mm-dd', eDate.Date));
    except
      on E: Exception do
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(E.Message);
          end);
    end;

    // ocultem animació
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        if ValidCode <> '' then
        begin
          if Supports(Owner, IMainMenu, Intf) then
            Intf.ShowAcceptButton(True);

          TMessage.Show(cSeeMail)
        end
        else
          TMessage.Show(cErrIncorrectData);
        Ani(False);
      end);
  end
  ).Start;
end;

function TValidUserFrm.CheckIdValue: Boolean;
var
  Id: string;
  TmpI: Integer;
begin
  Result := True;

  TFileIni.SetFileIni(TGenFunc.GetIniName);
  Id := TFileIni.GetStrValue('GENERAL', 'ID');

  if not TryStrToInt(Id, TmpI) or (TmpI = -1) then
    Result := CreateDefaultIni(False);
end;

constructor TValidUserFrm.Create(AOwner: TComponent);
begin
  inherited;

  FValidCode := '';
  tcFooter.ActiveTab := tiButton;
  {$IFDEF MSWINDOWS}
  eMail.KeyboardType:= TVirtualKeyboardType.Default;
  eDate.KeyboardType := TVirtualKeyboardType.Default;
  eMobile.KeyboardType := TVirtualKeyboardType.Default;
  {$ENDIF}
  eDate.Date := Date;

  if not FileExists(TGenFunc.GetIniName) then
    CreateDefaultIni(False)
  else
    CheckIdValue;
end;

function TValidUserFrm.CreateDefaultIni(MsgCtrl: Boolean): Boolean;
var
  Id: string;
  TmpI: Integer;
  TmpB: Boolean;
  Start: Boolean;
begin
  TFileIni.SetFileIni(TGenFunc.GetIniName);

  if MsgCtrl then
    Ani(True);
  Start := True;
  TmpB := False;
  TThread.CreateAnonymousThread(procedure
  begin
    Id := TValidUserMdl.GetDeviceId(TGenFunc.GetDeviceId, TGenFunc.GetOperatingSystem);
    TmpB := (Id <> '-1') and TryStrToInt(Id, TmpI);

    if MsgCtrl then
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if not TmpB then
            TMessage.MsjErr(cErrGetingId, [])
          else
            TMessage.Show(Format(cInfoNewId, [Id]));
        end);

    TFileIni.SetStrValue('GENERAL', 'ID', Id);

    if MsgCtrl then
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Ani(False);
        end);

    Start := False;
  end
  ).Start;

  while Start do ;   // tinc dos opcions, o no ho faig amb fils, o faig aquesta guarrada

  Result := TmpB;
end;

function TValidUserFrm.DeactivateAll: Boolean;
begin
  Result := True;
end;

function TValidUserFrm.DefaultStateAcceptButton: Boolean;
begin
  Result := False;
end;

function TValidUserFrm.EnabledBackButton: Boolean;
begin
  Result := False;
end;

function TValidUserFrm.SetCaption: string;
begin
  Result := cCaption;
end;

procedure TValidUserFrm.SetValidCode(const Value: string);
begin
  FValidCode := Value;

  if FValidCode = '' then // ha hagut un error en l'assignació
  begin
    TMessage.MsjErr(cErrGettingValidCode, []);
    tcFooter.ActiveTab := tiButton;
    Exit;
  end;

  tcFooter.ActiveTab := tiEdit;
end;

end.
