// @include(..\docs\ULoginFrm.txt)
unit ULoginFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Objects;

type
  // @include(..\docs\ULoginFrm.TLoginFrm.txt)
  TLoginFrm = class(TForm)
    {@exclude} lUser: TLabel;
    {@exclude} lPassword: TLabel;
    {@exclude} bGo: TButton;
    {@exclude} eUser: TEdit;
    {@exclude} ePassword: TEdit;
    {@exclude} cbRemember: TCheckBox;
    {@exclude} pData: TPanel;
    {@exclude} imgLogo: TImage;
    {@exclude} StyleBook1: TStyleBook;
    {@exclude} bRegister: TButton;
    {@exclude} PasswordEditButton1: TPasswordEditButton;
    {@exclude} procedure FormResize(Sender: TObject);
    // @include(..\docs\ULoginFrm.TLoginFrm.bRegisterClick.txt)
    procedure bRegisterClick(Sender: TObject);
    // @include(..\docs\ULoginFrm.TLoginFrm.bGoClick.txt)
    procedure bGoClick(Sender: TObject);
  private
  public
    // @include(..\docs\ULoginFrm.TLoginFrm.Create.txt)
    constructor Create(AOwner: TComponent); override;
    // @include(..\docs\ULoginFrm.TLoginFrm.Destroy.txt)
    destructor Destroy; override;
  end;

var
  LoginFrm: TLoginFrm;

implementation

uses
  UNewUserFrm, UUserMdl, UMenuFrm, UStyleMdl;

const
  cValidUserFunc = 'IsValidUser';

{$R *.fmx}

{ TForm2 }

procedure TLoginFrm.bGoClick(Sender: TObject);
const
  Params = '&user=%s&pass=%s';
var
  TmpStr: string;
begin
  TmpStr := Format(Params, [eUser.Text, ePassword.Text]);
  if UserMdl.IsValidUser(cValidUserFunc, TmpStr, nil) then
  begin
    TMenuFrm.ShowForm;
  end
  else
  begin
    ShowMessage('Usuari innexistent o mot clau erroni.');
  end;
end;

procedure TLoginFrm.bRegisterClick(Sender: TObject);
begin
  TNewUserFrm.ShowForm;
end;

constructor TLoginFrm.Create(AOwner: TComponent);
begin
  inherited;

  UserMdl := TUserMdl.Create(Self);
  StyleMdl := TStyleMdl.Create(Self);
end;

destructor TLoginFrm.Destroy;
begin
  if Assigned(UserMdl) then
    FreeAndNil(UserMdl);

  inherited;
end;

procedure TLoginFrm.FormResize(Sender: TObject);
begin
  pData.Position.Y := (Height - pData.Height) / 2;
  pData.Width := Width;
  eUser.Width := Width - 140;
  ePassword.Width := pData.Width - 140;
  cbRemember.Width := pData.Width - 40;
  bGo.Width := pData.Width - 40;
  bRegister.Width := pData.Width - 40;
  imgLogo.Position.X := (Width - imgLogo.Width) / 2;
end;

end.
