// @include(..\docs\UNewUserFrm.txt)
unit UNewUserFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UAcceptFrm, FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Edit;

type
  // @include(..\docs\UNewUserFrm.TNewUserFrm.txt)
  TNewUserFrm = class(TAcceptFrm)
    {@exclude} ListBox1: TListBox;
    {@exclude} ListBoxItem1: TListBoxItem;
    {@exclude} ListBoxItem2: TListBoxItem;
    {@exclude} eUser: TEdit;
    {@exclude} ListBoxItem3: TListBoxItem;
    {@exclude} ListBoxItem4: TListBoxItem;
    {@exclude} ePass: TEdit;
    {@exclude} ListBoxItem5: TListBoxItem;
    {@exclude} ListBoxItem6: TListBoxItem;
    {@exclude} eRepPass: TEdit;
    {@exclude} PasswordEditButton1: TPasswordEditButton;
    {@exclude} PasswordEditButton2: TPasswordEditButton;
    {@exclude} ListBoxItem7: TListBoxItem;
    {@exclude} ListBoxItem8: TListBoxItem;
    {@exclude} eMail: TEdit;
    {@exclude} ClearEditButton1: TClearEditButton;
    {@exclude} ClearEditButton2: TClearEditButton;
  private
  protected
    // @include(..\docs\UNewUserFrm.TNewUserFrm.CheckValues.txt)
    function CheckValues: Boolean; override;
  public
    // @include(..\docs\UNewUserFrm.TNewUserFrm.ShowForm.txt)
    class procedure ShowForm;
  end;

var
  NewUserFrm: TNewUserFrm;

implementation

uses
  UGenFuncs, UInsResultMdl;

const
  cInsFunc = 'new_user';

{$R *.fmx}

{ TNewUserFrm }

function TNewUserFrm.CheckValues: Boolean;
begin
  Result := True;
  eUser.Text := Trim(eUser.Text);
  eMail.Text := Trim(eMail.Text);

  if eUser.Text = '' then
  begin
    ShowMessage('L''usuari no pot estar en blanc.');
    eUser.SetFocus;
    Result := False;
    Exit;
  end;

  if ePass.Text = '' then
  begin
    ShowMessage('El mot clau no pot estar en blanc.');
    eUser.SetFocus;
    Result := False;
    Exit;
  end;

  if eRepPass.Text <> ePass.Text then
  begin
    ShowMessage('Els mots claus no coincideixen.');
    eRepPass.SetFocus;
    Result := False;
    Exit;
  end;

  if eMail.Text = '' then
  begin
    ShowMessage('El correu no pot estar en blanc.');
    eMail.SetFocus;
    Result := False;
    Exit;
  end;

  if not TGenFuncs.IsValidEmail(eMail.Text) then
  begin
    ShowMessage('El correu no és vàlid.');
    eMail.SetFocus;
    Result := False;
    Exit;
  end;
end;

class procedure TNewUserFrm.ShowForm;
const
  Params = '&user=%s&pass=%s&mail=%s';
var
  F: TNewUserFrm;
  TmpStr: string;
  NewId: Integer;
begin
  F := TNewUserFrm.Create(nil);
  F.ShowModal(
    procedure(ModalR: TModalResult)
    begin
      if IsPositiveResult(ModalR) then
      begin
        TmpStr := Format(Params, [F.eUser.Text, F.ePass.Text, F.eMail.Text]);
        NewId := TInsResultMdl.InsertNewRecord(cInsFunc, TmpStr, F.aiIndicator);
        if NewId > 0 then
          ShowMessage('Uusuari creat correctament. Consulti correu per activar-ho');
      end;
    end
  );
end;

end.
