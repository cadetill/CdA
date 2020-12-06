{
  @abstract(Unit to manage messages showed to the user)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 11, 2020)
  @lastmod(October 11, 2020)

  The uMessage unit contains the definition and implementation of @link(TMessage) class. This class manage messages.

  @bold(Change List) @br
  @unorderedList(
    @item(11/10/2020 : first version)
  )
}
unit uMessage;

interface

uses
  FMX.Dialogs, System.Classes, System.SysUtils,
  uToastUnit;

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uMessage.TMessage.txt)
  TMessage = class
  public
    // @include(..\..\docs\help\uMessage.TMessage.Show.txt)
    class procedure Show(const Msj: string);
    // @include(..\..\docs\help\uMessage.TMessage.MsjErr.txt)
    class procedure MsjErr(const Msj: string; const Args: array of const);
    // @include(..\..\docs\help\uMessage.TMessage.MsjAttn.txt)
    class procedure MsjAttn(const Msj: string; const Args: array of const);
    // @include(..\..\docs\help\uMessage.TMessage.MsjInfo.txt)
    class procedure MsjInfo(const Msj: string; const Args: array of const);
    // @include(..\..\docs\help\uMessage.TMessage.MsjSiNo.txt)
    class procedure MsjSiNo(const Msj: string; const Args: array of const; YesProc: TProc = nil; NoProc: TProc = nil);
  end;

implementation

uses
  FMX.DialogService, System.UITypes;

{ TMessage }

class procedure TMessage.MsjAttn(const Msj: string; const Args: array of const);
begin
  TDialogService.MessageDialog(Format(Msj, Args), TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, nil);
end;

class procedure TMessage.MsjErr(const Msj: string; const Args: array of const);
begin
  TDialogService.MessageDialog(Format(Msj, Args), TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, nil);
end;

class procedure TMessage.MsjInfo(const Msj: string; const Args: array of const);
begin
  TDialogService.MessageDialog(Format(Msj, Args), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0, nil);
end;

class procedure TMessage.MsjSiNo(const Msj: string;
  const Args: array of const; YesProc: TProc; NoProc: TProc);
begin
  TDialogService.MessageDialog(Format(Msj, Args), System.UITypes.TMsgDlgType.mtConfirmation,
      [System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo],
      System.UITypes.TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYES: if Assigned(YesProc) then YesProc;
          mrNo: if Assigned(NoProc) then NoProc;
        end;
      end);
end;

class procedure TMessage.Show(const Msj: string);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      {$IFDEF ANDROID}
      uToastUnit.Toast(Msj, TToastLength.LongToast);
      {$ELSE}
      TMessage.MsjAttn(Msj, []);
      {$ENDIF}
    end);
end;

end.
