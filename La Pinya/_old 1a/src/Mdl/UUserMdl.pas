// @include(..\..\docs\UUserMdl.txt)
unit UUserMdl;

interface

uses
  System.SysUtils, System.Classes, UBaseQueryMdl, Data.DB, Datasnap.DBClient,
  FMX.StdCtrls;

type
  // @include(..\..\docs\UUserMdl.TUserMdl.txt)
  TUserMdl = class(TBaseQueryMdl)
    // @include(..\..\docs\UUserMdl.TUserMdl.cdsMail.txt)
    cdsMail: TClientDataSet;
    // @include(..\..\docs\UUserMdl.TUserMdl.cdsUsers.txt)
    cdsUsers: TClientDataSet;
  private
    FIdUser: Integer;
  public
    // @include(..\..\docs\UUserMdl.TUserMdl.IsValidUser.txt)
    function IsValidUser(FuncName, Params: string; AniInd: TAniIndicator): Boolean;
    // @include(..\..\docs\UUserMdl.TUserMdl.PosUser.txt)
    function PosUser(Id: string): Boolean;

    // @include(..\..\docs\UUserMdl.TUserMdl.GetUsersList.txt)
    procedure GetUsersList(FuncName: string; AniInd: TAniIndicator);

    // @include(..\..\docs\UUserMdl.TUserMdl.IdUser.txt)
    property IdUser: Integer read FIdUser;
  end;

var
  UserMdl: TUserMdl;

implementation

uses
  FMX.DialogService, System.UITypes, FMX.Dialogs;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TUserMdl }

procedure TUserMdl.GetUsersList(FuncName: string; AniInd: TAniIndicator);
const
  cData = 'func=%s';
var
  TmpStr: string;
begin
  if cdsUsers.Active then
    cdsUsers.Close;

  TmpStr := Format(cData, [FuncName]);
  CallServer(TmpStr, cdsUsers, AniInd);
end;

function TUserMdl.IsValidUser(FuncName, Params: string;
  AniInd: TAniIndicator): Boolean;
const
  cData = 'func=%s%s';
  cReEmail = 'func=SendActivationMail&user=%s&withinfo=true';
var
  TmpStr: string;
begin
  TmpStr := Format(cData, [FuncName, Params]);
  CallServer(TmpStr, cdsResult, AniInd);

  Result := False;

  // mirem què retorna en el CDS. Si no retorna res, error
  if cdsResult.IsEmpty then
  begin
    FIdUser := 0;
  end
  else
  begin
    FIdUser := cdsResult.FieldByName('id').AsInteger;
    if cdsResult.FieldByName('status').AsString = '' then
      Exit;

    // comprovem que l'usuari estigui actiu
    if cdsResult.FieldByName('active').AsInteger = 0 then
    begin
      // no està actiu, demanem si vol reenviar correu
      TDialogService.MessageDialog('L''usuari no està actiu i cal activar-lo. Vols que et tornem a enviar el correu d''activació?',
                                   System.UITypes.TMsgDlgType.mtWarning,
                                   [System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo],
                                   System.UITypes.TMsgDlgBtn.mbYes,
                                   0,
        procedure(const AResult: TModalResult)
        begin
          case AResult of
            mrYES:
              begin
                TmpStr := Format(cReEmail, [cdsResult.FieldByName('user').AsString]);
                CallServer(TmpStr, cdsMail, AniInd);
                //WaitUntilExit;
                if cdsMail.IsEmpty or (cdsMail.FieldByName('status').AsString <> 'ok') then
                  ShowMessage('Error enviant correu')
                else
                  ShowMessage('Correu enviat correctament.');
              end;
            mrNo: ;
          end;
        end);
    end
    else // està actiu
      Result := True;
  end;
end;

function TUserMdl.PosUser(Id: string): Boolean;
begin
  Result := False;

  if not cdsUsers.Active then Exit;

  Result := cdsUsers.Locate('id', Id, [loPartialKey]);
end;

end.
