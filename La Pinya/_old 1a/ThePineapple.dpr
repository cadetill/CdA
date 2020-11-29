program ThePineapple;

uses
  System.StartUpCopy,
  FMX.Forms,
  ULoginFrm in 'src\ULoginFrm.pas' {LoginFrm},
  URESTDataMdl in 'src\REST\URESTDataMdl.pas',
  UInsResultMdl in 'src\Mdl\UInsResultMdl.pas' {InsResultMdl: TDataModule},
  UGenFuncs in 'src\Classes\UGenFuncs.pas',
  UBaseFrm in 'src\Bases\UBaseFrm.pas' {BaseFrm},
  UAcceptFrm in 'src\Bases\UAcceptFrm.pas' {AcceptFrm},
  UNewUserFrm in 'src\UNewUserFrm.pas' {NewUserFrm},
  UBaseQueryMdl in 'src\Bases\Models\UBaseQueryMdl.pas' {BaseQueryMdl: TDataModule},
  USelectQueryMdl in 'src\Mdl\USelectQueryMdl.pas' {SelectQueryMdl: TDataModule},
  UUserMdl in 'src\Mdl\UUserMdl.pas' {UserMdl: TDataModule},
  URESTMdl in 'src\REST\URESTMdl.pas' {RESTMdl: TDataModule},
  UMenuFrm in 'src\UMenuFrm.pas' {MenuFrm},
  UUsersFrm in 'src\UUsersFrm.pas' {UsersFrm},
  UConfigFrm in 'src\UConfigFrm.pas' {ConfigFrm},
  UInterfaces in 'src\Classes\UInterfaces.pas',
  UBaseChildFrm in 'src\Bases\UBaseChildFrm.pas' {BaseChildFrm},
  UHelpers in 'src\Classes\UHelpers.pas',
  UStyleMdl in 'src\Mdl\UStyleMdl.pas' {StyleMdl: TDataModule},
  UUsersGesFrm in 'src\UUsersGesFrm.pas' {UsersGesFrm},
  UFormList in 'src\Classes\UFormList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLoginFrm, LoginFrm);
  Application.CreateForm(TMenuFrm, MenuFrm);
  Application.Run;
end.
