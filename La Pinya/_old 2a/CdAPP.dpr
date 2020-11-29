program CdAPP;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMenuFrm in 'src\UMenuFrm.pas' {MenuFrm},
  UBaseModalFrm in 'src\Bases\Forms\src\UBaseModalFrm.pas' {BaseModalFrm},
  UMessage in 'src\Classes\UMessage.pas',
  USets in 'src\Classes\USets.pas',
  UToastUnit in 'src\Classes\UToastUnit.pas',
  UClasses in 'src\Classes\UClasses.pas',
  UHelpers in 'src\Classes\UHelpers.pas',
  URESTMdl in 'src\REST\URESTMdl.pas',
  UCalendarMdl in 'src\REST\UCalendarMdl.pas',
  UValidUserFrm in 'src\UValidUserFrm.pas' {ValidUserFrm},
  UIniFiles in 'src\Classes\UIniFiles.pas',
  UValidUserMdl in 'src\UValidUserMdl.pas' {ValidUserMdl: TDataModule},
  UStyleMdl in 'src\Models\UStyleMdl.pas' {StyleMdl: TDataModule},
  UInterfaces in 'src\Classes\UInterfaces.pas',
  UHomeFrm in 'src\UHomeFrm.pas' {HomeFrm},
  UHomeMdl in 'src\UHomeMdl.pas' {HomeMdl: TDataModule},
  UImagesMdl in 'src\Models\UImagesMdl.pas' {ImagesMdl: TDataModule},
  UNewsFrm in 'src\UNewsFrm.pas' {NewsFrm},
  UConfigFrm in 'src\UConfigFrm.pas' {ConfigFrm},
  USocisMdl in 'src\USocisMdl.pas' {SocisMdl: TDataModule},
  UCalendarFrm in 'src\UCalendarFrm.pas' {CalendarFrm},
  UCalFrm in 'src\Frames\UCalFrm.pas' {CalFrm: TFrame},
  UTest in 'src\UTest.pas' {TestFrm},
  UBaseFrm in 'src\Bases\Forms\src\UBaseFrm.pas' {BaseFrm},
  UAssistenciaFrm in 'src\UAssistenciaFrm.pas' {AssistenciaFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMenuFrm, MenuFrm);
  Application.CreateForm(TStyleMdl, StyleMdl);
  Application.Run;
end.
