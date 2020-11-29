program CollApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMainFrm in 'src\UMainFrm.pas' {MainFrm},
  uCalendars in 'src\Classes\uCalendars.pas',
  Pkg.Json.DTO in 'src\Classes\Pkg.Json.DTO.pas',
  uInterfaces in 'src\Interfaces\uInterfaces.pas',
  uGenFunc in 'src\Classes\uGenFunc.pas',
  uMessage in 'src\Classes\uMessage.pas',
  uToastUnit in 'src\Classes\uToastUnit.pas',
  NetworkState.iOS in 'src\Classes\NetworkState.iOS.pas',
  NetworkState in 'src\Classes\NetworkState.pas',
  NetworkState.Windows in 'src\Classes\NetworkState.Windows.pas',
  NetworkState.Android in 'src\Classes\NetworkState.Android.pas',
  UCalendarsFrm in 'src\UCalendarsFrm.pas',
  UCalendarFrm in 'src\UCalendarFrm.pas',
  uResultRequest in 'src\Classes\uResultRequest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
