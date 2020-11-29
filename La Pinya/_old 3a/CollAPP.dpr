program CollAPP;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMainFrm in 'src\UMainFrm.pas' {MainFrm},
  uIniFiles in 'src\Classes\uIniFiles.pas',
  UInterfaces in 'src\Classes\UInterfaces.pas',
  uMessage in 'src\Classes\uMessage.pas',
  uToastUnit in 'src\Classes\uToastUnit.pas',
  uBaseJson in 'src\Classes\uBaseJson.pas',
  uGenFunc in 'src\Classes\uGenFunc.pas',
  uMultiLang in 'src\Classes\uMultiLang.pas',
  uRESTMdl in 'src\Models\uRESTMdl.pas',
  uCalendars in 'src\Classes\uCalendars.pas',
  UCalendarsFrm in 'src\UCalendarsFrm.pas' {CalendarsFrm},
  UCalendarFrm in 'src\UCalendarFrm.pas' {CalendarFrm},
  uResultRequest in 'src\Classes\uResultRequest.pas',
  USeasonsFrm in 'src\USeasonsFrm.pas' {SeasonsFrm},
  USeasonFrm in 'src\USeasonFrm.pas' {SeasonFrm},
  UEventsFrm in 'src\UEventsFrm.pas' {EventsFrm},
  UEventFrm in 'src\UEventFrm.pas' {EventFrm},
  UFEvent in 'src\Frames\UFEvent.pas' {FEvent: TFrame},
  uMaps in 'src\Classes\uMaps.pas',
  uColles in 'src\Classes\uColles.pas',
  uSocis in 'src\Classes\uSocis.pas',
  uRols in 'src\Classes\uRols.pas',
  USocisFrm in 'src\USocisFrm.pas' {SocisFrm},
  USociFrm in 'src\USociFrm.pas' {SociFrm},
  URolsFrm in 'src\URolsFrm.pas' {RolsFrm},
  NetworkState in 'src\Classes\NetworkState.pas',
  NetworkState.Windows in 'src\Classes\NetworkState.Windows.pas',
  SCNetworkReachability in 'src\Classes\SCNetworkReachability.pas',
  CaptiveNetwork in 'src\Classes\CaptiveNetwork.pas',
  NetworkState.Android in 'src\Classes\NetworkState.Android.pas',
  NetworkState.iOS in 'src\Classes\NetworkState.iOS.pas',
  uGoogleCal in 'src\Classes\uGoogleCal.pas',
  uEvents in 'src\Classes\uEvents.pas',
  uSeasons in 'src\Classes\uSeasons.pas',
  UFEvLocalit in 'src\Frames\UFEvLocalit.pas' {FEvLocalit: TFrame},
  uEventSoci in 'src\Classes\uEventSoci.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
