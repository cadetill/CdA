program Project2;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  UHelpers in '..\src\Classes\UHelpers.pas',
  UCalendarMdl in '..\src\REST\UCalendarMdl.pas' {CalendarMdl: TDataModule},
  URESTMdl in '..\src\REST\URESTMdl.pas' {RESTMdl: TDataModule},
  UToastUnit in '..\src\Classes\UToastUnit.pas',
  UMessage in '..\src\Classes\UMessage.pas',
  UClasses in '..\src\Classes\UClasses.pas',
  USets in '..\src\Classes\USets.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TRESTMdl, RESTMdl);
  Application.Run;
end.
