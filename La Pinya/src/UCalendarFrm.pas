unit UCalendarFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects,
  uInterfaces;

type
  TCalendarFrm = class(TForm, IChildren)
    rKey: TRectangle;
    lKey: TLabel;
    eKey: TEdit;
    rIdCal: TRectangle;
    lIdCal: TLabel;
    eIdCal: TEdit;
    rNom: TRectangle;
    lNom: TLabel;
    eNom: TEdit;
    rNomCurt: TRectangle;
    lNomCurt: TLabel;
    eNomCurt: TEdit;
    sbData: TScrollBox;
  private
    FThreadEnd: Boolean;
    procedure ThreadTerminated(Sender: TObject);
  public
    function SetCaption: string;
    function ShowOkButton: Boolean;
    function ShowBackButton: Boolean;
    function AcceptForm: Boolean;
    procedure AfterShow;
  end;

var
  CalendarFrm: TCalendarFrm;

implementation

uses
  uCalendars, uGenFunc, uMessage;

{$R *.fmx}

{ TCalendarFrm }

function TCalendarFrm.AcceptForm: Boolean;
var
  Intf: IMainMenu;
  Thrd: TThread;
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Result := False;
    Exit;
  end;

  Result := True;

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TCalendar) then Exit;

  TCalendar(TagObject).key := eKey.Text;
  TCalendar(TagObject).idcalendar := eIdCal.Text;
  TCalendar(TagObject).nom := eNom.Text;
  TCalendar(TagObject).nom_curt := eNomCurt.Text;

  FThreadEnd := False;
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);
  Thrd := TThread.CreateAnonymousThread(procedure
  begin
    try
      TCalendars.EditCalendar(TCalendar(TagObject));
    finally
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Supports(Owner, IMainMenu, Intf)  then
            Intf.ShowAni(False);
        end);
    end;
  end
  );
  Thrd.OnTerminate := ThreadTerminated;
  Thrd.Start;

  repeat
    Sleep(50);
    Application.ProcessMessages;
  until (FThreadEnd);
//  if Assigned(TCalendar(TagObject).OnChange) then
//    TCalendar(TagObject).OnChange(TCalendar(TagObject));
end;

procedure TCalendarFrm.AfterShow;
begin
  eKey.Text := '';
  eIdCal.Text := '';
  eNom.Text := '';
  eNomCurt.Text := '';

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TCalendar) then Exit;

  eKey.Text := TCalendar(TagObject).key;
  eIdCal.Text := TCalendar(TagObject).idcalendar;
  eNom.Text := TCalendar(TagObject).nom;
  eNomCurt.Text := TCalendar(TagObject).nom_curt;
end;

function TCalendarFrm.SetCaption: string;
begin
  Result := 'Definició del Calendari';
end;

function TCalendarFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TCalendarFrm.ShowOkButton: Boolean;
begin
  Result := True;
end;

procedure TCalendarFrm.ThreadTerminated(Sender: TObject);
begin
  FThreadEnd := True;
end;

end.
