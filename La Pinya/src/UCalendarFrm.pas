{
  @abstract(Unit for show or edit a specific @link(TCalendar))
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 10, 2020)
  @lastmod(October 10, 2020)

  The UCalendarFrm unit contains the information of an individual @link(TCalendar). You can modify this information.

  Change List @br
  @unorderedList(
    @item(10/10/2020 : first version)
  )
}
unit UCalendarFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects,
  UBaseGesFrm;

type
  { -------------------------------------------------------------------------- }
  // @include(..\docs\help\UCalendarFrm.TCalendarFrm.txt)
  TCalendarFrm = class(TBaseGesFrm)
    // @exclude
    rKey: TRectangle;
    // @exclude
    lKey: TLabel;
    // @exclude
    eKey: TEdit;
    // @exclude
    rIdCal: TRectangle;
    // @exclude
    lIdCal: TLabel;
    // @exclude
    eIdCal: TEdit;
    // @exclude
    rNom: TRectangle;
    // @exclude
    lNom: TLabel;
    // @exclude
    eNom: TEdit;
    // @exclude
    rNomCurt: TRectangle;
    // @exclude
    lNomCurt: TLabel;
    // @exclude
    eNomCurt: TEdit;
  protected
    // @include(..\docs\help\UCalendarFrm.TCalendarFrm.DoCheck.txt)
    function DoCheck: Boolean;
  public
    // @include(..\docs\help\UBaseGesFrm.TBaseGesFrm.SetCaption.txt)
    function SetCaption: string; override;
    // @include(..\docs\help\UBaseGesFrm.TBaseGesFrm.AcceptForm.txt)
    function AcceptForm: Boolean; override;
    // @include(..\docs\help\UBaseGesFrm.TBaseGesFrm.AfterShow.txt)
    procedure AfterShow; override;
  end;

implementation

uses
  uCalendars, uGenFunc, uMessage, uResultRequest, uInterfaces;

{$R *.fmx}

{ TCalendarFrm }

function TCalendarFrm.AcceptForm: Boolean;
var
  Intf: IMainMenu;
  Thrd: TThread;
begin
  Result := DoCheck;

  if not Result then
    Exit;

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
  var
    Resp: TResultRequest;
  begin
    try
      Resp := TCalendars.EditCalendar(TCalendar(TagObject));
      if Resp.Error <> '' then
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show('Error: ' + Resp.Error + #13 + 'Param: ' + Resp.ErrParam);
          end);
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
  Thrd.FreeOnTerminate := True;
  Thrd.Start;

  repeat
    Sleep(50);
    Application.ProcessMessages;
  until (FThreadEnd);
  if Assigned(TCalendar(TagObject).OnChange) then
    TCalendar(TagObject).OnChange(TCalendar(TagObject));
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

function TCalendarFrm.DoCheck: Boolean;
begin
  Result := (eNom.Text <> '') and
            (eNomCurt.Text <> '') and
            (eKey.Text <> '') and
            (eIdCal.Text <> '');
  if not Result then
  begin
    TMessage.Show('Falta algun camp');
    Exit;
  end;

  Result := TGenFunc.IsConnected;
  if not Result then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;
end;

function TCalendarFrm.SetCaption: string;
begin
  Result := 'Definició del Calendari';
end;

end.
