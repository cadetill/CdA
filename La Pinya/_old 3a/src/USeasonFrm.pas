unit USeasonFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  uInterfaces;

type
  TSeasonFrm = class(TForm, IChildren)
    sbData: TScrollBox;
    rYear: TRectangle;
    lYear: TLabel;
    eYear: TEdit;
    rDescrip: TRectangle;
    lDescrip: TLabel;
    eDescrip: TEdit;
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
  SeasonFrm: TSeasonFrm;

implementation

uses
  uSeasons;

{$R *.fmx}

{ TSeasonFrm }

function TSeasonFrm.AcceptForm: Boolean;
var
  Intf: IMainMenu;
  Thrd: TThread;
begin
  Result := True;

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TSeason) then Exit;

  TSeason(TagObject).any := eYear.Text;
  TSeason(TagObject).descripcio := eDescrip.Text;

  FThreadEnd := False;
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);
  Thrd := TThread.CreateAnonymousThread(procedure
  begin
    try
      TSeasons.EditSeason(TSeason(TagObject));
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
end;

procedure TSeasonFrm.AfterShow;
begin
  eYear.Text := '';
  eDescrip.Text := '';

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TSeason) then Exit;

  eYear.Text := TSeason(TagObject).any;
  eDescrip.Text := TSeason(TagObject).descripcio;
end;

function TSeasonFrm.SetCaption: string;
begin
  Result := 'Dades de la Temporada';
end;

function TSeasonFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TSeasonFrm.ShowOkButton: Boolean;
begin
  Result := True;
end;

procedure TSeasonFrm.ThreadTerminated(Sender: TObject);
begin
  FThreadEnd := True;
end;

end.
