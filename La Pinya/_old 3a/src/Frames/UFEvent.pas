unit UFEvent;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects,
  uEvents;

type
  TFEvent = class(TFrame)
    rCal: TRectangle;
    pTitre: TRectangle;
    lType: TLabel;
    lDesc: TLabel;
    lnCal: TLine;
    pData: TRectangle;
    rDay: TRectangle;
    lDay: TLabel;
    imgDay: TImage;
    rTime: TRectangle;
    lTime: TLabel;
    imgTime: TImage;
    rPlace: TRectangle;
    lPlace: TLabel;
    imgPlace: TImage;
    rColles: TRectangle;
    lColles: TLabel;
    imgColles: TImage;
    bEdit: TSpeedButton;
    procedure rPlaceClick(Sender: TObject);
    procedure bEditClick(Sender: TObject);
  private
    FIdEvent: string;
    FEvent: TEvent;
    procedure SetEvent(const Value: TEvent);
    procedure OnChangeEvent(Sender: TObject);
  public
    property IdEvent: string read FIdEvent write FIdEvent;
    property Event: TEvent read FEvent write SetEvent;
  end;

implementation

uses
  uMaps, UInterfaces, UEventFrm, uGenFunc, uMessage;

{$R *.fmx}

procedure TFEvent.bEditClick(Sender: TObject);
var
  Intf: IMainMenu;
begin
  if not Assigned(FEvent) then
  begin
    TMessage.Show('Event no assignat.');
    Exit;
  end;

  ShowMessage('cal canviar el TEvent per un TEventSoci quan identifiqui el Soci actiu');
  // si es pot, creem formulari d'assistència
  if Supports(Owner.Owner, IMainMenu, Intf) then
    Intf.CreateForm(TEventFrm, FEvent);
end;

procedure TFEvent.OnChangeEvent(Sender: TObject);
begin
  if not (Sender is TEvent) then
    Exit;

  lColles.Text := TEvent(Sender).NameColles;
end;

procedure TFEvent.rPlaceClick(Sender: TObject);
begin
  if lPlace.Text <> '' then
    TMaps.OpenNavigation(lPlace.Text);
end;

procedure TFEvent.SetEvent(const Value: TEvent);
begin
  FEvent := Value;
  FEvent.OnChange := OnChangeEvent;

  bEdit.Visible := TGenFunc.StringToDate(Event.dataf) >= Date;
end;

end.
