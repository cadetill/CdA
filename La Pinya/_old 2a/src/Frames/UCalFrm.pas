unit UCalFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation;

type
  TCalFrm = class(TFrame)
    recCal: TCalloutRectangle;
    pImages: TPanel;
    imgCalendar: TImage;
    imgTime: TImage;
    imgPlace: TImage;
    imgMeeting: TImage;
    imgBus: TImage;
    imgColles: TImage;
    pData: TPanel;
    lCalendar: TLabel;
    lTime: TLabel;
    lPlace: TLabel;
    lMeeting: TLabel;
    lBus: TLabel;
    lColles: TLabel;
    pTitre: TPanel;
    lType: TLabel;
    lDesc: TLabel;
    lnCal: TLine;
    imgEdit: TImage;
  private
    FId: string;
  public
    procedure Loaded; override;

    function GetTotalHeight: Single;
    procedure SetColorStroke(aColor: TAlphaColor);

    property Id: string read FId write FId;
  end;

implementation

{$R *.fmx}

{ TCalFrm }

function TCalFrm.GetTotalHeight: Single;
begin
  lCalendar.Visible := lCalendar.Text <> '';
  imgCalendar.Visible := lCalendar.Text <> '';

  lBus.Visible := lBus.Text <> '';
  imgBus.Visible := lBus.Text <> '';

  lMeeting.Visible := lMeeting.Text <> '';
  imgMeeting.Visible := lMeeting.Text <> '';

  lPlace.Visible := lPlace.Text <> '';
  imgPlace.Visible := lPlace.Text <> '';

  lTime.Visible := lTime.Text <> '';
  imgTime.Visible := lTime.Text <> '';

  lColles.Visible := lColles.Text <> '';
  imgColles.Visible := lColles.Text <> '';

  Result := recCal.Margins.Top + recCal.Margins.Bottom + 10 +
            pTitre.Height + pTitre.Margins.Top + pTitre.Margins.Bottom +
            lnCal.Height + lnCal.Margins.Top + lnCal.Margins.Bottom;

  if lCalendar.Visible then Result := Result + lCalendar.Height + lCalendar.Margins.Top + lCalendar.Margins.Bottom;
  if lBus.Visible then Result := Result + lBus.Height + lBus.Margins.Top + lBus.Margins.Bottom;
  if lMeeting.Visible then Result := Result + lMeeting.Height + lMeeting.Margins.Top + lMeeting.Margins.Bottom;
  if lPlace.Visible then Result := Result + lPlace.Height + lPlace.Margins.Top + lPlace.Margins.Bottom;
  if lTime.Visible then Result := Result + lTime.Height + lTime.Margins.Top + lTime.Margins.Bottom;
  if lColles.Visible then Result := Result + lColles.Height + lColles.Margins.Top + lColles.Margins.Bottom;
end;

procedure TCalFrm.Loaded;
begin
  inherited;

  lType.Text := '';
  lDesc.Text := '';
  lCalendar.Text := '';
  lBus.Text := '';
  lMeeting.Text := '';
  lPlace.Text := '';
  lTime.Text := '';
  lColles.Text := '';
end;

procedure TCalFrm.SetColorStroke(aColor: TAlphaColor);
begin
  recCal.Stroke.Color := aColor;
  if aColor = TAlphaColorRec.Black then
    recCal.Stroke.Thickness := 1
  else
    recCal.Stroke.Thickness := 3;
end;

end.
