unit UFEvLocalit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.DateTimeCtrls, FMX.Objects, FMX.Edit, FMX.Controls.Presentation;

type
  TFEvLocalit = class(TFrame)
    rTitol: TRectangle;
    lTitol: TLabel;
    eTitol: TEdit;
    rMeeting: TRectangle;
    imgMeeting: TImage;
    eMeeting: TEdit;
    rDateMeeting: TRectangle;
    eDateMeeting: TDateEdit;
    imgIDateMeeting: TImage;
    rTimeMeeting: TRectangle;
    eTimeMeeting: TTimeEdit;
    imgTimeMeeting: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
