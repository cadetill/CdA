// @include(..\..\docs\UAcceptFrm.txt)
unit UAcceptFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseFrm, FMX.Controls.Presentation;

type
  // @include(..\..\docs\UAcceptFrm.TAcceptFrm.txt)
  TAcceptFrm = class(TBaseFrm)
    // @include(..\..\docs\UAcceptFrm.TAcceptFrm.bOk.txt)
    bOk: TButton;
    // @include(..\..\docs\UAcceptFrm.TAcceptFrm.aiIndicator.txt)
    aiIndicator: TAniIndicator;
    // @include(..\..\docs\UAcceptFrm.TAcceptFrm.bOkClick.txt)
    procedure bOkClick(Sender: TObject);
  private
  protected
    // @include(..\..\docs\UAcceptFrm.TAcceptFrm.CheckValues.txt)
    function CheckValues: Boolean; virtual; abstract;
  public
  end;

var
  AcceptFrm: TAcceptFrm;

implementation

{$R *.fmx}

procedure TAcceptFrm.bOkClick(Sender: TObject);
begin
  inherited;

  if not CheckValues then
    Abort;
end;

end.
