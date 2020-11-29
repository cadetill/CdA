unit UBaseFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  UHelpers;

type
  TBaseFrm = class(TForm)
    gplLayout: TGridPanelLayout;
  private
  protected
    procedure Ani(pAnimate: Boolean);
  public
  end;

var
  BaseFrm: TBaseFrm;

implementation

uses
  UInterfaces;

{$R *.fmx}

{ TBaseFrm }

procedure TBaseFrm.Ani(pAnimate: Boolean);
var
  Intf: IMainMenu;
begin
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(pAnimate)
  else
    gplLayout.Ani(pAnimate);
end;

end.
