unit UBaseModalFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  UHelpers;

type
  TBaseModalFrm = class(TForm)
    lyMain: TLayout;
  private
  protected
    procedure Ani(pAnimate: Boolean);
  public
  end;

var
  BaseModalFrm: TBaseModalFrm;

implementation

uses
  UStyleMdl, UInterfaces;

{$R *.fmx}

{ TBaseModalFrm }

procedure TBaseModalFrm.Ani(pAnimate: Boolean);
var
  Intf: IMainMenu;
begin
  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(pAnimate)
  else
    lyMain.Ani(pAnimate);
end;

end.
