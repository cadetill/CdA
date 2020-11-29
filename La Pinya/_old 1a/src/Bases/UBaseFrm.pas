// @include(..\..\docs\UBaseFrm.txt)
unit UBaseFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  // @include(..\..\docs\UBaseFrm.TBaseFrm.txt)
  TBaseFrm = class(TForm)
    {@exclude} sbStyles: TStyleBook;
    {@exclude} pHeader: TToolBar;
    {@exclude} bBack: TButton;
    {@exclude} pFooter: TToolBar;
    {@exclude} procedure bBackClick(Sender: TObject);
    {@exclude} procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
    // @include(..\..\docs\UBaseFrm.TBaseFrm.BackControl.txt)
    procedure BackControl; virtual;
  public
  end;

var
  BaseFrm: TBaseFrm;

implementation

{$R *.fmx}

procedure TBaseFrm.BackControl;
begin
  Close;
end;

procedure TBaseFrm.bBackClick(Sender: TObject);
begin
  BackControl;
end;

procedure TBaseFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

end.
