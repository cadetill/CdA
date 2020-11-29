unit UBaseChildFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  UInterfaces, UFormList;

type
  TBaseChildFrm = class(TForm, ILoadData, IFormList)
    lBase: TLayout;
  private
  protected
    FFrmList: TFormList;

    procedure LoadData; virtual;
  public
    procedure SetFormList(FrmList: TFormList);
  end;

var
  BaseChildFrm: TBaseChildFrm;

implementation

{$R *.fmx}

{ TBaseChildFrm }

procedure TBaseChildFrm.LoadData;
begin
  // a implementar en els fills
end;

procedure TBaseChildFrm.SetFormList(FrmList: TFormList);
begin
  FFrmList := FrmList;
end;

end.
