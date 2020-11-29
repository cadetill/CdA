unit UUsersGesFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseChildFrm, FMX.Layouts,
  UInterfaces, FMX.Controls.Presentation, FMX.ListBox;

type
  TUsersGesFrm = class(TBaseChildFrm, ILoadData)
    Layout1: TLayout;
    lUser: TLabel;
    lRols: TLabel;
    ListBox1: TListBox;
    Label1: TLabel;
  private
  protected
    // @include(..\docs\UUsersGesFrm.TUsersGesFrm.LoadData.txt)
    procedure LoadData; override;
  public
  end;

var
  UsersGesFrm: TUsersGesFrm;

implementation

{$R *.fmx}

end.
