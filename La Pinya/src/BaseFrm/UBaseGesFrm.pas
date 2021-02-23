{
  @abstract(Unit containing the base class for management forms)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(February 7, 2021)
  @lastmod(February 7, 2021)

  The UBaseGesFrm unit contains the minimum components and methods for management forms.

  Change List @br
  @unorderedList(
    @item(02/07/2020 : first version)
  )
}
unit UBaseGesFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  uInterfaces;

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.txt)
  TBaseGesFrm = class(TForm, IChildren)
    // @exclude
    sbData: TScrollBox;
  private
  protected
    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.FThreadEnd.txt)
    FThreadEnd: Boolean;

    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.ThreadTerminated.txt)
    procedure ThreadTerminated(Sender: TObject);
  public
    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.SetCaption.txt)
    function SetCaption: string; virtual;
    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.ShowOkButton.txt)
    function ShowOkButton: Boolean; virtual;
    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.ShowBackButton.txt)
    function ShowBackButton: Boolean; virtual;
    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.AcceptForm.txt)
    function AcceptForm: Boolean; virtual;
    // @include(..\..\docs\help\UBaseGesFrm.TBaseGesFrm.AfterShow.txt)
    procedure AfterShow; virtual;
  end;

implementation

{$R *.fmx}

{ TBaseGesFrm }

function TBaseGesFrm.AcceptForm: Boolean;
begin
  Result := False;
end;

procedure TBaseGesFrm.AfterShow;
begin

end;

function TBaseGesFrm.SetCaption: string;
begin
  Result := '';
end;

function TBaseGesFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TBaseGesFrm.ShowOkButton: Boolean;
begin
  Result := True;
end;

procedure TBaseGesFrm.ThreadTerminated(Sender: TObject);
begin
  FThreadEnd := True;
end;

end.
