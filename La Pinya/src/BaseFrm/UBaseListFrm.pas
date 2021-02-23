{
  @abstract(Base unit for list items)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(February 7, 2021)
  @lastmod(February 7, 2021)

  The UBaseListFrm unit contains base components and methods for list items from a REST request.

  Change List @br
  @unorderedList(
    @item(02/07/2020 : first version)
  )
}
unit UBaseListFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.SearchBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts,
  uInterfaces;

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.txt)
  TBaseListFrm = class(TForm, IChildren)
    // @exclude
    lbData: TListBox;
    // @exclude
    ListBoxItem1: TListBoxItem;
    // @exclude
    SpeedButton1: TSpeedButton;
    // @exclude
    SpeedButton2: TSpeedButton;
    // @exclude
    SpeedButton3: TSpeedButton;
    // @exclude
    lbiSearch: TSearchBox;
    // @exclude
    bAdd: TSpeedButton;
  private
  protected
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.OnClickBEdit.txt)
    procedure OnClickBEdit(Sender: TObject); virtual; abstract;
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.OnClickBDel.txt)
    procedure OnClickBDel(Sender: TObject); virtual; abstract;

    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.CreateLBItem.txt)
    function CreateLBItem(Text, Detail, Id: string): TListBoxItem;
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.CreateButtonEdit.txt)
    procedure CreateButtonEdit(Id: string; lbItem: TListBoxItem);
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.CreateButtonDel.txt)
    procedure CreateButtonDel(Id: string; lbItem: TListBoxItem);
  public
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.SetCaption.txt)
    function SetCaption: string; virtual;
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.ShowOkButton.txt)
    function ShowOkButton: Boolean; virtual;
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.ShowBackButton.txt)
    function ShowBackButton: Boolean; virtual;
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.AcceptForm.txt)
    function AcceptForm: Boolean; virtual;
    // @include(..\..\docs\help\UBaseListFrm.TBaseListFrm.AfterShow.txt)
    procedure AfterShow; virtual;
  end;

implementation

{$R *.fmx}

{ TBaseListFrm }

function TBaseListFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TBaseListFrm.AfterShow;
begin
end;

procedure TBaseListFrm.CreateButtonDel(Id: string; lbItem: TListBoxItem);
begin
  with TSpeedButton.Create(lbItem) do
  begin
    Align := TAlignLayout.Right;
    Width := 40;
    StyleLookup := 'trashtoolbutton';
    Parent := lbItem;
    TagString := Id;
    OnClick := OnClickBDel;
  end;
end;

procedure TBaseListFrm.CreateButtonEdit(Id: string; lbItem: TListBoxItem);
begin
  with TSpeedButton.Create(lbItem) do
  begin
    Align := TAlignLayout.Right;
    Width := 40;
    StyleLookup := 'composetoolbutton';
    Parent := lbItem;
    TagString := Id;
    OnClick := OnClickBEdit;
  end;
end;

function TBaseListFrm.CreateLBItem(Text, Detail, Id: string): TListBoxItem;
begin
  Result := TListBoxItem.Create(lbData);
  Result.Text := Text;
  Result.TagString := Id;
  Result.ItemData.Detail := Detail;
  lbData.AddObject(Result);
end;

function TBaseListFrm.SetCaption: string;
begin
  Result := '';
end;

function TBaseListFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TBaseListFrm.ShowOkButton: Boolean;
begin
  Result := False;
end;

end.
