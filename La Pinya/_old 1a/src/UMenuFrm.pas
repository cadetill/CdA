// @include(..\docs\UMenuFrm.txt)
unit UMenuFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseFrm, FMX.Controls.Presentation, FMX.MultiView, FMX.Edit,
  System.ImageList, FMX.ImgList, FMX.Layouts, FMX.ListBox, FMX.TabControl,
  FMX.Objects,
  UFormList;

type
  // @include(..\docs\UMenuFrm.TMenuFrm.txt)
  TMenuFrm = class(TBaseFrm)
    {@exclude} mvMenu: TMultiView;
    {@exclude} bMultiview: TSpeedButton;
    {@exclude} pContent: TPanel;
    {@exclude} ToolBar1: TToolBar;
    {@exclude} imgAdmin: TImage;
    {@exclude} tcMenu: TTabControl;
    {@exclude} tiAdmin: TTabItem;
    {@exclude} lbAdmin: TListBox;
    {@exclude} lbiUsers: TListBoxItem;
    {@exclude} lbiAdmin: TListBoxHeader;
    {@exclude} lAdmin: TLabel;
    {@exclude} tiEnd: TTabItem;
    {@exclude} imgConfig: TImage;
    {@exclude} tiConfig: TTabItem;
    {@exclude} lbConfig: TListBox;
    {@exclude} lbiConfig: TListBoxItem;
    {@exclude} lbiConfiguration: TListBoxHeader;
    {@exclude} lConfig: TLabel;
    {@exclude} procedure imgAdminClick(Sender: TObject);
    {@exclude} procedure mvMenuStartShowing(Sender: TObject);
    {@exclude} procedure bBackClick(Sender: TObject);
    {@exclude} procedure lbiUsersClick(Sender: TObject);
    {@exclude} procedure lbiConfigClick(Sender: TObject);
  private
    FFormList: TFormList;
  protected
    // @include(..\docs\UMenuFrm.TMenuFrm.CreateForm.txt)
    procedure CreateForm(ClassForm: TFmxObjectClass);
  public
    // @include(..\docs\UMenuFrm.TMenuFrm.Create.txt)
    constructor Create(AOwner: TComponent); override;
    // @include(..\docs\UMenuFrm.TMenuFrm.Destroy.txt)
    destructor Destroy; override;

    // @include(..\docs\UMenuFrm.TMenuFrm.ShowForm.txt)
    class procedure ShowForm;
  end;

var
  MenuFrm: TMenuFrm;

implementation

uses
  UUsersFrm, UConfigFrm, UInterfaces;

{$R *.fmx}

{ TMenuFrm }

procedure TMenuFrm.bBackClick(Sender: TObject);
begin
  if FFormList.GetFormCount > 0 then
    FFormList.PopForm
  else
    inherited;
end;

constructor TMenuFrm.Create(AOwner: TComponent);
begin
  inherited;

  mvMenu.Visible := False;
  tcMenu.TabPosition := TTabPosition.None;
  tcMenu.ActiveTab := tiEnd;
  FFormList := TFormList.Create(pContent);
end;

procedure TMenuFrm.CreateForm(ClassForm: TFmxObjectClass);
var
  aForm: TCustomForm;
  Intf: IFormList;
begin
  inherited;

  aForm := ClassForm.Create(Self) as TCustomForm;
  if Supports(aForm, IFormList, Intf) then
    Intf.SetFormList(FFormList);

  aForm.Name := aForm.Name + FormatDateTime('hhnnssmm', Now);
  FFormList.PushForm(aForm);
  mvMenu.HideMaster;
end;

destructor TMenuFrm.Destroy;
begin
  if Assigned(FFormList) then
    FreeAndNil(FFormList);

  inherited;
end;

procedure TMenuFrm.imgAdminClick(Sender: TObject);
begin
  inherited;

  if not (Sender is TImage) then Exit;

  tcMenu.TabIndex := TImage(Sender).Tag;
end;

procedure TMenuFrm.lbiConfigClick(Sender: TObject);
begin
  inherited;

  CreateForm(TConfigFrm);
end;

procedure TMenuFrm.lbiUsersClick(Sender: TObject);
begin
  inherited;

  CreateForm(TUsersFrm);
end;

procedure TMenuFrm.mvMenuStartShowing(Sender: TObject);
begin
  inherited;

  tcMenu.ActiveTab := tiEnd;
end;

class procedure TMenuFrm.ShowForm;
var
  F: TMenuFrm;
begin
  F := TMenuFrm.Create(nil);
  F.ShowModal(
    procedure(ModalR: TModalResult)
    begin
      if IsPositiveResult(ModalR) then
      begin
      end;
    end
  );
end;

end.
