{
  @abstract(unit with main form)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 10, 2020)
  @lastmod(October 10, 2020)

  The UMainFrm unit contains the Main form of the application. This form will manage all child forms.

  Change List @br
  @unorderedList(
    @item(10/10/2020 : first version)
  )
}
unit UMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Generics.Collections, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.MultiView, FMX.Objects, FMX.TabControl,
  UInterfaces;

type
  { -------------------------------------------------------------------------- }
  // @include(..\docs\help\UMainFrm.TMainFrm.txt)
  TMainFrm = class(TForm, IMainMenu)
    // @exclude
    mvMenu: TMultiView;
    // @exclude
    pContent: TPanel;
    // @exclude
    tlbHeader: TToolBar;
    // @exclude
    bMenu: TSpeedButton;
    // @exclude
    rMenuTop: TRectangle;
    // @exclude
    vsMenu: TVertScrollBox;
    // @exclude
    rCalendars: TRectangle;
    // @exclude
    imgCalendars: TImage;
    // @exclude
    rMenu1: TRectangle;
    // @exclude
    tcMenu: TTabControl;
    // @exclude
    tiAdmin: TTabItem;
    // @exclude
    cMenuTop: TCircle;
    // @exclude
    rHeader: TRectangle;
    // @exclude
    bBack: TSpeedButton;
    // @exclude
    lHeader: TLabel;
    // @exclude
    bOk: TSpeedButton;
    // @exclude
    aiIndicator: TAniIndicator;
    // @exclude
    lCalendars: TLabel;
    // @exclude
    rSeasons: TRectangle;
    // @exclude
    lSeasons: TLabel;
    // @exclude
    imgSeasons: TImage;
    // @exclude
    imgAdmin: TImage;
    // @exclude
    imgGeneral: TImage;
    // @exclude
    tiGeneral: TTabItem;
    // @exclude
    rEvents: TRectangle;
    // @exclude
    lEvents: TLabel;
    // @exclude
    imgEvents: TImage;
    // @exclude
    rGeneral: TRectangle;
    // @exclude
    rAdmin: TRectangle;
    // @exclude
    rRols: TRectangle;
    // @exclude
    lRols: TLabel;
    // @exclude
    imgRols: TImage;
    // @exclude
    rSocis: TRectangle;
    // @exclude
    lSocis: TLabel;
    // @exclude
    imgSocis: TImage;
    // @include(..\docs\help\UMainFrm.TMainFrm.bOkClick.txt)
    procedure bOkClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.bBackClick.txt)
    procedure bBackClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.rCalendarsClick.txt)
    procedure rCalendarsClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.rSeasonsClick.txt)
    procedure rSeasonsClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.rGeneralClick.txt)
    procedure rGeneralClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.rAdminClick.txt)
    procedure rAdminClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.rEventsClick.txt)
    procedure rEventsClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.FormKeyUp.txt)
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    // @include(..\docs\help\UMainFrm.TMainFrm.rSocisClick.txt)
    procedure rSocisClick(Sender: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.rRolsClick.txt)
    procedure rRolsClick(Sender: TObject);
  protected
    // @include(..\docs\help\UMainFrm.TMainFrm.FFrmList.txt)
    FFrmList: TObjectList<TCustomForm>;

    // @include(..\docs\help\UMainFrm.TMainFrm.PushForm.txt)
    procedure PushForm(AForm: TCustomForm);
    // @include(..\docs\help\UMainFrm.TMainFrm.PopForm.txt)
    procedure PopForm;

    // @include(..\docs\help\UMainFrm.TMainFrm.ShowAcceptButton.txt)
    procedure CreateForm(ClassForm: TFmxObjectClass; DataObject: TObject);
    // @include(..\docs\help\UMainFrm.TMainFrm.ShowAcceptButton.txt)
    procedure ShowAcceptButton(State: Boolean);
    // @include(..\docs\help\UMainFrm.TMainFrm.ShowAni.txt)
    function ShowAni(Show: Boolean): Boolean;
  public
    // @include(..\docs\help\UMainFrm.TMainFrm.Create.txt)
    constructor Create(AOwner: TComponent); override;
    // @include(..\docs\help\UMainFrm.TMainFrm.Destroy.txt)
    destructor Destroy; override;
  end;

var
  MainFrm: TMainFrm;

implementation

uses
  FMX.Platform, FMX.VirtualKeyboard,
  UCalendarsFrm;//, USeasonsFrm, UEventsFrm, USocisFrm, URolsFrm;

{$R *.fmx}

{ TMainFrm }

procedure TMainFrm.bBackClick(Sender: TObject);
begin
  if FFrmList.Count = 0 then
    Close
  else
    PopForm;
//  if FFrmList.Count > 1 then
//    PopForm
//  else
//    Close;
end;

procedure TMainFrm.bOkClick(Sender: TObject);
var
  Intf: IChildren;
  AForm: TCustomForm;
begin
  if FFrmList.Count = 0 then
    Exit;

  AForm := FFrmList[FFrmList.Count - 1];

  if Supports(AForm, IChildren, Intf) and Intf.AcceptForm then
    PopForm;
end;

constructor TMainFrm.Create(AOwner: TComponent);
begin
  inherited;

  FFrmList := TObjectList<TCustomForm>.Create;
  ShowAni(False);
  lHeader.Text := '';
  mvMenu.HideMaster;
  tcMenu.ActiveTab := tiGeneral;

//  CreateForm(THomeFrm, nil);
end;

procedure TMainFrm.CreateForm(ClassForm: TFmxObjectClass; DataObject: TObject);
var
  aForm: TCustomForm;
begin
  inherited;

  mvMenu.HideMaster;
  aForm := ClassForm.Create(Self) as TCustomForm;
  aForm.TagObject := DataObject;
  aForm.Name := aForm.Name + FormatDateTime('hhnnssmm', Now);
  PushForm(aForm);
end;

destructor TMainFrm.Destroy;
begin
  if Assigned(FFrmList) then
    FreeAndNil(FFrmList);

  inherited;
end;

procedure TMainFrm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  FService: IFMXVirtualKeyboardService;
begin
  if Key = vkHardwareBack then
  begin
    TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FService));
    if (FService <> nil) and (TVirtualKeyboardState.Visible in FService.VirtualKeyBoardState) then
    begin
      // Back button pressed, keyboard visible, so do nothing...
    end
    else
    begin
      // Back button pressed, keyboard not visible or not supported on this platform, lets exit the app...
      Key := 0;
      bBack.OnClick(nil);
    end;
  end;
end;

procedure TMainFrm.PopForm;
var
  AForm: TCustomForm;
  Intf: IChildren;
begin
  // if don't have stack forms, bye bye
  if FFrmList.Count = 0 then
    Exit;

  // we return parent references
  while pContent.ChildrenCount > 0 do
    pContent.Children[0].Parent := FFrmList.Items[FFrmList.Count - 1];

  // unstack last shown form
  FFrmList.Delete(FFrmList.Count - 1);

  lHeader.Text := '';
  bOk.Visible := False;

  // if any form is into the stack
  if FFrmList.Count > 0 then
  begin
    // get last form
    AForm := FFrmList.Items[FFrmList.Count - 1];

    // put new references to the principal container
    while AForm.ChildrenCount > 0 do
      AForm.Children[0].Parent := pContent;

    if Supports(AForm, IChildren, Intf) then
    begin
      lHeader.Text := Intf.SetCaption;
      bBack.Visible := Intf.ShowBackButton;
      bOk.Visible := Intf.ShowOkButton;

      Intf.AfterShow;
    end;
  end;
end;

procedure TMainFrm.PushForm(AForm: TCustomForm);
var
  Intf: IChildren;
begin
  if FFrmList.Count > 0 then
  begin
    while pContent.ChildrenCount > 0 do
      pContent.Children[0].Parent := FFrmList.Items[FFrmList.Count - 1];
  end;

  FFrmList.Add(AForm);
  bOk.Visible := False;

  while AForm.ChildrenCount > 0 do
    AForm.Children[0].Parent := pContent;

  if Supports(AForm, IChildren, Intf) then
  begin
    lHeader.Text := Intf.SetCaption;
    bBack.Visible := Intf.ShowBackButton;
    bOk.Visible := Intf.ShowOkButton;

    Intf.AfterShow;
  end;
end;

procedure TMainFrm.rCalendarsClick(Sender: TObject);
begin
  CreateForm(TCalendarsFrm, nil);
end;

procedure TMainFrm.rEventsClick(Sender: TObject);
begin
//  CreateForm(TEventsFrm, nil);
end;

procedure TMainFrm.rAdminClick(Sender: TObject);
begin
  tcMenu.ActiveTab := tiAdmin;
  rGeneral.Stroke.Color := rMenu1.Fill.Color;
  rAdmin.Stroke.Color := talphacolors.Crimson;
end;

procedure TMainFrm.rGeneralClick(Sender: TObject);
begin
  tcMenu.ActiveTab := tiGeneral;
  rGeneral.Stroke.Color := talphacolors.Crimson;
  rAdmin.Stroke.Color := rMenu1.Fill.Color;
end;

procedure TMainFrm.rRolsClick(Sender: TObject);
begin
//  CreateForm(TRolsFrm, nil);
end;

procedure TMainFrm.rSeasonsClick(Sender: TObject);
begin
//  CreateForm(TSeasonsFrm, nil);
end;

procedure TMainFrm.rSocisClick(Sender: TObject);
begin
//  CreateForm(TSocisFrm, nil);
end;

procedure TMainFrm.ShowAcceptButton(State: Boolean);
begin

end;

function TMainFrm.ShowAni(Show: Boolean): Boolean;
begin
  Result := aiIndicator.Enabled;
  aiIndicator.Enabled := Show;
  aiIndicator.Visible := Show;
end;

end.
