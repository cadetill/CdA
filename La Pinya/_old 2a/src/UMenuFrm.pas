unit UMenuFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.MultiView,
  FMX.StdCtrls, FMX.Controls.Presentation, System.Generics.Collections,
  System.Actions, FMX.ActnList, FMX.Objects,

  UHelpers, UInterfaces, UClasses, FMX.ScrollBox, FMX.Memo;

type
  TMenuFrm = class(TForm, IMainMenu)
    mvMenu: TMultiView;
    tbHeader: TToolBar;
    pContent: TPanel;
    bBack: TButton;
    bMenu: TButton;
    alActions: TActionList;
    aBack: TAction;
    imgMenu: TImage;
    imgBack: TImage;
    bAccept: TButton;
    imgAccept: TImage;
    lHeader: TLabel;
    aAccept: TAction;
    bHome: TButton;
    bConfig: TButton;
    bCalendar: TButton;
    bAdmin: TButton;
    Label1: TLabel;
    aHome: TAction;
    aConfig: TAction;
    aCalendar: TAction;
    aAdmin: TAction;
    procedure aBackExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aAcceptExecute(Sender: TObject);
    procedure aHomeExecute(Sender: TObject);
    procedure aConfigExecute(Sender: TObject);
    procedure aCalendarExecute(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FFrmList: TObjectList<TCustomForm>;
    FUser: TUser;

    procedure ShowAni(pAnimate: Boolean; pMessage: string = 'Carregant...');
    procedure ShowAcceptButton(State: Boolean);
    procedure MenuControl;
    procedure CreateForm(ClassForm: TFmxObjectClass; DataObject: TObject);
    function GetInfoUser: TUser;

    procedure PushForm(AForm: TCustomForm);
    procedure PopForm;

    function HaveUserDefined: Boolean;

    procedure OnTherminateGetRol(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MenuFrm: TMenuFrm;

implementation

uses
  UValidUserFrm, UHomeFrm, UConfigFrm, UCalendarFrm,
  UValidUserMdl, UStyleMdl, UIniFiles;

{$R *.fmx}

{ TMenuFrm }

procedure TMenuFrm.aAcceptExecute(Sender: TObject);
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

procedure TMenuFrm.aBackExecute(Sender: TObject);
begin
  if FFrmList.Count > 1 then
    PopForm
  else
    Close;
end;

procedure TMenuFrm.aCalendarExecute(Sender: TObject);
begin
  CreateForm(TCalendarFrm, nil);
end;

procedure TMenuFrm.aConfigExecute(Sender: TObject);
begin
  CreateForm(TConfigFrm, nil);
end;

procedure TMenuFrm.aHomeExecute(Sender: TObject);
begin
  CreateForm(THomeFrm, nil);
end;

constructor TMenuFrm.Create(AOwner: TComponent);
begin
  inherited;

  FFrmList := TObjectList<TCustomForm>.Create;
  bAccept.Visible := False;
  lHeader.Text := '';
end;

procedure TMenuFrm.CreateForm(ClassForm: TFmxObjectClass; DataObject: TObject);
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

destructor TMenuFrm.Destroy;
begin
  if Assigned(FFrmList) then
    FreeAndNil(FFrmList);

  inherited;
end;

procedure TMenuFrm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkHardwareBack then
    aBackExecute(nil);
end;

procedure TMenuFrm.FormShow(Sender: TObject);
begin
  inherited;

  if not FileExists(TGenFunc.GetIniName) or not HaveUserDefined then
    CreateForm(TValidUserFrm, nil)
  else
    MenuControl;
end;

function TMenuFrm.GetInfoUser: TUser;
begin
  Result := FUser;
end;

function TMenuFrm.HaveUserDefined: Boolean;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    TFileIni.SetFileIni(TGenFunc.GetIniName);
    TFileIni.GetSection('USERS', L);
    Result := L.Count > 0;
  finally
    FreeAndNil(L)
  end;
end;

procedure TMenuFrm.MenuControl;
var
  L: TStringList;
  ValCode: string;
  myThread: TThread;
begin
  ValCode := '';
  L := TStringList.Create;
  try
    TFileIni.SetFileIni(TGenFunc.GetIniName);
    TFileIni.GetSection('USERS', L);
    if L.Count > 0 then
      ValCode := L[0];
  finally
    FreeAndNil(L)
  end;

  if ValCode = '' then
  begin
    bHome.Visible := True;
    bConfig.Visible := False;
    bCalendar.Visible := False;
    bAdmin.Visible := False;
  end
  else
  begin
    myThread := TThread.CreateAnonymousThread(procedure
    begin
      TValidUserMdl.GetUserRol(ValCode, FUser);
    end
    );
    myThread.OnTerminate := OnTherminateGetRol;
    myThread.Start;
  end;
end;

procedure TMenuFrm.OnTherminateGetRol(Sender: TObject);
begin
  bHome.Visible := FUser.IsConvidat or FUser.IsCasteller;
  bConfig.Visible := FUser.IsCasteller;
  bCalendar.Visible := FUser.IsCasteller;
  bAdmin.Visible := FUser.IsJunta or FUser.IsTecnica;
  if FFrmList.Count = 0 then
    CreateForm(THomeFrm, nil);
end;

procedure TMenuFrm.PopForm;
var
  Intf: IChildren;
  AForm: TCustomForm;
begin
  if FFrmList.Count = 0 then
  begin
    CreateForm(THomeFrm, nil);
    Exit;
  end;

  while pContent.ChildrenCount > 0 do
    pContent.Children[0].Parent := FFrmList.Items[FFrmList.Count - 1];

  FFrmList.Delete(FFrmList.Count - 1);

  mvMenu.MasterButton := bMenu;
  mvMenu.Visible := True;
  bMenu.Visible := True;
  bBack.Visible := True;
  bAccept.Visible := False;
  lHeader.Text := '';
  if FFrmList.Count > 0 then
  begin
    AForm := FFrmList.Items[FFrmList.Count - 1];
    lHeader.Text := '';

    while AForm.ChildrenCount > 0 do
      AForm.Children[0].Parent := pContent;

    if Supports(aForm, IChildren, Intf) then
    begin
      if Intf.DeactivateAll  then
      begin
        mvMenu.MasterButton := nil;
        mvMenu.Visible := False;
        bMenu.Visible := False;
        bBack.Visible := False;
        bAccept.Visible := True;
      end;
      lHeader.Text := Intf.SetCaption;
    end;
  end
  else
    CreateForm(THomeFrm, nil);
end;

procedure TMenuFrm.PushForm(AForm: TCustomForm);
var
  Intf: IChildren;
begin
  if FFrmList.Count > 0 then
  begin
    while pContent.ChildrenCount > 0 do
      pContent.Children[0].Parent := FFrmList.Items[FFrmList.Count - 1];
  end;

  FFrmList.Add(AForm);
  lHeader.Text := '';

  while AForm.ChildrenCount > 0 do
    AForm.Children[0].Parent := pContent;

  if Supports(AForm, IChildren, Intf) then
  begin
    if Intf.DeactivateAll then
    begin
      if Intf.EnabledBackButton then
      begin
        mvMenu.MasterButton := bMenu;
        mvMenu.Visible := True;
        bMenu.Visible := True;
        bBack.Visible := True;
      end
      else
      begin
        mvMenu.MasterButton := nil;
        mvMenu.Visible := False;
        bMenu.Visible := False;
        bBack.Visible := False;
      end;
      bAccept.Visible := Intf.DefaultStateAcceptButton;
    end;

    lHeader.Text := Intf.SetCaption;

    Intf.AfterShow;
  end;
end;

procedure TMenuFrm.ShowAcceptButton(State: Boolean);
begin
  bAccept.Visible := State;
end;

procedure TMenuFrm.ShowAni(pAnimate: Boolean; pMessage: string);
begin
  pContent.Ani(pAnimate, pMessage);
end;

end.
