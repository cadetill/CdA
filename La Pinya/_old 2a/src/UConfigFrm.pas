unit UConfigFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseModalFrm, FMX.Layouts, FMX.DateTimeCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Edit, FMX.Controls.Presentation, FMX.MultiResBitmap, FMX.ListBox, FMX.Objects,
  System.Actions, FMX.ActnList,
  UInterfaces, UClasses;

const
  cCaption = 'Configuració';
  cDelSoci = 'Vol desvincular el soci "%s"?';

type
  TConfigFrm = class(TBaseModalFrm, IChildren)
    sbContent: TScrollBox;
    lTitre: TLabel;
    lbCastAct: TListBox;
    lContent: TLabel;
    lVersion: TLabel;
    lInfo: TLabel;
    bNewUser: TButton;
    lyDiades: TLayout;
    swDiades: TSwitch;
    lDiades: TLabel;
    lyAssajos: TLayout;
    swAssajos: TSwitch;
    lAssajos: TLabel;
    lyEvents: TLayout;
    swEvents: TSwitch;
    lEvents: TLabel;
    lyNews: TLayout;
    swNews: TSwitch;
    lNews: TLabel;
    lErrors: TLabel;
    pCastellers: TPanel;
    pNotify: TPanel;
    pInfo: TPanel;
    ListBoxItem1: TListBoxItem;
    imgDel: TImage;
    ActionList1: TActionList;
    aNewUser: TAction;
    procedure aNewUserExecute(Sender: TObject);
  private
    procedure CreateNewListBoxItem(ActCode, Nom: string; IsFirst: Boolean; bDel: TCustomBitmapItem);
    procedure OnChangeItemConfig(Sender: TItemConfig);

    procedure AddUsers(ActCodes: string; ClearBefore: Boolean);

    procedure OnDelClick(Sender: TObject);
  public
    function SetCaption: string;
    function DeactivateAll: Boolean;
    function AcceptForm: Boolean;
    function EnabledBackButton: Boolean;
    function DefaultStateAcceptButton: Boolean;
    procedure AfterShow;
  end;

var
  ConfigFrm: TConfigFrm;

implementation

uses
  UIniFiles, USocisMdl, UImagesMdl, UHelpers, UMessage, UValidUserFrm;

{$R *.fmx}

{ TConfigFrm }

function TConfigFrm.AcceptForm: Boolean;
var
  i: Integer;
  L: TStringList;
begin
  TFileIni.SetFileIni(TGenFunc.GetIniName);

  // grabem usuaris
  L := TStringList.Create;
  try
    for i := 0 to lbCastAct.Count - 1 do
      L.Add(lbCastAct.ItemByIndex(i).TagString);
    TFileIni.SetSection('USERS', L);
  finally
    FreeAndNil(L);
  end;

  // grabem opcions de missatgeria
  TFileIni.SetBolValue('NOTIFICACIONS', 'DIADES', swDiades.IsChecked);
  TFileIni.SetBolValue('NOTIFICACIONS', 'ASSAJOS', swAssajos.IsChecked);
  TFileIni.SetBolValue('NOTIFICACIONS', 'EVENTS', swEvents.IsChecked);
  TFileIni.SetBolValue('NOTIFICACIONS', 'NOTICIES', swNews.IsChecked);

  Result := True;
end;

procedure TConfigFrm.AddUsers(ActCodes: string; ClearBefore: Boolean);
var
  L: TStringList;
  Mdl: TSocisMdl;
  MdlImg: TImagesMdl;
  bmDel: TCustomBitmapItem;
  Size: TSize;
begin
  if ClearBefore then
    lbCastAct.Clear;

  Ani(True);
  TThread.CreateAnonymousThread(procedure
  begin
    L := nil;
    Mdl := nil;
    MdlImg := nil;
    try
      L := TStringList.Create;
      Mdl := TSocisMdl.Create(Self);
      MdlImg := TImagesMdl.Create(Self);

      MdlImg.ilImages.BitmapItemByName('trash', bmDel, Size);

      L.CommaText := ActCodes;
      Mdl.GetSocisName(L.CommaText);
      if Mdl.cdsResult.Active then
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          var
            i: Integer;
          begin
            for i := 0 to L.Count - 1 do
            begin
              if Mdl.cdsResult.Locate('actcode', L[i], []) then
              begin
                CreateNewListBoxItem(Mdl.cdsResult.FieldByName('actcode').AsString,
                                     Mdl.cdsResult.FieldByName('nom').AsString,
                                     lbCastAct.Count = 0,
                                     bmDel);
              end;
            end;
          end);
    finally
      FreeAndNil(MdlImg);
      FreeAndNil(Mdl);
      FreeAndNil(L);
    end;

    // ocultem animació
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        Ani(False);
      end);
  end
  ).Start;
end;

procedure TConfigFrm.AfterShow;
var
  L: TStringList;
begin
  TFileIni.SetFileIni(TGenFunc.GetIniName);

  L := nil;
  try
    L := TStringList.Create;
    TFileIni.GetSection('USERS', L);
    AddUsers(L.CommaText, True);
  finally
    FreeAndNil(L);
  end;

  swDiades.IsChecked := TFileIni.GetBolValue('NOTIFICACIONS', 'DIADES');
  swAssajos.IsChecked := TFileIni.GetBolValue('NOTIFICACIONS', 'ASSAJOS');
  swEvents.IsChecked := TFileIni.GetBolValue('NOTIFICACIONS', 'EVENTS');
  swNews.IsChecked := TFileIni.GetBolValue('NOTIFICACIONS', 'NOTICIES');
end;

procedure TConfigFrm.aNewUserExecute(Sender: TObject);
var
  Intf: IMainMenu;
begin
  inherited;

  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TValidUserFrm, TItemConfig.Create(OnChangeItemConfig));
end;

procedure TConfigFrm.CreateNewListBoxItem(ActCode, Nom: string;
  IsFirst: Boolean; bDel: TCustomBitmapItem);
var
  lbItem: TListBoxItem;
  imgDel: TImage;
begin
  lbItem := TListBoxItem.Create(lbCastAct);
  lbItem.Size.PlatformDefault := False;
  lbItem.Name := 'CastAct' + ActCode;
  lbItem.Text := Nom;
  lbItem.TagString := ActCode;
  lbItem.StyleLookup := 'sListBoxItem';

  if not IsFirst then
  begin
    // creem imatge de delete
    imgDel := TImage.Create(Self);
    imgDel.Parent := lbItem;
    imgDel.Align := TAlignLayout.MostRight;
    imgDel.Bitmap := bDel.MultiResBitmap.Bitmaps[1.0];
    imgDel.Name := lbItem.Name + 'imgd';
    imgDel.MultiResBitmap.Height := 35;
    imgDel.MultiResBitmap.Width := 35;
    imgDel.Width := lbItem.Height;
    imgDel.LBItem := lbItem;
    imgDel.OnClick := OnDelClick;
  end;

  // assignem Item a la llista
  lbCastAct.AddObject(lbItem);
end;

function TConfigFrm.DeactivateAll: Boolean;
begin
  Result := True;
end;

function TConfigFrm.DefaultStateAcceptButton: Boolean;
begin
  Result := True;
end;

function TConfigFrm.EnabledBackButton: Boolean;
begin
  Result := False;
end;

procedure TConfigFrm.OnChangeItemConfig(Sender: TItemConfig);
begin
  ShowMessage('1');
  AddUsers(Sender.ActCode, False);
end;

procedure TConfigFrm.OnDelClick(Sender: TObject);
var
  TmpB: Boolean;
  TmpStr: string;
begin
  if not (Sender is TImage) then Exit;

  TFileIni.SetFileIni(TGenFunc.GetIniName);
  TmpStr := TFileIni.GetStrValue('GENERAL', 'ID');

  // confirmació d'esborrar
  TMessage.MsjSiNo(cDelSoci, [TImage(Sender).LBItem.Text],
    procedure
    begin
      Ani(True);
      TThread.CreateAnonymousThread(procedure
      begin
        try
          TmpB := TSocisMdl.DesvinculaSoci(TImage(Sender).LBItem.TagString, TmpStr);
          if TmpB then
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                lbCastAct.RemoveObject(lbCastAct.ListItems[TImage(Sender).LBItem.Index]);
              end);
          AcceptForm;
        finally
          // ocultem animació
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              Ani(False);
            end);
        end;
      end
      ).Start;
    end);
end;

function TConfigFrm.SetCaption: string;
begin
  Result := cCaption;
end;

end.
