unit UHomeFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseModalFrm, FMX.Layouts, FMX.ScrollBox, FMX.Memo, FMX.Objects,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Effects, System.Actions,
  FMX.ActnList, FMX.MultiResBitmap,
  UInterfaces, UHelpers, UClasses;

const
  cCaption = 'Notícies';
  cDelNews = 'Vol esborrar la noticia "%s"?';

type
  THomeFrm = class(TBaseModalFrm, IChildren)
    lbNews: TListBox;
    lbItem1: TListBoxItem;
    recContent: TRectangle;
    lFooter: TLabel;
    lnFooter: TLine;
    lHeader: TLabel;
    lnHeader: TLine;
    lbItem1m: TText;
    Text1: TText;
    recMenu: TRectangle;
    imgAdd: TImage;
    imgMore: TImage;
    imgEdit: TImage;
    imgDel: TImage;
    bAdd: TButton;
    seAdd: TShadowEffect;
    bMore: TButton;
    seMore: TShadowEffect;
    ActionList1: TActionList;
    actAddNews: TAction;
    actMoreNews: TAction;
    procedure FormShow(Sender: TObject);
    procedure actAddNewsExecute(Sender: TObject);
    procedure actMoreNewsExecute(Sender: TObject);
  private
    FStart: Integer;
    FCount: Integer;
    FOffset: Single;

    procedure OnChangeItemNews(Sender: TItemNews);
    procedure CreateNewListBoxItem(Id, Titol, Contingut, Publicar, Hora: string; bEdit, bDel: TCustomBitmapItem);
    procedure GetMoreNews;

    procedure OnEditClick(Sender: TObject);
    procedure OnDelClick(Sender: TObject);

    procedure DoPermisions;
  public
    constructor Create(AOwner: TComponent); override;

    function SetCaption: string;
    function DeactivateAll: Boolean;
    function AcceptForm: Boolean;
    function EnabledBackButton: Boolean;
    function DefaultStateAcceptButton: Boolean;
    procedure AfterShow;
  end;

var
  HomeFrm: THomeFrm;

implementation

uses
  UStyleMdl, UHomeMdl, UImagesMdl, UMessage, UNewsFrm;

{$R *.fmx}

function THomeFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure THomeFrm.actAddNewsExecute(Sender: TObject);
var
  Intf: IMainMenu;
begin
  inherited;

  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TNewsFrm, TItemNews.Create(OnChangeItemNews));
end;

procedure THomeFrm.actMoreNewsExecute(Sender: TObject);
begin
  inherited;

  GetMoreNews
end;

procedure THomeFrm.AfterShow;
begin
  DoPermisions;
  GetMoreNews;
end;

constructor THomeFrm.Create(AOwner: TComponent);
begin
  inherited;

  FStart := 0;
  FCount := 10;
  FOffset := 0;
  lbNews.Clear;
end;

procedure THomeFrm.CreateNewListBoxItem(Id, Titol, Contingut, Publicar,
  Hora: string; bEdit, bDel: TCustomBitmapItem);
var
  lbItem: TListBoxItem;
  rRec: TRectangle;
  lFooter: TLabel;
  lHeader: TLabel;
  lnFooter: TLine;
  lnHeader: TLine;
  imgEdit: TImage;
  imgDel: TImage;
  mContent: TText;
  Intf: IMainMenu;
  User: TUser;
begin
  if Supports(Owner, IMainMenu, Intf) then
    User := Intf.GetInfoUser;

  lbItem := TListBoxItem.Create(lbNews);
  lbItem.Size.PlatformDefault := False;
  lbItem.Size.Height := 337;
  lbItem.Name := 'News' + Id;
  lbItem.Text := '';
  lbItem.Width := lbNews.Width;

  // creem objecte per posar la info del registre
  lbItem.ItemNews := TItemNews.Create(OnChangeItemNews);
  lbItem.ItemNews.Id := Id;
  lbItem.ItemNews.Titol := Titol;
  lbItem.ItemNews.Contingut := Contingut;
  lbItem.ItemNews.Publicar := Publicar;
  lbItem.ItemNews.Hora := Hora;

  // Creem Rectangle contenidor
  rRec := TRectangle.Create(Self);
  rRec.Parent := lbItem;
  rRec.Name := lbItem.Name + 'Rec';
  rRec.Align := TAlignLayout.Client;
  rRec.Margins.Left := 20;
  rRec.Margins.Top := 20;
  rRec.Margins.Right := 20;
  rRec.Margins.Bottom := 20;
  rRec.Stroke.Color := TAlphaColorRec.Firebrick;
  rRec.XRadius := 20;
  rRec.YRadius := 20;

  // creem etiqueta Footer
  lFooter := TLabel.Create(Self);
  lFooter.Parent := rRec;
  lFooter.Name := lbItem.Name + 'lf';
  lFooter.Align := TAlignLayout.Bottom;
  lFooter.Height := 17;
  lFooter.Margins.Right := 20;
  lFooter.Margins.Bottom := 5;
  lFooter.TextSettings.HorzAlign := TTextAlign.Trailing;
  lFooter.Text := lbItem.ItemNews.PubDate + ' ' + lbItem.ItemNews.Hora;
  lFooter.StyleLookup := 'sLabelContent';

  // creem línia Footer
  lnFooter := TLine.Create(Self);
  lnFooter.Parent := rRec;
  lnFooter.Name := lbItem.Name + 'lnf';
  lnFooter.Align := TAlignLayout.Bottom;
  lnFooter.LineType := TLineType.Top;
  lnFooter.Margins.Left := 20;
  lnFooter.Margins.Right := 20;
  lnFooter.Size.Height := 6;
  lnFooter.Size.PlatformDefault := False;
  lnFooter.Stroke.Color := TAlphaColorRec.Firebrick;

  // creem etiqueta Header
  lHeader := TLabel.Create(Self);
  lHeader.Parent := rRec;
  lHeader.Name := lbItem.Name + 'lh';
  lHeader.Align := TAlignLayout.Top;
  lHeader.Height := 29;
  lHeader.Margins.Right := 20;
  lHeader.Margins.Top := 5;
  lHeader.TextSettings.HorzAlign := TTextAlign.Center;
  lHeader.Text := lbItem.ItemNews.Titol;
  lHeader.StyleLookup := 'sLabelTitol';

  // creem línia Header
  lnHeader := TLine.Create(Self);
  lnHeader.Parent := rRec;
  lnHeader.Name := lbItem.Name + 'lnh';
  lnHeader.Align := TAlignLayout.Top;
  lnHeader.LineType := TLineType.Bottom;
  lnHeader.Margins.Left := 20;
  lnHeader.Margins.Right := 20;
  lnHeader.Size.Height := 6;
  lnHeader.Size.PlatformDefault := False;
  lnHeader.Stroke.Color := TAlphaColorRec.Firebrick;
  lnHeader.Position.Y := lHeader.Height;

  // creem imatge de edit
  imgEdit := TImage.Create(Self);
  imgEdit.Parent := lHeader;
  imgEdit.Align := TAlignLayout.MostRight;
  imgEdit.Bitmap := bEdit.MultiResBitmap.Bitmaps[1.0];
  imgEdit.Name := lbItem.Name + 'imge';
  imgEdit.MultiResBitmap.Height := 48;
  imgEdit.MultiResBitmap.Width := 48;
  imgEdit.Width := lHeader.Height;
  imgEdit.LBItem := lbItem;
  imgEdit.OnClick := OnEditClick;
  imgEdit.Visible := User.IsAdmin;

  // creem imatge de delete
  imgDel := TImage.Create(Self);
  imgDel.Parent := lHeader;
  imgDel.Align := TAlignLayout.MostRight;
  imgDel.Position.X := imgEdit.Width;
  imgDel.Bitmap := bDel.MultiResBitmap.Bitmaps[1.0];
  imgDel.Name := lbItem.Name + 'imgd';
  imgDel.MultiResBitmap.Height := 48;
  imgDel.MultiResBitmap.Width := 48;
  imgDel.Width := lHeader.Height;
  imgDel.LBItem := lbItem;
  imgDel.OnClick := OnDelClick;
  imgDel.Visible := User.IsAdmin;

  // creem text
  mContent := TText.Create(Self);
  mContent.Parent := rRec;
  mContent.Name := lbItem.Name + 'm';
  mContent.Text := lbItem.ItemNews.Contingut;
  mContent.TextSettings.Font.Size := 16;
  mContent.TextSettings.HorzAlign := TTextAlign.Leading;
  mContent.TextSettings.VertAlign := TTextAlign.Leading;
  mContent.Margins.Left := 20;
  mContent.Margins.Right := 20;
  mContent.Margins.Top := 10;
  mContent.Margins.Bottom := 10;
  mContent.Align := TAlignLayout.Top;
  mContent.Position.Y := lHeader.Height + lnHeader.Height;
  mContent.AutoSize := True;

  FOffset := lHeader.Height + lHeader.Margins.Top + lHeader.Margins.Bottom +
             lFooter.Height + lFooter.Margins.Top + lFooter.Margins.Bottom +
             lnHeader.Height + lnHeader.Margins.Top + lnHeader.Margins.Bottom +
             lnFooter.Height + lnFooter.Margins.Top + lnFooter.Margins.Bottom +
             rRec.Margins.Top + rRec.Margins.Bottom +
             mContent.Height + mContent.Margins.Top + mContent.Margins.Bottom;

  lbItem.Height := FOffset;
  mContent.Align := TAlignLayout.Client;

  // assignem Item a la llista
  lbNews.AddObject(lbItem);
  lbItem.ItemNews.IndexLB := lbNews.Count;
end;

function THomeFrm.DeactivateAll: Boolean;
begin
  Result := False;
end;

function THomeFrm.DefaultStateAcceptButton: Boolean;
begin
  Result := False;
end;

procedure THomeFrm.DoPermisions;
var
  Intf: IMainMenu;
begin
  bAdd.Visible := False;
  if Supports(Owner, IMainMenu, Intf) then
    bAdd.Visible := Intf.GetInfoUser.IsCommunication;
end;

function THomeFrm.EnabledBackButton: Boolean;
begin
  Result := True;
end;

procedure THomeFrm.FormShow(Sender: TObject);
begin
  inherited;

  GetMoreNews;
end;

procedure THomeFrm.GetMoreNews;
var
  Mdl: THomeMdl;
  MdlImg: TImagesMdl;
  bmEdit: TCustomBitmapItem;
  bmDel: TCustomBitmapItem;
  Size: TSize;
  Intf: IMainMenu;
  User: TUser;
begin
  if Supports(Owner, IMainMenu, Intf) then
    User := Intf.GetInfoUser;

  Ani(True);
  TThread.CreateAnonymousThread(procedure
  begin
    Mdl := nil;
    MdlImg := nil;
    try
      Mdl := THomeMdl.Create(Self);
      MdlImg := TImagesMdl.Create(Self);

      MdlImg.ilImages.BitmapItemByName('pencil', bmEdit, Size);
      MdlImg.ilImages.BitmapItemByName('trash', bmDel, Size);

      Mdl.GetNews(FStart, FCount, User.IsCommunication);
      FStart := FStart + FCount;

      FOffset := 0;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          while not Mdl.cdsNews.Eof do
          begin
            // creem Item
            CreateNewListBoxItem(
                                 Mdl.cdsNews.FieldByName('id').AsString,
                                 Mdl.cdsNews.FieldByName('Titol').AsString,
                                 Mdl.cdsNews.FieldByName('Contingut').AsString,
                                 Mdl.cdsNews.FieldByName('Publicar').AsString,
                                 Mdl.cdsNews.FieldByName('Hora').AsString,
                                 bmEdit,
                                 bmDel
                                );

            Mdl.cdsNews.Next;
          end;
        end);
    finally
      FreeAndNil(Mdl);
      FreeAndNil(MdlImg);
    end;

    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        //ResizeItems;
      end);

    // ocultem animació
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        Ani(False);
      end);
  end
  ).Start;
end;

procedure THomeFrm.OnChangeItemNews(Sender: TItemNews);
var
  bmEdit: TCustomBitmapItem;
  bmDel: TCustomBitmapItem;
  MdlImg: TImagesMdl;
  Size: TSize;
  cComp: TComponent;
  Tmp: string;
  lbItem: TListBoxItem;
begin
  // si és una nova notícia
  if Sender.IndexLB = -1 then
  begin
    MdlImg := TImagesMdl.Create(Self);
    try
      MdlImg.ilImages.BitmapItemByName('pencil', bmEdit, Size);
      MdlImg.ilImages.BitmapItemByName('trash', bmDel, Size);
      CreateNewListBoxItem(Sender.Id, Sender.Titol, Sender.Contingut, Sender.Publicar, Sender.Hora, bmEdit, bmDel);
    finally
      FreeAndNil(MdlImg);
    end;
  end
  else // si és una modificación
  begin
    lbItem := TListBoxItem(lbNews.ItemByIndex(Sender.IndexLB-1));
    if not Assigned(lbItem) then Exit;

    // modifiquem títol
    Tmp := lbItem.Name + 'lh';
    cComp := FindComponent(Tmp);
    if Assigned(cComp) and (cComp is TLabel) then
      TLabel(cComp).Text := Sender.Titol;

    // modifiquem contingut
    Tmp := lbItem.Name + 'm';
    cComp := FindComponent(Tmp);
    if Assigned(cComp) and (cComp is TText) then
      TText(cComp).Text := Sender.Contingut;
  end;
end;

procedure THomeFrm.OnDelClick(Sender: TObject);
var
  TmpB: Boolean;
begin
  if not (Sender is TImage) then Exit;

  TMessage.MsjSiNo(cDelNews, [TImage(Sender).LBItem.ItemNews.Titol],
    procedure
    begin
      TThread.CreateAnonymousThread(procedure
      begin
        TmpB := THomeMdl.DelNews(TImage(Sender).LBItem.ItemNews.Id);
        if TmpB then
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              lbNews.RemoveObject(lbNews.ListItems[TImage(Sender).LBItem.Index]);
            end);
      end
      ).Start;
    end);
end;

procedure THomeFrm.OnEditClick(Sender: TObject);
var
  Intf: IMainMenu;
begin
  if not (Sender is TImage) then Exit;

  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TNewsFrm, TImage(Sender).LBItem.ItemNews);
end;

function THomeFrm.SetCaption: string;
begin
  Result := cCaption;
end;

end.
