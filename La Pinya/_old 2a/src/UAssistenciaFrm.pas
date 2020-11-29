unit UAssistenciaFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseFrm, FMX.Layouts, FMX.Controls.Presentation, FMX.Objects, FMX.ListBox,

  UCalFrm, UInterfaces, UClasses, System.ImageList, FMX.ImgList;

const
  cCaption = 'Gestió Assistència';

type
  TAssistenciaFrm = class(TBaseFrm, IChildren)
    pIcons: TPanel;
    imgBus: TImage;
    imgOk: TImage;
    imgNone: TImage;
    imgNo: TImage;
    imgCar: TImage;
    cbSoci: TComboBox;
    pSoci: TPanel;
    pData: TPanel;
    frData: TCalFrm;
    pCastells: TPanel;
    lCastells: TLabel;
    recColles: TCalloutRectangle;
    Image1: TImage;
    lbCastells: TListBox;
    pIconsC: TPanel;
    ilImages: TImageList;
    procedure cbSociChange(Sender: TObject);
    procedure frDataimgEditClick(Sender: TObject);
  private
    procedure SelectCurrentUser;
    procedure GetInfoEventFromUser;
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
  AssistenciaFrm: TAssistenciaFrm;

implementation

uses
  FMX.MultiResBitmap,
  UIniFiles, USocisMdl, UImagesMdl;

{$R *.fmx}

{ TAssistenciaFrm }

function TAssistenciaFrm.AcceptForm: Boolean;
begin

end;

procedure TAssistenciaFrm.AfterShow;
var
  L: TStringList;
begin
  Ani(True);
  TThread.CreateAnonymousThread(procedure
    begin
      // afegim usuaris
      TFileIni.SetFileIni(TGenFunc.GetIniName);
      L := nil;
      try
        L := TStringList.Create;
        TFileIni.GetSection('USERS', L);
        TSocisMdl.AddUsersToCombobox(L.CommaText, cbSoci);
      finally
        FreeAndNil(L);
      end;

      // seleccionem usuari
      SelectCurrentUser;

      // seleccionem informació de l'event i l'usuari seleccionat
      GetInfoEventFromUser;

      // treiem animació
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Ani(False);
        end);
    end).Start;

  if not Assigned(TagObject) then Exit;
  if not (TagObject is TItemEvent) then Exit;

  frData.Id := TItemEvent(TagObject).Id;

  frData.lType.Text := TItemEvent(TagObject).Nom_curt + ':';
  frData.lDesc.Text := TItemEvent(TagObject).Descrip;

  imgBus.Visible := TItemEvent(TagObject).Bus = '1';
  imgCar.Visible := TItemEvent(TagObject).Cotxe = '1';
  pIconsC.Width := imgOk.Width + imgNone.Width + imgNo.Width;
  if imgBus.Visible then pIconsC.Width := pIconsC.Width + imgBus.Width;

  if TItemEvent(TagObject).Datai <> '' then
    frData.lCalendar.Text := FormatDateTime('dd/mm/yyyy', TItemEvent(TagObject).DataiToDate);
  if TItemEvent(TagObject).Busd <> '' then
    frData.lBus.Text := FormatDateTime('dd/mm/yyyy', TItemEvent(TagObject).BusdToDate);
  if TItemEvent(TagObject).Buslloc <> '' then
    if frData.lBus.Text <> '' then
      frData.lBus.Text := frData.lBus.Text + ' - ' + TItemEvent(TagObject).Buslloc
    else
      frData.lBus.Text := TItemEvent(TagObject).Buslloc;
  frData.lMeeting.Text := TItemEvent(TagObject).Quedar;
  frData.lPlace.Text := TItemEvent(TagObject).Lloc;
  if (TItemEvent(TagObject).Horai <> '') and (TItemEvent(TagObject).Horai <> '00:00:00') then
    frData.lTime.Text := FormatDateTime('hh''h''nn', TItemEvent(TagObject).HoraiToTime);
  frData.lColles.Text := TItemEvent(TagObject).Colles;

  gplLayout.RowCollection.Items[2].Value := frData.GetTotalHeight;
end;

procedure TAssistenciaFrm.cbSociChange(Sender: TObject);
begin
  inherited;

  Ani(True);
  TThread.CreateAnonymousThread(procedure
    begin
      // seleccionem informació de l'event i l'usuari seleccionat
      GetInfoEventFromUser;

      // treiem animació
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Ani(False);
        end);
    end).Start;
end;

constructor TAssistenciaFrm.Create(AOwner: TComponent);
begin
  inherited;

  lbCastells.Clear;
end;

function TAssistenciaFrm.DeactivateAll: Boolean;
begin
  Result := True;
end;

function TAssistenciaFrm.DefaultStateAcceptButton: Boolean;
begin
  Result := True;
end;

function TAssistenciaFrm.EnabledBackButton: Boolean;
begin
  Result := False;
end;

procedure TAssistenciaFrm.frDataimgEditClick(Sender: TObject);
begin
  inherited;

  ShowMessage('edit event');
end;

procedure TAssistenciaFrm.GetInfoEventFromUser;
var
  Assit: TAssistencia;
  MdlImg: TImagesMdl;
  bmImg: TCustomBitmapItem;
  Size: TSize;
begin
  if cbSoci.ItemIndex = -1 then
    Exit;

  Assit := TSocisMdl.GetInfoEventFromSoci(TSoci(cbSoci.Items.Objects[cbSoci.ItemIndex]).ActCode, frData.Id);

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      MdlImg := TImagesMdl.Create(Self);
      try
        if Assit.Bus = '1' then
          MdlImg.ilImages.BitmapItemByName('bus32', bmImg, Size)
        else
          MdlImg.ilImages.BitmapItemByName('bus_d32', bmImg, Size);
        imgBus.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];

        if Assit.Assist = '' then
        begin
          MdlImg.ilImages.BitmapItemByName('cal_no_d32', bmImg, Size);
          imgNo.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];

          MdlImg.ilImages.BitmapItemByName('cal_ok_d32', bmImg, Size);
          imgOk.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];

          MdlImg.ilImages.BitmapItemByName('cal_none_d32', bmImg, Size);
          imgNone.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
        end
        else
          case Assit.Assist.ToInteger of
            0:
            begin
              MdlImg.ilImages.BitmapItemByName('cal_d32', bmImg, Size);
              imgNo.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
              MdlImg.ilImages.BitmapItemByName('cal_ok_d32', bmImg, Size);
              imgOk.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
              MdlImg.ilImages.BitmapItemByName('cal_none_d32', bmImg, Size);
              imgNone.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
            end;
            1:
            begin
              MdlImg.ilImages.BitmapItemByName('cal_no_d32', bmImg, Size);
              imgNo.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
              MdlImg.ilImages.BitmapItemByName('cal_ok32', bmImg, Size);
              imgOk.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
              MdlImg.ilImages.BitmapItemByName('cal_none_d32', bmImg, Size);
              imgNone.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
            end;
            2:
            begin
              MdlImg.ilImages.BitmapItemByName('cal_no_d32', bmImg, Size);
              imgNo.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
              MdlImg.ilImages.BitmapItemByName('cal_ok_d32', bmImg, Size);
              imgOk.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
              MdlImg.ilImages.BitmapItemByName('cal_none32', bmImg, Size);
              imgNone.Bitmap := bmImg.MultiResBitmap.Bitmaps[1.0];
            end;
          end;
      finally
        FreeAndNil(MdlImg);
      end;
    end);
end;

procedure TAssistenciaFrm.SelectCurrentUser;
var
  i: Integer;
begin
  for i := 0 to cbSoci.Items.Count - 1 do
  begin
    if TSoci(cbSoci.Items.Objects[i]).ActCode = TItemEvent(TagObject).Soci.ActCode then
    begin
      cbSoci.ItemIndex := i;
      Break;
    end;
  end;
end;

function TAssistenciaFrm.SetCaption: string;
begin
  Result := cCaption;
end;

end.
