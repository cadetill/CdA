unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects,
  System.ImageList, FMX.ImgList,
  UHelpers, UCalendarMdl, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid;

type
  TForm2 = class(TForm)
    bGetCalendars: TButton;
    bGetEventsGC: TButton;
    bGetEventsCdA: TButton;
    Panel1: TPanel;
    lbCalendars: TListBox;
    ImageList1: TImageList;
    lbItems: TListBox;
    ListBoxItem2: TListBoxItem;
    Image2: TImage;
    Button4: TButton;
    cbTemporades: TComboBox;
    bGetSeasons: TButton;
    Image1: TImage;
    lbSeason: TListBox;
    ListBoxItem1: TListBoxItem;
    Image3: TImage;
    bMergeEvents: TButton;
    procedure bGetCalendarsClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure bGetEventsGCClick(Sender: TObject);
    procedure bGetSeasonsClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure bGetEventsCdAClick(Sender: TObject);
    procedure bMergeEventsClick(Sender: TObject);
  private
    FCalMdl: TCalendarMdl;

    procedure OnClickDelCalendar(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form2: TForm2;

implementation

uses
  System.Threading, FMX.MultiResBitmap,
  UMessage, UClasses;

{$R *.fmx}

procedure TForm2.bGetCalendarsClick(Sender: TObject);
begin
  Panel1.Ani(True);
  TThread.CreateAnonymousThread(procedure
    var
      lbItem: TListBoxItem;
      Img: TImage;
      bmItem: TCustomBitmapItem;
      Size: TSize;
    begin
      FCalMdl.GetCalendars;

      ImageList1.BitmapItemByName('trash', bmItem, Size);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          lbCalendars.Clear;

          while not FCalMdl.cdsItems.Eof do
          begin
            // creem ListBoxItem i assignem propietats
            lbItem := TListBoxItem.Create(lbCalendars);
            lbItem.Text := FCalMdl.cdsItems.FieldByName('nom').AsString;
            lbItem.ItemData.Detail := FCalMdl.cdsItems.FieldByName('idcalendar').AsString;

            // creem objecte per posar la info del registre
            lbItem.ItemCal := TItemCal.Create;
            lbItem.ItemCal.Id := FCalMdl.cdsItems.FieldByName('id').AsString;
            lbItem.ItemCal.IdCalendar := FCalMdl.cdsItems.FieldByName('idcalendar').AsString;
            lbItem.ItemCal.Key := FCalMdl.cdsItems.FieldByName('key').AsString;
            lbItem.ItemCal.Nom := FCalMdl.cdsItems.FieldByName('nom').AsString;

            // creem imatge d'esborrar
            Img := TImage.Create(Self);
            Img.Parent := lbItem;
            Img.Bitmap := bmItem.MultiResBitmap.Bitmaps[1.0];
            Img.Align := TAlignLayout.Right;
            Img.Margins.Bottom := 0;
            Img.Margins.Left := 0;
            Img.Margins.Right := 3;
            Img.Margins.Top := 0;
            Img.Width := Img.Height;
            Img.OnClick := OnClickDelCalendar;
            {$IFDEF MSWINDOWS}
            Img.Cursor := crHandPoint;
            {$ENDIF}

            // assignem Item a la llista
            lbCalendars.AddObject(lbItem);

            FCalMdl.cdsItems.Next;
          end;
        end);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Panel1.Ani(False);
        end);
    end).Start;
end;

procedure TForm2.bGetEventsGCClick(Sender: TObject);
var
  lbItem: TListBoxItem;
begin
  if lbCalendars.Count = 0 then
  begin
    TMessage.Show('No hi ha calendaris definits');
    Exit;
  end;

  Panel1.Ani(True);
  TThread.CreateAnonymousThread(procedure
    var
      i: Integer;
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          lbItems.Clear;
        end);

      for i := 0 to lbCalendars.Count - 1 do
      begin
        lbItem := TListBoxItem(lbCalendars.Content.Controls[i]);
        // per cada un dels calendaris, cal fer una consulta a Google Calendar
        FCalMdl.GetCalendarItems(lbItem.ItemCal.Id,
                                 lbItem.ItemCal.IdCalendar,
                                 lbItem.ItemCal.Key,
                                 TTemporada(cbTemporades.Items.Objects[cbTemporades.ItemIndex]).Any);
      end;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          FCalMdl.cdsTmpEvents.First;
          while not FCalMdl.cdsTmpEvents.Eof do
          begin
            //if FCalMdl.cdsTmpEventsstatus.AsString = cStatusConfirmed then
            begin
              lbItem := TListBoxItem.Create(lbItems);
              lbItem.Text := FCalMdl.cdsTmpEventssummary.AsString;
              lbItem.ItemData.Detail := FCalMdl.cdsTmpEventsdatai.AsString + ' - ' + FCalMdl.cdsTmpEventsstatus.AsString;

              lbItems.AddObject(lbItem);
            end;

            FCalMdl.cdsTmpEvents.Next;
          end;
        end);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Panel1.Ani(False);
        end);
    end).Start;
end;

procedure TForm2.bGetEventsCdAClick(Sender: TObject);
begin
  if cbTemporades.ItemIndex = -1 then
  begin
    TMessage.Show('Selecciona una temporada!');
    Exit;
  end;

  Panel1.Ani(True);

  TThread.CreateAnonymousThread(procedure
    var
      lbItem: TListBoxItem;
    begin
      FCalMdl.GetEventsBySeason(TTemporada(cbTemporades.Items.Objects[cbTemporades.ItemIndex]).Any);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          lbSeason.Clear;

          while not FCalMdl.cdsEvents.Eof do
          begin
            // creem ListBoxItem i assignem propietats
            lbItem := TListBoxItem.Create(lbSeason);
            lbItem.Text := FCalMdl.cdsEvents.FieldByName('descrip').AsString;
            lbItem.ItemData.Detail := FCalMdl.cdsEvents.FieldByName('lloc').AsString;

            // assignem Item a la llista
            lbSeason.AddObject(lbItem);

            FCalMdl.cdsEvents.Next;
          end;
        end);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Panel1.Ani(False);
        end);
    end).Start;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  TMessage.Show('això és una demo de missatge');
end;

procedure TForm2.bGetSeasonsClick(Sender: TObject);
begin
  Panel1.Ani(True);
  TThread.CreateAnonymousThread(procedure
    begin
      FCalMdl.GetTemporades;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          Temp: TTemporada;
        begin
          cbTemporades.Clear;

          while not FCalMdl.cdsTemporades.Eof do
          begin
            Temp := TTemporada.Create;
            Temp.Any := FCalMdl.cdsTemporades.FieldByName('any').AsString;
            Temp.Descripcio := FCalMdl.cdsTemporades.FieldByName('descripcio').AsString;

            cbTemporades.Items.AddObject(Temp.Descripcio, Temp);

            FCalMdl.cdsTemporades.Next;
          end;

          if cbTemporades.Count > 0 then
            cbTemporades.ItemIndex := 0;
        end);

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Panel1.Ani(False);
        end);
    end).Start;
end;

procedure TForm2.bMergeEventsClick(Sender: TObject);
begin
  FCalMdl.MergeEvents;
end;

constructor TForm2.Create(AOwner: TComponent);
begin
  inherited;

  FCalMdl := TCalendarMdl.Create(Self);
end;

destructor TForm2.Destroy;
begin
  if Assigned(FCalMdl) then
    FreeAndNil(FCalMdl);

  inherited;
end;

procedure TForm2.Image1Click(Sender: TObject);
begin
  // nova pantalla on es demanarà any i descripció
end;

procedure TForm2.OnClickDelCalendar(Sender: TObject);
begin
  // comprovacions
  if not (Sender is TImage) then Exit;
  if not (TImage(Sender).Parent is TListBoxItem) then Exit;
  if not Assigned(TListBoxItem(TImage(Sender).Parent).ItemCal) then Exit;

  TMessage.MsjSiNo('Vol esborrar el calendari "%s"?', [TListBoxItem(TImage(Sender).Parent).ItemCal.Nom],
        procedure
        begin
          // esborrem calendari
          TThread.CreateAnonymousThread(procedure
            begin
              FCalMdl.DelCalendar(TListBoxItem(TImage(Sender).Parent).ItemCal.Id);

              TThread.Synchronize(TThread.CurrentThread,
                procedure
                begin
                  TListBox(TListBoxItem(TImage(Sender).Parent).Parent.Parent).Items.Delete(TListBoxItem(TImage(Sender).Parent).Index);
                end);
            end).Start;
        end,
        nil);
end;

end.
