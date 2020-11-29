unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ExtDlgs,
  System.ImageList, Vcl.ImgList;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    bExecute: TButton;
    OpenTextFileDialog1: TOpenTextFileDialog;
    eFile: TButtonedEdit;
    ImageList1: TImageList;
    SaveDialog1: TSaveDialog;
    cbWithHeader: TCheckBox;
    procedure eFileRightButtonClick(Sender: TObject);
    procedure bExecuteClick(Sender: TObject);
  private
    function GetField(S: string; FieldIndex: integer; Delimiter: Char): string;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.bExecuteClick(Sender: TObject);
var
  F,D: TStringList;
  i: Integer;
  Start: Integer;
  Tmp: string;
begin
  if not FileExists(eFile.Text) then
    Exit;

  F := TStringList.Create;
  D := TStringList.Create;
  try
    F.LoadFromFile(eFile.Text);

    Start := 0;
    if cbWithHeader.Checked then
      Start := 1;
    D.Add('Castell,Casteller');
    for i := Start to F.Count - 1 do
    begin
      Tmp := GetField(F[i],3,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],4,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],5,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],6,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],7,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],8,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],9,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
      Tmp := GetField(F[i],10,',');
      if Tmp <> '' then D.Add(GetField(F[i],2,',') + ',' + Tmp);
    end;
    if SaveDialog1.Execute then
      D.SaveToFile(SaveDialog1.FileName);
  finally
    FreeAndNil(F);
    FreeAndNil(D);
  end;
end;

procedure TForm2.eFileRightButtonClick(Sender: TObject);
begin
  if OpenTextFileDialog1.Execute then
    eFile.Text := OpenTextFileDialog1.FileName;
end;

function TForm2.GetField(S: string; FieldIndex: integer;
  Delimiter: Char): string;
var
  DelimiterPos: integer;
  loopCount: integer;
  sRecord,
  sField: string;
begin
  loopCount := 1;
  sRecord := S;
  while loopCount <= FieldIndex do
  begin
    DelimiterPos := Pos(Delimiter, sRecord);
    if DelimiterPos <> 0 then
    begin
      sField := Copy(sRecord, 1, DelimiterPos - 1);
      Delete(sRecord, 1, DelimiterPos);
    end
    else sField := sRecord;
    loopCount := loopCount + 1;
  end;
  Result := sField;
end;

end.
