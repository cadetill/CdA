unit uGenFunc;

interface

uses
  FMX.ListBox,
  System.Classes;

type
  TGenFunc = record
  public
    class function GetIniName: string; static;
    class function GetBaseFolder: string; static;
    class function GetImgFolder: string; static;

    class function GetField(S: string; FieldIndex: Integer; Delimiter: Char): string; static;

    class procedure QuickSort(var A: array of Integer; iLo, iHi: Integer); static;

    class procedure CopyToClipboard(Text: string); static;

    class function StringToDate(Value: string; WithSeparator: Boolean = True): TDate; static;
    class function StringToTime(Value: string): TTime; static;
    class function NormalizeString(Value: string): string; static;
    class function DeNormalizeString(Value: string): string; static;

    class function IsConnected: Boolean; static;
    class function IsWifiConnected: Boolean; static;
    class function IsMobileConnected: Boolean; static;
    class function CurrentSSID: string; static;
  end;

implementation

uses
  uMessage, NetworkState,
  System.IOUtils, System.SysUtils,
  FMX.Platform;

{ TGenFunc }

class procedure TGenFunc.CopyToClipboard(Text: string);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    Svc.SetClipboard(Text);

    TMessage.Show('Text copiat al portapapers.');
  end;
end;

class function TGenFunc.CurrentSSID: string;
var
  NS: TNetworkState;
begin
  NS := TNetworkState.Create;
  try
    Result := NS.CurrentSSID;
  finally
    NS.Free;
  end;
end;

class function TGenFunc.DeNormalizeString(Value: string): string;
begin
  Result := StringReplace(Value, '%%%', '''', [rfReplaceAll]);
end;

class function TGenFunc.GetBaseFolder: string;
begin
  {$IFDEF MSWINDOWS}
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(ParamStr(0)));
  {$ELSE}
    {$IFDEF ANDROID}
  Result := TPath.Combine(TPath.GetDocumentsPath, IncludeTrailingPathDelimiter('CollAPP'));
    {$ELSE}
  Result := TPath.Combine(TPath.GetSharedDownloadsPath, IncludeTrailingPathDelimiter('CollAPP'));
    {$ENDIF}
  {$ENDIF}
  if not TDirectory.Exists(TPath.GetDirectoryName(Result)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(Result))
end;

class function TGenFunc.GetField(S: string; FieldIndex: Integer; Delimiter: Char): string;
var
  DelimiterPos: Integer;
  loopCount: Integer;
  sRecord, sField: string;
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
    else
      sField := sRecord;
    loopCount := loopCount + 1;
  end;
  Result := sField;
end;

class function TGenFunc.GetImgFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(TGenFunc.GetBaseFolder + 'Images');
end;

class function TGenFunc.GetIniName: string;
begin
  Result := TGenFunc.GetBaseFolder + 'Config.ini';
end;

class function TGenFunc.IsConnected: Boolean;
var
  NS: TNetworkState;
begin
  NS := TNetworkState.Create;
  try
    Result := NS.IsConnected;
  finally
    NS.Free;
  end;
end;

class function TGenFunc.IsMobileConnected: Boolean;
var
  NS: TNetworkState;
begin
  NS := TNetworkState.Create;
  try
    Result := NS.IsMobileConnected;
  finally
    NS.Free;
  end;
end;

class function TGenFunc.IsWifiConnected: Boolean;
var
  NS: TNetworkState;
begin
  NS := TNetworkState.Create;
  try
    Result := NS.IsWifiConnected;
  finally
    NS.Free;
  end;
end;

class function TGenFunc.NormalizeString(Value: string): string;
begin
  Result := StringReplace(Value, '''', '%%%', [rfReplaceAll]);
end;

class procedure TGenFunc.QuickSort(var A: array of Integer; iLo, iHi: Integer);
var
  Lo, Hi, Pivot, T: Integer;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := A[(Lo + Hi) div 2];

  repeat
    while A[Lo] < Pivot do
      Inc(Lo);
    while A[Hi] > Pivot do
      Dec(Hi);
    if Lo <= Hi then
    begin
      T := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;

  if Hi > iLo then
    QuickSort(A, iLo, Hi);
  if Lo < iHi then
    QuickSort(A, Lo, iHi);
end;

class function TGenFunc.StringToDate(Value: string;
  WithSeparator: Boolean): TDate;
var
  TmpI: Integer;
begin
  Result := EncodeDate(1900,1,1);

  if (Value = '') or (Value = '0000-00-00') then Exit;

  TmpI := 10;
  if not WithSeparator then TmpI := 8;
  if Length(Value) < TmpI then Exit;

  Value := Copy(Value, 1, TmpI);

  if WithSeparator then
    Result := EncodeDate(StrToInt(Copy(Value, 1, 4)), StrToInt(Copy(Value, 6, 2)), StrToInt(Copy(Value, 9, 2)))
  else
    Result := EncodeDate(StrToInt(Copy(Value, 1, 4)), StrToInt(Copy(Value, 5, 2)), StrToInt(Copy(Value, 7, 2)));
end;

class function TGenFunc.StringToTime(Value: string): TTime;
begin
  Result := EncodeTime(0,0,0,0);

  if Value = '' then
    Exit;

  if Length(Value) >= 19 then
  begin
    Value := Copy(Value, 12, 8);
    Result := EncodeTime(StrToInt(GetField(Value, 1, ':')), StrToInt(GetField(Value, 2, ':')), StrToInt(GetField(Value, 3, ':')), 0);
  end;

  if Length(Value) = 8 then
    Result := EncodeTime(StrToInt(GetField(Value, 1, ':')), StrToInt(GetField(Value, 2, ':')), StrToInt(GetField(Value, 3, ':')), 0);
end;

end.

