unit uMaps;

interface

uses
  System.Sensors;

type
  TMaps = record
  private
    class function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean; static;
  public
    class function OpenNavigation(const Addr: string): Boolean; overload; static;
    class function OpenNavigation(const Addr: string; const Coord: TLocationCoord2D): Boolean; overload; static;
  end;

implementation

uses
  {$IFDEF ANDROID}
  FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  {$ENDIF}
  {$IFDEF IOS}
  iOSapi.Foundation, FMX.Helpers.iOS, Macapi.Helpers,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Winapi.Shellapi, Winapi.Windows,
  {$ENDIF}
  {$IFDEF MACOS}
  {$ENDIF}
  IdURI, System.SysUtils, System.Classes, FMX.Dialogs;

{ TMaps }

class function TMaps.OpenNavigation(const Addr: string): Boolean;
var
  Coord: TLocationCoord2D;
begin
  Coord.Latitude := 0.0;
  Coord.Longitude := 0.0;
  Result := TMaps.OpenNavigation(Addr, Coord);
end;

class function TMaps.OpenNavigation(const Addr: string;
  const Coord: TLocationCoord2D): Boolean;
var
  CoordString: string;
begin
  {$IFDEF MSWINDOWS}
  Exit(OpenURL('https://www.google.com/maps/dir/' + Addr));
  {$ENDIF}

  //Open in Google Maps
  {$IFDEF ANDROID}
  Exit(OpenURL('http://maps.google.com/?q=' + Addr));
  {$ENDIF}

  //In iOS, if Google Maps is installed, use it, otherwise, use Apple Maps
  //If we have coordinates, use them as the start address
  {$IFDEF IOS}
  //Get a string of the longitute and latitute seperated by a comma if set
  CoordString := '';
  if (Coord.Latitude <> 0.0) or (Coord.Longitude <> 0.0) then
    CoordString := Coord.Latitude.ToString + ',' + Coord.Longitude.ToString;

  if not OpenURL('comgooglemaps://?daddr=' + Addr) then
  begin
    if (0.0 < CoordString.Length) then
    begin
      Exit(OpenURL('http://maps.apple.com/?daddr=' + Addr + '&saddr=loc:' + CoordString));
    end
    else begin
      Exit(OpenURL('http://maps.apple.com/?daddr=' + Addr));
    end;
  end
  else
  begin
    Exit(True);
  end;
  {$ENDIF IOS}

  //Unsupported platform
  Result := False;
end;

class function TMaps.OpenURL(const URL: string; const DisplayError: Boolean): Boolean;
{$IFDEF ANDROID}
var
  Intent: JIntent;
begin
  // There may be an issue with the geo: prefix and URLEncode.
  // will need to research
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
    //TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(URL))));
    TJnet_Uri.JavaClass.parse(StringToJString(URL)));
  try
    SharedActivity.startActivity(Intent);
    Exit(True);
  except
    on e: Exception do
    begin
      if DisplayError then ShowMessage('Error: ' + e.Message);
      Exit(False);
    end;
  end;
end;
{$ELSE}
  {$IFDEF IOS}
  var
    NSU: NSUrl;
  begin
    // iOS doesn't like spaces, so URL encode is important.
    // NSU := StrToNSUrl(TIdURI.URLEncode(URL));
    NSU := StrToNSUrl(URL);
    if SharedApplication.canOpenURL(NSU) then
      Exit(SharedApplication.openUrl(NSU))
    else
    begin
      if DisplayError then
        ShowMessage('Error: Opening "' + URL + '" not supported.');
      Exit(False);
    end;
  end;
  {$ELSE}
    {$IFDEF MSWINDOWS}
    begin
      ShellExecute(0, 'Open', PChar(Url), nil, nil, SW_SHOWNORMAL);
      Exit(True);
    end;
    {$ELSE}
    begin
      raise Exception.Create('Not supported!');
    end;
    {$ENDIF MSWINDOWS}
  {$ENDIF IOS}
{$ENDIF ANDROID}

end.
