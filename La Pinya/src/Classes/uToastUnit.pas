{
  @abstract(unit to generate a toast in Android)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 11, 2020)
  @lastmod(October 11, 2020)

  A toast is a message that is shown on the screen for a few seconds to the user and then automatically disappears again without requiring any type of action on their part, and without receiving the focus at any time.

  @bold(Change List) @br
  @unorderedList(
    @item(11/10/2020 : first version)
  )
}
unit uToastUnit;

interface

{$IFDEF ANDROID}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText;

type
  TToastLength = (LongToast, ShortToast);

  JToast = interface;

  JToastClass = interface(JObjectClass)
    ['{69E2D233-B9D3-4F3E-B882-474C8E1D50E9}']
    { Property methods }
    function _GetLENGTH_LONG: Integer; cdecl;
    function _GetLENGTH_SHORT: Integer; cdecl;
    { Methods }
    function init(context: JContext): JToast; cdecl; overload;
    function makeText(context: JContext; text: JCharSequence; duration: Integer): JToast; cdecl;
    { Properties }
    property LENGTH_LONG: Integer read _GetLENGTH_LONG;
    property LENGTH_SHORT: Integer read _GetLENGTH_SHORT;
  end;

  [JavaSignature('android/widget/Toast')]
  JToast = interface(JObject)
    ['{FD81CC32-BFBC-4838-8893-9DD01DE47B00}']
    { Methods }
    procedure cancel; cdecl;
    function getDuration: Integer; cdecl;
    function getGravity: Integer; cdecl;
    function getHorizontalMargin: Single; cdecl;
    function getVerticalMargin: Single; cdecl;
    function getView: JView; cdecl;
    function getXOffset: Integer; cdecl;
    function getYOffset: Integer; cdecl;
    procedure setDuration(value: Integer); cdecl;
    procedure setGravity(gravity, xOffset, yOffset: Integer); cdecl;
    procedure setMargin(horizontalMargin, verticalMargin: Single); cdecl;
    procedure setText(s: JCharSequence); cdecl;
    procedure setView(view: JView); cdecl;
    procedure show; cdecl;
  end;

  TJToast = class(TJavaGenericImport<JToastClass, JToast>)
  end;

procedure Toast(const Msg: string; duration: TToastLength = ShortToast);

{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  FMX.Helpers.Android,
  Androidapi.Helpers;

procedure Toast(const Msg: string; duration: TToastLength);
var
  ToastLength: Integer;
begin
  if duration = ShortToast then
    ToastLength := TJToast.JavaClass.LENGTH_SHORT
  else
    ToastLength := TJToast.JavaClass.LENGTH_LONG;
  CallInUiThread(
    procedure
    begin
      TJToast.JavaClass.makeText(TAndroidHelper.Context, StrToJCharSequence(Msg), ToastLength).show
    end);
end;

{$ENDIF}

end.
