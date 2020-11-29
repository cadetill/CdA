// @include(..\..\docs\UGenFuncs.txt)
unit UGenFuncs;

interface

type
  // @include(..\..\docs\UGenFuncs.TGenFuncs.txt)
  TGenFuncs = class
  public
    // @include(..\..\docs\UGenFuncs.TGenFuncs.IsValidEmail.txt)
    class function IsValidEmail(eMail: string): Boolean;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils, System.Character;

{ TGenFuncs }

class function TGenFuncs.IsValidEmail(eMail: string): Boolean;
var
  RegEx: TRegEx;
begin
  RegEx := TRegex.Create('^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]*[a-zA-Z0-9]+$');
  Result := RegEx.Match(eMail).Success;
end;

end.
