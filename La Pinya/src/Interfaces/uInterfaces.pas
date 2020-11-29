{
  @abstract(unit to define interfaces)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 06, 2020)
  @lastmod(October 06, 2020)

  The uInterfaces unit contains the definition of @link(IChildren) and @link(IMainMenu) interfaces.

  @bold(Change List) @br
  @unorderedList(
    @item(06/10/2020 : first version)
  )
}
unit uInterfaces;

interface

uses
  FMX.Forms, FMX.Types;

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uInterfaces.IChildren.txt)
  IChildren = interface
    ['{1E07F6BA-1273-442C-B1F9-7B10CC2AB63E}']
    // @include(..\..\docs\help\uInterfaces.IChildren.SetCaption.txt)
    function SetCaption: string;
    // @include(..\..\docs\help\uInterfaces.IChildren.AcceptForm.txt)
    function AcceptForm: Boolean;
    // @include(..\..\docs\help\uInterfaces.IChildren.ShowBackButton.txt)
    function ShowBackButton: Boolean;
    // @include(..\..\docs\help\uInterfaces.IChildren.ShowOkButton.txt)
    function ShowOkButton: Boolean;
    // @include(..\..\docs\help\uInterfaces.IChildren.AfterShow.txt)
    procedure AfterShow;
  end;

  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uInterfaces.IMainMenu.txt)
  IMainMenu = interface
    ['{E1646D23-CB62-4E82-8A04-8E51AABAD439}']
    // @include(..\..\docs\help\uInterfaces.IMainMenu.ShowAni.txt)
    function ShowAni(Show: Boolean): Boolean;
    // @include(..\..\docs\help\uInterfaces.IMainMenu.ShowAcceptButton.txt)
    procedure ShowAcceptButton(State: Boolean);
    // @include(..\..\docs\help\uInterfaces.IMainMenu.CreateForm.txt)
    procedure CreateForm(ClassForm: TFmxObjectClass; DataObject: TObject);
  end;

implementation

end.
