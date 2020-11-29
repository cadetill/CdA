unit UInterfaces;

interface

uses
  FMX.Forms, FMX.Types,
  UClasses;

type
  IChildren = interface
    ['{1E07F6BA-1273-442C-B1F9-7B10CC2AB63E}']
    function SetCaption: string;
    function DeactivateAll: Boolean;
    function AcceptForm: Boolean;
    function EnabledBackButton: Boolean;
    function DefaultStateAcceptButton: Boolean;
    procedure AfterShow;
  end;

  IMainMenu = interface
    ['{E1646D23-CB62-4E82-8A04-8E51AABAD439}']
    procedure ShowAni(pAnimate: Boolean; pMessage: string = 'Carregant...');
    procedure ShowAcceptButton(State: Boolean);
    procedure MenuControl;
    procedure CreateForm(ClassForm: TFmxObjectClass; DataObject: TObject);
    function GetInfoUser: TUser;
  end;

implementation

end.
