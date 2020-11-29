// @include(..\docs\UFormList.txt)
unit UFormList;

interface

uses
  FMX.Forms, System.Generics.Collections, FMX.Controls,
  System.Classes,
  UHelpers;

type
  // @include(..\docs\UFormList.TFormList.txt)
  TFormList = class
  private
    FFrmList: TObjectList<TCustomForm>;
    FContent: TControl;

    procedure ThreadTerminated(Sender: TObject);
  public
    // @include(..\docs\UFormList.TFormList.Create.txt)
    constructor Create(aContent: TControl);
    // @include(..\docs\UFormList.TFormList.Destroy.txt)
    destructor Destroy; override;

    // @include(..\docs\UFormList.TFormList.PushForm.txt)
    procedure PushForm(AForm: TCustomForm);
    // @include(..\docs\UFormList.TFormList.PopForm.txt)
    procedure PopForm;

    // @include(..\docs\UFormList.TFormList.GetFormCount.txt)
    function GetFormCount: Integer;
  end;

implementation

uses
  System.SysUtils,
  UInterfaces;

{ TFormList }

constructor TFormList.Create(aContent: TControl);
begin
  FContent := aContent;
  FFrmList := TObjectList<TCustomForm>.Create;
end;

destructor TFormList.Destroy;
begin
  if Assigned(FFrmList) then
    FreeAndNil(FFrmList);

  inherited;
end;

function TFormList.GetFormCount: Integer;
begin
  Result := FFrmList.Count;
end;

procedure TFormList.PopForm;
var
  AForm: TCustomForm;
begin
  // si no hi ha formularis apilats, marxem
  if FFrmList.Count = 0 then
    Exit;

  // tornem referències de parent
  while FContent.ChildrenCount > 0 do
    FContent.Children[0].Parent := FFrmList.Items[FFrmList.Count - 1];

  // desapilem darrer formulari mostrat
  FFrmList.Delete(FFrmList.Count - 1);

  // si queden formularis
  if FFrmList.Count > 0 then
  begin
    // agafem formulari
    AForm := FFrmList.Items[FFrmList.Count - 1];

    // asignem noves referencies al contenedor principal
    while AForm.ChildrenCount > 0 do
      AForm.Children[0].Parent := FContent;
  end;
end;

procedure TFormList.PushForm(AForm: TCustomForm);
var
  Thrd: TThread;
begin
  // si hi ha algun formulari actiu, li tornem les referències
  if FFrmList.Count > 0 then
  begin
    while FContent.ChildrenCount > 0 do
      FContent.Children[0].Parent := FFrmList.Items[FFrmList.Count - 1];
  end;

  // afegim nou form a la llista
  FFrmList.Add(AForm);

  // asignem noves referencies al contenedor principal
  while AForm.ChildrenCount > 0 do
    AForm.Children[0].Parent := FContent;

  FContent.Ani(True);
  Thrd := TThread.CreateAnonymousThread(
    procedure
    var
      Intf: ILoadData;
    begin
      if Supports(AForm, ILoadData, Intf) then
        Intf.LoadData;
    end
  );
  Thrd.OnTerminate := ThreadTerminated;
  Thrd.Start;
end;

procedure TFormList.ThreadTerminated(Sender: TObject);
begin
  FContent.Ani(False);
end;

end.
