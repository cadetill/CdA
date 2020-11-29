unit URolsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.SearchBox, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts,
  uInterfaces, uRols;

type
  TRolsFrm = class(TForm, IChildren)
    lbRols: TListBox;
    ListBoxItem1: TListBoxItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lbiSearch: TSearchBox;
  private
    FRols: TRols;

    procedure CreateItems;
    procedure CreateItem(Rol: TRol);
  public
    function SetCaption: string;
    function ShowOkButton: Boolean;
    function ShowBackButton: Boolean;
    function AcceptForm: Boolean;
    procedure AfterShow;
  end;

var
  RolsFrm: TRolsFrm;

implementation

uses
  uMessage, uResultRequest;

{$R *.fmx}

{ TRolsFrm }

function TRolsFrm.AcceptForm: Boolean;
begin
  Result := True;
end;

procedure TRolsFrm.AfterShow;
var
  Intf: IMainMenu;
begin
  lbRols.Clear;
  lbiSearch.Text := '';

  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  var
    Res: TResultRequest;
  begin
    Res := TResultRequest.Create;
    try
      FRols := TRols.GetRols(Res);

      if Res.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(Res.Error);
          end);
      end;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          CreateItems;
        end);
    finally
      FreeAndNil(Res);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Supports(Owner, IMainMenu, Intf)  then
            Intf.ShowAni(False);
        end);
    end;
  end
  ).Start;
end;

procedure TRolsFrm.CreateItem(Rol: TRol);
var
  lbItem: TListBoxItem;
begin
  lbItem := TListBoxItem.Create(lbRols);
  lbItem.Text := Rol.descrip;
  lbItem.TagString := Rol.id;
  lbRols.AddObject(lbItem);
end;

procedure TRolsFrm.CreateItems;
var
  i: Integer;
begin
  if not Assigned(FRols) then
    Exit;

  lbRols.BeginUpdate;
  for i := 0 to FRols.Count do
    CreateItem(FRols.Items[i]);
  lbRols.EndUpdate;
end;

function TRolsFrm.SetCaption: string;
begin
  Result := 'Rols';
end;

function TRolsFrm.ShowBackButton: Boolean;
begin
  Result := True;
end;

function TRolsFrm.ShowOkButton: Boolean;
begin
  Result := False;
end;

end.
