{
  @abstract(Unit with the list of members)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(February 7, 2021)
  @lastmod(February 7, 2021)

  The UMembersFrm unit contains the list of all members defineds. You can edit, delete and add members.

  Change List @br
  @unorderedList(
    @item(02/07/2020 : first version)
  )
}
unit UMembersFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.SearchBox, FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts,
  UBaseListFrm, uMembers;

type
  { -------------------------------------------------------------------------- }
  // @include(..\docs\help\UMembersFrm.TMembersFrm.txt)
  TMembersFrm = class(TBaseListFrm)
    // @include(..\docs\help\UMembersFrm.TMembersFrm.bAddClick.txt)
    procedure bAddClick(Sender: TObject);
  protected
    // @include(..\docs\help\UMembersFrm.TMembersFrm.FData.txt)
    FData: TMembers;

    // @include(..\docs\help\UMembersFrm.TMembersFrm.CreateItems.txt)
    procedure CreateItems;
    // @include(..\docs\help\UMembersFrm.TMembersFrm.CreateItem.txt)
    procedure CreateItem(Data: TMember);
    // @include(..\docs\help\UMembersFrm.TMembersFrm.OnClickBEdit.txt)
    procedure OnClickBEdit(Sender: TObject); override;
    // @include(..\docs\help\UMembersFrm.TMembersFrm.OnClickBDel.txt)
    procedure OnClickBDel(Sender: TObject); override;
    // @include(..\docs\help\UMembersFrm.TMembersFrm.OnDataChange.txt)
    procedure OnDataChange(Sender: TObject);
  public
    // @include(..\docs\help\UMembersFrm.TMembersFrm.Create.txt)
    constructor Create(AOwner: TComponent); override;
    // @include(..\docs\help\UMembersFrm.TMembersFrm.Destroy.txt)
    destructor Destroy; override;

    // @include(..\docs\help\UBaseListFrm.TBaseListFrm.SetCaption.txt)
    function SetCaption: string; override;
  end;

implementation

uses
  FMX.DialogService,
  uMessage, uGenFunc, uResultRequest, UMemberFrm, uInterfaces;

{$R *.fmx}

{ TMembersFrm }

procedure TMembersFrm.bAddClick(Sender: TObject);
begin
  inherited;

  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  TDialogService.InputQuery('Nou Soci', ['Nom'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    var
      Resp: TResultRequest;
      TmpI: Integer;
    begin
      if AResult <> mrOk then
        Exit;

      if (Trim(AValues[0]) = '') then
        Exit;

      Resp := TMembers.AddMember(Trim(AValues[0]));
      if TryStrToInt(Resp.Id, TmpI) and (TmpI > 0) and (Resp.Error = '') then
      begin
        TmpI := FData.Add(Resp.id, Trim(AValues[0]));
        CreateItem(FData.Items[TmpI]);
      end
      else
        TMessage.MsjErr('Error creant soci', []);
    end);
end;

constructor TMembersFrm.Create(AOwner: TComponent);
var
  Intf: IMainMenu;
begin
  inherited;

  lbData.Clear;
  lbiSearch.Text := '';

  if Supports(Owner, IMainMenu, Intf)  then
    Intf.ShowAni(True);

  TThread.CreateAnonymousThread(procedure
  begin
    try
      FData := TMembers.GetMembers;
      if FData.Error <> '' then
      begin
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            TMessage.Show(FData.Error);
          end);
      end;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          CreateItems;
        end);
    finally
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

procedure TMembersFrm.CreateItem(Data: TMember);
var
  lbItem: TListBoxItem;
begin
  Data.OnChange := OnDataChange;

  lbItem := CreateLBItem(Data.Nom, Data.Nom + ' / ' + Data.Malnom, Data.Id);
  CreateButtonEdit(Data.Id, lbItem);
  CreateButtonDel(Data.Id, lbItem);
end;

procedure TMembersFrm.CreateItems;
var
  i: Integer;
begin
  if not Assigned(FData) then
    Exit;

  lbData.BeginUpdate;
  try
    for i := 0 to FData.Items.Count - 1 do
      CreateItem(FData.Items[i]);
  finally
    lbData.EndUpdate;
  end;
end;

destructor TMembersFrm.Destroy;
begin
  if Assigned(FData) then
    FreeAndNil(FData);

  inherited;
end;

procedure TMembersFrm.OnClickBDel(Sender: TObject);
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  if not (Sender is TSpeedButton) or not (TSpeedButton(Sender).Owner is TListBoxItem) then
    Exit;

  TMessage.MsjSiNo('Realment vol esborrar el soci "%s"?', [TListBoxItem(TSpeedButton(Sender).Owner).Text],
    procedure
    var
      Resp: TResultRequest;
      Intf: IMainMenu;
    begin
      if Supports(Owner, IMainMenu, Intf)  then
        Intf.ShowAni(True);

      TThread.CreateAnonymousThread(procedure
      begin
        Resp := TMembers.DelMember(TSpeedButton(Sender).TagString);

        if Resp.Error <> '' then
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              TMessage.Show(Resp.Error);
            end);

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            if Supports(Owner, IMainMenu, Intf)  then
              Intf.ShowAni(False);
          end);
      end
      ).Start;

      lbData.RemoveObject(TListBoxItem(TSpeedButton(Sender).Owner));
    end);
end;

procedure TMembersFrm.OnClickBEdit(Sender: TObject);
var
  Intf: IMainMenu;
  Idx: Integer;
begin
  if not TGenFunc.IsConnected then
  begin
    TMessage.Show('Sense Connexió');
    Exit;
  end;

  if not (Sender is TSpeedButton) then
    Exit;

  Idx := FData.IndexOf(TSpeedButton(Sender).TagString);
  if Idx < 0 then
    Exit;

  // si es pot, creem formulari
  if Supports(Owner, IMainMenu, Intf) then
    Intf.CreateForm(TMemberFrm, FData.Items[Idx]);
end;

procedure TMembersFrm.OnDataChange(Sender: TObject);
var
  Idx: Integer;
  i: Integer;
begin
  if not (Sender is TMember) then
    Exit;

  Idx := FData.IndexOf(TMember(Sender).id);
  if Idx < 0 then
    Exit;

  for i := 0 to lbData.Count - 1 do
  begin
    if SameText(lbData.ItemByIndex(i).TagString, Tmember(Sender).id) then
    begin
      lbData.ItemIndex := i;
      lbData.ItemByIndex(i).Text := TMember(Sender).Nom;
      lbData.ItemByIndex(i).ItemData.Detail := TMember(Sender).Nom + ' / ' + TMember(Sender).Malnom;
      Break;
    end;
  end;
end;

function TMembersFrm.SetCaption: string;
begin
  Result := 'Membres de la colla';
end;

end.
