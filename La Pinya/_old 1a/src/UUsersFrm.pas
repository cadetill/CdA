// @include(..\docs\UUsersFrm.txt)
unit UUsersFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UBaseChildFrm,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.SearchBox,
  UInterfaces, System.Rtti;

type
  // @include(..\docs\UUsersFrm.TUsersFrm.txt)
  TUsersFrm = class(TBaseChildFrm, ILoadData, IFormList)
    {@exclude} lbUsers: TListBox;
    {@exclude} SearchBox1: TSearchBox;
  private
    {@exclude} procedure ListBoxItemClick(Sender: TObject);
  protected
    // @include(..\docs\UUsersFrm.TUsersFrm.LoadData.txt)
    procedure LoadData; override;
  public
  end;

  // @include(..\docs\UUsersFrm.TUser.txt)
  TUser = class
  private
    FId: string;
    FIdSoci: string;
    FMail: string;
    FActive: string;
    FUser: string;
  public
    // @include(..\docs\UUsersFrm.TUser.Id.txt)
    property Id: string read FId write FId;
    // @include(..\docs\UUsersFrm.TUser.User.txt)
    property User: string read FUser write FUser;
    // @include(..\docs\UUsersFrm.TUser.Mail.txt)
    property Mail: string read FMail write FMail;
    // @include(..\docs\UUsersFrm.TUser.IdSoci.txt)
    property IdSoci: string read FIdSoci write FIdSoci;
    // @include(..\docs\UUsersFrm.TUser.Active.txt)
    property Active: string read FActive write FActive;
  end;

var
  UsersFrm: TUsersFrm;

implementation

uses
  UUserMdl, UUsersGesFrm, UStyleMdl;

{$R *.fmx}

{ TUsersFrm }

procedure TUsersFrm.ListBoxItemClick(Sender: TObject);
var
  UsrGesFrm: TUsersGesFrm;
begin
  if not (Sender is TListBoxItem) then Exit;

  if not Assigned(TListBoxItem(Sender).TagObject) or not (TListBoxItem(Sender).TagObject is TUser) then Exit;

  if not UserMdl.PosUser(TUser(TListBoxItem(Sender).TagObject).Id) then Exit;

  UsrGesFrm := TUsersGesFrm.Create(Self);
  UsrGesFrm.SetFormList(FFrmList);
  UsrGesFrm.Name := UsrGesFrm.Name + FormatDateTime('hhnnssmm', Now);
  FFrmList.PushForm(UsrGesFrm);
end;

procedure TUsersFrm.LoadData;
const
  cDetail = 'Soci: %s     Actiu: %s    Correu: %s';
var
  lbItem: TListBoxItem;
begin
  UserMdl.GetUsersList('GetUsersList', nil);

  try
    lbUsers.BeginUpdate;
    lbUsers.Clear;
    while not UserMdl.cdsUsers.Eof do
    begin
      lbItem := TListBoxItem.Create(lbUsers);
      lbItem.TagObject := TUser.Create;
      TUser(lbItem.TagObject).Id := UserMdl.cdsUsers.FieldByName('id').AsString;
      TUser(lbItem.TagObject).User := UserMdl.cdsUsers.FieldByName('user').AsString;
      TUser(lbItem.TagObject).Mail := UserMdl.cdsUsers.FieldByName('mail').AsString;
      TUser(lbItem.TagObject).IdSoci := UserMdl.cdsUsers.FieldByName('id_soci').AsString;
      TUser(lbItem.TagObject).Active := UserMdl.cdsUsers.FieldByName('active').AsString;
      lbItem.Text := UserMdl.cdsUsers.FieldByName('user').AsString;
      lbItem.ItemData.Detail := Format(cDetail, [UserMdl.cdsUsers.FieldByName('id_soci').AsString,
                                                 UserMdl.cdsUsers.FieldByName('active').AsString,
                                                 UserMdl.cdsUsers.FieldByName('mail').AsString]);
      lbItem.ItemData.Accessory := TListBoxItemData.TAccessory(1);
      lbItem.StyleLookup := 'lbItemStyle';
      lbItem.OnClick := ListBoxItemClick;
      lbUsers.AddObject(lbItem);

      UserMdl.cdsUsers.Next;
    end;
  finally
    lbUsers.EndUpdate;
  end;
end;

end.
