unit UNewsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UBaseModalFrm, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.DateTimeCtrls,
  UInterfaces;

const
  cCaption = 'Gestió Notícies';
  cErrIncompletData = 'Falten dades per introduir';
  cErrSavingInfo = 'Error actualitzant dades en el servidor.';

type
  TNewsFrm = class(TBaseModalFrm, IChildren)
    sbContent: TScrollBox;
    lTitre: TLabel;
    eTitre: TEdit;
    eDate: TDateEdit;
    lContent: TLabel;
    mContent: TMemo;
    lDate: TLabel;
    lTime: TLabel;
    eTime: TTimeEdit;
  private
  public
    constructor Create(AOwner: TComponent); override;

    function SetCaption: string;
    function DeactivateAll: Boolean;
    function AcceptForm: Boolean;
    function EnabledBackButton: Boolean;
    function DefaultStateAcceptButton: Boolean;
    procedure AfterShow;
  end;

var
  NewsFrm: TNewsFrm;

implementation

uses
  UMessage, UClasses, UHomeMdl;

{$R *.fmx}

{ TNewsFrm }

function TNewsFrm.AcceptForm: Boolean;
var
  Intf: IMainMenu;
  User: TUser;
  Error: Boolean;
begin
  Result := False;

  if (eTitre.Text = '') or (mContent.Text = '') then
  begin
    TMessage.Show(cErrIncompletData);
    Exit;
  end;

  if not Assigned(TagObject) then
    TagObject := TItemNews.Create(nil);
  if not (TagObject is TItemNews) then
  begin
    TagObject.Free;
    TagObject := TItemNews.Create(nil);
  end;

  if Supports(Owner, IMainMenu, Intf) then
    User := Intf.GetInfoUser;

  // asignem valors a l'objecte TagObject
  TItemNews(TagObject).Titol := eTitre.Text;
  TItemNews(TagObject).Contingut := mContent.Text;
  TItemNews(TagObject).Publicar := FormatDateTime('yyyy-mm-dd', eDate.Date);
  TItemNews(TagObject).Hora := eTime.Text;

  // crear/modificar info en BASE DE DADES
  if TItemNews(TagObject).IndexLB = -1 then // insert
  begin
    TItemNews(TagObject).Id := THomeMdl.AddNews(TItemNews(TagObject).Titol,
                                                TItemNews(TagObject).Contingut,
                                                TItemNews(TagObject).Publicar,
                                                TItemNews(TagObject).Hora,
                                                User.Id.ToString);
    Error := TItemNews(TagObject).Id = '';
  end
  else // edit
    Error := not THomeMdl.EditNews(TItemNews(TagObject).Id,
                                   TItemNews(TagObject).Titol,
                                   TItemNews(TagObject).Contingut,
                                   TItemNews(TagObject).Publicar,
                                   TItemNews(TagObject).Hora,
                                   User.Id.ToString);

  if Error then
    TMessage.Show(cErrSavingInfo)
  else
  begin
    if Assigned(TItemNews(TagObject).OnChange) then
      TItemNews(TagObject).OnChange(TItemNews(TagObject));
    Result := True;
  end;
end;

procedure TNewsFrm.AfterShow;
begin
  if not Assigned(TagObject) then Exit;
  if not (TagObject is TItemNews) then Exit;
  if TItemNews(TagObject).IndexLB = -1 then Exit;

  eTitre.Text := TItemNews(TagObject).Titol;
  mContent.Text := TItemNews(TagObject).Contingut;
  eDate.Text := TItemNews(TagObject).PubDate;
  eDate.Enabled := eDate.Date > Date;
  eTime.Text := TItemNews(TagObject).Hora;
  eTime.Enabled := (eTime.Time > Time) and (eDate.Date >= Date);
end;

constructor TNewsFrm.Create(AOwner: TComponent);
begin
  inherited;

  eDate.Date := Date;
  eTime.Time := Time;
end;

function TNewsFrm.DeactivateAll: Boolean;
begin
  Result := True;
end;

function TNewsFrm.DefaultStateAcceptButton: Boolean;
begin
  Result := True;
end;

function TNewsFrm.EnabledBackButton: Boolean;
begin
  Result := False;
end;

function TNewsFrm.SetCaption: string;
begin
  Result := cCaption;
end;

end.
