unit UHelpers;

interface

uses
  FMX.Controls, FMX.ListBox, FMX.Types, FMX.Objects,
  UClasses;

type
  TControlHelper = class helper for TControl
  public
    procedure Ani(pAnimate: Boolean; pMessage: string = 'Carregant...');
  end;

  TListBoxItem = class(FMX.ListBox.TListBoxItem)
  private
    FItemCal: TItemCal;
    FItemNews: TItemNews;
  public
    destructor Destroy; override;

    property ItemCal: TItemCal read FItemCal write FItemCal;
    property ItemNews: TItemNews read FItemNews write FItemNews;
  end;

  TImage = class(FMX.Objects.TImage)
  private
    FLBItem: TListBoxItem;
  public
    property LBItem: TListBoxItem read FLBItem write FLBItem;
  end;

implementation

{ TControlHelper }

procedure TControlHelper.Ani(pAnimate: Boolean; pMessage: string);
var
  vAnimacion: TAnimation;
begin
  vAnimacion := nil;
  if Assigned(TagObject) then
  begin
    if TagObject is TAnimation then
      vAnimacion := TAnimation(TagObject)
  end
  else
  begin
    vAnimacion := TAnimation.Create(Self);
    vAnimacion.Align := TAlignLayout.Center;
    vAnimacion.Parent := Self;
    vAnimacion.BringToFront;
  end;

  if Assigned(vAnimacion) then
  begin
    vAnimacion.Text.Text := pMessage;
    vAnimacion.Visible := pAnimate;
    vAnimacion.Animate := pAnimate;

    if not pAnimate then
    begin
      TagObject.DisposeOf;
      TagObject := nil;
    end
    else
      TagObject := vAnimacion;
  end;
end;

{ TListBoxItem }

destructor TListBoxItem.Destroy;
begin
  if Assigned(FItemCal) then
    FItemCal.Free;

  inherited;
end;

end.
