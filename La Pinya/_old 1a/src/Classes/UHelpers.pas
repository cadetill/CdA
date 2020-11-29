unit UHelpers;

interface

uses
  FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Effects, System.Classes,
  FMX.Types, System.UITypes;

type
  TAnimation = class(TRectangle)
    FAniIndicator: TAniIndicator;
  private
    FShadow: TShadowEffect;
    FAnimate: Boolean;
    FText: TLabel;

    procedure SetAnimate(const Value: Boolean);
  public
    property Animate: Boolean read FAnimate write SetAnimate;
    property Text: TLabel read FText write FText;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TControlHelper = class helper for TControl
  public
    procedure Ani(pAnimate: Boolean; pMessage: string = 'Carregant...');
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

{ TAnimation }

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAniIndicator := TAniIndicator.Create(Self);
  FAniIndicator.Align := TAlignLayout.Client;
  FAniIndicator.StyleLookup := 'aniindicatorstyle';
  FAniIndicator.Parent := Self;

  FText := TLabel.Create(Self);
  FText.Align := TAlignLayout.Bottom;
  FText.Text := 'Carregant...';
  FText.TextAlign := TTextAlign.Center;
  FText.Parent := Self;

  Width := 150;
  Height := 100;
  Stroke.Thickness := 0;
  Fill.Color := 4292927712;

  Padding.Left := 5;
  Padding.Bottom := 5;
  Padding.Top := 5;
  Padding.Right := 5;

  XRadius := 10;
  YRadius := 10;

  FShadow := TShadowEffect.Create(Self);
  FShadow.Distance := 1;
  FShadow.Direction := 90;
  FShadow.Softness := 0.100000001490116100;
  FShadow.Opacity := 0.699999988079071100;
  FShadow.ShadowColor := TAlphaColorRec.Black;
  FShadow.Parent := Self;
  FShadow.Enabled := True;
end;

destructor TAnimation.Destroy;
begin
  FShadow.DisposeOf;
  FAniIndicator.DisposeOf;
  FText.DisposeOf;

  inherited;
end;

procedure TAnimation.SetAnimate(const Value: Boolean);
begin
  FAnimate := Value;
  FAniIndicator.Enabled := Value;
  FAniIndicator.Visible := Value;
  Self.BringToFront;
end;

end.
