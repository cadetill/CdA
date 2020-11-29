unit uEventSoci;

interface

uses
  uSocis, uEvents;

type
  TEventSoci = class
  private
    FSoci: TSoci;
    FEven: TEvent;
  public
    constructor Create(pEvent: TEvent; pSoci: TSoci); virtual;

    property Event: TEvent read FEven;
    property Soci: TSoci read FSoci;
  end;

implementation

{ TEventSoci }

constructor TEventSoci.Create(pEvent: TEvent; pSoci: TSoci);
begin
  FEven := pEvent;
  FSoci := pSoci;
end;

end.
