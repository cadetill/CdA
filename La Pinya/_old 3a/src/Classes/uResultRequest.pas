unit uResultRequest;

interface

uses
  uBaseJson;

type
  TResultRequest = class(TBaseJson)
  private
    FAffected: string;
    FId: string;
    FStatus: string;
    FConnected: string;
    FError: string;
    FJson: string;
  public
    procedure Clear;

    property affected: string read FAffected write FAffected;
    property id: string read FId write FId;
    property status: string read FStatus write FStatus;
    property connected: string read FConnected write FConnected;
    property Error: string read FError write FError;
    property Json: string read FJson write FJson;
  end;

implementation

uses
  Rest.Json;

{ TResultRequest }

procedure TResultRequest.Clear;
begin
  FAffected := '';
  FId := '';
  FStatus := '';
  FConnected := '';
  FError := '';
  FJson := '';
end;

end.
