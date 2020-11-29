unit NetworkState.Windows;

interface

uses
  NetworkState;

type
  TPlatformNetworkState = class(TCustomNetworkState)
  public
    function CurrentSSID: string; override;
    function IsConnected: Boolean; override;
    function IsWifiConnected: Boolean; override;
    function IsMobileConnected: Boolean; override;
  end;

implementation

uses
  System.SysUtils, System.Net.HttpClientComponent, System.Net.HttpClient;

{ TPlatformNetworkState }

function TPlatformNetworkState.CurrentSSID: string;
begin
  Result := '';
end;

function TPlatformNetworkState.IsConnected: Boolean;
var
  Client: TNetHTTPClient;
  Request: TNetHTTPRequest;
  Resp: IHTTPResponse;
begin
  Client := nil;
  Request := nil;
  try
    Client := TNetHTTPClient.Create(nil);
    Request := TNetHTTPRequest.Create(nil);

    Request.Client := Client;
    Request.URL := 'https://www.google.com';
    Request.MethodString := 'GET';
    try
      Resp := Request.Execute;
      Result := Resp.StatusCode = 200;
    except
      Result := False;  // si hi ha excepció, no hi ha connexió
    end;
  finally
    FreeAndNil(Client);
    FreeAndNil(Request);
  end;
end;

function TPlatformNetworkState.IsMobileConnected: Boolean;
begin
  Result := False;
end;

function TPlatformNetworkState.IsWifiConnected: Boolean;
begin
  Result := True;
end;

end.
