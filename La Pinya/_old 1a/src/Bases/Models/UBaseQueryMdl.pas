// @include(..\..\..\docs\UBaseQueryMdl.txt)
unit UBaseQueryMdl;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, FMX.StdCtrls;

type
  // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.txt)
  TBaseQueryMdl = class(TDataModule)
    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.cdsResult.txt)
    cdsResult: TClientDataSet;
  private
    FCtrlEnd: Boolean;
    FWaitTime: Integer;
  protected
    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.OnThreadEnd.txt)
    procedure OnThreadEnd(Sender: TObject); deprecated;
    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.WaitUntilExit.txt)
    procedure WaitUntilExit; deprecated;
    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.CallServer_1.txt)
    procedure CallServer(Params: string; DataSet: TDataSet; OnTerminateEvent: TNotifyEvent; AniInd: TAniIndicator); overload; deprecated;
    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.CallServer_2.txt)
    procedure CallServer(Params: string; DataSet: TDataSet; AniInd: TAniIndicator); overload;

    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.CtrlEnd.txt)
    property CtrlEnd: Boolean read FCtrlEnd write FCtrlEnd;
  public
    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.Create.txt)
    constructor Create(AOwner: TComponent); override;

    // @include(..\..\..\docs\UBaseQueryMdl.TBaseQueryMdl.WaitTime.txt)
    property WaitTime: Integer read FWaitTime write FWaitTime;
  end;

var
  BaseQueryMdl: TBaseQueryMdl;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.DateUtils,
  {URESTDataMdl, }URESTMdl;

{ TBaseQueryMdl }

procedure TBaseQueryMdl.CallServer(Params: string; DataSet: TDataSet;
  OnTerminateEvent: TNotifyEvent; AniInd: TAniIndicator);
begin
  CallServer(Params, DataSet, AniInd);
  //TRESTDataMdl.GetRESTResponse(Params, DataSet, OnTerminateEvent, AniInd, nil);
end;

procedure TBaseQueryMdl.CallServer(Params: string; DataSet: TDataSet;
  AniInd: TAniIndicator);
begin
  TRESTMdl.GetRESTResponse(Params, DataSet, AniInd, nil);
end;

constructor TBaseQueryMdl.Create(AOwner: TComponent);
begin
  inherited;

  FWaitTime := 5;
  FCtrlEnd := False;
end;

procedure TBaseQueryMdl.OnThreadEnd(Sender: TObject);
begin
  FCtrlEnd := True;
end;

procedure TBaseQueryMdl.WaitUntilExit;
var
  TEnd: TTime;
begin
  TEnd := IncSecond(Time, FWaitTime);
  repeat
    Sleep(1);
  until FCtrlEnd or (Time > TEnd);
end;

end.
