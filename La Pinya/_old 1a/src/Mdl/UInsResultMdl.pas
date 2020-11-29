// @include(..\..\docs\UInsResultMdl.txt)
unit UInsResultMdl;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, FMX.StdCtrls,
  UBaseQueryMdl;

type
  // @include(..\..\docs\UInsResultMdl.TInsResultMdl.txt)
  TInsResultMdl = class(TBaseQueryMdl)
    // @include(..\..\docs\UInsResultMdl.TInsResultMdl.cdsResultstatus.txt)
    cdsResultstatus: TStringField;
    // @include(..\..\docs\UInsResultMdl.TInsResultMdl.cdsResultid.txt)
    cdsResultid: TStringField;
  private
  protected
    // @include(..\..\docs\UInsResultMdl.TInsResultMdl.GetNewId.txt)
    function GetNewId: Integer;
  public
    // @include(..\..\docs\UInsResultMdl.TInsResultMdl.InsertNewRecord.txt)
    class function InsertNewRecord(FuncName, Params: string; AniInd: TAniIndicator): Integer;
  end;

var
  InsResultMdl: TInsResultMdl;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TInsResultMdl }

function TInsResultMdl.GetNewId: Integer;
begin
  if cdsResult.RecordCount = 0 then Result := -1
  else
  begin
    if not TryStrToInt(cdsResultid.AsString, Result) then
      Result := -1;
  end;
end;

class function TInsResultMdl.InsertNewRecord(FuncName, Params: string;
  AniInd: TAniIndicator): Integer;
const
  cData = 'func=%s%s';
var
  Dm: TInsResultMdl;
  TmpStr: string;
begin
  TmpStr := Format(cData, [FuncName, Params]);
  Dm := TInsResultMdl.Create(nil);
  try
    Dm.cdsResult.CreateDataSet;
    Dm.CallServer(TmpStr, Dm.cdsResult, AniInd);
    Result := Dm.GetNewId;
  finally
    FreeAndNil(Dm);
  end;
end;

end.
