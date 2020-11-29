// @include(..\..\docs\USelectQueryMdl.txt)
unit USelectQueryMdl;

interface

uses
  System.SysUtils, System.Classes, UBaseQueryMdl, Data.DB, Datasnap.DBClient,
  FMX.StdCtrls;

type
  // @include(..\..\docs\USelectQueryMdl.TSelectQueryMdl.txt)
  TSelectQueryMdl = class(TBaseQueryMdl)
  private
  public
  end;

var
  SelectQueryMdl: TSelectQueryMdl;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
