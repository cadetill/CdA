{
  @abstract(unit to manage result of an update, insert or delete statement)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 10, 2020)
  @lastmod(October 10, 2020)

  The uResultRequest unit contains the definition and implementation of @link(TResultRequest) class. This class manage request of an update, insert or delete statement.

  Change List @br
  @unorderedList(
    @item(10/10/2020 : first version)
  )
}
unit uResultRequest;

interface

uses
  Pkg.Json.DTO;

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uResultRequest.TResultRequest.txt)
  TResultRequest = class(TJsonDTO)
  private
    FDetail01: string;
    FId: string;
    FMaster: string;
  public
    // @include(..\..\docs\help\uResultRequest.TResultRequest.Detail01.txt)
    property Detail01: string read FDetail01 write FDetail01;
    // @include(..\..\docs\help\uResultRequest.TResultRequest.Id.txt)
    property Id: string read FId write FId;
    // @include(..\..\docs\help\uResultRequest.TResultRequest.Master.txt)
    property Master: string read FMaster write FMaster;
  end;

implementation

end.
