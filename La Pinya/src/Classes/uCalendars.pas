{
  @abstract(Unit to manage calendars)
  @author(Xavier Martinez (cadetill) <cadetill@gmail.com>)
  @created(October 06, 2020)
  @lastmod(October 06, 2020)

  The uCalendars unit contains the definition and implementation of @link(TCalendars) and @link(TCalendar) classes. This classes manage the calendars of the colla castellera.

  @bold(Change List) @br
  @unorderedList(
    @item(06/10/2020 : first version)
  )
}
unit uCalendars;

interface

uses
  System.Classes, System.Generics.Collections, REST.Json.Types,
  Pkg.Json.DTO, uResultRequest;

{$M+}

type
  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uCalendars.TCalendar.txt)
  TCalendar = class
  private
    FId: string;
    FIdcalendar: string;
    FKey: string;
    FNom: string;
    FNom_Curt: string;
    FOnChange: TNotifyEvent;
  public
    // @include(..\..\docs\help\uCalendars.TCalendar.Id.txt)
    property Id: string read FId write FId;
    // @include(..\..\docs\help\uCalendars.TCalendar.Idcalendar.txt)
    property Idcalendar: string read FIdcalendar write FIdcalendar;
    // @include(..\..\docs\help\uCalendars.TCalendar.Key.txt)
    property Key: string read FKey write FKey;
    // @include(..\..\docs\help\uCalendars.TCalendar.Nom.txt)
    property Nom: string read FNom write FNom;
    // @include(..\..\docs\help\uCalendars.TCalendar.Nom_Curt.txt)
    property Nom_Curt: string read FNom_Curt write FNom_Curt;

    // @include(..\..\docs\help\uCalendars.TCalendar.OnChange.txt)
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { -------------------------------------------------------------------------- }
  // @include(..\..\docs\help\uCalendars.TCalendars.txt)
  TCalendars = class(TJsonDTO)
  private
    [JSONName('Items')]
    FItemsArray: TArray<TCalendar>;
    [GenericListReflect]
    FItems: TObjectList<TCalendar>;

    function GetItems: TObjectList<TCalendar>;
  public
    // @include(..\..\docs\help\uCalendars.TCalendars.Destroy.txt)
    destructor Destroy; override;

    // @include(..\..\docs\help\uCalendars.TCalendars.IndexOf.txt)
    function IndexOf(Id: string): Integer;

    // @include(..\..\docs\help\uCalendars.TCalendars.GetCalendar.txt)
    class function GetCalendar: TCalendars;
    (* esto es una prueba*)
    class function EditCalendar(const Item: TCalendar): TResultRequest;
    //class function AddCalendar(const IdCalendari, Key: string): TResultRequest;
    //class function DelCalendar(const Id: string): TResultRequest;

    // @include(..\..\docs\help\uCalendars.TCalendars.Items.txt)
    property Items: TObjectList<TCalendar> read GetItems;
  end;

implementation

uses
  System.JSON, System.SysUtils;

{ TCalendars }

destructor TCalendars.Destroy;
begin
  GetItems.DisposeOf;

  inherited;
end;

class function TCalendars.EditCalendar(const Item: TCalendar): TResultRequest;
var
  Json: string;
  Obj: TJSONObject;
begin
  Result := TResultRequest.Create;
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('editCalendar'));
    Obj.AddPair('id', TJSONString.Create(Item.id));
    Obj.AddPair('idcalendar', TJSONString.Create(Item.idcalendar));
    Obj.AddPair('key', TJSONString.Create(Item.key));
    Obj.AddPair('nom', TJSONString.Create(Item.nom));
    Obj.AddPair('nom_curt', TJSONString.Create(Item.nom_curt));

    Json := Result.GetJson(Obj);
    Result.AsJson := Json;
  finally
    Obj.DisposeOf;
  end;
end;

class function TCalendars.GetCalendar: TCalendars;
var
  Json: string;
  Obj: TJSONObject;
begin
  Result := TCalendars.Create;
  Obj := TJSONObject.Create;
  try
    Obj.AddPair('func', TJSONString.Create('getCalendars'));

    Json := Result.GetJson(Obj);
    Result.AsJson := Json;
  finally
    Obj.DisposeOf;
  end;
end;

function TCalendars.GetItems: TObjectList<TCalendar>;
begin
  if not Assigned(FItems) then
  begin
    FItems := TObjectList<TCalendar>.Create;
    FItems.AddRange(FItemsArray);
  end;
  Result := FItems;
end;

function TCalendars.IndexOf(Id: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FItems.Count - 1 do
    if SameText(FItems[i].Id, Id) then
    begin
      Result := i;
      Break;
    end;
end;

end.
