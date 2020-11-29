unit uGoogleCal;

interface

uses
  uBaseJson, uResultRequest;

type
  TCtrlDate = class(TBaseJson)
  private
    FDateTime: string;
    FTimeZone: string;
    FDate: string;
  public
    property dateTime: string read FDateTime write FDateTime;
    property date: string read FDate write FDate;
    property timeZone: string read FTimeZone write FTimeZone;
  end;

  TGoogleItem = class(TBaseJson)
  private
    FCreated: string;
    FEnd: TCtrlDate;
    FEtag: string;
    FHtmlLink: string;
    FICalUID: string;
    FId: string;
    FKind: string;
    FLocation: string;
    FRecurrence: TArray<string>;
    FSequence: Extended;
    FStart: TCtrlDate;
    FStatus: string;
    FSummary: string;
    FUpdated: string;
    FDescription: String;
    FRecurringEventId: string;
    FOriginalStartTime: TCtrlDate;
  public
    constructor Create;
    destructor Destroy; override;

    property created: string read FCreated write FCreated;
    property description: string read FDescription write FDescription;
    [JsonName('end')]
    property EndDate: TCtrlDate read FEnd write FEnd;
    property etag: string read FEtag write FEtag;
    property htmlLink: string read FHtmlLink write FHtmlLink;
    property iCalUID: string read FICalUID write FICalUID;
    property id: string read FId write FId;
    property kind: string read FKind write FKind;
    property location: string read FLocation write FLocation;
    property recurrence: TArray<string> read FRecurrence write FRecurrence;
    property sequence: Extended read FSequence write FSequence;
    property start: TCtrlDate read FStart write FStart;
    property status: string read FStatus write FStatus;
    property summary: string read FSummary write FSummary;
    property recurringEventId: string read FRecurringEventId write FRecurringEventId;
    property updated: string read FUpdated write FUpdated;
    property originalStartTime: TCtrlDate read FOriginalStartTime write FOriginalStartTime;
  end;

  TGoogleCal = class(TBaseJson)
  private
    FAccessRole: string;
    FDescription: string;
    FEtag: string;
    FItems: TArray<TGoogleItem>;
    FKind: string;
    FNextSyncToken: string;
    FSummary: string;
    FTimeZone: string;
    FUpdated: string;

    function GetCount: Integer;
  public
    destructor Destroy; override;

    class function GetGoogleCal(var Res: TResultRequest; const IdCal, Key, Any: string): TGoogleCal;

    property accessRole: string read FAccessRole write FAccessRole;
    property description: string read FDescription write FDescription;
    property etag: string read FEtag write FEtag;
    property items: TArray<TGoogleItem> read FItems write FItems;
    property kind: string read FKind write FKind;
    property nextSyncToken: string read FNextSyncToken write FNextSyncToken;
    property summary: string read FSummary write FSummary;
    property timeZone: string read FTimeZone write FTimeZone;
    property updated: string read FUpdated write FUpdated;
    property Count: Integer read GetCount;
  end;

implementation

uses
  System.SysUtils,
  uRESTMdl;

{ TGoogleCal }

destructor TGoogleCal.Destroy;
var
  Item: TGoogleItem;
begin
  for Item in FItems do
    Item.Free;

  inherited;
end;

function TGoogleCal.GetCount: Integer;
begin
  Result := High(FItems);
end;

class function TGoogleCal.GetGoogleCal(var Res: TResultRequest; const IdCal, Key,
  Any: string): TGoogleCal;
begin
  Result := nil;
  Res := TRESTMdl.GetData(tcGetGoogleCal, Format(cUrlGoogleCal, [IdCal, Key, Any, Any]));
  if Res.Json <> '' then // si tenim dades, creem objecte de sortida
    Result := TGoogleCal.FromJsonString<TGoogleCal>(Res.Json);
end;

{ TGoogleItem }

constructor TGoogleItem.Create;
begin
  inherited;

  FStart := TCtrlDate.Create;
  FEnd := TCtrlDate.Create;
  FOriginalStartTime := TCtrlDate.Create;
end;

destructor TGoogleItem.Destroy;
begin
  if Assigned(FStart) then
    FStart.Free;
  if Assigned(FEnd) then
    FEnd.Free;
  if Assigned(FOriginalStartTime) then
    FOriginalStartTime.Free;

  inherited;
end;

end.
