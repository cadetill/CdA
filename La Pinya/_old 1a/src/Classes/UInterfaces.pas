unit UInterfaces;

interface

uses
  UFormList;

type
  ILoadData = interface
  ['{07BF55B5-0C54-44BB-9591-9E708C104794}']
    procedure LoadData;
  end;

  IFormList = interface
  ['{5E21E0C7-A99E-4625-B920-529C1F57010F}']
    procedure SetFormList(FrmList: TFormList);
  end;

implementation

end.
