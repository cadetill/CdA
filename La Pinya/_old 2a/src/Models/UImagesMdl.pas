unit UImagesMdl;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList;

type
  TImagesMdl = class(TDataModule)
    ilImages: TImageList;
  private
  public
  end;

var
  ImagesMdl: TImagesMdl;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
