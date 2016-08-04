unit FMX.BitmapHelper;

interface

uses
  System.Classes, FMX.Graphics;

type
  TBitmapHelper = class helper for TBitmap
  public
    procedure LoadFromUrl(AURL: string);
  end;

implementation

uses
  System.SysUtils, System.Types,
  IdHttp, AnonThread;

procedure TBitmapHelper.LoadFromUrl(AURL: string);
var
  ATask: TAnonymousThread<TMemoryStream>;
begin
  ATask := TAnonymousThread<TMemoryStream>.Create(
    function: TMemoryStream
    var
      AHTTP: TIdHttp;
    begin
      Result := TMemoryStream.Create;
      AHTTP := TIdHttp.Create(nil);
      try
        try
          AHTTP.Get(AURL, Result);
        except
          FreeAndNil(Result);
        end;
      finally
        FreeAndNil(AHTTP);
      end;
    end,
    procedure(AResult: TMemoryStream)
    begin
      if not Assigned(AResult) then
        exit;

      if AResult.Size > 0 then
        LoadFromStream(AResult);
      FreeAndNil(AResult);
    end,
    procedure(AException: Exception)
    begin
    end);
end;

end.
