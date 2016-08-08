unit FMX.Devgear.Extentions;

interface

uses
  System.Classes, FMX.Graphics, FMX.Forms;

type
  TBitmapHelper = class helper for TBitmap
  public
    procedure LoadFromURL(AURL, AFileName: string; Sender: TObject = nil); overload;
  end;

implementation

uses
  System.SysUtils, System.Types, System.iOUtils, IdHttp, AnonThread;

procedure TBitmapHelper.LoadFromURL(AURL, AFileName: string; Sender: TObject = nil);
var
  AThread: TAnonymousThread<TBytesStream>;
  // AFileName: string;
  ANotExistFile: boolean;
begin
  // AFileName := TPath.Combine(TPath.GetDocumentsPath, md5(AURL));
  ANotExistFile := not FileExists(AFileName);

  AThread := TAnonymousThread<TBytesStream>.Create(
    function: TBytesStream
    var
      AHTTP: TIdHTTP;
    begin
      Result := nil;
      if Sender = nil then
        exit;

      Result := TBytesStream.Create;
      if ANotExistFile then
      begin
        AHTTP := TIdHTTP.Create(nil);
        AHTTP.HandleRedirects := true;
      end;

      try
        try
          if ANotExistFile then
            AHTTP.Get(AURL, Result)
          else
            Result.LoadFromFile(AFileName);
        except
          FreeAndNil(Result);
        end;
      finally
        if ANotExistFile then
          FreeAndNil(AHTTP);
      end;
    end,

    procedure(AResult: TBytesStream)
    begin
      if (AResult = nil) or (Sender = nil) then
      begin
        FreeAndNil(AResult);
        exit;
      end;

      if (AResult.Size > 0) then
      begin
        if (Self <> nil) and (Sender <> nil) then
        begin
          AResult.Seek(0, 0);
          LoadFromStream(AResult);
        end;

        if not ANotExistFile then
        begin
          AResult.Seek(0, 0);
          AResult.SaveToFile(AFileName);
        end;
      end;
      FreeAndNil(AResult);
    end);
end;

end.
