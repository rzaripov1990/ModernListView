unit FMX.FireMonkey.Parser;

interface

uses
  System.Classes, System.Types, System.Threading, System.IOUtils, System.StrUtils, System.SysUtils,
  System.Generics.Collections, FMX.Types,
  IdHTTP, IdCompressorZLib, IdHashMessageDigest,
  FMX.ListView;

type
  TFMXMember = record
    Name: string;
    URL: string;
    FileName: string;
    constructor Create(aName, aURL: string);
  end;

  TFireMonkey = record
    class function GetURL: string; static;
    class procedure Request(aURL: string); static;
    class procedure MakeList; static;
    class procedure CachePictures(const aLV: TListView); static;
    class procedure Clean; static;
  end;

var
  FMembersList: TList<TFMXMember>;
  FStreamList: array of TMemoryStream;
  Pool: TThreadPool = nil;

function md5(const Value: string): string;

implementation

var
  FContent: string = '';

function Parse(const tag1, tag2, source: string): string;
var
  p, p2: Integer;
begin
  Result := '';
  p := PosEx(tag1, source);
  p2 := PosEx(tag2, source, p + tag1.Length + 1);
  if (p = 0) or (p2 = 0) then
    Exit;
  if p2 > p then
    Result := (Copy(source, p + tag1.Length, p2 - p - tag1.Length));
end;

function md5(const Value: string): string;
begin
  with TIdHashMessageDigest5.Create do
  begin
    Result := AnsiLowerCase(HashStringAsHex(Value)) + '.jpg';
    Free;
  end;
end;

{ TFMXSiteParser }

class procedure TFireMonkey.MakeList;
var
  aName, aURL, aUser: string;
  I: Integer;
begin
  System.Delete(FContent, 1, pos('ipsList_reset cStream_members', FContent)-1);

  I := 1;
  while I < 21 do
  begin
    aUser := Parse('<img src=', '>', FContent);
    if not aUser.IsEmpty then
    begin
      aURL := Copy(aUser, 1, PosEx(' alt=', aUser)-1).DeQuotedString;
      aURL := StringReplace(aURL, 'https:', 'http:', []);
      aName := Copy(aUser, PosEx('alt=', aUser)+4).DeQuotedString;

      System.Delete(FContent, 1, pos(aUser, FContent) + aUser.Length + 1);

      FMembersList.Add(TFMXMember.Create(aName, aURL));
      // SetLength(FStreamList, Length(FStreamList) + 1);
      // FStreamList[Length(FStreamList) - 1] := TMemoryStream.Create;
    end;
    Inc(I);
  end;
end;

class procedure TFireMonkey.Request(aURL: string);
var
  Decompress: TIdCompressorZLib;
  Content: TMemoryStream;
  AStream: TStringStream;
  aPath: string;
begin
  aPath := TPath.Combine(TPath.GetDocumentsPath, 'content.txt');

  if not FileExists(aPath) then
  begin
    Decompress := TIdCompressorZLib.Create(nil);
    with TIdHTTP.Create(nil) do
    begin
      Compressor := Decompress;

      AllowCookies := true;
      with Request do
      begin
        Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
        AcceptLanguage := 'ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3';
        AcceptCharSet := 'text/html;charset=UTF-8';
        // AcceptEncoding := 'gzip, deflate';
        // UserAgent := 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:47.0) Gecko/20100101 Firefox/47.0';
        Host := 'fire-monkey.ru';
      end;

      Content := TMemoryStream.Create;
      try
        Get(aURL, Content);
        Content.SaveToFile(aPath);
      finally
        Content.Free;
      end;

      Free;
    end;
    Decompress.Free;
  end;

  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AStream.LoadFromFile(aPath);
    FContent := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

class procedure TFireMonkey.CachePictures(const aLV: TListView);
begin
  Pool.SetMaxWorkerThreads(10); // TThreadPool.Default.MaxWorkerThreads

  TParallel.For(0, FMembersList.Count - 1,
    procedure(Idx: Integer)
    var
      AHTTP: TIdHTTP;
    begin
      if not FileExists(FMembersList[Idx].FileName) then
      begin
        AHTTP := TIdHTTP.Create(nil);
        try
          AHTTP.HandleRedirects := true;
          FStreamList[Idx].Clear;
          AHTTP.Get(FMembersList[Idx].URL, FStreamList[Idx]);
        finally
          FreeAndNil(AHTTP);
        end;

        TThread.Queue(TThread.CurrentThread,
          procedure
          var
            AItem: Integer;
          begin
            if FStreamList[Idx].Size > 0 then
            begin
              FStreamList[Idx].SaveToFile(FMembersList[Idx].FileName);
              AItem := Trunc(Idx / aLV.Columns);
              if AItem < aLV.Items.Count then
                aLV.Adapter.ResetView(aLV.Items[AItem]);
            end;
            Log.d('Current Thread = ' + inttostr(Idx));
          end);
      end;
    end, Pool);
end;

{ class procedure TFireMonkey.CachePictures;
  begin
  FThreadMain := TTask.Run(
  procedure
  var
  I: Integer;
  AHTTP: TIdHTTP;
  begin
  FThreadWork := true;
  for I := 0 to FMembersList.Count - 1 do
  begin
  AHTTP := TIdHTTP.Create(nil);
  try
  AHTTP.HandleRedirects := true;
  AHTTP.Get(FMembersList[I].URL, FStreamList[I]);
  finally
  FreeAndNil(AHTTP);
  end;
  Log.d('current thread = ' + inttostr(I));
  end;

  TThread.Synchronize(TThread.CurrentThread,
  procedure
  var
  I: Integer;
  begin
  for I := 0 to FMembersList.Count - 1 do
  begin
  if FStreamList[I].Size > 0 then
  FStreamList[I].SaveToFile(FMembersList[I].FileName);
  end;
  end);
  FThreadWork := false;
  end)
  end; }

class procedure TFireMonkey.Clean;
var
  I: Integer;
begin
  for I := Low(FStreamList) to High(FStreamList) do
    FStreamList[I].Free;
  SetLength(FStreamList, 0);
end;

class function TFireMonkey.GetURL: string;
begin
  Result := 'http://fire-monkey.ru/search/?type=core_members&group%5B4%5D=1&group%5B6%5D=1&group%5B3%5D=1&sortby=pp_reputation_points&sortdirection=desc&page=1';
end;

{ TFMXMembers }

constructor TFMXMember.Create(aName, aURL: string);
begin
  Self.Name := aName;
  Self.URL := aURL;
  Self.FileName := TPath.Combine(TPath.GetDocumentsPath, md5(aURL));
end;

initialization

FMembersList := TList<TFMXMember>.Create;
Pool := TThreadPool.Create;

finalization

FMembersList.Free;
Pool.Free;

end.

