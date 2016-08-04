unit FMX.FireMonkey.Parser;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, System.StrUtils,
  System.Generics.Collections,
  IdHTTP, IdCompressorZLib,
  FMX.ListView;

type
  TFMXMember = record
    Name: string;
    URL: string;
    constructor Create(aName, aURL: string);
  end;

  TFireMonkey = record
    class function GetURL: string; static;
    class procedure Request(aURL: string); static;
    class procedure MakeList; static;
  end;

var
  FMembersList: TList<TFMXMember>;

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

{ TFMXSiteParser }

class procedure TFireMonkey.MakeList;
var
  aDel, aName, aURL: string;
  I: Integer;
begin
  System.Delete(FContent, 1, pos('<div class="ipsAreaBackground_light ipsPad">', FContent));

  I := 1;
  while I < 26 do
  begin
    aName := Parse(' alt=', ' itemprop="image">', FContent);
    if not aName.IsEmpty then
    begin
      aURL := Parse('<img src=', ' alt=' + aName, FContent).DeQuotedString;
      aURL := StringReplace(aURL, 'https:', 'http:', []);
      aName := aName.DeQuotedString;

      aDel := '<img src=' + Parse('<img src=', 'itemprop="image">', FContent) + 'itemprop="image">';
      System.Delete(FContent, 1, pos(aDel, FContent) + aDel.Length + 1);

      FMembersList.Add(TFMXMember.Create(aName, aURL));
    end;
    Inc(I);
  end;
end;

class procedure TFireMonkey.Request(aURL: string);
var
  Decompress: TIdCompressorZLib;
  Content: TBytesStream;
  AStream: TStringStream;
  aPath: string;
begin
  aPath := TPath.Combine(TPath.GetDocumentsPath, 'content.txt');

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

    Content := TBytesStream.Create;
    try
      Get(aURL, Content);
      Content.SaveToFile(aPath);

      AStream := TStringStream.Create('', TEncoding.UTF8);
      try
        AStream.LoadFromFile(aPath);
        FContent := AStream.DataString;
      finally
        AStream.Free;
      end;

    finally
      Content.Free;
    end;

    Free;
  end;
  Decompress.Free;
end;

class function TFireMonkey.GetURL: string;
begin
  Result := 'http://fire-monkey.ru/search/?type=core_members&group%5B4%5D=1&group%5B6%5D=1&group%5B3%5D=1';
  Result := Result + '&sortby=pp_reputation_points&sortdirection=desc&page=1';
end;

{ TFMXMembers }

constructor TFMXMember.Create(aName, aURL: string);
begin
  Self.Name := aName;
  Self.URL := aURL;
end;

initialization

FMembersList := TList<TFMXMember>.Create;

finalization

FMembersList.Free;

end.
