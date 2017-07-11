unit uForm3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView;

type
  TForm3 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    ListView1: TListView;
    AniIndicator1: TAniIndicator;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
    procedure ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure ReLoadLV;

    procedure OnColumnClick(const Sender: TObject; const Column: Integer; const X, Y: Single;
      const AItem: TListViewItem; const DrawebleName: string);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses
  System.Threading, System.Math, System.IOUtils, System.Net.HTTPClient,
  FMX.FireMonkey.Parser,
  uForm2;

const
  colTitle = 'title';
  colBitmap = 'bitmap';
  colLogo = 'logo';
  colLoading = 'loading';
  ColumnStartIndex = 1;

function getRealIndex(const Row, Column, Columns: Integer): Integer;
begin
  Result := (((Row + 1) * Columns) - 1) - (Columns - Column);
end;

procedure LoadBitmapFromURL(const aURL: string; aBitmap: TBitmap);
// System.Net.HTTPClient
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      Http: THTTPClient;
      Result: TMemoryStream;
    begin
      Result := TMemoryStream.Create;
      Http := THTTPClient.Create;
      try
        try
          Http.HandleRedirects := true;
          Http.Get(aURL, Result);
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            var
              aSourceBmp: TBitmap;
            begin
              aSourceBmp := TBitmap.Create;
              aSourceBmp.LoadFromStream(Result);
              if not aSourceBmp.IsEmpty then
              begin
                aBitmap.Clear(TAlphaColorRec.White);
                aBitmap.SetSize(aSourceBmp.Width, aSourceBmp.Height);
                aBitmap.CopyFromBitmap(aSourceBmp);
              end;
              FreeAndNil(aSourceBmp);
            end);
        except
          FreeAndNil(Result);
        end;
      finally
        FreeAndNil(Result);
        FreeAndNil(Http);
      end;
    end);
  thread.FreeOnTerminate := true;
  thread.start;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TFireMonkey.Clean;
end;

procedure TForm3.FormResize(Sender: TObject);
begin
  if Tag = 0 then
    ReLoadLV;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  ListView1.ColumnWidth := 160;
  ListView1.AutoColumns := true;
  ListView1.ItemAppearance.ItemHeight := 180;
  ListView1.StyleLookup := 'listviewstyle_panel';
  ListView1.OnColumnClick := OnColumnClick;
  ListView1.ShowScrollBar := false;
  ListView1.EnableTouchAnimation(false);

  TTask.Run(
    procedure
    begin
      TFireMonkey.Request(TFireMonkey.GetURL);
      TFireMonkey.MakeList;

      TThread.Synchronize(nil,
        procedure
        begin
          Tag := 0;
          ReLoadLV;
        end);
    end);
end;

procedure TForm3.ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  I, J, RowColumns: Integer;
  iBitmap: TListItemImage;
  aFirst, aLast: Integer;
begin
  if ListView1.Items.Count <= 0 then
    exit;

  aFirst := Max(0, ListView1.getFirstVisibleItemIndex);
  aLast := aFirst + ListView1.getVisibleCount;

  for I := aFirst to aLast do
  begin
    if InRange(I, 0, ListView1.Items.Count - 1) then
    begin
      Label1.Text := string.Join(':', [I, ListView1.getFirstVisibleItemIndex, ListView1.getVisibleCount,
        ListView1.getLastVisibleItemindex]);

      RowColumns := ListView1.Items[I].Tag;
      for J := 1 to RowColumns do
      begin
        iBitmap := ListView1.Items[I].Objects.FindObjectT<TListItemImage>(colBitmap + IntToStr(J));

        if Assigned(iBitmap) then
        begin
          if Assigned(iBitmap.Bitmap) and (ListView1.Items[I].Data[colLoading + IntToStr(J)].AsInteger = 1) then
          begin
            ListView1.Items[I].Data[colLoading + IntToStr(J)] := 0;
            LoadBitmapFromURL(ListView1.Items[I].Data[colLogo + IntToStr(J)].AsString, iBitmap.Bitmap);
          end;
        end;
      end;
    end
    else
      break;
  end;
end;

procedure TForm3.ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
var
  iTitle: TListItemText;
  iBitmap: TListItemImage;
  aPos: Single;
  I: Integer;
//  realIndex: Integer;
begin
  for I := 1 to ListView1.Columns do
  begin
    if not InRange(I, ColumnStartIndex, AItem.Tag) then
      continue;

    aPos := (((ListView1.ColumnWidth * I) - ListView1.ColumnWidth) - 6) + Trunc(ListView1.ColumnOffset * I);

    iBitmap := AItem.Objects.FindObjectT<TListItemImage>(colBitmap + IntToStr(I));
    if iBitmap = nil then
      iBitmap := TListItemImage.Create(AItem);
    iBitmap.Name := colBitmap + IntToStr(I);
    iBitmap.Width := ListView1.ColumnWidth - 8;
    iBitmap.Height := ListView1.ColumnWidth - 8;
    iBitmap.ScalingMode := TImageScalingMode.Stretch;
    iBitmap.PlaceOffset.X := aPos;
    iBitmap.PlaceOffset.Y := 4;
    iBitmap.OwnsBitmap := true;
    if (iBitmap.Bitmap = nil) then
      iBitmap.Bitmap := TBitmap.Create;

    // çàãîëîâîê
    iTitle := AItem.Objects.FindObjectT<TListItemText>(colTitle + IntToStr(I));
    if iTitle = nil then
      iTitle := TListItemText.Create(AItem);
    iTitle.Name := colTitle + IntToStr(I);
    iTitle.TextAlign := TTextAlign.Center;
    iTitle.TextVertAlign := TTextAlign.Center;
    iTitle.SelectedTextColor := TAlphaColorRec.Black;
    iTitle.TextColor := TAlphaColorRec.Black;
    iTitle.Font.Size := 14;
    iTitle.WordWrap := true;
    iTitle.Width := iBitmap.Width - 8;
    iTitle.PlaceOffset.X := aPos + 8;
    iTitle.PlaceOffset.Y := iBitmap.Height;
    iTitle.Height := ListView1.ItemAppearance.ItemHeight - iTitle.PlaceOffset.Y;
    iTitle.Text := AItem.Data[colTitle + IntToStr(I)].AsString;
  end;

  AHandled := true;
end;

procedure TForm3.OnColumnClick(const Sender: TObject; const Column: Integer; const X, Y: Single;
const AItem: TListViewItem; const DrawebleName: string);
// var
// realIndex: Integer;
begin
  ShowMessage(AItem.Data[colLogo + IntToStr(Column)].AsString);
  // realIndex := getRealIndex(AItem.Index, Column, ListView1.Columns);
  // ShowMessage(DrawebleName + #13#10 + realIndex.ToString + '  ' + FMembersList[realIndex].FileName + #13#10 +
  // AItem.Data[colLogo + IntToStr(Column)].AsString);
end;

procedure TForm3.ReLoadLV;
var
  J, realIndex, ColumnInRow, RowCount: Integer;
  AItem: TListViewItem;
  iBitmap: TListItemImage;
begin
  ListView1.BeginUpdate;
  try
  ListView1.OnPaint := nil;
  AniIndicator1.Enabled := true;

  while ListView1.Items.Count > 0 do
  begin
    for J := 1 to ListView1.Items[0].Tag do
    begin
      iBitmap := ListView1.Items[0].Objects.FindObjectT<TListItemImage>(colBitmap + IntToStr(J));
      if (Assigned(iBitmap) and iBitmap.OwnsBitmap) then
      begin
        iBitmap.Bitmap.Free;
        iBitmap.Bitmap := nil;
      end;
    end;

    ListView1.Items.Delete(0);
  end;

  if FMembersList.Count < 0 then
    exit;

  RowCount := ceil(FMembersList.Count / ListView1.Columns);
  realIndex := -1;

  for J := 0 to RowCount - 1 do
  begin
    inc(realIndex);

    AItem := ListView1.Items.Add;
    with AItem do
    begin
      ColumnInRow := ColumnStartIndex;

      while realIndex < FMembersList.Count do
      begin
        Data[colTitle + IntToStr(ColumnInRow)] := FMembersList.Items[realIndex].Name;
        Data[colLogo + IntToStr(ColumnInRow)] := FMembersList.Items[realIndex].URL;
        Data[colLoading + IntToStr(ColumnInRow)] := 1;
        Tag := ColumnInRow;

        if ColumnInRow mod ListView1.Columns = 0 then
          break;
        inc(realIndex);
        inc(ColumnInRow);
      end;

    end;
    ListView1.Adapter.ResetView(AItem);
  end;

  AniIndicator1.Enabled := false;
  ListView1.OnPaint := ListView1Paint;
  finally
    ListView1.EndUpdate;
    ListView1.Repaint;
  end;
end;

end.
