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
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
    procedure ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Label1Tap(Sender: TObject; const Point: TPointF);
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
  FUniqueTag: Integer = 0;

implementation

{$R *.fmx}

uses
  System.Threading, System.Math, System.IOUtils,
  FMX.FireMonkey.Parser, FMX.BitmapHelper,
  uForm2;

const
  colTitle = 'title';
  colBitmap = 'bitmap';
  colLogo = 'logo';
  colLoading = 'loading';
  ColumnStartIndex = 1;

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

  TTask.Run(
    procedure
    begin
      TFireMonkey.Request(TFireMonkey.GetURL);
      TFireMonkey.MakeList;

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Tag := 0;
          ReLoadLV;
        end);
    end);
end;

procedure TForm3.Label1Tap(Sender: TObject; const Point: TPointF);
begin
  ShowMessage(TPath.Combine(TPath.GetDocumentsPath, 'content.txt'));
end;

procedure TForm3.ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  I, J, RowColumns: Integer;
  iBitmap: TListItemImage;
begin
  if (ListView1.Tag <> FUniqueTag) or (ListView1.Items.Count <= 0) then
    exit;

  for I := ListView1.getFirstVisibleItemIndex to ListView1.getFirstVisibleItemIndex + ListView1.getVisibleCount do
  begin
    if (I >= 0) and (I < ListView1.Items.Count) then
    begin
      RowColumns := ListView1.Items[I].Tag;
      for J := 1 to RowColumns do
      begin
        iBitmap := ListView1.Items[I].Objects.FindObjectT<TListItemImage>(colBitmap + IntToStr(J));

        if Assigned(iBitmap) and (Assigned(iBitmap.Bitmap)) then
        begin
          if (ListView1.Items[I].Data[colLoading + IntToStr(J)].AsInteger = 1) then
          begin
            if (ListView1.Tag = FUniqueTag) then
            begin
              ListView1.Items[I].Data[colLoading + IntToStr(J)] := 0;
              iBitmap.Bitmap.LoadFromURL(ListView1.Items[I].Data[colLogo + IntToStr(J)].AsString);
              // , nil, true,               ListView1.Tag);
            end;
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
    if (iBitmap.Bitmap = nil) or (not iBitmap.OwnsBitmap) then
    begin
      iBitmap.OwnsBitmap := true;
      iBitmap.Bitmap := TBitmap.Create;
    end;

    // заголовок
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
begin
  ShowMessage(DrawebleName + #13#10 + AItem.Data[colLogo + IntToStr(Column)].AsString);
end;

procedure TForm3.ReLoadLV;
var
  J, RealIndex, ColumnInRow, RowCount: Integer;
  AItem: TListViewItem;
  iBitmap: TListItemImage;
begin
  FUniqueTag := RandomRange(1000, 9999) + 1;

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
  RealIndex := -1;

  for J := 0 to RowCount - 1 do
  begin
    inc(RealIndex);

    AItem := ListView1.Items.Add;
    with AItem do
    begin
      ColumnInRow := ColumnStartIndex;

      while RealIndex < FMembersList.Count do
      begin
        Data[colTitle + IntToStr(ColumnInRow)] := FMembersList.Items[RealIndex].Name;
        Data[colLogo + IntToStr(ColumnInRow)] := FMembersList.Items[RealIndex].URL;
        Data[colLoading + IntToStr(ColumnInRow)] := 1;
        Tag := ColumnInRow;

        if ColumnInRow mod ListView1.Columns = 0 then
          break;
        inc(RealIndex);
        inc(ColumnInRow);
      end;

    end;
    ListView1.Adapter.ResetView(AItem);
  end;
  ListView1.Tag := FUniqueTag;
end;

end.
