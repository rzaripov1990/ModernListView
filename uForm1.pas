unit uForm1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts,
  FMX.ListView, FMX.Objects;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    Layout1: TLayout;
    Label1: TLabel;
    Image1: TImage;
    Button1: TButton;
    Layout2: TLayout;
    Label2: TLabel;
    Switch1: TSwitch;
    cbTransSepar: TCheckBox;
    cbTransItems: TCheckBox;
    cbTransBackg: TCheckBox;
    procedure ListView1ButtonClick(const Sender: TObject; const AItem: TListItem;
      const AObject: TListItemSimpleControl);
    procedure Button1Click(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ListView1ApplyStyleLookup(Sender: TObject);
    procedure cbTransBackgChange(Sender: TObject);
    procedure cbTransItemsChange(Sender: TObject);
    procedure cbTransSeparChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses FMX.ListView.TextButtonFix;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ListView1.SetColorItemSelected(TAlphaColorRec.Orangered);
  ListView1.SetColorItemFill(TAlphaColorRec.Whitesmoke);
  ListView1.SetColorItemFillAlt(TAlphaColorRec.Lightgrey);
  ListView1.SetColorBackground(TAlphaColorRec.Whitesmoke);
  ListView1.SetColorItemSeparator(TAlphaColorRec.Red);

  ListView1.SetColorText(TAlphaColorRec.Darkmagenta);
  ListView1.SetColorTextSelected(TAlphaColorRec.Blueviolet);
  ListView1.SetColorTextDetail(TAlphaColorRec.Darksalmon);

  ListView1.SetColorHeader(TAlphaColorRec.Crimson);
  ListView1.SetColorTextHeader(TAlphaColorRec.Whitesmoke);
  ListView1.SetColorTextHeaderShadow(TAlphaColorRec.grey);
end;

procedure TForm1.cbTransBackgChange(Sender: TObject);
begin
  ListView1.Transparent := cbTransBackg.IsChecked;
end;

procedure TForm1.cbTransItemsChange(Sender: TObject);
begin
  ListView1.TransparentItems := cbTransItems.IsChecked;
end;

procedure TForm1.cbTransSeparChange(Sender: TObject);
begin
  ListView1.TransparentSeparators := cbTransSepar.IsChecked;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  I: Integer;
  AItem: TListViewItem;
begin
  ListView1.ItemsClearTrue;
  for I := 0 to 20 do
  begin
    AItem := ListView1.Items.Add;
    with AItem do
    begin
      if I mod 11 = 0 then
      begin
        Text := 'Header';
        Purpose := TListItemPurpose.Header;
      end
      else
      begin
        Text := 'Item Random ' + I.ToString;
        Detail := 'Detail for ' + Text;
        ButtonText := 'Custom Color';
        Bitmap := Image1.Bitmap;
      end;
    end;
    ListView1.Adapter.ResetView(AItem); // fix TextButton ( TListViewTextButtonFix )
  end;
end;

procedure TForm1.ListView1ApplyStyleLookup(Sender: TObject);
begin
  ListView1.SetColorPullRefresh(TAlphaColorRec.Lime);
  ListView1.SetColorPullRefreshIndicator(TAlphaColorRec.Limegreen);
  ListView1.SetColorStretchGlow(TAlphaColorRec.Limegreen);
end;

procedure TForm1.ListView1ButtonClick(const Sender: TObject; const AItem: TListItem;
  const AObject: TListItemSimpleControl);
begin
  if ListView1.IsCustomColorUsed(AItem.Index) then
    ListView1.SetDefaultColorForItem(AItem.Index)
  else
    ListView1.SetCustomColorForItem(AItem.Index, TAlphaColorF.Create(random(255) / 255, random(255) / 255,
      random(255) / 255, random(255) / 255).ToAlphaColor);
end;

procedure TForm1.ListView1UpdatingObjects(const Sender: TObject; const AItem: TListViewItem; var AHandled: Boolean);
begin
  // TListViewTextButtonFix.Rendering(Sender, AItem, AHandled);
end;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  ListView1.AlternatingColors := Switch1.IsChecked;
end;

end.
