unit uForm2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListView, FMX.Edit, FMX.EditBox,
  FMX.SpinBox;

type
  TForm2 = class(TForm)
    ListView1: TListView;
    Layout1: TLayout;
    Layout2: TLayout;
    Label2: TLabel;
    Switch1: TSwitch;
    Layout3: TLayout;
    cbShowScrollBar: TCheckBox;
    cbMakeSelect: TCheckBox;
    Layout4: TLayout;
    cbSearch: TCheckBox;
    cbStyled: TCheckBox;
    StyleBook1: TStyleBook;
    Layout5: TLayout;
    Label3: TLabel;
    sbBottom: TSpinBox;
    Layout6: TLayout;
    Label1: TLabel;
    sbTop: TSpinBox;
    procedure Switch1Switch(Sender: TObject);
    procedure cbShowScrollBarChange(Sender: TObject);
    procedure cbMakeSelectChange(Sender: TObject);
    procedure cbSearchChange(Sender: TObject);
    procedure cbStyledChange(Sender: TObject);
    procedure sbBottomChange(Sender: TObject);
    procedure sbTopChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.cbMakeSelectChange(Sender: TObject);
begin
  ListView1.MakeSelectedItemVisible := cbMakeSelect.IsChecked;
end;

procedure TForm2.cbSearchChange(Sender: TObject);
begin
  ListView1.SearchVisible := cbSearch.IsChecked;
end;

procedure TForm2.cbShowScrollBarChange(Sender: TObject);
begin
  ListView1.ShowScrollBar := cbShowScrollBar.IsChecked;
end;

procedure TForm2.cbStyledChange(Sender: TObject);
begin
  if cbStyled.IsChecked then
    ListView1.StyleLookup := 'listviewstyle_panel'
  else
    ListView1.StyleLookup := '';
end;

procedure TForm2.FormShow(Sender: TObject);
var
  I: Integer;
begin
  ListView1.ItemsClearTrue;
  for I := 0 to 20 do
  begin
    with ListView1.Items.Add do
    begin
      Text := 'Item ' + I.ToString;
      Detail := '';
    end;
  end;
end;

procedure TForm2.sbBottomChange(Sender: TObject);
begin
  ListView1.OffsetBottom := trunc(sbBottom.Value);
end;

procedure TForm2.sbTopChange(Sender: TObject);
begin
  ListView1.OffsetTop := trunc(sbTop.Value);
end;

procedure TForm2.Switch1Switch(Sender: TObject);
begin
  ListView1.Horizontal := Switch1.IsChecked;
  if ListView1.Horizontal then
  begin
    ListView1.ItemAppearance.ItemHeight := 220;
    ListView1.ItemAppearance.ItemEditHeight := 220;
    ListView1.Height := 120;
  end
  else
  begin
    ListView1.ItemAppearance.ItemHeight := 50;
    ListView1.ItemAppearance.ItemEditHeight := 50;
    ListView1.Height := ClientHeight - Layout1.Height - 20;
  end;
end;

end.
