unit uForm5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm5 = class(TForm)
    ListView1: TListView;
    cbTransBackg: TCheckBox;
    cbTransItems: TCheckBox;
    cbTransSepar: TCheckBox;
    cbTransHeader: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure cbTransBackgChange(Sender: TObject);
    procedure cbTransItemsChange(Sender: TObject);
    procedure cbTransSeparChange(Sender: TObject);
    procedure cbTransHeaderChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.cbTransBackgChange(Sender: TObject);
begin
  ListView1.Transparent := cbTransBackg.IsChecked;
end;

procedure TForm5.cbTransHeaderChange(Sender: TObject);
begin
  ListView1.TransparentHeaders := cbTransHeader.IsChecked;
end;

procedure TForm5.cbTransItemsChange(Sender: TObject);
begin
  ListView1.TransparentItems := cbTransItems.IsChecked;
end;

procedure TForm5.cbTransSeparChange(Sender: TObject);
begin
  ListView1.TransparentSeparators := cbTransSepar.IsChecked;
end;

procedure TForm5.FormActivate(Sender: TObject);
var
  I: Integer;
begin
  ListView1.ItemsClearTrue;
  for I := 0 to 49 do
  begin
    with ListView1.Items.Add do
    begin
      if I mod 5 = 0 then
        Purpose := TlistItemPurpose.header;
      Text := 'Item ' + I.ToString;
      Detail := '';
      Height := 50 + random(99);
    end;
  end;
end;

end.
