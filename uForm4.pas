unit uForm4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.ListView, FMX.Objects;

type
  TForm4 = class(TForm)
    ListView1: TListView;
    Label1: TLabel;
    Text1: TText;
    procedure FormActivate(Sender: TObject);
    procedure ListView1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure ListView1ScrollViewChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;
  Counter: Integer = 0;

implementation

{$R *.fmx}

procedure TForm4.FormActivate(Sender: TObject);
var
  I: Integer;
begin
  ListView1.ItemsClearTrue;
  for I := 0 to 49 do
  begin
    with ListView1.Items.Add do
    begin
      Text := 'Item ' + I.ToString;
      Detail := '';
    end;
  end;
end;

procedure TForm4.ListView1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  Text1.Text := string.Join(':', [ListView1.getFirstVisibleItemIndex,
    ListView1.getVisibleCount, ListView1.getLastVisibleItemindex]);
  inc(Counter);
  Caption := Counter.ToString;
end;

procedure TForm4.ListView1ScrollViewChange(Sender: TObject);
begin
  Counter := 0;
  Caption := Counter.ToString;
end;

end.
