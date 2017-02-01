unit uForm6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm6 = class(TForm)
    ListView1: TListView;
    Switch1: TSwitch;
    Label1: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    Label2: TLabel;
    Switch2: TSwitch;
    procedure Switch1Switch(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Switch2Switch(Sender: TObject);
  private
    { Private declarations }
    procedure OnSwipeDirection(Sender: TObject; const Direction: TSwipeDirection);
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.FormShow(Sender: TObject);
var
  I: Integer;
begin
  ListView1.ItemsClearTrue;
  for I := 0 to 99 do
  begin
    with ListView1.Items.Add do
    begin
      Text := 'Item ' + I.ToString;
      Detail := '';
      Height := 25 + random(99);
    end;
  end;
end;

procedure TForm6.OnSwipeDirection(Sender: TObject; const Direction: TSwipeDirection);
begin
  if Direction = TSwipeDirection.ToLeft then
    ShowMessage('ToLeft')
  else
    ShowMessage('ToRigth');
end;

procedure TForm6.Switch1Switch(Sender: TObject);
begin
  ListView1.CanScroll := Switch1.IsChecked;
end;

procedure TForm6.Switch2Switch(Sender: TObject);
begin
  ListView1.CanSwipeDirection := Switch2.IsChecked;

  ListView1.OnSwipeDirection := OnSwipeDirection;
end;

end.
