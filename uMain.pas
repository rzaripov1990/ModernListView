unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.ListView.Types, FMX.Controls.Presentation, FMX.ListView, FMX.Objects,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.Edit;

type
  TFormMain = class(TForm)
    btnColorizer: TButton;
    btnHorizontal: TButton;
    Label1: TLabel;
    btnColumns: TButton;
    Label2: TLabel;
    btnParameters: TButton;
    btnTransparent: TButton;
    btnScroll: TButton;
    procedure btnColorizerClick(Sender: TObject);
    procedure btnHorizontalClick(Sender: TObject);
    procedure btnColumnsClick(Sender: TObject);
    procedure btnParametersClick(Sender: TObject);
    procedure btnTransparentClick(Sender: TObject);
    procedure btnScrollClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  uForm1, uForm2, uForm3, uForm4, uForm5, uForm6;

procedure TFormMain.btnColorizerClick(Sender: TObject);
begin
  Form1.Show;
end;

procedure TFormMain.btnHorizontalClick(Sender: TObject);
begin
  Form2.Show;
end;

procedure TFormMain.btnColumnsClick(Sender: TObject);
begin
  Form3.Show;
end;

procedure TFormMain.btnParametersClick(Sender: TObject);
begin
  Form4.Show;
end;

procedure TFormMain.btnScrollClick(Sender: TObject);
begin
  Form6.Show;
end;

procedure TFormMain.btnTransparentClick(Sender: TObject);
begin
  Form5.Show;
end;

end.
