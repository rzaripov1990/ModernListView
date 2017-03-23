program ModernLV;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FormMain},
  uForm1 in 'uForm1.pas' {Form1},
  uForm2 in 'uForm2.pas' {Form2},
  uForm3 in 'uForm3.pas' {Form3},
  FMX.ListView in 'FMX.ListView.pas',
  FMX.ListView.TextButtonFix in 'comps\FMX.ListView.TextButtonFix.pas',
  FMX.FireMonkey.Parser in 'comps\FMX.FireMonkey.Parser.pas',
  uForm4 in 'uForm4.pas' {Form4},
  uForm5 in 'uForm5.pas' {Form5},
  uForm6 in 'uForm6.pas' {Form6},
  FMX.ListView.Types in 'FMX.ListView.Types.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm6, Form6);
  Application.Run;

end.
