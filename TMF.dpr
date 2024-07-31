program TMF;

uses
  Vcl.Forms,
  Main in '..\TextureMaterialFinder\Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
