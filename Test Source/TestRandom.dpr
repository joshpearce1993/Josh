program TestRandom;



uses
  Forms,
  ESBPCSRegistry,
  TestMain in 'TestMain.pas' {Form1},
  AMRandom in 'AMRandom.pas',
  MRNG in 'MRNG.pas',
  AMRandomRS in 'AMRandomRS.pas';

{$R *.RES}

begin
     Application.Initialize;
     Application.Title := 'Test AMRandom';
{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 18.5 }
     if WinVistaOrBetter then
          Application.MainFormOnTaskBar := True;
     Application.ModalPopupMode := pmAuto;
{$IFEND}
{$ENDIF}
     Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

