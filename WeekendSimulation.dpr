program WeekendSimulation;

uses
  Vcl.Forms,
  AMRandom in '..\amrandom\AMRandom.pas',
  AMRandomRS in '..\amrandom\AMRandomRS.pas',
  MRNG in '..\amrandom\MRNG.pas',
  DiscreteEventSimulation in 'DiscreteEventSimulation.pas' {FormSimulation},
  DataTypes in 'DataTypes.pas',
  GraphOccupation in 'GraphOccupation.pas' {FormGraphOccupation};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSimulation, FormSimulation);
  Application.CreateForm(TFormGraphOccupation, FormGraphOccupation);
  Application.Run;
end.
