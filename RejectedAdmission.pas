unit RejectedAdmission;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, Vcl.ExtCtrls, VCLTee.TeeProcs,
  VCLTee.Chart, VCLTee.Series, DiscreteEventSimulation, DataTypes, Vcl.StdCtrls;

type
  TFormRefusedAdmission = class(TForm)
    Chart1: TChart;
    Series1: TFastLineSeries;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRefusedAdmission: TFormRefusedAdmission;

implementation

{$R *.dfm}

procedure DrawRefusedAdmission;
var
  i, j, NumberPoints: integer;
  MinX, MaxX, Step: double;
begin
  // Clear the series
  FormRefusedAdmission.Chart1.Series[0].Clear;

  // Determine the number of points to draw
  NumberPoints := Length(PerformanceData.BlockedDayHour);

  // Determine the minimum and maximum
  MinX := 0;
  MaxX := (NumberPoints * 5) / 60;

  // Set the x-axis value
  FormRefusedAdmission.Chart1.axes[0].Minimum := MinX;
  FormRefusedAdmission.Chart1.axes[0].Maximum := MaxX;

  // Determine the step value
  Step := MaxX / NumberPoints;

  // Plot the rejected admission against time
  for i := 0 to NumberPoints - 1 do
  begin
    for j := 0 to High(AggregatePerformance[i].BlockedDayHour) do
    begin
      FormRefusedAdmission.Chart1.Series[0].AddXY(MinX + Step * i,
      AggregatePerformance[i].BlockedDayHour[j]);
    end;
  end;

end;

procedure TFormRefusedAdmission.Button1Click(Sender: TObject);
begin
  DrawRefusedAdmission;
end;

end.
