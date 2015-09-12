unit GraphOccupation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
  VclTee.Series, Vcl.ExtCtrls, VclTee.TeeProcs, VclTee.Chart, SimWeekend,
  DataTypes, math, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Chart1: TChart;
    Series2: TFastLineSeries;
    Series3: TFastLineSeries;
    OccupationButton: TButton;
    Series1: TFastLineSeries;
    Series4: TFastLineSeries;
    procedure OccupationButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DrawBedOccupancy;
  end;

var
  Form2: TForm2;
  BedbezettingArray: array of array of integer;
  // number of occupied beds of [sim] at [time]
  GemBedBezettingArray: array of double;
  // gemiddeld (average) num of occupied beds over all sims at [time]
  StDevBedBezettingArray: array of double;
  // stdev of u=number of occupied beds over all sims at [time]

  // fill the gembedbezettingarray with data
procedure FillAverageAndDeviationBedBezettingArray;

// this procedure fills the bedbezetting arrays to be used when drawn the ocupation graph
// usese mutation arrays from SimWeekend (aggregateperformance[sim].mutationarray)
procedure FillBedBezettingArrays;

implementation

{$R *.dfm}

// this procedure fills the bedbezetting arrays to be used when drawn the ocupation graph
// usese mutation arrays from SimWeekend (aggregateperformance[sim].mutationarray)
procedure FillBedBezettingArrays;
var
  i: integer;
  simtime: double;
  slots: integer;
  simcounter, timecounter, OccupiedCounter: integer;
  TimeOfMutation, TimeOfSlot: double;
  Arrival: boolean;
begin
  // clear arrays
  setlength(BedbezettingArray, 0);
  setlength(GemBedBezettingArray, 0);
  setlength(StDevBedBezettingArray, 0);

  // determine number of 5min slots that fit in the sim
  simtime := scenario.StopTime - scenario.StartTime;
  // now sim time is expressed in days
  if (simtime * 24 * 12) - trunc(simtime * 24 * 12) > 0 then
    halt; // this should not happen!!!
  slots := trunc(simtime * 24 * 12);
  // 24H per day and 12slots per hour (should work as sim is always in (integer) multiple of hours)

  // init arrays
  setlength(BedbezettingArray, scenario.NumRepl);
  for i := 0 to high(BedbezettingArray) do
  begin
    setlength(BedbezettingArray[i], slots);
  end;
  setlength(GemBedBezettingArray, slots);
  setlength(StDevBedBezettingArray, slots);

  // first fill the bedbezetting array with all sim data
  for simcounter := 0 to scenario.NumRepl - 1 do
  begin
    BedbezettingArray[simcounter][0] := scenario.NrOccopiedBedsOnStart;
    // beginning with starting number of beds
    // walk through mutatino array until after the current time slot is time
    OccupiedCounter := scenario.NrOccopiedBedsOnStart; // init
    TimeOfMutation := -1; // init
    TimeOfSlot := scenario.StartTime; // init
    i := 0; // init
    TimeOfMutation := max(AggregatePerformance[simcounter].MutationArray[i],
      AggregatePerformance[simcounter].MutationArray[i] * -1);
    // set to first mutation entry

    // for every time that is measured (5min interval)
    for timecounter := 0 to high(BedbezettingArray[simcounter]) do
    begin

      // there are two possibilities
      // until the next mutation has not happened yet
      while (TimeOfMutation < TimeOfSlot) AND
        (i <= high(AggregatePerformance[simcounter].MutationArray)) do
      begin
        i := i + 1;
        TimeOfMutation := max(AggregatePerformance[simcounter].MutationArray[i],
          AggregatePerformance[simcounter].MutationArray[i] * -1);
        if AggregatePerformance[simcounter].MutationArray[i] > 0 then
        begin
          OccupiedCounter := OccupiedCounter + 1;
        end
        else
        begin
          OccupiedCounter := OccupiedCounter - 1;
        end;

      end; // end while
      BedbezettingArray[simcounter][timecounter] := OccupiedCounter;

      TimeOfSlot := TimeOfSlot + (1 / (24 * 12)); // add 5 minutes
    end; // end for

  end;

end;

// fill the gembedbezettingarray with data
procedure FillAverageAndDeviationBedBezettingArray;
var
  i, j: integer;
begin
  // dim array
  setlength(GemBedBezettingArray, 0); // init
  setlength(GemBedBezettingArray, length(BedbezettingArray[0]));
  // dim same size as first sim (are all equal)

  // fill array per time slot
  for i := 0 to high(GemBedBezettingArray) do
  begin
    GemBedBezettingArray[i] := 0; // init value;
    j := 0; // init;
    // for every simulation
    while j <= high(BedbezettingArray) do
    begin
      GemBedBezettingArray[i] := GemBedBezettingArray[i] +
        BedbezettingArray[j][i];
      j := j + 1;

    end; // end while
    // all values are added in while loop, div by j+1
    GemBedBezettingArray[i] := GemBedBezettingArray[i] / (j + 1);
  end; // end for

  // now determine unbiased variance and subsequantly stdev
  setlength(StDevBedBezettingArray, 0); // init
  setlength(StDevBedBezettingArray, length(GemBedBezettingArray)); // init

  for i := 0 to high(StDevBedBezettingArray) do
  begin
    StDevBedBezettingArray[i] := 0; // init value (prolly overbodig!)
    j := 0; // init
    // for every simulation do
    while j <= high(BedbezettingArray) do
    begin
      StDevBedBezettingArray[i] := StDevBedBezettingArray[i] +
        Power((BedbezettingArray[j][i] - GemBedBezettingArray[i]), 2);
      j := j + 1;
    end;
    // now divide by j (=N-1)
    StDevBedBezettingArray[i] := StDevBedBezettingArray[i] / j;
    // take root to get stdev
    StDevBedBezettingArray[i] := Sqrt(StDevBedBezettingArray[i]);
  end;
end;

procedure TForm2.DrawBedOccupancy;
var
  NumSim, NumPoints, i, jcount: integer;
  Variance, MinX, MaxX, Y, EWval, EWVar, OldMVar, MVar, SVar, CIHW,Step: double;
  DrawEWperSim: boolean;
  TimeIndex, PatIndex: integer;
  j, correctedpatindex: integer;
begin
  // clear series
  Chart1.Series[0].clear;
  Chart1.Series[1].clear;
  Chart1.Series[2].clear;
  Chart1.Series[3].clear;//max line
  NumPoints := length(GemBedBezettingArray); // get number of points to draw

  // get the min and max values?
  MinX := 0;
  MaxX := (NumPoints * 5) / 60; // max x is number of hours passed in simulation

  // set x axis value
  Chart1.axes[0].Minimum := MinX;
  Chart1.axes[0].Maximum := MaxX;

  // get step value
  Step := MaxX / NumPoints;

  // for all ticked graphs draw distributions
  for i := 0 to NumPoints - 1 do
  begin
    Chart1.Series[0].AddXY(MinX + Step * i, GemBedBezettingArray[i]);
    // add mean
    // add stdevs
    Chart1.Series[1].AddXY(MinX + Step * i,
      (GemBedBezettingArray[i] + StDevBedBezettingArray[i]));
    Chart1.Series[2].AddXY(MinX + Step * i,
      (GemBedBezettingArray[i] - StDevBedBezettingArray[i]));
  end;

  //draw bed max line
  Chart1.Series[3].AddXY(MinX, Scenario.MaxBeds);
  Chart1.Series[3].AddXY(MaxX, Scenario.MaxBeds) ;


end;

procedure TForm2.OccupationButtonClick(Sender: TObject);
begin
FillBedBezettingArrays;
  FillAverageAndDeviationBedBezettingArray;

  DrawBedOccupancy;
end;

initialization

finalization

end.
