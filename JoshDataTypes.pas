unit JoshDataTypes;

interface

uses dialogs, sysutils;

type

  TEventType = (StartSimulation, StopSimulation, PatientArrival,
  PatientTreated, PatientDeparture, UpdatePerformance);

  TMutationType = (Arrival, Departure);

  PEvent = ^TEvent; //// What does ^ mean?

  TEvent = record  // Event record
    Time: double;  // Time of event
    EventType: TEventType; // Type of event
    Next: PEvent; // Pointer to the next event in list
    procedure Clear;
  end;

  TRateArray = array of double;

  TPatientType = record
    PatientType: string;
    LOSDistName: string;
    mean: double;
    par2: double;
    par3: double;
    lambda: TRateArray;
    procedure Clear;
  end;

  TScenario = record
    MaxBeds: integer;
    NumberStartOccupation: integer;
    StartDischargePeriod: integer;
    StopDischargePeriod: integer;
    NumberReplications: integer;
    StartTime: double;
    StopTime: double;
    TimeNeededForDischarge: integer;
    //  Matrix consisting of the arrival rates per day/hour
    PoissonHourlyArrivalRates: TRateArray;
    // Maximum arrival rate (aux used to simulate Poisson arrivals) ////aux???
    MaxPoissonRate: double;
    // Array with the patient types and name and distribution info
    PatientTypes: array of TPatientType;
    procedure Clear;
  end;

  TIntegerArray = array of integer;

  TPerformanceData = record
    CurrentWardOccupation: integer;
    PatientsReadyToLeave: integer;
    WardOccupation: TIntegerArray;
    ArrivalsDayHour: TIntegerArray;
    BlockedDayHour: TIntegerArray;
    DepartureDayHour: TIntegerArray;
    // An array consisting of arrival and departure times of patients
    MutationArray: array of TDateTime;
    NumberMutations: integer;
    procedure Clear;
  end;

procedure AddEvent(Event: PEvent);
procedure RemoveEvent(Event: PEvent);
function GetEvent(Time: double; EventType: TEventType): PEvent;
function NextEvent: PEvent;
procedure ClearMemory(Event: PEvent);

procedure ShowError(ErrorString: string);
Function Min(a, b: integer): integer;

var
  EventList: PEvent; // Event list
  DeletedEventList: PEvent; // List of deleted events
  Scenario: TScenario; // Contains the information needed to run simulation
  PerformanceData: TPerformanceData; // Contains the informance on performance

implementation

procedure TScenario.Clear;
begin
  self := Default (TScenario);
end;

procedure TPatientType.Clear;
begin
  self := Default (TPatientType);
end;

procedure TPerformanceData.Clear;
begin
  self := Default (TPerformanceData);
end;

procedure TEvent.Clear;
begin
  self := Default (TEvent);
end;

procedure AddEvent(Event: PEvent);
{
This procedure adds an event to the event list
}
var
  Time: double;
  EventType: TEventType;
  CurrentEvent: PEvent;
begin
  //  Determine the time of the event that will be added
  Time := Event.Time;

  {If the event list is empty, or the event list is after the current time,
  add the event to the front of the list and exit the procedure (as EventList
  is the current upcoming event)}
  if (EventList = nil) or (EventList.Time > Time) then
  begin
    // The event after the added event is set as EventList
    Event.Next := EventList;
    // The first event to execute is the event to be added
    EventList := Event;
    // Stop procedure. Done adding events.
    exit;
  end;

  // Determine the event type of the added event
  EventType := Event.EventType;
  // Determine the current event
  CurrentEvent := EventList;

  // Cycle through events until we find the proper spot to include the event
  while ((CurrentEvent.Next <> nil) and ((CurrentEvent.Next.Time < Time) or
  (CurrentEvent.Next.EventType < EventType))) do
  CurrentEvent := CurrentEvent.Next;

  // We have the current event, after which the added event should be placed
  if CurrentEvent.Next = nil then
  // If there is no next event, then:
  begin
    CurrentEvent.Next := Event; // Add in queue
    Event.Next := nil;
    exit; // Stop procedure
  end;

  // Insert event otherwise
  event.Next := CurrentEvent.Next;
  // Forward follow up event to added event (from current event)
  CurrentEvent.Next := Event; // Queue event

end;

procedure RemoveEvent(Event: PEvent);
{
This procedure removes an event from the event list
}
begin
  // Point to the current last deleted event
  Event.Next := DeletedEventList;
  // Make remove event the current last removed event
  DeletedEventList := Event;
end;

function GetEvent(Time: double; EventType: TEventType): PEvent;
{
This function returns an event based on input time and event type
}
var
  p: PEvent;
begin
  {First check if there are deleted events (memory locations) that may be used,
  otherwise create new memory block}
  if DeletedEventList <> nil then
  begin
    p := DeletedEventList;
    DeletedEventList := DeletedEventList.Next;
  end
  else
  begin
    p := nil;
    try
      New(p);
    except
      ShowError('Insuffiecient memory');
    end;
  end;

  p.Time := Time;
  p.EventType := EventType;
  p.Next := Nil;

  Result := p;

end;

function NextEvent: PEvent;
{
This function returns the next event in the event list
}
var
  point: PEvent;
begin
  // Set aux pointer to current top event
  point := EventList;
  // Update the event list
  EventList := EventList.Next;
  // Return the (now old) top event
  Result := point;
end;

procedure ShowError(ErrorString: string);
{
This procedure displays an error message
}
begin
  ShowMessage(ErrorString);
  Halt(1);
end;

procedure ClearMemory(Event: PEvent);
{
This procedure frees all memory still allocated to the deleted events
}
var
  point: PEvent;
begin
  while DeletedEventList.Next <> nil do
  begin
    // Point to the first deleted event
    point := DeletedEventList;
    // Point to the next event
    DeletedEventList := DeletedEventList.Next;
    Dispose(point);
  end;

  // Clear last record
  Dispose(DeletedEventList);

end;

Function Min(a, b: integer): integer;
{
This function returns the minimum of two integers
}
begin
  if b < a then
    Result := b
  else
    Result := a;
end;

end.

