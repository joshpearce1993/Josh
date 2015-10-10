unit DataTypes;

interface

uses dialogs, sysutils;

type
  TEventType = (StartSimulation, StopSimulation, PatientArrival, PatientTreated,
  PatientDeparture, UpdatePerformance);

  TMutationType = (Arrival, Departure);

  PEvent = ^TEvent; // Pointer to an event record

  TEvent = record // Event record
    Time: double; // Time of event
    EventType: TEventType; // Type of event
    Next: PEvent; // Pointer to the next event in list
    PatientSpeciality: string;  ////
  end;

  TRateArray = array of double;

  TPatientType = record
    PatientType: string;
    LOSDistName: string;
    mean: double;
    shape: double;
    scale: double;
    lambda: TRateArray;
    ////
    DischargeDistName: string;
    DischargeMean: double;
    DischargeShape: double;
    DischargeScale: double;
    procedure Clear;
  end;

  TScenario = record
    NumberStartOccupation: integer;
    MaxBeds: integer;
    StartTime: double;
    StopTime: double;
    NumberReplications: integer;
    // Time needed in minutes to discharge a patient
    //// TimeNeededForDischarge in user form
    TimeNeededForDischarge: double;
    //  Matrix consisting of the arrival rates per day/hour
    PoissonHourlyArrivalRates: TRateArray;
    // Maximum arrival rate (Used to simulate Poisson arrivals)
    MaxPoissonRate: double;
    // Array with the patient types and name and distribution information
    PatientTypes: array of TPatientType;
    // Start of time during which patients may be discharged
    StartDischargePeriod: integer;
    // End of time during which patients may be discharged
    StopDischargePeriod: integer;
    ////
    DischargeTimeArray: Array of integer;
    procedure Clear;
  end;

  TIntegerArray = array of integer;

  TPerformanceData = record
    CurrentWardOccupation: integer; // The current ward occupation
    // The number of patients in the ward that are ready to leave
    PatientsReadyToLeave: integer;
    WardOccupation: TIntegerArray;
    ArrivalsDayHour: TIntegerArray;
    BlockedDayHour: TIntegerArray;
    DepartureDayHour:TIntegerArray;
    // An array consisting of arrival and departure times of patients
    MutationArray: array of TDateTime;
    NumberMutations: integer;
    procedure Clear;
  end;

procedure AddEvent(Event: PEvent);
Procedure RemoveEvent(Event: PEvent);
function GetEvent(Time: double; EventType: TEventType): PEvent;
function GetEventWithString(Time: double; EventType: TEventType; name:string): PEvent;

function NextEvent: PEvent;
procedure ClearMemory(Event: PEvent);
function Min(a, b: integer): integer;
procedure ShowError(ErrorString: string);

var
  EventList: PEvent; // Event list
  DeletedEventList: PEvent; // List of deleted events
  Scenario: TScenario; // Contains the information needed to run simulation
  PerformanceData: TPerformanceData; // Consists of the perforance information

implementation

procedure TScenario.Clear;
begin
  SetLength(PoissonHourlyArrivalRates,0);
  SetLength(PatientTypes,0);
end;

procedure TPatientType.Clear;
begin
  self := Default (TPatientType);
end;

procedure TPerformanceData.Clear;
begin
  SetLength(WardOccupation ,0);
  SetLength(ArrivalsDayHour ,0);
  SetLength(BlockedDayHour ,0);
  SetLength(DepartureDayHour ,0);
  SetLength(MutationArray ,0);
end;

procedure ClearMemory(Event: PEvent);
{
This procedure frees all memory still allocated to the deleted events.
}
var
  point: PEvent;
begin
  if (Event=nil) then
    Exit;
  while Event.Next <> Nil do
  begin
    // Point to the first deleted event
    point := Event;
    // Point to the next event
    Event := Event.Next;
    Dispose(point);
  end;

  // Clear last record
  dispose(Event);
end;

procedure AddEvent(Event: PEvent);
{
This procedure adds an event to the event list.
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
  while (CurrentEvent.Next <> nil) and
  ((CurrentEvent.Next.Time < Time) or ((CurrentEvent.Next.Time = Time) and
  (CurrentEvent.Next.EventType < EventType))) do
  CurrentEvent := CurrentEvent.Next;

  // We have the current event, after which the added event should be placed
  if CurrentEvent.Next = nil then
  // If there is no next event, begin
  begin
    CurrentEvent.Next := Event; // Add in queue
    Event.Next := nil;
    exit; // Stop procedure
  end;

  // Insert event otherwise
  Event.Next := CurrentEvent.Next;

  // Forward follow up event to added event (from current event)
  CurrentEvent.Next := Event; // Queue event

end;

Procedure RemoveEvent(Event: PEvent);
{
This procedure removes an event from the event list.
}
begin
  // Point to the current last deleted event
  Event.Next := DeletedEventList;
  // Make remove event the current last removed event
  DeletedEventList := Event;
end;

procedure ShowError(ErrorString: string);
{
This procedure displays an error message.
}
begin
  ShowMessage(ErrorString);
  Halt(1);
end;

function GetEventWithString(Time: double; EventType: TEventType; name:string): PEvent;
{
This function returns an event based on input time and event type.
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
  // Also save the string information containing the speciality
  p.PatientSpeciality:= Name;
  p.Next := Nil;

  Result := p;

end;


function GetEvent(Time: double; EventType: TEventType): PEvent;
{
This function returns an event based on input time and event type.
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
This function returns the next event in the event list.
}
var
  point: PEvent;
begin
  // Set pointer to current top event
  point := EventList;
  // Update the event list
  EventList := EventList.Next;
  // Return the (now old) top event
  Result := point;
end;

Function Min(a, b: integer): integer;
{
This function returns the minimum of two integers.
}
begin
  if b < a then
    Result := b
  else
    Result := a;
end;

end.
