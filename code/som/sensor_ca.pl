/*
A sensor CA is a static (a priori) cognition actor that communicates with a body sensor to obtain its sensed value.

A sensor CA experiences its sensor's latest reading which pairs a value with an error tolerance.

A sensor CA listens to prediction events about its latest reading.

It may emit a prediction error event if the predicted sensed value diverges from the latest reading
more than the error tolerance of the sensor.

Each reading taken affects the CA's wellbeing. It reduces the CA's fullness (energy cost) and integrity (wear and tear) by 1 (they start at 100), and increases
its engagement by 1 (it starts at 0). A sensor CA can not read its sensor if its fullness or integrity is at 0. The CA publishes
changes to its wellbeing and receives transfers from its parents.

Certain readings incur additional costs/benefits to wellbeing, for example, a positive reading of the touch sensor
reduces the CA's integrity, or a color reading of green (food) increases fullness but a color reading of red (poison)
reduces integrity.

Messages:

* In
	* `adopted(Parent)` when added to the umwelt of a CA

* Out to a parent
	* `prediction_error(PredictionError)` - responding with the correct value to a prediction event where the prediction is incorrect - PredictionError = prediction_error{prediction:Prediction, actual_value:Value, confidence:Confidence, by: CA}

* In from a parent
    * `prediction(Prediction) - a parent makes a prediction about a sense reading - Prediction = prediction{name:Name, object:Object, value:Value, confidence:Confidence, by: CA, for:CAs}
 	* `wellbeing_transfer(Wellbeing)` - payload is wellbeing{fullness:N1, integrity:N2, engagement:N3}

Events:
	
* Out
    * topic: ca_started, payload: [level = Level]

Queries:

* In
    * level - 0
    * type - sensor_ca
    * latency - unknown - an effector CA has no set latency
    * experience_domain -> [predictable{name:distance, object:SensorName, domain:SenseDomain, by:CA}]
    * wellbeing -> wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement}

State:
	* parents - parent CAs
    * sensor - the body sensor the CA is responsible for
	* experiences - experiences from last reading - [experience(name:SensorName, object:Sense, value:Value, confidence:1.0)] - ignore tolerance
	* wellbeing - wellbeing metrics - initially wellbeing{fullness:1.0, integrity:1.0, engagement:0.0}

Lifecycle:
  * Created once for a body sensor
  * Repeatedly
    * Adopted by a CA as part of its umwelt (new parent) - subscribes to umwelt events from parent
    * Queried for its experience domain (only experiences its latest reading)
    * Handles prediction events by making readings and then maybe emitting prediction errors
      if the readings differ from the predictions more than the error tolerance of the sensor
  * Parent CA terminated - unsubscribes from umwelt events originating from the parent
*/

:- module(sensor_ca, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(agency(body)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/static_ca)).
:- use_module(agency(som/wellbeing)).

%! name_from_sensor(+Sensor, -Name) is det
% the name of the sensor CA given the sensor it wraps
name_from_sensor(Sensor, Name) :-
    atomic_list_concat([sensor, Sensor.id, Sensor.capabilities.sense], ':', Name).

%- level_from_name(+Name, -Level) is det
% the level of the CA given its name
level_from_name(_, 0).

%! latency(-Latency) is det
% the latency of a sensor CA is unknown
latency(unknown).

%! recruit(+Name, -Eagerness) :- is det
% a sensor CA always volunteer for recruitment with maximum eagerness, i.e. 1.0
recruit(_, 1.0).

%%% Actor logic

init(Options, State) :-
    log(info, sensor, "Initiating with ~p", [Options]),
    empty_state(EmptyState),
    option(sensor(Sensor), Options),
    initial_wellbeing(InitialWellbeing),
    put_state(EmptyState, [parents-[], sensor-Sensor, experiences-[], wellbeing-InitialWellbeing], State),
	subscribed_to_events(),
    published(ca_started, [level(0)]).

signal_processed(control(stopped)) :-
    all_unsubscribed,
    worker:stopped.

terminated :-
    log(warn, sensor_ca, "Terminated").

%% Actor interactions

handled(query(type), _, sensor_ca).

handled(query(level), _, 0).

handled(query(latency), _, unknown).

handled(query(experience_domain), State, [predictable{name:SensorName, object:SenseName, domain:SenseDomain, by:Name}]) :-
    self(Name),
    sense_name(State, SenseName),
    sensor_name(State, SensorName),
    sense_domain(State, SenseDomain).

handled(query(reading), State, Reading) :-
    sense_read(State, Reading).

handled(query(wellbeing), State, Wellbeing) :-
    get_state(State, wellbeing, Wellbeing).

handled(query(Query), State, Answer) :-
    ca_support : handled(query(Query), State, Answer).

% The sensor CA is adopted (fail if already adopted by this parent)
handled(message(adopted, Parent), State, NewState) :-
    \+ from_parent(Parent, State),
    all_subscribed([prediction - Parent]),
    acc_state(State, parents, Parent, NewState).

% Prediction = prediction{name:Name, object:Object, value:Value, confidence:Confidence, by: CA, for:CAs}
handled(message(prediction(Prediction), Parent), State, NewState) :-
    log(info, sensor_ca, "~@ received prediction ~p from ~w in ~p", [self, Prediction, Parent, State]),
    experiences_updated(Prediction, State, State1),
 	static_ca : prediction_handled(Prediction, Parent, State1, NewState).

handled(message(Message, Source), State, NewState) :-
   ca_support: handled(message(Message, Source), State, NewState).

handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

handled(event(Topic, Payload, Source), State, NewState) :-
    ca_support : handled(event(Topic, Payload, Source), State, NewState).

handled(message(wellbeing_transfer(WellbeingTransfer), _), State, NewState) :-
	wellbeing_transfered(State, WellbeingTransfer, NewState).

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).

%%%%

initial_wellbeing(wellbeing{fullness:1.0, integrity:1.0, engagement:0.0}).

subscribed_to_events() :-
	all_subscribed([ca_terminated]).

sense_name(State, SenseName) :-
    SenseName = State.sensor.capabilities.sense.

sense_domain(State, SenseDomain) :-
    SenseDomain = State.sensor.capabilities.domain.

sensor_name(State, SensorName) :-
    SensorName = State.sensor.id.

sense_url(State, SenseURL) :-
    SenseURL = State.sensor.url.

% Read the sensor value, update wellbeing, update experience in latest reading.
experiences_updated(Prediction, State, NewState) :-
    log(info, sensor_ca, "~@ is updating experiences from prediction ~p", [self, Prediction]),
    prediction{name:SensorName, object:SenseName} :< Prediction,
    sense_name(State, SenseName),
    sensor_name(State, SensorName),
    % Ignore tolerance and timestamp in reading for now
    sense_read(State, reading(Value, _, _)),
    !,
    % A sensor is 100% confident in what it experiences
    Experience = experience{name:SensorName, object:SenseName, value:Value, confidence:1.0},
    log(info, sensor_ca, "~@ experiences ~p", [self, Experience]),
    put_state(State, experiences, [Experience],  State1),
    wellbeing_changed(State, SenseName, Value, UpdatedWellbeing),
    put_state(State1, wellbeing, UpdatedWellbeing, NewState),
    log(info, sensor_ca, "~@ updated experiences in ~p", [self, NewState]).

sense_read(State, reading(Value, Tolerance, Timestamp)) :-
    sense_url(State, Url),
    body:sense_value(Url, Value, Tolerance),
    get_time(Timestamp),
    log(debug, sensor_ca, "Sensor ~w read ~w => ~p - ~p", [State.sensor.id, State.sensor.capabilities.sense, Value, Tolerance]).

% Update wellbeing from reading.
wellbeing_changed(State, SenseName, Value, UpdatedWellbeing) :-
    log(info, sensor_ca, "Changing wellbeing from sense ~w having value ~w", [SenseName, Value]),
    delta_fullness(SenseName, Value, DeltaFullness),
    delta_integrity(SenseName, Value, DeltaIntegrity),
    delta_engagement(SenseName, Value, DeltaEngagement),
    get_state(State, wellbeing, Wellbeing),
    DeltaWellbeing = wellbeing{fullness: DeltaFullness, integrity: DeltaIntegrity, engagement: DeltaEngagement},
    log(info, sensor_ca, "Change in wellbeing is ~p", [DeltaWellbeing]),
    UpdatedWellbeing = Wellbeing.add(DeltaWellbeing).

% Changes in wellbeing specific to the sensor reading
delta_fullness(color, green, 0.2).
delta_fullness(_, _, 0).

delta_integrity(contact, pressed, -0.2).
delta_integrity(color, red, -0.2).
delta_integrity(_, _, 0).

delta_engagement(_, _, 0).

