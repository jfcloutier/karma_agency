/*
A sensor CA is a static (a priori) cognition actor that communicates with a body sensor to obtain its sensed value.

A sensor CA believes its sensor's latest reading which pairs a value with an error tolerance.

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
    * `prediction_error(PredictionPayload, ActualValue)` - responding to prediction event - the parent is wrong about how many times an action was executed

* In from a parent
 	* wellbeing_transfer(WellbeingPayload) - payload is [fullness = N1, integrity = N2, engagement = N3]

Events:

* In from parents
	* topic: prediction, payload: [belief = Belief] - a parent makes a prediction about how many of a given action the effector CA believes were executed
	
* Out
    * topic: ca_started, payload: [level = Level]
	* topic: wellbeing_changed, payload: WellbeingPayload  - payload is [fullness = N1, integrity = N2, engagement = N3]

Queries:

* In
    * level - 0
    * type - sensor_ca
    * latency - unknown - an effector CA has no set latency
    * belief_domain -> [predictable{name:distance, object:SensorName, value:SenseDomain}]

State:
	* parents - parent CAs
    * sensor - the body sensor the CA is responsible for
	* beliefs - beliefs from last reading - [distance(SensorName, Value)]
	* wellbeing - wellbeing metrics - initially [fullness = 100, integrity = 100, engagement = 0]

Lifecycle:
  * Created once for a body sensor
  * Repeatedly
    * Adopted by a CA as part of its umwelt (new parent) - subscribes to umwelt events from parent
    * Queried for its belief domain (only believes its latest reading)
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
    log(info, sensor_ca, "Initiating with ~p", [Options]),
    empty_state(EmptyState),
    option(sensor(Sensor), Options),
    initial_wellbeing(InitialWellbeing),
    put_state(EmptyState, [parents-[], sensor-Sensor, beliefs-[], wellbeing-InitialWellbeing], State),
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

handled(query(belief_domain), State, [predictable{name:SenseName, object:SensorName, value:SenseDomain}]) :-
    sense_name(State, SenseName),
    sensor_name(State, SensorName),
    sense_domain(State, SenseDomain).

handled(query(reading), State, Reading) :-
    sense_read(State, Reading).

handled(query(Query), State, Answer) :-
    ca_support : handled(query(Query), State, Answer).

% The sensor CA is adopted (fail if already adopted by this parent)
handled(message(adopted, Parent), State, NewState) :-
    \+ from_parent(Parent, State),
    all_subscribed([prediction - Parent]),
    acc_state(State, parents, Parent, NewState).

handled(message(Message, Source), State, NewState) :-
   ca_support: handled(message(Message, Source), State, NewState).

handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

handled(event(prediction, PredictionPayload, Parent), State, NewState) :-
    log(info, sensor_ca, "~@ is handling prediction ~p from ~w in ~p", [self, PredictionPayload, Parent, State]),
    option(belief(PredictedBelief), PredictionPayload),
    beliefs_updated(PredictedBelief, State, NewState),
 	ca_support : prediction_handled(PredictionPayload, Parent, NewState).

handled(event(Topic, Payload, Source), State, NewState) :-
    ca_support : handled(event(Topic, Payload, Source), State, NewState).

handled(message(wellbeing_transfer(WellbeingTransfer), _), State, NewState) :-
	wellbeing_transfered(State, WellbeingTransfer, NewState).

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).

%%%%

initial_wellbeing([fullness = 100, integrity = 100, engagement = 0]).

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

% Read the sensor value, update wellbeing and publish wellbeing_changed, update belief in latest reading.
beliefs_updated(PredictedBelief, State, NewState) :-
    PredictedBelief =.. [SenseName, SensorName, _],
    sense_name(State, SenseName),
    sensor_name(State, SensorName),
    sense_read(State, reading(Value, Tolerance, _)),
    !,
    wellbeing_changed(State, SenseName, Value, UpdatedWellbeing),
    Belief =.. [SenseName, SensorName, Value-Tolerance],
    put_state(State, beliefs, [Belief],  State1),
    put_wellbeing(State1, UpdatedWellbeing, NewState).

sense_read(State, reading(Value, Tolerance, Timestamp)) :-
    sense_url(State, Url),
    body:sense_value(Url, Value, Tolerance),
    get_time(Timestamp),
    log(debug, sensor_ca, "Sensor ~w read ~w => ~p - ~p", [State.sensor.id, State.sensor.capabilities.sense, Value, Tolerance]).

% Update wellbeing from reading.
wellbeing_changed(State, SenseName, Value, UpdatedWellbeing) :-
    delta_fullness(SenseName, Value, DeltaFullness),
    delta_integrity(SenseName, Value, DeltaIntegrity),
    delta_engagement(SenseName, Value, DeltaEngagement),
    get_wellbeing(State, Fullness, Integrity, Engagement),
    !,
    UpdatedFullness is Fullness + DeltaFullness - 1,
    UpdatedIntegrity is Integrity + DeltaIntegrity - 1,
    UpdatedEngagement is Engagement + DeltaEngagement + 1,
    UpdatedWellbeing = [fullness = UpdatedFullness, integrity = UpdatedIntegrity, engagement = UpdatedEngagement],
    published(wellbeing_changed, UpdatedWellbeing).


% Changes in wellbeing specific to the sensor reading
delta_fullness(color, green, 10).
delta_fullness(_, _, 0).

delta_integrity(contact, pressed, -10).
delta_integrity(color, red, -10).
delta_integrity(_, _, 0).

delta_engagement(_, _, 0).

