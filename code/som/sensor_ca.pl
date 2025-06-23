/*
A sensor CA is an a priori cognition actor that communicates with a body sensor to obtain
its sensed value.

A sensor CA believes its sensor's latest reading which pairs a value with an error tolerance.

A sensor CA listens to prediction events about its latest reading.

It may emit a prediction error event if the predicted sensed value diverges from the latest reading
more than the error tolerance of the sensor.

A sensor CA:

* receives a message `adopted` when added to the umwelt of a CA

* subscribes to events from its parents:
	* topic: prediction, payload: [belief = Belief] - a parent makes a prediction about a sensed value

* sends messages to a parent in response to events received:
    * prediction event -> maybe respond with message `prediction_error(PredictionPayload, ActualValue)` - the parent is wrong about the sensed value

* like all CAs, a sensor CA responds to queries about
    * name
    * level - 0
    * latency - unknown - a sensor CA has no set latency
    * belief_domains -> responds with [predictable{name:sensed, objects:[], values:[]}],
                        e.g. [predictable(name:sensed, objects:[distance], values:domain{from:0, to:250})]

Lifecycle:
  * Created once for a body sensor
  * Repeatedly
    * Adopted by a CA as part of its umwelt (new parent) - subscribes to umwelt events from parent
    * Queried for its belief domains (only believes its latest reading)
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
    put_state(EmptyState, [parents-[], sensor-Sensor, beliefs-[]], State),
	subscribed_to_events(),
    published(ca_started, [level(0)]).

signal_processed(control(stopped)) :-
    all_unsubscribed,
    worker:stopped.

terminated :-
    log(warn, sensor_ca, "Terminated").

handled(query(name), _, Name) :-
    self(Name).

handled(query(type), _, sensor_ca).

handled(query(level), _, 0).

handled(query(latency), _, unknown).

handled(query(belief_domains), State, [predictable{name:sensed, objects:[SenseName], values:SenseDomain}]) :-
    sense_name(State, SenseName),
    sense_domain(State, SenseDomain).

handled(query(policy_domain), _, []).

handled(query(Query), State, Answer) :-
    ca_support : handled(query(Query), State, Answer).

% The sensor CA is adopted
handled(message(adopted, Parent), State, NewState) :-
    all_subscribed([ca_terminated - Parent, prediction - Parent]),
    acc_state(State, parents, Parent, NewState).

handled(message(Message, Source), State, NewState) :-
   ca_support: handled(message(Message, Source), State, NewState).

handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

handled(event(prediction, PredictionPayload, Parent), State, NewState) :-
 	option(belief(PredictedBelief), PredictionPayload),
    beliefs_updated(PredictedBelief, State, NewState),
	about_belief(PredictedBelief, NewState, Belief),
	belief_value(PredictedBelief, PredictedValue),
	belief_value(Belief, ActualValue),
	(same_belief_value(PredictedValue, ActualValue) ->
		true;
		message_sent(Parent, prediction_error(PredictionPayload, ActualValue))
	).	

handled(event(Topic, Payload, Source), State, NewState) :-
    ca_support : handled(event(Topic, Payload, Source), State, NewState).

%%%%

subscribed_to_events() :-
	all_subscribed([ca_terminated, executed]).

sense_name(State, SenseName) :-
    get_state(State, sensor, Sensor),
    SenseName = Sensor.capabilities.sense.

sense_domain(State, SenseDomain) :-
    get_state(State, sensor, Sensor),
    SenseDomain = Sensor.capabilities.domain.

sense_url(State, SenseURL) :-
    SenseURL = State.sensor.url.

beliefs_updated(PredictedBelief, State, NewState) :-
    PredictedBelief =.. [sensed, SenseName, _],
    sense_name(State, SenseName),
    sense_read(State, reading(Value, Tolerance, _)),
    put_state(State, beliefs, [sensed(SenseName, Value-Tolerance)], NewState).

sense_read(State, reading(Value, Tolerance, Timestamp)) :-
    sense_url(State, Url),
    body:sense_value(Url, Value, Tolerance),
    get_time(Timestamp).
