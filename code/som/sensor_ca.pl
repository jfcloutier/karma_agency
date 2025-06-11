/*
A sensor CA is an a priori cognition actor that communicates with a body sensor to read one sense with some confidence.
Each sensor CA holds one neutral (neither pleasant nor unpleasant) belief about the sense value it measures.

A sensor CA can not "act". It only listens to prediction events about its belief and may emit a prediction error event
based on the latest reading, if not stale (latency), else based on a present reading, filtered on past readings.

A sensor CA:

* receives a message `adopted` when added to the umwelt of a CA

* subscribes to events from its parents:
    * topic: prediction, payload: [predicted = belief(Name, Value)]

* publishes events
    * topic: prediction error, payload:[predicted = belief(Name, Value1), actual = belief(Name, Value2), confidence = 1.0])

* like all CAs, a sensor CA responds to queries about
    * name
    * level
    * latency
    * belief_domains -> responds with [Name-ValueDomain | _], e.g. [distance - domain{from:0, to:250}]
    * action_domain -> always responds with []


* believes its sensor reading (observation) after noise reduction
    * it remembers past readings
    * applies a Kalman filter to filter the current sense value
        * see https://thekalmanfilter.com/kalman-filter-explained-simply/ and https://bilgin.esme.org/BitsAndBytes/KalmanFilterforDummies
        * to reduce sensor noise and extract a signal from each reading

Lifecycle
  * Created for a body sensor
  * Repeatedly
    * Adopted by a CA as part of its umwelt (new parent)
    * Queried for its belief domains (what predictions about its one belief it can receive) - belief name = sense name
    * Handles prediction events by making readings, maybe emitting prediction errors
    * Parent CA terminated
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
% the latency of a sensor CA is half a sceond
latency(0.5).

%! level(-Level) is det
% the CA has the lowest level of abstraction, i.e. 0
level(0).

%! recruit(+Name, -Eagerness) :- is det
% a sensor CA always volunteer for recruitment with maximum eagerness, i.e. 1.0
recruit(_, 1.0).

%%% Actor logic

init(Options, State) :-
    log(info, sensor_ca, "Initiating with ~p", [Options]),
    empty_state(EmptyState),
    option(sensor(Sensor), Options),
    put_state(EmptyState, sensor, Sensor, State1),
    % Singleton: a sensor CA has only one belief domain
    belief_domain(State1, BeliefDomain),
    empty_reading(State1, Reading),
    put_state(State1, [parents-[], belief_domain-BeliefDomain, readings-[Reading]], State),
    published(ca_started, [level(0)]).

signal_processed(control(stopped)) :-
    terminated,
    worker:stopped.

terminated :-
    log(warn, sensor_ca, "Terminated").

% The sensor CA is adopted
handled(message(adopted, Parent), State, NewState) :-
    all_subscribed([ca_terminated - Parent, prediction - Parent]),
    get_state(State, parents, Parents),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(Message, Source), State, NewState) :-
   ca_support: handled(message(Message, Source), State, NewState).

handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    delete(Parent, Parents, Parents1),
    put_state(State, parents, Parents1, NewState).

handled(event(prediction, PredictionPayload, Source), State, NewState) :-
    % Sense is the belief name for a sensor CA
    option(believed(Sense, PredictedValue), PredictionPayload),
    % This is the sense of this sensor CA
    sense(State, Sense),
    log(debug, sensor_ca, "~@ received prediction ~w from ~w", [self, believed(Sense, PredictedValue), Source]),
    !,
    current_reading(State, Reading, NewState),
    (prediction_accurate(Sense, PredictedValue, Reading) ->
        (log(info, sensor_ca, "~@ received accurate prediction ~p about ~w from ~w", [self, PredictedValue, Sense, Source])
        ;
        prediction_error(PredictionPayload, Reading, PredictionErrorPayload),
        published(prediction_error, PredictionErrorPayload),
        log(info, sensor_ca, "~@ published prediction error ~p", [self, PredictionErrorPayload])
        )
    ).

handled(event(Topic, Payload, Source), State, NewState) :-
    ca_support : handled(event(Topic, Payload, Source), State, NewState).

handled(query(name), _, Name) :-
    self(Name).

handled(query(type), _, ca).

handled(query(level), _, Level) :-
    level(Level).

handled(query(latency), _, Latency) :-
    latency(Latency).

handled(query(belief_domains), State, [Sense-Domain]) :-
    get_state(State, belief_domain, Sense-Domain).

handled(query(action_domain), _, []).

handled(query(Query), State, Answer) :-
    ca_support : handled(query(Query), State, Answer).

%%%%

% Example distance-domain{from:0, to:250}
belief_domain(State, Sense-Domain) :-
    sense(State, Sense),
    sense_domain(State, Domain).

subscribed_to_events_from_parent(Parent) :-
    all_subscribed(ca_terminated - Parent, prediction - Parent).

prediction_topics(State, [prediction(Sense)]) :-
    sense(State, Sense).

empty_reading(State, Reading) :-
    sense(State, Sense),
    get_time(Timestamp),
    Reading = reading(Sense, unknown, 0, Timestamp).

sense(State, Sense) :-
    Sense = State.sensor.capabilities.sense.

sense_url(State, SenseURL) :-
    SenseURL = State.sensor.url.

sense_domain(State, SenseDomain) :-
    SenseDomain = State.sensor.capabilities.domain.

current_reading(State, Reading, NewState) :-
    last_reading(State, LastReading),
    (reading_stale(LastReading) ->
        sense(State, Sense),
        sense_read(State, Sense, Reading),
        put_state(State, readings, [Reading | State.readings], NewState)
        ;
        Reading = LastReading,
        NewState = State
    ).

last_reading(State, Reading) :-
    [Reading | _] = State.readings.

reading_stale(reading(_, unknown, _, _)) :- !.
reading_stale(reading(_, _, _, Timestamp)) :-
    get_time(Now),
    latency(Latency),
    Age is Now - Timestamp,
    Age > Latency.

prediction_accurate(Sense, PredictedValue, reading(Sense, Value, 0, _)) :-
    !,
    PredictedValue == Value.
prediction_accurate(Sense, PredictedValue, reading(Sense, Value, Tolerance, _)) :-
    Delta is abs(PredictedValue - Value),
    Delta =< Tolerance.

sense_read(State, Sense, reading(Sense, Value, Tolerance, Timestamp)) :-
    sense_url(State, Url),
    body:sense_value(Url, Value, Tolerance),
    get_time(Timestamp).

% Sense is the belief name
% [predicted = belief(Name, Value1), actual = belief(Name, Value2), confidence = 1.0])
prediction_error(belief(Sense, PredictedValue), reading(Sense, ActualValue, _, _),
                        
                        prediction_error([predicted = belief(Sense, PredictedValue),
                                          actual = ActualValue, 
                                          confidence = 1.0])).