/*
A sensor CA is an a priori cognition actor that communicates with a body sensor to read one sense.
Each sensor CA holds one neutral (neither pleasant nor unpleasant) belief about the sense value it measures.
A sensor CA has no noton of action.
It only listens to prediction events about its belief and may emit a prediction error event
based on the latest reading, if not stale, else based on a present reading, filtered on past readings.

A sensor CA:

* listens to events about:
    * prediction(Sense), expecting payload [value(Value)]

* sends messages about
    * prediction errors -> prediction_error([belief_name(Sense), 
                                             predicted_value(PredictedValue), 
                                             actual_value(ActualValue), 
                                             confidence(1.0), 
                                             source(Self)]))

* like all CAs, a sensor CA responds to queries about
    * name
    * level
    * latency
    * belief_domain -> responds with a [Sense-ValueDomain], e.g. [distance-domain{from:0, to:250}]
    * action_domain -> always responds with []

* believes its sensor's reading (observation) to a point
    * it remembers past sensings
    * applies a Kalman filter to determine its belief about the current sense value
        * see https://thekalmanfilter.com/kalman-filter-explained-simply/
        * to reduce sensor noise
*/

:- module(sensor_ca, []).

:- [load].

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(worker)).
:- use_module(agent(body)).

% The name of the sensor CA given the sensor it wraps
name_from_sensor(Sensor, Name) :-
    atomic_list_concat([sensor, Sensor.id, Sensor.capabilities.sense], ':', Name).

% The latency of a sensor CA is half a sceond
latency(0.5).

% The CA has the lowest level of abstraction
level(0).

init(Options, State) :-
    log(info, sensor_ca, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(sensor(Sensor), Options),
    put_state(EmptyState, sensor, Sensor, State1),
    belief_domain(State1, BeliefDomain),
    subcribe_to_predictions(State1, Topics),
    empty_reading(State1, Reading),
    put_state(State1, [subscriptions-Topics, belief_domain-BeliefDomain, readings-[Reading]], State),
    publish(ca_started, [level(0)]).

process_signal(control(stop)) :-
    terminate,
    worker:stop.

terminate :-
    log(warn, sensor_ca, 'Terminated').

handle(message(Message, Source), State, State) :-
   log(debug, sensor_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

% Sense is the belief name
handle(event(prediction(Sense), Payload, Source), State, NewState) :-
    self(Name),
    option(value(Value), Payload),
    log(debug, sensor_ca, '~w received prediction ~w is ~w by ~w', [Name, Sense, Value, Source]),
    sense(State, Sense),
    !,
    current_reading(State, Reading, NewState),
    (prediction_accurate(Sense, Value, Reading) ->
        log(info, sensor_ca, '~w received accurate prediction ~p about ~w from ~w', [Name, Value, Sense, Source])
        ;
        make_prediction_error(Sense, Value, Reading, PredictionError),
        send_message(Source, PredictionError),
        log(info, sensor_ca, '~w sent prediction error ~p to ~w', [Name, PredictionError, Source])
    ).

handle(event(Topic, Payload, Source), State, State) :-
    log(debug, sensor_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(name), _, Name) :-
    self(Name).

handle(query(type), _, ca).

handle(query(level), _, Level) :-
    level(Level).

handle(query(latency), _, Latency) :-
    latency(Latency).

handle(query(belief_domain), State, BeliefDomain) :-
    get_state(State, belief_domain, BeliefDomain).

handle(query(action_domain), _, []).

handle(query(Query), _, unknown) :-
    log(debug, sensor_ca, '~@ is NOT handling query ~p', [self, Query]).

%%%%

% Example [distance-domain{from:0, to:250}]
belief_domain(State, [Sense-Domain]) :-
    sense(State, Sense),
    sense_domain(State, Domain).

subcribe_to_predictions(State, [Topic]) :-
    sense(State, Sense),
    subscribe(prediction(Sense)).

empty_reading(State, Reading) :-
    sense(State, Sense),
    get_time(Timestamp),
    Reading = reading(Sense, 'unknown', 0, Timestamp).

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
        read_sense(State, Sense, Reading),
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

read_sense(State, Sense, reading(Sense, Value, Tolerance, Timestamp)) :-
    sense_url(State, Url),
    body:sense_value(Url, Value, Tolerance),
    get_time(Timestamp).

% Sense is the belief name
make_prediction_error(Sense, PredictedValue, reading(Sense, ActualValue, _, _), 
                        prediction_error([belief_name(Sense), 
                                          predicted_value(PredictedValue), 
                                          actual_value(ActualValue), 
                                          confidence(1.0), 
                                          source(Source)])) :-
    self(Source).