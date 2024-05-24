/*
A sensor is an a priori cognition actor that communicates with a body sensor to read one sense.
Each sensor CA holds one belief. It is about the sense value it measures.
It listens to prediction events about its belief and may emit a prediction error event
based on the latest reading, if not staled, else based on a present reading.
*/

:- module(sensor_ca, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).

% The name of the sensor CA
name(Sensor, Name) :-
    atomic_list_concat([sensor, Sensor.id, Sensor.capabilities.sense], ':', Name).

% The period of the sensor CA
period(0.5).

init(Options, State) :-
    log(info, sensor_ca, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(sensor(Sensor), Options),
    put_state(EmptyState, sensor, Sensor, State),
    send_message(start).

terminate :-
    log(warn, sensor_ca, 'Terminating').

handle(message(start, Source), State, NewState) :-
    thread_self(Source),
    belief_domain(State, BeliefDomain),
    subcribe_to_predictions(State, Topics),
    empty_reading(State, Reading),
    put_state(State, [subscriptions-Topics, belief_domain-BeliefDomain, readings-[Reading]], NewState).


handle(message(Message, Source), State, State) :-
   log(info, sensor_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(prediction(Sense), Value, Source), State, NewState) :-
    sense(State, Sense),
    self(Name),
    current_reading(State, Reading, NewState),
    (prediction_accurate(Sense, Value, Reading) ->
        log(info, sensor_ca, '~w received accurate prediction ~p about ~w from ~w', [Name, Value, Sense, Source])
        ;
        make_prediction_error(Sense, Value, Reading, PredictionError),
        send_message(Source, PredictionError),
        log(info, sensor_ca, '~w sent prediction error ~p to ~w', [Name, PredictionError, Source])
    ).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, sensor_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(belief_domain), State, BeliefDomain) :-
    get_state(State, belief_domain, BeliefDomain).

handle(query(Query), _, unknown) :-
    log(info, sensor_ca, '~@ is NOT handling query ~p', [self, Query]).

%%%%

% sensor(sensor{capabilities:capabilities{domain:domain{from:0,to:250},sense:distance},id:'ultrasonic-in4',type:ultrasonic,url:'http://127.0.0.1:4000/api/sense/ultrasonic-in4/distance'})])]

% Example [distance-domain{from:0, to:250}]
belief_domain(State, [Sense-Domain]) :-
    sense(State, Sense),
    sense_domain(State, Domain).

subcribe_to_predictions(State, [Topic]) :-
    sense(State, Sense),
    Topic =.. [prediction, Sense],
    subscribe(Topic).

empty_readings(State, [Reading]) :-
    sense(State, Sense),
    get_time(Timestamp),
    Reading = reading(Sense, 'unknown', Timestamp).

sense(State, Sense) :-
    Sense = State.sensor.capabilities.sense.

sense_url(State, SenseURL) :-
    SenseURL = State.sensor.url.

sense_domain(State, SenseDomain) :-
    SenseDomain = State.sensor.capabilities.domain.

last_reading(State, Reading) :-
    [Reading | _] = State.readings.

current_reading(State, Reading, NewState) :-
    last_reading(State, LastReading),
    (reading_stale(LastReading) ->
        sense(State, Sense),
        read_sense(State, Sense, Reading),
        put_state(State, readings, [Reading | state.readings], NewState)
        ;
        Reading = LastReading,
        NewState = State
    ).

reading_stale(reading(_, unknown, _)) :- !.
reading_stale(reading(_, _, Timestamp)) :-
    get_time(Now),
    period(Period),
    age is Now - Timestamp,
    age > Period.

prediction_accurate(Sense, Value, reading(Sense, Value, _)).

read_sense(State, Sense, reading(Sense, Value, Timestamp)) :-
    Url = sense_url(State),
    body:sense_value(Url, Value),
    get_time(Timestamp).

make_prediction_error(Sense, PredictedValue, reading(Sense, ActualValue, _), prediction_error(Sense, PredictedValue, ActualValue)).