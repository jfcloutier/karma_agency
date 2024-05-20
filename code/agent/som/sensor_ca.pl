/*
A sensor is an a priori cognition actor that communicates with a body sensor to read one sense.
Each sensor CA has one belief, about the sense value it measures.
It listens to prediction events about its belief and may emit a prediction error event,
based on the latest reading, if not too old, else given a reading it presently makes.
*/

:- module(sensor_ca, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(Sensor, Name) :-
    atomic_list_concat([sensor, Sensor.id, Sensor.capabilities.sense], ':', Name).

init(Options, State) :-
    log(info, sensor_ca, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(sensor(Sensor), Options),
    put_state(EmptyState, sensor, Sensor, State),
    send_message(start).

terminate :-
    log(warn, sensor_ca, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, sensor_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, sensor_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(Query), _, tbd) :-
    log(info, sensor_ca, '~@ is NOT handling query ~p', [self, Query]).
