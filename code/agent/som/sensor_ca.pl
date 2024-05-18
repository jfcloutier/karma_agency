/*
A sensor is an a priori cognition actor that communicates with a body sensor to read one semse.
*/

:- module(sensor_ca, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(Sensor, Name) :-
    atomic_list_concat([sensor, Sensor.id, Sensor.capabilities.sense], ':', Name).

init(Options, State) :-
    log(info, sensor_ca, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(device(Sensor), Options),
    put_state(EmptyState, device, Sensor, State),
    send_message(start).

terminate :-
    log(warn, sensor_ca, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, sensor_ca, 'Handling message ~p from ~w', [Message, Source]).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, sensor_ca, 'Handling event ~w, with payload ~p from ~w)', [Topic, Payload, Source]).

handle(query(Query), _, tbd) :-
    log(info, sensor_ca, 'Handling query ~p', [Query]).
