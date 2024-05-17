/*
The agent's Society of Mind.

The SOM manages the collective of cognition actors

*/

:- module(som, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(som).

init(Options, State) :-
    log(info, som, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(sensors(Sensors), Options, []),
    option(effectors(Effectors), Options, []),
    put_state(EmptyState, cas, [], State),
    send_message(start(Sensors, Effectors)).

terminate :-
    log(warn, som, 'Terminating').

handle(message(start(Sensors, Effectors), _), State, UpdatedState) :-
    start_sensor_cas(State, Sensors, State1),
    start_effector_cas(State1, Effectors, UpdatedwState).

handle(message(Message, Source), State, State) :-
   log(info, som, 'Handling message ~p from ~w', [Message, Source]).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, som, 'Handling event ~w, with payload ~p from ~w)', [Topic, Payload, Source]).

handle(query(Query), _, tbd) :-
    log(info, som, 'Handling query ~p', [Query]).

%%% Private

start_sensor_cas(State, [], State).
% start_sensor_cas(State, [Sensor | Others], UpdatedState) :-

