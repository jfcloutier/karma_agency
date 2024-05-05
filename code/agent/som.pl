/*
The agent's Society of Mind.

The SOM manages the collective of cognition actors

*/

:- module(som, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

init([Sensors, Effectors], State) :-
    log(info, som, 'Initializing SOM with sensors ~p and effectors ~p', [Sensors, Effectors]).
    empty_state(EmptyState),
    put_state(EmptyState, sensors, Sensors, State1),
    put_state(State1, effectors, Effectors, State).

terminate :-
    log(warn, som, 'Terminating').

handle(event(Topic, Payload, Source), State, State) :-
    log(info, som, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, som, 'Handling query ~p in state ~p', [Query, State]).