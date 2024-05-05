/*

The feeling of pain.

*/

:- module(integrity, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

init(_, State) :-
    log(info, integrity, 'Initiating'),
    empty_state(State).

terminate :-
    log(info, integrity, 'Terminating').
    
handle(event(Topic, Payload, Source), State, State) :-
    log(info, integrity, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, integrity, 'Handling query ~p in state ~p', [Query, State]).