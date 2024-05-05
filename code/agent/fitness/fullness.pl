/*

The feeling of hunger.

*/

:- module(fullness, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

init(_, State) :-
    log(info, fullness, 'Initiating'),
    empty_state(State).

terminate :-
    log(info, fullness, 'Terminating').
    
handle(event(Topic, Payload, Source), State, State) :-
    log(info, fullness, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, fullness, 'Handling query ~p in state ~p', [Query, State]).