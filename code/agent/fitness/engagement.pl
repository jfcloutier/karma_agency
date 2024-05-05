/*

The feeling of boredom.

*/

:- module(engagement, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

init(_, State) :-
    log(info, engagement, 'Initiating'),
    empty_state(State).

terminate :-
    log(info, engagement, 'Terminating').
    
handle(event(Topic, Payload, Source), State, State) :-
    log(info, engagement, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, engagement, 'Handling query ~p in state ~p', [Query, State]).