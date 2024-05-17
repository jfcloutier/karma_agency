/*

The feeling of hunger.

*/

:- module(fullness, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(fullness).

init(_, State) :-
    log(info, fullness, 'Initiating'),
    empty_state(State),
    send_message(start).

terminate :-
    log(info, fullness, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, fullness, 'Handling message ~p from ~w in state ~p', [Message, Source, State]).
    
handle(event(Topic, Payload, Source), State, State) :-
    log(info, fullness, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, fullness, 'Handling query ~p in state ~p', [Query, State]).