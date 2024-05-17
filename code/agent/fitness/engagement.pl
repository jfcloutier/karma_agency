/*

The feeling of boredom.

*/

:- module(engagement, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(engagement).

init(_, State) :-
    log(info, engagement, 'Initiating'),
    empty_state(State),
    send_message(start).

terminate :-
    log(info, engagement, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, engagement, 'Handling message ~p from ~w in state ~p', [Message, Source, State]).
    
handle(event(Topic, Payload, Source), State, State) :-
    log(info, engagement, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, engagement, 'Handling query ~p in state ~p', [Query, State]).