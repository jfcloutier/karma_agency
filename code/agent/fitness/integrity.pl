/*

The feeling of pain.

*/

:- module(integrity, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(integrity).

init(_, State) :-
    log(info, integrity, 'Initiating'),
    empty_state(State),
    send_message(start).

terminate :-
    log(info, integrity, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, integrity, '~@ is NOT handling message ~p from ~w in state ~p', [self, Message, Source, State]).
    
handle(event(Topic, Payload, Source), State, State) :-
    log(info, integrity, '~@ is NOT handling event event(~w, ~p, ~w) in state ~p', [self, Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, integrity, '~@ is NOT handling query ~p in state ~p', [self, Query, State]).