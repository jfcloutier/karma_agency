/*
Cognition Actor support library.
*/

:- module(ca_support, [from_parent/2]).

:- use_module(actor_model(actor_utils)).
:- use_module(utils(logger)).

handled(message(Message, Source), State, State) :-
	log(debug, ca_support, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handled(query(Query), _, unknown) :-
    log(debug, ca_support, '~@ is NOT handling query ~p', [self, Query]).

% Remove from parents if applicable
handled(event(ca_terminated, _, Source), State, NewState) :-
    get_state(State, parents, Parents),
    member(Source, Parents),
    subtract(Parents, [Source], Parents1),
    put_state(State, parents, Parents1, NewState).

handled(event(Topic, Payload, Source), State, State) :-
    log(debug, ca_support, '~@ is NOT handling event ~p with ~p from ~w', [self, Topic, Payload, Source]).
    
from_parent(Source, State) :-
    get_state(State, parents, Parents),
    member(Source, Parents).
    