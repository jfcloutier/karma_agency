/*

The feeling of pain.

*/

:- module(integrity, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(worker)).

name(integrity).

init(_, State) :-
	log(info, integrity, 'Initiating'), 
	empty_state(State), 
	send_message(start).

process_signal(control(stop)) :-
	worker : stop.

terminate :-
	log(info, integrity, 'Terminating').

handle(message(Message, Source), State, State) :-
	log(debug, integrity, '~@ is NOT handling message ~p from ~w in state ~p', [self, Message, Source, State]).

handle(event(Topic, Payload, Source), State, State) :-
	log(debug, integrity, '~@ is NOT handling event event(~w, ~p, ~w) in state ~p', [self, Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
	log(debug, integrity, '~@ is NOT handling query ~p in state ~p', [self, Query, State]).

handle(terminating, _).