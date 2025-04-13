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

process_signal(control(stop)) :-
	worker : stop.

terminate :-
	log(info, fullness, 'Terminating').

handle(message(Message, Source), State, State) :-
	log(debug, fullness, '~@ is NOT handling message ~p from ~w in state ~p', [self, Message, Source, State]).

handle(event(Topic, Payload, Source), State, State) :-
	log(debug, fullness, '~@ is NOT handling event event(~w, ~p, ~w) in state ~p', [self, Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
	log(debug, fullness, '~@ is NOT handling query ~p in state ~p', [self, Query, State]).