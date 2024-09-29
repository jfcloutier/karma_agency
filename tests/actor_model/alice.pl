%% Start/stop shortcuts and callbacks for an worker thread named alice.

:- module(alice, []).

:- [load].

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(worker)).
:- use_module(actor_model(pubsub), [publish/2]).

name(alice).

%% Worker callbacks

init(Options, State) :-
	option(
		mood(Mood), Options, bored), 
	log(info, alice, "Initializing with mood ~w", [Mood]), 
	empty_state(EmptyState), 
	put_state(EmptyState, mood, Mood, State).

process_signal(control(stop)) :-
	worker : stop.

terminate :-
	writeln("[alice] Terminating").

handle(control(exit), _, _) :-
	throw(
		exit(normal)).

% Ignore all events from self
handle(event(_,_, alice), State, State).

handle(event(party, PartyGoers, _), State, NewState) :-
	log(info, alice, "Ready to party with ~w", [PartyGoers]), 
	put_state(State, mood, pleased, NewState), 
	contact_others(PartyGoers), 
	publish(police, "Anything wrong, officer?").

handle(event(police, Payload, _), State, NewState) :-
	log(info, alice, "Police! ~w", [Payload]), 
	put_state(State, mood, displeased, NewState).

handle(query(mood), State, Response) :-
	get_state(State, mood, Response), 
	log(info, alice, "Alice is ~p", [Response]).

handle(query(_), _, "Pardon?").

handle(message(Message, Source), State, State) :-
	log(info, alice, "Received ~w from ~w", [Message, Source]).
 
%% Private

contact_others([]).

contact_others([alice|Others]) :-
	contact_others(Others).

contact_others([Name|Others]) :-
	worker : send_message(Name, "Alice says howdy!"), 
	contact_others(Others).

