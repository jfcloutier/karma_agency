%% Start/stop shortcuts and callbacks for an worker thread named bob.

:- module(bob, []).

:- [load].

:- use_module(actor_model(worker)).
:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(timer)).
:- use_module(actor_model(pubsub), [publish/2]).

:- thread_local timer/1.

%% Worker callbacks

name(bob).

init(Options, State) :-
	option(
		mood(Mood), Options, bored), 
	log(info, bob, "Initializing with mood ~w", [Mood]), 
	empty_state(EmptyState), 
	self(Name), 
	send_at_interval(Name, clock, 
		event(tictoc, true, Name), 1, Timer), 
	assert(
		timer(Timer)), 
	put_state(EmptyState, [mood - Mood], State).

process_signal(control(stop)) :-
	log(debug, bob, 'Processing stop signal'), worker : stop.

terminate :-
	log(debug, bob, 'bob terminating'), 
	timer(Timer), 
	timer : stop(Timer), 
	log(debug, bob, 'bob terminated').

% Ignore all events from self
handle(event(_,_, bob), State, State).

handle(event(party, PartyGoers, _), State, NewState) :-
	log(info, bob, "Ready to party with ~w", [PartyGoers]), 
	put_state(State, mood, excited, NewState), 
	contact_others(PartyGoers), 
	publish(police, "Wassup?").

handle(event(police, Payload, _), State, NewState) :-
	log(info, bob, "Police! ~w", [Payload]), 
	put_state(State, mood, panicking, NewState).

handle(event(tictoc, _, _), State, State) :-
	log(info, bob, "Tictoc!").

handle(query(mood), State, Answer) :-
	get_state(State, mood, Answer), 
	log(info, bob, "Bob is ~p", [Answer]).

handle(query(_), _, "Ugh?").

handle(control(exit), _, _) :-
	throw(
		exit(normal)).

handle(message(Message, Source), State, State) :-
	log(info, bob, "Received ~w from ~w", [Message, Source]).

%% Private

contact_others([]).

contact_others([bob|Others]) :-
	contact_others(Others).

contact_others([Name|Others]) :-
	worker : send_message(Name, "Bob says howdy!"), 
	contact_others(Others).
