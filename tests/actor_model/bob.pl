%% Start/stop shortcuts and callbacks for an worker thread named bob.

:- module(bob, []).

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

signal_processed(control(stopped)) :-
	log(debug, bob, 'Processing stop signal'), worker : stopped.

terminated :-
	log(debug, bob, 'bob terminating'), 
	timer(Timer), 
	timer : stopped(Timer), 
	log(debug, bob, 'bob terminated').

% Ignore all events from self
handled(event(_,_, bob), State, State).

handled(event(party, PartyGoers, _), State, NewState) :-
	log(info, bob, "Ready to party with ~w", [PartyGoers]), 
	put_state(State, mood, excited, NewState), 
	contact_others(PartyGoers), 
	publish(police, "Wassup?").

handled(event(police, Payload, _), State, NewState) :-
	log(info, bob, "Police! ~w", [Payload]), 
	put_state(State, mood, panicking, NewState).

handled(event(tictoc, _, _), State, State) :-
	log(info, bob, "Tictoc!").

handled(query(mood), State, Answer) :-
	get_state(State, mood, Answer), 
	log(info, bob, "Bob is ~p", [Answer]).

handled(query(_), _, "Ugh?").

handled(control(exit), _, _) :-
	throw(
		exit(normal)).

handled(message(Message, Source), State, State) :-
	log(info, bob, "Received ~w from ~w", [Message, Source]).

%% Private

contact_others([]).

contact_others([bob|Others]) :-
	contact_others(Others).

contact_others([Name|Others]) :-
	worker : message_sent(Name, "Bob says howdy!"), 
	contact_others(Others).
