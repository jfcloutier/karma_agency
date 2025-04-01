%%%
% Worker actor logic.
%
% A worker thread subscribes to and handles broadcast events, and
% sends/receives (semantic) messages, queries and control directives.
% 
% A worker is supervised and thus implements start/3 and terminate/0.
%
% A worker is started by giving it a unique name and options.
% Options:
%   topics - the list of topics for the events the worker wants to receive (defaults to [])
%
% Callbacks:
%   handle/3 - for handling events, semantic messages and queries
%   init/1 - sets the initial state
%
% A worker holds a state that is updated from processing messages.
%%%
:- module(worker, []).

:- [load].

:- use_module(library(option)).
:- use_module(actor_utils).
:- use_module(pubsub).
:- use_module(code(logger)).

%%% Supervised behavior

start(Name, Module, Options, Supervisor) :-
	log(debug, worker, 'Starting ~w implemented by ~w with options ~p supervised by ~w', [Name, Module, Options, Supervisor]), 
	option(
		topics(Topics), Options, []), 
	option(init(Args), Options, []),
	Handler =.. [ : , Module, handle], 
	Init =.. [ : , Module, init, Args], 
	Terminate =.. [ : , Module, terminate], 
	log(debug, worker, "Creating worker ~w", [Name]), 
	actor_started(Name, 
		worker : worker_started(Module, Name, Topics, Init, Handler, Supervisor), 
		[at_exit(Terminate)]).

stop :-
	self(Name), 
	log(debug, worker, "Exiting ~w", [Name]), 
	(
		pubsub : name(Name) ->
			true;
			unsubscribe_all), 
	thread_detach(Name), 
	thread_exit(true).

%%% Private - in thread

worker_started(Module, Name, Topics, Init, Handler, Supervisor) :-
	Init =.. [ : , Module, _, _], 
	catch(
		run_started(Topics, Init, Handler), Exit, 
		process_exit(Module, Name, Exit, Supervisor)).

process_exit(Module, Name, Exit, Supervisor) :-
	log(warn, worker, "Exit ~p of ~w", [Exit, Name]), unsubscribe_all, 
	thread_detach(Name), 
	notify_supervisor(Supervisor, Module, Name, Exit), 
	thread_exit(true).

run_started(Topics, Init, Handler) :-
	log(debug, worker, 'Start run of ~@ with topics ~p, init ~p and handler ~p', [self, Topics, Init, Handler]),
	Init =.. [ : , Module, Head, Args], 
	Goal =.. [Head, Args, State], 
	InitGoal =.. [ : , Module, Goal],
	call(InitGoal), 
	subscribe_all(Topics), 
	run(Handler, State).

run(Handler, State) :-
	thread_self(Name), 
	log(debug, worker, 'Worker ~w is waiting...', [Name]), 
	thread_get_message(Message), 
	process_message(Message, Handler, State, NewState), 
	run(Handler, NewState).

process_signal(control(stop)) :-
	thread_self(Name), 
	thread_detach(Name), 
	thread_exit(true).

process_signal(Signal) :-
	log(info, worker, '~@ received unknown signal ~p', [self, Signal]).

% Write out the current state of the worker

process_message(state, _, State, State) :-
	writeln(State).

process_message(unsubscribe(Topic), _, _, _) :-
	unsubscribe(Topic).

process_message(query(Question, From), Handler, State, State) :-
	Handler =.. [ : , Module, Head], 
	Goal =.. [Head, query(Question), State, Answer], 
	ModuleGoal =.. [ : , Module, Goal], 
	thread_self(Name), 
	log(debug, worker, "~w got query ~p from ~w", [Name, Question, From]), 
	!,
	call(ModuleGoal), 
	send(From, response(Answer, Question, Name)).

process_message(Message, Handler, State, NewState) :-
	Handler =.. [ : , Module, Head], 
	Goal =.. [Head, Message, State, NewState], 
	ModuleGoal =.. [ : , Module, Goal], 
	thread_self(Name), 
	log(debug, worker, "~w received ~p", [Name, Message]), 
	call(ModuleGoal).

% Inform supervisor of the exit

notify_supervisor(Supervisor, Module, Name, Exit) :-
	send(Supervisor, 
		exited(worker, Module, Name, Exit)).
