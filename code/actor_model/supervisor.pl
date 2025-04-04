%% Supervisor actor logic.
%
%% Starts, restarts and stops actor threads. 

%% A supervisor actor restarts terminated child threads according to option restarted(Restart) with which they were started,
%% where Restart is one of permanent (always restart), temporary (never restart) or transient (restart on abnormal exit - the default).
%% The restarted goal options are set to include `restarting(true)` to indicate the context in which the actor is starting.
%%
%% A supervisor can itself be supervised. If a supervisor is started with option `restarting(true)`, 
%% its non-temporary, dynamic children are restarted, in addition to its static children.
%%
%% Explicitly stopping a supervisor effectively stops it and all its children (i.e. no restarting).
%%

/*
[load].
[actor_model(supervisor), code(logger)].
[load_tests].
set_log_level(debug).
run_tests(actor_model:restarting_supervised_supervisors).
*/
:- module(supervisor, []).

:- [load].

:- use_module(library(option)).
:- use_module(actor_utils).
:- use_module(actor_model(worker)).
:- use_module(code(logger)).

% child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Restart) -- Kind is worker or supervisor
:- dynamic child/7.

% So that children survive the exit of supervisor threads 
% and can be restarted if the supervisor thread is restarted.

% Start a top supervisor thread

started(Supervisor) :-
	started(Supervisor, []).

started(Supervisor, Options) :-
	log(debug, supervisor, 'Starting supervisor ~w with options ~p', [Supervisor, Options]),
	actor_started(Supervisor,
		supervisor : run_unsupervised(Options)),
	option(children(Children),
		Options,
		[]),
	log(debug, supervisor, 'Waiting for static children of ~w', [Supervisor]),
	wait_for_static_children(Children).

stopped(Supervisor) :-
	log(debug, supervisor, 'Stopping supervisor ~w', [Supervisor]),
	control_sent(Supervisor, supervisor, stopped),
	actor_stopped(Supervisor).

stopped(Supervisor, WaitSecs) :-
	Timeout is WaitSecs*4,
	log(debug, supervisor, 'Stopping supervisor ~w', [Supervisor]),
	control_sent(Supervisor, supervisor, stopped),
	actor_stopped(Supervisor, Timeout).

exit(Supervisor) :-
	actor_exited(Supervisor).

children(Supervisor, Children) :-
	log(debug, supervisor, 'Getting children of ~w', [Supervisor]),
	query_sent(Supervisor, children, Children).

%%% Public

% Dynamically start and supervise a singleton worker child

worker_child_started(Supervisor, Module, Options) :-
	singleton_thread_name(Module, Name),
	worker_child_started(Supervisor, Module, Name, Options).

% Dynamically start and supervise a named worker child

worker_child_started(Supervisor, Module, Name, Options) :-
	starting_dynamic_child(Supervisor, worker, Module, Name, Options).

% Dynamically start and supervise a named child (any number with same implementation module)

% All supervisors are implemented by the supervisor module

supervisor_child_started(Supervisor, Name, Options) :-
	starting_dynamic_child(Supervisor, supervisor, supervisor, Name, Options).

% Dynamically start pubsub - pubsub is a singleton actor of kind pubsub implemented by the pubsub module

pubsub_started(Supervisor) :-
	singleton_thread_name(pubsub, Name),
	starting_dynamic_child(Supervisor, worker, pubsub, Name, []).

% Stop a supervised singleton child actor

child_stopped(Supervisor, Kind, Module) :-
	singleton_thread_name(Module, Name),
	child_stopped(Supervisor, Kind, Module, Name).

% Stop a named child actor

child_stopped(Supervisor, Kind, Module, Name) :-
	log(debug, supervisor, 'Stop ~w child ~w named ~w of ~w', [Kind, Module, Name, Supervisor]),
	sent(Supervisor,
		child_stopped(Kind, Module, Name)).

%%% Private 

% Start a static, supervised supervisor thread

started(Supervisor, Options, ParentSupervisor) :-
	log(debug, supervisor, 'Starting supervisor ~w under ~w with options ~p', [Supervisor, ParentSupervisor, Options]),
	actor_started(Supervisor,
		supervisor : run_supervised(Options, ParentSupervisor)),
	option(children(Children),
		Options,
		[]),
	wait_for_static_children(Children).

singleton_thread_name(Module, Name) :-
	ModuleGoal =.. [ : , Module, name(Name)],
	catch(call(ModuleGoal),
		Exception,
		(log(debug, supervisor, 'Failed to get the singleton thread name of ~w: ~p', [Module, Exception]),
			fail)).
    
% Starting a dynamic, supervised named child actor of some kind (supervisor, worker or pubsub) with options and implemented by a given module

starting_dynamic_child(Supervisor, Kind, Module, Name, Options) :-
	log(debug, supervisor, 'Starting dynamic supervised ~w ~w named ~w with ~p under ~w', [Kind, Module, Name, Options, Supervisor]),
	% The default restart strategy is transient
	option(restarted(Restart),
		Options,
		transient),
	Goal =.. [started, Name, Module, Options, Supervisor],
	ChildGoal =.. [ : , Kind, Goal],
	sent(Supervisor,
		child_started(dynamic, Kind, Module, Name, ChildGoal, Restart)),
	actor_ready(Name).

wait_for_static_children([]).

wait_for_static_children([Child|Others]) :-
	child_name(Child, Name),
	actor_ready(Name),
	wait_for_static_children(Others).

child_name(pubsub, Name) :-
	pubsub : name(Name).

child_name(worker(Name, _, _), Name).

child_name(worker(Module, _), Name) :-
	singleton_thread_name(Module, Name).

child_name(supervisor(Name, _), Name).

% Called when starting as a child of a parent supervisor

started(Name, supervisor, Options, Supervisor) :-
	started(Name, Options, Supervisor).

%%% In thread

% Supervisor is run unsupervised; it will not be restarted on exit.
% Its static childen are started.

run_unsupervised(Options) :-
	static_children_started(Options),
	log(debug, supervisor, 'Started static children ~p', [Options]),
	catch(run(Options),
		Exit,
		exit_processed(Exit)).

% Supervisor is run supervised, so it might be restarted on exit.
% Its static childen are started.
% If it is restarting, then its supervised children might also be restarted.

run_supervised(Options, ParentSupervisor) :-
	option(restarting(Restarting),
		Options,
		false),
	static_children_started(Options),
	dynamic_children_maybe_restarted(Restarting),
	delete(Options,
		restarting(_),
		RestOptions),
	catch(run(RestOptions),
		Exit,
		exit_processed(Exit, ParentSupervisor)).

% If pubsub is one of the chlidren, start it first

static_children_started(Options) :-
	option(children(Children),
		Options,
		[]),
	log(debug, supervisor, 'Starting static children ~p', [Children]),
	(member(pubsub, Children) ->
	static_child_started(pubsub);
	true),
	forall((member(Child, Children),
			Child \== pubsub),
		static_child_started(Child)).

exit_processed(Exit) :-
	thread_self(Supervisor),
	log(warn, supervisor, 'Exit ~p of unsupervised ~w', [Exit, Supervisor]),
	all_children_stopped(Supervisor),
	log(warn, supervisor, 'Detaching and exiting thread ~w', [Supervisor]),
	thread_detach(Supervisor),
	thread_exit(true).

exit_processed(Exit, ParentSupervisor) :-
	thread_self(Supervisor),
	log(warn, supervisor, 'Exit ~p of supervised ~w', [Exit, Supervisor]),
	all_children_stopped(Supervisor),
	thread_detach(Supervisor),
	notify_supervisor(ParentSupervisor, Supervisor, Exit),
	thread_detach(Supervisor),
	thread_exit(true).

% Inform supervisor of the exit

notify_supervisor(ParentSupervisor, Supervisor, Exit) :-
	sent(ParentSupervisor,
		exited(supervisor, supervisor, Supervisor, Exit)).

% Options are not used, yet.

run(Options) :-
	thread_get_message(Message),
	message_processed(Message, Options),
	!,
	run(Options).

% Stop and don't inform the supervisor's supervisor, if any

signal_processed(control(stopped)) :-
	exit_processed(normal).

% Process start, stop and exit messages
% Options not used at the moment

message_processed(child_started(StaticOrDynamic, Kind, Module, Name, Goal, Restart), _) :-
	log(info, supervisor, 'Starting ~w ~w child ~w named ~w by calling ~p', [StaticOrDynamic, Kind, Module, Name, Goal]),
	child_forgotten(Kind, Module, Name),
	catch(child_running(StaticOrDynamic, Kind, Module, Name, Goal, Restart),
		Exception,
		log(debug, supervisor, 'Failed to start child ~w named ~w: ~p', [Kind, Name, Exception])).

message_processed(child_stopped(Kind, Module, Name), _) :-
	do_child_stopped(Kind, Module, Name).

message_processed(control(exit), _) :-
	log(debug, supervisor, 'Received control(exit)'),
	throw(exit(normal)).

% A child exited

message_processed(exited(Kind, Module, Name, Exit), _) :-
	thread_self(Supervisor),
	child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Restart),
	log(debug, supervisor, 'Child ~w ~w named ~w exited with ~p. Restart is ~w', [Kind, Module, Name, Exit, Restart]),
	!,
	% avoid race condition by letting the exiting thread complete its exit
	sleep(0.5),
	child_maybe_restarted(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Exit, Restart).

message_processed(exited(Kind, Module, Name, Exit), _) :-
	log(debug, supervisor, 'Unsupervised ~w child ~w named ~w exited (~p)', [Kind, Module, Name, Exit]).

% message_processed(control(die)), _) :-
    % thread_self(Supervisor),
    % all_children_stopped(Supervisor),
    % thread_detach(Supervisor), 
    % thread_exit(true).

message_processed(query(children, From), _) :-
	thread_self(Supervisor),
	log(debug, supervisor, '~w processing query(children, ~w)', [Supervisor, From]),
	findall(child(Kind, Name),
		child(_, Supervisor, Kind, _, Name, _, _),
		Children),
	sent(From,
		response(Children, children, Supervisor)).

all_children_stopped(Supervisor) :-
	log(debug, supervisor, 'Stopping all children of ~w', [Supervisor]),
	forall((child(_, Supervisor, Kind, Module, Name, _, _)),
		do_child_stopped(Kind, Module, Name)).

static_child_started(pubsub) :-
	thread_self(Supervisor),
	log(debug, supervisor, 'Starting static supervised pubsub under ~w', [Supervisor]),
	singleton_thread_name(pubsub, Name),
	Goal =.. [
		started,
		Name,
		pubsub,
		[],
		Supervisor],
	ChildGoal =.. [ : , worker, Goal],
	child_running(static, worker, pubsub, Name, ChildGoal, transient).

static_child_started(worker(Module, Options)) :-
	log(debug, supervisor, 'Starting singleton ~w static child with ~p', [Module, Options]),
	singleton_thread_name(Module, Name),
	static_child_started(worker(Name, Module, Options)).

static_child_started(worker(Name, Module, Options)) :-
	thread_self(Supervisor),
	log(debug, supervisor, '~w starting supervised static worker ~w named ~w with ~p under ~w', [Supervisor, Module, Name, Options, Supervisor]),
	option(restarted(Restart),
		Options,
		transient),
	Goal =.. [started, Name, Module, Options, Supervisor],
	ChildGoal =.. [ : , worker, Goal],
	child_running(static, worker, Module, Name, ChildGoal, Restart).

static_child_started(supervisor(Name, Options)) :-
	thread_self(Supervisor),
	log(debug, supervisor, 'Starting supervised static supervisor named ~w with ~p under ~w', [Name, Options, Supervisor]),
	option(restarted(Restart),
		Options,
		transient),
	% All supervisors are implemented by the supervisor module
	Goal =.. [started, Name, supervisor, Options, Supervisor],
	ChildGoal =.. [ : , supervisor, Goal],
	child_running(static, supervisor, supervisor, Name, ChildGoal, Restart).

child_running(StaticOrDynamic, Kind, Module, Name, Goal, Restart) :-
	call(Goal),
	child_remembered(StaticOrDynamic, Kind, Module, Name, Goal, Restart).

do_child_stopped(Kind, Module, Name) :-
	log(debug, supervisor, 'Stopping ~w child ~w named ~w', [Kind, Module, Name]),
	control_sent(Name, Module, stopped),
	actor_stopped(Name),
	child_forgotten(Kind, Module, Name).

dynamic_children_maybe_restarted(true) :-
	!,
	thread_self(Supervisor),
	log(debug, supervisor, 'Restarting dynamic children of ~w', [Supervisor]),
	forall(child(dynamic, Supervisor, Kind, Module, Name, Goal, Restart),
		child_maybe_restarted(dynamic,
			Supervisor,
			Kind,
			Module,
			Name,
			Goal,
			exit(normal),
			Restart)).

dynamic_children_maybe_restarted(_).

child_maybe_restarted(_, _, Kind, Module, Name, _, _, temporary) :-
	child_forgotten(Kind, Module, Name).

child_maybe_restarted(_, _, Kind, Module, Name, _, exit(normal), transient) :-
	child_forgotten(Kind, Module, Name).

child_maybe_restarted(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, _, transient) :-
	child_restarting_goal(Goal, RestartingGoal),
	sent(Supervisor,
		child_started(StaticOrDynamic, Kind, Module, Name, RestartingGoal, transient)).

child_maybe_restarted(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, _, permanent) :-
	log(debug, supervisor, 'Getting restarting goal from ~p', [Goal]),
	child_restarting_goal(Goal, RestartingGoal),
	log(debug, supervisor, 'Restarting goal is ~p', [RestartingGoal]),
	sent(Supervisor,
		child_started(StaticOrDynamic, Kind, Module, Name, RestartingGoal, permanent)).

% supervisor:child_restarting_goal(worker:started(bob, bob, [topics([party, police]), restarted(permanent)], bottom), _660)

child_restarting_goal(ModuleGoal, RestartingModuleGoal) :-
	ModuleGoal =.. [ : , Kind, Goal],
	Goal =.. [started, Name, Module, Options, Supervisor],
	merge_options([restarting(true)],
		Options,
		RestartingOptions),
	RestartingGoal =.. [started, Name, Module, RestartingOptions, Supervisor],
	RestartingModuleGoal =.. [ : , Kind, RestartingGoal].

all_children_forgotten(Supervisor) :-
	log(debug, supervisor, 'Forgetting all children of ~w', [Supervisor]),
	retractall(child(_, Supervisor, _, _, _, _)).

child_forgotten(Kind, Module, Name) :-
	thread_self(Supervisor),
	log(debug, supervisor, 'Forgetting ~w ~w named ~w child of ~w', [Kind, Module, Name, Supervisor]),
	retractall(child(_, Supervisor, Kind, Module, Name, _, _)).


child_remembered(StaticOrDynamic, Kind, Module, Name, Goal, Restart) :-
	thread_self(Supervisor),
	Child = child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Restart),
	log(debug, supervisor, 'Remembering ~p', [Child]),
	assert(Child).
