%% Supervisor actor logic.
%
%% Starts, restarts and stops actor threads. 

%% A supervisor actor restarts terminated child threads according to option restart(Restart) with which they were started,
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

start(Supervisor) :-
	start(Supervisor, []).

start(Supervisor, Options) :-
	log(debug, supervisor, 'Starting supervisor ~w with options ~p', [Supervisor, Options]),
	start_actor(Supervisor,
		supervisor : run_unsupervised(Options)),
	option(children(Children),
		Options,
		[]),
	log(debug, supervisor, 'Waiting for static children of ~w', [Supervisor]),
	wait_for_static_children(Children).

stop(Supervisor) :-
	log(debug, supervisor, 'Stopping supervisor ~w', [Supervisor]),
	send_control(Supervisor, supervisor, stop),
	wait_for_actor_stopped(Supervisor).

stop(Supervisor, WaitSecs) :-
	Timeout is WaitSecs*4,
	log(debug, supervisor, 'Stopping supervisor ~w', [Supervisor]),
	send_control(Supervisor, supervisor, stop),
	wait_for_actor_stopped(Supervisor, Timeout).

exit(Supervisor) :-
	exit_actor(Supervisor).

children(Supervisor, Children) :-
	log(debug, supervisor, 'Getting children of ~w', [Supervisor]),
	send_query(Supervisor, children, Children).

%%% Public

% Dynamically start and supervise a singleton worker child

start_worker_child(Supervisor, Module, Options) :-
	singleton_thread_name(Module, Name),
	start_worker_child(Supervisor, Module, Name, Options).

% Dynamically start and supervise a named worker child

start_worker_child(Supervisor, Module, Name, Options) :-
	starting_dynamic_child(Supervisor, worker, Module, Name, Options).

% Dynamically start and supervise a named child (any number with same implementation module)

% All supervisors are implemented by the supervisor module

start_supervisor_child(Supervisor, Name, Options) :-
	starting_dynamic_child(Supervisor, supervisor, supervisor, Name, Options).

% Dynamically start pubsub - pubsub is a singleton actor of kind pubsub implemented by the pubsub module

start_pubsub(Supervisor) :-
	singleton_thread_name(pubsub, Name),
	starting_dynamic_child(Supervisor, worker, pubsub, Name, []).

% Stop a supervised singleton child actor

stop_child(Supervisor, Kind, Module) :-
	singleton_thread_name(Module, Name),
	stop_child(Supervisor, Kind, Module, Name).

% Stop a named child actor

stop_child(Supervisor, Kind, Module, Name) :-
	log(debug, supervisor, 'Stop ~w child ~w named ~w of ~w', [Kind, Module, Name, Supervisor]),
	send(Supervisor,
		stop_child(Kind, Module, Name)).

%%% Private 

% Start a static, supervised supervisor thread

start(Supervisor, Options, ParentSupervisor) :-
	log(debug, supervisor, 'Starting supervisor ~w under ~w with options ~p', [Supervisor, ParentSupervisor, Options]),
	start_actor(Supervisor,
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
	option(restart(Restart),
		Options,
		transient),
	Goal =.. [start, Name, Module, Options, Supervisor],
	ChildGoal =.. [ : , Kind, Goal],
	send(Supervisor,
		start_child(dynamic, Kind, Module, Name, ChildGoal, Restart)),
	wait_for_actor(Name).

wait_for_static_children([]).

wait_for_static_children([Child|Others]) :-
	child_name(Child, Name),
	wait_for_actor(Name),
	wait_for_static_children(Others).

child_name(pubsub, Name) :-
	pubsub : name(Name).

child_name(worker(Name, _, _), Name).

child_name(worker(Module, _), Name) :-
	singleton_thread_name(Module, Name).

child_name(supervisor(Name, _), Name).

% Called when starting as a child of a parent supervisor

start(Name, supervisor, Options, Supervisor) :-
	start(Name, Options, Supervisor).

%%% In thread

% Supervisor is run unsupervised; it will not be restarted on exit.
% Its static childen are started.

run_unsupervised(Options) :-
	start_static_children(Options),
	log(debug, supervisor, 'Started static children ~p', [Options]),
	catch(run(Options),
		Exit,
		process_exit(Exit)).

% Supervisor is run supervised, so it might be restarted on exit.
% Its static childen are started.
% If it is restarting, then its supervised children might also be restarted.

run_supervised(Options, ParentSupervisor) :-
	option(restarting(Restarting),
		Options,
		false),
	start_static_children(Options),
	maybe_restart_dynamic_children(Restarting),
	delete(Options,
		restarting(_),
		RestOptions),
	catch(run(RestOptions),
		Exit,
		process_exit(Exit, ParentSupervisor)).

% If pubsub is one of the chlidren, start it first

start_static_children(Options) :-
	option(children(Children),
		Options,
		[]),
	log(debug, supervisor, 'Starting static children ~p', [Children]),
	(member(pubsub, Children) ->
	start_static_child(pubsub);
	true),
	forall((member(Child, Children),
			Child \== pubsub),
		start_static_child(Child)).

process_exit(Exit) :-
	thread_self(Supervisor),
	log(warn, supervisor, 'Exit ~p of unsupervised ~w', [Exit, Supervisor]),
	stop_all_children(Supervisor),
	log(warn, supervisor, 'Detaching and exiting thread ~w', [Supervisor]),
	thread_detach(Supervisor),
	thread_exit(true).

process_exit(Exit, ParentSupervisor) :-
	thread_self(Supervisor),
	log(warn, supervisor, 'Exit ~p of supervised ~w', [Exit, Supervisor]),
	stop_all_children(Supervisor),
	thread_detach(Supervisor),
	notify_supervisor(ParentSupervisor, Supervisor, Exit),
	thread_detach(Supervisor),
	thread_exit(true).

% Inform supervisor of the exit

notify_supervisor(ParentSupervisor, Supervisor, Exit) :-
	send(ParentSupervisor,
		exited(supervisor, supervisor, Supervisor, Exit)).

% Options are not used, yet.

run(Options) :-
	thread_get_message(Message),
	process_message(Message, Options),
	!,
	run(Options).

% Stop and don't inform the supervisor's supervisor, if any

process_signal(control(stop)) :-
	process_exit(normal).

% Process start, stop and exit messages
% Options not used at the moment

process_message(start_child(StaticOrDynamic, Kind, Module, Name, Goal, Restart), _) :-
	log(info, supervisor, 'Starting ~w ~w child ~w named ~w by calling ~p', [StaticOrDynamic, Kind, Module, Name, Goal]),
	forget_child(Kind, Module, Name),
	catch(do_start_child(StaticOrDynamic, Kind, Module, Name, Goal, Restart),
		Exception,
		log(debug, supervisor, 'Failed to start child ~w named ~w: ~p', [Kind, Name, Exception])).

process_message(stop_child(Kind, Module, Name), _) :-
	do_stop_child(Kind, Module, Name).

process_message(control(exit), _) :-
	log(debug, supervisor, 'Received control(exit)'),
	throw(exit(normal)).

% A child exited

process_message(exited(Kind, Module, Name, Exit), _) :-
	thread_self(Supervisor),
	child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Restart),
	log(debug, supervisor, 'Child ~w ~w named ~w exited with ~p. Restart is ~w', [Kind, Module, Name, Exit, Restart]),
	!,
	% avoid race condition by letting the exiting thread complete its exit
	sleep(0.5),
	maybe_restart_child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Exit, Restart).

process_message(exited(Kind, Module, Name, Exit), _) :-
	log(debug, supervisor, 'Unsupervised ~w child ~w named ~w exited (~p)', [Kind, Module, Name, Exit]).

% process_message(control(die)), _) :-
    % thread_self(Supervisor),
    % stop_all_children(Supervisor),
    % thread_detach(Supervisor), 
    % thread_exit(true).

process_message(query(children, From), _) :-
	thread_self(Supervisor),
	log(debug, supervisor, '~w processing query(children, ~w)', [Supervisor, From]),
	findall(child(Kind, Name),
		child(_, Supervisor, Kind, _, Name, _, _),
		Children),
	send(From,
		response(Children, children, Supervisor)).

stop_all_children(Supervisor) :-
	log(debug, supervisor, 'Stopping all children of ~w', [Supervisor]),
	forall((child(_, Supervisor, Kind, Module, Name, _, _)),
		do_stop_child(Kind, Module, Name)).

start_static_child(pubsub) :-
	thread_self(Supervisor),
	log(debug, supervisor, 'Starting static supervised pubsub under ~w', [Supervisor]),
	singleton_thread_name(pubsub, Name),
	Goal =.. [
		start,
		Name,
		pubsub,
		[],
		Supervisor],
	ChildGoal =.. [ : , worker, Goal],
	do_start_child(static, worker, pubsub, Name, ChildGoal, transient).

start_static_child(worker(Module, Options)) :-
	log(debug, supervisor, 'Starting singleton ~w static child with ~p', [Module, Options]),
	singleton_thread_name(Module, Name),
	start_static_child(worker(Name, Module, Options)).

start_static_child(worker(Name, Module, Options)) :-
	thread_self(Supervisor),
	log(debug, supervisor, '~w starting supervised static worker ~w named ~w with ~p under ~w', [Supervisor, Module, Name, Options, Supervisor]),
	option(restart(Restart),
		Options,
		transient),
	Goal =.. [start, Name, Module, Options, Supervisor],
	ChildGoal =.. [ : , worker, Goal],
	do_start_child(static, worker, Module, Name, ChildGoal, Restart).

start_static_child(supervisor(Name, Options)) :-
	thread_self(Supervisor),
	log(debug, supervisor, 'Starting supervised static supervisor named ~w with ~p under ~w', [Name, Options, Supervisor]),
	option(restart(Restart),
		Options,
		transient),
	% All supervisors are implemented by the supervisor module
	Goal =.. [start, Name, supervisor, Options, Supervisor],
	ChildGoal =.. [ : , supervisor, Goal],
	do_start_child(static, supervisor, supervisor, Name, ChildGoal, Restart).

do_start_child(StaticOrDynamic, Kind, Module, Name, Goal, Restart) :-
	call(Goal),
	remember_child(StaticOrDynamic, Kind, Module, Name, Goal, Restart).

do_stop_child(Kind, Module, Name) :-
	log(debug, supervisor, 'Stopping ~w child ~w named ~w', [Kind, Module, Name]),
	send_control(Name, Module, stop),
	wait_for_actor_stopped(Name),
	forget_child(Kind, Module, Name).

maybe_restart_dynamic_children(true) :-
	!,
	thread_self(Supervisor),
	log(debug, supervisor, 'Restarting dynamic children of ~w', [Supervisor]),
	forall(child(dynamic, Supervisor, Kind, Module, Name, Goal, Restart),
		maybe_restart_child(dynamic,
			Supervisor,
			Kind,
			Module,
			Name,
			Goal,
			exit(normal),
			Restart)).

maybe_restart_dynamic_children(_).

maybe_restart_child(_, _, Kind, Module, Name, _, _, temporary) :-
	forget_child(Kind, Module, Name).

maybe_restart_child(_, _, Kind, Module, Name, _, exit(normal), transient) :-
	forget_child(Kind, Module, Name).

maybe_restart_child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, _, transient) :-
	child_restarting_goal(Goal, RestartingGoal),
	send(Supervisor,
		start_child(StaticOrDynamic, Kind, Module, Name, RestartingGoal, transient)).

maybe_restart_child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, _, permanent) :-
	log(debug, supervisor, 'Getting restarting goal from ~p', [Goal]),
	child_restarting_goal(Goal, RestartingGoal),
	log(debug, supervisor, 'Restarting goal is ~p', [RestartingGoal]),
	send(Supervisor,
		start_child(StaticOrDynamic, Kind, Module, Name, RestartingGoal, permanent)).

% supervisor:child_restarting_goal(worker:start(bob, bob, [topics([party, police]), restart(permanent)], bottom), _660)

child_restarting_goal(ModuleGoal, RestartingModuleGoal) :-
	ModuleGoal =.. [ : , Kind, Goal],
	Goal =.. [start, Name, Module, Options, Supervisor],
	merge_options([restarting(true)],
		Options,
		RestartingOptions),
	RestartingGoal =.. [start, Name, Module, RestartingOptions, Supervisor],
	RestartingModuleGoal =.. [ : , Kind, RestartingGoal].

forget_all_children(Supervisor) :-
	log(debug, supervisor, 'Forgetting all children of ~w', [Supervisor]),
	retractall(child(_, Supervisor, _, _, _, _)).

forget_child(Kind, Module, Name) :-
	thread_self(Supervisor),
	log(debug, supervisor, 'Forgetting ~w ~w named ~w child of ~w', [Kind, Module, Name, Supervisor]),
	retractall(child(_, Supervisor, Kind, Module, Name, _, _)).


remember_child(StaticOrDynamic, Kind, Module, Name, Goal, Restart) :-
	thread_self(Supervisor),
	Child = child(StaticOrDynamic, Supervisor, Kind, Module, Name, Goal, Restart),
	log(debug, supervisor, 'Remembering ~p', [Child]),
	assert(Child).
