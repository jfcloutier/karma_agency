%% Supervisor actor logic.
%
%% Starts, restarts and kills actor threads. 

%% A supervisor actor restarts terminated child threads according to option restart(Restart) with which they were started,
%% where Restart is one of permanent (always restart), temporary (never restart) or transient (restart on abnormal exit - the default).
%% The restarted goal options are set to include `restarting(true)`
%%
%% A supervisor can itself be supervised. If a supervisor is started with option `restarting(true)`, 
%% its non-temporary, dynamic children are restarted, in addition to its static children.
%%
%% Explicitly stopping a supervisor effectively kills it and all its children.
%%

/*
[load].
[actor_model(supervisor), code(logger)].
[load_tests].
set_log_level(debug).
run_tests(actor_model:restarting_supervised_supervisors).
*/
:- module(supervisor, []).

:- use_module(library(option)).
:- use_module(actor_utils).
:- use_module(code(logger)).
:- use_module(actor_model(worker)).

% So that children survive the exit of supervisor threads 
% and can be restarted if the supervisor thread is restarted.
% child(StaticOrDynamic, Supervisor, Module, Name, Goal, Restart)
:- dynamic child/6.

% Start a top supervisor thread
start(Supervisor) :-
    start(Supervisor, []).

start(Supervisor, Options) :-
    log(debug, supervisor, "Starting supervisor ~w with options ~p", [Supervisor, Options]),
    start_actor(Supervisor, supervisor:run_unsupervised(Options)),
    option(children(Children), Options, []),
    log(debug, supervisor, 'Waiting for static children of ~w', [Supervisor]),
    wait_for_static_children(Children).

stop(Supervisor) :- 
    log(debug, supervisor, "Stopping supervisor ~w", [Supervisor]),
    send_message(Supervisor, control(stop)),
    wait_for_actor_stopped(Supervisor).

kill(Supervisor) :-
    stop(Supervisor).

%%% Public

% Dynamically start and supervise a singleton worker child
start_worker_child(Supervisor, Module, Options) :-
    singleton_thread_name(Module, Name),
    start_worker_child(Supervisor, worker, Module, Name, Options).

% Dynamically start and supervise a named worker child
start_worker_child(Supervisor, Module, Name, Options) :-
    % A worker is implemented by a module other than worker
    starting_child(Supervisor, worker, Module, Name, Options).

% Dynamically start and supervise a named child (any number with same implementation module)
start_supervisor_child(Supervisor, Name, Options) :-
    % All supervisors are implemented by the supervisor module
    starting_child(Supervisor, supervisor, supervisor, Name, Options).

% Dynamically start pubsub
start_pubsub(Supervisor) :-
    % pubsub is a singleton actor of kind pubsub implemented by the pubsub module
    singleton_thread_name(pubsub, Name),
    starting_child(Supervisor, pubsub, pubsub, Name, []).

% Kill a supervised singleton child actor
kill_child(Supervisor, Kind) :-
    singleton_thread_name(Kind, Name),
    kill_child(Supervisor, Kind, Name).

% Kill a named child actor
kill_child(Supervisor, Kind, Name) :-
    log(debug, supervisor, 'Kill ~w child named ~w of ~w', [Kind, Name, Supervisor]),
    send_message(Supervisor, stop_child(Kind, Name)).

%%% Private 

% Start a static, supervised supervisor thread
start(Supervisor, Options, ParentSupervisor) :-
    log(debug, supervisor, "Starting supervisor ~w under ~w with options ~p", [Supervisor, ParentSupervisor, Options]),
    start_actor(Supervisor, supervisor:run_supervised(Options, ParentSupervisor)),
    option(children(Children), Options, []),
    wait_for_static_children(Children).

singleton_thread_name(Module, Name) :-
    ModuleGoal =.. [:, Module, name(Name)],
    catch(call(ModuleGoal), Exception, (log(debug, supervisor, "Failed to get the singleton thread name of ~w: ~p", [Module, Exception]), fail)).
    
% Starting a supervised named child actor of some kind (supervisor, worker or pubsub) with options and implemented by a given module
starting_child(Supervisor, Kind, Module, Name, Options) :-
    log(debug, supervisor, "Starting supervised ~w ~w named ~w with ~p under ~w", [Kind, Module, Name, Options, Supervisor]),
    % The default restart strategy is transient
    option(restart(Restart), Options, transient),
    Goal =.. [start, Name, Module, Options, Supervisor],
    ChildGoal =.. [:, Kind, Goal],
    send_message(Supervisor, start_child(dynamic, Kind, Name, ChildGoal, Restart)),
    wait_for_actor(Name).

wait_for_static_children([]).
wait_for_static_children([Child | Others]) :-
    child_name(Child, Name), 
    wait_for_actor(Name),
    wait_for_static_children(Others).

child_name(pubsub, Name) :-
    pubsub:name(Name).
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
    catch(run(Options), Exit, process_exit(Exit)).

% Supervisor is run supervised, so it might be restarted on exit.
% Its static childen are started.
% If it is restarting, then its supervised children might also be restarted.
run_supervised(Options, ParentSupervisor) :-
    option(restarting(Restarting), Options, false),
    start_static_children(Options),
    maybe_restart_dynamic_children(Restarting),
    delete(Options, restarting(_), RestOptions),
    catch(run(RestOptions), Exit, process_exit(Exit, ParentSupervisor)).

% If pubsub is one of the chlidren, start it first
start_static_children(Options) :-
    option(children(Children), Options, []),
    (member(pubsub, Children) -> start_static_child(pubsub) ; true),
    forall((member(Child, Children), Child \== pubsub), start_static_child(Child)).

process_exit(Exit) :-
    thread_self(Supervisor),
    log(warn, supervisor, "Exit ~p of unsupervised ~w", [Exit, Supervisor]),
    kill_all_children(Supervisor),
    thread_detach(Supervisor),
    thread_exit(true).

process_exit(Exit, ParentSupervisor) :-
    thread_self(Supervisor),
    log(warn, supervisor, "Exit ~p of supervised ~w", [Exit, Supervisor]),
    kill_all_children(Supervisor),
    thread_detach(Supervisor), 
    notify_supervisor(ParentSupervisor, Supervisor, Exit),
    thread_exit(true).

% Inform supervisor of the exit
notify_supervisor(ParentSupervisor, Supervisor, Exit) :-
    send_message(ParentSupervisor, exited(supervisor, Supervisor, Exit)).

% Options are not used, yet.
run(Options) :-
    thread_get_message(Message),
    process_message(Message, Options),
    !,
    run(Options).

% Process start, stop and exit messages
% Options not used at the moment
process_message(start_child(StaticOrDynamic, Kind, Name, Goal, Restart), _) :-
    log(info, supervisor, "Starting ~w child ~w named ~w by calling ~p", [StaticOrDynamic, Kind, Name, Goal]),
    forget_child(Kind, Name),
    catch(do_start_child(StaticOrDynamic, Kind, Name, Goal, Restart), Exception, log(debug, supervisor, "Failed to start child ~w named ~w: ~p", [Kind, Name, Exception])).

process_message(stop_child(Kind, Name), _) :-
    do_kill_child(Kind, Name).

% A child exited
process_message(exited(Kind, Name, Exit), _) :-
    thread_self(Supervisor),
    child(StaticOrDynamic, Supervisor, Kind, Name, Goal, Restart),
    log(debug, supervisor, 'Child ~w named ~w exited with ~p. Restart is ~w', [Kind,Name,Exit,Restart]),
    !,
    % avoid race condition by letting the exiting thread complete its exit
    sleep(0.5),
    maybe_restart_child(StaticOrDynamic, Supervisor, Kind, Name, Goal, Exit, Restart).

process_message(exited(Kind, Name, Exit), _) :-
    log(debug, supervisor, 'Unsupervised child ~w named ~w (~p)', [Kind, Name, Exit]).

process_message(control(stop), _) :-
    throw(exit(normal)).

% process_message(contro(die)), _) :-
    % thread_self(Supervisor),
    % kill_all_children(Supervisor),
    % thread_detach(Supervisor), 
    % thread_exit(true).

kill_all_children(Supervisor) :-
    log(debug, supervisor, "Killing all children of ~w", [Supervisor]),
    % Kill all except pubsub
    forall((child(_, Supervisor, Kind, Name, _, _), Kind \== pubsub), do_kill_child(Kind, Name)),
    % Kill pubsub child last, if any
    child(_, Supervisor, pubsub, Name, _, _) -> do_kill_child(pubsub, Name) ; true.

start_static_child(pubsub) :-
    thread_self(Supervisor),
    log(debug, supervisor, "Starting supervised pubsub under ~w", [Supervisor]),
    singleton_thread_name(pubsub, Name),
    Goal =.. [start, Supervisor],
    ChildGoal =.. [:, pubsub, Goal],
    do_start_child(static, pubsub, Name, ChildGoal, transient).

start_static_child(worker(Module, Options)) :-
    singleton_thread_name(Module, Name),
    start_static_child(worker(Name, Module, Options)).

start_static_child(worker(Name, Module, Options)) :-
    thread_self(Supervisor),
    log(debug, supervisor, "Starting supervised static worker ~w named ~w with ~p under ~w", [Module, Name, Options, Supervisor]),
    option(restart(Restart), Options, transient),
    Goal =.. [start, Name, Module, Options, Supervisor],
    ChildGoal =.. [:, worker, Goal],
    do_start_child(static, worker, Name, ChildGoal, Restart).

start_static_child(supervisor(Name, Options)) :-
    thread_self(Supervisor),
    log(debug, supervisor, "Starting supervised static supervisor named ~w with ~p under ~w", [Name, Options, Supervisor]),
    option(restart(Restart), Options, transient),
    % All supervisors are implemented by the supervisor module
    Goal =.. [start, Name, supervisor, Options, Supervisor],
    ChildGoal =.. [:, supervisor, Goal],
    do_start_child(static, supervisor, Name, ChildGoal, Restart).

do_start_child(StaticOrDynamic, Kind, Name, Goal, Restart) :-
    call(Goal),
    remember_child(StaticOrDynamic, Kind, Name, Goal, Restart).

do_kill_child(Kind, Name) :-
    log(debug, supervisor, "Killing child ~w named ~w", [Kind, Name]),
    Goal =.. [kill, Name],
    KindGoal =.. [:, Kind, Goal],
    catch(call(KindGoal), Exception, log(debug, supervisor, "Failed to kill child ~w named ~w: ~p", [Kind, Name, Exception])),
    forget_child(Kind, Name).

maybe_restart_dynamic_children(true) :-
    !,
    thread_self(Supervisor),
    log(debug, supervisor, 'Restarting dynamic children of ~w', [Supervisor]),
    forall(
        child(dynamic, Supervisor, Kind, Name, Goal, Restart), 
        maybe_restart_child(dynamic, Supervisor, Kind, Name, Goal, exit(normal), Restart)).
maybe_restart_dynamic_children(_).

maybe_restart_child(_, _, Kind, Name, _, _, temporary) :-
    forget_child(Kind, Name).
maybe_restart_child(_, _, Kind, Name, _, exit(normal), transient) :-
    forget_child(Kind, Name).
maybe_restart_child(StaticOrDynamic, Supervisor, Kind, Name, Goal, _, transient) :-
    child_restarting_goal(Goal, RestartingGoal),
    send_message(Supervisor, start_child(StaticOrDynamic, Kind, Name, RestartingGoal, transient)).
maybe_restart_child(StaticOrDynamic,Supervisor, Kind, Name, Goal, _, permanent) :-
    log(debug, supervisor, 'Getting restarting goal from ~p', [Goal]),
    child_restarting_goal(Goal, RestartingGoal),
    log(debug, supervisor, 'Restarting goal is ~p', [RestartingGoal]),
    send_message(Supervisor, start_child(StaticOrDynamic, Kind, Name, RestartingGoal, permanent)).

% supervisor:child_restarting_goal(worker:start(bob, bob, [topics([party, police]), restart(permanent)], bottom), _660)
child_restarting_goal(ModuleGoal, RestartingModuleGoal) :-
    ModuleGoal =.. [:, Kind, Goal],
    Goal =.. [start, Name, Module, Options, Supervisor],
    merge_options([restarting(true)], Options, RestartingOptions),
    RestartingGoal =.. [start, Name, Module, RestartingOptions, Supervisor],
    RestartingModuleGoal =.. [:, Kind, RestartingGoal].

forget_all_children(Supervisor) :-
    log(debug, supervisor, "Forgetting all children of ~w", [Supervisor]),
    retractall(child(_, Supervisor, _,_,_,_)).

forget_child(Kind, Name) :-
    thread_self(Supervisor),
    log(debug, supervisor, "Forgetting ~w named ~w child of ~w", [Kind, Name, Supervisor]),
    retractall(child(_, Supervisor, Kind, Name, _, _)).

remember_child(StaticOrDynamic, Kind, Name, Goal, Restart) :-
    thread_self(Supervisor),
    Child = child(StaticOrDynamic, Supervisor, Kind, Name, Goal, Restart),
    log(debug, supervisor, "Remembering ~p", [Child]),
    assert(Child).
