%% Supervisor actor logic.
%
%% Starts, restarts and kills actor threads. 

%% A supervisor actor restarts terminated child threads according to option restart(Restart) with which they were started,
%% where Restart is one of permanent (always restart), temporary (never restart) or transient (restart on abnormal exit - the default).
%% The restarted goal options are set to include `restarting(true)`
%%
%% A supervisor can itself be supervised. If a supervisor is started with option `restarting(true)`, 
%% it's *permanent* children (and only them) are restarted.
%%
%% Explicitly stopping a supervisor effectively kills it and all its children.
%%
:- module(supervisor, []).

:- use_module(library(option)).
:- use_module(actor_utils).
:- use_module(code(logger)).

% Dynamic so that children survive the exit of supervisor threads and can be restarted if the supervisor thread is restarted.
:- dynamic child/5.

% Start a top supervisor thread
start(Supervisor) :-
    start(Supervisor, []).

start(Supervisor, Options) :-
    log(debug, supervisor, "[supervisor] Starting supervisor ~w~n", [Supervisor]),
    start_actor(Supervisor, supervisor:run(Options)).

% Start a supervised supervisor thread
start(Supervisor, Options, ParentSupervisor) :-
    log(debug, supervisor, "[supervisor] Starting supervisor ~w under ~w~n", [Supervisor, ParentSupervisor]),
    start_actor(Supervisor, supervisor:run_supervised(Options, ParentSupervisor)).

stop(Supervisor) :- 
    log(debug, supervisor, "[supervisor] Stopping supervisor ~w~n", [Supervisor]),
    send_message(Supervisor, control(stop)),
    wait_for_actor_stopped(Supervisor).

kill(Supervisor) :-
    kill_all_children(Supervisor),
    forget_all_children(Supervisor),
    stop(Supervisor).

%%% Public

% Start and supervise an actor's singleton thread
start_child(Supervisor, Module, Options) :-
    singleton_thread_name(Module, Name),
    start_child(Supervisor, Module, Name, Options).

% Start and supervise an actor's named thread
start_child(Supervisor, Module, Name, Options) :-
    starting_child(Supervisor, Module, Name, Options).

% Starting a supervised named module thread
starting_child(Supervisor, Module, Name, Options) :-
    log(debug, supervisor, "[supervisor] Starting supervised child ~w ~w with ~p under ~w~n", [Module, Name, Options, Supervisor]),
    option(restart(Restart), Options, transient),
    Goal =.. [start, Name, Options, Supervisor],
    ModuleGoal =.. [:, Module, Goal],
    send_message(Supervisor, start_child(Module, Name, ModuleGoal, Restart)),
    wait_for_actor(Name).

kill_child(Supervisor, Module) :-
    singleton_thread_name(Module, Name),
    kill_child(Supervisor, Module, Name).

kill_child(Supervisor, Module, Name) :-
    log(debug, supervisor, "[supervisor] Stop child ~w ~w of ~w~n", [Module, Name, Supervisor]),
    send_message(Supervisor, stop_child(Module, Name)).

%%% Private 

singleton_thread_name(Module, Name) :-
    ModuleGoal =.. [:, Module, name(Name)],
    catch(call(ModuleGoal), Exception, (log(debug, supervisor, "[supervisor] Failed to get the singleton thread name of ~w: ~p~n", [Module, Exception]), fail)).

% Supervisor is running supervised, so it might be restarted on exit.
% If it is, then its supervised children might also be restarted.
run_supervised(Options, ParentSupervisor) :-
    option(restarting(Restarting), Options, false),
    Restarting -> maybe_restart_children; true,
    delete(Options, restarting(_), RestOptions),
    catch(run(RestOptions), Exit, process_exit(Exit, ParentSupervisor)).

process_exit(Exit, ParentSupervisor) :-
    thread_self(Supervisor),
    log(warn, supervisor, "[supervisor] Exit ~p of ~w~n", [Exit, Supervisor]),
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
    process_message(Message),
    !,
    run(Options).

% Process start, stop and exit messages
process_message(start_child(Module, Name, Goal, Restart)) :-
    log(debug, supervisor, "[supervisor] Starting child ~w ~w by calling ~p~n", [Module, Name, Goal]),
    forget_child(Module, Name),
    catch(do_start_child(Module, Name, Goal, Restart), Exception, log(debug, supervisor, "Failed to start child ~w ~w: ~p~n", [Module, Name, Exception])).

process_message(stop_child(Module, Name)) :-
    do_kill_child(Module, Name).

process_message(exited(Module, Name, Exit)) :-
    thread_self(Supervisor),
    child(Supervisor, Module, Name, Goal, Restart),
    log(debug, supervisor, "[supervise] Child ~w ~w exited with ~p. Restart is ~w~n", [Module,Name,Exit,Restart]),
    !,
    % avoid race condition by letting the exiting thread complete its exit
    sleep(1),
    maybe_restart(Supervisor, Module, Name, Goal, Exit, Restart).

process_message(exited(Module, Name, Exit)) :-
    log(debug, supervisor, "[supervisor] Unsupervised child ~w ~w (~p)~n", [Module, Name, Exit]).

process_message(control(stop)) :-
    thread_self(Supervisor),
    kill_all_children(Supervisor),
    thread_detach(Supervisor), 
    thread_exit(true).

kill_all_children(Supervisor) :-
    log(debug, supervisor, "Killing all children of ~w~n", [Supervisor]),
    foreach(child(Supervisor, Module, Name, _, _), do_kill_child(Module, Name)).

do_start_child(Module, Name, Goal, Restart) :-
    call(Goal),
    remember_child(Module, Name, Goal, Restart).

do_kill_child(Module, Name) :-
    log(debug, supervisor, "Killing child ~w ~w~n", [Module, Name]),
    Goal =.. [kill, Name],
    ModuleGoal =.. [:, Module, Goal],
    catch(call(ModuleGoal), Exception, log(debug, supervisor, "Failed to kill child ~w ~w: ~p~n", [Module, Name, Exception])),
    forget_child(Module, Name).

maybe_restart_children :-
    thread_self(Supervisor),
    foreach(
        child(Supervisor, Module, Name, Goal, Restart), 
        maybe_restart(Supervisor, Module, Name, Goal, exit(normal), Restart)).

maybe_restart(_, Module, Name, _, _, temporary) :-
    forget_child(Module, Name).
maybe_restart(_, Module, Name, _, exit(normal), transient) :-
    forget_child(Module, Name).
maybe_restart(Supervisor, Module, Name, Goal, _, transient) :-
    restarting_goal(Goal, RestartingGoal),
    send_message(Supervisor, start_child(Module, Name, RestartingGoal, transient)).
maybe_restart(Supervisor, Module, Name, Goal, _, permanent) :-
    restarting_goal(Goal, RestartingGoal),
    send_message(Supervisor, start_child(Module, Name, RestartingGoal, permanent)).

restarting_goal(ModuleGoal, RestartingModuleGoal) :-
    ModuleGoal =.. [:, Module, Goal],
    Goal =.. [start, Name, Options, Supervisor],
    merge_options([restarting(true)], Options, RestartingOptions),
    RestartingGoal =.. [start, Name, RestartingOptions, Supervisor],
    RestartingModuleGoal =.. [:, Module, RestartingGoal].

forget_all_children(Supervisor) :-
    log(debug, supervisor, "Forgetting all children of ~w~n", [Supervisor]),
    retractall(child(Supervisor, _,_,_,_)).

forget_child(Module, Name) :-
    thread_self(Supervisor),
    log(debug, supervisor, "Forgetting ~w ~w child of ~w~n", [Module, Name, Supervisor]),
    retractall(child(Supervisor, Module, Name, _, _)).

remember_child(Module, Name, Goal, Restart) :-
    thread_self(Supervisor),
    Child = child(Supervisor, Module, Name, Goal, Restart),
    log(debug, supervisor, "Remembering ~p~n", [Child]),
    assert(Child).
