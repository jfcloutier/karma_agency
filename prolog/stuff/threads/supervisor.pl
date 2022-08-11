%% Supervisor.
%
%% Starts and kills threads in modules that comply with supervised behavior:
%%  implements start(Name, Options), stop(Name) and kill(Name)
%%  if the module runs a singleton thread, it must implement name(Name)
%%  sends exited(Module, Name, Exit) messages where Exit = exit(normal) means normal exit.
%
%% Restarts terminated threads according to option restart(Restart) 
%% where Restart is one of permanent (always restart), temporary (never restart) or transient (restart on abnormal exit - the default).
%%
%% A supervisor can itself be supervised.
:- module(supervisor, []).

:- use_module(library(option)).
:- use_module(thread_utils).

:- thread_local child/4.

% Start a top supervisor thread
start(Supervisor) :-
    start(Supervisor, []).

start(Supervisor, Options) :-
    format("[supervisor] Starting supervisor ~w~n", [Supervisor]),
    start_thread(Supervisor, supervisor:run(Options)).

% Start a supervised supervisor thread
start(Supervisor, Options, ParentSupervisor) :-
    format("[supervisor] Starting supervisor ~w under ~w~n", [Supervisor, ParentSupervisor]),
    start_thread(Supervisor, supervisor:run_supervised(Options, ParentSupervisor)).

stop(Supervisor) :- 
    format("[supervisor] Stopping supervisor ~w~n", [Supervisor]),
    send_message(Supervisor, control(stop)).

kill(Supervisor) :-
    stop(Supervisor).

%%% Public

% Start and supervise a module's singleton thread
start_child(Supervisor, Module, Options) :-
    singleton_thread_name(Module, Name),
    start_child(Supervisor, Module, Name, Options).

% Start and supervise a named module thread
start_child(Supervisor, Module, Name, Options) :-
    format("[supervisor] Starting supervised child ~w ~w with ~p under ~w~n", [Module, Name, Options, Supervisor]),
    option(restart(Restart), Options, transient),
    Goal =.. [start, Name, Options, Supervisor],
    ModuleGoal =.. [:, Module, Goal],
    send_message(Supervisor, start_child(Module, Name, ModuleGoal, Restart)),
    wait_for_thread(Name).

kill_child(Supervisor, Module) :-
    singleton_thread_name(Module, Name),
    kill_child(Supervisor, Module, Name).

kill_child(Supervisor, Module, Name) :-
    format("[supervisor] Stop child ~w ~w of ~w~n", [Module, Name, Supervisor]),
    send_message(Supervisor, stop_child(Module, Name)).

%%% Private 

singleton_thread_name(Module, Name) :-
    ModuleGoal =.. [:, Module, name(Name)],
    catch(call(ModuleGoal), Exception, (format("[supervisor] Failed to get the singleton thread name of ~w: ~p~n", [Module, Exception]), fail)).

run_supervised(Options, ParentSupervisor) :-
    catch(run(Options), Exit, process_exit(Exit, ParentSupervisor)).

process_exit(Exit, ParentSupervisor) :-
    thread_self(Supervisor),
    format("[supervisor] Exit ~p of ~w~n", [Exit, Supervisor]),
    kill_all_children(),
    thread_detach(Supervisor), 
    notify_supervisor(ParentSupervisor, Supervisor, Exit),
    thread_exit(true).

kill_all_children() :-
    foreach(child(Module, Name, _, _), do_kill_child(Module, Name)).

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
    format("[supervisor] Starting child ~w ~w by calling ~p~n", [Module, Name, Goal]),
    forget_child(Module, Name),
    catch(do_start_child(Module, Name, Goal, Restart), Exception, format("Failed to start child ~w ~w: ~p~n", [Module, Name, Exception])).

process_message(stop_child(Module, Name)) :-
    do_kill_child(Module, Name).

process_message(exited(Module, Name, Exit)) :-
    child(Module, Name, Goal, Restart),
    thread_self(Supervisor),
    format("[supervise] Child ~w ~w exited with ~p. Restart is ~w~n", [Module,Name,Exit,Restart]),
    !,
    % avoid race condition by letting the exiting thread complete its exit
    sleep(1),
    maybe_restart(Supervisor, Module, Name, Goal, Exit, Restart).

process_message(exited(Module, Name, Exit)) :-
    format("[supervisor] Unsupervised child ~w ~w (~p)~n", [Module, Name, Exit]).

process_message(control(stop)) :-
    thread_self(Supervisor),
    kill_all_children(),
    thread_detach(Supervisor), 
    thread_exit(true).

do_start_child(Module, Name, Goal, Restart) :-
    call(Goal),
    remember_child(Module, Name, Goal, Restart).

do_kill_child(Module, Name) :-
    forget_child(Module, Name),
    Goal =.. [kill, Name],
    ModuleGoal =.. [:, Module, Goal],
    !,
    catch(call(ModuleGoal), Exception, format("Failed to kill child ~w ~w: ~p~n", [Module, Name, Exception])).

maybe_restart(_, Module, Name, _, _, temporary) :-
    forget_child(Module, Name).
maybe_restart(_, Module, Name, _, exit(normal), transient) :-
    forget_child(Module, Name).
maybe_restart(Supervisor, Module, Name, Goal, _, transient) :-
    send_message(Supervisor, start_child(Module, Name, Goal, transient)).
maybe_restart(Supervisor, Module, Name, Goal, _, permanent) :-
    send_message(Supervisor, start_child(Module, Name, Goal, permanent)).

forget_child(Module, Name) :-
    retractall(child(Module, Name, _, _)).

remember_child(Module, Name, Goal, Restart) :-
    assert(child(Module, Name, Goal, Restart)).
