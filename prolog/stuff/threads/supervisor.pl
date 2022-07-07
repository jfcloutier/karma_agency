%% Supervisor.
%
%% Starts and stops threads in modules that comply with supervised behaviour
%%  implements start(Name, Options) and stop(Name, Options)
%%  sends exited(Module, Name, Exit) messages where Exit = exit(normal) means normal exit.
%
%% Restarts terminated threads according to option restart(Restart) 
%% where Restart is one of permanent, temporary or transient (the default).
:- module(supervisor, [supervise/2, supervise/3, stop_supervised/1, stop_supervised/2]).

:- use_module(library(option)).

% Singleton thread so no need to use thread_local
:- dynamic child/4.

%Start the supervisor thread
start() :-
    writeln("[supervisor] Starting"),
    thread_create(run(), _, [alias(supervisor)]).

stop() :- 
    writeln("[supervisor] Stopping"),
    thread_send_message(supervisor, control(stop)).

%%% Public

% Start and supervise a module's singleton thread
supervise(Module, Options) :-
    singleton_thread_name(Module, Name),
    supervise(Module, Name, Options).

% Start and supervise a named module thread
supervise(Module, Name, Options) :-
    format("[supervisor] Supervising child ~w ~w with ~p~n", [Module, Name, Options]),
    option(restart(Restart), Options, transient),
    Goal =.. [start, Name, Options],
    ModuleGoal =.. [:, Module, Goal],
    thread_send_message(supervisor, start_child(Module, Name, ModuleGoal, Restart)).

stop_supervised(Module) :-
    singleton_thread_name(Module, Name),
    stop_supervised(Module, Name).

stop_supervised(Module, Name) :-
    format("[supervisor] Stop child ~w ~w~n", [Module, Name]),
    thread_send_message(supervisor, stop_child(Module, Name)).

%%% Private 

singleton_thread_name(Module, Name) :-
    ModuleGoal =.. [:, Module, name(Name)],
    catch(call(ModuleGoal), Exception, (format("[supervisor] Failed to get the singleton thread name of ~w: ~p~n", [Module, Exception]), fail)).

run() :-
    thread_get_message(Message),
    process_message(Message),
    !,
    run().

% Process start, stop and exit messages
process_message(start_child(Module, Name, Goal, Restart)) :-
    format("[supervisor] Starting child ~w ~w by calling ~p~n", [Module, Name, Goal]),
    retractall(child(Module, Name, _, _)),
    assert(child(Module, Name, Goal, Restart)),
    !,
    catch(call(Goal), Exception, format("Failed to start child ~w ~w: ~p~n", [Module, Name, Exception])).

process_message(stop_child(Module, Name)) :-
    retractall(child(Module, Name, _, _)),
    Goal =.. [stop, Name],
    ModuleGoal =.. [:, Module, Goal],
    !,
    catch(call(ModuleGoal), Exception, format("Failed to stop child ~w ~w: ~p~n", [Module, Name, Exception])).

process_message(exited(Module, Name, Exit)) :-
    child(Module, Name, Goal, Restart),
    format("[supervise] Child ~w ~w exited with ~p. Restart is ~w~n", [Module,Name,Exit,Restart]),
    !,
    % avoid racecondition by letting the exiting thread complete its exit
    sleep(1),
    maybe_restart(Module, Name, Goal, Exit, Restart).

process_message(exited(Module, Name, Exit)) :-
    format("[supervisor] Unsupervised child ~w ~w (~p)~n", [Module, Name, Exit]).

process_message(control(stop)) :-
    thread_detach(supervisor), 
    thread_exit(true).

maybe_restart(_, _, _, _, temporary).
maybe_restart(_, _, _, exit(normal), transient).
maybe_restart(Module, Name, Goal, _, transient) :-
    thread_send_message(supervisor, start_child(Module, Name, Goal, transient)).
maybe_restart(Module, Name, Goal, _, permanent) :-
    thread_send_message(supervisor, start_child(Module, Name, Goal, permanent)).
