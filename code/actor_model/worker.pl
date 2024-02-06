%%%
% Worker actor logic.
%
% A worker thread subscribes to and handles broadcast events, 
% sends/receives direct messages and responds to control directives.
% 
% A worker is supervised and thus implements start/3, stop/1 and kill/1.
%
% A worker is started by giving it a unique name and options.
% Options:
%   topics - the list of topics for the events the worker wants to receive (defaults to [])
%   handler - the fully qualified name of the clause header handling events (required)
%   init - the fully qualified name of the clause called when initiating the agent (defaults to worker:noop/0)
%   terminate - the fully qualified name of the clause called when terminating the agent (defaults to worker:noop/0)
%%%
:- module(worker, [send/2]).

:- use_module(library(option)).
:- use_module(actor_utils).
:- use_module(pubsub).

%%% Supervised behavior

start(Name, Options, Supervisor) :-
    option(handler(Handler), Options),
    option(topics(Topics), Options, []),
    option(init(Init), Options, worker:noop),
    option(terminate(Terminate), Options, worker:noop),
    % Topics, Init, Handler
    format("[worker] Creating worker ~w~n", [Name]),
    start_actor(Name, worker:start_worker(Name, Topics, Init, Handler, Supervisor), [at_exit(Terminate)]).

stop(Name) :-
    format("[worker] Stopping worker ~w~n", [Name]),
    % Cause a normal exit
    send_message(Name, control(stop)),
    wait_for_actor_stopped(Name).

kill(Name) :-
    format("[worker] Killing worker ~w~n", [Name]),
    % Force exit
    send_message(Name, control(die)),
    wait_for_actor_stopped(Name).

%%% Public

send(Name, Message) :-
    thread_self(Source),
    send_message(Name, message(Message, Source)).

%%% Private - in thread

start_worker(Name, Topics, Init, Handler, Supervisor) :-
    catch(start_run(Topics, Init, Handler), Exit, process_exit(Name, Exit, Supervisor)).

process_exit(Name, Exit, Supervisor) :-
    format("[worker] Exit ~p of ~w~n", [Exit, Name]),
    unsubscribe_all,
    thread_detach(Name), 
    notify_supervisor(Supervisor, Name, Exit),
    thread_exit(true).


start_run(Topics, Init, Handler) :-
    call(Init),
    subscribe_all(Topics),
    run(Handler).

run(Handler) :-
    thread_get_message(Message),
    process_message(Message, Handler),
    run(Handler).

process_message(control(stop), _) :-
    throw(exit(normal)).

process_message(control(die), _) :-
    thread_self(Name),
    thread_detach(Name),
    thread_exit(true).


process_message(Message, Handler) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, Message],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    format("[worker] ~w got ~p; calling ~p~n", [Name, Message, Goal]),
    call(ModuleGoal).

% Inform supervisor of the exit
notify_supervisor(Supervisor, Name, Exit) :-
    send_message(Supervisor, exited(worker, Name, Exit)).

% Just succeed
noop.

