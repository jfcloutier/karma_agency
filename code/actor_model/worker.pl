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
%
% Callbacks:
%   handle/3 - for handling events and queries
%   init/1 - sets the initial state
%   terminate/0 - called when terminating the actor
%
% A worker holds a state that is updated from processing messages.
%%%
:- module(worker, []).

:- use_module(library(option)).
:- use_module(code(logger)).
:- use_module(actor_utils).
:- use_module(pubsub).

%%% Supervised behavior

start(Name, Module, Options, Supervisor) :-
    log(debug, worker, 'Starting ~w implemented by ~w with options ~p supervised by ~w', [Name, Module, Options, Supervisor]),
    option(topics(Topics), Options, []),
    option(init(Args), Options, []),
    Handler =.. [:, Module, handle],
    Init =.. [:, Module, init, Args],
    Terminate =.. [:, Module, terminate],
    log(debug, worker, "Creating worker ~w", [Name]),
    start_actor(Name, worker:start_worker(Name, Topics, Init, Handler, Supervisor), [at_exit(Terminate)]).

stop(Name) :-
    log(debug, worker, "Stopping worker ~w", [Name]),
    % Cause a normal exit
    send_control(Name, stop),
    wait_for_actor_stopped(Name).

kill(Name) :-
    log(debug, worker, "Killing worker ~w", [Name]),
    % Force exit
    send_control(Name, die),
    wait_for_actor_stopped(Name).

unsubscribe(Name, Topic) :-
    log(debug, worker, "Unsubscribing worker ~w from ~w", [Name, Topic]),
    send(pubsub, unsubscribe(Name, Topic)).

%%% Private - in thread

start_worker(Name, Topics, Init, Handler, Supervisor) :-
    catch(start_run(Topics, Init, Handler), Exit, process_exit(Name, Exit, Supervisor)).

process_exit(Name, Exit, Supervisor) :-
    log(warn, worker, "Exit ~p of ~w", [Exit, Name]),
    unsubscribe_all,
    thread_detach(Name), 
    notify_supervisor(Supervisor, Name, Exit),
    thread_exit(true).


start_run(Topics, Init, Handler) :-
    log(debug, worker, 'Start run with topics ~p, init ~p and handler ~p', [Topics, Init, Handler]),
    Init =.. [:, Module, Head, Args],
    Goal =.. [Head, Args, State],
    InitGoal =.. [:, Module, Goal],
    call(InitGoal),
    subscribe_all(Topics),
    run(Handler, State).

run(Handler, State) :-
    thread_self(Name),
    log(debug, worker, 'Worker ~w is waiting...', [Name]),
    thread_get_message(Message),
    process_message(Message, Handler, State, NewState),
    run(Handler, NewState).

process_message(control(stop), _, _, _) :-
    throw(exit(normal)).

process_message(control(die), _, _, _) :-
    thread_self(Name),
    thread_detach(Name),
    thread_exit(true).

process_message(unsubscribe(Topic), _, _, _) :-
    unsubscribe(Topic).

process_message(query(Question, From), Handler, State, State) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, query(Question), State, Response],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    log(debug, worker, "~w got query ~p from ~w", [Name, Question, From]),
    call(ModuleGoal),
    send(From, response(Response, Question, Name)).

process_message(Message, Handler, State, NewState) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, Message, State, NewState],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    log(debug, worker, "~w got message ~p; calling ~p", [Name, Message, ModuleGoal]),
    call(ModuleGoal).

% Inform supervisor of the exit
notify_supervisor(Supervisor, Name, Exit) :-
    send(Supervisor, exited(worker, Name, Exit)).
