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
%   init - the fully qualified name of the 1-ary clause called when initiating the actor's state (defaults to worker:init)
%   terminate - the fully qualified name of the 0-ary clause called when terminating the actor (defaults to worker:terminate)
%
% A worker holds a state that is updated from processing messages.
%%%
:- module(worker, [send/2]).

:- use_module(library(option)).
:- use_module(code(logger)).
:- use_module(actor_utils).
:- use_module(pubsub).

%%% Supervised behavior

start(Name, Options, Supervisor) :-
    option(handler(Handler), Options),
    option(topics(Topics), Options, []),
    option(init(Init), Options, worker:init),
    option(terminate(Terminate), Options, worker:terminate),
    % Topics, Init, Handler
    log(debug, worker, "Creating worker ~w~n", [Name]),
    start_actor(Name, worker:start_worker(Name, Topics, Init, Handler, Supervisor), [at_exit(Terminate)]).

stop(Name) :-
    log(debug, worker, "Stopping worker ~w~n", [Name]),
    % Cause a normal exit
    send_message(Name, control(stop)),
    wait_for_actor_stopped(Name).

kill(Name) :-
    log(debug, worker, "Killing worker ~w~n", [Name]),
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
    log(warn, worker, "Exit ~p of ~w~n", [Exit, Name]),
    unsubscribe_all,
    thread_detach(Name), 
    notify_supervisor(Supervisor, Name, Exit),
    thread_exit(true).


start_run(Topics, Init, Handler) :-
    Init =.. [:, Module, Head],
    Goal =.. [Head, State],
    InitGoal =.. [:, Module, Goal],
    call(InitGoal),
    subscribe_all(Topics),
    run(Handler, State).

run(Handler, State) :-
    thread_get_message(Message),
    process_message(Message, Handler, State, NewState),
    run(Handler, NewState).

process_message(control(stop), _, _, _) :-
    throw(exit(normal)).

process_message(control(die), _, _, _) :-
    thread_self(Name),
    thread_detach(Name),
    thread_exit(true).

process_message(query(Question, From), Handler, State, State) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, query(Question), State, Response],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    log(debug, worker, "~w got query ~p from ~m~n", [Name, Question, From]),
    call(ModuleGoal),
    send_message(From, response(Response, Name)).

process_message(Message, Handler, State, NewState) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, Message, State, NewState],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    log(debug, worker, "~w got ~p; calling ~p~n", [Name, Message, Goal]),
    call(ModuleGoal).

% Inform supervisor of the exit
notify_supervisor(Supervisor, Name, Exit) :-
    send_message(Supervisor, exited(worker, Name, Exit)).

% Just succeed
terminate.

% Start with an empty state
init(State) :-
    empty_state(State).

