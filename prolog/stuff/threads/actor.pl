%%%
% Simple actor framework.
%
% An actor thread subscribes to and handles broadcast events, 
% sends/receives direct messages and responds to control directives.
%%%
:- module(actor, [send/2]).

:- use_module(library(option)).
:- use_module(pubsub).

%%% Supervised behaviour

start(Name, Options) :-
    option(handler(Handler), Options),
    option(topics(Topics), Options, []),
    option(init(Init), Options, actor:noop()),
    option(terminate(Terminate), Options, actor:noop()),
    % Topics, Init, Handler
    format("[actor] Creating actor ~w~n", [Name]),
    thread_create(start_actor(Name, Topics, Init, Handler), _, [alias(Name), at_exit(Terminate)]).

stop(Name) :-
    format("[actor] Stopping actor ~w~n", [Name]),
    % Cause a normal exit
    thread_send_message(Name, control(stop)).

%%% Public

send(Queue, Message) :-
    thread_self(Source),
    thread_send_message(Queue, message(Message,Source)).

%%% Private - in thread

start_actor(Name, Topics, Init, Handler) :-
    catch(start_run(Name, Topics, Init, Handler), Exit, process_exit(Name, Exit)).

process_exit(Name, Exit) :-
    format("[actor] Exit ~p of ~w~n", [Exit, Name]),
    unsubscribe_all(Name),
    thread_detach(Name), 
    notify_supervisor(Name, Exit),
    thread_exit(true).


start_run(Name, Topics, Init, Handler) :-
    call(Init),
    subscribe_all(Name, Topics),
    run(Handler).

run(Handler) :-
    thread_get_message(Message),
    process_message(Message, Handler),
    run(Handler).

process_message(control(stop), _) :-
    throw(exit(normal)).

process_message(Message, Handler) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, Message],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    format("[actor] ~w got ~p; calling ~p~n", [Name, Message, Goal]),
    call(ModuleGoal).

% Inform supervisor of the exit
notify_supervisor(Name, Exit) :-
    thread_send_message(supervisor, exited(actor, Name, Exit)).

% Just succeed
noop().

