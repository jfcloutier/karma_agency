%%%
% Simple actor framework.
%
% An actor thread subscribes to and handles broadcast events, 
% sends/receives direct messages and responds to control directives.
% 
% An actor is supervised and thus implements start/3 and stop/1.
%
% An actor is started by giving it a unique name and options.
% Options:
%   topics - the list of topics forthe events the actor wantsto receive (defaults to [])
%   handler - the fully qualified name of the clause header handling events (required)
%   init - the fully qualified name of the clause called when initiating the agent (defaults to actor:noop/0)
%   terminate - the fully qualified name of the clause called when terminating the agent (defaults to actor:noop/0)
%%%
:- module(actor, [send/2]).

:- use_module(library(option)).
:- use_module(thread_utils).
:- use_module(pubsub).

%%% Supervised behavior

start(Name, Options, Supervisor) :-
    option(handler(Handler), Options),
    option(topics(Topics), Options, []),
    option(init(Init), Options, actor:noop()),
    option(terminate(Terminate), Options, actor:noop()),
    % Topics, Init, Handler
    format("[actor] Creating actor ~w~n", [Name]),
    start_thread(Name, actor:start_actor(Name, Topics, Init, Handler, Supervisor), [at_exit(Terminate)]).

stop(Name) :-
    format("[actor] Stopping actor ~w~n", [Name]),
    % Cause a normal exit
    send_message(Name, control(stop)).

kill(Name) :-
    format("[actor] Killing actor ~w~n", [Name]),
    % Force exit
    send_message(Name, control(die)).

%%% Public

send(Name, Message) :-
    thread_self(Source),
    send_message(Name, message(Message, Source)).

%%% Private - in thread

start_actor(Name, Topics, Init, Handler, Supervisor) :-
    catch(start_run(Name, Topics, Init, Handler), Exit, process_exit(Name, Exit, Supervisor)).

process_exit(Name, Exit, Supervisor) :-
    format("[actor] Exit ~p of ~w~n", [Exit, Name]),
    unsubscribe_all(Name),
    thread_detach(Name), 
    notify_supervisor(Supervisor, Name, Exit),
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

process_message(control(die), _) :-
    thread_self(Name),
    thread_detach(Name),
    thread_exit(true).


process_message(Message, Handler) :-
    Handler =.. [:, Module, Head],
    Goal =.. [Head, Message],
    ModuleGoal =.. [:, Module, Goal],
    thread_self(Name),
    format("[actor] ~w got ~p; calling ~p~n", [Name, Message, Goal]),
    call(ModuleGoal).

% Inform supervisor of the exit
notify_supervisor(Supervisor, Name, Exit) :-
    send_message(Supervisor, exited(actor, Name, Exit)).

% Just succeed
noop().

