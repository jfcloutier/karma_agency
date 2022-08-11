:- module(pubsub, [subscribe/2, subscribe_all/2, publish/2, unsubscribe_all/1]).

:- use_module(library(aggregate)).
:- use_module(bb).
:- use_module(thread_utils).

:- thread_local subscription/2.

%% Supervised

start(pubsub, _, Supervisor) :-
    writeln("[pubsub] Starting"),
    thread_create(start_pubsub(Supervisor), _, [alias(pubsub)]).

stop(pubsub) :-
    writeln("[pubsub] Stopping"),
    send_message(pubsub, stop()).

kill(pubsub) :-
    writeln("[pubsub] Dying"),
    send_message(pubsub, die()).

% Singleton thread's name
name(pubsub).

%% Public

subscribe_all(_, []).
subscribe_all(Name, [Topic | Others]) :-
    subscribe(Name, Topic),
    subscribe_all(Name, Others).
    

subscribe(Name, Topic) :-
   send_message(pubsub, subscribe(Name, Topic)).

unsubscribe_all(Name) :-
    send_message(pubsub, unsubscribe(Name)).

publish(Topic, Payload) :-
    thread_self(Source),
    send_message(pubsub, event(Topic, Payload, Source)).

%% Private

start_pubsub(Supervisor) :-
    catch(start_run(), Exit, process_exit(Exit, Supervisor)).

start_run() :-
    % Do some initializations here
    run().

process_exit(Exit, Supervisor) :-
    format("[pubsub] Exit ~p~n", [Exit]),
    thread_detach(pubsub), 
    % race condition?
    notify_supervisor(Supervisor, Exit),
    thread_exit(true).


run() :-
    writeln("[pubsub] Waiting..."),
    thread_get_message(Message),
    process_message(Message),
    run().

process_message(subscribe(Name, Topic)) :-
    assertz(subscription(Name, Topic)).

process_message(unsubscribe(Name)) :-
    retractall(subscription(Name, _)).

process_message(stop()) :-
    writeln("[pubsub] Stopping"),
    retractall(subscription/2),
    throw(exit(normal)).

process_message(die()) :-
    writeln("[pubsub] Dying"),
    retractall(subscription/2),
    thread_detach(pubsub), 
    thread_exit(true).

process_message(event(Topic,Payload, Source)) :-
    broadcast(event(Topic, Payload, Source)).

broadcast(event(Topic, Payload, Source)) :-
    foreach(subscription(Name, Topic), send_event(event(Topic, Payload, Source), Name)).

send_event(Event, Name) :-
    format("[pubsub] Sending event ~p to ~w~n", [Event, Name]),
    send_message(Name, Event).

% Inform supervisor of the exit
notify_supervisor(Supervisor, Exit) :-
    send_message(Supervisor, exited(pubsub, pubsub, Exit)).
