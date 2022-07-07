:- module(pubsub, [subscribe/2, subscribe_all/2, publish/2, unsubscribe_all/1]).

:- use_module(library(aggregate)).
:- use_module(bb).

% Singleton thread so no need to use thread_local
:- dynamic subscription/2.

%% Supervised

start(Name, _) :-
    writeln("[pubsub] Starting"),
    thread_create(start_pubsub(), _, [alias(Name)]).

stop(Name) :-
    writeln("[pubsub] Stopping"),
    thread_send_message(Name, control(stop(Name))).

% Singleton thread's name
name(pubsub).

%% Public

subscribe_all(_, []).
subscribe_all(Queue, [Topic | Others]) :-
    subscribe(Queue, Topic),
    subscribe_all(Queue, Others).
    

subscribe(Queue, Topic) :-
    assertz(subscription(Queue, Topic)).

unsubscribe_all(Queue) :-
    foreach(subscription(Queue, Topic), retract(subscription(Queue, Topic))).

publish(Topic, Payload) :-
    thread_self(Source),
    thread_send_message(pubsub, event(Topic, Payload, Source)).

%% Private

start_pubsub() :-
    catch(start_run(), Exit, process_exit(Exit)).

start_run() :-
    % Do some initializations here
    run().

process_exit(Exit) :-
    format("[pubsub] Exit ~p~n", [Exit]),
    thread_detach(pubsub), 
    % race condition?
    notify_supervisor(Exit),
    thread_exit(true).


run() :-
    writeln("[pubsub] Waiting..."),
    thread_get_message(Event),
    format("Handling ~p~n", [Event]),
    handle_event(Event),
    run().

handle_event(event(control, stop(pubsub), _)) :-
    writeln("[pubsub] Stopping"),
    retractall(subscription/2),
    throw(exit(normal)).

handle_event(event(Topic,Payload, Source)) :-
    broadcast(event(Topic, Payload, Source)).

broadcast(event(Topic, Payload, Source)) :-
    foreach(subscription(Queue, Topic), send_event(event(Topic, Payload, Source), Queue)).

send_event(Event, Queue) :-
    format("[pubsub] Sending event ~p to ~w~n", [Event, Queue]),
    thread_send_message(Queue, Event).

% Inform supervisor of the exit
notify_supervisor(Exit) :-
    thread_send_message(supervisor, exited(pubsub, pubsub, Exit)).
