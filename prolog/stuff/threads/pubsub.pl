:- module(pubsub, [subscribe/2, subscribe_all/2, publish/2, unsubscribe_all/1]).

:- use_module(library(aggregate)).
:- use_module(bb).

:- dynamic subscription/2.

start() :-
    thread_create(init(), _, [alias(pubsub)]).

stop() :-
    publish(control, stop(pubsub)).

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

init() :-
    writeln("[pubsub] Starting"),
    % Do some initiazations here
    run().

run() :-
    writeln("[pubsub] Waiting..."),
    thread_get_message(Event),
    format("Handling ~p~n", [Event]),
    handle_event(Event),
    run().

handle_event(event(control, stop(pubsub), _)) :-
    writeln("[pubsub] Stopping"),
    thread_detach(pubsub),
    throw(exit(normal)).

handle_event(event(Topic,Payload, Source)) :-
    broadcast(event(Topic, Payload, Source)).

broadcast(event(Topic, Payload, Source)) :-
    foreach(subscription(Queue, Topic), send_event(event(Topic, Payload, Source), Queue)).

send_event(Event, Queue) :-
    format("[pubsub] Sending event ~p to ~w~n", [Event, Queue]),
    thread_send_message(Queue, Event).
