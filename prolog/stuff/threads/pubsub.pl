:- module(pubsub, [start_pubsub/0, stop_pubsub/0, listens_to/2, remove_listener/1]).

:- use_module(library(aggregate)).
:- use_module(bb).

:- dynamic listener/2.

start_pubsub() :-
    thread_create(init(), _, [alias(pubsub)]).

stop_pubsub() :-
    add_event(event(control, stop(pubsub))).

listens_to(Queue, Topic) :-
    assertz(listener(Queue, Topic)).

remove_listener(Queue) :-
    foreach(listener(Queue,Topic), retract(listener(Queue, Topic))).

init() :-
    % Do some initiazations here
    run().

run() :-
    writeln("WAITING FOR EVENTS"),
    thread_wait(
        handle_events(Events), 
        [retry_every(30), module(bb),  wait_preds([+(event/2)]), modified(Events)]),
    run().

handle_events([]) :-
    writeln("NO EVENT"), !, fail.

handle_events(Events) :- 
    format("HANDLING EVENTS ~w~n", [Events]),
    foreach(bb:event(Topic, Payload), handle_event(Topic,Payload)).

handle_event(Topic, Payload) :-
    Event =.. [event, Topic, Payload],
    remove_events(Event),
    process_event(Event).

process_event(event(control, stop(pubsub))) :-
    writeln("STOPPING"),
    thread_detach(pubsub),
    throw(exit).

process_event(event(Topic,Payload)) :-
    broadcast(event(Topic, Payload)).

broadcast(event(Topic, Payload)) :-
    Event =.. [event, Topic, Payload],
    foreach(listener(Queue, Topic), send_message(Event, Queue)).

send_message(Event, Queue) :-
    format("Sending event ~w to ~w~n", [Event, Queue]).
    % thread_send_message(Queue, Event).
