:- module(pubsub, [subscribe/1, subscribe_all/1, publish/2, unsubscribe_all/0]).

:- use_module(library(aggregate)).
:- use_module(code(logger)).
:- use_module(actor_utils).

% TODO - Subscriptions are lost if pubsub is restarted
% TODO - Use a run state to hold subscriptions
:- thread_local subscription/2.

%% Supervised

start(Supervisor) :-
    log(debug, pubsub, "Starting"),
    thread_create(start_pubsub(Supervisor), _, [alias(pubsub)]),
    wait_for_actor(pubsub).

start(Supervisor, _, _, _) :-
    start(Supervisor).

stop(_) :-
    log(debug, pubsub, "Stopping"),
    send_message(pubsub, stop).

kill(_) :-
    log(debug, pubsub, "Dying"),
    send_message(pubsub, die).

% Singleton thread's name
name(pubsub).

%% Public

subscribe_all([]).
subscribe_all([Topic | Others]) :-
    subscribe(Topic),
    subscribe_all(Others).
    

subscribe(Topic) :-
   name(Name),
   is_thread(Name) -> 
        (thread_self(Subscriber),
        send_message(Name, subscribe(Subscriber, Topic))
        )
        ;
        true.

unsubscribe_all :-
    name(Name),
    is_thread(Name) ->
        (thread_self(Subscriber),
        send_message(pubsub, unsubscribe(Subscriber))
        )
        ;
        true.

publish(Topic, Payload) :-
    name(Name),
    is_thread(Name) ->
        (thread_self(Source),
        send_message(pubsub, event(Topic, Payload, Source))
        )
        ;
        true.

%% Private

start_pubsub(Supervisor) :-
    catch(start_run, Exit, process_exit(Exit, Supervisor)).

start_run :-
    % Do some initializations here
    run.

process_exit(Exit, Supervisor) :-
    log(debug, pubsub,  "Exit ~p~n", [Exit]),
    thread_detach(pubsub), 
    % race condition?
    notify_supervisor(Supervisor, Exit),
    thread_exit(true).


run :-
    log(debug, pubsub, "Waiting..."),
    thread_get_message(Message),
    process_message(Message),
    run.

process_message(subscribe(Name, Topic)) :-
    log(debug, pubsub, "Subscribing ~w to topic ~w~n", [Name, Topic]),
    assertz(subscription(Name, Topic)).

process_message(unsubscribe(Name)) :-
    retractall(subscription(Name, _)).

process_message(stop) :-
    log(debug, pubsub, "Stopping"),
    retractall(subscription/2),
    throw(exit(normal)).

process_message(die) :-
    log(debug, pubsub, "Dying"),
    retractall(subscription/2),
    thread_detach(pubsub), 
    thread_exit(true).

process_message(event(Topic, Payload, Source)) :-
    broadcast(event(Topic, Payload, Source)).

broadcast(event(Topic, Payload, Source)) :-
    foreach(subscription(Name, Topic), send_event(event(Topic, Payload, Source), Name)).

send_event(Event, Name) :-
    log(debug, pubsub, "Sending event ~p to ~w~n", [Event, Name]),
    send_message(Name, Event).

% Inform supervisor of the exit
notify_supervisor(Supervisor, Exit) :-
    name(Name),
    send_message(Supervisor, exited(pubsub, Name, Exit)).
