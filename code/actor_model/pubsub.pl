/*
The PubSub singleton actor. 

It receives subscriptions to topics from listening actors and dispatches events received to topic listeners.
*/


:- module(pubsub, [subscribe/1, subscribe/2, subscribe_all/1, subscribe_all/2, unsubscribe_all/0, unsubscribe_all/1, unsubscribe/1, unsubscribe/2, publish/2]).

:- [load].

:- use_module(library(aggregate)).
:- use_module(code(logger)).
:- use_module(actor_utils).
:- use_module(worker).

% Singleton thread's name
name(pubsub).

%% Public


subscribe_all(Topics) :-
    self(Subscriber), 
    subscribe_all(Subscriber, Topics).

subscribe_all(_, []).
subscribe_all(Subscriber, [Topic | Others]) :-
    subscribe(Subscriber, Topic),
    subscribe_all(Subscriber, Others).

subscribe(Topic) :-
    self(Subscriber),
    subscribe(Subscriber, Topic).

subscribe(Subscriber, Topic) :-
    name(Pubsub),
    send_message(Pubsub, subscribe(Subscriber, Topic)).

unsubscribe(Topic) :-
    self(Subscriber),
    unsubscribe(Subscriber, Topic).

unsubscribe(Subscriber, Topic) :-
    name(Pubsub),
    send_message(Pubsub, unsubscribe(Subscriber, Topic)).

unsubscribe_all :-
    self(Subscriber),
    unsubscribe_all(Subscriber).

unsubscribe_all(Subscriber) :-
    name(Pubsub),
    send_message(Pubsub, unsubscribe_all(Subscriber)).

publish(Topic, Payload) :-
    self(Source),
    name(Pubsub),
    send_message(Pubsub, event(Topic, Payload, Source)).

%% Private

init(_, State) :-
    log(info, pubsub, 'Initializing pubsub'),
    empty_state(EmptyState),
    put_state(EmptyState, [subscriptions-[]], State).

% Called when worker exits
terminate :-
    log(info, pubsub, 'Pubsub ~@ terminated', [self]).

process_signal(control(stop)) :-
    log(debug, pubsub, 'Stopping pubsub'),
    worker:stop.

handle(message(subscribe(Subscriber, Topic), _), State, NewState) :-
    log(debug, pubsub, "Subscribing ~w to topic ~w~n", [Subscriber, Topic]),
    add_subscription(Subscriber, Topic, State, NewState).

handle(message(unsubscribe(Subscriber, Topic), _), State, NewState) :-
    log(debug, pubsub, "Unsubscribing ~w from topic ~w~n", [Subscriber, Topic]),
    remove_subscription(Subscriber, Topic, State, NewState).

handle(message(unsubscribe_all(Subscriber), _), State, NewState) :-
    remove_all_subscriptions(Subscriber, State, NewState).

handle(message(event(Topic, Payload, Source), _), State, State) :-
    broadcast(event(Topic, Payload, Source), State).

broadcast(event(Topic, Payload, Source), State) :-
    get_state(State, subscriptions, Subscriptions),
    foreach(member(subscription(Name, Topic), Subscriptions), send_event_to(event(Topic, Payload, Source), Name)).

send_event_to(Event, Name) :-
    log(debug, pubsub, "Sending event ~p to ~w~n", [Event, Name]),
    send(Name, Event).

add_subscription(Subscriber, Topic, State, NewState) :-
    get_state(State, subscriptions, Subscriptions),
    (member(subscription(Subscriber, Topic), Subscriptions) ->
        true
    ;
        put_state(State, subscriptions, [subscription(Subscriber, Topic) | Subscriptions], NewState)
    ).

remove_subscription(Subscriber, Topic, State, NewState) :-
    get_state(State, subscriptions, Subscriptions),
    delete(Subscriptions, subscription(Subscriber, Topic), Subscriptions1),
    put_state(State, subscriptions, Subscriptions1, NewState).

remove_all_subscriptions(Subscriber, State, NewState) :-
    get_state(State, subscriptions, Subscriptions),
    delete(Subscriptions, subscription(Subscriber, _), Subscriptions1),
    put_state(State, subscriptions, Subscriptions1, NewState).    
