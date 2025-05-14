/*
The PubSub singleton worker actor. 

It receives subscriptions from actors to listen to topics, and dispatches topical events received to topic listeners.

TODO: Set an exclusive affinity to the PubSub thread to maxinimize its time being scheduled to a core
*/

:- module(pubsub, [subscribed/1, subscribed/2, all_subscribed/1, all_subscribed/2, all_unsubscribed/0, all_unsubscribed/1, unsubscribed/1, unsubscribed/2, published/2]).

:- use_module(library(aggregate)).
:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(worker)).

% Singleton thread's name
name(pubsub).

%% Public

all_subscribed(Topics) :-
	self(Subscriber),
	all_subscribed(Subscriber, Topics).

all_subscribed(_, []) :-
	!.
all_subscribed(Subscriber, [Topic|Others]) :-
	subscribed(Subscriber, Topic),
	all_subscribed(Subscriber, Others).

subscribed(Topic) :-
	self(Subscriber),
	subscribed(Subscriber, Topic).

subscribed(Subscriber, Topic) :-
	name(Pubsub),
	message_sent(Pubsub,
		subscribed(Subscriber, Topic)).

unsubscribed(Topic) :-
	self(Subscriber),
	unsubscribed(Subscriber, Topic).

unsubscribed(Subscriber, Topic) :-
	name(Pubsub),
	message_sent(Pubsub,
		unsubscribed(Subscriber, Topic)).

all_unsubscribed :-
	self(Subscriber),
	all_unsubscribed(Subscriber).

all_unsubscribed(Subscriber) :-
	name(Pubsub),
	message_sent(Pubsub,
		all_unsubscribed(Subscriber)).

published(Topic, Payload) :-
	self(Source),
	name(Pubsub),
	message_sent(Pubsub,
		event(Topic, Payload, Source)).

%% Private

init(_, State) :-
	log(info, pubsub, 'Initializing pubsub'),
	empty_state(EmptyState),
	put_state(EmptyState, [subscriptions - []], State).

% Called when worker exits
terminated :-
	log(info, pubsub, 'Pubsub ~@ terminated', [self]).

signal_processed(control(stopped)) :-
	log(debug, pubsub, 'Stopping pubsub'),
	worker : stopped.

handled(message(subscribed(Subscriber, Topic), _), State, NewState) :-
	log(debug, pubsub, "Subscribing ~w to topic ~w~n", [Subscriber, Topic]),
	add_subscription(Subscriber, Topic, State, NewState).

handled(message(unsubscribed(Subscriber, Topic), _), State, NewState) :-
	log(debug, pubsub, "Unsubscribing ~w from topic ~w~n", [Subscriber, Topic]),
	 remove_subscription(Subscriber, Topic, State, NewState).

handled(message(all_unsubscribed(Subscriber), _), State, NewState) :-
	 remove_all_subscriptions(Subscriber, State, NewState).

handled(message(event(Topic, Payload, Source), _), State, State) :-
	broadcast(event(Topic, Payload, Source),
		State).

broadcast(event(Topic, Payload, Source), State) :-
	get_state(State, subscriptions, Subscriptions),
	foreach(member(subscription(Name, Topic),
			Subscriptions),
		event_sent_to(event(Topic, Payload, Source),
			Name)).

event_sent_to(Event, Name) :-
	log(debug, pubsub, "Sending event ~p to ~w~n", [Event, Name]),
	sent(Name, Event).

add_subscription(Subscriber, Topic, State, NewState) :-
	get_state(State, subscriptions, Subscriptions),
	(member(subscription(Subscriber, Topic),
			Subscriptions) ->
	true;
	put_state(State,
			subscriptions,
			[subscription(Subscriber, Topic)|Subscriptions],
			NewState)).

remove_subscription(Subscriber, Topic, State, NewState) :-
	get_state(State, subscriptions, Subscriptions),
	delete(Subscriptions,
		subscription(Subscriber, Topic),
		Subscriptions1),
	put_state(State, subscriptions, Subscriptions1, NewState).

remove_all_subscriptions(Subscriber, State, NewState) :-
	get_state(State, subscriptions, Subscriptions),
	delete(Subscriptions,
		subscription(Subscriber, _),
		Subscriptions1),
	put_state(State, subscriptions, Subscriptions1, NewState).    
