/*
The PubSub singleton worker actor. 

It receives subscriptions from actors to listen to Subs from sources, and dispatches events received
to topic listeners.

A subscription is a topic - source  pair. The topic is an atom and Source is the name of a matching source (`any`, the default, for all sources to match)

Actors publish an atom (the topic), a payload and the source (an actor's nanme)
Actors receive events `event(Topic, Payload, Source).

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

all_subscribed(Subs) :-
	self(Subscriber),
	all_subscribed(Subscriber, Subs).

all_subscribed(_, []) :-
	!.
all_subscribed(Subscriber, [Sub|Others]) :-
	subscribed(Subscriber, Sub),
	all_subscribed(Subscriber, Others).

subscribed(Sub) :-
	self(Subscriber),
	subscribed(Subscriber, Sub).

subscribed(Subscriber, Sub) :-
	name(Pubsub),
	message_sent(Pubsub,
		subscribed(Subscriber, Sub)).

unsubscribed(Sub) :-
	self(Subscriber),
	unsubscribed(Subscriber, Sub).

unsubscribed(Subscriber, Sub) :-
	name(Pubsub),
	message_sent(Pubsub,
		unsubscribed(Subscriber, Sub)).

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
		pub(Topic, Payload, Source)).

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

handled(message(subscribed(Subscriber, Sub), _), State, NewState) :-
	log(debug, pubsub, "Subscribing ~w to ~w~n", [Subscriber, Sub]),
	add_subscription(Subscriber, Sub, State, NewState).

handled(message(unsubscribed(Subscriber, Sub), _), State, NewState) :-
	log(debug, pubsub, "Unsubscribing ~w from ~w~n", [Subscriber, Sub]),
	 remove_subscription(Subscriber, Sub, State, NewState).

handled(message(all_unsubscribed(Subscriber), _), State, NewState) :-
	 remove_all_subscriptions(Subscriber, State, NewState).

handled(message(pub(Topic, Payload, Source), _), State, State) :-
	broadcast(event(Topic, Payload, Source),
		State).

broadcast(event(Topic, Payload, Source), State) :-
	Sub = Topic - Source,
	get_state(State, subscriptions, Subscriptions),
	foreach((member(subscription(Name, Subscription),Subscriptions), 
			sub_match(Sub, Subscription)),
		event_sent_to(event(Topic, Payload, Source),
			Name)).

sub_match(Topic - _, Topic - any).
sub_match(Topic - any, Topic - _).
sub_match(Topic - Source, Topic - Source).

event_sent_to(Event, Name) :-
	log(debug, pubsub, "Sending event ~p to ~w~n", [Event, Name]),
	sent(Name, Event).

add_subscription(Subscriber, Topic-Source, State, NewState) :-
	Sub = Topic-Source, !,
	get_state(State, subscriptions, Subscriptions),
	(member(subscription(Subscriber, Sub),
			Subscriptions) ->
	true;
	put_state(State,
			subscriptions,
			[subscription(Subscriber, Sub)|Subscriptions],
			NewState)).

add_subscription(Subscriber, Topic, State, NewState) :-
	add_subscription(Subscriber, Topic-any, State, NewState).

remove_subscription(Subscriber, Topic-Source, State, NewState) :-
	Sub = Topic-Source, !,
	get_state(State, subscriptions, Subscriptions),
	delete_matching_subs(Subscriptions,
		subscription(Subscriber, Sub),
		Subscriptions1),
	put_state(State, subscriptions, Subscriptions1, NewState).

remove_subscription(Subscriber, Topic, State, NewState) :-
	remove_subscription(Subscriber, Topic-any, State, NewState).

delete_matching_subs([], _, []).

delete_matching_subs([subscription(Subscriber, Sub) | Rest],
	subscription(Subscriber, DeleteSub),
		RemainingSubscriptions) :-
			sub_match(Sub, DeleteSub), !,
			delete_matching_subs(Rest, subscription(Subscriber, DeleteSub), RemainingSubscriptions).
	
delete_matching_subs([Subscription | Rest],
	subscription(Subscriber, DeleteSub),
		[Subscription | RemainingSubscriptions]) :-
			delete_matching_subs(Rest, subscription(Subscriber, DeleteSub),
		RemainingSubscriptions).

remove_all_subscriptions(Subscriber, State, NewState) :-
	get_state(State, subscriptions, Subscriptions),
	delete(Subscriptions,
		subscription(Subscriber, _),
		Subscriptions1),
	put_state(State, subscriptions, Subscriptions1, NewState).    
