/*
An effector is an a priori cognition actor that communicates with a body effector to execute actions.
The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA

* expects messages about
    * actuate(Action)

* like all CAs, an effector CA responds to queries about
    * name
    * level
    * latency
    * belief_domain -> always responds with []
    * action_domain -> always responds with [Action, ...], e.g. [spin, reverse_spin]

*/



:- module(effector_ca, []).

:- [load].

:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(worker)).
:- use_module(code(logger)).
:- use_module(agent(body)).

name_from_effector(Effector, Name) :-
	atomic_list_concat([effector, Effector.id], ':', Name).

% An effector CA has no latency
latency(0).

% The CA has the lowest level of abstration
level(0).

init(Options, State) :-
	log(info, effector_ca, 'Initiating with ~p', [Options]), 
	empty_state(EmptyState), 
	option(
		effectors(Effectors), Options), 
	put_state(EmptyState, effectors, Effectors, State1), 
	action_domain(State1, ActionDomain), 
	put_state(State1, [action_domain - ActionDomain], State), 
	publish(ca_started, [level(0)]).

process_signal(control(stop)) :-
	worker : stop.

terminate :-
	log(warn, effector_ca, 'Terminated').

handle(message(actuate(Action), _), State, State) :-
	actuate(State, Action).

handle(message(Message, Source), State, State) :-
	log(debug, effector_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(Topic, Payload, Source), State, State) :-
	log(debug, effector_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(name), _, Name) :-
	self(Name).

handle(query(type), _, ca).

handle(query(level), _, Level) :-
	level(Level).

handle(query(latency), _, Latency) :-
	latency(Latency).

handle(query(belief_domain), _, []).

handle(query(action_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handle(query(Query), _, tbd) :-
	log(debug, effector_ca, '~@ is NOT handling query ~p', [self, Query]).

%%%%

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

actuate(State, Action) :-
	action_url(State, Action, ActionUrl), 
	body : actuate(ActionUrl).