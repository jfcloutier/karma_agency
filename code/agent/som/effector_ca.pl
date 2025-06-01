/*
An effector is an a priori cognition actor that communicates with a body effector to execute actions.
The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA

* expects messages about
    * actuated(Action)

* like all CAs, an effector CA responds to queries about
    * name
    * level
    * latency
    * belief_domain -> always responds with []
    * action_domain -> always responds with [Action, ...], e.g. [spin, reverse_spin]

*/

:- module(effector_ca, []).

:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(worker)).
:- use_module(utils(logger)).
:- use_module(agent(body)).
:- use_module(som(ca_support)).

name_from_effector(Effector, Name) :-
	atomic_list_concat([effector, Effector.id], ':', Name).

level_from_name(_, 0).

% An effector CA has no latency
latency(0).

% The CA has the lowest level of abstration
level(0).

% Always fully volunteer for recruitment
recruit(_, 1).

init(Options, State) :-
	log(info, effector_ca, 'Initiating with ~p', [Options]), 
	empty_state(EmptyState), 
	option(
		effectors(Effectors), Options), 
	put_state(EmptyState, effectors, Effectors, State1), 
	action_domain(State1, ActionDomain), 
	put_state(State1, [parents-[], action_domain - ActionDomain], State),
	subscribed_to_events(State1),
	published(ca_started, [level(0)]).

subscribed_to_events(State) :-
	subscribed(ca_terminated).

signal_processed(control(stopped)) :-
	worker : stopped.

terminated :-
	log(warn, effector_ca, 'Terminated').

handled(message(adopted, Parent), State, NewState) :-
	get_state(State, parents, Parents),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(actuated(Action), _), State, State) :-
	actuated(State, Action).

handled(message(Message, Source), State, State) :-
	log(debug, effector_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).
	 
handled(query(name), _, Name) :-
	self(Name).

handled(query(type), _, ca).

handled(query(level), _, Level) :-
	level(Level).

handled(query(latency), _, Latency) :-
	latency(Latency).

handled(query(belief_domain), _, []).

handled(query(action_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handled(query(Query), State, Answer) :-
	ca_support : handled(query(Query), State, Answer).
%%%%

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

actuated(State, Action) :-
	action_url(State, Action, ActionUrl), 
	body : actuated(ActionUrl).