/*
An effector CA is an a priori cognition actor that communicates with a body effector to execute actions.
An effector CA receives intents to carry out an action, responds with readiness if the action is within
its action domain, and then may be told to execute.
An effector CA believes or not in having recently attempted a given action. 

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA:

* receives the message `adopted` when added to the umwelt of a CA

* subscribes to events from its parents:
	* topic: intent, payload: [goal = belief(Action, true), priority = Float]
	* topic: execute, payload: [policy = [command(Action)]]
	* topic: prediction, payload: [predicted = belief(Action, Boolean)]

* publishes events:
	* topic: can_execute, payload: [goal = belief(Action, true), policy = [command(Action)]]
    * topic: prediction_error, payload:[predicted = belief(Action, Boolean), actual = belief(Action, Boolean1), confidence = 1.0])

* like all CAs, an effector CA responds to queries about
    * name
    * level
    * latency - for how long having taken an action is believed to be true
    * belief_domains -> always responds with [attempted - [Action | _]] % the object domain is the names of the avaialble actions
    * action_domain -> always responds with [Action, ...], e.g. [spin, reverse_spin]

* makes observations:
	* executed(Action)

Lifecycle
  * Created for a body sensor
  * Repeatedly
    * Adopted by a CA as part of its umwelt (new parent)
    * Queried for its action domain (what intents it can receive)
	* Queried for its belief domains (what action it can believe to have taken)
    * Handles intent and execute events, sends can_execute events
	* Handles predictions about actions taken - sends prediction_error events
    * Parent CA terminated
*/

:- module(effector_ca, []).

:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(utils(logger)).
:- use_module(agency(body)).
:- use_module(agency(som/ca_support)).

name_from_effector(Effector, Name) :-
	atomic_list_concat([effector, Effector.id], ':', Name).

level_from_name(_, 0).

% An effector CA has no fixed latency - its timeframe terminates after an action was executed,
% and the attempt observed and beliefs updated 
latency(0).

% The CA has the lowest level of abstration
level(0).

% Always fully volunteer for recruitment
recruit(_, 1).

init(Options, State) :-
	log(info, effector_ca, "Initiating with ~p", [Options]), 
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
	log(warn, effector_ca, "Terminated").

handled(message(adopted, Parent), State, NewState) :-
	get_state(State, parents, Parents),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(actuated(Action), _), State, State) :-
	actuated(State, Action).

handled(message(Message, Source), State, State) :-
	log(debug, effector_ca, "~@ is NOT handling message ~p from ~w", [self, Message, Source]).

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