/*
An effector CA is an a priori cognition actor that communicates with a body effector to actuate it.

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA receives from its parents intents to carry out policies, responds with readiness for directives within
its action domain, and then may be told to actuate by telling the body to prepare to execute actions as directed.

Each action taken affects the CA's wellbeing. It reduces the CA's fullness and integrity by 1 (they start at 100), and increases
its engagement by 1 (it starts at 0). An effector CA can not actuate if its fullness or integrity is at 0. An effector CA publishes
changes to its wellbeing and effects transfers to and from its parents.

An effector's actions are "atomic" policies (i.e. not involving other CAs's policies); their names are actuations the body can execute.

An effector CA believes or not in the body having last executed a certain number of the effector CA's actions/atomic policies.
Its beliefs are not normative (neither pleasant or unpleasant). 

Messages:

* In
	* `adopted(Parent)` when added to the umwelt of a CA

* Out to a parent
    * `can_actuate(IntentPayload, Directives)` - responding to intent event - it can actuate these directives
    * `cannot_actuate(IntentPayload)` - responding to intent event - this effector CA can't do any part of the parent's intended policy
	* `executable(ActuationPayload, Directives)` responding to actuation message - the effector CA has primed the body for execution
    * `prediction_error(PredictionPayload, ActualValue)` - responding to prediction event - the parent is wrong about how many times an action was executed

* In from a parent
    * actuation(DirectivesPayload) - a parent communicates the actuations it wants the effector CA to execute
	* wellbeing_transfer(WellbeingPayload) - payload is [fullness = N1, integrity = N2, engagement = N3]

Events:

* In
	* topic: execute, payload: [] - execution is triggered

* In from parents
	* topic: intent, payload: [policy = Policy] - a parent CA communicates a policy it built that it intends to execute if possible
	* topic: prediction, payload: [belief = Belief] - a parent makes a prediction about how many of a given action the effector CA believes were executed
	
* Out
    * topic: ca_started, payload: [level = Level]
	* topic: wellbeing_changed, payload: WellbeingPayload  - payload is [fullness = N1, integrity = N2, engagement = N3]

Queries:

* In
    * level - 0
    * latency - unknown - an effector CA has no set latency
    * belief_domain -> always responds with [predictable{name:count, object:Action, value:positive_integer}, ...] 
		- Action is the external name for `executed(Action)`, the factual observation that the Action was executed by the body
    * policy_domain -> always responds with [Action, ...], e.g. [spin, reverse_spin]

State:
	* parents - parent CAs
	* effectors - the body effectors (1 action per body effector) the CA is responsible for
	* observations - actions last executed - [executed(Action), ...]
	* beliefs - beliefs from last execution - [count(Action, N), ...]
	* action_domain - actions the effector can execute - [Action, ...]
	* actuations - actions ready for execution - [Action, ...]
	* wellbeing - wellbeing values - initially [fullness = 100, integrity = 100, engagement = 0]

Lifecycle
  * Created for a body effector
  * Repeatedly
    * Adopted by a CA in its umwelt (new parent) - subscribes to umwelt events from parent
    * Queried for its policy domain (what policies - actions -  it can actuate)
	* Queried for its belief domains (only believes in counts of actions executed)
    * Handles intent events - sends can_actuate/2 or cannot_actuate/2 events in response
	* Handles actuation messages - tells the body to prepare to actuate, records the actuations
	* Handles executed event (the body has aggregated and executed actuations) - updates count/2 beliefs
	* Handles predictions events from parents about the last executed actions - sends prediction_error events
	* Updates its wellbeing from actions executed and publishes wellbeing_changed events
	* Handles wellbeing _tranfer messages from parents
  * Parent CA terminated - unsubscribes from umwelt events originating from the parent

An effector CA has no fixed latency; its unique timeframe is updated after notice of 
execution by the body of its accumulated actuations, but only if some originated from the effector CA.

It does not have a memory of past timeframes; it only remembers its yet-to-be executed actuations and its beliefs about the latest
execution. Upon receiving an intent event, an effector CA resets its list of activations. Upon receiving an execution event, an
effector CA recomputes its observations and beliefs.
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

% Always volunteer fully for recruitment
recruit(_, 1.0).

%% Actor creation

init(Options, State) :-
	log(info, effector_ca, "Initiating with ~p", [Options]), 
	empty_state(EmptyState), 
	option(
		effectors(Effectors), Options), 
	put_state(EmptyState, effectors, Effectors, State1), 
	action_domain(State1, ActionDomain),
	initial_wellbeing(InitialWellbeing),
	put_state(State1, [parents - [], observations - [], beliefs - [], action_domain - ActionDomain, actuations - [], wellbeing - InitialWellbeing], State),
	subscribed_to_events(),
	published(ca_started, [level(0)]).

%% Actor termination - only when agency terminates

signal_processed(control(stopped)) :-
	all_unsubscribed,
	worker : stopped.

terminated :-
	log(warn, effector_ca, "Terminated").

%% Actor interactions

handled(query(type), _, effector_ca).

handled(query(level), _, 0).

handled(query(latency), _, unknown).

handled(query(belief_domain), State, BeliefDomain) :-
	get_state(State, action_domain, ActionDomain),
	bagof(predictable{name:count, object:Action, value:positive_integer}, member(Action, ActionDomain), BeliefDomain).

handled(query(policy_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handled(query(Query), State, Answer) :-
	ca_support : handled(query(Query), State, Answer).

handled(message(adopted, Parent), State, NewState) :-
	% Ignore if adopter is already a parent
	\+ from_parent(Parent, State),
    all_subscribed([ca_terminated - Parent, prediction - Parent, intent - Parent]),
    acc_state(State, parents, Parent, NewState).

handled(message(actuation(ActuationPayload), Parent), State, NewState) :-
	option(policy(ParentPolicy), ActuationPayload),
	directed_actions(ParentPolicy, State, AllActions),
	log(info, effector_ca, "~@ is actuating actions ~p from policy ~p", [self, AllActions, ParentPolicy]),
	foreach(member(Action, AllActions), body_actuated(State, Action)),
	bagof(command(Action), (member(Action, AllActions)), Commands),
	message_sent(Parent, executable(ActuationPayload, Commands)),
	acc_state(State, actuations, AllActions, NewState).

handled(message(wellbeing_transfer(WellbeingTransfer), _), State, NewState) :-
	wellbeing_transfered(State, WellbeingTransfer, NewState).	

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).	
	 
handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).	

handled(event(intent, IntentPayload, Parent), State, NewState) :-
	well_enough(State),
	option(policy(ParentPolicy), IntentPayload),
	setof(Directive, (member(Directive, ParentPolicy), can_do(Directive, State)), Directives), 
    [_ | _] = Directives,
	!,
	log(info, effector_ca, "~@ can actuate directives ~p from intent ~p", [self, Directives, IntentPayload]),
	message_sent(Parent, can_actuate(IntentPayload, Directives)),
	actuations_reset(State, NewState).	

handled(event(intent, IntentPayload, Parent), State, NewState) :-
	message_sent(Parent, cannot_actuate(IntentPayload)),
	actuations_reset(State, NewState).	

handled(event(execute, _, _), State, NewState) :-
	actions_executed(),
	observations_from_actuations(State, Observations),
	put_state(State, observations, Observations, State1),
	beliefs_from_observations(State1, Beliefs),
	put_state(State1, beliefs, Beliefs, State2),
	(wellbeing_changed(State2, UpdatedWellbeing) ->
		put_wellbeing(State2, UpdatedWellbeing, NewState)
		;
		NewState = State2
		).	
		
handled(event(prediction, PredictionPayload, Parent), State, State) :-
	option(belief(PredictedBelief), PredictionPayload),
	about_belief(PredictedBelief, State, Belief),
	belief_value(PredictedBelief, PredictedValue),
	belief_value(Belief, ActualValue),
	(same_belief_value(PredictedValue, ActualValue) ->
		true;
		message_sent(Parent, prediction_error(PredictionPayload, ActualValue))
	).	

handled(event(Topic, Payload, Parent), State, NewState) :-
	ca_support : handled(event(Topic, Payload, Parent), State, NewState).

%%%%

initial_wellbeing([fullness = 100, integrity = 100, engagement = 0]).

subscribed_to_events() :-
	all_subscribed([execute]).

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

can_do(command(Action), State) :-
	get_state(State, action_domain, ActionDomain),
	member(Action, ActionDomain).

can_do(goal(count(Action, N)), State) :-
	N > 0,
	get_state(State, action_domain, ActionDomain),
	member(Action, ActionDomain).

directed_actions([], _, []).

directed_actions([Directive | OtherDirectives], State, AllActions) :-
	how_to(Directive, State, Actions),
	directed_actions(OtherDirectives, State, OtherActions),
	flatten([Actions | OtherActions], AllActions).

how_to(command(Action), State, [Action]) :-
	can_do(command(Action), State), !.

how_to(goal(count(Action, N)), State, Actions) :-
	get_state(State, action_domain, ActionDomain),
	member(Action, ActionDomain),
	!,
	length(Actions, N),
	maplist(=(Action), Actions).

how_to(_, _, []).

% Each action executed decrements fullness by 1 (energy spent),
% integrity by 1 (wear and tear), 
% and increments engagement by 1 (we acted in the world).
% Fails if wellbeing was not changed.
wellbeing_changed(State, UpdatedWellbeing) :-
	get_wellbeing(State, Fullness, Integrity, Engagement),
	get_state(State, observations, Observations),
	length(Observations, Count),
	Count > 0,
	Fullness1 is max(Fullness - Count, 0),
	Integrity1 is max(Integrity - Count, 0),
	Engagement1 is min(Engagement + Count, 100),
	UpdatedWellbeing = [fullness = Fullness1, integrity = Integrity1, engagement = Engagement1],
	published(wellbeing_changed, UpdatedWellbeing).


actuations_reset(State, NewState) :-
	put_state(State, actuations, [], NewState).

observations_from_actuations(State, Observations) :-
	get_state(State, actuations, Actuations),
	bagof(executed(Action), member(Action, Actuations), Observations).

beliefs_from_observations(State, Beliefs) :-
	get_state(State, observations, Observations),
	bagof(Action, member(executed(Action), Observations), ExecutedActions),
	sort(ExecutedActions, ActionTypes),
	map_list_to_pairs(count_in(ExecutedActions), ActionTypes, Counts),
    % The name of an action is how an effector CA presents `executed(Action)` to its parents
	bagof(count(ActionType, N), member(N - ActionType, Counts), Beliefs).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

% Tell the body to prepare to carry out this actuation (together with possibly others)
body_actuated(State, Action) :-
	action_url(State, Action, ActionUrl), 
	body : actuated(ActionUrl),
	log(debug, effector_ca, "~@ asked body to actuate ~w", [self, Action]).

actions_executed :-
	query_answered(agency, option(body_host), Host),
	body : actions_executed(Host).
