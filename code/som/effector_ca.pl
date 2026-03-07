/*
An effector CA is a static (a priori) cognition actor that communicates with a body effector to actuate it.

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA receives from its parents intended actions.
It responds with the actions it is able to actuate and those it can't.

It then may be told, one at a time, to prime body effectors for some/all of these goals.

Eventually, the body is told to actuate all primed affectors by the CA that initiated the cascade of directives.

The effector CA updates its wellbeing based on its participation and sends an event
about its change in wellbeing.

Each action taken affects the CA's wellbeing. It reduces the CA's fullness and integrity by 1 (they start at 100), and increases
its engagement by 1 (it starts at 0). An effector CA can not actuate if its fullness or integrity is at 0. An effector CA publishes
changes to its wellbeing and effects transfers to and from its parents.

An effector's actions are the IDs of the goals it can achieve.

An effector CA neither observes nor experiences. 

Messages:

* In
	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `wellbeing_transfer(Wellbeing)` - Wellbeing is wellbeing{fullness: Delta1, integrity:Delta2, engagement:Delta3} - a transfer of wellbeing - an effector only consumes fullness and integrity
	
* Out to a parent
	* `actions_received(PlanId)`
	* `actuations_ready(PlanId)`

Events:

* In from an ancestor
	* `intent_completed, [intent_id=IntentId]`
	* `abandoned, [intent_id=IntentId]`

* In from a parent
	* `planned_actions, [actions=[Action, ...], plan_id=PlanId, intent_id=IntentId]` - Intended actions accumulate ONLY while not their actuation is not ready
	* `ready_actuations, [plan_id=PlanId, intent_id=IntentId]` - Only the maximum number of identical actions in any plan under the intent gets executed
	* `plan_executed, [plan_id=PlanId]`
	* `ca_terminated, [level=Level]`

* Out
	*`ca_started, [level = Level]`

Queries:

* In
    * level - 0
	* type - effector_ca
    * latency - unknown - an effector CA has no set latency
	* action_domain -> the actions the effector_ca can take
	* wellbeing -> wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} - the engagement of an effector CA is fixed at 1.0

State:
	* parents - parent CAs
	* effectors - the body effectors (1 action per body effector) the CA is responsible for
	* action_domain - actions the effector can execute - [Action, ...]
	* actuations - actuations intended and readied - [actuation{action:Action, repeats:N, plan_id:PlanId, intent_id:IntentId, ready:Boolean, executed:Boolean}, ...]
	* wellbeing - wellbeing values - initially wellbeing{fullness:1.0, integrity:1.0, engagement:1.0}

An effector CA has no fixed latency; its unique timeframe is updated after notice of 
execution by the body of its accumulated actuations, but only if some originated from the effector CA.

It does not have a memory of past timeframes; it only remembers its yet-to-be executed actuations.
Upon receiving an `intent_completed` event, an effector CA resets its list of actuations for thate intent.
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
	put_state(State1, [parents - [], actuations - [], action_domain - ActionDomain, wellbeing - InitialWellbeing], State),
	subscribed_to_global_events(),
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

handled(query(action_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handled(query(wellbeing), State, Wellbeing) :-
	get_state(State, wellbeing, Wellbeing).

handled(query(Query), State, Answer) :-
	ca_support : handled(query(Query), State, Answer).

handled(message(adopted, Parent), State, NewState) :-
	% Ignore if adopter is already a parent
	\+ from_parent(Parent, State),
    all_subscribed([ca_terminated - Parent, planned_actions - Parent, ready_actuations - Parent, plan_executed - Parent, intent_completed]),
    acc_state(State, parents, Parent, ca_support:agency_state_sorter, NewState).

% An effector CA only consumes fullness and integrity, Its engagement remains constant at 1.0.
handled(message(wellbeing_transfer(WellbeingTransfer), _), State, NewState) :-
	wellbeing_transfered(State, wellbeing{fullness:WellbeingTransfer.fullness, integrity:WellbeingTransfer.integrity, engagement:0}, NewState).	

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).	
	 
handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

% Accumulates not-yet-ready actuations by plan in the context of an intent.
% Tells the parent that actions were received for the intent.
handled(event(planned_actions, Payload, Parent), State, NewState) :-
	option(actions(Actions), Payload),
	option(plan_id(PlanId), Payload),
	option(intent_id(IntentId), Payload),
	clumped(Actions, ActionCountPairs),	
	actuations_accumulated(ActionCountPairs, PlanId, IntentId, State, NewState),
	message_sent(Parent, actions_received(PlanId)).

% Tell the body to ready all actuations for the given intent.
% Respond back to parent that the intent's actuations are ready to execute.
handled(event(ready_actuations, Payload, Parent), State, NewState) :-
	option(plan_id(PlanId), Payload),
	option(intent_id(IntentId), Payload),
	actuations_readied(PlanId, IntentId, State, NewState),
	message_sent(Parent, actuations_ready(PlanId)).

% Mark all actuations for the plan as executed
handled(event(plan_executed, Payload, _), State, NewState) :-
	option(plan_id(PlanId), Payload),
	(wellbeing_changed(PlanId, State, UpdatedWellbeing) ->
		put_state(State, wellbeing, UpdatedWellbeing, State1)
		;
		State1 = State
		),
	actuations_updated(State.actuations, PlanId, executed, true, State1, NewState).

% Forget all actuations for the intent when abandoned
handled(event(abandoned, Payload, _), State, NewState) :-
	option(intent_id(IntentId), Payload),
    actuations_reset_for_intent(IntentId, State, NewState).	

handled(event(Topic, Payload, Parent), State, NewState) :-
	ca_support : handled(event(Topic, Payload, Parent), State, NewState).

%%%%

initial_wellbeing(wellbeing{fullness:1.0, integrity:1.0, engagement:1.0}).

subscribed_to_global_events() :-
	all_subscribed([intent_completed, abandoned]).

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

actuations_accumulated([], _, _, State, State).

% Accumulate the count of a given action by changing the max repeats in plans per intent
actuations_accumulated([Action-Count | Rest], PlanId, IntentId, State, NewState) :-
	actuation_accumulated(Action, Count, PlanId, IntentId, State, State1),
	actuations_accumulated(Rest, PlanId, IntentId, State1, NewState).

actuation_accumulated(Action, Count, PlanId, IntentId, State, NewState) :-
	member(Action, State.action_domain),
	acc_state(State, actuations, actuation{action:Action, plan_id:PlanId, intent_id:IntentId, repeats:Count, ready:false, executed:false}, ca_support:agency_state_sorter, NewState).

actuation_accumulated(_, _, _, _, State, State).

% Ready planned actions so that we don't exceed the maximum number of action executions
% in any plan under the intent
actuations_readied(PlanId, IntentId, State, NewState) :-
	log(info, effector_ca, "~@ is readying actuations for plan ~w given intent ~w", [self, PlanId, IntentId]),
	findall(Actuation, (member(Actuation, State.actuations), Actuation.plan_id == PlanId), PlannedActuations),
	findall(Actuation, (member(Actuation, State.actuations), Actuation.intent_id == IntentId), IntendedActuations),
	unexecuted_actuations_readied(PlannedActuations, IntendedActuations, State, NewState).

unexecuted_actuations_readied([], _, State, State).

unexecuted_actuations_readied([PlannedActuation | Rest], IntendedActuations, State, NewState) :-
	unexecuted_actuation_readied(PlannedActuation, IntendedActuations, State, NewState),
	unexecuted_actuations_readied(Rest, IntendedActuations, State, NewState).

unexecuted_actuations_readied([_ | Rest], IntendedActuations, State, NewState) :-
	unexecuted_actuations_readied(Rest, IntendedActuations, State, NewState).

unexecuted_actuation_readied(PlannedActuation, IntendedActuations, State, NewState) :-
	log(info, effector_ca, "Readying actuation ~p given intended actuations ~w", [PlannedActuation, IntendedActuations]),
	actuation{action:Action, repeats:Repeats, ready:false} :< PlannedActuation,
	max_planned_action_repeats(Action, IntendedActuations, MaxRepeats),
	executed_action_count(Action, IntendedActuations, ExecutedCount),
	MaxCount is max(0, MaxRepeats - ExecutedCount),
	Count is max(0, Repeats - MaxCount),
	body_actuated(State, Action, Count),
	actuation_updated(PlannedActuation, ready, true, State, NewState).

max_planned_action_repeats(Action, IntendedActuations, MaxRepeats) :-
	findall(Repeats, (member(Actuation, IntendedActuations), actuation{action:Action, repeats:Repeats} :< Actuation), AllRepeats),
	max_list(AllRepeats, MaxRepeats).
	
executed_action_count(Action, IntendedActuations, ExecutedCount) :-
	findall(Repeats, (member(Actuation, IntendedActuations), actuation{action:Action, repeats:Repeats, executed:true} :< Actuation), AllRepeats),
	max_list(AllRepeats, ExecutedCount).	

actuations_updated([], _, _, _, State, State).

% Readying an actuation is idempotent
actuations_updated([Actuation | Rest], PlanId, Property, Value, State, NewState) :-
	actuation{plan_id:PlanId, intent_id:IntentId, action:Action, repeats:Count, ready:false} :< Actuation,
	!,
	log(info, effector_ca, "~@ is updating ~w action(s) ~w for plan ~w and intent ~w to ~w = ~p", [self, Count, Action, PlanId, IntentId, Property, Value]),
	body_actuated(State, Action, Count),
	actuation_updated(Actuation, Property, Value, State, State1),
	actuations_updated(Rest, PlanId, Property, Value, State1, NewState).

actuations_updated([_ | Rest], PlanId, Property, Value, State, NewState) :-
	actuations_updated(Rest, PlanId, Property, Value, State, NewState).

actuation_updated(Actuation, Property, Value, State, NewState) :-
	delete(State.actuations, Actuation, Actuations1),
	Actuation1 = Actuation.put(Property, Value),
	acc_state(State, actuations, [Actuation1 | Actuations1], ca_support:agency_state_sorter, NewState).

% Each action executed decrements fullness by 1 (energy spent),
% integrity by 1 (wear and tear), 
% and increments engagement by 1 (we acted in the world).
% Fails if wellbeing was not changed.
wellbeing_changed(PlanId, State, UpdatedWellbeing) :-
	get_state(State, wellbeing, Wellbeing),
	get_state(State, actuations, Actuations),
	findall(Actuation, (member(Actuation, Actuations), Actuation.plan_id == PlanId), PlannedActuations),
	length(PlannedActuations, Count),
	Count \= 0,
    Delta is Count * 0.1,
	UpdatedWellbeing = Wellbeing.add(wellbeing{fullness: -Delta, integrity: -Delta, engagement: 0}),
	log(debug, effector_ca, "~@ update wellbeing to ~p", [self, UpdatedWellbeing]).

actuations_reset_for_intent(IntentId, State, NewState) :-
	get_state(State, actuations, Actuations),
	findall(Actuation, (member(Actuation, Actuations), Actuation.intent_id == IntentId), IntendedActuations),
	dec_state(State, actuations, IntendedActuations, NewState).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

% Tell the body to prepare to carry out this actuation a number of times
body_actuated(State, Action, Count) :-
	action_url(State, Action, ActionUrl), 
	foreach(between(1, Count, _), body : actuated(ActionUrl)),
	log(debug, effector_ca, "~@ asked body to actuate ~w ~w times", [self, Action, Count]).

effector_name(State, EffectorName) :-	
	get_state(State, effectors, [Effector | _]),
	EffectorName = Effector.id.	
