/*
An effector CA is a static (a priori) cognition actor that communicates with a body effector to actuate it.

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA receives from its parents intents to carry out directives (prioritized goals).
It responds with the goals it is able to realize/actuate It then may be told, one at a time, to prime body effectors for some/all of these goals.
It responds when done.
Eventually, the body is told to actuate all primed affectors by the CA that initiated the cascade of directives.
The effector CA learns that the body executed primed actuations. It updates its wellbeing based on its participation and sends an event
about its change in wellbeing.

parent CA --- intent(directives) -->> umwelt CAs
parent CA <-- can_actuate(goals - can be empty) --- umwelt CA
parent CA --- ready_actuation(goal) ---> umwelt CA
...
parent CA <-- actuation_ready(goal) --- umwelt CA
...
(initiating CA tells the body to execute all primed actuations)
initiating CA ---- executed ---->> all CAs
parent CA <<-- wellbeing_changed -- umwelt CA

Note: -->> denote event broadcasts and --> denote message unicasts

Each action taken affects the CA's wellbeing. It reduces the CA's fullness and integrity by 1 (they start at 100), and increases
its engagement by 1 (it starts at 0). An effector CA can not actuate if its fullness or integrity is at 0. An effector CA publishes
changes to its wellbeing and effects transfers to and from its parents.

An effector's actions are "atomic" affordances (i.e. not involving other CAs's affordances); their names are actuations the body can execute.

An effector CA experiences or not in the body having last executed a certain number of the effector CA's actions/atomic affordances.
Its experiences are not normative (neither pleasant or unpleasant). 

Messages:

* In
	* `adopted(Parent)` when added to the umwelt of a CA

* In from a parent
	* ready_actuation(Goal) - a parent communicates an actuation it wants the effector CA to execute
	* wellbeing_transfer(WellbeingValues) - payload is [fullness = N1, integrity = N2, engagement = N3]

* Out to a parent	
	* `can_actuate(Goals)` - responding to intent event - it can actuate these directives (can be empty)
	* `actuation_ready(Goal)` responding to actuation message - the effector CA has primed the body for execution
    * `prediction_error(PredictionPayload, ActualValue)` - responding to prediction event - the parent is wrong about how many times an action was executed
	
Events:

* In
	* topic: executed, payload: [] - body actuation was triggered

* In from parents
	* topic: intent, payload: [id = Id, priority = Priority, goals = [Goal, ...] - a parent CA communicates an affordance it built that it intends to execute if possible
	* topic: intent_completed, payload: [id = Id, executed = Boolean] - ignored
	* topic: prediction, payload: [experience = Experience] - a parent makes a prediction about how many of a given action the effector CA experiences were executed
	
* Out
    * topic: ca_started, payload: [level = Level]
	* topic: wellbeing_changed, payload: WellbeingPayload  - payload is [fullness = N1, integrity = N2, engagement = N3]

Queries:

* In
    * level - 0
	* type - effector_ca
    * latency - unknown - an effector CA has no set latency
    * experience_domain -> always responds with [predictable{name:Action, object:EffectorName, value:boolean}, ...] 
		- Action is an action the effector can execute
	* action_domain -> the actions the effector_ca can take

State:
	* parents - parent CAs
	* effectors - the body effectors (1 action per body effector) the CA is responsible for
	* observations - actions last executed - [executed(Action), ...]
	* experiences - experiences from last execution - [Action(EffectorName, Boolean), ...]
	* action_domain - actions the effector can execute - [Action, ...]
	* actuations - actions ready for execution - [Action, ...]
	* wellbeing - wellbeing values - initially [fullness = 100, integrity = 100, engagement = 0]

Lifecycle
  * Created for a body effector
  * Repeatedly
    * Adopted by a CA in its umwelt (new parent) - subscribes to umwelt events from parent
	* Queried for its experience domain (only experiences in counts of actions executed)
    * Handles intent events - sends can_actuate/2 message in response
	* Handles ready_actuation messages - tells the body to prepare to actuate, records the actuations
	* Handles executed event (the body has aggregated and executed actuations) - updates experiences and informs all of wellbeing changes
	* Handles predictions events from parents about the last executed actions - sends prediction_error events
	* Handles wellbeing _tranfer messages from parents
  * Parent CA terminated - unsubscribes from umwelt events originating from the parent

An effector CA has no fixed latency; its unique timeframe is updated after notice of 
execution by the body of its accumulated actuations, but only if some originated from the effector CA.

It does not have a memory of past timeframes; it only remembers its yet-to-be executed actuations and its experiences about the latest
execution. Upon receiving an intent event, an effector CA resets its list of activations. Upon receiving an execution event, an
effector CA recomputes its observations and experiences.
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
	put_state(State1, [parents - [], observations - [], experiences - [], action_domain - ActionDomain, actuations - [], wellbeing - InitialWellbeing], State),
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

handled(query(experience_domain), State, ExperienceDomain) :-
	get_state(State, action_domain, ActionDomain),
	effector_name(State, EffectorName),
	bagof(predictable{name:Action, object:EffectorName, value:boolean}, member(Action, ActionDomain), ExperienceDomain).

handled(query(action_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handled(query(Query), State, Answer) :-
	ca_support : handled(query(Query), State, Answer).

handled(message(adopted, Parent), State, NewState) :-
	% Ignore if adopter is already a parent
	\+ from_parent(Parent, State),
    all_subscribed([ca_terminated - Parent, prediction - Parent, intent - Parent]),
    acc_state(State, parents, Parent, NewState).

handled(message(ready_actuation(Goal), Parent), State, NewState) :-
	goal_action(Goal, State, Action),
	log(info, effector_ca, "~@ is readying action ~p from goal ~p", [self, Action, Goal]),
	% The body is ready to execute the action
	body_actuated(State, Action),
	message_sent(Parent, actuation_ready(Goal)),
	acc_state(State, actuations, Action, NewState).

handled(message(wellbeing_transfer(WellbeingTransfer), _), State, NewState) :-
	wellbeing_transfered(State, WellbeingTransfer, NewState).	

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).	
	 
handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).	

handled(event(intent, Intent, Parent), State, NewState) :-
	well_enough(State),
	log(info, effector_ca, "~@ handling intent ~p from ~w", [self, Intent, Parent]),
	option(goals(Goals), Intent),
	option(id(Id), Intent),
	setof(Goal, (member(Goal, Goals), can_do_goal(Goal, State)), ActuatableGoals), 
	log(info, effector_ca, "~@ can actuate goals ~p from intent ~p", [self, ActuatableGoals, Id]),
	message_sent(Parent, can_actuate(ActuatableGoals)),
	actuations_reset(State, NewState).		

handled(event(executed, _, _), State, NewState) :-
	observations_from_actuations(State, Observations),
	put_state(State, observations, Observations, State1),
	experiences_from_observations(State1, Experiences),
	put_state(State1, experiences, Experiences, State2),
	(wellbeing_changed(State2, UpdatedWellbeing) ->
		put_wellbeing(State2, UpdatedWellbeing, State3)
		;
		State3 = State2
		),
	actuations_reset(State3, NewState).	
	
handled(event(prediction, PredictionPayload, Parent), State, State) :-
	ca_support:prediction_handled(PredictionPayload, Parent, State).

handled(event(Topic, Payload, Parent), State, NewState) :-
	ca_support : handled(event(Topic, Payload, Parent), State, NewState).

%%%%

initial_wellbeing([fullness = 100, integrity = 100, engagement = 0]).

subscribed_to_global_events() :-
	all_subscribed([executed]).

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

effector_name(State, EffectorName) :-
	get_state(State, effectors, [Effector | _]),
	EffectorName = Effector.id.

goal_action(Goal, State, Action) :-
	Goal =.. [Action, EffectorName, true],
	effector_name(State, EffectorName),
	get_state(State, action_domain, ActionDomain),
	member(Action, ActionDomain).

can_do_goal(Goal, State) :-
	goal_action(Goal, State, _).

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
	bagof(executed(Action), member(Action, Actuations), Observations),
	log(effector_ca, debug, "~@ observed ~p from actuations ~p", [self, Observations, Actuations]).

experiences_from_observations(State, Experiences) :-
	log(debug, effector_ca, "~@ is getting experiences from observations in ~p", [self, State]),
	get_state(State, observations, Observations),
	effector_name(State, EffectorName),
	experiences_from_executions(Observations, EffectorName, Experiences),
	log(debug, effector_ca, "~@ experiences ~p from observations ~p", [self, Experiences, Observations]).

experiences_from_executions([], _, []).
experiences_from_executions([executed(Action) | Rest], EffectorName, [Experience | OtherExperiences]) :-
	Experience =.. [Action, EffectorName, true],
	experiences_from_executions(Rest, EffectorName, OtherExperiences).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

% Tell the body to prepare to carry out this actuation (together with possibly others)
body_actuated(State, Action) :-
	action_url(State, Action, ActionUrl), 
	body : actuated(ActionUrl),
	log(debug, effector_ca, "~@ asked body to actuate ~w", [self, Action]).
