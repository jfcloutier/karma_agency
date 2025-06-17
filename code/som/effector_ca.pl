/*
An effector CA is an a priori cognition actor that communicates with a body effector to actuate actions.

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA receives from its parents intents to carry out policies, responds with readiness for directives within
its action domain, and then may be told to actuate by telling the body to prepare to execute actions as directed.

An effector's actions are "terminal" policies (i.e. not involving other CAs); their names are actuations the body can execute.

An effector CA believes or not in the body having last executed a certain number of the effector CA's actions/terminal policies. 

An effector CA:

* receives the message `adopted` when added to the umwelt of a CA

* subscribes to events from its parents:
	* topic: intent, payload: [policy = Policy] - a parent CA communicates a policy it built that it intends to execute if possible
	* topic: actuation, payload: [policy = Policy] - a parent communicates a policy it built that it is actuating (execution pending)
	* topic: prediction, payload: [belief = Belief] - a parent makes a prediction about how many of a given action the effector CA believes were executed

* subscribes to events from any source:
	* topic: executed, payload: [] - the actuated policy was executed

* sends messages to a parent in response to events received:
    * intent event -> can_actuate(IntentPayload, Directive) - telling the parent it can actuate a parent's directive
    * intent event -> cannot_actuate(IntentPayload) - telling the parent this effector CA can't do any part of the intended policy
    * prediction event -> prediction_error(PredictionPayload, ActualValue) - error in how many times a given action command was predicted to have been executed

* like all CAs, an effector CA responds to queries about
    * name
    * level - 0
    * latency - unknown - an effector CA has no set latency
    * belief_domain -> always responds with [predictable{name:count, objects:[Action, ...], values:positive_integer}] 
		- Action is the external name for `executed(Action)`, the factual observation that the Action was executed by the body
    * policy_domain -> always responds with [Action, ...], e.g. [spin, reverse_spin]

Lifecycle
  * Created for a body effector
  * Repeatedly
    * Adopted by a CA in its umwelt (new parent) - subscribes to umwelt events from parent
    * Queried for its policy domain (what policies - actions -  it can actuate)
	* Queried for its belief domains (only believes in counts of actions executed)
    * Handles intent events - sends can_actuate/2 or cannot_actuate/2 events in response
	* Handles actuation events - tells the body to prepare to actuate, records the actuations
	* Handles executed events (the body has aggregated and executed actuations) - updates count/2 beliefs
	* Handles predictions about the last executed actions - sends prediction_error events
    * Parent CA terminated - unsubscribes from umwelt events originating from the parent

An effector CA has no fixed latency; its unique timeframe is updated after notice of 
execution by the body of its accumulated actuations, but only if some originated from the effector CA.

It also does not have a memory of past timeframes; it only remembers its yet-to-be executed actuations and its beliefs about the last
execution of its actuations.
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
	put_state(State1, [parents - [], action_domain - ActionDomain, actuations - [], observations - [], beliefs - []], State),
	subscribed_to_events(),
	published(ca_started, [level(0)]).

%% Actor termination - only when agency terminates

signal_processed(control(stopped)) :-
	all_unsubscribed,
	worker : stopped.

terminated :-
	log(warn, effector_ca, "Terminated").

%% Actor interactions

handled(query(name), _, Name) :-
	self(Name).

handled(query(type), _, effector_ca).

handled(query(level), _, 0).

handled(query(latency), _, unknown).

handled(query(belief_domain), State, [predictable{name:count, objects:ActionDomain, values:positive_integer}]) :-
	get_state(State, action_domain, ActionDomain).

handled(query(policy_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handled(query(Query), State, Answer) :-
	ca_support : handled(query(Query), State, Answer).

handled(message(adopted, Parent), State, NewState) :-
    all_subscribed([ca_terminated - Parent, prediction - Parent, intent - Parent, actuation - Parent]),
    acc_state(State, parents, Parent, NewState).

handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).
	 
handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

handled(event(intent, IntentPayload, Parent), State, State) :-
	option(policy(ParentPolicy), IntentPayload),
	setof(Directive, (member(Directive, ParentPolicy), can_do(Directive, State)), Directives), 
    [_ | _] = Directives,
	!,
	foreach(member(Directive, Directives), message_sent(Parent, can_actuate(IntentPayload, Directive))).

handled(event(intent, IntentPayload, Parent), State, State) :-
	message_sent(Parent, cannot_actuate(IntentPayload)).

handled(event(actuation, ActuationPayload, _), State, NewState) :-
	option(policy(ParentPolicy), ActuationPayload),
	bagof(ActionsDirected, (member(Directive, ParentPolicy), how_to(Directive, State, ActionsDirected)), Actions),
	flatten(Actions, AllActions),
	foreach(member(Action, AllActions), actuated(State, Action)), 
	acc_state(State, actuations, AllActions, NewState).

handled(event(executed, _, _), State, NewState) :-
	observations_from_actuations(State, Observations),
	put_state(State, observations, Observations, State1),
	beliefs_from_observations(State1, Beliefs),
	put_state(State1, beliefs, Beliefs, NewState).

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

subscribed_to_events() :-
	all_subscribed([ca_terminated, executed]).

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

how_to(command(Action), State, [Action]) :-
	can_do(command(Action), State), !.

how_to(goal(count(Action, N)), State, Actions) :-
	get_state(State, action_domain, ActionDomain),
	member(Action, ActionDomain),
	!,
	length(Actions, N),
	maplist(=(Action), Actions).

how_to(_, _, []).

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
actuated(State, Action) :-
	action_url(State, Action, ActionUrl), 
	body : actuated(ActionUrl).