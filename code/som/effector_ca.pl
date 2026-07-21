/*
An effector CA is a static (a priori) cognition actor that communicates with a body effector to actuate it.

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.

Same-device effectors are combined in one effector CA, i.e. an effector can potentially take different actions (e.g. the left wheel can spin and reverse-spin)

An effector CA receives from its parents intended actions as commands in the context of an intent.

The effector CA updates its wellbeing based on its participation and sends an event
about its change in wellbeing.

Each action taken affects the CA's wellbeing. It reduces the CA's fullness and integrity by 1 (they start at 100), and increases
its engagement by 1 (it starts at 0). An effector CA can not actuate if its fullness or integrity is at 0. An effector CA publishes
changes to its wellbeing and effects transfers to and from its parents.

An effector CA neither observes nor percepts.

An effector CA can receive commands todo from multiple plans prior to their executions.
These plans either participate in realizing the same intent or different intents.
The effector CA groups commanded actions (to actualize) by intent.
The number of repeated actions to be actualized under an intent can not exceed the maximum number of repeated actions in a plan participating in the intent.
When a command under an intent is to be executed, all accumulated instances of the commanded action under the instance are readied, if not already.
When the intent is abandoned, the effector CA forgets about yet-to-be actualized commanded actions under the intent.

Messages:

* In
	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `fullness_transfer(Fullness)` - Fullness is received and is added to current wellbeing
    * `ready_actuation(Command)` - Tell the body to ready for imminent actuation, if the command targets the effector CA and wellbeing allows
	* `actuations_executed` - A movement was executed by the body
	
Events:

* Out
	* `ca_started, [level = Level]`
	* `wellbeing, [current=Wellbeing] - The effector CA publishes its wellbeing whenever it changes

Queries:

* In
    * level - 0
	* type - effector_ca
	* action_domain -> the actions the effector_ca can take

State:
	* parents - parent CAs
	* effectors - the body effectors (1 action per body effector) the CA is responsible for
	* action_domain - actions the effector can execute - [Action, ...]
	* wellbeing - wellbeing values - initially wellbeing{fullness:1.0, integrity:1.0, engagement:1.0}

An effector CA has no latency and is purely reactive.

TODO - Repair of lost integrity when needed

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

%% Actor creation

init(Options, State) :-
	log(info, effector_ca, "Initiating with ~p", [Options]), 
	empty_state(EmptyState), 
	option(
		effectors(Effectors), Options), 
	put_state(EmptyState, effectors, Effectors, State1), 
	action_domain(State1, ActionDomain),
	initial_wellbeing(InitialWellbeing),
	put_state(State1, [parents - [], action_domain - ActionDomain, wellbeing - InitialWellbeing], State),
	published(ca_started, [level(0)]).

%% Actor termination - only when agency terminates

signal_processed(control(stopped)) :-
	all_unsubscribed,
	worker : stopped.

terminated :-
	log(warn, effector_ca, "Terminated").

%% Actor interactions

handled(query(type), _, effector_ca).

% FOR DEBUGGING
handled(query(level), _, 0).

% FOR DEBUGGING
handled(query(wellbeing), State, Wellbeing) :-
    get_state(State, wellbeing, Wellbeing).

% FOR DEBUGGING
handled(query(effector_name), State, EffectorName) :-	
	get_state(State, effectors, [Effector | _]),
	EffectorName = Effector.id.	

handled(query(action_domain), State, ActionDomain) :-
	get_state(State, action_domain, ActionDomain).

handled(query(Query), State, Answer) :-
	ca_support : handled(query(Query), State, Answer).

handled(message(adopted, Parent), State, NewState) :-
	% Ignore if adopter is already a parent
	\+ from_parent(Parent, State),
    all_subscribed([ca_terminated - Parent]),
    acc_state(State, parents, Parent, ca_support:agency_state_sorter, NewState).

% Tell the body to ready actuation of the action
handled(message(ready_actuation(Command), _), State, NewState) :-
	actuation_readied(Command, State, NewState).

% A movement was completed.
handled(message(actuations_executed, _), State, NewState) :-
	actuations_executed(State, NewState).
	
handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).	
	 
handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

handled(event(Topic, Payload, Parent), State, NewState) :-
	ca_support : handled(event(Topic, Payload, Parent), State, NewState).

initial_wellbeing(wellbeing{fullness:1.0, integrity:1.0, engagement:1.0}).

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

% Check if action in domain and wellbeing allows taking it
can_do(Command, State) :-
	command{effector_ca:Name, action:Action} :< Command,
	self(Name),
	get_state(State, action_domain, ActionDomain),
	memberchk(Action, ActionDomain),
	get_state(State, wellbeing, Wellbeing),
	Wellbeing.fullness > 0,
	Wellbeing.integrity > 0.

% If applicable, ready actuation of the command
% main is sending message(ready_actuation(command{action:reverse_spin,effector_ca:'effector:tacho_motor-outA'}),main) to effector:tacho_motor-outA
%  '09:26:23.890630' warn [worker] Exit error(type_error(dict,command),_4276) of effector:tacho_motor-outA
actuation_readied(Command, State, NewState) :-
	can_do(Command, State),
	!,
	body_actuated(State, Command.action, NewState).

actuation_readied(Command, State, State) :-
	log(debug, effector_ca, "~@ can not ready actuation from command ~p", [self, Command]).

% All actuations in a movement were executed, publish current wellbeing.
% TODO - effect repair if needed
actuations_executed(State, State) :-
	get_state(State, wellbeing, Wellbeing),
	published(wellbeing, [current=Wellbeing]).

% Tell the body to prepare to carry out this actuation 
% (the body accumulates them until told to execute them all)
% Assume the body will execute so update wellbeing.
body_actuated(State, Action, NewState) :-
	action_url(State, Action, ActionUrl), 
	body : actuated(ActionUrl),
	log(debug, effector_ca, "~@ asked body to actuate ~w", [self, Action]),
	wellbeing_changed(Action, State, UpdatedWellbeing),
	put_state(State, wellbeing, UpdatedWellbeing, NewState).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

% Each action executed decrements fullness by 1 (energy spent),
% integrity by 1 (wear and tear), 
% and increments engagement by 1 (we acted in the world).
% Fails if wellbeing was not changed.
wellbeing_changed(_, State, UpdatedWellbeing) :-
	get_state(State, wellbeing, Wellbeing),
    Delta = 0.01, % TODO - separate fullness, wear and tear, maybe differs per action?
	UpdatedWellbeing = Wellbeing.add(wellbeing{fullness: -Delta, integrity: -Delta, engagement: 0}),
	log(debug, effector_ca, "~@ update wellbeing to ~p", [self, UpdatedWellbeing]).
