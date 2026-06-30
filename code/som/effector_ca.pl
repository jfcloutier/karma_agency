/*
An effector CA is a static (a priori) cognition actor that communicates with a body effector to actuate it.

The body considers each possible action a given device can take (always sequentially) as defining a separate effector.
Same-device effectors are combined in one effector_ca.

An effector CA receives from its parents intended actions as commands in the context of an intent.

The effector CA updates its wellbeing based on its participation and sends an event
about its change in wellbeing.

Each action taken affects the CA's wellbeing. It reduces the CA's fullness and integrity by 1 (they start at 100), and increases
its engagement by 1 (it starts at 0). An effector CA can not actuate if its fullness or integrity is at 0. An effector CA publishes
changes to its wellbeing and effects transfers to and from its parents.

An effector CA neither observes nor experiences.

An effector CA can receive commands todo from multiple plans prior to their executions.
These plans either participate in realizing the same intent or different intents.
The effector CA groups commanded actions (to actualize) by intent.
The number of repeated actions to be actualized under an intent can not exceed the maximum number of repeated actions in a plan participating in the intent.
When a command under an intent is to be executed, all accumulated instances of the commanded action under the instance are readied, if not already.
When the intent is abandoned, the effector CA forgets about yet-to-be actualized commanded actions under the intent.

Messages:

* In
	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `wellbeing_transfer(Wellbeing)` - Wellbeing is wellbeing{fullness: Delta1, integrity:Delta2, engagement:Delta3} - a transfer of wellbeing - an effector only consumes fullness and integrity
    * `ready_actuation(Command)`
	* `actuation_executed(Command)`
	
Events:

* In from an ancestor
	* `intent_completed, [intent_id=IntentId]`
	* `abandoned, [intent_id=IntentId]`

* In from a parent
	* `todo, [directives=[Command, ...]]` - Commands a parent tentatively wants effector CAs in its umwelt to execute
	* `find_plan([directive=Command]) - A parent asks if the directive is a command for an action the effector CA could get actualized by the body

* Out
	* `ca_started, [level = Level]`
	* `can_seek([directive=Command])` - The effector CA has the command's action in its repertoire - in response to todo
	* `cannot_seek([directive=Command])` - The effector CA does not have the command's action in its repertoire - in response to todo
	* `can_execute([directive=Command])` - The effector CA has the command's action in its repertoire - in response to find_plan
	* `cannot_execute([directive=Command])` - The effector CA does not have the command's action in its repertoire - in response to find_plan

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
	* actuations - actuations intended and readied - [actuation{action:Action, intent_id:IntentId, status:Status}, ...] -- Status in [none, ready, executed]
	* wellbeing - wellbeing values - initially wellbeing{fullness:1.0, integrity:1.0, engagement:1.0}

An effector CA has no fixed latency; its unique timeframe is updated after notice of 
execution by the body of its accumulated actuations, but only if some originated from the effector CA.

It does not have a memory of past timeframes; it only remembers intent-associated actuations.
Upon receiving an `intent_completed` event, an effector CA forgets its list of actuations for that intent.
*/

:- module(effector_ca, []).

:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(utils(logger)).
:- use_module(utils(tools)).
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
    all_subscribed([ca_terminated - Parent, todo - Parent, find_plan - Parent, execute - Parent]),
    acc_state(State, parents, Parent, ca_support:agency_state_sorter, NewState).

% An effector CA only consumes fullness and integrity, Its engagement remains constant at 1.0.
handled(message(wellbeing_transfer(WellbeingTransfer), _), State, NewState) :-
	wellbeing_transfered(State, wellbeing{fullness:WellbeingTransfer.fullness, integrity:WellbeingTransfer.integrity, engagement:0}, NewState).	

% Pick an actuation of the command with status=none,
% 	Tell the body to ready actuation of the action
% 	Update the actuation status to ready
handled(message(ready_actuation(Command), _), State, NewState) :-
	actuation_readied(Command, State, NewState).

% Mark the actuation as executed
handled(message(actuation_executed(Command), _), State, NewState) :-
	actuation_executed(Command, State, NewState).
	
handled(message(Message, Source), State, NewState) :-
	ca_support: handled(message(Message, Source), State, NewState).	
	 
handled(event(ca_terminated, _, Parent), State, NewState) :-
    get_state(State, parents, Parents),
    member(Parent, Parents),
    unsubscribed_from(Parent),
    dec_state(State, parents, Parent, NewState).

% Accumulates not-yet-ready actuations for actionable commands targeting the effector CA in the context of an intent.
% The maximum of action actuations accumulated until an intent is completed is the largest number of such actions in a plan for that intent.
% Tells the parent whether it can or cannot seek each (possibly repeated) todo command.
handled(event(todo, Payload, _), State, NewState) :-
	log(info, effector_ca, "~@ received todo with ~p", [self, Payload]),
	option(directives(Commands), Payload),
	can_and_cannot_do(Commands, State, CanDoCommands, CantDoCommands),
	% Can-do commands all have an id
	actuations_accumulated(CanDoCommands, State, NewState),
	forall(member(CanDo, CanDoCommands), published(can_seek, [directive=CanDo])),
	forall(member(CannotDo, CantDoCommands), published(cannot_seek, [directive=CannotDo])).

% The effector CA has a plan if there's an actuation for the command (from prior todo event).
handled(event(find_plan, Payload, _), State, State) :-
	log(info, effector_ca, "~@ finding plan for ~p", [self, Payload]),
	option(directive(Command), Payload),
	(is_actuation(Command, State) ->
		published(can_execute, [directive=Command])
		;
		published(cannot_execute, [directive=Command])
	).

% Forget all actuations for the intent when completed or abandoned
handled(event(IntentEvent, Payload, _), State, NewState) :-
	memberchk(IntentEvent, [intent_completed, abandoned]),
	option(intent_id(IntentId), Payload),
    intent_actuations_removed(IntentId, State, NewState).	

handled(event(Topic, Payload, Parent), State, NewState) :-
	ca_support : handled(event(Topic, Payload, Parent), State, NewState).

initial_wellbeing(wellbeing{fullness:1.0, integrity:1.0, engagement:1.0}).

subscribed_to_global_events() :-
	all_subscribed([intent_completed, abandoned]).

action_domain(State, ActionDomain) :-
	findall(Action, 
		(
			member(Effector, State.effectors), Action = Effector.capabilities.action), ActionDomain).

% command{effector_ca: CA_ID, action: Action, intent_id: IntentId}
% Ignore commands meant for another effector CA
% Divide the others into can do and can't do
can_and_cannot_do(Commands, State, CanDoCommands, CannotDoCommands) :-
	self(CA),
	findall(CACommand, (member(CACommand, Commands), CACommand.effector_ca == CA), MyCommands),
	findall(CanDoCommand, (member(CanDoCommand, MyCommands), can_do(CanDoCommand, State)), CanDoCommands),
	subtract(MyCommands, CanDoCommands, CannotDoCommands),
	log(debug, effector_ca, "~@ found can do ~p and can't do ~p", [self, CanDoCommands, CannotDoCommands]).

% TODO - also check if wellbeing allows taking the in-domain action
can_do(Command, State) :-
	get_state(State, action_domain, ActionDomain),
	memberchk(Command.action, ActionDomain).

% actuations = [actuation{intent_id: IntentId, action: Action, status: none|ready|executed}]
actuations_accumulated([], State, State).

actuations_accumulated(Commands, State, NewState) :-
	[Command | _] = Commands,
	IntentId = Command.intent_id,
	findall(Action, (member(Cmd, Commands), Action = Cmd.action), Actions),
	clumped(Actions, ClumpedActions),
	effects_added(ClumpedActions, IntentId, State, NewState).

effects_added([], _, State, State).

effects_added([Action-Count | Rest], IntentId, State, NewState) :-
	log(debug, effector_ca, "~@ adding effects ~p with intent ~w", [self, [Action-Count | Rest], IntentId]),
	get_state(State, actuations, Actuations),
	findall(Actuation, (member(Actuation, Actuations), actuation{intent_id: IntentId, action:Action} :< Actuation), Effects),
	length(Effects, CurrentCount),
	DeltaCount is Count - CurrentCount,
	(DeltaCount > 0 ->
		replicate(actuation{intent_id:IntentId, action:Action, status:none}, DeltaCount, NewActuations),
		append(Actuations, NewActuations, UpdatedActuations),
		put_state(State, actuations, UpdatedActuations, State1),
		effects_added(Rest, IntentId, State1, NewState)
		;
		effects_added(Rest, IntentId, State, NewState)
	).

is_actuation(Command, State) :-
	actuation_for(Command, _, State, _),	
	!.
	
actuation_readied(Command, State, NewState) :-
	actuation_for(Command, none, State, Actuation),
	!,
	body_actuated(State, Command.action),
	actuation_updated(Actuation, status, ready, State, NewState).

actuation_readied(_, State, State).

actuation_executed(Command, State, NewState) :-
	actuation_for(Command, ready, State, Actuation),
	!,
	actuation_updated(Actuation, status, executed, State, NewState).

actuation_executed(_, State, State).

actuation_for(Command, Status, State, Actuation) :-
	self(Self),
	command{effector_ca: Self, action:Action, intent_id:IntentId} :< Command,
	get_state(State, actuations, Actuations),
	member(Actuation, Actuations),
	actuation{action:Action, intent_id:IntentId, status: Status} :< Actuation.

% Tell the body to prepare to carry out this actuation (the body accumulates them until told to execute them all)
body_actuated(State, Action) :-
	action_url(State, Action, ActionUrl), 
	body : actuated(ActionUrl),
	log(debug, effector_ca, "~@ asked body to actuate ~w ~w times", [self, Action]).

action_url(State, Action, ActionUrl) :-
	member(Effector, State.effectors), Action == Effector.capabilities.action, ActionUrl = Effector.url.

actuation_updated(Actuation, Property, Value, State, NewState) :-
	delete(State.actuations, Actuation, Actuations1),
	Actuation1 = Actuation.put(Property, Value),
	put_state(State, actuations, [Actuation1 | Actuations1], NewState).

intent_actuations_removed(IntentId, State, NewState) :-
	get_state(State, actuations, Actuations),
	findall(Actuation, (member(Actuation, Actuations), Actuation.intent_id == IntentId), IntendedActuations),
	dec_state(State, actuations, IntendedActuations, NewState).


% ============================ UNUSED ==========================

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

effector_name(State, EffectorName) :-	
	get_state(State, effectors, [Effector | _]),
	EffectorName = Effector.id.	
