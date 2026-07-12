/*

Dynamic cognition actor

A dynamic cognition actor is a transient component of the agent's society of mind (SOM). It is a cognition actor (CA) that is born and dies during the lifetime of the SOM.
This is in contrast to the static CAs that exist a priori and in perpetuity (as an interface to the body's permanent sensors and effectors).
Unlike static CAs, a dynamic CA has other CAs as its umwelt.

See [design notes](https://github.com/jfcloutier/karma_system/tree/main/design_notes)

Messages:

	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `phase_progressed(Phase, StateDeltas, WellbeingDeltas)` - sent from itself when a phase has made progress
	* `phase_done(Phase, StateDeltas, WellbeingDeltas)` - sent from itself when a phase is done
	* `prediction(Prediction)` - Prediction = prediction{origin:object{type:Type, id:ID}, kind:Kind, value:Value, weight:Weight, confidence:Confidence, by: CA} - Name is a reproducible id of the derivation, Object is what the prediction is about distance, trend...)
	* `prediction_error(PredictionError)` - responding with the correct value to a prediction event where the prediction is incorrect - PredictionError = prediction_error{prediction:Prediction, actual_value:Value, confidence:Confidence, by: CA}
	* `wellbeing_transfer(Wellbeing)` - Wellbeing is wellbeing{fullness: Delta1, integrity:Delta2, engagement:Delta3} - a transfer in either direction of wellbeing
	* `causal_theory(Theory)` - when the Apperception Engine has found a causal theory for the dynamic CA
	
	Events:
	
	* `ca_started([level = Level])`
	* `end_of_phase([phase=Phase, state_deltas=State, wellbeing_deltas=WellbeingDeltas])`
	* `end_of_timeframe([level=Level, count=Count])`
	* `end_of_life([level=Level])`
	* `todo([directives=[Directive, ...]])` - A parent tentatively emits directives to its umwelt
	* `abandoned([intent_id=IntentId])` - A CA emits an intent it is abandoning (relevant if ancestor)
	* `intent_completed([intent_id=IntentId])` - A CA emits that its intent's execution has completed
	* `can_seek([directive=Directive])` - An umwelt CA confirms a directive is meaningful to it
	* `cannot_seek([directive=Directive])`  - An umwelt CA confirms a directive is meaningless to it
	* `find_plan([directive=Directive])` - The umwelt CAs are told to each find a plan to realize a directive they had received, if it is meaningful to them
	* `can_execute([directive=Directive, plan_id=PlanId])` - An umwelt CA confirms that it has an executable plan for a directive it received from a parent
	* `cannot_execute([directive=Directive])` - An umwelt CA confirms that it did not find an executable plan for a directive it had received from a parent
	* `execute([directive=Directive])` - A CA tells its umwelt to execute any plan they have to realize a directive they had received
	* `executed([directive=Directive])` - An umwelt CA confirms that it executed a plan to realize a directive it had received
 
Queries:

* level -> Integer > 0
* type -> dynamic_ca
* latency -> Integer (secs)
* umwelt -> CA names
* wellbeing -> Wellbeing
* goal_states -> [GoalState, ...] - The act phase of a CA needs the latest goal states with each unit of work it carries out
* plans -> [Plan, ...]  - The act phase of a CA needs the latest plans with each unit of work it carries out

Lifecycle
---------

The lifecycle of a dynamic CA (sensor and effector CAs are static) is cyclical and is driven by messages it sends itself to progress through lifecycle phases.

The lifecycle of a dynamic CA ends when it decides to terminate itself.

At any point in the lifecycle, the dynamic CA immediately processes all events and messages from other CAs and updates its state.

Executing a lifecycle phase:

1. The CA initializes by creating an initial state with phase `initiating`
2. Transition to the next phase
3. Start a timeboxed thread for this phase with a copy of the CA's state
4. Empty out the CA state properties consumed by the phase
4. A phase task may emit events and messages to its and other CAs
5. On completing a unit of work or on ending (b/c its done or time has expired), the current phase task sends a message to the CA with the modifications to the state and wellbeing.
6. On ending a phase, complete the current lifecyle (if this was the last phase in a timeframe's lifecycle) else goto 2

When all phases of a timeframe have completed:

1. memorize key state properties in this timeframe
2. decide whether to start a new timeframe 

State
-----

Data:
    * parents - parent CAs
    * umwelt - child CAs
	* umwelt_actions - actions available from the umwelt (empy except for level 1 CAs)
    * phase - the name of the current time frame phase
	* predictions_out - predictions made
	* predictions_in - predictions received
	* prediction_errors - prediction errors received
	* observations - current observations
	* experiences - current experiences
	* intent - self-assigned goal - none | goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id: IntentId, intent_level: Level}
	* goal_states - states of goals known to the CA (intent, directives sent and received) - [goal_state{goal: Goal, status: Status, messages: [GoalMessage, ...]}, ...]
	* plans - plans built by the CA to realize its intent and directives received - [plan{id: ID, goal_id: GoalID, directives: [goal{...} | Action, ...], , status:Status, score: Score}, ...]
	* causal_theory - current causal theory induced and abduced from past observations, or `none`
	* wellbeing - wellbeing metrics {fullness:Percent, integrity:Percent, engagement:Percent}
    * timeframes - [Timeframe, ...] - Latest first. A Timeframe carries over from the previous timeframe its observations, experiences, feeling, intent, plans and wellbeing.
    * timeframe_count - the number of timeframes since CA creation
*/

:- module(dynamic_ca, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/goal_states)).
:- use_module(agency(som/plans)).
:- use_module(agency(body)).

% Thread state
:- thread_local level/1, timer/1.

%! ca_name(+Options, -Name) is det
% Get a presumably unique name for a dynamic CA given naming options
ca_name(Options, Name) :-
	option(level(Level), Options),
	option(prefix(Prefix), Options),
	atomic_list_concat([ca, level, Level, Prefix], ":", Name).

%! latency(+Level, -Latency) is det
% The time in seconds allocated to a cognition actor to complete a time frame given its level in the SOM
latency(Level, Latency) :-
	% Latency is max(0.1, 2 ** (Level - 1) / 2).
	Latency is max(0.2, 4 ** (Level - 1) / 2).

%! level_from_name(+Name, -Level) is det
% Get the level of a named CA
level_from_name(Name, Level) :-
	atomic_list_concat([ca, level, LevelAtom | _], ":", Name),
	atom_number(LevelAtom, Level).

%! umwelt(+Name, -Umwelt) is det
% Get the umwelt (a list of CA names) of a named CA
umwelt(Name, Umwelt) :-
	query_answered(Name, umwelt, Umwelt).

% A step is done if it is the current status or comes before it in the goal status progression
is_step_already_taken(Step, Step).

is_step_already_taken(Step, Status) :-
	goal_progress(Prior, Status),
	is_step_already_taken(Step, Prior).

% Worker

init(Options, State) :-
	self(Name),
	log(info, dynamic_ca, "Initiating dynamic ca ~w with ~p", [Name, Options]),
	remember_level(Options, Level),
	option(umwelt(Umwelt), Options),
	option(settings(Settings), Options),
	latency(Level, Latency),
	initial_wellbeing(Wellbeing),
	initial_state(Latency, Umwelt, Settings, Wellbeing, InitialState),
	all_subscribed([ca_started, ca_terminated, abandoned]),
	subscribed_to_umwelt_events(Umwelt),
	announce_adoptions(Umwelt),
	log(info, dynamic_ca, "~@ is started", [self]),
	% Start life
	published(ca_started, [level(Level)]),
	phase_transition(InitialState, State).

initial_state(Latency, Umwelt, Settings, Wellbeing, State) :-
	empty_state(EmptyState),
	put_state(EmptyState, 
		[settings - Settings, alive - true, latency - Latency, parents - [], umwelt - Umwelt, umwelt_actions - [],
		 causal_theory - none, wellbeing - Wellbeing, phase - initiating,
		 predictions_in - [], predictions_out - [], prediction_errors - [],
		 observations - [], experiences - [],
		 intent - none, plans - [], goal_states - [],
		 timeframes - [], timeframe_count - 1, feeling - none], State).

remember_level(Options, Level) :-
	option(level(Level), Options),
	assert(level(Level)).

subscribed_to_umwelt_events(Umwelt) :-
	forall(member(UCA, Umwelt), 
	       all_subscribed([can_seek - UCA, cannot_seek - UCA, can_execute - UCA, cannot_execute - UCA, executed - UCA])).

subscribed_to_parent_events(Parent) :-
	all_subscribed([todo - Parent, find_plan - Parent, execute - Parent]).

announce_adoptions(Umwelt) :-
	concurrent_forall(member(Child, Umwelt), message_sent(Child, adopted)).

signal_processed(control(stopped)) :-
	worker : stopped.

terminated :-
	log(warn, dynamic_ca, "~@ terminating", [self]),
	timer(TimerName),
	timer : stopped(TimerName),
	level(Level),
	published(ca_terminated,
		[level(Level)]),
	log(warn, dynamic_ca, "Terminated ~@", [self]).

% Ignore since the umwelt is constituted at "birth"
handled(event(ca_started, _, _), State, State).

% `todo([directives=[Directive, ...]])`
% A parent broadcast a list of directives (from having formulated a plan)
% Make sure there is a goal_state for each received directive with at least `todo` for each directive
handled(event(todo, Payload, Source), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -todo- with ~p from ~w", [self, Payload, Source]),
	option(directives(Directives), Payload),
	maybe_goals_advanced(Directives, todo, Source, State, NewState).

% From ancestor (if the intent is known)
% `abandoned([intent_id=IntentId])` or `intent_completed([intent_id=IntentId])`
% Get rid of goal states associated with the intent and all plans for these goals
handled(event(Topic, Payload, Source), State, NewState) :-
	memberchk(Topic, [intent_completed, abandoned]),
	log(info, dynamic_ca, "~@ received event ~w with ~p from ~w", [self, Topic, Payload, Source]),
	option(intent_id(IntentId), Payload),
	findall(GoalState, (member(GoalState, State.goals_states), GoalState.goal.intent_id == IntentId), ObsoleteGoalStates ),
	findall(Plan, (member(Plan, State.plans), member(GoalState, ObsoleteGoalStates), Plan.goal_id == GoalState.goal.id), ObsoletePlans),
	dec_state(State, goal_states, ObsoleteGoalStates, NewState1),
	dec_state(NewState1, plans, ObsoletePlans, NewState).

% Received from an umwelt CA confirming that a directive received is relevant.
% If all directives in a plan are can_seek, then the CA publishes to its umwelt the request to find_plan for each directive in the plan.
handled(event(can_seek, Payload, Source), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -can_seek- with ~p from ~w", [self, Payload, Source]),
	option(directive(Directive), Payload),
	% Record the advancement of the sent directive's state
	maybe_goal_advanced(Directive, can_seek, Source, State, NewState1),
	% Update existing plans with status unknown to status possible when now possible
	maybe_make_plans_possible(NewState1, Directive, NewState),
	% If this can_seek directive makes plans possible (plans of the CA in which this directive appears),
	% then ask the umwelt to find plans for each directive in these CA plans
	% This can cause a CA to repeat asking for a plan to be found. Umwelt CAs ignore such repeated requests.
	maybe_ask_to_find_plans(Directive, NewState).

% Received from the umwelt.
% For each plan containing the directive, add the message to the plan's goal's state
% If any directive in the plan got a cannot_seek or cannot_execute from all umwelt CA, the plan's goal's state remains the same (planning)
% because another plan might be found during the acting phase (so no progress is made and there's nothing to publish yet to parent CAs)
handled(event(DirectiveStatus, Payload, Source), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -~w- with ~p from ~w", [self, DirectiveStatus, Payload, Source]),
	member(DirectiveStatus, [cannot_seek, cannot_execute]),
	option(directive(Directive), Payload),
	% Record the deadend-ing of the directive if this is what happened
	maybe_goal_advanced(Directive, DirectiveStatus, Source, State, State1),
	% If the entire umwelt cannot seek the directive, then forget all plans containing the directive
	maybe_abandon_plans_with_directive(Directive, State1, NewState).

% Received from the umwelt.
% For each plan containing the directive, add the message to the plan's goal's state
% If all directives of a plan can_execute then the plan as a whole can_execute.
% If the plan's' goal is a received directive, publish can_execute to parents about the directive.
% If the plan is for an intent, then the CA executes the plan immediately.
handled(event(can_execute, Payload, Source), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -can_execute- with ~p from ~w", [self, Payload, Source]),
	option(directive(Directive), Payload),
	maybe_goal_advanced(Directive, can_execute, Source, State, State1),
    maybe_can_execute_plans_with_directive(Directive, State1, NewState).

% Received from parents
% find_plan([directive=Directive])
% Move the goal_state for the directive to planning (plan-making happens in the acting phase)
handled(event(find_plan, Payload, Source), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -find_plan- with ~p from ~w", [self, Payload, Source]),
	option(directive(Directive), Payload),
	maybe_goal_advanced(Directive, planning, Source, State, NewState).

% Received from parents
% Given a plan for the directive, mark the first non-executed (sub) directive in the plan as executing, if not already
handled(event(execute, Payload, Parent), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -execute- with ~p from ~w", [self, Payload, Parent]),
	option(directive(Directive), Payload),
	maybe_goal_advanced(Directive, executing, Parent, State, State1),
	plan_for_directive(Directive, State1, can_execute, Plan),
	level(Level),
	maybe_plan_execution_advanced(Plan, Parent, Level, State1, NewState).

% Received from umwelt
% A directive sent by the CA or sibling was executed by an umwelt CA
% Mark the received directive as executed if relevant. If all other directives in the plan it is part of were also executed,
% then propagate `executed` to parents for the received directive for which plan completed execution.
% Convert what the CA executed as observations.
handled(event(executed, Payload, Source), State, NewState) :-
	log(info, dynamic_ca, "~@ received event -executed- with ~p from ~w", [self, Payload, Source]),
	option(directive(Directive), Payload),
	maybe_goal_advanced(Directive, executed, Source, State, State1),
	activation_observed(Directive, Source, State1, State2),
	maybe_plans_with_directive_executed(Directive, State2, NewState).

handled(event(ca_terminated, _, Source), State, NewState) :-
	removed_from_umwelt(Source, State, NewState),
	% TODO - cleanup goal_states?
	log(info, dynamic_ca, "CA ~w was removed from the umwelt of  ~@", [Source, self]).

handled(event(Topic, Payload, Source), State, State) :-
	ca_support : handled(event(Topic, Payload, Source), State, State).

handled(message(adopted, Parent), State, NewState) :-
	get_state(State, parents, Parents),
	subscribed_to_parent_events(Parent),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(phase_progressed(Phase, StateDeltas, WellbeingDeltas), _), State, NewState) :-
	phase_consumes_produces(Phase, _, ProducedProperties),
	log(info, dynamic_ca, "(~@) Phase ~w producing ~p PROGRESSED with state deltas ~p", [self, Phase, ProducedProperties, StateDeltas]),
	merge_phase_deltas(StateDeltas, WellbeingDeltas, ProducedProperties, State, NewState).

handled(message(phase_done(Phase, StateDeltas, WellbeingDeltas), _), State, NewState) :-
	phase_consumes_produces(Phase, _, ProducedProperties),
	log(info, dynamic_ca, "(~@) Phase ~w producing ~p DONE with state deltas ~p", [self, Phase, ProducedProperties, StateDeltas]),
	merge_phase_deltas(StateDeltas, WellbeingDeltas, ProducedProperties, State, State1),
    (timeframe_continues(State1) ->
		phase_transition(State1, NewState)
		; 
		Count = State.timeframe_count,
		log(info, dynamic_ca, "~@ ended timeframe ~w", [self, Count]),
		level(Level),
		published(end_of_timeframe, [level(Level), count(Count)]),
		% The next timeframe starts after memorializing the terminated timeframe and with an initial phase
		new_timeframe(State1, NewState)
	).

% Handle a received prediction upon receipt
handled(message(prediction(Prediction), _), State, NewState) :-
	prediction_handled(Prediction, State, State1),
	acc_state(State1, predictions_in, Prediction, ca_support:agency_state_sorter, NewState).

handled(message(prediction_error(PredictionError), _), State, NewState) :-
	acc_state(State, prediction_errors, PredictionError, ca_support:agency_state_sorter, NewState).

handled(message(causal_theory(CausalTheory)), State, NewState) :-
	put_state(State, causal_theory, CausalTheory, NewState).

handled(message(Message, Source), State, NewState) :-
	ca_support : handled(message(Message, Source), State, NewState).

handled(query(level), _, Level) :-
	level(Level).

handled(query(type), _, dynamic_ca).

handled(query(umwelt), State, Umwelt) :-
	get_state(State, umwelt, Umwelt).

handled(query(wellbeing), State, Wellbeing) :-
	get_state(State, wellbeing, Wellbeing).

handled(query(goal_states), State, GoalStates) :-
	get_state(State, goal_states, GoalStates).

handled(query(plans), State, Plans) :-
	get_state(State, plans, Plans).

handled(query(Query), Source, Answer) :-
	ca_support : handled(query(Query), Source, Answer).

removed_from_umwelt(CA, State, NewState) :-
	get_state(State, umwelt, Umwelt),
	member(CA, Umwelt),
	subtract(Umwelt, CA, Umwelt1),
	put_state(State, umwelt, Umwelt1, NewState).

% Maximum fullness and integrity. Neutral engagement.
initial_wellbeing(Wellbeing) :-
	Wellbeing = wellbeing{fullness:1.0, integrity:1.0, engagement:0.5}.

overall_wellbeing(_, 1).

% Not at end of timeframe (there's a next phase)
timeframe_continues(State) :-
	next_phase(State.phase, _).
	
% The next timeframe starts after memorializing the terminated timeframe and with an initial phase
% 
new_timeframe(State, NewState) :-
	get_state(State, alive, true) ->
		timeframe_created(State, State1),
		log(info, dynamic_ca, "New timeframe started for CA ~@", [self]),
		inc_timeframe_count(State1, State2),
		phase_transition(State2, NewState)
	;
		end_of_life(State, NewState).

% Remember the terminated timeframe and set the phase to initiating, otherwise carry over the state
% Remove all executed and cannot_execute plans from the previous timeframe
timeframe_created(State, NewState) :-
	retained_timeframe(State, Timeframe),
	acc_state(State, timeframes, Timeframe, ca_support:agency_state_sorter, State1),
	findall(Plan, (member(Plan, State1.plans), memberchk(Plan.status, [cannot_execute, executed])), DroppedPlans),
	dec_state(State1, plans, DroppedPlans, State2),
	put_state(State2, phase, initiating, NewState).

retained_timeframe(State, Timeframe) :-
	state{observations:Observations, experiences:Experiences, intent:Intent, plans:Plans, wellbeing:Wellbeing, feeling:Feeling} :< State,
	Timeframe = timeframe{observations:Observations, experiences:Experiences, intent:Intent, plans:Plans, wellbeing:Wellbeing, feeling:Feeling}.

inc_timeframe_count(State, NewState) :-
	get_state(State, timeframe_count, Count),
	Inc is Count + 1,
	log(info, dynamic_ca, "Incremented timeframe count in ~@ to ~w", [self, Inc]),
	put_state(State, timeframe_count, Inc, NewState).

% TODO - Die gracefully before letting others know
end_of_life(State, State) :-
	level(Level),
	published(end_of_life, [level(Level)]).

% Try advancing the states of a set of goals the same way.
maybe_goals_advanced([], _, _, State, State).

maybe_goals_advanced([Goal | Rest], Step, Source, State, NewState) :-
	maybe_goal_advanced(Goal, Step, Source, State, State1),
	maybe_goals_advanced(Rest, Step, Source, State1, NewState).

% Advance a goal state's status if it already exists, is not at a deadend and this is a step forward; 
% else create the goal state and add it to the CA's state
maybe_goal_advanced(Goal, Step, Source, State, NewState) :-
	(current_goal_state(Goal, State, GoalState) ->
		maybe_take_step(GoalState, Step, Source, GoalState1)
		;
	    new_goal_state(Goal, Step, Source, GoalState1)
	),
	log(info, dynamic_ca, "~@'s goal maybe advanced to ~p from step ~w", [self, Goal, GoalState1, Step]),
	goal_state_updated(GoalState1, State, NewState).

% New goal state. Received is true if this is a goal the CA is expected to realize.
% Received is false if the goal is one the CA exepcts its umwelt to realize.
new_goal_state(Goal, todo, Source, GoalState) :-
	GoalState = goal_state{goal: Goal, received:true, status: todo, messages: [goal_message{about:todo, from:Source}]}.

new_goal_state(Goal, Step, Source, GoalState) :-
	Step \= todo,
	GoalState = goal_state{goal: Goal, received:false, status: Step, messages: [goal_message{about:Step, from:Source}]},
	log(info, dynamic_ca, "~@ has new goal state ~p from ~w", [self, GoalState, Step]).

% goal_state{goal: Goal, status: Status, messages: [GoalMessage, ...]}
current_goal_state(Goal, State, GoalState) :-
	member(GoalState, State.goal_states),
	Goal.id == GoalState.goal.id.

% Move a goal state forward if not already past the step and not at a deadend. Keep track of the message in any case.
maybe_take_step(GoalState, Step, Source, UpdatedGoalState) :-
	((is_step_already_taken(GoalState.status, Step) ; is_deadend_status(Step))->
		GoalState1 = GoalState
		;
		GoalState1 = GoalState.put(status, Step),
		log(info, dynamic_ca, "~@ updated goal state to ~p from ~w", [self, GoalState1, Step])
	),
	goal_state_message_added(GoalState1, goal_message{about:Step, from:Source}, UpdatedGoalState).

% Replace or add the goal state to the CA state
goal_state_updated(GoalState, State, NewState) :-
	((member(GS, State.goal_states),
	GS.goal.id == GoalState.goal.id) ->
		delete(State.goal_states, GS, GoalStates1)
		;
		GoalStates1 = State.goal_states
    ),
	put_state(State, goal_states, [GoalState | GoalStates1], NewState).

% A directive sent to the umwelt was executed.
% It becomes the observation of an activation, the attempted impacting of an umwelt experience.
activation_observed(Directive, Source, State, NewState) :-
	activation_observation(Source, Directive, State.timeframe_count, Observation),
	log(info, dynamic_ca, "~@ observed activation ~p as ~p", [self, Directive, Observation]),
	acc_state(State, observations, Observation, ca_support:agency_state_sorter, NewState).

% observation{origin:object{type:dynamic_ca, id:ID}, kind:activation, value:ActionOrDirectiveID-TimeframeIndex, confidence:Confidence, by:CA, id:Id}
activation_observation(Source, Directive, TimeframeIndex, Observation) :-
	self(CA),
	GoalId = Directive.id,
	ObservationWithoutId = observation{origin:object{type:dynamic_ca, id:Source}, kind:activation, value:GoalId-TimeframeIndex, confidence:1.0, by:CA},
	observation_with_id(ObservationWithoutId, Observation).

maybe_plans_with_directive_executed(Directive, State, State) :-
	findall(Plan, (member(Plan, State.plans), member(Directive, Plan.directives)), Plans),
	forall(member(Plan, Plans),
		(is_plan_executed(Plan, State) ->
			known_directive(Plan.goal_id, State, Directive),
			published(executed, [directive(Directive)])
			;
			true
		)
	).

% Are all directives in the plan executed?
is_plan_executed(Plan, State) :-
	get_state(State, goal_states, GoalStates),
	forall(member(Directive, Plan.directives), is_directive_executed(Directive, GoalStates)),
	log(info, dynamic_ca, "~@'s plan ~p is executed", [self, Plan]).

is_directive_executed(Directive, GoalStates) :-
	member(GoalState, GoalStates),
	GoalState.goal.id == Directive.id,
	GoalState.status == executed.

% Mark the next un-executed directive as executing.
% The directive will be actually executed in the act phase, when it gets to it.
maybe_plan_execution_advanced(Plan, Source, Level, State, NewState) :-
	Level > 1,
	next_directive_to_be_executed(Plan, State, Directive),
	UpdatedPlan = Plan.put(status, executing),
	acc_state(State, plans, UpdatedPlan, ca_support:agency_state_sorter, State1),
	maybe_goal_advanced(Directive, executing, Source, State1, NewState).

% If at level 1, the plan is a sequence of commands.
% Do nothing now. The plan (for an executing directive) will be executed in the `act` phase.
% The plan is to be executed at once.
maybe_plan_execution_advanced(_, _, Level, State, State) :-
	Level = 1.
maybe_plan_execution_advanced(_, _, _, State, State).

% Find the first directive in the plan that has not executed
next_directive_to_be_executed(Plan, State, Directive) :-
	member(Directive, Plan.directives),
	current_goal_state(Directive, State, GoalState),
	\+ is_step_already_taken(executed, GoalState.status),
	log(info, dynamic_ca, "~@'s next directive to be executed in plan ~p is ~p", [self, Plan, Directive]).

% A given message was received from each CA in the umwelt
received_from_entire_umwelt(About, GoalState, State) :-
	get_state(State, umwelt, Umwelt),
	forall(member(Child, Umwelt), (member(Message, GoalState.messages), message{about:About, from:Child } :< Message)),
	log(info, dynamic_ca, "~@'s ~p has received messages ~w from entire umwelt", [self, GoalState, About]).

% Now that we know the umwelt can seek to achieve a directive sent to it,
% See which plans containing this directive are now possible (i.e. might be executable)
maybe_make_plans_possible(State, CanSeekDirective, NewState) :-
	findall(Plan, (member(Plan, State.plans), member(CanSeekDirective, Plan.directives), is_plan_now_possible(Plan, State)), PossiblePlans),
	% Change the status of these plans to `possible`
	findall(UpdatedPlan, (member(PlanToUpdate, PossiblePlans), UpdatedPlan = PlanToUpdate.put(status, possible)), UpdatedPlans),
	log(info, dynamic_ca, "~@ plans made possible due to can_seek ~p: ~p", [self, CanSeekDirective, UpdatedPlans]),
	% Update the CA's plans
	acc_state(State, plans, UpdatedPlans, ca_support:agency_state_sorter, NewState).

% If plans of the CA in which this directive appears are now `possible` (all their directives including this one can be sought by one or more umwelt CAs),
% then ask the umwelt to find plans for each directive in these plans of the CA (now that we have established that these plans only contain directives meaningful to the umwelt) 
% It is expected that this ask will be ignored by a receiving umwelt CA if the directive is not relevant to it or if it already found a plan for the directive.
maybe_ask_to_find_plans(Directive, State) :-
	findall(Plan, (member(Plan, State.plans), member(Directive, Plan.directives), Plan.status == possible), PossiblePlans),
	ask_to_find_plans_for_directives_in_plans(PossiblePlans, State).

% A plan is `possible` if it still only 'unknown' and all its directives have been reported by some umwelt CA as meaningful (progress can be made with them)
is_plan_now_possible(Plan, State) :-
	Plan.status == unknown,
	forall(member(Directive, Plan.directives), is_directive_meaningful(Directive, State)).

is_directive_meaningful(Directive, State) :-
	goal_state_of(Directive, State, GoalState),
	goal_state_is_meaningful(GoalState).

goal_state_of(Directive, State, GoalState) :-
	member(GoalState, State.goal_states),
	GoalState.goal.id == Directive.id.

ask_to_find_plans_for_directives_in_plans([], _).

ask_to_find_plans_for_directives_in_plans([Plan | Rest], State) :-
	log(info, dynamic_ca, "~@ is asking its umwelt to find a plan for all directives in ~p", [self, Plan]),
	ask_to_find_plans_for_directives(Plan.directives, State),
	ask_to_find_plans_for_directives_in_plans(Rest, State).

ask_to_find_plans_for_directives([], _).

ask_to_find_plans_for_directives([Directive | Rest], State) :-
	log(info, dynamic_ca, "~@ is asking its umwelt to find a plan for directive ~p", [self, Directive]),
	published(find_plan, [directive(Directive)]),
	ask_to_find_plans_for_directives(Rest, State).

% If the entire umwelt cannot seek the directive (it is now somehow meaningless to all umwelt CAs), then all plans containing the directive become cannot_execute
maybe_abandon_plans_with_directive(Directive, State, NewState) :-
	umwelt_cannot_seek(Directive, State) ->
		plans_with_directive(Directive, State, Plans),
		findall(UpdatedPlan, (member(Plan, Plans), UpdatedPlan = Plan.put(status, cannot_execute)), UpdatedPlans),
		% Update the CA's plans, removing duplicates via the sorter
		log(info, dynamic_ca, "~@ is forced to abandon plans ~p because umwelt cannot seek ~p", [self, UpdatedPlans, Directive]),
		acc_state(State, plans, UpdatedPlans, ca_support:agency_state_sorter, NewState)
		;
		NewState = State.

plans_with_directive(Directive, State, Plans) :-
	findall(Plan, (member(Plan, State.plans), member(Directive, Plan.directives)), Plans).

umwelt_cannot_seek(Directive, State) :-
	forall(member(UmweltCA, State.umwelt), umwelt_ca_cannot_seek(UmweltCA, Directive, State)).

umwelt_ca_cannot_seek(UmweltCA, Directive, State) :-
	goal_state_of(Directive, State, GoalState),
	member(Message, GoalState.messages),
	message{source:UmweltCA, about:cannot_seek} :< Message.

% If all directives of a plan can_execute then the plan as a whole can_execute and the CA publishes this to parents for the plan's' goal.
% If the plan if for an intent, then the CA executes the plan.
maybe_can_execute_plans_with_directive(Directive, State, NewState) :-
	plans_with_directive(Directive, State, Plans),
	forall(member(Plan, Plans), maybe_can_execute_plan(Plan, State, NewState)).

% If the plan is possible and the plan's goal is not already `executing` and all of its directives `can_execute`, then
% execute the plan if it is for the CA's intent,
% else let the parents know that the received directive has a plan that 'can_execute''.
maybe_can_execute_plan(Plan, State, NewState) :-
	(Plan.status == possible,
	forall(member(Directive, Plan.directives), directive_can_execute(Directive, State))) ->
		can_execute_plan(Plan, State, NewState)
		;
		true.

% The status of a goal given its id
goal_status(GoalId, State, Status) :-
	member(GoalState, State.goal_states),
	GoalId == GoalState.goal.id,
	Status = GoalState.status.

% At least on umwelt CA has communicated that it can execute the directive
directive_can_execute(Directive, State) :-
	member(GoalState, State.goal_states),
	GoalState.goal.id == Directive.id,
	member(Message, GoalState.messages),
	Message.about == can_execute.

% The plan can be executed, so now what?
% If the plan is for the CA's intent, tell the umwelt to execute (the plans for) its directives since the CA initiates carrying out its intent.
% Else inform parents that the CA has an executable plan for the received directive 
can_execute_plan(Plan, State, NewState) :-
	log(info, dynamic_ca, "~@ can execute ~p", [self, Plan]),
	current_goal_state(Plan.goal, State, GoalState),
	(is_plan_for_intent(Plan, State, _) ->
		GoalState1 = GoalState.put(status, executing),
	    goal_state_updated(GoalState1, State, State1),
		UpdatedPlan = Plan.put(status, executing),
		acc_state(State1, plans, UpdatedPlan, ca_support:agency_state_sorter, NewState),
		forall(member(Directive, Plan.directives), published(execute, [directive_id(Directive.id)]))
		;
		% Move the plan's goal (a received directive) to can_execute
		GoalState1 = GoalState.put(status, can_execute),
	    goal_state_updated(GoalState1, State, State1),
		UpdatedPlan = Plan.put(status, can_execute),
		acc_state(State1, plans, UpdatedPlan, ca_support:agency_state_sorter, NewState),
		published(can_execute, [directive_id(Plan.goal_id)])
	).

known_directive(directive_id, State, Directive) :-
	get_state(State, goal_states, GoalStates),
	member(GoalState, GoalStates),
	goal_state{goal: Directive} :< GoalState,
	Directive.id == directive_id.

% The plan is for the CA's intent
is_plan_for_intent(Plan, State, Intent) :-
	Intent = State.intent,
	Intent \= none,
	Intent.id == Plan.goal_id.


