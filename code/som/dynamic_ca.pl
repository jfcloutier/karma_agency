/*

Dynamic cognition actor

A dynamic cognition actor is a transient component of the agent's society of mind (SOM). It is a cognition actor (CA) that is born and dies during the lifetime of the SOM.
This is in contrast to the static CAs that exist a priori and in perpetuity (as an interface to the body's permanent sensors and effectors).
Unlike static CAs, a dynamic CA has other CAs as its umwelt.

See [design notes](https://github.com/jfcloutier/karma_system/tree/main/design_notes/cognition_architecture.md)

Messages:

	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `phase_progressed(Phase, StateDeltas, WellbeingDelta)` - sent from itself when a phase has made progress
	* `phase_done(Phase, StateDeltas, WellbeingDelta)` - sent from itself when a phase is done
	* `prediction(Prediction)` - Prediction = prediction{origin:object{type:Type, id:ID}, kind:Kind, value:Value, weight:Weight, confidence:Confidence, by: CA}
	* `prediction_error(PredictionError)` - responding with the correct value to a prediction event where the prediction is incorrect - PredictionError = prediction_error{prediction:Prediction, actual_value:Value, confidence:Confidence, by: CA}
	* `wellbeing_transfer(Wellbeing)` - Wellbeing is wellbeing{fullness: Delta1, integrity:Delta2, engagement:Delta3} - a transfer in either direction of wellbeing
	* `causal_theory(Theory)` - when the Apperception Engine has found a causal theory for the dynamic CA
	
	Events:
	
	* `ca_started([level = Level])`
	* `end_of_phase([phase=Phase, state_deltas=State, wellbeing_delta=WellbeingDelta])`
	* `end_of_timeframe([level=Level, count=Count])`
	* `end_of_life([level=Level])`
    
	For testing only:
	* `phase_before_work([phase=Phase, state_deltas=StateDeltas, wellbeing_delta=WellbeingDelta)`
	* `phase_progressed([phase=Phase, state_deltas=StateDeltas, wellbeing_delta=WellbeingDelta)`
 
Queries:

* level -> Integer > 0
* type -> dynamic_ca
* latency -> Integer (secs)
* umwelt -> CA names
* wellbeing -> Wellbeing

Lifecycle
---------

The lifecycle of a dynamic CA is cyclical and is driven by messages it sends itself to progress through lifecycle phases.

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
    * settings - operational parameters
	* level - hierarchical level (0 is implied for static CAs)
	* latency - the duration of a timeframe
    * parents - parent CAs
    * umwelt - child CAs
	* umwelt_actions - actions available from the umwelt (empy except for level 1 CAs)
    * phase - the name of the current time frame phase
	* predictions_out - predictions made
	* predictions_in - predictions received
	* prediction_errors - prediction errors received
	* observations - current observations
	* experiences - current experiences
	* feeling - current feeling
	* intent - self-assigned goal - none | goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id: IntentId, intent_level: Level, timeframe_index: Index}
	* affordances - reusable plans with an effectiveness score - [affordance{plan: Plan, score: Score}, ...]
	* plans - plans built by the CA to realize its intent and directives received - [plan{goal: Goal, directives: [goal{...} | command{...}, ...]}, ...]
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
	initial_state(Level, Latency, Umwelt, Settings, Wellbeing, InitialState),
	all_subscribed([ca_started, ca_terminated]),
	announce_adoptions(Umwelt),
	log(info, dynamic_ca, "~@ is started", [self]),
	% Start life
	published(ca_started, [level(Level)]),
	phase_transition(InitialState, State).

initial_state(Level, Latency, Umwelt, Settings, Wellbeing, State) :-
	empty_state(EmptyState),
	put_state(EmptyState, 
		[settings - Settings, alive - true, level - Level, latency - Latency, parents - [], umwelt - Umwelt, umwelt_actions - [],
		 causal_theory - none, wellbeing - Wellbeing, phase - initiating,
		 predictions_in - [], predictions_out - [], prediction_errors - [],
		 observations - [], experiences - [],
		 intent - none, plans - [], affordances - [],
		 timeframes - [], timeframe_count - 1, feeling - none], State).

% Set thread locals for convenience
remember_level(Options, Level) :-
	option(level(Level), Options),
	assert(level(Level)).

% Tell all umwelt CAs that they have this CA has a parent
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

% A CA terminated. Remove from umwelt if applicable.
handled(event(ca_terminated, _, Source), State, NewState) :-
	removed_from_umwelt(Source, State, NewState),
	log(info, dynamic_ca, "CA ~w was removed from the umwelt of  ~@", [Source, self]).

% Default non-handling of irrelevant event
handled(event(Topic, Payload, Source), State, State) :-
	ca_support : handled(event(Topic, Payload, Source), State, State).

handled(message(adopted, Parent), State, NewState) :-
	get_state(State, parents, Parents),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(phase_progressed(Phase, StateDeltas, WellbeingDelta), _), State, NewState) :-
	phase_consumes_produces(Phase, _, ProducedProperties),
	log(info, dynamic_ca, "(~@) Phase ~w producing ~p PROGRESSED with state deltas ~p", [self, Phase, ProducedProperties, StateDeltas]),
	merge_phase_deltas(StateDeltas, WellbeingDelta, ProducedProperties, State, NewState).

handled(message(phase_done(Phase, StateDeltas, WellbeingDelta), _), State, NewState) :-
	phase_consumes_produces(Phase, _, ProducedProperties),
	log(info, dynamic_ca, "(~@) Phase ~w producing ~p DONE with state deltas ~p", [self, Phase, ProducedProperties, StateDeltas]),
	merge_phase_deltas(StateDeltas, WellbeingDelta, ProducedProperties, State, State1),
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

% Timestamp and handle a received prediction upon receipt
handled(message(prediction(Prediction), _), State, NewState) :-
	TimeStampedPrediction = Prediction.put(when_received, State.timeframe_count),
	prediction_handled(TimeStampedPrediction, State, State1),
	acc_state(State1, predictions_in, TimeStampedPrediction, ca_support:agency_state_sorter, NewState).

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

% Default non-handling of unsupported queries
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
timeframe_created(State, NewState) :-
	retained_timeframe(State, Timeframe),
	acc_state(State, timeframes, Timeframe, ca_support:agency_state_sorter, State1),
	put_state(State1, phase, initiating, NewState).

retained_timeframe(State, Timeframe) :-
	state{observations:Observations, experiences:Experiences, intent:Intent, plans:Plans, affordances:Affordances, wellbeing:Wellbeing, feeling:Feeling} :< State,
	Timeframe = timeframe{observations:Observations, experiences:Experiences, intent:Intent, plans:Plans, affordances:Affordances, wellbeing:Wellbeing, feeling:Feeling}.

inc_timeframe_count(State, NewState) :-
	get_state(State, timeframe_count, Count),
	Inc is Count + 1,
	log(info, dynamic_ca, "Incremented timeframe count in ~@ to ~w", [self, Inc]),
	put_state(State, timeframe_count, Inc, NewState).

% TODO - Die gracefully before letting others know
end_of_life(State, State) :-
	level(Level),
	published(end_of_life, [level(Level)]).


