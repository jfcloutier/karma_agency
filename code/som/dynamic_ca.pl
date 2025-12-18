/*
Dynamic cognition actor

A dynamic cognition actor is a transient component of the agent's society of mind (SOM). It is a cognition actor (CA) that is born and dies during the lifetime of the SOM.
This is in contrast to a static CA that exists a priori and in perpetuity (as an interface to a body's permanent sensor or effector).
Unlike static CAs, dynamic CA has as its umwelt other CAs.

A dynamic CA is born with a permanent, assigned umwelt of a small number of CAs from the abstraction layer below.
A dynamic CA's umwelt establishes a "parent-children" relationship. A dynamic CA can be in more than one umwelt.
A dynamic CA is kept updated about the kinds of experiences (experiential domain) its umwelt holds so it can make meaningful predictions about what its umwelt actually experiences.

A dynamic CA operates one timeframe at a time, each timeframe representing a "thick now". The more abstract the dynamic CA, the (qualitatively) longer each timeframe.
The life of a dynamic CA is a sequence of timeframes.

During the current timeframe, a dynamic CA observes its umwelt, updates its own experiences from observations, and then maybe acts to verify or impact (persist or terminate) a salient experience.

A dynamic CA predicts the experiences held in its umwelt by using its causal model and applying it to the latest observations it made of its umwelt. It may receive prediction errors.
A dynamic CA's observations made in a timeframe is the resulting set of uncontested predictions made plus the prediction errors received in that timeframe.

An experience composed of prediction errors is surprising. The larger the ratio of errors, the more surprising it is.
A CA has low confidence in a surprising experience and may not act to impact it until it has sufficient confidence. Confidence grows in an experience that persists, however surprising.
A CA may however act to validate a surprising experience to test if it will last.

A dynamic CA maintains and updates a causal model/theory (a generated logical program) that makes sense of its umwelt. It is, at first, a trivial one model ("nothing changes").
As the dynamic CA's history of observations grows and as the prediction error rate rises, the dynamic CA eventually requests a new causal theory from the Apperception Engine,
one with hopefully better accuracy than the last.

Near the end of the current timeframe, a dynamic CA updates its experiences by detecting patterns in previous and current observations of the CAs in its umwelt.
It then attaches normativity (good vs. bad) to each experience based on updated and correlated wellbeing measures and gradients.

The wellbeing measures of a dynamic CA are fullness (think energy level), integrity (think health) and engagement (think relevance).
They are modified by the efforts the dynamic CA makes (cognitive and physical), what it senses (collisions, feeding), and by the passing of time (aging, recovery).
The wellbeing of a dynamic CA goes up and down from the dynamic CA's own activity and sensing but also from diffusion (equilibration) across CAs along parent-child relationships.
The wellbeing of a dynamic CA (think stress) affects what the dynamic CA can do (low-fullness behavior vs high-fullness behavior, etc.) and what it decides to do.

Before ending a timeframe, a dynamic CA may decide on a goal to realize. This goal is either a salient-enough experience to persist/terminate, or it is a goal imposed on it by a parent.
The salience of an experience is a function of confidence and associated wellbeing.

To realize the selected goal, the dynamic CA formulates a plan or reuses a time-tested affordance (a reusable plan).
A plan is a set of prioritized goals (directives) sent to the umwelt. The plan is likely to be effective, according to the causal theory, at impacting the experience held by the dynamic CA,
because the experience is derived from observed umwelt experiences the plan seeks to impact.
The CA expresses its intent to act and expects (perhaps wrongly) to have the plan realized by its umwelt.

Before terminating the current timeframe, the dynamic CA decides whether to birth a new CA (if it succeeds at enlisting enough CAs in the layer below to form a new umwelt),
terminate itself (if its wellbeing is consistently too low), or do neither.
If it decides to keep on living, it commits the current timeframe to memory, trimming it as constrained by its wellbeing,
perhaps requests an update to its causal theory, and then, finally, it initiates a new timeframe.

Over the next timeframes, the dynamic CA will assess the success or failures of plans executed in prior timeframes by
evaluating how closely they correlate, if at all, to sought experience changes. If a plan appears to be effective, a dynamic CA will package the plan and its goal in an affordance.
A dynamic CA will tend to use affordances that appear to have been successful at achieving a given goal.

Communications from other CAs
-----------------------------
Messages:

* In
	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `phase_done(PhaseEndState, WellbeingDeltas)` - sent from itself when a phase is done
	* `causal_theory(Theory)` - when the Apperception Engine has found a causal theory for the dynamic CA
	* `die` - when the dynamic CA has reached end-of-life
	
* Out to a parent (when ending the current time frame)	- thus also in from a child
	* `can_actuate(Goals)` - responding to intent events - the dynamic CA commits (until intent completed) to realizing each of these goals (can be empty) 
	                       - a goal is an experience the dynamic CA is requested to initiate, persist or terminate
	* `actuation_ready(Goal)` responding to ready_actuation message - the dynamic CA has successfully and transitively primed the body for execution of the goal
	* `prediction(Prediction)` - Prediction = prediction{name:Name, object:Object, value:Value, confidence:Confidence, by: CA, for:CAs} - Name is a reproducible id of the derivation, Object is what the prediction is about distance, trend...)
	* `prediction_error(PredictionError)` - responding with the correct value to a prediction event where the prediction is incorrect - PredictionError = prediction_error{prediction:Prediction, actual_value:Value, confidence:Confidence, by: CA}
		
* In from a parent
	* ready_actuation(Goal, Boolean) - a parent communicates that the dynamic CA was selected (or not) to realize a goal by actuating whatever plan the dynamic CA chose or built

* In 
	* wellbeing_transfer(Wellbeing) - Wellbeing is wellbeing{fullness: Delta1, integrity:Delta2, engagement:Delta3} - a transfer in either direction of wellbeing

Events:

* In
	* topic: executed, payload: [] - body actuation was triggered - whatever actuations were readied by the dynamic CA, if any, were carried out by the body's effectors


* In from parents (accumulated until end of time frame)
	* topic: intent, payload: [id = IntentID, directives = [Directive, ...]] - a parent CA communicates a list of directives (prioritized goals) it intends to execute if possible
	* topic: intent_completed, payload: [id = IntentID, status =  SuccessOrFailure]
													 - a parent CA has completed preparations to realize its intent, or has failed to
	                                                 - the dynamic CA's timeframe can now terminate, propagating the readied actuations awaiting execution into the next timeframe if intent was successful

* Out
    * topic: ca_started, payload: [level = Level]
	* topic: end_of_phase, payload: [phase=Phase, state=State],
	* topic: end_of_timeframe, payload: [level=Level]
	* topic: end_of_life, payload: [level=Level]
	* topic: causal_theory_wanted, payload: [pinned_predicates = PinnedPredicated, pinned_objects = PinnedObjects]
  
* Queries:

* In
  * level -> Integer > 0
  * type -> dynamic_ca
  * latency -> Integer (secs)
  * umwelt -> CA names
  * experience_domain -> [Predictable, ...]
  * wellbeing -> Wellbeing

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
4. The task may emit events and messages to its and other CAs
5. On ending the task (b/c its done or time has expired), send a message to the CA with the modified state
6. Add to the CA state what the task produced, including contributions to wellbeing

When all phases of a timeframe have completed:

1. memorize key state properties in this timeframe
2. decide whether to start a new timeframe 

State
-----

Data:
    * parents - parent CAs
    * umwelt - child CAs
    * phase - the name of the current time frame phase
	* predictions_out - predictions made
	* predictions_in - predictions received
	* prediction_errors - prediction errors received
	* observations - current observations
	* experiences - current experiences
	* directives_in - received directives
	* goal - selected goal {experience:Experience, impact:Impact, priority:Priority}
	* plan - plan to achieve the goal [directive, ...]
	* causal_theory - current causal theory or `none`
	* affordances - [goal-directives, ...]
	* wellbeing - wellbeing metrics {fullness:Percent, integrity:Percent, engagement:Percent}
    * timeframes - [Timeframe, ...] - Latest first. A Timeframe remembers only observations, goal and plan
*/

:- module(dynamic_ca, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/phase)).

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
	Latency is max(0.1, 2 ** (Level - 1) / 2).

%! level_from_name(+Name, -Level) is det
% Get the level of a named CA
level_from_name(Name, Level) :-
	atomic_list_concat([ca, level, LevelAtom | _], ":", Name),
	atom_number(LevelAtom, Level).

%! umwelt(+Name, -Umwelt) is det
% Get the umwelt (a list of CA names) of a named CA
umwelt(Name, Umwelt) :-
	query_answered(Name, umwelt, Umwelt).

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
	subscribed_to_events(),
	announce_adoptions(Umwelt),
	% Start life
	published(ca_started, [level(Level)]),
	phase_transition(InitialState, State).

initial_state(Latency, Umwelt, Settings, Wellbeing, State) :-
	empty_state(EmptyState),
	put_state(EmptyState, 
		[settings - Settings, alive - true, latency - Latency, parents - [], umwelt - Umwelt,
		 causal_theory - none, wellbeing - Wellbeing, phase - initiating,
		 predictions_in - [], predictions_out - [], prediction_errors - [],
		 observations - [], experiences - [], affordances - [],
		 goal - none, plan - [],
		 timeframes - [], timeframe_count - 0], State).

remember_level(Options, Level) :-
	option(level(Level), Options),
	assert(level(Level)).

announce_adoptions(Umwelt) :-
	concurrent_forall(member(Child, Umwelt), message_sent(Child, adopted)).

subscribed_to_events :-
	forall(member(Topic, [ca_started, ca_terminated]),
		subscribed(Topic)).

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

handled(message(adopted, Parent), State, NewState) :-
	get_state(State, parents, Parents),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(phase_done(PhaseState, WellbeingDeltas), _), State, NewState) :-
	Phase = PhaseState.phase,
	log(info, dynamic_ca, "Phase ~w done for ~@ with state ~p", [Phase, self, PhaseState]),
	merge_phase_state(Phase, PhaseState, WellbeingDeltas, State, State1),
    (timeframe_continues(State1) ->
		phase_transition(State1, NewState)
		; 
		log(info, dynamic_ca, "Timeframe ended"),
		level(Level),
		published(end_of_timeframe, [level(Level)]),
		% The next timeframe starts after memorializing the terminated timeframe and with an initial phase
		new_timeframe(State1, NewState)
	).

handled(message(prediction(Prediction), _), State, NewState) :-
	% TODO - Maybe reply with a prediction error
	acc_state(State, predictions_in, Prediction, NewState).

handled(message(prediction_error(PredictionError), _), State, NewState) :-
	acc_state(State, prediction_errors, PredictionError, NewState).

handled(message(causal_theory(CausalTheory)), State, NewState) :-
	put_state(State, causal_theory, CausalTheory, NewState).

handled(message(die), State, NewState) :-
	put_state(State, alive, false, NewState).

handled(message(Message, Source), State, NewState) :-
	ca_support : handled(message(Message, Source), State, NewState).

handled(query(level), _, Level) :-
	level(Level).

handled(query(type), _, dynamic_ca).

handled(query(umwelt), State, Umwelt) :-
	get_state(State, umwelt, Umwelt).

handled(query(experience_domain), State, ExperienceDomain) :-
	get_state(State, experiences, Experiences),
	domain_from_experiences(Experiences, ExperienceDomain).

handled(query(wellbeing), State, Wellbeing) :-
	get_state(State, wellbeing, Wellbeing).

handled(query(Query), Source, Answer) :-
	ca_support : handled(query(Query), Source, Answer).

% Ignored
handled(event(ca_started, _, _), State, State).
% Ignore if the terminated CA is not in the umwelt, else update the umwelt
handled(event(ca_terminated, _, Source), State, NewState) :-
	removed_from_umwelt(Source, State, NewState),
	log(info, dynamic_ca, "CA ~w was removed from the umwelt of  ~@", [Source, self]).

% TODO
% handled(event(goal, Goal, Source), State, State).
handled(event(goal, _, _), State, State).

handled(event(Topic, Payload, Source), State, State) :-
	ca_support : handled(event(Topic, Payload, Source), State, State).

removed_from_umwelt(CA, State, NewState) :-
	get_state(State, umwelt, Umwelt),
	member(CA, Umwelt),
	subtract(Umwelt, CA, Umwelt1),
	put_state(State, umwelt, Umwelt1, NewState).


initial_wellbeing(Wellbeing) :-
	Wellbeing = wellbeing{fullness:1.0, integrity:1.0, engagement:0.0}.

overall_wellbeing(_, 1).

% Merge wellbeing deltas and properties changed by the phase
merge_phase_state(Phase, PhaseState, WellbeingDeltas, State, NewState) :-
	phase_consumes_produces(Phase, _, ProducedProperties),
	merge_phase_state_properties(ProducedProperties, PhaseState, State, State1),
	merge_wellbeing(State1, WellbeingDeltas, NewState).
	
merge_phase_state_properties([], _, State, State).

merge_phase_state_properties([Property | Rest], PhaseState, State, NewState) :-
	get_state(PhaseState, Property, PropertyValue),
	put_state(State, Property, PropertyValue, State1),
	merge_phase_state_properties(Rest, PhaseState, State1, NewState).

merge_wellbeing(State, WellbeingDeltas, NewState) :-
	log(info, dynamic_ca, "Merge wellbeing deltas ~p into state ~p", [WellbeingDeltas, State]),
	get_state(State, wellbeing, Wellbeing),
	apply_wellbeing_deltas(Wellbeing, WellbeingDeltas, Wellbeing1),
	put_state(State, wellbeing, Wellbeing1, NewState).

apply_wellbeing_deltas(Wellbeing, WellbeingDeltas, NewWellbeing) :-
	log(info, dynamic_ca, "Apply wellbeing deltas ~p to wellbeing ~p", [WellbeingDeltas, Wellbeing]),
	Fullness is max(Wellbeing.fullness + WellbeingDeltas.fullness, 0),
	Integrity is max(Wellbeing.integrity + WellbeingDeltas.integrity, 0),
	Engagement is max(Wellbeing.engagement + WellbeingDeltas.engagement, 0),
	NewWellbeing = wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement}.

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

% Remember the terminated timeframe and set the phase to initiating
timeframe_created(State, NewState) :-
	retained_timeframe(State, Timeframe),
	acc_state(State, timeframes, Timeframe, State1),
	put_state(State1, phase, initiating, NewState).

retained_timeframe(State, Timeframe) :-
	state{observations:Observations, experiences:Experiences, goal:Goal, plan:Plan} :< State,
	Timeframe = timeframe{observations:Observations, experiences:Experiences, goal:Goal, plan:Plan}.

inc_timeframe_count(State, NewState) :-
	log(info, dynamic_ca, "Incrementing timeframe count in ~p", [State]),
	get_state(State, timeframe_count, Count),
	Inc is Count + 1,
	put_state(State, timeframe_count, Inc, NewState).

% TODO - Die gracefully and let others know
end_of_life(State, State) :-
	level(Level),
	published(end_of_life, [level(Level)]).

% TODO
% domain_from_experiences(Experiences, []).
domain_from_experiences(_, []).
