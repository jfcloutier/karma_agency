/*
Dynamic cognition actor

A dynamic cognition actor is a transient component of the agent's society of mind (SOM). It is a cognition actor (CA) that is born and dies during the lifetime of the SOM.
This is in contrast to a static CA that exists a priori and in perpetuity (as an interface to a body's permanent sensor or effector).
Unlike static CAs, dynamic CA has as its umwelt other CAs.

A dynamic CA is born with a permanent, assigned umwelt of a small number of CAs from the abstraction layer below.
A dynamic CA's umwelt establishes a "parent-children" relationship. A dynamic CA can be in more than one umwelt.
A dynamic CA is kept updated about the kinds of beliefs its umwelt holds so it can make meaningful predictions about what its umwelt actually believes.

A dynamic CA operates one timeframe at a time, each timeframe representing a "thick now". The more abstract the dynamic CA, the (qualitatively) longer each timeframe.
The life of a dynamic CA is a sequence of timeframes.

During the current timeframe, a dynamic CA observes its umwelt, updates its own beliefs from observations, and then maybe acts to impact (persist or terminate) a salient belief.

A dynamic CA predicts the beliefs held in its umwelt by using its causal model and applying it to the latest observations it made of its umwelt. It may receive prediction errors.
A dynamic CA's observations made in a timeframe is the resulting set of uncontested predictions made plus the prediction errors received in that timeframe.

A dynamic CA maintains and updates a causal model/theory (a generated logical program) that makes sense of its umwelt. It is, at first, a trivial one model ("nothing changes").
As the dynamic CA's history of observations grows and as the prediction error rate rises, the dynamic CA eventually requests a new causal theory from the Apperception Engine,
one with hopefully better accuracy than the last.

Near the end of the current timeframe, a dynamic CA updates its beliefs by detecting patterns in previous and current observations of the CAs in its umwelt.
It then attaches normativity (pleasantness/unpleasantness) to each belief based on updated and correlated wellbeing measures and gradients.

The wellbeing measures of a dynamic CA are fullness (think energy level), integrity (think health) and engagement (think relevance).
They are modified by the efforts the dynamic CA makes (cognitive and physical), what it senses (collisions, feeding), and by the passing of time (aging, recovery).
The wellbeing of a dynamic CA goes up and down from the dynamic CA's own activity and sensing but also from diffusion (equilibration) across CAs along parent-child relationships.
The wellbeing of a dynamic CA (think stress) affects what the dynamic CA can do (low-fullness behavior vs high-fullness behavior, etc.) and what it decides to do.

Before ending a timeframe, a dynamic CA may decide on a goal to realize. This goal is either a salient-enough belief to persist/terminate, or it is a goal imposed on it by a parent.
To realize the selected goal, the dynamic CA formulates a novel policy or reuses a time-tested one.
A policy is a set of goals delegated to the umwelt. The policy is likely to be effective, according to the causal theory, at impacting the belief held by the dynamic CA,
because the belief is derived from observed umwelt beliefs the policy seeks to impact.
The CA then attempts to have the policy realized by its umwelt.

Before terminating the current timeframe, the dynamic CA decides whether to birth a new CA (if it succeeds at enlisting enough CAs in the layer below to form a new umwelt),
terminate itself (if its wellbeing is consistently too low), or do neither.
If it decides to keep on living, it commits the current timeframe to memory, trimming it as constrained by its wellbeing, and then, finally, it initiates a new timeframe.

Over the next timeframes, the dynamic CA will assess the success or failures of policies executed in prior timeframes by
evaluating how closely they correlate, if at all, to sought belief changes. A dynamic CA will tend to reuse policies successful at achieving a given goal.

Communications from other CAs
-----------------------------
Messages:

* In
	* `adopted(Parent)` - when added to the umwelt of a dynamic CA one level up
	* `causal_theory(Theory)` - when the Apperception Engine has found a causal theory for the dynamic CA

* Out to a parent (when ending the current time frame)	
	* `can_actuate(Goals)` - responding to intent events - the dynamic CA commits (until intent completed) to realizing each of these goals (can be empty) - a goal is a belief the dynamic CA is requested to initiate, persist or terminate
	* `actuation_ready(Goal)` responding to ready_actuation message - the dynamic CA has successfully and transitively primed the body for execution of the goal
	* `prediction_error(PredictionPayload, ActualValue)` - responding with the correct value to a prediction event where the prediction is incorrect
		
* In from a parent
	* ready_actuation(Goal, Boolean) - a parent communicates that the dynamic CA was selected (or not) to realize a goal by actuating whatever policy the dynamic CA chose or built
	* wellbeing_transfer(WellbeingValues) - payload is [fullness = Delta1, integrity = Delta2, engagement = Delta3] - a transfer in either direction of wellbeing

Events:

* In
	* topic: executed, payload: [] - body actuation was triggered - whatever actuations were readied by the dynamic CA, if any, were carried out by the body's effectors

* In from parents (accumulated until end of time frame)
	* topic: intent, payload: [id = IntentID, directives = [Directive, ...]] - a parent CA communicates a list of directives (prioritized goals) it intends to execute if possible
	* topic: intent_completed, payload: [id = IntentID, status =  SuccessOrFailure]
													 - a parent CA has completed preparations to realize its intent, or has failed to
	                                                 - the dynamic CA's timeframe can now terminate, propagating the readied actuations awaiting execution into the next timeframe if intent was successful
	* topic: prediction, payload: [belief = Belief] - a parent makes a prediction about what the dynamic CA believes
	
* Out
    * topic: ca_started, payload: [level = Level]
	* topic: wellbeing_changed, payload: WellbeingPayload  - [fullness = N1, integrity = N2, engagement = N3]
	* topic: belief_domain_changed, payload: BeliefDomain - [predicatble(name: BeliefName, object: BeliefObject, value: ValueDomain), ...]
	* topic: causal_theory_wanted, payload: [pinned_predicates = PinnedPredicated, pinned_objects = PinnedObjects]
  
* Queries:

* In
  * level -> Integer > 0
  * type -> dynamic_ca
  * latency -> Integer (secs)
  * umwelt -> CA names
  * belief_domain -> [Predictable, ...]
  * wellbeing -> [fullness = N1, integrity = N2, engagement = N3]

Lifecycle
---------

The lifecycle of a dynamic CA (sensor and effector CAs are static) is cyclical and is driven by messages it sends itself to progress through lifecycle stages.

The lifecycle of a dynamic CA ends when it decides to terminate itself.

At any point in the lifecycle, the dynamic CA immediately processes all events and messages from other CAs and updates its state.

Executing a lifecycle stage:

1. Receive a lifecycle message from itself with an updated state
2. Merge the current state with the updated state
3. Start a timeboxed, async task for this stage with the new state
4. The task may emit events and messages to other CAs
5. On ending the task (b/c its done or time has expired),
   send the next stage lifecycle message to self with the modified state

Pattern: Message to self -> Task -> Follow-up message to self

begin -> Initialize new time frame from the previous one -> observe
observe -> Make predictions about umwelt beliefs -> believe
believe -> Update own beliefs from observations -> act
act -> Select a goal (a held belief to impact) and attempt to realize it via a policy directing the umwelt to change its beliefs -> assess
assess -> Evaluate the accuracy of the causal model and the effectiveness of past policies -> age
age -> Maybe modify the SOM (cytosis/apoptosis) and diffuse stress -> begin

State
-----

Data:
    * parents - parent CAs
    * umwelt - child CAs
    * time_frame - the current time frame
	  * start_time
	  * phase - the time frame phase
	  * wellbeing - wellbeing tokens
	  * predictions_out - predictions made
	  * predictions_in - predictions received
	  * prediction_errors -prediction errors received
	  * beliefs - current beliefs
	  * causal_theory - current causal theory
	  * directives - received directives
	  * goal - selected goal
	  * policy - policy to achieve the goal
    * history - [TimeFrame, ...]
*/

:- module(dynamic_ca, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(library(uuid)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/phase)).

% Thread statis state - TODO - revise
:- thread_local level/1, timer/1.

%! name_from_level(+Level, -Name) is det
% Get a unique name for a dynamic CA given the level it occupies in the SOM
name_from_level(Level, Name) :-
	uuid(ID),
	atomic_list_concat([ca, level, Level, id, ID], ":", Name).

%! latency(+Level, -Latency) is det
% The time in seconds allocated to a cognition actor to complete a time frame given its level in the SOM
latency(Level, Latency) :-
	Latency is (2**Level).

%! level(+Name, -Level) is det
% Get the level of a named CA
level_from_name(Name, Level) :-
	atomic_list_concat([ca, level, LevelAtom, id, _], ":", Name),
	atom_number(LevelAtom, Level).

%! umwelt(+Name, -Umwelt) is det
% Get the umwelt (a list of CA names) of a named CA
umwelt(Name, Umwelt) :-
	query_answered(Name, umwelt, Umwelt).

% Eagerness is between 0 and 1 with 1 the maximum level of enthusiasm to be recruited in an umwelt
recruit(Name, Eagerness) :-
	query_answered(Name, recruit, Eagerness).

% Worker

init(Options, State) :-
	log(info, dynamic_ca, "Initiating ca with ~p", [Options]),
	empty_state(EmptyState),
	remember_level(Options, Level),
	option(umwelt(Umwelt), Options),
	self(Name),
	latency(Level, Latency),
	empty_time_frame(TimeFrame),
	put_state(EmptyState, [latency - Latency, parents - [], umwelt - Umwelt, time_frame - TimeFrame, history - []], InitialState),
	subscribed_to_events(),
	announce_adoptions(Umwelt),
	% Start life
	published(ca_started, [level(Level)]),
	phase_transition(InitialState, State).

remember_level(Options, Level) :-
	option(level(Level), Options),
	assert(level(Level)).

announce_adoptions(Umwelt) :-
	forall(member(Child, Umwelt), message_sent(Child, adopted)).

% TODO - prediction/1, prediction_error/1, belief_domain/1 etc.
subscribed_to_events :-
	forall(member(Topic, [ca_started, ca_terminated, prediction, belief_domain, directives]),
		subscribed(Topic)).

% Empty time frame 
empty_time_frame(EmptyFrame) :-
	get_time(Now),
	put_state(frame{}, [start_time - Now, phase - initiating, wellbeing - [], predictions_out - [], predictions_in  - [], prediction_errors - [], beliefs - [], directives -[]], EmptyFrame).

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

handled(message(end_of_phase(UpdatedState), _), State, NewState) :-
	phase_ended(State, UpdatedState, State1),
    (life_continues(State1, State2) ->
	phase_transition(State2, NewState)
	; 
	end_of_life(State1)).

handled(message(Message, Source), State, NewState) :-
	ca_support : handled(message(Message, Source), State, NewState).

handled(query(level), _, Level) :-
	level(Level).

handled(query(type), _, dynamic_ca).

handled(query(umwelt), State, Umwelt) :-
	get_state(State, umwelt, Umwelt).

/*
A dynamic CA can be recruited only if it has a causal theory.
The probability that it accepts goes up:
* the less it participates in umwelts already
* the higher its wellbeing
*/
handled(query(recruit), State, Eagerness) :-
	get_state(State, causal_theory, _),
	get_state(State, parents, Parents),
	length(Parents, NumParents),
	wellbeing(State, Wellbeing),
    Eagerness is Wellbeing / ((NumParents + 1) ** 2).

handled(query(recruit), _, 0).

handled(query(Query), Source, Answer) :-
	ca_support : handled(query(Query), Source, Answer).

% Ignored
handled(event(ca_started, Options, Source), State, State).
% Ignore if the terminated CA is not in the umwelt, else update the umwelt
handled(event(ca_terminated, Options, Source), State, NewState) :-
	removed_from_umwelt(Source, State, NewState),
	log(info, dynamic_ca, "CA ~w was removed from the umwelt of  ~@", [Source, self]).
% TODO
handled(event(prediction, Belief, Source), State, State).
% TODO
handled(event(prediction_error, Prediction, Source), State, State).
% TODO
handled(event(goal, Goal, Source), State, State).
% TODO
handled(event(belief_domain, BeliefDomain, Source), State, State).

handled(event(Topic, Payload, Source), State, State) :-
	ca_support : handled(event(Topic, Payload, Source), State, State).

removed_from_umwelt(CA, State, NewState) :-
	get_state(State, umwelt, Umwelt),
	member(CA, Umwelt),
	subtract(Umwelt, Source, Umwelt1),
	put_state(State, umwelt, Umwelt1, NewState).

% TODO

wellbeing(_, 1).

% Merge the CA and phase states
phase_ended(State, UpdatedState, NewState).

% Decide whether to go on living and whether to start a new time frame
life_continues(State, NewState).

% Die gracefully and let others know
end_of_life(State).