/*
Cognition actor (CA)

Events:

* In and out
  * topic: ca_started, payload: level(Level)
  * topic: ca_terminated, payload: level(Level)
  * topic: prediction, payload: Belief = belief(PropertyName, Value) | belief(RelationName, ObjectName1, ObjectName2)
  * topic: prediction_error, payload: [predicted - Belief, actual - Belief, confidence - Float]
  * topic: directive, payload: goal(BeliefName, Impact) | command(PolicyName) = Directive
  * topic: directive_status, payload: directive_report(Directive, rejected | ready | executed_as(PolicyName))
  * topic: belief_domains_changed
* Out
  * topic: causal_theory_wanted, payload: [pinned_predicates - PinnedPredicated, pinned_objects - PinnedObjects]
  * topic: wellbeing_changed, payload: [fullness - N1, integrity - N2, engagement - N3]

Messages:

* In and Out
  * adopted
  * wellbeing_exchange_request([fullness - N1, integrity - N2, engagement - N3])
  * wellbeing_exchanged([fullness - N1, integrity - N2, engagement - N3])

* In
  * causal_theory_found(Theory)

* Queries received:
  * level - Integer > 0
  * umwelt - CA names
  * belief_domains - [BeliefDomain, ...]
  * wellbeing_counts - [fullness - N1, integrity - N2, engagement - N3]
    
Thread locals:
    * level - 1..? - the level of the CAs it is responsible for
    * timer - the frame timer

State:
    * causal_theory - the CA's causal theory (can be absent)
    * parents - parent CAs
    * umwelt - child CAs
	* buffered_events - events received and suspended while ending
    * frame - the current frame
	  * start_time
	  * status - initiating, started, ending or ended
	  * wellbeing - wellbeing tokens
	  * predictions_out - predictions made
	  * predictions_in - predictions received
	  * prediction_errors -prediction errors received
	  * beliefs - current beliefs
	  * causal_theory - current causal theory
	  * goal - current goal
	  * policy - policy to achieve the goal
    * history - [Frame, ...]
	* 
*/

:- module(ca, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(worker)).
:- use_module(library(uuid)).
:- use_module(agency(som/ca_support)).

% Thread statis state
:- thread_local level/1, timer/1, buffered_events/1.

%! name_from_level(+Level, -Name) is det
% Get a unique name for a CA given the level it occupies in the SOM
name_from_level(Level, Name) :-
	uuid(ID),
	atomic_list_concat([ca, level, Level, id, ID], ":", Name).

%! latency(+Level, -Latency) is det
% The time allocated to a cognition actor to complete a time frame given its level in the SOM
latency(Level, Latency) :-
	Latency is (2**Level).

%! level(+Name, -Level) is det
% Get the level of a named CA
level_from_name(Name, Level) :-
	atomic_list_concat([ca, level, LevelAtom, id, _], ":", Name),
	atom_number(LevelAtom, Level).
	% query_answered(Name, level, Level).

%! umwelt(+Name, -Umwelt) is det
% Get the umwelt (a list of CA names) of a named CA
umwelt(Name, Umwelt) :-
	query_answered(Name, umwelt, Umwelt).

% Eagerness is between 0 and 1 with 1 the maximum level of enthusiasm to be recruited in an umwelt
recruit(Name, Eagerness) :-
	query_answered(Name, recruit, Eagerness).

% Worker

init(Options, State) :-
	log(info, ca, "Initiating ca with ~p", [Options]),
	empty_state(EmptyState),
		remember_level(Options, Level),
	option(umwelt(Umwelt),
		Options),
	self(Name),
	latency(Level, Latency),
	empty_frame(Frame),
	put_state(EmptyState, [parents - [], umwelt - Umwelt, frame - Frame, history - [], buffered_events - [], status -  initiating], State),
	subscribed_to_events(),
	announce_adoptions(Umwelt),
	clock_started(Name, Latency).

remember_level(Options, Level) :-
	option(level(Level),
		Options),
	assert(level(Level)).

announce_adoptions(Umwelt) :-
	forall(member(Child, Umwelt), message_sent(Child, adopted)).

% TODO - prediction/1, prediction_error/1, belief_domain/1 etc.
subscribed_to_events :-
	forall(member(Topic, [ca_started, ca_terminated, prediction, prediction_error, belief_domain, policy_domain, directive, directive_status]),
		subscribed(Topic)).

clock_started(Name, Delay) :-
	call_at_interval(Name,
		framer,
		message_sent(Name, tick),
		Delay,
		TimerName),
	assert(timer(TimerName)),
	% Send first tick now
	message_sent(tick).

% Empty time frame 
empty_frame(EmptyFrame) :-
	get_time(Now),
	put_state(frame{}, [start_time - Now, wellbeing - [], predictions_out - [], predictions_in  - [], prediction_errors - [], beliefs - []], EmptyFrame).

signal_processed(control(stopped)) :-
	worker : stopped.

terminated :-
	log(warn, ca, "~@ terminating", [self]),
	timer(TimerName),
	timer : stopped(TimerName),
	level(Level),
	published(ca_terminated,
		[level(Level)]),
	log(warn, ca, "Terminated ~@", [self]).

% Time to start a timeframe
handled(message(tick, _), State, NewState) :-
	(get_state(State, history, []) ->
	published(ca_started,
			[level(Level)]),
		State1 = State;
	frame_ended(State, State1)),
	frame_started(State1, NewState).

handled(message(adopted, Parent), State, NewState) :-
	get_state(State, parents, Parents),
    put_state(State, parents, [Parent | Parents], NewState).

handled(message(Message, Source), State, NewState) :-
	ca_support : handled(message(Message, Source), State, NewState).

handled(query(level), _, Level) :-
	level(Level).

handled(query(umwelt), State, Umwelt) :-
	get_state(State, umwelt, Umwelt).

/*
A CA can be recruited only if it has a causal theory.
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

handled(event(Topic, Payload, Source), State, NewState) :-
	event_of_interest(Topic, Source, State),
	!,
	get_state(State, status, started) ->
	event_handled_now(event(Topic, Payload, Source),
		State,
		NewState);
	event_handled_later(event(Topic, Payload, Source),
		State,
		NewState).

handled(event(Topic, Payload, Source), State, State) :-
	ca_support : handled(event(Topic, Payload, Source), State, State).

event_of_interest(Topic, Source, State) :-
	member(Topic,
		member(Topic, [ca_started, ca_terminated, prediction_error, belief_domain, directive_status])),
	from_umwelt(Source, State).

event_of_interest(Topic, Source, State) :-
	member(Topic,
		member(Topic, [prediction, directive])),
	from_parent(Source, State).

event_handled_later(event(Topic, Payload, Source), State, NewSTate) :-
	get_state(State, buffered_events, BufferedEvents),
	put_state(State,
		buffered_events,
		[event(Topic, Payload, Source)|BufferedEvents],
		NewState).

from_umwelt(Source, State) :-
	get_state(State, umwelt, Umwelt),
	member(Source, Umwelt).

% Ignored
event_handled_now(event(ca_started, Options, Source), State, State).
% Ignore if the terminated CA is not in the umwelt, else update the umwelt
event_handled_now(event(ca_terminated, Options, Source), State, NewState) :-
	removed_from_umwelt(Source, State, NewState),
	log(info, ca, "CA ~w was removed from the umwelt of  ~@", [Source, self]).
% TODO
event_handled_now(event(prediction, Belief, Source), State, State).
% TODO
event_handled_now(event(prediction_error, Prediction, Source), State, State).
% TODO
event_handled_now(event(goal, Impact, Source), State, State).
% TODO
event_handled_now(event(command, Policy, Source), State, State).
% TODO
event_handled_now(event(belief_domain, BeliefDomain, Source), State, State).

event_handled_now(event(Topic, Payload, Source), State, State) :-
	log(info, ca, "CA ~@ is NOT handling event event(~w, ~p, ~w)", [self, Topic, Payload, Source]).

removed_from_umwelt(CA, State, NewState) :-
	get_state(State, umwelt, Umwelt),
	member(CA, Umwelt),
	subtract(Umwelt, Source, Umwelt1),
	put_state(State, umwelt, Umwelt1, NewState).

% The previous frame becomes the new frame by updating the start_time,
% processing buffered events and re-activating processing events upon receipt
frame_started(State, NewState) :-
	log(info, ca, "CA ~@ is starting a new frame", [self]),
	past_frame_continuation(State, State1),
	suspended_events_processed(State, State2),
	put_state(State2, status, started, NewState).

% TODO
past_frame_continuation(State, State). 

suspended_events_processed(State, NewState) :-
	get_state(State, buffered_events, BufferedEvents),
	reverse(BufferedEvents, Events),
	events_handled_now(Events, State, NewState).

events_handled_now([], State, State).
events_handled_now([event(Topic, Payload, Source)|Rest], State, NewState) :-
	event_handled_now(event(Topic, Payload, Source),
		State,
		State1),
	events_handled_now(Rest, State1, NewState).

% Ending a frame means suspending processing events, 
% completing the frame (updating beliefs, maybe taking action, and assessing efficacy of prior actions),
% then putting the frame in history.
frame_ended(State, NewState) :-
	put_state(State, status, ending, State1),
	log(info, ca, "CA ~@ is completing its current frame", [self]).
	% wellbeing_diffused(State1, State2),
	% causal_theory_evaluated(State2, State3),
	% beliefs_updated(State3, State4),
	% predictions_made(State4, State5),
	% goal_selected(State5, State6),
	% policy_attempted(State6, State7),
	% history_updated(State7, NewState).

% TODO
wellbeing(_, 1).