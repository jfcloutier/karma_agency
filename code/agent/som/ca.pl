/*
Cognition actor (CA)

Events:

* In and out
  * topic: ca_started, payload: level(Level)
  * topic: ca_terminated, payload: level(Level)
  * topic: prediction, payload: belief(PropertyName, Value) | belief(RelationName, ObjectName1, ObjectName2)
  * topic: prediction_error, payload: prediction(Belief)
  * topic: belief_domain, payload: [belief_spec(BeliefName, Domain), ...]
  * topic: directive, payload: goal(BeliefName, Impact) | command(PolicyName)
  * topic: directive_status, payload: directive_report(Directive, rejected | ready | executed(PolicyName))
* Out
  * topic: causal_theory_wanted, payload: [pinned_predicates - PinnedPredicated, pinned_objects - PinnedObjects]
  * topic: wellbeing_changed, payload: [fullness - N1, integrity - N2, engagement - N3]

Messages:

* In and Out
  * wellbeing_exchange_request([fullness - N1, integrity - N2, engagement - N3])
  * wellbeing_exchanged([fullness - N1, integrity - N2, engagement - N3])

* In
  * causal_theory_found(Theory)

* Queries received:
  * level - Integer > 0
  * umwelt - CA names
  * wellbeing_counts - [fullness - N1, integrity - N2, engagement - N3]
    
Thread locals:
    * level - 1..? - the level of the CAs it is responsible for
    * timer - the frame timer

State:
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

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(worker)).
:- use_module(library(uuid)).
:- use_module(pubsub).

% Thread statis state
:- thread_local level/1, timer/1, buffered_events/1.

name_from_level(Level, Name) :-
	uuid(ID),
	atomic_list_concat([ca, level, Level, id, ID], ":", Name).

% A cognition actor completes a time frame every 2 ** Level seconds

latency(Level, Latency) :-
	Latency is (2**Level).

level(Name, Level) :-
	query_answered(Name, level, Level).

umwelt(Name, Umwelt) :-
	query_answered(Name, umwelt, Umwelt).

% Worker

init(Options, State) :-
	log(info, ca, "Initiating ca with ~p", [Options]),
	empty_state(EmptyState),
	option(level(Level),
		Options),
	assert(level(Level)),
	option(umwelt(Umwelt),
		Options),
	self(Name),
	latency(Level, Latency),
	empty_frame(Frame),
	put_state(EmptyState, [parents - [], umwelt - Umwelt, frame - Frame, history - [], buffered_events - [], status -  initiating], State),
	subscribed_to_events(),
	clock_started(Name, Latency).

subscribed_to_events :-
	foreach(member(Topic, [ca_started, ca_terminated, prediction, prediction_error, belief_domain, directive, directive_status]),
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
	log(warn, ca, '~@ terminating', [self]),
	timer(TimerName),
	timer : stopped(TimerName),
	level(Level),
	published(ca_terminated,
		[level(Level)]),
	log(warn, ca, 'Terminated ~@', [self]).

% Time to start a timeframe
handled(message(tick, _), State, NewState) :-
	(get_state(State, history, []) ->
	published(ca_started,
			[level(Level)]),
		State1 = State;
	frame_ended(State, State1)),
	frame_started(State1, NewState).

handled(message(Message, Source), State, State) :-
	log(debug, ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handled(query(level), _, Level) :-
	level(Level).

handled(query(umwelt), State, Umwelt) :-
	get_state(State, umwelt, Umwelt).

handled(query(Query), _, unknown) :-
	log(debug, ca, '~@ is NOT handling query ~p', [self, Query]).

handled(event(Topic, Payload, Source), State, NewState) :-
	get_state(State, status, started),
	event_of_interest(Topic, Source, State),
	!,
	event_handled_now(event(Topic, Payload, Source),
		State,
		NewState).

handled(event(Topic, Payload, Source), State, NewState) :-
	event_of_interest(Topic, Source, State),
	get_state(State, buffered_events, BufferedEvents),
	put_state(State,
		buffered_events,
		[event(Topic, Payload, Source)|BufferedEvents],
		NewState).

handled(event(Topic, Payload, Source), State, State) :-
	log(debug, ca, '~@ is NOT handling event ~p with ~p from ~w', [self, Topic, Payload, Source]).

event_of_interest(Topic, Source, State) :-
	member(Topic,
		member(Topic, [ca_started, ca_terminated, prediction_error, belief_domain, directive_status])),
	from_umwelt(Source, State).

event_of_interest(Topic, Source, State) :-
	member(Topic,
		member(Topic, [prediction, directive])),
	from_parent(Source, State).

from_umwelt(Source, State) :-
	get_state(State, umwelt, Umwelt),
	member(Source, Umwelt).

from_parent(Source, State) :-
	get_state(State, parents, Parents),
	member(Source, Parents).

% TODO
event_handled_now(event(prediction, Belief, Source), State, State).
event_handled_now(event(prediction_error, Prediction, Source), State, State).
event_handled_now(event(goal, Impact, Source), State, State).
event_handled_now(event(command, Policy, Source), State, State).
event_handled_now(event(belief_domain, BeliefDomain, Source), State, State).
event_handled_now(event(ca_started, Options, Source), State, State).
event_handled_now(event(ca_terminated, Options, Source), State, State).
event_handled_now(event(), State, State).


event_handled_now(event(Topic, Payload, Source), State, State) :-
	log(info, ca, "CA ~@ is NOT handling event event(~w, ~p, ~w)", [self, Topic, Payload, Source]).

% The previous frame becomes the new frame by updating the start_time,
% processing buffered events and re-activating processing events upon receipt
frame_started(State, NewState) :-
	log(info, ca, 'CA ~@ is starting a new frame', [self]),
	continue_past_frame(State, State1),
	suspended_events_processed(State, State2),
	put_state(State2, status, started, NewState).

% TODO
continue_past_frame(State, State). 

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
	log(info, ca, 'CA ~@ is completing its current frame', [self]).
	% wellbeing_diffused(State1, State2),
	% causal_theory_evaluated(State2, State3),
	% beliefs_updated(State3, State4),
	% predictions_made(State4, State5),
	% goal_selected(State5, State6),
	% policy_attempted(State6, State7),
	% history_updated(State7, NewState).
