/*
Cognition actor (CA)

Events: 
* In
  * 
* Out
  * ca_started, [level(Level)]
  * ca_terminated, [level(Level)]

* Queries answered
  * level - Integer > 0
  * umwelt - CA names
    
Thread locals:
    * level - 1..? - the level of the CAs it is responsible for
    * timer - the frame timer

State:
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

% Thread statis state
:- thread_local level/1, timer/3, buffered_events/1.

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
	put_state(EmptyState, [umwelt - Umwelt, frame - Frame, history - [], buffered_events - [], status -  initiating], State),
	clock_started(Name, Latency, Timer).

clock_started(Name, Delay, Timer) :-
	send_at_interval(Name,
		framer,
		message(tick, Name),
		Delay,
		TimerName),
	Timer = timer(TimerName, Goal, Delay),
	assert(Timer),
	% Send first tick now
	send(Name,
		message(tick, Name)).

% Empty time frame 
empty_frame(EmptyFrame) :-
	get_time(Now),
	put_state(frame{}, [start_time - Now, wellbeing - [], predictions_out - [], predictions_in  - [], prediction_errors - [], beliefs - []], EmptyFrame).

signal_processed(control(stopped)) :-
	worker : stopped.

terminated :-
	log(warn, ca, '~@ terminating', [self]),
	timer(TimerName, _, _),
	timer : stopped(TimerName),
	level(Level),
	publish(ca_terminated,
		[level(Level)]),
	log(warn, ca, 'Terminated ~@', [self]).

% Time to start a timeframe
handled(message(tick, _), State, NewState) :-
	(get_state(State, history, []) ->
	publish(ca_started,
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
	!,
	event_handled_now(event(Topic, Payload, Source),
		State,
		NewState).

handled(event(Topic, Payload, Source), State, NewState) :-
	get_state(State, buffered_events, BufferedEvents),
	put_state(State,
		buffered_events,
		[event(Topic, Payload, Source)|BufferedEvents],
		NewState).

% TODO
event_handled_now(event(prediction, belief(Belief), Source), State, State).
event_handled_now(event(prediction_error, prediction(Belief), Source), State, State).
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
	log(info, ca, 'CA ~@ is completing its current frame', [self]),
	wellbeing_diffused(State1, State2),
	causal_theory_evaluated(State2, State3),
	beliefs_updated(State3, State4),
	predictions_made(State4, State5),
	goal_selected(State5, State6),
	policy_attempted(State6, State7),
	history_updated(State7, NewState).
