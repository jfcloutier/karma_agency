/*
Cognition actor (CA)

A CA strives to survive; the more competent a CA is, the more likely it is that its parents CAs will be competent as well and survive, and thus the more likely it is that
the CA itself will survive since orphaned CAs are the ones susceptible to being removed.

A CA operates one timeframe at a time. Within each time frame, it updates its beliefs and acts on its umwelt to impact current valued beliefs.

The higher the CA's level within the SOM hierarchy, the longer the timeframe. Timeframes are not synchronized across CAs.

A CA strives to become increasingly competent at making sense of its umwelt and at altering or re-enforcing its consequent beliefs via effective policies.

A CA makes sense of observations of its umwelt by predicting beliefs of umwelt CAs (using a discovered causal model) and by deriving beliefs from its own past and current observations.

A CA obtains, and refreshes as needed, a causal theory from the Apperception Engine. It uses its current causal theory to make predictions about umwelt beliefs and about
the projected efficacy of candidate policies. The initial causal theory is essentially "it is what it is". Eventually, a CA gathers enough observations to have the Apperception Engine induce
a more discerning causal theory.

A CA derives its beliefs from objects and relations abduced by its current causal model, and from the detection of patterns (or termination thereof) in its observations over
remembered timeframes.

The CA's beliefs become observations available for parent CAs to make predictions about. An umwelt CA receives predictions about its current beliefs from parent CAs. 
It responds with prediction errors if the predictions are off.

An observation predicted by a CA becomes an actual observation of the CA if is not contradicted by a prediction error from a CA in its umwelt for the duration of the timeframe
within which the prediction was made. A prediction error could arrive in a subsequent timeframe to contradict the triggering prediction or a subsequent one.

A CA's current observations are the sum of its latest uncontradicated predictions, plus the latest contradicting prediction errors it received.

A large errors-to-predictions ratio lowers confidence in the CA's causal theory and thus in its predictions, observations from them, consequent beliefs,
and thus the prediction errors it itself emits. At some point, a CA will try to replace an unacceptably inaccurate causal theory with a new one.

A CA informws its parent CAs (those in which umwelts it sits) of the types and domains of the predicates it synthesized to express its beliefs, so the parent CAs can produce predictions
about the CA's beliefs.

Wellbeing measures are a function of events received from all CAs and from the passage of time. Are the CAs reducing the fullness of the agent or increasing it?
Are detected bumps decreasing the agent's integrity faster and outpacing its self-healing (which speed depends on fullness)?

A CA emits events about the activities it carries out that are used by wellbeing agents to compute the current levels of fullness, integrity and engagement.
Fluctuations in wellbeing cause fluctuations in belief values. For example, a CA obtaining a new causal theory is cosltly and it sends an event that reduces the agent's fullness.

Sensor and effector CAs send events whenever a motor is activated (a fullness cost), or touch is sensed (an integrity cost). Combined sensor and effector events
can also be interpreted as affecting wellness: Running the feeding motor (mouth) while sensing food color (green) increases fullness,
whereas running the feeding motor (mouth) when sensing poison color (red) decreases integrity.

The value of a CA's belief is determined by the known (to the CA) wellness metrics at the time the belief is formed or last updated. Wellness measures are not aggregated;
a belief might be associated with high fullness but low integrity measures. A belief is pleasant if it is associated with no negative wellness measures. A belief is unpleasnat
if it is associated with one or more negative wellness measures. A belief is neutral if it is associated with neither positive nor negative wellness measures.

A CA is effective at impacting its umwelt if it has reliable policies it can execute to validate pleasant beliefs or invalidate unpleasant beliefs. The unpleasantness of a belief 
is the value of its most negative adjusted wellness measure. A wellness measure is adjusted by the relative priority of each type of wellness: integrity > fullness > engagement.

A CA has a limited time (the current timeframe) to act on its beliefs and thus must decide which beliefs to impact first until the current timeframe ends.
Unpleasant beliefs get immediate attention (i.e. the CA formulates policies to invalidate them) with the most unpleasant belief getting the most attention.
Then, if time allows, pleasant beliefs get attention (the CA formulates policies to validate them). Neutral beliefs are ignored. If time allows, the CA may act randomly (babble),
just to "see what happens".

Upon selecting a belief (pleasant vs unpleasant) to impact (persist vs terminate), a CA composes a policy to that effect. Since a belief is synthesized from a history of observations,
a policy to impact this belief will attempt to alter future observations so that the synthesis of that belief is further underwritten (for pleasant beliefs) or undermined (for
unpleasant beliefs). The choice of observations to affect and how they are to be affected depends on the nature of the synthesis that led to the belief.

The CA emits a policy to its umwelt CAs, in essence directing them to impact their own beliefs (as observed by the CA). When a CA receives a policy from a parent CA, it behaves
the same way as if it had decided to impact each beliefs referenced in the policy, assuming it holds them. In fact, for a CA, impacting its own beliefs at the request of a parent CA
takes priority.


Events: 
* In
  * wellbeing, [Kind(Level)] - Kind in fullness, integrity, engagement - Level in 0..1 
  * prediction, dicted(BeliefName, Domain)
  * prediction_error, contradicted(Prediction, Actuality) - Prediction = dicted(BeliefName, Domain) - Actuality is actual_domain(Domain) or terminated
  * goal, impact(BeliefName, Actuality]
  * definition, concept(BeliefName, Domain) or affordance(ActionName)
  * execution, action(ActonName)
  * causal_theory(Theory)
* Out
  * ca_started, [level(Level)]
  * ca_terminated, [level(Level)]
  * belief_domain, [belief_types([BeliefType, ...])] 
  * prediction_error, [prediction(Prediction), correction(Correction), confidence(0..1)]
  * causal_theory_needed

* Queries
  * type - ca
  * level - Integer > 0
  * umwelt - CA names
  * belief_domain - belief predicate types
    
Thread locals:
    * level - 1..? - the level of the CAs it is responsible for
    * timer - the frame timer

State:
    * umwelt - child CAs
    * frame - the current frame
	  * start_time
	  * status - initiating or started or ending
	  * wellness
	  * predictions
	  * prediction_errors
	  * beliefs
	  * causal_theory
	  * policy
    * history - [Frame, ...]
	* 

*/

:- module(ca, []).

:- [load].

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(worker)).
:- use_module(library(uuid)).
:- use_module(library(check)).

% Thread statis state
:- thread_local level/1, timer/3, buffered_events/1.

name_from_level(Level, Name) :-
	uuid(ID), 
	atomic_list_concat([ca, level, Level, id, ID], ":", Name).

% A cognition actor completes a time frame every 2 ** Level seconds

latency(Level, Latency) :-
	Latency is (2**Level).

level(Name, Level) :-
	send_query(Name, level, Level).

umwelt(Name, Umwelt) :-
	send_query(Name, umwelt, Umwelt).

% Worker

init(Options, State) :-
	log(info, ca, "Initiating ca with ~p", [Options]), 
	empty_state(EmptyState), 
	option(
		level(Level), Options), 
	assert(
		level(Level)), 
	option(
		umwelt(Umwelt), Options), 
	self(Name), 
	latency(Level, Latency), 
    ticking_started(Name, Latency, Timer),
	assert(
		timer(Timer)),
	empty_frame(0, Frame), 
	put_state(EmptyState, [umwelt - Umwelt, frame - Frame, history - [], buffered_events - [], status - initiating], State), 
	send_message(Name, frame_started).

ticking_started(Name, Delay, Timer) :-
	send_at_interval(Name, framer, 
		message(tick, Name), Delay, TimerName), 
	Timer = timer(TimerName, Goal, Delay),
	assert(Timer).

reset_ticking :-
	timer(TimerName, Goal, Delay),
	retractall(timer/3),
	timer : stop(TimerName),
	ticking_started(Name, Delay, Timer).

empty_frame(Level, EmptyFrame) :-
	get_time(Now),
	put_state(frame{}, [start_time - Now, wellness - [], predictions - [], prediction_errors - [], beliefs - []], EmptyFrame).

process_signal(control(stop)) :-
	worker : stop.

terminate :-
	log(warn, ca, '~@ terminating', [self]), 
	timer(TimerName, _, _), 
	timer : stop(TimerName), 
	level(Level), 
	publish(ca_terminated, 
		[level(Level)]), 
	log(warn, ca, 'Terminated ~@', [self]).

% Initial start
handle_message(message(frame_started, _), State, NewState) :-
	get_state(State, status, initiating),
	frame_started(State, NewState),
	get_state(State, level, Level),
    publish(ca_started, [level(Level)]).

% Start the next frame
handle_message(message(frame_started, _), State, NewState) :-
	get_state(State, status, ending),
	frame_started(State, NewState).

% Only end frame if started. Then start the next frame.
handle(message(tick, _), State, NewState) :-
	get_state(State, status, started),
	end_frame(State, State1), 
	frame_started(State1, NewState).

handle(message(Message, Source), State, State) :-
	log(debug, ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(query(type), _, ca).

handle(query(level), _, Level) :-
	level(Level).

handle(query(umwelt), State, Umwelt) :-
	get_state(State, umwelt, Umwelt).

handle(query(Query), _, unknown) :-
	log(debug, ca, '~@ is NOT handling query ~p', [self, Query]).

handle(event(Topic, Payload, Source), State, NewState) :-
	get_state(State, status, started), !,
	handle_event_now(event(Topic, Payload, Source), State, NewState).

handle(event(Topic, Payload, Source), State, NewState) :-
	get_state(State, buffered_events, BufferedEvents),
	put_state(State, buffered_events, [event(Topic, Payload, Source) | BufferedEvents], NewState).

% TODO
handle_event_now(event(prediction, belief(Belief), Source), State, State).
handle_event_now(event(prediction_error, prediction(Belief), Source), State, State).
handle_event_now(event(Topic, Payload, Source), State, State) :-
	log(info, ca, "CA ~@ is NOT handling event event(~w, ~p, ~w)", [self, Topic, Payload, Source]).

% The previous frame becomes the new frame by updating the start_time,
% processing buffered events and re-activating processing events upon receipt
frame_started(State, NewState) :-
	log(info, ca, 'CA ~@ is starting a new frame', [self]),
	process_suspended_events(State, State1),
	reset_ticking,
	put_state(State1, status, started, NewState).

process_suspended_events(State, NewState) :-
	get_state(State, buffered_events, BufferedEvents),
	reverse(BufferedEvents, Events),
	handle_events_now(Events, State, NewState).

handle_events_now([], State, State).
handle_events_now([event(Topic, Payload, Source) | Rest], State, NewState) :-
	handle_event_now(event(Topic, Payload, Source), State, State1),
	handle_events_now(Rest, State1, NewState).

% Ending a frame means suspending processing events, 
% completing the frame (updating beliefs, maybe taking action, and assessing efficacy of prior actions),
% then putting the frame in history.
end_frame(State, NewState) :-
	put_state(State, status, ending, State1),
	log(info, ca, 'CA ~@ is completing its current frame', [self]),
	update_beliefs(State1, State2),
	make_predictions(State2, State3),
	execute_policy(State3, State4),
	update_history(State4, NewState).
