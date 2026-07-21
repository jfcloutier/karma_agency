:- module(ca_test_helper, [test_in_timeframe/2]).

/*
Helpers for agency testing.
*/

:- use_module(test_helper).
:- use_module(actors(actor_utils)).

test_in_timeframe(CA, Index) :-
	get_matching_message(option(phase, predict), event(end_of_phase, PredictPayload, CA), 10),
	state_delta(predictions_out, PredictPayload, PredictionsOut),
	assertion(succeeds_at('Predictions made', Index, PredictionsOut \== [])),

	get_matching_message(option(phase, observe), event(end_of_phase, ObservePayload, CA), 10),
	state_delta(observations, ObservePayload, Observations),
	assertion(succeeds_at('Observations made', Index, Observations \== [])),

	get_matching_message(option(phase, perceiving), event(phase_progressed, PerceptPayload, CA), 10),
	% perceiving phase progresses by producing percepts
	state_delta(percepts, PerceptPayload, Percepts),
	assertion(succeeds_at('Percepts produced', Index, Percepts \= [])),

	get_matching_message(option(phase, perceiving), event(end_of_phase, PerceptPayload1, CA), 10),
	state_delta(percepts, PerceptPayload1, Percepts1),
	% perceiving phase ends on an empty delta
	assertion(succeeds_at('Percei completed with no more percepts', Index, Percepts1 == [])),

	sleep(1),
	get_matching_message(option(phase, feel), event(end_of_phase, FeelPayload, CA), 10),
	state_delta(percepts, FeelPayload, FeltPercepts),
	assertion(succeeds_at('Percepts are felt', 
		            Index, 
					ca_test_helper:all_felt_percepts(FeltPercepts))),
		           
	get_matching_message(option(phase, act), event(end_of_phase,ActPayload, CA), 10),
	option(state_deltas(Deltas), ActPayload),
	% act phase ends on an empty delta
	assertion(succeeds_at('Act is done with no more to do', Index, Deltas == [])),

	sleep(1),
	query_answered(CA, state, State2),
	% act phase ends after work units updating/adding an intent and goal states
	assertion(succeeds_at('An intent was produced from act', Index, State2.intent \= none)),
    assertion(succeeds_at('Act completed with plans produced', Index, State2.plans \= [])),
	get_matching_message(option(phase, assess), event(end_of_phase,_, CA), 10).

all_felt_percepts(Percepts) :-
	Percepts \== [],
	forall(member(Percept, Percepts), number(Percept.feeling)).

state_delta(DeltaName, Payload, Delta) :-
	state_delta(DeltaName, Payload, Delta, []).

state_delta(DeltaName, Payload, Delta, Default) :-
	option(state_deltas(Deltas), Payload),
	Option =.. [DeltaName, Delta],
	(option(Option, Deltas) ->
		true
		;
		Delta = Default
    ).

