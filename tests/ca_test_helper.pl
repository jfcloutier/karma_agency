:- module(ca_test_helper, [test_in_timeframe/2]).

/*
Helpers for agency testing.
*/

:- use_module(test_helper).
:- use_module(actors(actor_utils)).

test_in_timeframe(CA, Index) :-

	get_matching_message(option(phase, beginning), event(end_of_phase, _, CA), 10),
	assertion(succeeds_at('Beginning done', Index, true)),

	get_matching_message(option(phase, predicting), event(end_of_phase, PredictPayload, CA), 10),
	state_delta(predictions_out, PredictPayload, PredictionsOut),
	assertion(succeeds_at('Predictions made', Index, PredictionsOut \== [])),

	get_matching_message(option(phase, observing), event(end_of_phase, ObservePayload, CA), 10),
	state_delta(observations, ObservePayload, Observations),
	assertion(succeeds_at('Observations made', Index, Observations \== [])),

	get_matching_message(option(phase, perceiving), event(phase_progressed, PerceptPayload, CA), 10),
	% perceiving phase progresses by producing percepts
	state_delta(percepts, PerceptPayload, Percepts),
	assertion(succeeds_at('Percepts produced', Index, Percepts \= [])),

	get_matching_message(option(phase, perceiving), event(end_of_phase, PerceptPayload1, CA), 10),
	state_delta(percepts, PerceptPayload1, Percepts1),
	% perceiving phase ends on an empty delta
	assertion(succeeds_at('Perceiving completed with no more percepts', Index, Percepts1 == [])),
	sleep(1),
	get_matching_message(option(phase, feeling), event(end_of_phase, FeelPayload, CA), 10),
	state_delta(percepts, FeelPayload, FeltPercepts),
	assertion(succeeds_at('Percepts are felt', 
		            Index, 
					ca_test_helper:all_felt_percepts(FeltPercepts))),
		           
	get_matching_message(option(phase, acting), event(end_of_phase, ActPayload, CA), 10),
	option(state_deltas(Deltas), ActPayload),
	% act phase ends on an empty delta
	assertion(succeeds_at('Acting is done with no more to do', Index, Deltas == [])),
	% End of phase assessing
	get_matching_message(option(phase, assessing), event(end_of_phase, AssessPayload, CA), 10),
	option(state_deltas(Deltas1), AssessPayload),
 	assertion(succeeds_at('Assessing is done', Index, Deltas1 \= [])),
   % End of timeframe
    get_matching_message(option(state, State), event(end_of_timeframe, _, CA), 10),
	assertion(Index == 1 -> succeeds_at('Intent created', Index, State.intent \= none) ; true),
	assertion(Index == 3 -> succeeds_at('Affordance found', Index, State.affordances \= []) ; true).

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

