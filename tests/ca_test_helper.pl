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

	get_matching_message(option(phase, experience), event(phase_progressed, ExperiencePayload, CA), 10),
	% experience phase progresses by producing experiences
	state_delta(experiences, ExperiencePayload, Experiences),
	assertion(succeeds_at('Experiences produced', Index, Experiences \= [])),

	get_matching_message(option(phase, experience), event(end_of_phase, ExperiencePayload1, CA), 10),
	state_delta(experiences, ExperiencePayload1, Experiences1),
	% experience phase ends on an empty delta
	assertion(succeeds_at('Experiencing completed with no more experiences', Index, Experiences1 == [])),

	sleep(1),
	get_matching_message(option(phase, feel), event(end_of_phase, FeelPayload, CA), 10),
	state_delta(experiences, FeelPayload, FeltExperiences),
	assertion(succeeds_at('Experiences are felt', 
		            Index, 
					ca_test_helper:all_felt_experiences(FeltExperiences))),
		           
	get_matching_message(option(phase, act), event(end_of_phase,ActPayload, CA), 10),
	option(state_deltas(Deltas), ActPayload),
	% act phase ends on an empty delta
	assertion(succeeds_at('Act is done with no more to do', Index, Deltas == [])),

	sleep(1),
	query_answered(CA, state, State2),
	% act phase ends after work units updating/adding an intent and goal states
	assertion(succeeds_at('An intent was produced from act', Index, State2.intent \= none)),
    assertion(succeeds_at('Act completed with goal states produced', Index, State2.goal_states \= [])),
    assertion(succeeds_at('Act completed with plans produced', Index, State2.plans \= [])),
	get_matching_message(option(phase, assess), event(end_of_phase,_, CA), 10).

all_felt_experiences(Experiences) :-
	Experiences \== [],
	forall(member(Experience, Experiences), number(Experience.feeling)).

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

