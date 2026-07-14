:- module(ca_test_helper, [test_in_timeframe/2]).

/*
Helpers for agency testing.
*/

:- use_module(test_helper).
:- use_module(actors(actor_utils)).

test_in_timeframe(CA, Index) :-
	get_matching_message(option(phase, predict), event(end_of_phase, PredictPayload, CA), 1),
	assert_indexed('Predictions made', Index, PredictPayload.state_deltas.predictions_out \== []),
	get_matching_message(option(phase, observe), event(end_of_phase, ObservePayload, CA), 1),
	assert_indexed('Observations made', Index, ObservePayload.state_deltas.observations \== []),
	get_matching_message(option(phase, experience), event(end_of_phase, ExperiencePayload, CA), 1),
	% experience phase ends on an empty delta
	assert_indexed('Experiencing completed with no more experiences', Index, ExperiencePayload.state_deltas == []),
	sleep(1),
	query_answered(CA, state, State1),
	% experience phase ends after work units updating/adding experiences
	assert_indexed('Experiences produced', Index, State1.experiences \= []),
	get_matching_message(option(phase, feel), event(end_of_phase, FeelPayload, CA), 1),
	assert_indexed('Experiences are felt', 
		            Index, 
					ca_test_helper:all_felt_experiences(FeelPayload)),
		           
	get_matching_message(option(phase, act), event(end_of_phase,ActPayload, CA), 1),
	% act phase ends on an empty delta
	assert_indexed('Act is done with no more to do', Index, ActPayload.state_deltas == []),
	sleep(1),
	query_answered(CA, state, State2),
	% act phase ends after work units updating/adding an intent and goal states
	assert_indexed('An intent was produced from act', Index, State2.intent \= none),
    assert_indexed('Act completed with goal states produced', Index, State2.goal_states \= []),
    assert_indexed('Act completed with plans produced', Index, State2.plans \= []),
	get_matching_message(option(phase, assess), event(end_of_phase,_, CA), 1).

all_felt_experiences(FeelPayload) :-
	Experiences = FeelPayload.state_deltas.experiences, 
	Experiences \== [],
	forall(member(Experience, Experiences), number(Experience.feeling)).

