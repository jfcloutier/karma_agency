:- module(ca_test_helper, [test_in_timeframe/2]).

:- use_module(test_helper).
:- use_module(actors(actor_utils)).

test_in_timeframe(CA, Count) :-
	get_matching_message(option(phase, predict), event(end_of_phase, PredictPayload, CA), 1),
	assert_counted('Predictions made', Count, PredictPayload.state_deltas.predictions_out \== []),
	get_matching_message(option(phase, observe), event(end_of_phase, ObservePayload, CA), 1),
	assert_counted('Observations made', Count, ObservePayload.state_deltas.observations \== []),
	get_matching_message(option(phase, experience), event(end_of_phase, ExperiencePayload, CA), 1),
	% experience phase ends on an empty delta
	assert_counted('Experiencing completed with no more experiences', Count, ExperiencePayload.state_deltas == []),
	sleep(1),
	query_answered(CA, state, State1),
	% experience phase ends after work units updating/adding experiences
	assert_counted('Experiences produced', Count, State1.experiences \= []),
	get_matching_message(option(phase, feel), event(end_of_phase, FeelPayload, CA), 1),
	assert_counted('Experiences are felt', 
		            Count, 
		           (Experiences = FeelPayload.state_deltas.experiences, Experiences \== [])),
					% , forall(member(Experience, Experiences), number(Experience.feeling)))),
	get_matching_message(option(phase, act), event(end_of_phase,ActPayload, CA), 1),
	% act phase ends on an empty delta
	assert_counted('Act is done with no more to do', Count, ActPayload.state_deltas == []),
	sleep(1),
	query_answered(CA, state, State2),
	% act phase ends after work units updating/adding an intent and goal states
	assert_counted('An intent was produced from act', Count, State2.intent \= none),
    assert_counted('Act completed with goal states produced', Count, State2.goal_states \= []),
    assert_counted('Act completed with plans produced', Count, State2.plans \= []),
	get_matching_message(option(phase, assess), event(end_of_phase,_, CA), 1).

