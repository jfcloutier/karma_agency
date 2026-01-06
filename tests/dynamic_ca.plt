/*
Tests of dynamic CAs

%% Start the world and body servers
[load].
[load_tests].
run_tests(dynamic_ca).
*/

:- begin_tests(dynamic_ca, [setup(init_som), cleanup(terminate_som)]).

:- use_module(test_helper).
:- use_module(utils(logger)).
:- use_module(actors(supervisor)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(agent)).
:- use_module(agency(som)).

:- set_log_level(info).

% The SOM is initialized and starts growing.
% Succeed once a dynamic CA was created and has completed a first timeframe and all of its phases.
test(timeframe) :-
	all_subscribed([end_of_phase, end_of_timeframe, end_of_life, ca_started]),
	som : growing([max_timeframes=1]),
	get_message(event(ca_started, [level(1)], _)),
	query_answered(som, children, SOMChildren),
	assertion(SOMChildren \== unknown),
	findnsols(1,
		CA,
		(member(child(worker, CA),
			SOMChildren), query_answered(CA, type, dynamic_ca)),
		L),
	assertion(L \== []),
	!,
	get_matching_message(option(phase, predict), event(end_of_phase, PredictState, CA), 1),
	assertion(PredictState.state.predictions_out \== []),
	get_matching_message(option(phase, observe), event(end_of_phase, ObserveState, CA), 1),
	assertion(ObserveState.state.observations \== []),
	get_matching_message(option(phase, experience), event(end_of_phase, ExperienceState, CA), 1),
	assertion(ExperienceState.state.experiences \== []),
	get_matching_message(option(phase, feel), event(end_of_phase,_, CA), 1),
	get_matching_message(option(phase, plan), event(end_of_phase,_, CA), 1),
	get_matching_message(option(phase, act), event(end_of_phase,_, CA), 1),
	get_matching_message(option(phase, assess), event(end_of_phase,_, CA), 1),
	get_matching_message(option(phase, bind), event(end_of_phase,_, CA), 1),

	get_message(event(end_of_timeframe, _, _)),
	get_message(event(end_of_life, _, _)).

:- end_tests(dynamic_ca).

