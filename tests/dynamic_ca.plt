/*
Tests of dynamic CAs

%% Start the world and body servers
[load].
[load_tests].
run_tests(dynamic_ca).
*/

:- begin_tests(dynamic_ca, [setup(init_som), cleanup(terminate_som)]).

:- use_module(test_helper).
:- use_module(ca_test_helper).
:- use_module(utils(logger)).
:- use_module(actors(supervisor)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(agent)).
:- use_module(agency(som)).


:- set_log_level(info).

% The SOM is initialized and starts growing.
% Succeed once a dynamic CA was created and has completed a few timeframes and all of their phases.
test(timeframes) :-
	MaxTimeframes = 3, 
	all_subscribed([phase_before_work, phase_progressed, end_of_phase, end_of_timeframe, end_of_life, ca_started]),
	som : growing([max_timeframes=MaxTimeframes]),
	get_message(event(ca_started, [level(1)], _)),
	query_answered(som, children, SOMChildren),
	assertion(SOMChildren \== unknown),
	findnsols(1,
		Child,
		(member(child(worker, Child), SOMChildren), query_answered(Child, type, dynamic_ca)),
		L),
	[CA] = L,
	!,
	test_in_timeframe(CA, 1),
	% Timeframe 1 ended
	% get_message(event(end_of_timeframe, _, _)),
	get_matching_message(option(count, 1), event(end_of_timeframe, _, CA), 5),
	test_in_timeframe(CA, 2),
	% Timeframe 2 ended
	% get_message(event(end_of_timeframe, _, _)),
	get_matching_message(option(count, 2), event(end_of_timeframe, _, CA), 5),
	test_in_timeframe(CA, 3),
	% Wait for CA to die
	get_message(event(end_of_life, _, _)).

:- end_tests(dynamic_ca).


