/*
%% Start world and body servers
[load].
[load_tests].
run_tests(dynamic_ca).
*/

:- begin_tests(dynamic_ca, [cleanup(terminate_som)]).

:- use_module(test_helper).

:- use_module(utils(logger)).
:- use_module(actors(supervisor)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(agent)).
:- use_module(agency(som)).

:- set_log_level(info).

test(dynamic_ca_created_and_completes_timeframe) :-
	agent : started('localhost:4000', []),
	subscribed(end_of_timeframe),
	som : level_one_ca_started,
	query_answered(som, children, SOMChildren),
	assertion(SOMChildren \== unknown),
	findnsols(1,
		CA,
		(member(child(worker, CA),
			SOMChildren), query_answered(CA, type, dynamic_ca)),
		L),
	assertion(L \== []),
	get_message(event(end_of_timeframe, _, _)).

:- end_tests(dynamic_ca).

