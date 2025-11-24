/*
%% Start world and body servers
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
% Succeed once a dynamic CA was created and completed a timeframe.
test(dynamic_ca_created_and_completes_timeframe) :-
	subscribed(end_of_timeframe),
	som : growing,
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

