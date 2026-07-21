/*
Tests of effector CAs

%% Start world and body servers
[load].
[load_tests].
run_tests(effector_ca).
*/

:- begin_tests(effector_ca, [setup(init_static_som), cleanup(terminate_som)]).

:- use_module(test_helper).

:- use_module(utils(logger)).
:- use_module(actors(supervisor)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(agent)).
:- use_module(agency(body)).
:- use_module(agency(som)).
:- use_module(agency(som/ca_support)).

:- set_log_level(info).

test(effector_ca_in_som) :-
	query_answered(som, children, SOMChildren),
	assertion(SOMChildren \== unknown),
	findnsols(1,
		CA,
		(member(child(worker, CA),
				SOMChildren),
			query_answered(CA, type, effector_ca)),
		L),
	assertion(L \== []).

test(all_effector_cas_are_at_level_0) :-
	som : effector_cas(EffectorCAs),
	assertion(forall(member(EffectorCA, EffectorCAs),
			query_answered(EffectorCA, level, 0))).

test(movement_executed) :-
    EffectorCA = 'effector:tacho_motor-outA',
    all_subscribed([wellbeing - EffectorCA]),
   % adopt
    message_sent(EffectorCA, adopted),
    sleep(1),
	  query_answered(EffectorCA, parents, Parents),
	  thread_self(Self),
	  assertion(member(Self, Parents)),
    query_answered(EffectorCA, wellbeing, InitialWellbeing),
    % Executing a movement including actions the effector can execute
    Movement = [command{effector_ca:EffectorCA, action:spin}, command{effector_ca:EffectorCA, action:spin}, command{effector_ca:EffectorCA, action:reverse_spin}, command{effector_ca:EffectorCA, action:jump}],
    forall(member(Command, Movement), message_sent(Command.effector_ca, ready_actuation(Command))),
    query_answered(agency, option(body_host), Host),
  	body : actions_executed(Host),
    message_sent(EffectorCA, actuations_executed),
    sleep(1),
    get_matching_message(option(current, FinalWellbeing), event(wellbeing, _, EffectorCA), 1),
    assert_wellbeing_changed(InitialWellbeing, FinalWellbeing, [fullness = < , integrity = <, engagement = =]).