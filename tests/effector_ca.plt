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

test(intent_executed) :-
    EffectorCA = 'effector:tacho_motor-outA',
    EffectorName = 'tacho_motor-outA',
    all_subscribed([activation - EffectorCA]),
    query_answered(EffectorCA, state, InitialState),
    get_state(InitialState, wellbeing, InitialWellbeing),
   % adopt
    message_sent(EffectorCA, adopted),
	query_answered(EffectorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
    % intent - the CA informs of the subset of intended directives that can be actuated
    Id = 'abc',
    IntentPayload = [id = Id, goals = [spin(EffectorName, true), reverse_spin(EffectorName, true), jump(EffectorName, true)], priority = 1],
    published(intent, IntentPayload),
    get_message(message(can_actuate(Goals), EffectorCA)),
    assertion(member(spin(EffectorName, true), Goals)),
    assertion(member(reverse_spin(EffectorName, true), Goals)),
    assertion(\+ member(jump(EffectorName, true), Goals)),
    % actuate - prepare to carry out executable actions that (partially) realize intent
    message_sent(EffectorCA, ready_actuation(spin(EffectorName, true))),
    message_sent(EffectorCA, ready_actuation(reverse_spin(EffectorName, true))),
    get_message(message(actuation_ready(spin(EffectorName, true)), EffectorCA)),
    get_message(message(actuation_ready(reverse_spin(EffectorName, true)), EffectorCA)),
    % execute
    query_answered(agency, option(body_host), Host),
  	body : actions_executed(Host),
    % executed 
    published(executed),
    % Allow time for the wellbeing to update
    sleep(1),
    % wellbeing updated
    query_answered(EffectorCA, wellbeing, FinalWellbeing),
    assert_wellbeing_changed(InitialWellbeing, FinalWellbeing, [fullness = <, integrity = <, engagement = >]),
    % intent completed
    published(intent_completed, [id = Id, executed = true]),
    % activation events received
    get_matching_message(option(directive, spin), event(activation, _, EffectorCA), 1),
    get_matching_message(option(directive, reverse_spin), event(activation, _, EffectorCA), 1).
