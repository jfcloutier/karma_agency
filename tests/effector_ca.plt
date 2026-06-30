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

test(plan_executed) :-
    EffectorCA = 'effector:tacho_motor-outA',
    all_subscribed([can_seek, cannot_seek, can_execute]),
    % EffectorName = 'tacho_motor-outA',
    % query_answered(EffectorCA, state, InitialState),
   % adopt
    message_sent(EffectorCA, adopted),
    sleep(1),
	  query_answered(EffectorCA, parents, Parents),
	  thread_self(Self),
	  assertion(member(Self, Parents)),
    % Executing actions in the context of an intent
    IntentId = 'some intent',
    findall(Command, (member(Action, [spin, spin, reverse_spin]), Command = command{effector_ca:EffectorCA, action:Action, intent_id:IntentId}), CanDoCommands),
    CantDoCommand = command{effector_ca:EffectorCA, action:jump, intent_id:IntentId},
    Commands = [CantDoCommand | CanDoCommands],
    published(todo, [directives=Commands]),
    sleep(1),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:spin, intent_id:IntentId}), event(can_seek, _, EffectorCA), 1),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:spin, intent_id:IntentId}), event(can_seek, _, EffectorCA), 1),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:reverse_spin, intent_id:IntentId}), event(can_seek, _, EffectorCA), 1),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:jump, intent_id:IntentId}), event(cannot_seek, _, EffectorCA), 1),
    forall(member(Command, CanDoCommands), published(find_plan, [directive=Command])),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:spin, intent_id:IntentId}), event(can_execute, _, EffectorCA), 1),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:spin, intent_id:IntentId}), event(can_execute, _, EffectorCA), 1),
    get_matching_message(option(directive, command{effector_ca:EffectorCA, action:reverse_spin, intent_id:IntentId}), event(can_execute, _, EffectorCA), 1),
  	  % Ready actuation can-do commands and have the body execute them all
	  forall(member(Command, CanDoCommands), message_sent(EffectorCA, ready_actuation(Command))),
    query_answered(agency, option(body_host), Host),
  	body : actions_executed(Host),

    forall(member(Command, CanDoCommands), message_sent(EffectorCA, actuation_executed(Command))),
    % Allow time for the wellbeing to update
    sleep(1),
    % wellbeing updated
    % query_answered(EffectorCA, wellbeing, FinalWellbeing),
    % assert_wellbeing_changed(InitialState.wellbeing, FinalWellbeing, [fullness = <, integrity = <, engagement = =]),
    published(intent_completed, [intent_id=IntentId]).

  test(intent_abandoned) :-
    EffectorCA = 'effector:tacho_motor-outA',
    % EffectorName = 'tacho_motor-outA',
    query_answered(EffectorCA, state, InitialState),
    EffectorCA = 'effector:tacho_motor-outA',
    all_subscribed([can_seek, cannot_seek, can_execute]),
    % EffectorName = 'tacho_motor-outA',
    % query_answered(EffectorCA, state, InitialState),
   % adopt
    message_sent(EffectorCA, adopted),
    sleep(1),
	  query_answered(EffectorCA, parents, Parents),
	  thread_self(Self),
	  assertion(member(Self, Parents)),
    % Executing actions in the context of an intent
    IntentId = 'some intent',
    findall(Command, (member(Action, [spin, spin, reverse_spin]), Command = command{effector_ca:EffectorCA, action:Action, intent_id:IntentId}), CanDoCommands),
    CantDoCommand = command{effector_ca:EffectorCA, action:jump, intent_id:IntentId},
    Commands = [CantDoCommand | CanDoCommands],
    published(todo, [directives=Commands]),
    sleep(1),
    published(abandoned, [intent_id=IntentId]),
    sleep(1),
    query_answered(EffectorCA, state, CurrentState),
    assertion(length(CurrentState.actuations, 0)),
    % Allow time for the wellbeing to update
    sleep(1),
    % wellbeing updated
    query_answered(EffectorCA, wellbeing, FinalWellbeing),
    assert_wellbeing_changed(InitialState.wellbeing, FinalWellbeing, [fullness = = , integrity = =, engagement = =]).
