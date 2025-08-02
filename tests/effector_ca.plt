/*
%% Start world and body servers
[load].
[load_tests].
run_tests(effector_ca).
*/

:- begin_tests(effector_ca, [setup(init_som), cleanup(terminate_som)]).

:- use_module(test_helper).

:- use_module(utils(logger)).
:- use_module(actors(supervisor)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(agent)).
:- use_module(agency(som)).

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

test(effector_belief_and_policy_domains) :-
	query_answered(som, children, SOMChildren), 
    forall(
        (member(child(worker, EffectorCA), SOMChildren), 
         query_answered(EffectorCA, type, effector_ca)
        ), 
        (query_answered(EffectorCA, belief_domain, BeliefDomain), 
         query_answered(EffectorCA, policy_domain, ActionDomain), 
         assertion(member(predictable{name:count, objects:ActionDomain, values:positive_integer}, BeliefDomain)))).

test(intent_command_actuation_execution) :-
    EffectorCA = 'effector:tacho_motor-outA',
    query_answered(EffectorCA, state, InitialState),
    % adopt
    message_sent(EffectorCA, adopted),
	query_answered(EffectorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
    % intent - the CA informs of the subset of intended directives that can be actuated 
    IntentPayload = [policy = [command(spin), command(reverse_spin), command(jump)]],
    published(intent, IntentPayload),
    get_message(message(can_actuate(IntentPayload, Directives), EffectorCA)),
    assertion(member(command(spin), Directives)),
    assertion(member(command(reverse_spin), Directives)),
    assertion(\+ member(command(jump), Directives)),
    % actuate - prepare to carry out executable actions that (partially) realize intent
    ActuationPayload = [policy = Directives],
    published(actuation, ActuationPayload),
    get_message(message(executable(ActuationPayload, Commands), EffectorCA)),
    assertion(member(command(spin), Commands)),
    assertion(member(command(reverse_spin), Commands)),
    % execute 
    message_sent(EffectorCA, execute),
    get_message(message(executed, EffectorCA)),
    % wellbeing updated
    query_answered(EffectorCA, state, FinalState),
    assert_wellbeing_changed(InitialState, FinalState, [fullness = <, integrity = <, engagement = >]).

test(intent_goal_actuation_execution) :-
    EffectorCA = 'effector:tacho_motor-outA',
    query_answered(EffectorCA, state, InitialState),
    % adopt
    message_sent(EffectorCA, adopted),
	query_answered(EffectorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
    % intent - the CA informs of the subset of intended directives that can be actuated 
    IntentPayload = [policy = [goal(count(spin, 2))]],
    published(intent, IntentPayload),
    get_message(message(can_actuate(IntentPayload, Directives), EffectorCA)),
    assertion(member(goal(count(spin, 2)), Directives)),
    % actuate - prepare to carry out executable actions that (partially) realize intent
    ActuationPayload = [policy = Directives],
    published(actuation, ActuationPayload),
    get_message(message(executable(ActuationPayload, Commands), EffectorCA)),
    assertion([command(spin), command(spin)] == Commands),
    % execute 
    message_sent(EffectorCA, execute),
    get_message(message(executed, EffectorCA)),
    % wellbeing updated
    query_answered(EffectorCA, state, FinalState),
    assert_wellbeing_changed(InitialState, FinalState, [fullness = <, integrity = <, engagement = >]).

test(execution_prediction) :-
    EffectorCA = 'effector:tacho_motor-outA',
    message_sent(EffectorCA, adopted),
    query_answered(EffectorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
    IntentPayload = [policy = [command(spin), command(reverse_spin), command(jump)]],
    published(intent, IntentPayload),
    get_message(message(can_actuate(IntentPayload, Directives), EffectorCA)),
    ActuationPayload = [policy = Directives],
    published(actuation, ActuationPayload),
    get_message(message(executable(ActuationPayload, _), EffectorCA)),
    message_sent(EffectorCA, execute),
    get_message(message(executed, EffectorCA)),
    % incorrectly predict belief from execution
    Prediction1 = [belief = count(spin, 2)],
	published(prediction, Prediction1),
	get_message(message(prediction_error(Prediction1, 1), EffectorCA)),
    % correctly predict belief
    Prediction2 = [belief = count(reverse_spin, 1)],
	published(prediction, Prediction2),
	\+ get_message(message(prediction_error(Prediction2, _), EffectorCA), 2).