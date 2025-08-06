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
         forall(member(Action, ActionDomain), assertion(member(predictable{name:count, object:Action, value:positive_integer}, BeliefDomain))))).

test(intent_command_actuation_execution) :-
    EffectorCA = 'effector:tacho_motor-outA',
    subscribed(wellbeing_changed),
    query_answered(EffectorCA, state, InitialState),
    get_state(InitialState, wellbeing, InitialWellbeing),
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
    message_sent(EffectorCA, actuation(ActuationPayload)),
    get_message(message(executable(ActuationPayload, Commands), EffectorCA)),
    assertion(member(command(spin), Commands)),
    assertion(member(command(reverse_spin), Commands)),
    % execute 
    published(execute),
    % wellbeing updated
    get_message(event(wellbeing_changed, FinalWellbeing, EffectorCA)),
    assert_wellbeing_changed(InitialWellbeing, FinalWellbeing, [fullness = <, integrity = <, engagement = >]).

test(intent_goal_actuation_execution) :-
    EffectorCA = 'effector:tacho_motor-outA',
    subscribed(wellbeing_changed),
    query_answered(EffectorCA, state, InitialState),
    get_state(InitialState, wellbeing, InitialWellbeing),
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
    message_sent(EffectorCA, actuation(ActuationPayload)),
    get_message(message(executable(ActuationPayload, Commands), EffectorCA)),
    assertion([command(spin), command(spin)] == Commands),
    % execute 
    published(execute),
    % wellbeing updated
    get_message(event(wellbeing_changed, FinalWellbeing, EffectorCA)),
    assert_wellbeing_changed(InitialWellbeing, FinalWellbeing, [fullness = <, integrity = <, engagement = >]).

% Verify that engaging an effector CA into executing a policy updates its beliefs
test(execution_prediction) :-
    % Get the CA for a tacho motor effector adopted by the test
    EffectorCA = 'effector:tacho_motor-outA',
    subscribed(wellbeing_changed),
    message_sent(EffectorCA, adopted),
    query_answered(EffectorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
    % Express intent to spin, reverse spin and jump
    IntentPayload = [policy = [command(spin), command(reverse_spin), command(jump)]],
    published(intent, IntentPayload),
    % Verify that the effector CA for the tacho motor can only actuate spin and reverse_spin
    get_message(message(can_actuate(IntentPayload, Directives), EffectorCA)),
    assertion(member(command(spin), Directives)),
    assertion(member(command(reverse_spin), Directives)),
    assertion(\+ member(command(jump), Directives)),
    % Tell the effector CA to prepare to actuate spin and reverse_spin, once each
    ActuationPayload = [policy = Directives],
    message_sent(EffectorCA, actuation(ActuationPayload)),
    % Get confirmation that the effector CA is ready to execute the requested actuations
    get_message(message(executable(ActuationPayload, _), EffectorCA)),
    % Make it so!
    published(execute),
    % Consume the message
    get_message(event(wellbeing_changed, _, EffectorCA)),
    % Incorrectly predict the belief that spin was executed twice
    Prediction1 = [belief = count(spin, 2)],
	published(prediction, Prediction1),
    % Get a prediction error message
	get_message(message(prediction_error(Prediction1, 1), EffectorCA)),
    % Correctly predict the belief that reverse_spin was executed once
    Prediction2 = [belief = count(reverse_spin, 1)],
	published(prediction, Prediction2),
    % Don't get an error prediction message
	\+ get_message(message(prediction_error(Prediction2, _), EffectorCA), 2).