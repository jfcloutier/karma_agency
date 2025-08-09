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

test(effector_belief_domain) :-
	query_answered(som, children, SOMChildren), 
    forall(
        (member(child(worker, EffectorCA), SOMChildren), 
         query_answered(EffectorCA, type, effector_ca)
        ), 
        (query_answered(EffectorCA, belief_domain, BeliefDomain),
         forall(member(Predictable, BeliefDomain),
                assertion(unifiable(Predictable, predictable{name:count, object:_, value:positive_integer}, _)))
        )
    ).

test(intent_executed) :-
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
    IntentPayload = [directive{goal:count(spin, 1), priority:1}, directive{goal:count(reverse_spin, 1), priority:1}, directive{goal:count(jump, 1), priority:1}],
    published(intent, IntentPayload),
    get_message(message(can_actuate(Goals), EffectorCA)),
    assertion(member(count(spin, 1), Goals)),
    assertion(member(count(reverse_spin, 1), Goals)),
    assertion(\+ member(count(jump, 1), Goals)),
    % actuate - prepare to carry out executable actions that (partially) realize intent
    message_sent(EffectorCA, ready_actuation(count(spin, 1))),
    message_sent(EffectorCA, ready_actuation(count(reverse_spin, 1))),
    get_message(message(actuation_ready(count(spin, 1)), EffectorCA)),
    get_message(message(actuation_ready(count(reverse_spin, 1)), EffectorCA)),
    % execute
    query_answered(agency, option(body_host), Host),
  	body : actions_executed(Host),
    % executed 
    published(executed),
    % wellbeing updated
    get_message(event(wellbeing_changed, FinalWellbeing, EffectorCA)),
    assert_wellbeing_changed(InitialWellbeing, FinalWellbeing, [fullness = <, integrity = <, engagement = >]),
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
