/*
%% Start world and body servers
[load].
[load_tests].
run_tests(effector_ca).
*/

:- begin_tests(effector_ca, [setup(init_som), cleanup(terminate_som)]).

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
    assertion(wellbeing_updated_from_acting(InitialState, FinalState)).

% Test effecting a goal

wellbeing_updated_from_acting(State, State1) :-
	get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	option(integrity(Integrity), Wellbeing),
	option(engagement(Engagement), Wellbeing),
    get_state(State1, wellbeing, Wellbeing1),
	option(fullness(Fullness1), Wellbeing1),
	option(integrity(Integrity1), Wellbeing1),
	option(engagement(Engagement1), Wellbeing1),
    assertion(Fullness > Fullness1),
	assertion(Integrity > Integrity1),
	assertion(Engagement < Engagement1).