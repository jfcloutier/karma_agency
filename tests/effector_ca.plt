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

test(intent_actuation_execution) :-
    EffectorCA = 'effector:tacho_motor-outA',
    % adopt
    message_sent(EffectorCA, adopted),
	query_answered(EffectorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
    % intent is partially actuate-able
    IntentPayload = [policy = [spin, reverse_spin, jump]],
    published(intent, IntentPayload),
    % TODO - NOT RECEIVED
    thread_get_message(message(can_actuate(IntentPayload, Directive), EffectorCA)),
    assertion(member(spin, Directive)),
    assertion(member(reverse_spin, Directive)),
    assertion(\+member(jump, Directive)).
