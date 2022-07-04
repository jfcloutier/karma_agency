:- module(scope,
          [ empty_scope/1,
            gm_scope/2,
            object_domain/2,
            sensory_domain/2,
            belief_domain/2,
            action_domain/2
          ]).

:- use_module(gms_db).
:- use_module(vocabulary).

empty_scope(scope{action_domain:[], belief_domain:[], object_domain:[], sensory_domain:[]}).

gm_scope(GM, Scope) :-
    gm(GM),
    object_domain(GM, ObjectDomain),
    object_domain(GM, ObjectDomain),
    sensory_domain(GM, SensoryDomain),
    belief_domain(GM, BeliefDomain),
    action_domain(GM, ActionDomain),
    Scope=scope{action_domain:ActionDomain, belief_domain:BeliefDomain, object_domain:ObjectDomain, sensory_domain:SensoryDomain}.

object_domain(GM, ObjectDomain) :-
    findall(typed_object(Name, Type),
            GM:in_object_domain(Name, Type),
            ObjectDomain).

sensory_domain(GM, SensoryDomain) :-
    findall(SensoryPredicate, sensory_predicate(GM, SensoryPredicate), SensoryDomain).

belief_domain(GM, BeliefDomain) :-
    findall(BeliefPredicate, belief_predicate(GM, BeliefPredicate), BeliefDomain).

action_domain(GM, ActionDomain) :-
    findall(ActionPredicate, action_predicate(GM, ActionPredicate), ActionDomain).

sensory_predicate(GM, SensoryPredicate) :-
    GM:in_sensory_domain(PredicateName, ObjectType),
    property_type(PredicateName, ObjectType, ValueTypeName),
    value_type(ValueTypeName, ValueDomain),
    SensoryPredicate=sensory_predicate(PredicateName, ObjectType, ValueDomain).

belief_predicate(GM, BeliefPredicate) :-
    GM:in_belief_domain(PredicateName, ObjectType, ValueTypeName),
    value_type(ValueTypeName, ValueDomain),
    BeliefPredicate=belief_predicate(PredicateName, ObjectType, ValueDomain).

action_predicate(GM, ActionPredicate) :-
    GM:in_action_domain(ActionName, Effect),
    ActionPredicate=action(ActionName, Effect).

