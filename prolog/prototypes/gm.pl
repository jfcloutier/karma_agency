:- module(gm, [is_gm/1, gm_umwelt/2, gm_sensory_domain/2,  gm_belief_domain/2, gm_action_domain/2]).

:- use_module(gms_db).
:- use_module(vocabulary).

is_gm(GM) :-
    gm(GM).

gm_umwelt(GM, Umwelt) :-
    is_gm(GM),
    setof(object(ObjectName, ObjectType), GM:in_umwelt(ObjectName, ObjectType), Umwelt), !.
gm_umwelt(GM, []) :-
    is_gm(GM).

gm_sensory_domain(GM, SensoryDomain) :-
    is_gm(GM),
    setof(sense(PropertyName, ObjectName, ValueType), gm_can_sense(GM, PropertyName, ObjectName, ValueType), SensoryDomain), !.
gm_sensory_domain(GM, []) :-
    is_gm(GM).

gm_belief_domain(GM, BeliefyDomain) :-
    is_gm(GM),
    setof(belief(PropertyName, ObjectName, ValueType), gm_can_believe(GM, PropertyName, ObjectName, ValueType), BeliefyDomain), !.
gm_belief_domain(GM, []) :-
    is_gm(GM).

gm_action_domain(GM, ActionDomain) :-
    is_gm(GM),
    setof(action(ActionName), GM:Effect^in_action_domain(ActionName, Effect), ActionDomain), !.
gm_action_domain(GM, []) :-
    is_gm(GM).

%% PRIVATE

gm_can_sense(GM, PropertyName, ObjectName, ValueType) :-
    GM:in_umwelt(ObjectName, ObjectType),
    GM:in_sensory_domain(PropertyName, ObjectType),
    property_type(PropertyName, ObjectType, ValueType).

gm_can_believe(GM, PropertyName, ObjectName, ValueType) :-
    GM:in_umwelt(ObjectName, ObjectType),       
    GM:in_belief_domain(PropertyName, ObjectType, ValueType).
