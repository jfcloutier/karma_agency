:- module(vocabulary, [object/1, object_type/1, value_type/2, property_type/3, added_property_type/3]).

:- use_module(gms_db).

%% The agent's dynamic extension to the core typology

:- use_module(prior_types).

object_type(ObjectType) :-
    prior_types:prior_object_type(ObjectType).

property_type(Predicate, ObjectType, ValueTypeName) :-
    added_property_type(Predicate, ObjectType, ValueTypeName) 
    ; prior_types:prior_property_type(Predicate, ObjectType, ValueTypeName).

value_type(ValueTypeName, ValueDomain) :-
    prior_types:prior_value_type(ValueTypeName, ValueDomain).

value_type(ValueTypeName, ValueDomain) :-
    property_value_type(nonsense, ValueTypeName, ValueDomain).

added_property_type(Predicate, ObjectType, ValueTypeName) :-
    gm(GM),
    GM:in_object_domain(_, ObjectType),
    GM:in_belief_domain(Predicate, ObjectType, ValueTypeName).

property_value_type(PropertyName, ObjectType, ValueType) :-
    property_type(PropertyName, ObjectType, ValueType).

% TODO
object(Object) :- true.