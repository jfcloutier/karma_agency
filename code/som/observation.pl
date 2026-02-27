/*
Utilities for observations
*/

:- module(observation, [observation_with_id/2]).

:- use_module(agency(som/ca_support)).

observation_with_id(ObservationWithoutId, Observation) :-
    observation_id(ObservationWithoutId, Id),
    Observation = ObservationWithoutId.put(id, Id).

% Two observations from any CAs must have the same ids if they are semantically equivalent.
observation_id(Observation, Id) :-
    observation{origin:Object, kind:Kind, value:Value} :< Observation,
    object_hash(Object, ObjectHash),
    value_hash(Value, ValueHash),
    atomic_list_hash([ObjectHash, Kind, ValueHash], Id).
