/*
Utilities for observations

% observation{id:Id, origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}
*/

:- module(observations, [observation_with_id/2, is_activation_observation/1, observation_target/2, is_sensory_observation/1]).

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

is_activation_observation(Observation) :-
    observation{kind:activation} :< Observation.

observation_target(Observation, Target) :-
    Target = target{origin: Observation.origin, kind: Observation.kind, value: Observation.value}.

is_sensory_observation(Observation) :-
    object{type: sensor} :< Observation.origin.
