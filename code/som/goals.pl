/*
Utilities for goals.

goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id: IntentId, intent_level: Level}
target{origin: Origin, kind: Kind, value: Value}

*/

:- module(goals, [goal_with_id/2]).

:- use_module(agency(som/ca_support)).

goal_with_id(GoalWithoutId, Goal) :-
    goal_id(GoalWithoutId, Id),
    Goal = GoalWithoutId.put(id, Id).

% Two goals from any CAs must have the same ids if they are semantically equivalent (same target and same impact)
%  A goal's ID is fully determined by Target and Impact.
goal_id(Goal, Id) :-
    goal{target: Target, impact: Impact} :< Goal,
    value_hash(Impact, ImpactHash),
    target_hashes(Target, TargetHashes),
    atomic_list_hash([ImpactHash | TargetHashes], Id).


target_hashes(Target, [OriginHash, KindHash, ValueHash]) :-
    target{origin: Origin, kind: Kind, value: Value} :< Target,
    object_hash(Origin, OriginHash),
    value_hash(Kind, KindHash),
    value_hash(Value, ValueHash).
