/*
Utilities for goals.

goal{target: Target, impact: Impact, priority: Priority, intent_level: Level}
target{origin: Origin, kind: Kind, value: Value}
*/

:- module(goals, [goal_id/2, is_goal/1]).

:- use_module(agency(som/ca_support)).

is_goal(Directive) :-
    is_dict(Directive, goal).

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

