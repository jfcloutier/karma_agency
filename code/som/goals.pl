/*
Utilities for goals.

goal{target: Target, impact: Impact, priority: Priority, intent_level: Level}
target{origin: Origin, kind: Kind, value: Value}
Impact = terminate | persist | create
*/

:- module(goals, [is_goal/1, is_command/1, same_goals/2]).

:- use_module(agency(som/ca_support)).

is_goal(Directive) :-
    is_dict(Directive, goal).

is_command(Directive) :-
    is_dict(Directive, command).

same_goals(Goal1, Goal2) :-
    is_goal(Goal1),
    is_goal(Goal2),
    goal_id(Goal1, GoalId),
    goal_id(Goal2, GoalId).


