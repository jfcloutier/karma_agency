/*
Utilities for goals.

goal{target: Target, impact: Impact, priority: Priority, intent_level: Level}
target{origin: Origin, kind: Kind, value: Value}
Impact = terminate | persist | create
*/

:- module(goals, [is_goal/1]).

:- use_module(agency(som/ca_support)).

is_goal(Directive) :-
    is_dict(Directive, goal).


