/*
Utilities for affordances.

affordance{plan: plan{...}, score: Score} - Score is 0.0..1.0 | none
*/

:- module(affordances, [affordance/3, is_affordance_scored/1]).

affordance(Plan, Score, Affordance) :-
    Affordance = affordance{plan:Plan, score:Score}.

is_affordance_scored(Affordance) :-
    affordance{score: Score} :< Affordance,
    number(Score).