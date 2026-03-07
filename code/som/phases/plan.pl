/*
1. Select an intent
    The candidate goals are:
        * An unachieved but unexpired prior intent - there is at most 1
        * Directives received as plan(s)
        * Threshold-crossing experiences to impact
    If none, do nothing more
    Else, pick the highest priority goal (priority is skewed by source)

2. Select a candidate intent and reuse/build a plan for it

3. Emit plan to umwelt

Data:

A goal is a relation/property to impact
An intent is a self-assigned goal
A directive is a delegated goal
A plan is a set of directives to achieve an intent
An affordance is a reusable plan with an effectiveness score

goal{id: ID, target: Target, impact: Impact, priority: Priority, status: Status, plan: Plan, of: CA}

    Target: target{origin: Origin, kind: Kind, value: Value} - matches observations/experiences
    Impact :: create | persist | terminate
    Plan :: PlanId | none
    Status :: pending | achieved | failed
    Priority :: 0.0..1.0

plan{id: ID, for_goal: GoalId, directives: [Goal, ...], source: Source, status: Status, score: Score, of: CA}

    Source :: self or CA
    Score: 0.0..1.0 | none
*/

:- module(plan, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, _, [], WellbeingDeltas) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 
unit_of_work(_, _, done([], WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).
