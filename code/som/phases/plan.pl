/*
Formulate and prioritize goals (formulated and received), select a goal, construct plan and emit it
*/

:- module(plan, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution. 
unit_of_work(CA, State, more(State, WellbeingDeltas)) :-
    member(N, [1,2,3,4,5,6,7,8,9]),
    sleep(0.02),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase plan more (~w) for CA ~w", [N, CA]).

unit_of_work(_, State, done(State, WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).

