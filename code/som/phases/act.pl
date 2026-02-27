/**
Confirm plan feasibility, execute it, remember goal and plan for later assessment
**/

:- module(act, []).

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

