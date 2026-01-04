/**
Confirm plan feasibility, execute, remember goal and plan for later assessment
**/

:- module(act, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, State, State).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution. 
unit_of_work(CA, State, more(State, WellbeingDeltas)) :-
    member(N, [1,2,3,4,5,6,7,8,9]),
    sleep(0.02),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase act more (~w) for CA ~w", [N, CA]).

unit_of_work(_, State, done(State, WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).

