/*
Evaluate causal theory and request new one if unsatisfactory, grant past plans affordance status if goals achieved
*/

:- module(assess, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution. 
unit_of_work(CA, State, done(EndState, WellbeingDeltas)) :-
    put_state(State, alive, false, EndState),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase assess ended for CA ~w", [CA]).

unit_of_work(_, State, done(State, WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).
