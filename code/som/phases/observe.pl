/**
Merge predictions and prediction errors into new observations, elevate umwelt observations, remove obsolete observations
**/

:- module(observe, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 
unit_of_work(CA, State, done(State)) :-
    log(info, observe, "Phase observe done for CA ~w with ~p", [CA, State]).

