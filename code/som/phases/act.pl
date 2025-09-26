/**
Confirm plan feasibility, execute, remember goal and plan for later assessment
**/

:- module(act, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 
unit_of_work(CA, State, done(State)) :-
    log(info, act, "Phase act done for CA ~w with ~p", [CA, State]).
