/**
Integrate updated history of observations into new experiences, remove obsolete experiences, (re)assign value
**/

:- module(experience, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 
unit_of_work(CA, State, done(State)) :-
    log(info, experience, "Phase experience done for CA ~w with ~p", [CA, State]).
