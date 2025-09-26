/**
Evaluate causal theory and request new one if unsatisfactory, grant past plans affordance status if goals achieved
**/

:- module(assess, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 
unit_of_work(CA, State, done(State)) :-
    log(info, assess, "Phase assess done for CA ~w with ~p", [CA, State]).
