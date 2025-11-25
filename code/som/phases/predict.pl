/**
Make predictions from current observations 
**/

:- module(predict, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 
unit_of_work(CA, State, more(State)) :-
    member(N, [1,2,3,4,5,6,7,8,9]),
    sleep(0.02),
    log(info, predict, "Phase predict more (~w) for CA ~w", [N, CA]).
