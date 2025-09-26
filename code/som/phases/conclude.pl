/**
The timeframe concludes - update wellbeing measures and emit wellbeing changes
**/

:- module(conclude, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 
unit_of_work(CA, State, done(State)) :-
    log(info, conclude, "Phase conclude done for CA ~w with ~p", [CA, State]).
