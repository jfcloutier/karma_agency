/*
Evaluate goodness of each experience.
Evaluate causal theory and request new one if unsatisfactory.
Grant past plans affordance status if goals achieved
*/

:- module(assess, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, _, [], WellbeingDeltas) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).


% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 
unit_of_work(CA, State, done(StateDeltas, WellbeingDeltas)) :-
    staying_alive(State, Alive),
    StateDeltas = [alive=Alive],
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, assess, "Phase assess ended for CA ~w", [CA]).

staying_alive(State, Alive) :-
    get_state(State, timeframe_count, Count),
    get_state(State, settings, Settings),
    option(max_timeframes(Max), Settings, infinity),
    ((Max \== infinity, Count >= Max) -> Alive = false ; Alive = true).