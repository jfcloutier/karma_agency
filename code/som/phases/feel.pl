/*
Assign a feeling (from worst to best) to each experience.

First compute a feeling for the current timeframe, then, for each persistent experience,
modulate the feeling from its history.
*/

:- module(feel, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, State, State).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 
unit_of_work(CA, State, done(StateDeltas, WellbeingDeltas)) :-
    feeling(CA, State, Feeling),
    felt_experiences(CA, State, Feeling, FeltExperiences),
    StateDeltas = [feeling-Feeling, experiences-FeltExperiences],
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, feel, "Phase feel ended for CA ~w", [CA]).

% Captures in an unbounded, scalar integer value the current overall feeling of the CA at the moment
feeling(CA, State, Feeling) :-
    findall(WellbeingFeeling, (wellbeing:dimension(Dimension), wellbeing_feeling(State, Dimension, WellbeingFeeling)), WellbeingFeelings),
    weighted_abs_max(WellbeingFeelings, Feeling).

% A negative value weighs more than its absolute value
weighted_abs_max([Val | Rest], Max) :-
    weighted_abs_max(Rest, Val, Max).

weighted_abs_max([], Max, Max).
weighted_abs_max([Val | Rest], Acc, Max) :-
    weighted(Val, Val1),
    weighted(Acc, Acc1),
    abs(Val1) > abs(Acc1) ->
        weighted_abs_max(Rest, Val, Max)
        ;
        weighted_abs_max(Rest, Acc, Max).

% A negative value weighs twice as much as its absolute value
weighted(Val, Val) :-
    Val >= 0.
weighted(Val, Val1) :-
    Val < 0,
    Val1 is Val * 2.

wellbeing_feeling(State, Dimension, WellbeingFeeling) :-
    WellbeingFeeling = State.get(wellbeing/Dimension).

% Adds a feeling value to each experience based on the current overall feeling
% and the history of the expeirence if it is persisting over multiple timeframes
felt_experiences(CA, State, Feeling, FeltExperiences) :-
    findall(FeltExperience, (member(Experience, State.experiences), felt_experience(State, Feeling, Experience, FeltExperience)), FeltExperiences).

% Modulate the current feeling by the experience's history, and assign to it.
felt_experience(State, Feeling, Experience, FeltExperience) :-
    feeling_modulation(State, Experience, Modulation),
    ModulatedFeeling is Feeling * Modulation,
    FeltExperience = Experience.put(feeling, ModulatedFeeling).

% TODO
feeling_modulation(State, Experience, 1).