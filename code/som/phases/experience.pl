/*
Integrate current and past observations into experiences by:

1- synthesizing as many `count`, `more` and `trend` experiences as time allows 
2- elevating all unused observations as experiences (need before_work/3 and after_work/3)

Value domains: 

* count: 1, 2, 3, `many`
* more: another synthetic object
* trend: `up`, `down`, `same`, `ended`

Synthesizing experiences:

* Before work, update prior experiences and assign their confidences
  * A prior experience may no longer exist,
  * or a trend may take value `ended`
  * or a count may take value 1
* In each unit of work, find a novel experience and assign a confidence

Finding a novel experience:

* Choose in order a kind from [count, more, trend]
* Find a non-empty, maximal set of observations to which the kind applies
* The maximal set of observations defines a synthetic object as the origin of the property/relation
    * count: 2 or more countables: 
        * Identical properties (same origin, kind and value)
        * Properties with same kind and value
        * Relations with same kind and value object
        * Relations with same kind and origin
    * more: 2 non-equal comparables (each a different maximal set of observations)
        * Properties with same kind and value X 2 (more * --P-> value A than * --Q-> value B)
        * Relations with same kind and value object X 2 (more * --X-> object A than * --Y-> object B - there are more objects with relations X to object A than there are objects with relation Y to object B)
        * Relations with same kind and origin X 2 (more object A --X-> * than object B --Y-> * - object A has more relations X than object B has relations Y)
    * trend: 2 or more trendables over the last N > 1 time frames
        * Properties with the same origin and kind
            * If values are numerical, the values can be `up`, `down` or `same`
            * Otherwise, the trend values can only be `same`

Assigning a confidence to an experience:

* Take the minimum confidence in the observed set(s) composing the synthetic object(s) of the experience (don't multiply individual confidences)
* if a trend, decrease the minimum confidence C by C/N where N is the number of trending observations

Elevating unused observations:

* After work
    * Select all observations not composing objects in current experiences
    * Add them as experiences with unchanged confidence

*/

:- module(experience, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, State, State).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution. 

% Add one experience at a time, most relevant first.
% unit_of_work(CA, State, more(NewState, WellbeingDeltas)) :-
%     experience(CA, State, Experience),
%     acc_state(State, experience, Experience, NewState),
%     wellbeing:empty_wellbeing(WellbeingDeltas),
%     log(info, experience, "Phase experience for CA ~w added ~p", [CA, Experience]).

% unit_of_work(CA, State, done(State, WellbeingDeltas)) :-
%     wellbeing:empty_wellbeing(WellbeingDeltas),
%     log(info, experience, "Phase experience done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

% TODO
% experience(CA, State, Experience)
% experience(_, _, _).

unit_of_work(CA, State, more(State, WellbeingDeltas)) :-
    member(N, [1,2,3,4,5,6,7,8,9]),
    sleep(0.02),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase plan more (~w) for CA ~w", [N, CA]).

unit_of_work(_, State, done(State, WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).
