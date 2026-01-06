/*
The dynamic cognition actor integrates current and past observations into its current experiences by:

1- synthesizing as many `count`, `more` and `trend` experiences as time allows 
2- elevating all "unexperienced" observations (observations of umwelt experiences not integrated into new experiences) as experiences of the dynamic cognition actor.

An experience, like an observation, is a property or relation to which a confidence and other meta-data is associated.
A property or relation applies to one object (its origin) and it has one value.
The value of a relation is another object.
The value of a property is a number or a label (e.g. true, false, red, green, up, down etc.)
The kind of property/relation determines the domain of permissible values.

Value domains of synthetic experiences: 

* count: 1, 2, 3, `many`
* trend: `up`, `down`, `same`, `ended`
* more: an object

Synthesizing experiences:

* Before work
    * Update prior experiences and assign their confidences
        * A prior experience may no longer exist,
        * or a trend may take value `ended`
        * or a count may take value 1
* In each unit of work
    * find a novel experience and assign a confidence
* After work
    * Select all observations not composing objects in current experiences
    * Add them as experiences with unchanged confidence

Finding a novel experience (non-deterministic):

* Choose a kind of experience from [count, more, trend]
* Find a non-empty, maximal (can not be grown further) set of observations to which the kind applies

Finding a maximal set of observations (or two if a relation) for a kind of experience:

* A maximal set of observations defines a synthetic object as 
    * the origin of any property/relation
    * the value of a `more` relation
* A meaningful set per kind:
    * `count`: 2 or more countables: 
        * Identical properties (same origin, kind and value)
        * Properties with same kind and value
        * Relations with same kind and value object
        * Relations with same kind and origin
    *`more`: 2 non-equal comparables (each a different maximal set of observations, one as origin, the other as value)
        * Properties with same kind and value X 2 (more * --P-> value A than * --Q-> value B)
        * Relations with same kind and value object X 2 (more * --X-> object A than * --Y-> object B - there are more objects with relations X to object A than there are objects with relation Y to object B)
        * Relations with same kind and origin X 2 (more object A --X-> * than object B --Y-> * - object A has more relations X than object B has relations Y)
    * `trend`: 2 or more trendables over the last N > 1 time frames
        * Properties with the same origin and kind
            * If values are numerical, the values can be `up`, `down` or `same`
            * Otherwise, the trend values can only be `same`

Assigning confidence to an experience:

* Take the minimum confidence in the observed set(s) composing the synthetic object(s) of the experience (don't multiply individual confidences)
* If a `trend`, decrease the minimum confidence C by C/N where N is the number of trending observations

*/

:- module(experience, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% Update prior experiences
before_work(CA, State, NewState) :-
    PriorExperiences = State.experiences,
    Observations = State.observations,
    updated_experiences(CA, Observations, PriorExperiences, [], UpdatedExperiences),
    put_state(State, experiences, UpdatedExperiences, NewState).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution. 

% Add one experience at a time, most relevant first.
unit_of_work(CA, State, more(NewState, WellbeingDeltas)) :-
    novel_experience(CA, State, Experience),
    acc_state(State, experience, Experience, NewState),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, experience, "Phase experience for CA ~w added ~p", [CA, Experience]).

unit_of_work(CA, State, done(State, WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, experience, "Phase experience done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

updated_experiences(_, _, [], Acc, Acc).
updated_experiences(CA, Observations, [PriorExperience | Rest], Acc, UpdatedExperiences) :-
    updated_experience(CA, Observations, PriorExperience.kind, PriorExperience, UpdatedExperience) ->
        updated_experiences(CA, Observations, Rest, [UpdatedExperience| Acc], UpdatedExperiences)
        ;
        updated_experiences(CA, Observations, Rest, Acc, UpdatedExperiences).

% TODO
updated_experience(CA, Observations, count, PriorExperience, UpdatedExperience) :-
    

% TODO
updated_experience(CA, Observations, more, PriorExperience, UpdatedExperience).


% TODO
updated_experience(CA, Observations, trend, PriorExperience, UpdatedExperience).


% TODO
% experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}
% object{type:sensor, id:Id, support: [Observation, ...]} - support is the private set of Observations from which the object was synthesized.
% novel_experience(CA, State, Experience)
novel_experience(_, _, _).




% unit_of_work(CA, State, more(State, WellbeingDeltas)) :-
%     member(N, [1,2,3,4,5,6,7,8,9]),
%     sleep(0.02),
%     wellbeing:empty_wellbeing(WellbeingDeltas),
%     log(info, predict, "Phase plan more (~w) for CA ~w", [N, CA]).

% unit_of_work(_, State, done(State, WellbeingDeltas)) :-
%     wellbeing:empty_wellbeing(WellbeingDeltas).
