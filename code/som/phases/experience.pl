/*
The dynamic cognition actor tracks changes in prior experiences and uncovers new ones by integrating current and past observations.

Experiences are instantiated by:

1- tracking changes to prior experiences
2- synthesizing as many novel `count`, `more` and `trend` experiences as time allows 
3- elevating all "unexperienced" observations (observations of umwelt experiences not integrated into new experiences) as experiences of the dynamic cognition actor

An experience, like an observation, is a property or relation to which a confidence and other meta-data is associated.
A property or relation applies to one object (its origin) and it has one value.
The value of a relation is another object.
The value of a property is a number or a label (e.g. true, false, red, green, up, down etc.)
The kind of property/relation determines the domain of permissible values.

Value domains of synthetic experiences: 

* count: 1, 2, 3, `many`
* trend: `up`, `down`, `ended`
* more: an object

Synthesizing experiences:

* Before work
    * Update prior experiences and assign their confidences
        * A prior experience may no longer exist,
        * or a trend may take value `ended`
* In each unit of work
    * find a novel experience and assign a confidence
* After work
    * Select all observations from static CAs not composing objects in current experiences
    * Add them as experiences with unchanged confidence

Finding a novel experience (non-deterministic):

* Choose a kind of experience from [count, more, trend]
* Find a non-empty, maximal (can not be grown further) set of observations to which the kind applies

Finding a maximal set of observations (or two if a relation) for a kind of experience:

* A maximal set of observations defines a synthetic object as 
    * the origin of any property/relation
    * the value of a `more` relation
* A meaningful set per kind:
    * `count`: 1 or more countables: 
        * Identical properties (same origin, kind and value)
        * Properties with same kind and value
        * Relations with same kind and value object
        * Relations with same kind and origin
    * `more`: 2 non-equal comparables (each a different maximal set of observations, one as origin, the other as value)
        * Properties with same kind and value X 2 (more * --P-> value A than * --Q-> value B)
        * Relations with same kind and value object X 2 (more * --X-> object A than * --Y-> object B - there are more objects with relations X to object A than there are objects with relation Y to object B)
        * Relations with same kind and origin X 2 (more object A --X-> * than object B --Y-> * - object A has more relations X than object B has relations Y)
    * `trend`: 2 or more trendables over the last N > 1 time frames
        * Properties with the same origin and kind
            * If values are numerical, the values can be `up` or `down`
            * Otherwise, the trend value can be `ended` if a trend from the prior timeframe is being updated

Assigning confidence to an experience:

* Take the average confidence in the observed set(s) composing the synthetic object(s) of the experience (don't multiply individual confidences)
* If a `trend`, confidence is boosted when the trend is maintained across timeframes (a trend over 5 timeframes is stronger than one over only 2)

*/

:- module(experience, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/ca_support)).

% Update prior, synthetic experiences that matter most (i.e. all integrated prior experiences attended to)
before_work(CA, State, NewState) :-
    [Timeframe | _] = State.timeframes,
    !,
    PriorExperiences = Timeframe.experiences,
    updated_experiences(CA, State, PriorExperiences, [], UpdatedExperiences),
    put_state(State, experiences, UpdatedExperiences, NewState),
    log(info, experience, "Phase experience (before) for CA ~w updated experiences ~p", [CA, UpdatedExperiences]).

before_work(_, State, State).

% Elevate all unexperienced observations from static CAs
after_work(CA, State, NewState) :-
    elevated_static_ca_observations(State, ElevatedExperiences),
    acc_state(State, experiences, ElevatedExperiences, NewState),
    log(info, experience, "Phase experience (after) for CA ~w elevated experiences ~p", [CA, ElevatedExperiences]).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 

% Add one experience at a time, most relevant first.
unit_of_work(CA, State, more(StateDeltas, WellbeingDeltas)) :-
    novel_experience(CA, State, Experience),
    StateDeltas = [experiences-[Experience]],
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, experience, "Phase experience (more) for CA ~w added ~p", [CA, Experience]).

unit_of_work(CA, _, done([], WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, experience, "Phase experience done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

% Get rid of repeated, updated experiences
updated_experiences(_, _, [], Acc, UpdatedExperiences) :- sort(Acc, UpdatedExperiences).
updated_experiences(CA, State, [PriorExperience | Rest], Acc, UpdatedExperiences) :-
    updated_experience(CA, State, PriorExperience.kind, PriorExperience, UpdatedExperience) ->
        updated_experiences(CA, State, Rest, [UpdatedExperience| Acc], UpdatedExperiences),
        log(info, experience, "~w has updated experience ~p to ~p", [CA, PriorExperience, UpdatedExperience])
        ;
        updated_experiences(CA, State, Rest, Acc, UpdatedExperiences).

% A prior `count` experience is updated by keeping current observations that have survived from the prior experience's set of support
% then, if not empty, attempting to extend this support from the surviving observations, and, finally, counting the maximal set.
updated_experience(CA, State, count, PriorExperience, UpdatedExperience) :-
    % Update the set of countable objects given the observations supporting the prior experience's origin object
    count_from_prior_object(State, PriorExperience.origin, Count, CountedObservations),
    count_experience(CA, Count, CountedObservations, UpdatedExperience).

% A prior `more` is updated by making current the counted observations being compared
% Note: The counts in the accumulated updated experiences could act as a cache of pre-recounts, to avoid unnecessary recounts (use tabling?)
updated_experience(CA, State, more, PriorExperience, UpdatedExperience) :-
    count_from_prior_object(State, PriorExperience.origin, Count1, CountedObservations1),
    count_from_prior_object(State, PriorExperience.value, Count2, CountedObservations2),
    more_experience(CA, Count1, CountedObservations1, Count2, CountedObservations2, UpdatedExperience).

% See if the prior experience is still a trend, albeit reversed or ended.
% Find the prior observation the prior experience trended on
% Find a current observation that would extend or change the trend
% Create an updated trend experience
updated_experience(CA, State, trend, PriorExperience, UpdatedExperience) :-
    [Timeframe | _] = State.timeframes,
    member(PriorObservation, Timeframe.observations),
    member(PriorObservation.id, PriorExperience.origin.support),
    member(Observation, State.observations),
    comparable_observations(PriorObservation, Observation),
    trend_experience(CA, State, PriorObservation, Observation, UpdatedExperience),
    !.

% If a prior experience can not be extended/changed by a current observation, it is ended
updated_experience(_, _, trend, PriorExperience, UpdatedExperience) :-
    UpdatedExperience = PriorExperience.put(value, ended).

count_from_prior_object(State, Object, Count, CountedObservations) :-
    PriorObservationIds = Object.support,
    surviving_observations(State, PriorObservationIds, [], Observations),
    % Find maximal set of countable observations starting from surviving observation. Fails if nothing to count.
    countable_observations(State.observations, Observations, CountedObservations),
    length(CountedObservations, Count).


% Observations from prior timeframe that are still current
surviving_observations(_, [], Acc, Acc).

surviving_observations(State, [ObservationId | Rest], Acc, CurrentObservations) :-
    surviving_observation(State, ObservationId, Observation) ->
        surviving_observations(State, Rest, [Observation | Acc], CurrentObservations)
        ;
        surviving_observations(State, Rest, Acc, CurrentObservations).

% An observation from a prior timeframe that is still current
surviving_observation(State, ObservationId, Observation) :-
    prior_observation_from_id(State, ObservationId, Observation),
    member(CurrentObservation, State.observations),
    equivalent_observation(Observation, CurrentObservation).

prior_observation_from_id(State, ObservationId, PriorObservation) :-
    member(Timeframe, State.timeframes),
    member(PriorObservation, Timeframe.observations),
    PriorObservation.id == ObservationId,
    !.

% Equivalent observation across timeframes?
% A timeframe generates a new id for each observation it converts from a prediction 
equivalent_observation(Observation, OtherObservation) :-
    observation{origin:Object1, kind:Kind, value:Value1} :< Observation,
    observation{origin:Object2, kind:Kind, value:Value2} :< OtherObservation,
    same_object(Object1, Object2),
    same_value(Value1, Value2).

% Two objects of the same type with the same id are semantically the same.
same_object(Object1, Object2) :-
    is_dict(Object1, object),
    is_dict(Object2, object),
    object{type:Type, id:Id} :< Object1,
    object{type:Type, id:Id} :< Object2.

countable_observations([], Acc, Acc).

countable_observations([Observation | Rest], Acc, CountedObservations) :-
    member(Observation, Acc) ->
      countable_observations(Rest, Acc, CountedObservations)
      ;
      (countable_with_all(Observation, Acc) ->
        countable_observations(Rest, [Observation | Acc], CountedObservations)
        ;
        countable_observations(Rest, Acc, CountedObservations)).

countable_with_all(Observation, OtherObservations) :-
    forall(member(OtherObservation, OtherObservations), countable_with(Observation, OtherObservation)).

/*
Countable if:
    * Same origin, kind and value - counting identically described objects, or
    * Same kind and value - counting objects with same description, or
    * Same origin and kind - counting alternate relations of a kind for an object
*/
countable_with(Observation, OtherObservation) :-
    equivalent_observation(Observation, OtherObservation).

countable_with(Observation, OtherObservation) :-
    observation{kind:Kind, value:Value1} :< Observation,
    observation{kind:Kind, value:Value2} :< OtherObservation,
    same_value(Value1, Value2).

countable_with(Observation, OtherObservation) :-
    observation{origin:Object1, kind:Kind} :< Observation,
    observation{origin:Object2, kind:Kind} :< OtherObservation,
    same_object(Object1, Object2).

same_value(Value, Value).

same_value(Value1, Value2) :-
    same_object(Value1,Value2).

count_experience(CA, Count, CountedObservations, Experience) :-
    simple_count(Count, SimpleCount),
    average_confidence(CountedObservations, Confidence),
    Confidence > 0,
    sorted_ids(CountedObservations, CountedObservationIds),
    synthetic_object(CountedObservationIds, Object),
    Experience = experience{origin:Object, kind:count, value:SimpleCount, confidence:Confidence, by:CA}.

more_experience(CA, Count1, CountedObservations1, Count2, CountedObservations2, Experience) :-
    Count1 \= Count2,
    sorted_ids(CountedObservations1, CountedObservationIds1),
    synthetic_object(CountedObservationIds1, Object1),
    sorted_ids(CountedObservations2, CountedObservationIds2),
    synthetic_object(CountedObservationIds2, Object2),
    average_confidence(CountedObservations1, Confidence1),
    average_confidence(CountedObservations2, Confidence2),
    (Confidence1 * Confidence2) > 0,
    Confidence is (Confidence1 + Confidence2) / 2,
    (grossly_more_than(Count1, Count2) ->
        Experience = experience{origin:Object1, kind:more, value:Object2, confidence:Confidence, by:CA}
        ;
        Experience = experience{origin:Object2, kind:more, value:Object1, confidence:Confidence, by:CA}
    ).

% A trend goes up or down or else there is no trend
trend_experience(CA, State, PriorObservation, Observation, Experience) :-
    PriorValue = PriorObservation.value,
    CurrentValue = Observation.value,
    sorted_ids([PriorObservation, Observation], ObservationIds),
    synthetic_object(ObservationIds, Object),
    (CurrentValue \= PriorValue ->
        CurrentValue > PriorValue ->
        TrendExperience = experience{origin:Object, kind:trend, value:up, by:CA}
        ;
        CurrentValue < PriorValue ->
        TrendExperience = experience{origin:Object, kind:trend, value:down, by:CA}
        ),
    average_confidence([PriorObservation, Observation], CurrentConfidence),
    trend_confidence(State, TrendExperience, CurrentConfidence, Confidence),
    Experience = TrendExperience.put(confidence, Confidence).

% Confidence in a trend is altered by its history 
trend_confidence(State, TrendExperience, CurrentConfidence, Confidence) :-
    previous_trend_experience(State, TrendExperience, PriorTrendExperience),
    !,
    adjust_trend_confidence(PriorTrendExperience, TrendExperience, CurrentConfidence, Confidence).

trend_confidence(_, _, Confidence, Confidence).

previous_trend_experience(State, TrendExperience, PriorTrendExperience) :-
    [Timeframe | _] = State.timeframes,
    member(PriorTrendExperience, Timeframe.experiences),
    trend == PriorTrendExperience.kind,
    % The origin of a prior trend experience shares exactly one supporting observation.
    intersection(PriorTrendExperience.origin.support, TrendExperience.origin.support, [_]).

adjust_trend_confidence(PriorTrendExperience, TrendExperience, CurrentConfidence, Confidence) :-
    (PriorTrendExperience.value == TrendExperience.value ->
        Confidence is min(1.0, CurrentConfidence + (PriorTrendExperience.confidence / 2))
        ;
        Confidence == CurrentConfidence
     ).

% object{type:synthetic, id:Id, support: [ObservationId, ...]} - support is the private set of Observations by the dCA from which the synthetic object was created by the dCA.
% Note: Two objects of the same type about the same "thing" must have identical ids, irrespective of the CA that created the object.
synthetic_object(ObservationIds, Object) :-
    atomic_list_hash(ObservationIds, ObjectId),
    Object = object{type:synthetic, id:ObjectId, support:ObservationIds}.

sorted_ids(Items, Ids) :-
    all_ids(Items, [], AllIds),
    sort(AllIds, Ids).

all_ids([], Acc, Acc).
all_ids([Item | Rest], Acc, AllIds) :-
    Id = Item.id,
    all_ids(Rest, [Id | Acc], AllIds).

% Count from 1 to 3 and then many. Else counting fails.
simple_count(N, Count) :-
    N > 0,
    (N > 3 -> Count = many ; Count = N).

% Is a count perceivable as greater than another?
% Comparing rounded log2 for numbers greater than 3.
grossly_more_than(Count, Count) :- fail, !.
grossly_more_than(Count1, Count2) :-
    Count1 < 4,
    Count1 > Count2.
grossly_more_than(Count1, Count2) :-
    Count1 > 3,
    round(Count1 ** 0.5) > round(Count2 ** 0.5).

% experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}
novel_experience(CA, State, Experience) :-
    member(Kind, [count, more, trend]),
    % Observations are sorted most confident first
    predsort(priority, State.observations, SortedObservations),
    new_experience(CA, State, Kind, SortedObservations, Experience),
    \+ member(Experience, State.experiences).

% Don't compare to (=) else, a dict with the same confidences as another is removed
priority(Delta, Dict1, Dict2) :-
    get_dict(confidence, Dict1, Confidence1),
    get_dict(confidence, Dict2, Confidence2),
    compare(Delta1, Confidence1, Confidence2),
    (Delta1 == (=) -> Delta = (<) ; Delta1 == (<) -> Delta = (>) ; Delta = (<)).

% A novel count experience must count at least 2 things
new_experience(CA, _, count, Observations, Experience) :-
    select(Observation, Observations, OtherObservations),
    countable_observations(OtherObservations, [Observation], CountedObservations),
    length(CountedObservations, Count),
    Count > 1,
    count_experience(CA, Count, CountedObservations, Experience).

% A new `more` experience is of interest if comparing to a value
new_experience(CA, _, more, Observations, Experience) :-
    select(Observation1, Observations, OtherObservations1),
    select(Observation2, Observations, OtherObservations2),
    Observation1.id \= Observation2.id,
    countable_observations(OtherObservations1, [Observation1], CountedObservations1),
    countable_observations(OtherObservations2, [Observation2], CountedObservations2),
    length(CountedObservations1, Count1),
    length(CountedObservations2, Count2),
    Count1 > Count2,
    Count2 > 1,
    more_experience(CA, Count1, CountedObservations1, Count2, CountedObservations2, Experience).

% A novel trend experience looks at the values of comparable number-valued observations (same origin and kind) from the previous and this timeframe
new_experience(CA, State, trend, _, Experience) :-
    [Timeframe | _] = State.timeframes,
    predsort(priority, Timeframe.observations, PriorObservations),
    member(PriorObservation, PriorObservations),
    number(PriorObservation.value),
    member(Observation, State.observations),
    comparable_observations(PriorObservation, Observation),
    trend_experience(CA, State, PriorObservation, Observation, Experience).

comparable_observations(Observation1, Observation2) :-
    is_dict(Observation1, observation),
    is_dict(Observation2, observation),
    Observation1.kind == Observation2.kind,
    same_object(Observation1.origin, Observation2.origin).

% Select all observations by the dCA from static CAs not composing objects in current experiences
% and add them, without duplicates, as experiences with unchanged confidence (the support of observed objects are always empty because observations are converted predictions)
elevated_static_ca_observations(State, Experiences) :-
    findall(Observation, 
        (member(Observation, State.observations), is_static_ca_observation(Observation), \+ in_experiences(Observation, State)), 
        Observations),
    findall(Experience, (member(Observation, Observations), observation_as_experience(Observation, Experience)), Experiences).

elevated_static_ca_observations(_, []).

is_static_ca_observation(Observation) :-
   memberchk(Observation.origin.type, [sensor, effector]).

in_experiences(Observation, State) :-
    member(Experience, State.experiences), 
    in_experience(Observation.id, Experience).

in_experience(ObservationId, Experience) :-
    ObservationIds = Experience.origin.support,
    member(ObservationId, ObservationIds).

in_experience(ObservationId, Experience) :-
    experience{kind:trend, value:Object} :< Experience,
    is_dict(Object, object),
    ObservationIds = Object.support,
    member(ObservationId, ObservationIds).

observation_as_experience(Observation, Experience) :-
    observation{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA} :< Observation,
    Experience = experience{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA}.