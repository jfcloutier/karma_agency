/*
The dynamic cognition actor tracks changes in prior experiences and uncovers new ones by integrating current and past observations.

Experiences -`activation` (goal status), `count` (how many), `more` (of what), and `trend` (up, down or steady)- are added to the current timeframe by:

* converting all current observations of plan directive activations into activation experiences of the CA's planned goals
* updating prior synthetic experiences (count, more and trend) if they still exist
* detecting as many novel synthetic experiences as time allows

Before work:

* Convert recent directive activation observations into goal activation experiences
* Update prior synthetic experiences
    * A prior experience may persist or may no longer exist
    * A prior count may now take a different value, including 1
    * A prior trend might take a different value (up to steady, down to up etc.)

With each unit of work:

    * find a novel synthetic experience, most relevant first
    * until time's up or no more can be found

A CA has limited counting and magnitude comparing capabilities (this restricts the range of possible experiences upon which world models are built):

   * A CA has no notion of negative or fractional quantities
   * A CA can only count 0, 1 ,2 , 3, many
   * A CA compares magnitudes absolutely for number less than 4, else it compares their log2 values.
*/

:- module(experience, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/objects)).
:- use_module(agency(som/goals)).

% Convert all (necessarily recent) activation observations into experiences
% and update prior, synthetic experiences that matter most (i.e. all integrated prior experiences still attended to)
before_work(_, State, [experiences=Experiences], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta),
    goal_activation_experiences(CA, State, ActivationExperiences),
    [PriorTimeframe | _] = State.timeframes,
    !,
    PriorExperiences = PriorTimeframe.experiences,
    updated_experiences(CA, State, PriorExperiences, [], SyntheticExperiences),
    append(SyntheticExperiences, ActivationExperiences, Experiences).

before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDelta) or done(StateDeltas, WellbeingDelta) as last solution. 

% Synthesize one experience at a time, most relevant first.
unit_of_work(CA, State, more(StateDeltas, WellbeingDelta)) :-
    novel_experience(CA, State, Experience),
    StateDeltas = [experiences=[Experience]],
    wellbeing:empty_wellbeing(WellbeingDelta).

% No new experiences can be sunthesized
unit_of_work(_, _, done([], WellbeingDelta)) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% For each of the CA's goals, i.e. intent and received directives (received as `planned` or `executed` activation predictions), 
% look up the goal's plan, if any, and
% integrate the planned directives observed activation statuses into an experienced goal activation status:
%   * all directives are executed -> plan's goal is executed
%   * all directives are failed or not_relevant -> plan's goal is failed
%   * else all directives are commands or they are planned or executed goals -> plan's goal is (inherently or transitively) planned
%   * else goal activation is necessarily relevant since there is a plan for it
%
% Note that a CA *implicitly* experiences as `relevant` a predicted activation about a goal for which it has no plan (yet)
% if the goal/directive targets an experience it currently has. This implied activation experience is expressed as a prediction error
% sent immediately, at any phase of the lifecycle, in response to activation prediction (see ../ca_support.pl).
% This implied experience is not retained by the CA as part of its explicit experiences.
goal_activation_experiences(CA, State, ActivationExperiences) :-
    get_state(State, plans, Plans),
    findall(ActivationExperience, (member(Plan, Plans), activation_experience_from_plan(Plan, CA, State, ActivationExperience)), ActivationExperiences).

activation_experience_from_plan(Plan, CA, State, ActivationExperience) :-
    plan_activation_status(Plan, State, Status),
    goal_id(Plan.goal, GoalId),
    Origin = object{kind:goal, id:GoalId},
    ActivationExperience = experience{origin:Origin, kind:activation, value:Status, confidence:1.0, by:CA}.

% A plan with commands is necessarily fully `planned`    
plan_activation_status(Plan, _, planned) :-
    forall(member(Directive, Plan.directives), is_command(Directive)).

plan_activation_status(Plan, State, PlanStatus) :-
    forall(member(Directive, Plan.directives), is_goal(Directive)), 
    get_state(State, observations, Observations),
    findall(DirectiveStatus, (member(Directive, Plan.directives), directive_status(Directive, Observations, DirectiveStatus)), DirectiveStatuses),
    planned_goal_status(DirectiveStatuses, PlanStatus).

directive_status(Directive, Observations, DirectiveStatus) :-
    goal_id(Directive, GoalId),
    member(Observation, Observations),
    observation{origin:Origin, kind:activation, value:DirectiveStatus} :< Observation,
    object{type:goal, id:GoalId} :< Origin,
    !.
% Assume the directive's activation status is `relevant` if not observed to be otherwise
directive_status(_, _, relevant).

% Derive the (activation) status of the plan's goal from the observed activation status of the directives in the plan.
planned_goal_status(DirectiveStatuses, executed) :-
    forall(member(Status, DirectiveStatuses), Status == executed).

planned_goal_status(DirectiveStatuses, failed) :-
    forall(member(Status, DirectiveStatuses), memberchk(Status, [failed, not_relevant])).

planned_goal_status(DirectiveStatuses, planned) :-
    forall(member(Status, DirectiveStatuses), memberchk(Status, [planned, executed])).

% The status is `relevant` by default
planned_goal_status(_, relevant).

% Get rid of repeated, updated experiences
updated_experiences(_, _, [], Acc, UpdatedExperiences) :- sort(Acc, UpdatedExperiences).
updated_experiences(CA, State, [PriorExperience | Rest], Acc, UpdatedExperiences) :-
    updated_experience(CA, State, PriorExperience.kind, PriorExperience, UpdatedExperience) ->
        updated_experiences(CA, State, Rest, [UpdatedExperience| Acc], UpdatedExperiences),
        log(info, experience, "~w has updated experience ~p to ~p", [CA, PriorExperience, UpdatedExperience])
        ;
        updated_experiences(CA, State, Rest, Acc, UpdatedExperiences).

% A prior `count` experience is updated by keeping current observations that have survived from the prior experience's evidence/base of support
% then, if not empty, attempting to extend this evidential support from the surviving observations, and, finally, counting the maximal set.
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
    prior_timeframe(State, Timeframe),
    member(PriorObservation, Timeframe.observations),
    member(PriorObservation.id, PriorExperience.origin.evidence),
    member(Observation, State.observations),
    comparable_observations(PriorObservation, Observation),
    trend_experience(CA, State, PriorObservation, Observation, UpdatedExperience),
    !.

% Produce a novel experience.
% Non-determinate so it explores a space of new experiences, one unit of work at a time.
% Start with `count` experiences, then `more` and then `trend`.
% For each kind of experience, look at observations with highest priority first.
novel_experience(CA, State, Experience) :-
    member(Kind, [count, more, trend]),
    % Observations are sorted best "informed" first
    get_state(State, observations, Observations),
    predsort(priority, Observations, PrioritizedObservations),
    new_experience(CA, State, Kind, PrioritizedObservations, Experience),
    get_state(State, experiences, Experiences),
    \+ member(Experience, Experiences).

% A novel `count` experience must count at least 2 things
new_experience(CA, _, count, Observations, Experience) :-
    select(Observation, Observations, OtherObservations),
    countable_observations(OtherObservations, [Observation], CountedObservations),
    length(CountedObservations, Count),
    Count > 1,
    count_experience(CA, Count, CountedObservations, Experience).

% A novel `more` experience is of interest one count is greater than another and both counts are greater than 1
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
    prior_timeframe(State, Timeframe),
    predsort(priority, Timeframe.observations, PriorObservations),
    member(PriorObservation, PriorObservations),
    number(PriorObservation.value),
    get_state(State, observations, Observations),
    member(Observation, Observations),
    comparable_observations(PriorObservation, Observation),
    trend_experience(CA, State, PriorObservation, Observation, Experience).

comparable_observations(Observation1, Observation2) :-
    is_dict(Observation1, observation),
    is_dict(Observation2, observation),
    Observation1.kind == Observation2.kind,
    same_object(Observation1.origin, Observation2.origin).

% Find how many counted observations evidencing a prior count experience can still be counted, together with current observations.
count_from_prior_object(State, Object, Count, CountedObservations) :-
    PriorObservationIds = Object.evidence,
    surviving_observations(State, PriorObservationIds, [], SurvivingObservations),
    % Find maximal set of countable observations starting from surviving observation. Fails if nothing to count.
    countable_observations(State.observations, SurvivingObservations, CountedObservations),
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

% Look for an observation from the prior timeframe with the given id
prior_observation_from_id(State, ObservationId, PriorObservation) :-
    prior_timeframe(State, Timeframe),
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
    * Not an activation
    * Same kind and value - counting objects with same description, or (e.g how many steady trends)
    * Same origin and kind - counting alternate relations of a kind for one object (e.g. how ways a given count is more than another)
*/
countable_with(Observation, OtherObservation) :-
    observation{kind:Kind, value:Value1} :< Observation,
    Kind \= activation,
    observation{kind:Kind, value:Value2} :< OtherObservation,
    same_value(Value1, Value2).

countable_with(Observation, OtherObservation) :-
    observation{origin:Object1, kind:Kind} :< Observation,
    Kind \= activation,
    observation{origin:Object2, kind:Kind} :< OtherObservation,
    same_object(Object1, Object2).

same_value(Value, Value).

same_value(Value1, Value2) :-
    same_object(Value1, Value2).

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

% A trend goes up or down or else is steady
trend_experience(CA, State, PriorObservation, Observation, Experience) :-
    PriorValue = PriorObservation.value,
    CurrentValue = Observation.value,
    synthetic_object([PriorObservation.id, Observation.id], Object),
    (CurrentValue \= PriorValue ->
        compare_simple_counts(CurrentValue, PriorValue, >) ->
            TrendExperience = experience{origin:Object, kind:trend, value:up, by:CA}
        ;
        compare_simple_counts(CurrentValue, PriorValue, <) ->
            TrendExperience = experience{origin:Object, kind:trend, value:down, by:CA}
        ;
            TrendExperience = experience{origin:Object, kind:trend, value:steady, by:CA}
        ),
    average_confidence([PriorObservation, Observation], CurrentConfidence),
    trend_confidence(State, TrendExperience, CurrentConfidence, Confidence),
    Experience = TrendExperience.put(confidence, Confidence).

persisted_observation_count(ObservationId, State, Count) :-
    persisted_observation_count(State.timeframes, ObservationId, 1, Count). % Start count at 1 to include the current timeframe

persisted_observation_count([Timeframe | Rest], ObservationId, Acc, Count) :-
    member(Obs, Timeframe.observations),
    Obs.id == ObservationId,
    Acc1 is Acc + 1,
    !,
    persisted_observation_count(Rest, ObservationId, Acc1, Count).

persisted_observation_count(_, _, Count, Count).

% Confidence in a trend is altered by its history 
trend_confidence(State, TrendExperience, CurrentConfidence, Confidence) :-
    previous_trend_experience(State, TrendExperience, PriorTrendExperience),
    !,
    adjust_trend_confidence(PriorTrendExperience, TrendExperience, CurrentConfidence, Confidence).

trend_confidence(_, _, Confidence, Confidence).

previous_trend_experience(State, TrendExperience, PriorTrendExperience) :-
    prior_timeframe(State, Timeframe),
    member(PriorTrendExperience, Timeframe.experiences),
    experience{origin:Origin, kind:trend} :< PriorTrendExperience,
    % The origin of a prior trend experience shares exactly one supporting observation.
    % Because the evidence of a trend consists of an observation from the timeframe and one from the timeframe before.
    intersection(Origin.evidence, TrendExperience.origin.evidence, [_]).

% Confidence in the current trend experience grows if it maintains an experience from the prior timeframe.
% It is extended by up to half the confidence of the persisted trend experience from the previous timeframe.
adjust_trend_confidence(PriorTrendExperience, TrendExperience, CurrentConfidence, Confidence) :-
    (PriorTrendExperience.value == TrendExperience.value ->
        Confidence is min(1.0, CurrentConfidence + (PriorTrendExperience.confidence / 2))
        ;
        Confidence == CurrentConfidence
     ).

% Comparison used to sort dicts with confidence values.
% Don't produce a (=) comparison, else sorting will eliminate items.
% Invert the comparisons so that the higher the confidence, the earlier in the sorted list.
priority(Delta, Dict1, Dict2) :-
    get_dict(confidence, Dict1, Confidence1),
    get_dict(confidence, Dict2, Confidence2),
    compare(Delta1, Confidence1, Confidence2),
    (Delta1 == (=) -> 
        Delta = (<)
        ; 
        Delta1 == (<) -> 
            Delta = (>)
            ;
            Delta = (<)).

sorted_ids(Items, Ids) :-
    all_ids(Items, [], AllIds),
    sort(AllIds, Ids).

all_ids([], Acc, Acc).
all_ids([Item | Rest], Acc, AllIds) :-
    Id = Item.id,
    all_ids(Rest, [Id | Acc], AllIds).

% Is a count perceivable as greater than another?
% Comparing rounded log2 for numbers greater than 3.
grossly_more_than(Count, Count) :- fail, !.
grossly_more_than(Count1, Count2) :-
    Count1 < 4,
    Count1 > Count2.
grossly_more_than(Count1, Count2) :-
    Count1 > 3,
    round(Count1 ** 0.5) > round(Count2 ** 0.5).
