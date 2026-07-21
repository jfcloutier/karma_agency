/*
The dynamic cognition actor tracks changes in prior percepts and uncovers new ones by integrating current and past observations.

Percepts -`activation` (goal status), `count` (how many of what), `more` (more what of what), and `trend` (what is going up, going down or staying steady)- are added to the current timeframe by:

* updating prior synthetic percepts (`count`, `more` and `trend`) if they still exist
* detecting as many novel synthetic percepts as time allows
* converting all current observations of plan directive activations into activation percepts of the CA's planned goals

Before work:

* Convert recent directive activation observations into goal activation percepts
* Update prior synthetic percepts
    * A prior percept may persist or may no longer exist
    * A prior count may now take a different value, including 1
    * A prior trend might take a different value (up to steady, down to up etc.)

With each unit of work:

    * find a novel synthetic percept, most relevant first
    * until time's up or no more can be found

A CA has limited counting and magnitude comparing capabilities (this restricts the range of possible percepts upon which world models are built):

   * A CA has no notion of negative or fractional quantities
   * A CA can only count 0, 1 ,2 , 3, many
   * A CA compares magnitudes absolutely for number less than 4, else it compares their log2 values.
*/

:- module(perceiving, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/objects)).
:- use_module(agency(som/goals)).

% Convert all (necessarily recent) activation observations into percepts
% and update prior, synthetic percepts that matter most (i.e. all integrated prior percepts still attended to)
before_work(_, State, [percepts=Percepts], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta),
    goal_activation_percepts(CA, State, ActivationPercepts),
    [PriorTimeframe | _] = State.timeframes,
    !,
    PriorPercepts = PriorTimeframe.percepts,
    updated_percepts(CA, State, PriorPercepts, [], SyntheticPercepts),
    append(SyntheticPercepts, ActivationPercepts, Percepts),
    log(info, perceiving, "~w perceived ~p", [CA, Percepts]).

before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDelta) or done(StateDeltas, WellbeingDelta) as last solution. 

% Synthesize one percept at a time, most relevant first.
unit_of_work(CA, State, more(StateDeltas, WellbeingDelta)) :-
    novel_percept(CA, State, Percept),
    StateDeltas = [percepts=[Percept]],
    wellbeing:empty_wellbeing(WellbeingDelta).

% No new percepts can be synthesized
unit_of_work(_, _, done([], WellbeingDelta)) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% For each of the CA's goals, i.e. intent and received directives (received as `planned` or `executed` activation predictions), 
% look up the goal's plan, if any, and
% integrate the planned directives observed activation statuses into a perceptd goal activation status:
%   * all directives are executed -> plan's goal is executed
%   * all directives are failed or not_relevant -> plan's goal is failed
%   * else all directives are commands or they are planned or executed goals -> plan's goal is (inherently or transitively) planned
%   * else goal activation is necessarily relevant since there is a plan for it
%
% Note that a CA *implicitly* percepts as `relevant` a predicted activation about a goal for which it has no plan (yet)
% if the goal/directive targets a percept it currently has. This implied activation percept is expressed as a prediction error
% sent immediately, at any phase of the lifecycle, in response to activation prediction (see ../ca_support.pl).
% This implied percept is not retained by the CA as part of its explicit percepts.
goal_activation_percepts(CA, State, ActivationPercepts) :-
    get_state(State, plans, Plans),
    findall(ActivationPercept, (member(Plan, Plans), activation_percept_from_plan(Plan, CA, State, ActivationPercept)), ActivationPercepts).

activation_percept_from_plan(Plan, CA, State, ActivationPercept) :-
    plan_activation_status(Plan, State, Status),
    goal_id(Plan.goal, GoalId),
    Origin = object{kind:goal, id:GoalId},
    ActivationPercept = percept{origin:Origin, kind:activation, value:Status, confidence:1.0, by:CA}.

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

% Get rid of repeated, updated percepts
updated_percepts(_, _, [], Acc, UpdatedPercepts) :- sort(Acc, UpdatedPercepts).
updated_percepts(CA, State, [PriorPercept | Rest], Acc, UpdatedPercepts) :-
    updated_percept(CA, State, PriorPercept.kind, PriorPercept, UpdatedPercept) ->
        updated_percepts(CA, State, Rest, [UpdatedPercept| Acc], UpdatedPercepts),
        log(info, percept, "~w has updated percept ~p to ~p", [CA, PriorPercept, UpdatedPercept])
        ;
        updated_percepts(CA, State, Rest, Acc, UpdatedPercepts).

% A prior `count` percept is updated by keeping current observations that have survived from the prior percept's evidence/base of support
% then, if not empty, attempting to extend this evidential support from the surviving observations, and, finally, counting the maximal set.
updated_percept(CA, State, count, PriorPercept, UpdatedPercept) :-
    % Update the set of countable objects given the observations supporting the prior percept's origin object
    count_from_prior_object(State, PriorPercept.origin, Count, CountedObservations),
    count_percept(CA, Count, CountedObservations, UpdatedPercept).

% A prior `more` percept is updated by re-comparing the prior observations on their possibly changed values (the same sensor can now read a different value)
updated_percept(CA, State, more, PriorPercept, UpdatedPercept) :-
    updated_observations(State, PriorPercept.origin, [], UpdatedObservations1),
    updated_observations(State, PriorPercept.value, [], UpdatedObservations2),
    member(Observation1, UpdatedObservations1),
    member(Observation2, UpdatedObservations2),
    are_observations_commensurable(Observation1, Observation2),
    more_percept(CA, Observation1, Observation2, UpdatedPercept).

% See if the prior percept is still a trend, albeit reversed or ended.
% Find the prior observation the prior percept trended on
% Find a current observation that would extend or change the trend
% Create an updated trend percept
updated_percept(CA, State, trend, PriorPercept, UpdatedPercept) :-
    prior_timeframe(State, Timeframe),
    member(PriorObservation, Timeframe.observations),
    member(PriorObservation.id, PriorPercept.origin.evidence),
    member(Observation, State.observations),
    comparable_observations(PriorObservation, Observation),
    trend_percept(CA, State, PriorObservation, Observation, UpdatedPercept),
    !.

% Produce a novel percept.
% Non-determinate so it explores a space of new percepts, one unit of work at a time.
% Start with `count` percepts, then `more` and then `trend`.
% For each kind of percept, look at observations with highest priority first.
novel_percept(CA, State, Percept) :-
    member(Kind, [count, more, trend]),
    % Observations are sorted best "informed" first
    get_state(State, observations, Observations),
    predsort(priority, Observations, PrioritizedObservations),
    new_percept(CA, State, Kind, PrioritizedObservations, Percept),
    get_state(State, percepts, Percepts),
    \+ member(Percept, Percepts).

% A novel `count` percept counts at least 2 observations that can be meaningfully grouped (i.e. are countable)
new_percept(CA, _, count, Observations, Percept) :-
    select(Observation, Observations, OtherObservations),
    countable_observations(OtherObservations, [Observation], CountedObservations),
    length(CountedObservations, Count),
    Count > 1,
    count_percept(CA, Count, CountedObservations, Percept).

% A novel `more` percept compares the numerical values of two observations that can be meaningfully compared (i.e. commensurable)
new_percept(CA, _, more, Observations, Percept) :-
    select(Observation1, Observations, OtherObservations),
    member(Observation2, OtherObservations),
    are_observations_commensurable(Observation1, Observation2),
    more_percept(CA, Observation1, Observation2, Percept).

% TODO the front IR distance is more than the back US distance

% A novel trend percept looks at the values of comparable number-valued observations (same origin and kind) from the previous and this timeframe
new_percept(CA, State, trend, _, Percept) :-
    prior_timeframe(State, Timeframe),
    predsort(priority, Timeframe.observations, PriorObservations),
    member(PriorObservation, PriorObservations),
    number(PriorObservation.value),
    get_state(State, observations, Observations),
    member(Observation, Observations),
    comparable_observations(PriorObservation, Observation),
    trend_percept(CA, State, PriorObservation, Observation, Percept).

comparable_observations(Observation1, Observation2) :-
    is_dict(Observation1, observation),
    is_dict(Observation2, observation),
    Observation1.kind == Observation2.kind,
    same_object(Observation1.origin, Observation2.origin).

% Find how many counted observations evidencing a prior count percept can still be counted, together with current observations.
count_from_prior_object(State, Object, Count, CountedObservations) :-
    PriorObservationIds = Object.evidence,
    surviving_observations(State, PriorObservationIds, [], SurvivingObservations),
    % Find maximal set of countable observations starting from surviving observation. Fails if nothing to count.
    countable_observations(State.observations, SurvivingObservations, CountedObservations),
    length(CountedObservations, Count).

% Two observations are commensurable if they are of the same kind and their values are numbers (e.g. two counts or two distance sensor readings) 
are_observations_commensurable(Observation1, Observation2) :-
    observation{kind:Kind, value:Value1} :< Observation1,
    observation{kind:Kind, value:Value2} :< Observation2,
    number(Value1),
    number(Value2).

% Observations from prior timeframe that are still current (have not changed)
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

% Find current observations still current given the IDs of prior observations
updated_observations(_, [], Acc, Acc).

updated_observations(State, [ObservationId | Rest], Acc, CurrentObservations) :-
    updated_observation(State, ObservationId, Observation) ->
        updated_observations(State, Rest, [Observation | Acc], CurrentObservations)
        ;
        updated_observations(State, Rest, Acc, CurrentObservations).

% An observation from a prior timeframe that is still current, about the same object, albeit with a different value
updated_observation(State, ObservationId, Observation) :-
    prior_observation_from_id(State, ObservationId, PriorObservation),
    member(Observation, State.observations),
    observation{origin:Object, kind:Kind} :< PriorObservation,
    observation{origin:Object, kind:Kind} :< Observation.

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
    * Same kind and value - counting objects with same description, or (e.g how many steady trends, how many sensors read a distance of 10)
    * Same origin and kind - counting alternate relations of a kind for one object (e.g. how many ways a given count is more than another)
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

count_percept(CA, Count, CountedObservations, Percept) :-
    simple_count(Count, SimpleCount),
    average_confidence(CountedObservations, Confidence),
    Confidence > 0,
    sorted_ids(CountedObservations, CountedObservationIds),
    synthetic_object(CountedObservationIds, Object),
    Percept = percept{origin:Object, kind:count, value:SimpleCount, confidence:Confidence, by:CA}.

more_percept(CA, Observation1, Observation2, Percept) :-
    observation{id:Id1, value:Value1, confidence:Confidence1} :< Observation1,
    observation{id:Id2, value:Value2, confidence:Confidence2} :< Observation2,
    Value1 \= Value2,
    (Confidence1 * Confidence2) > 0,
    synthetic_object([Id1], Object1),
    synthetic_object([Id2], Object2),
    Confidence is (Confidence1 + Confidence2) / 2,
    (grossly_more_than(Value1, Value2) ->
        Percept = percept{origin:Object1, kind:more, value:Object2, confidence:Confidence, by:CA}
        ;
        Percept = percept{origin:Object2, kind:more, value:Object1, confidence:Confidence, by:CA}
    ).

% A trend goes up or down or else is steady
trend_percept(CA, State, PriorObservation, Observation, Percept) :-
    PriorValue = PriorObservation.value,
    CurrentValue = Observation.value,
    synthetic_object([PriorObservation.id, Observation.id], Object),
    (CurrentValue \= PriorValue ->
        compare_simple_counts(CurrentValue, PriorValue, >) ->
            TrendPercept = percept{origin:Object, kind:trend, value:up, by:CA}
        ;
        compare_simple_counts(CurrentValue, PriorValue, <) ->
            TrendPercept = percept{origin:Object, kind:trend, value:down, by:CA}
        ;
            TrendPercept = percept{origin:Object, kind:trend, value:steady, by:CA}
        ),
    average_confidence([PriorObservation, Observation], CurrentConfidence),
    trend_confidence(State, TrendPercept, CurrentConfidence, Confidence),
    Percept = TrendPercept.put(confidence, Confidence).

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
trend_confidence(State, TrendPercept, CurrentConfidence, Confidence) :-
    previous_trend_percept(State, TrendPercept, PriorTrendPercept),
    !,
    adjust_trend_confidence(PriorTrendPercept, TrendPercept, CurrentConfidence, Confidence).

trend_confidence(_, _, Confidence, Confidence).

previous_trend_percept(State, TrendPercept, PriorTrendPercept) :-
    prior_timeframe(State, Timeframe),
    member(PriorTrendPercept, Timeframe.percepts),
    percept{origin:Origin, kind:trend} :< PriorTrendPercept,
    % The origin of a prior trend percept shares exactly one supporting observation.
    % Because the evidence of a trend consists of an observation from the timeframe and one from the timeframe before.
    intersection(Origin.evidence, TrendPercept.origin.evidence, [_]).

% Confidence in the current trend percept grows if it maintains a percept from the prior timeframe.
% It is extended by up to half the confidence of the persisted trend percept from the previous timeframe.
adjust_trend_confidence(PriorTrendPercept, TrendPercept, CurrentConfidence, Confidence) :-
    (PriorTrendPercept.value == TrendPercept.value ->
        Confidence is min(1.0, CurrentConfidence + (PriorTrendPercept.confidence / 2))
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
