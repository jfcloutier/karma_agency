/*
Make predictions about future observations.

The CA collects all the predictions it could make and drops those that fall too far below the average confidence of the lot (e.g. below half average).

The sources of predictions, with their associated weights, are:

* Predicting goal activation statuses, weight = 4
* Inferring predictions by applying the causal theory to prior observations = weight = 3
* Predicting from maintaining prior experiences, weight = 2
* Predicting repeated prior observations, weight = 1
* Making an empty prediction (only if no other prediction can be made), weight = 0

When uncontested predictions conflict (different values are predicted), the one from the highest weight can be made.
When conflicting predictions have equal weight, the one with highest confidence can be made.

The CA collects all the predictions it can make and drops the "minor" ones, those that fall too far below the average confidence of the lot (e.g. below half average).
This is an attention mechanism.

prediction{origin:Origin, kind:Kind, value:Value, weight:Weight, confidence:Confidence, by: CA, when_received: TimeframeIndex}
- `when_received` is applied only to predictions received and thus listed in `predictions_in`
*/

:- module(predict, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/plans)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/goals)).
:- use_module(agency(som/ca_support)).

before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% Make all constructed and selected predictions and send them to the umwelt at once.
% This is always a single unit of work. Where there's indeterminacy, random choices are made.
% Wellbeing: Making prediction increases engagement, otherwise it has no energy cost and is not damaging.
unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
    potential_predictions(CA, State, PotentialPredictions),
    deconflicted_predictions(PotentialPredictions, [], DeconflictedPredictions),
    major_predictions(DeconflictedPredictions, Predictions),
    predictions_sent_to_umwelt(CA, State.umwelt, Predictions),
    log(info, predict, "~w sent major predictions ~p", [CA, Predictions]),
    StateDeltas = [predictions_out=Predictions],
    wellbeing_delta(Predictions, WellbeingDelta),
    log(info, predict, "Phase predict done for CA ~w", [CA]).

% Find all potential predictions
potential_predictions(CA, State, Predictions) :-
    predictions_from_causal_theory(CA, State, Predictions1),
    predictions_from_experiences(CA, State, Predictions2),
    % log(info, predict, "@@@ Predictions from experience ~p", [Predictions2]),
    predictions_from_prior_observations(CA, State, Predictions3),
    % log(info, predict, "@@@ Predictions from observations ~p", [Predictions3]),
    predictions_from_plans(CA, State, Predictions4),
    %log(info, predict, "@@@ Predictions from plans ~p", [Predictions4]),
    flatten([Predictions1, Predictions2, Predictions3, Predictions4], AllPredictions),
    % If no grounded predictions can be made, make an empty prediction from complete ignorance 
    % to trigger a prediction error from each experience in the umwelt
    (length(AllPredictions, 0) ->
        empty_prediction(CA, EmptyPrediction),
        Predictions = [EmptyPrediction],
        log(info, predict, "@@@ Predictions from ignorance ~p", [Predictions])
        ;
        Predictions = AllPredictions
    ),
    log(info, predict, "~w could predict ~p", [CA, Predictions]).

% Apply causal theory to prior observations to infer expected observations (priority 3))
predictions_from_causal_theory(CA, State, Predictions) :-
    get_state(State, causal_theory, CausalTheory),
    get_state(State, observations, Observations),
    apply_causal_theory(CA, CausalTheory, Observations, NextObservations),
    Weight = 3, 
    observations_as_predictions(CA, NextObservations, Weight, Predictions).

% TODO
apply_causal_theory(_, _, _, []).

% Make predictions from prior experiences (weight is 2)
predictions_from_experiences(CA, State, Predictions) :-
    get_state(State, experiences, Experiences),
    findall(PredictionsFromExperience, 
        (member(Experience, Experiences), predictions_from_experience(CA, State, Experience, PredictionsFromExperience)), 
        Predictions1),
    flatten(Predictions1, Predictions).

% Predictions given a prior count or more experience
% Predict that the evidence is unchanged
predictions_from_experience(CA, State, Experience, Predictions) :-
    member(Experience.kind, [count, more]),
    object{evidence: ObservationIds} :< Experience.origin,
    observations_with_ids(State, ObservationIds, Observations),
    Weight = 2,
    observations_as_predictions(CA, Observations, Weight, Predictions),
    log(info, predict, "@@@ Predictions ~p from ~w experience ~p", [Predictions, Experience.kind, Experience]).

% Predictions given a prior trend experience
predictions_from_experience(CA, State, Experience, [Prediction]) :-
    Experience.kind == trend,
    expected_trending_observation(State, Experience, Observation),
    observation_as_prediction(CA, Observation, 2, Prediction),
    log(info, predict, "@@@ Prediction ~p from trending experience ~p", [Prediction, Experience]).

% No prediction can be made from an activation experience
predictions_from_experience(_, _, Experience, []) :-
    Experience.kind == activation.

% A steady trend means expecting the latest observation to be maintained
expected_trending_observation(State, Experience, Observation) :-
    steady == Experience.value,
    % The second evidence observation is the latest
    object{evidence:[_,ObservationId]} :< Experience.origin,
    observation_with_id(State, ObservationId, Observation).

% The observed up or down change in value is expected to continue
expected_trending_observation(State, Experience, ExpectedObservation) :-
    member(Experience.value, [up, down]),
    object{evidence:[PriorObservationId, ObservationId]} :< Experience.origin,
    observation_with_id(State, ObservationId, Observation),
    observation_with_id(State, PriorObservationId, PriorObservation),
    Delta is Observation.value - PriorObservation.value,
    (Experience.value == up -> 
        TrendedValue is Observation.value + Delta
        ;
        TrendedValue is Observation.value - Delta),
    simple_count(TrendedValue, ExpectedValue),
    ExpectedObservation = Observation.put(value, ExpectedValue).

% Predict prior observations (priority 1)
predictions_from_prior_observations(CA, State, Predictions) :-
    get_state(State, observations, Observations),
    Weight = 1,
    observations_as_predictions(CA, Observations, Weight, Predictions).

% Translate expected observations into predictions.
observations_as_predictions(CA, Observations, Weight, Predictions) :-
    findall(Prediction, (member(Observation, Observations), observation_as_prediction(CA, Observation, Weight, Prediction)), Predictions).
        
% Turn an observation into a prediction, unless it is an activation observation (those are made exclusively from plans)
observation_as_prediction(CA, Observation, Weight, Prediction) :-
    observation{origin:Origin, kind:Kind, value:Value, confidence:Confidence} :< Observation,
    Kind \= activation,
    Prediction = prediction{origin:Origin, kind:Kind, value:Value, confidence:Confidence, weight:Weight, by: CA}.

% Look for observations given IDs. Look at current ones and then in prior timeframes, more recent first.
observations_with_ids(_, [], []).

observations_with_ids(State, [Id | OtherIds], Observations) :-
    observation_with_id(State, Id, Observation) ->
        observations_with_ids(State, OtherIds, OtherObservations),
        Observations = [Observation | OtherObservations]
        ;
        observations_with_ids(State, OtherIds, Observations).

observation_with_id(State, Id, Observation) :-
    get_state(State, observations, Observations),
    member(Observation, Observations),
    Id == Observation.id, !.

observation_with_id(State, Id, Observation) :-
    member(Timeframe, State.timeframes),
    member(Observation, Timeframe.observations),
    Id == Observation.id, !.

predictions_from_plans(CA, State, Predictions) :-
    get_state(State, plans, Plans),
    findall(ActivePlan, (member(Plan, Plans), is_plan_active(Plan, State)), ActivePlans),
    findall(ActivationPredictions, (member(ActivePlan, ActivePlans), activation_predictions(ActivePlan, CA, State, ActivationPredictions)), Predictions).

% If the goal of the plan is predicted as executed and is experienced as (at least) planned, predict all of its directives as executed.
activation_predictions(Plan, CA, State, ActivationPredictions) :-
    Goal = Plan.goal, 
    predicted_activation_status(Goal, State, executed),
    experienced_activation_status(Plan.goal, State, planned),
    !,
    plan_directive_predictions(Plan, executed, CA, ActivationPredictions).

% If the goal of the plan is predicted as planned and is experienced as (at least) relevant, predict all of its directives as (at least) planned.
activation_predictions(Plan, CA, State, ActivationPredictions) :-
    Goal = Plan.goal, 
    predicted_activation_status(Goal, State, planned),
    experienced_activation_status(Plan.goal, State, relevant),
    !,
    plan_directive_predictions(Plan, planned, CA, ActivationPredictions).

% Predict all of its directives as (at least) relevant to show continued interest in the plan.
activation_predictions(Plan, CA, _, ActivationPredictions) :-
    plan_directive_predictions(Plan, relevant, CA, ActivationPredictions).

plan_directive_predictions(Plan, Status, CA, ActivationPredictions) :-
    findall(ActivationPrediction,
            (member(Directive, Plan.directives), prediction_of_activation(Directive, CA, Status, ActivationPrediction)),
            ActivationPredictions).

prediction_of_activation(Directive, CA, Status, Prediction) :-
    goal_id(Directive, GoalId),
    Origin = object{type: goal, id: GoalId},
    Prediction = prediction{origin:Origin, kind:activation, value:Status, confidence:1.0, weight:4, by: CA}.

% A prediction was received about the status for the activation of the goal.
% The prediction is true if the current status is at least the one predicted.
predicted_activation_status(Goal, State, PredictedStatus) :-
    get_state(State, predictions_in, PredictionsIn),
    member(PredictionIn, PredictionsIn),
    prediction{origin:Origin, kind:activation, value:CurrentStatus} :< PredictionIn,
    goal_id(Goal, GoalId),
    object{type:goal, id:GoalId} :< Origin,
    compare_activation_status(Order, CurrentStatus, PredictedStatus),
    memberchk(Order, [=, >]).

% The experienced activation of the CA's goal (intent or received directive) is at the given status or further toward executed
experienced_activation_status(Goal, State, Status) :-
    goal_id(Goal, GoalId),
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{origin:Origin, kind:activation, value:CurrentStatus} :< Experience,
    object{type:goal, id:GoalId} :< Origin,
    compare_activation_status(Order, CurrentStatus, Status),
    memberchk(Order, [=, >]).

% Resolve conflicting predictions.
% Two predictions conflict if they have the same name and are about the same object
% The one with greater priority and then confidence wins, else pick one randomly.
deconflicted_predictions([], Predictions, Predictions).
deconflicted_predictions([Prediction | Rest], Acc, ResolvedPredictions) :-
    findall(OtherPrediction, (member(OtherPrediction, Rest), conflicting_predictions(Prediction, OtherPrediction)), ConflictingPredictions),
    resolve_conflicts([Prediction | ConflictingPredictions], Resolution),
    subtract(Rest, ConflictingPredictions, Others),
    deconflicted_predictions(Others, [Resolution | Acc], ResolvedPredictions).

resolve_conflicts([Prediction], Prediction).

% Select randomly a prediction with the highest weight else confidence
resolve_conflicts(ConflictingPredictions, Prediction) :-
    random_permutation(ConflictingPredictions, Permutation),
    top_prediction(Permutation, Prediction).

% Predictions conflict if they have the same name (the name of the predicted experience)
% and object (what the prediction is about e.g. color, distance, luminance, count, more, unchanged, trend)
conflicting_predictions(Prediction, OtherPrediction) :-
    prediction{origin:Origin, kind:Kind} :< Prediction,
    prediction{origin:Origin, kind:Kind} :< OtherPrediction.

top_prediction(ConflictingPredictions, TopPrediction) :-
   max_weight(ConflictingPredictions, MaxWeight),
   findall(Prediction, (member(Prediction, ConflictingPredictions), Prediction.weight == MaxWeight), WeightedPredictions),
   sort(confidence, @>, WeightedPredictions, [TopPrediction | _]).

max_weight(ConflictingPredictions, MaxWeight) :-
    setof(Weight, (member(Prediction, ConflictingPredictions), Weight = Prediction.weight), Weights),
    max_list(Weights, MaxWeight).

% Drop predictions with significantly below-average confidence (less than half the average confidence)
major_predictions(Predictions, MajorPredictions) :-
    findall(Confidence, (member(Prediction, Predictions), Confidence = Prediction.confidence), Confidences),
    sum_list(Confidences, Sum),
    length(Confidences, N),
    Average is Sum / N,
    Cutoff is Average / 2,
    findall(Prediction, (member(Prediction, Predictions), Prediction.confidence >= Cutoff), MajorPredictions).

predictions_sent_to_umwelt(_, _, []).

predictions_sent_to_umwelt(CA, Umwelt, [Prediction | Rest]) :-
    forall(member(UmweltCA, Umwelt), prediction_sent_to_umwelt_ca(Prediction, UmweltCA, CA)),
    predictions_sent_to_umwelt(CA, Umwelt, Rest).

prediction_sent_to_umwelt_ca(Prediction, UmweltCA, CA) :-
    message_sent(UmweltCA, prediction(Prediction), CA),
    log(info, predict, "~w sent prediction ~p to ~w", [CA, Prediction, UmweltCA]).

wellbeing_delta(Predictions, WellbeingDelta) :-
    wellbeing:empty_wellbeing(EmptyWellbeing),
    length(Predictions, N),
    % TODO - Get the engagement value of a prediction made from settings
    Delta is N * 0.01,
    WellbeingDelta = EmptyWellbeing.add_engagement(Delta).


