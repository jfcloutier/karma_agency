/*
Make predictions about future observations.

The CA collects all the predictions it could make and drops those that fall too far below the average confidence of the lot (e.g. below half average).

The sources of predictions, with their associated weights, are:

* Predicting goal activation statuses, weight = 4
* Inferring predictions by applying the causal theory to prior observations = weight = 3
* Predicting from maintaining prior percepts, weight = 2
* Predicting repeated prior observations, weight = 1
* Making an empty prediction (only if no other prediction can be made), weight = 0

When uncontested predictions conflict (different values are predicted), the one from the highest weight can be made.
When conflicting predictions have equal weight, the one with highest confidence can be made.

The CA collects all the predictions it can make and drops the "minor" ones, those that fall too far below the average confidence of the lot (e.g. below half average).
This is an attention mechanism.

prediction{origin:Origin, kind:Kind, value:Value, weight:Weight, confidence:Confidence, by: CA, when_received: Age}
- `when_received` is applied only to predictions received and thus listed in `predictions_in`
*/

:- module(predicting, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/plans)).
:- use_module(agency(som/goals)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/predictions)).
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
    get_state(State, umwelt, Umwelt),
    predictions_sent_to_umwelt(CA, Umwelt, Predictions),
    log(info, predicting, "~w sent major predictions ~p", [CA, Predictions]),
    StateDeltas = [predictions_out=Predictions],
    wellbeing_delta(Predictions, WellbeingDelta).

% Find all potential predictions
potential_predictions(CA, State, Predictions) :-
    predictions_from_causal_theory(CA, State, Predictions1),
    predictions_from_percepts(CA, State, Predictions2),
    predictions_from_prior_observations(CA, State, Predictions3),
    predictions_from_plans(CA, State, Predictions4),
    flatten([Predictions1, Predictions2, Predictions3, Predictions4], AllPredictions),
    % If no grounded predictions can be made, make an empty prediction from complete ignorance 
    % to trigger a prediction error from each percept in the umwelt
    (length(AllPredictions, 0) ->
        empty_prediction(CA, EmptyPrediction),
        Predictions = [EmptyPrediction],
        log(info, predicting, "~w can only make predictions from ignorance", [CA])
        ;
        Predictions = AllPredictions
    ),
    log(info, predicting, "~w could predict ~p", [CA, Predictions]).

% Apply causal theory to prior observations to infer expected observations (priority 3))
predictions_from_causal_theory(CA, State, Predictions) :-
    get_state(State, causal_theory, CausalTheory),
    get_state(State, observations, Observations),
    apply_causal_theory(CA, CausalTheory, Observations, NextObservations),
    Weight = 3, 
    observations_as_predictions(CA, NextObservations, Weight, Predictions),
    log(info, predicting, "~w predicts from causal theory ~p", [CA, Predictions]).

% TODO
apply_causal_theory(_, _, _, []).

% Make predictions from prior percepts (weight is 2)
predictions_from_percepts(CA, State, Predictions) :-
    get_state(State, percepts, Percepts),
    findall(PredictionsFromPercept, 
        (member(Percept, Percepts), predictions_from_percept(CA, State, Percept, PredictionsFromPercept)), 
        Predictions1),
    flatten(Predictions1, Predictions),
    log(info, predicting, "~w makes from prior percepts ~p  predictions ~p", [CA, Percepts, Predictions]).

% Predictions given a prior count or more percept
% Predict that the evidence is the same
predictions_from_percept(CA, State, Percept, Predictions) :-
    member(Percept.kind, [count, more]),
    object{evidence: ObservationIds} :< Percept.origin,
    observations_with_ids(State, ObservationIds, Observations),
    Weight = 2,
    observations_as_predictions(CA, Observations, Weight, Predictions),
    log(info, predicting, "(~w) Predictions ~p from prior ~w percept ~p", [CA, Predictions, Percept.kind, Percept]).

% Predictions given a prior trend percept
predictions_from_percept(CA, State, Percept, [Prediction]) :-
    Percept.kind == trend,
    expected_trending_observation(State, Percept, Observation),
    observation_as_prediction(CA, Observation, 2, Prediction),
    log(info, predicting, "(~w) Prediction ~p from trend percept ~p", [CA, Prediction, Percept]).

% No prediction can be made from an activation percept
predictions_from_percept(_, _, Percept, []) :-
    Percept.kind == activation.

% A steady trend means expecting the latest observation to be maintained
expected_trending_observation(State, Percept, Observation) :-
    steady == Percept.value,
    % The second evidence observation is the latest
    object{evidence:[_,ObservationId]} :< Percept.origin,
    observation_with_id(State, ObservationId, Observation).

% The observed up or down change in value is expected to continue
expected_trending_observation(State, Percept, ExpectedObservation) :-
    member(Percept.value, [up, down]),
    object{evidence:[PriorObservationId, ObservationId]} :< Percept.origin,
    observation_with_id(State, ObservationId, Observation),
    observation_with_id(State, PriorObservationId, PriorObservation),
    Delta is Observation.value - PriorObservation.value,
    (Percept.value == up -> 
        TrendedValue is Observation.value + Delta
        ;
        TrendedValue is Observation.value - Delta),
    simple_count(TrendedValue, ExpectedValue),
    ExpectedObservation = Observation.put(value, ExpectedValue).

% Predict prior observations (priority 1)
predictions_from_prior_observations(CA, State, Predictions) :-
    get_state(State, observations, Observations),
    Weight = 1,
    observations_as_predictions(CA, Observations, Weight, Predictions),
    log(info, predicting, "~w makes from prior observations ~p  predictions ~p", [CA, Observations, Predictions]).

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
    findall(ActivationPredictions, (member(ActivePlan, ActivePlans), plan_activation_predictions(ActivePlan, CA, State, ActivationPredictions)), Predictions),
    log(info, predicting, "~w makes predictions ~p from plans ~p", [CA, Predictions, Plans]).

% If the goal of the plan is predicted as executed and is perceived as (at least) planned, predict all of its directives as executed.
plan_activation_predictions(Plan, CA, State, ActivationPredictions) :-
    Goal = Plan.goal, 
    predicted_activation_status(Goal, State, executed),
    perceived_activation_status(Plan.goal, State, planned),
    !,
    plan_directive_predictions(Plan, executed, CA, ActivationPredictions),
    log(info, predicting, "(~w) predictions ~p about plan ~p given planned its goal ~p predicted as executed", [CA, ActivationPredictions, Plan, Goal]).

% If the goal of the plan is predicted as planned and is perceived as (at least) relevant, predict all of its directives as (at least) planned.
plan_activation_predictions(Plan, CA, State, ActivationPredictions) :-
    Goal = Plan.goal, 
    predicted_activation_status(Goal, State, planned),
    perceived_activation_status(Plan.goal, State, relevant),
    !,
    plan_directive_predictions(Plan, planned, CA, ActivationPredictions),
    log(info, predicting, "(~w) predictions ~p about plan ~p given relevant its goal ~p predicted as planned", [CA, ActivationPredictions, Plan, Goal]).

% Predict all of its directives as (at least) relevant to show continued interest in the plan.
plan_activation_predictions(Plan, CA, _, ActivationPredictions) :-
    plan_directive_predictions(Plan, relevant, CA, ActivationPredictions),
    log(info, predicting, "(~w) predictions ~p about plan ~p assuming its goal is still relevant", [CA, ActivationPredictions, Plan]).

plan_directive_predictions(Plan, Status, CA, ActivationPredictions) :-
    findall(ActivationPrediction,
            (member(Directive, Plan.directives), prediction_of_activation(Directive, CA, Status, ActivationPrediction)),
            ActivationPredictions).

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

% The perceived activation of the CA's goal (intent or received directive) is at the given status or further toward executed
perceived_activation_status(Goal, State, Status) :-
    goal_id(Goal, GoalId),
    get_state(State, percepts, Percepts),
    member(Percept, Percepts),
    percept{origin:Origin, kind:activation, value:CurrentStatus} :< Percept,
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

% Predictions conflict if they have the same name (the name of the predicted percept)
% and object (what the prediction is about e.g. color, distance, luminance, count, more, trend)
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

wellbeing_delta(Predictions, WellbeingDelta) :-
    wellbeing:empty_wellbeing(EmptyWellbeing),
    length(Predictions, N),
    % TODO - Get the engagement value of a prediction made from settings
    Delta is N * 0.01,
    WellbeingDelta = EmptyWellbeing.add_engagement(Delta).


