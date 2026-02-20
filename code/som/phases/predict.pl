/*
Make predictions about future observations.

The CA collects all the predictions it could make and drops those that fall too far below the average confidence of the lot (e.g. below half average).

The sources of predictions, in decreasing order of priority, are:

* Inferring predictions by applying the causal theory to prior observations
* Extrapolating from prior experiences
* Predicting unchanged prior observations
* Making an empty prediction (only if no other prediction can be made)

When uncontested predictions conflict (different values are predicted), the one from the highest priority source will be made.
When conflicting predictions have equal priority, the one with highest confidence is the one that will be made.

The CA collects all the predictions it could make and drops the "minor" ones, those that fall too far below the average confidence of the lot (e.g. below half average).

prediction{origin:Origin, kind:Kind, value:Value, priority:Priority, confidence:Confidence, by: CA}
*/

:- module(predict, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/ca_support)).

% No work done before units of work
before_work(_, _, [], WellbeingDeltas) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).

% unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 

% Make predictions and send them to the umwelt.
% This is always a single unit of work. Where there's indeterminacy, random choices are made.
unit_of_work(CA, State, done(StateDeltas, WellbeingDeltas)) :-
    log(info, predict, "Predicting for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]),
    potential_predictions(CA, State, AllPredictions),
    resolve_conflicting_predictions(AllPredictions, [], Predictions1),
    major_predictions(Predictions1, Predictions),
    log(info, predict, "~w made major predictions ~p", [CA, Predictions]),
    predictions_sent_to_umwelt(CA, State.umwelt, Predictions),
    StateDeltas = [predictions_out=Predictions],
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase predict done for CA ~w", [CA]).

% Find all potential predictions
potential_predictions(CA, State, Predictions) :-
    predictions_from_causal_theory(CA, State, Predictions1),
    predictions_from_experiences(CA, State, Predictions2),
    % log(info, predict, "@@@ Predictions from experience ~p", [Predictions2]),
    predictions_from_observations(CA, State, Predictions3),
    log(info, predict, "@@@ Predictions from observations ~p", [Predictions3]),
    flatten([Predictions1, Predictions2, Predictions3], AllPredictions),
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
    observations_as_predictions(CA, NextObservations, 3, Predictions).

% TODO
apply_causal_theory(_, _, _, []).

% Make predictions from prior experiences (priority 2)
predictions_from_experiences(CA, State, Predictions) :-
    get_state(State, experiences, Experiences),
    findall(PredictionsFromExperience, 
        (member(Experience, Experiences), predictions_from_experience(CA, State, Experience, PredictionsFromExperience)), 
        Predictions1),
    flatten(Predictions1, Predictions).

% Predictions given a prior count or more experience
predictions_from_experience(CA, State, Experience, Predictions) :-
    member(Experience.kind, [count, more]),
    object{support: ObservationIds} :< Experience.origin,
    observations_with_ids(State, ObservationIds, Observations),
    observations_as_predictions(CA, Observations, 2, Predictions),
    log(info, predict, "@@@ Predictions ~p from ~w experience ~p", [Predictions, Experience.kind, Experience]).

% Predictions given a prior trend experience
predictions_from_experience(CA, State, Experience, [Prediction]) :-
    Experience.kind == trend,
    expected_trending_observation(State, Experience, Observation),
    observation_as_prediction(CA, Observation, 2, Prediction),
    log(info, predict, "@@@ Prediction ~p from trending experience ~p", [Prediction, Experience]).

expected_trending_observation(State, Experience, Observation) :-
    member(Experience.value, [up_stopped, down_stopped]),
    % The second support observation is the latest
    object{support:[_,ObservationId]} :< Experience,
    observation_with_id(State, ObservationId, Observation).

expected_trending_observation(State, Experience, ExpectedObservation) :-
    member(Experience.value, [up, down]),
    object{support:[PriorObservationId, ObservationId]} :< Experience,
    observation_with_id(State, ObservationId, Observation),
    observation_with_id(State, PriorObservationId, PriorObservation),
    Delta is Observation.value - PriorObservation.value,
    ExpectedValue is Observation.value + Delta,
    ExpectedObservation = Observation.put(value, ExpectedValue).

% Predict prior observations (priority 1)
predictions_from_observations(CA, State, Predictions) :-
    get_state(State, observations, Observations),
    observations_as_predictions(CA, Observations, 1, Predictions).

% Translate expected observations into predictions.
observations_as_predictions(CA, Observations, Priority, Predictions) :-
    findall(Prediction, (member(Observation, Observations), observation_as_prediction(CA, Observation, Priority, Prediction)), Predictions).
        
% Turn an observation into a prediction
observation_as_prediction(CA, Observation, Priority, Prediction) :-
    observation{origin:Origin, kind:Kind, value:Value, confidence:Confidence} :< Observation,
    Prediction = prediction{origin:Origin, kind:Kind, value:Value, confidence:Confidence, priority:Priority, by: CA}.

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

% Resolve conflicting predictions.
% Two predictions conflict if they have the same name and are about the same object
% The one with greater priority and then confidence wins, else pick one randomly.
resolve_conflicting_predictions([], Predictions, Predictions).
resolve_conflicting_predictions([Prediction | Rest], Acc, ResolvedPredictions) :-
    findall(OtherPrediction, (member(OtherPrediction, Rest), conflicting_predictions(Prediction, OtherPrediction)), ConflictingPredictions),
    resolve_conflicts([Prediction | ConflictingPredictions], Resolution),
    subtract(Rest, ConflictingPredictions, Others),
    resolve_conflicting_predictions(Others, [Resolution | Acc], ResolvedPredictions).

resolve_conflicts([Prediction], Prediction).

% Select randomly a prediction with the highest priority else confidence
resolve_conflicts(ConflictingPredictions, Prediction) :-
    random_permutation(ConflictingPredictions, Permutation),
    top_prediction(Permutation, Prediction).

% Predictions conflict if they have the same name (the name of the predicted experience)
% and object (what the prediction is about e.g. color, distance, luminance, count, more, trend)
conflicting_predictions(Prediction, OtherPrediction) :-
    prediction{origin:Origin, kind:Kind} :< Prediction,
    prediction{origin:Origin, kind:Kind} :< OtherPrediction.

top_prediction(ConflictingPredictions, TopPrediction) :-
   max_priority(ConflictingPredictions, MaxPriority),
   findall(Prediction, (member(Prediction, ConflictingPredictions), Prediction.priority == MaxPriority), PriorityPredictions),
   sort(confidence, @>, PriorityPredictions, [TopPrediction | _]).

max_priority(ConflictingPredictions, MaxPriority) :-
    setof(Priority, (member(Prediction, ConflictingPredictions), Priority = Prediction.priority), Priorities),
    max_list(Priorities, MaxPriority).

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
