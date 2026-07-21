/*
* Merge correct predictions and prediction errors into new observations.
*
* Activation observations are produced somewhat differently from other kinds of observations.
* Activation observations look for unversals vs existentials (Did all predicted activations of this directive fail? Did at least one execute?)
* All other types of observations look for highest confidence (What is the most certain correction, if any, to this predicted umwelt experience?)
*/

:- module(observe, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/ca_support)).

% No work done before units of work
before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% observation{origin:object{type:Type, id:ID}, kind:Kind, value:Value, confidence:Confidence, by:CA, id:Id}
% The object in an observation omits its "evidence set" (what was integrated in its synthesis as part of an umwelt experience) 
% The value observed may not be that experienced by each of the umwelt CAs under observation, just the one with highest confidence
unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
    observed_from_predictions(CA, State, Observations),
    StateDeltas = [observations=Observations],
    wellbeing_delta(Observations, WellbeingDelta),
    log(info, observe, "Phase observe done for CA ~w with wellbeing delta ~p", [CA, WellbeingDelta]).

observed_from_predictions(CA, State, Observations) :-
    aggregate_prediction_errors(State, Pairs),
    umwelt_size(State, UmweltSize),
    correct_predictions(Pairs, UmweltSize, CorrectPredictions),
    effective_prediction_errors(Pairs, CorrectPredictions, CA, State, EffectivePredictionErrors),
    predictions_to_observations(CorrectPredictions, CA, Observations1),
    prediction_errors_to_observations(EffectivePredictionErrors, CA, Observations2),
    append(Observations1, Observations2, Observations),
    log(debug, observe, "(~w) Observed ~p", [CA, Observations]).

umwelt_size(State, Size) :-
    get_state(State, umwelt, Umwelt),
    length(Umwelt, Size).

% Pair up predictions and their associated prediction errors if any
% Ignore an empty prediction, using predictions in prediction errors instead to replace it
aggregate_prediction_errors(State, Pairs) :-
    get_state(State, prediction_errors, PredictionErrors),
    effective_predictions(State, EffectivePredictions),
    pair_up_predictions_with_errors(EffectivePredictions, PredictionErrors, [], Pairs).

% Find all non-empty predictions made,
% plus all the implied predictions made via empty predictions (from examinimg the consequent prediction errors)
effective_predictions(State, EffectivePredictions) :-
    get_state(State, predictions_out, Predictions),
    get_state(State, prediction_errors, PredictionErrors),
    findall(Prediction, (member(Prediction, Predictions), \+ is_empty_prediction(Prediction)), FullPredictions),
    % Only empty predictions have `unknown` as values, even after they are "filled" by consequent prediction errors
    findall(ImpliedPrediction, 
        (member(PredictionError, PredictionErrors), ImpliedPrediction = PredictionError.prediction, ImpliedPrediction.value == unknown), 
        ImpliedPredictions),
    append(FullPredictions, ImpliedPredictions, EffectivePredictions1),
    % Remove duplicates
    sort(EffectivePredictions1, EffectivePredictions).

% For each effective prediction made, pair it with all the consequent prediction errors received
pair_up_predictions_with_errors([], _, Pairs, Pairs).

pair_up_predictions_with_errors([Prediction | Rest], PredictionErrors, Acc, Pairs) :-
    findall(PredictionError,
        (member(PredictionError, PredictionErrors), Prediction = PredictionError.prediction),
    RelatedPredictionErrors),
    pair_up_predictions_with_errors(Rest, PredictionErrors, [Prediction - RelatedPredictionErrors | Acc], Pairs).

correct_predictions(Pairs, UmweltSize, CorrectPredictions) :-
    findall(CorrectPrediction, 
        (member(Prediction-PredictionErrors, Pairs), correct_prediction(Prediction, PredictionErrors, UmweltSize, CorrectPrediction)), 
        CorrectPredictions).

% A prediction is correct if 
%   it is not an empty prediction (value is `unknown`)
%   it did cause prediction errors
%   it did NOT receive unknown-valued error from its entire umwelt (it is relevant to at least one member of its umwelt)
%   it received only valued prediction errors of lower confidence (confidence in the prediction is however reduced by the most confident prediction error)
correct_prediction(Prediction, _, _, _) :-
    Prediction.value == unknown,
    !,
    fail.

% The prediction is correct if it caused no prediction error - the prediction was meaningful to all and correct
correct_prediction(Prediction, [], _, Prediction).

% A prediction is not correct if it is meaningless to the entire umwelt (i.e. it is "not even wrong")
correct_prediction(_, PredictionErrors, UmweltSize, _) :-
    findall(PredictionError, (member(PredictionError, PredictionErrors), PredictionError.actual_value == unknown), Unknowns),
    length(Unknowns, UmweltSize),
    !,
    fail.

% If at least one valued prediction error received
% the prediction was correct if the prediction is more confident than the most confident, valued prediction error,
% but the prediction's confidence is reduced by the confidence of that prediction error.
correct_prediction(Prediction, PredictionErrors, _, CorrectPrediction) :-
    findall(PredictionError, (member(PredictionError, PredictionErrors), PredictionError.actual_value \= unknown), ValuedPredictionErrors),
    (length(ValuedPredictionErrors, 0) ->
        CorrectPrediction = Prediction
        ;
        sort(confidence, @>, ValuedPredictionErrors, [TopPredictionError | _]),
        Prediction.confidence > TopPredictionError.confidence,
        ReducedPConfidence is Prediction.confidence - TopPredictionError.confidence,
        CorrectPrediction = Prediction.put(confidence, ReducedPConfidence)
    ).

% A prediction error is effective if it is valued 
% and contests an incorrect prediction with the highest confidence among competing prediction errors.
effective_prediction_errors(Pairs, CorrectPredictions, CA, State, EffectivePredictionErrors) :-
    effective_prediction_errors(Pairs, CorrectPredictions, CA, State, [], EffectivePredictionErrors).

effective_prediction_errors(_, _, [], _, EffectivePredictionErrors, EffectivePredictionErrors).

effective_prediction_errors([Prediction-PredictionErrors | Rest], CorrectPredictions, CA, State, Acc, EffectivePredictionErrors) :-
    \+ member(Prediction, CorrectPredictions),
    effective_prediction_error(Prediction, PredictionErrors, CA, State, EffectivePredictionError),
    !,
    effective_prediction_errors(Rest, CorrectPredictions, CA, State, [EffectivePredictionError | Acc], EffectivePredictionErrors).

effective_prediction_errors([_ | Rest], CorrectPredictions, CA, State, Acc, EffectivePredictionErrors) :-
    effective_prediction_errors(Rest, CorrectPredictions, CA, State, Acc, EffectivePredictionErrors).

% The CA integrates prediction errors about an activation prediction into an effective prediction error from itself
effective_prediction_error(Prediction, PredictionErrors, CA, State, EffectivePredictionError) :-
    activation = Prediction.kind,
    !,
    get_state(State, umselt, Umwelt),
    actual_activation_status(PredictionErrors, Umwelt, ActivationStatus),
    log(info, observe, "Actual activation status for ~p by ~w is w", [Prediction, CA, ActivationStatus]),
    EffectivePredictionError = prediction_error{prediction: Prediction, actual_value: ActivationStatus, confidence: 1.0, by: CA}.

% Effective prediction error for a non-activation prediction
effective_prediction_error(_, PredictionErrors, _, _, EffectivePredictionError) :-
    findall(ValuedPredictionError, (member(ValuedPredictionError, PredictionErrors), ValuedPredictionError.actual_value \= unknown), ValuedPredictionErrors),
    sort(confidence, @>, ValuedPredictionErrors, [EffectivePredictionError | _]).

actual_activation_status(PredictionErrors, Umwelt, failed) :-
    forall(member(UmweltCA, Umwelt), is_corrected_status_in(UmweltCA, PredictionErrors, [failed, not_relevant])), !.

actual_activation_status(PredictionErrors, Umwelt, executed) :-
    forall(member(UmweltCA, Umwelt), is_corrected_status_in(UmweltCA, PredictionErrors, [executed])), !.

actual_activation_status(PredictionErrors, Umwelt, planned) :-
    member(UmweltCA, Umwelt), is_corrected_status_in(UmweltCA, PredictionErrors, [planned, executed]), !.

actual_activation_status(PredictionErrors, Umwelt, relevant) :-
    member(UmweltCA, Umwelt), is_corrected_status_in(UmweltCA, PredictionErrors, [relevant, planned, executed]), !.

is_corrected_status_in(UmweltCA, PredictionErrors, AlternativeStatuses) :-
    findall(PredictionErrorFromUmweltCA, (member(PredictionErrorFromUmweltCA, PredictionErrors), PredictionError.by == UmweltCA), ApplicablePredictionErrors),
    [_ | _] = ApplicablePredictionErrors,
    forall(member(PredictionError, ApplicablePredictionErrors), memberchk(PredictionError.actual_value, AlternativeStatuses)).

predictions_to_observations(Predictions, CA, Observations) :-
    findall(Observation, 
        (member(Prediction, Predictions), prediction_to_observation(Prediction, CA, Observation)), 
        Observations).

prediction_errors_to_observations(PredictionErrors, CA, Observations) :-
    findall(Observation, 
        (member(PredictionError, PredictionErrors), prediction_error_to_observation(PredictionError, CA, Observation)), 
        Observations).

prediction_to_observation(Prediction, CA, Observation) :-
    prediction{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA} :< Prediction,
    ObservationWithoutId = observation{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA},
    observation_with_id(ObservationWithoutId, Observation).

prediction_error_to_observation(PredictionError, CA, Observation) :-
    prediction{origin:Origin, kind:Kind} :< PredictionError.prediction,
    ObservationWithoutId = observation{origin:Origin, kind:Kind, value:PredictionError.actual_value, confidence:PredictionError.confidence, by:CA},
    observation_with_id(ObservationWithoutId, Observation).

wellbeing_delta(Observations, WellbeingDelta) :-
    wellbeing:empty_wellbeing(EmptyWellbeing),
    length(Observations, N),
    % TODO - Get the fullness cost of an observation made from settings
    Delta is N * 0.01,
    WellbeingDelta = EmptyWellbeing.subtract_fullness(Delta).

