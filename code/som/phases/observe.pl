/*
Merge predictions and prediction errors into new observations.
*/

:- module(observe, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/dynamic_ca)).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution.

% observation{origin:object{type:Type, id:ID}, kind:Kind, value:Value, confidence:Confidence, by:CA: of:UmweltCAs}
% The value observed may not be that experienced by each of the umwelt CAs under observation, just the one with highest confidence
unit_of_work(CA, State, done(NewState, WellbeingDeltas)) :-
    observed(CA, State, Observations),
    put_state(State, observations, Observations, NewState),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, observe, "Phase observe done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

observed(CA, State, Observations) :-
    get_state(State, predictions_out, Predictions),
    get_state(State, prediction_errors, PredictionErrors),
    high_confidence_prediction_errors(PredictionErrors, [], TopPredictionErrors),
    log(info, observe, "~w got high confidence prediction errors ~p to predictions ~p", [CA, TopPredictionErrors, Predictions]),
    observations_from_predictions(Predictions, TopPredictionErrors, CA, Observations).

% Resolve conflicting prediction errors by keeping the most confident one.
high_confidence_prediction_errors([], PredictionErrors, PredictionErrors).

high_confidence_prediction_errors([PredictionError | Rest], Acc, TopPredictionErrors) :-
    findall(OtherPredictionError, 
        (member(OtherPredictionError, Rest), conflicting_prediction_errors(PredictionError, OtherPredictionError)), 
        ConflictingPredictionErrors),
    % No need to randomize since order in which prediction errors are received and accumulated is non-deterministic
    highest_confidence_prediction_error([PredictionError | ConflictingPredictionErrors], Resolution),
    subtract(Rest, ConflictingPredictionErrors, Others),
    high_confidence_prediction_errors(Others, [Resolution | Acc], TopPredictionErrors).

% Prediction errors conflict if they are about possibly conflicting predictions
conflicting_prediction_errors(PredictionError, OtherPredictionError) :-
    prediction{origin:Origin, kind:Kind} :< PredictionError.prediction,
    prediction{origin:Origin, kind:Kind} :< OtherPredictionError.prediction.

% Find the most confident prediction error among a conflicting list
highest_confidence_prediction_error([PredictionError1 | Rest], PredictionError) :-
    highest_confidence_prediction_error(Rest, PredictionError1, PredictionError).

highest_confidence_prediction_error([], PredictionError, PredictionError).

highest_confidence_prediction_error([Candidate | Rest], BestSoFar, PredictionError) :-
    Candidate.confidence > BestSoFar.confidence ->
        highest_confidence_prediction_error(Rest, Candidate, PredictionError) 
        ;
        highest_confidence_prediction_error(Rest, BestSoFar, PredictionError).

observations_from_predictions(Predictions, PredictionErrors, CA, Observations) :-
    observations_from_predictions(Predictions, PredictionErrors, CA, [], Observations).

observations_from_predictions([], _, _, Observations, Observations).

observations_from_predictions([Prediction | Rest], PredictionErrors, CA, Acc, Observations) :-
    observation_from_prediction(Prediction, PredictionErrors, CA, Observation),
    observations_from_predictions(Rest, PredictionErrors, CA, [Observation | Acc], Observations).

observation_from_prediction(Prediction, PredictionErrors, CA, Observation) :-
    member(PredictionError, PredictionErrors),
    contradicted_by(Prediction, PredictionError),
    !,
    contradiction_to_observation(Prediction, PredictionError, CA, Observation).

observation_from_prediction(Prediction, _, CA, Observation) :-
    prediction_to_observation(Prediction, CA, Observation).

% A prediction error less confident than the prediction erodes the confidence in the contradicted prediction.
% A prediction error more or equally confident than the contradicted prediction overrides the prediction and keeps its confidence.
contradiction_to_observation(Prediction, PredictionError, CA, Observation) :-
    PredictionError.confidence >= Prediction.confidence ->
        prediction_error_to_observation(PredictionError, CA, Observation1),
        Observation = Observation1.put([of=Prediction.for])
    ;
        Confidence is Prediction.confidence - PredictionError.confidence,
        prediction_to_observation(Prediction, CA, Observation1),
        Observation = Observation1.put([confidence=Confidence]).
 
contradicted_by(Prediction, PredictionError) :-
    WrongPrediction = PredictionError.prediction,
    prediction{origin:Origin, kind:Kind} :< WrongPrediction,
    prediction{origin:Origin, kind:Kind} :< Prediction.

prediction_to_observation(Prediction, CA, Observation) :-
    prediction{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA, for:UmweltCAs} :< Prediction,
    Observation = observation{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA, of:UmweltCAs}.

prediction_error_to_observation(PredictionError, CA, Observation) :-
    prediction{origin:Origin, kind:Kind, for:UmweltCAs} :< PredictionError.prediction,
    Observation = observation{origin:Origin, kind:Kind, value:PredictionError.actual_value, confidence:PredictionError.confidence, by:CA, of:UmweltCAs}.
