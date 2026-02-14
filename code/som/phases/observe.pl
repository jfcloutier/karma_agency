/*
Merge correct predictions and effective prediction errors into new observations.
*/

:- module(observe, []).

:- use_module(library(uuid)).
:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/dynamic_ca)).
:- use_module(agency(som/ca_support)).

% No work done before units of work
before_work(_, State, State).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 

% observation{origin:object{type:Type, id:ID}, kind:Kind, value:Value, confidence:Confidence, by:CA: of:UmweltCAs, id:Id}
% The object in an observation omits its "support set" (what was integrated in its synthesis as part of an umwelt experience) 
% The value observed may not be that experienced by each of the umwelt CAs under observation, just the one with highest confidence
unit_of_work(CA, State, done(StateDeltas, WellbeingDeltas)) :-
    observed(CA, State, Observations),
    StateDeltas = [observations-Observations],
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, observe, "Phase observe done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

observed(CA, State, Observations) :-
    aggregate_prediction_errors(State, Pairs),
    umwelt_size(State, UmweltSize),
    correct_predictions(Pairs, UmweltSize,CorrectPredictions),
    effective_prediction_errors(Pairs, CorrectPredictions, EffectivePredictionErrors),
    predictions_to_observations(CorrectPredictions, CA, Observations1),
    prediction_errors_to_observations(EffectivePredictionErrors, CA, Observations2),
    append(Observations1, Observations2, Observations),
    log(info, observe, "(~w) Observed ~p", [CA, Observations]).

umwelt_size(State, Size) :-
    get_state(State, umwelt, Umwelt),
    length(Umwelt, Size).

% Pair up predictions and their associated prediction errors if any
% Ignore an empty prediction, using predictions in prediction errors instead to replace it
aggregate_prediction_errors(State, Pairs) :-
    get_state(State, prediction_errors, PredictionErrors),
    effective_predictions(State, EffectivePredictions),
    pair_up(EffectivePredictions, PredictionErrors, [], Pairs).

% Find all non-empty prediction made and the implied predictions made via an empty prediction
effective_predictions(State, EffectivePredictions) :-
    get_state(State, predictions_out, Predictions),
    get_state(State, prediction_errors, PredictionErrors),
    findall(Prediction, (member(Prediction, Predictions), \+ is_empty_prediction(Prediction)), FullPredictions),
    findall(ImpliedPrediction, 
        (member(PredictionError, PredictionErrors), ImpliedPrediction = PredictionError.prediction, ImpliedPrediction.value == unknown), 
        ImpliedPredictions),
    sort(ImpliedPredictions, UniqueImpliedPredictions),
    append(FullPredictions, UniqueImpliedPredictions, EffectivePredictions).

pair_up([], _, Pairs, Pairs).

pair_up([Prediction | Rest], PredictionErrors, Acc, Pairs) :-
    findall(PredictionError,
        (member(PredictionError, PredictionErrors), Prediction = PredictionError.prediction),
    RelatedPredictionErrors),
    pair_up(Rest, PredictionErrors, [Prediction - RelatedPredictionErrors | Acc], Pairs).

correct_predictions(Pairs, UmweltSize,CorrectPredictions) :-
    findall(CorrectPrediction, 
        (member(Prediction-PredictionErrors, Pairs), correct_prediction(Prediction, PredictionErrors, UmweltSize, CorrectPrediction)), 
        CorrectPredictions).

% A prediction is correct if 
%   it is not an empty prediction
%   it did not receive prediction errors
%   it did NOT receive unknown-valued error from its entire umwelt
%   it received only valued prediction errors of lower confidence (confidence reduced to the most confident one)
correct_prediction(Prediction, _, _, _) :-
    Prediction.value == unknown,
    !,
    fail.

correct_prediction(Prediction, [], _, Prediction).

correct_prediction(_, PredictionErrors, UmweltSize, _) :-
    findall(PredictionError, (member(PredictionError, PredictionErrors), PredictionError.actual_value == unknown), Unknowns),
    length(Unknowns, UmweltSize),
    !,
    fail.

correct_prediction(Prediction, PredictionErrors, _, CorrectPrediction) :-
    findall(PredictionError, (member(PredictionError, PredictionErrors), PredictionError.actual_value \= unknown), ValuedPredictionErrors),
    (length(ValuedPredictionErrors, 0) ->
        CorrectPrediction = Prediction
        ;
        sort(confidence, @>, ValuedPredictionErrors, [TopPredictionError | _]),
        Prediction.confidence > TopPredictionError.confidence,
        CorrectPrediction = Prediction.put(confidence, TopPredictionError.confidence)
    ).

% A prediction error is effective if
%   it is valued and contests a prediction with higher (and the highest) confidence)
effective_prediction_errors(Pairs, CorrectPredictions, EffectivePredictionErrors) :-
    findall(PredictionErrors,
        (member(Prediction-PredictionErrors, Pairs), \+ member(Prediction, CorrectPredictions)),
     PredictionErrorGroups),
     findall(SurvivingPredictionError,
        (member(PredictionErrorGroup, PredictionErrorGroups), sort(confidence, @>, PredictionErrorGroup, [SurvivingPredictionError | _])),
        EffectivePredictionErrors).

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
    observation_id(ObservationWithoutId, Id),
    Observation = ObservationWithoutId.put(id, Id).

prediction_error_to_observation(PredictionError, CA, Observation) :-
    prediction{origin:Origin, kind:Kind} :< PredictionError.prediction,
    ObservationWithoutId = observation{origin:Origin, kind:Kind, value:PredictionError.actual_value, confidence:PredictionError.confidence, by:CA},
    observation_id(ObservationWithoutId, Id),
    Observation = ObservationWithoutId.put(id, Id).

% Two observations from any CAs must have the same ids if they are semantically equivalent.
observation_id(Observation, Id) :-
    observation{origin:Object, kind:Kind, value:Value} :< Observation,
    object_hash(Object, ObjectHash),
    value_hash(Value, ValueHash),
    atomic_list_hash([ObjectHash, Kind, ValueHash], Id).

