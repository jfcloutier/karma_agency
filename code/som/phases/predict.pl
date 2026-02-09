/*
Make predictions about future observations from current observations given a causal theory.

If there is no causal theory (yet), predict the previous observations.
If there are no previous observations, copy all experiences in the umwelt as initial observations.
*/

:- module(predict, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/domain)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, State, State).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
% to more(StateDeltas, WellbeingDeltas) or done(StateDeltas, WellbeingDeltas) as last solution. 

% Make predictions and send them to the umwelt.
% This is always a single unit of work. Where there's indeterminacy, random choices are made.
unit_of_work(CA, State, done(StateDeltas, WellbeingDeltas)) :-
    log(info, predict, "Predicting for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]),
    predictions(CA, State, Predictions),
    resolve_conflicting_predictions(Predictions, [], Predictions1),
    log(info, predict, "~w made predictions ~p", [CA, Predictions1]),
    predictions_sent_to_umwelt(CA, Predictions1),
    StateDeltas = [predictions_out-Predictions1],
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase predict done for CA ~w", [CA]).

predictions(CA, State, Predictions) :-
    get_state(State, umwelt, Umwelt),
    get_state(State, observations, Observations),
    get_state(State, causal_theory, CausalTheory),
    predictions_from_observations(CA, CausalTheory, Observations, Umwelt, Predictions),
    log(info, predict, "~w predicts ~p", [CA, Predictions]).

% No observations yet. Guess randomly from experience domains.
predictions_from_observations(CA, _, [], Umwelt, Predictions) :-
    !,
    umwelt_random_predictions(CA, Umwelt, [], Predictions).

% No causal theory yet; predicting current observations.
predictions_from_observations(CA, none, Observations, _, Predictions) :-
    observations_as_predictions(CA, Observations, Predictions).

% Apply causal theory to predict the next observations.
predictions_from_observations(CA, CausalTheory, Observations, _, Predictions) :-
    apply_causal_theory(CA, CausalTheory, Observations, NextObservations),
    observations_as_predictions(CA, NextObservations, Predictions).

umwelt_random_predictions(_, [], Acc, Acc).

umwelt_random_predictions(CA, [UnweltCA | Rest], Acc, UmweltPredictions) :-
    log(info, predict, "Making random predictions for ~w", [UnweltCA]),
    ca_random_predictions(CA, UnweltCA, Predictions),
    append(Acc, Predictions, Acc1),
    umwelt_random_predictions(CA, Rest, Acc1, UmweltPredictions).

ca_random_predictions(CA, UmweltCA, Predictions) :-
    query_answered(UmweltCA, experience_domain, Predictables),
    random_predictions_from_predictables(CA, Predictables, Predictions).

% TODO - Applying a causal theory to current observations produces expected observations (caused and retained) to be converted into predictions
apply_causal_theory(_, _, Observations, Observations).

% sensor experience domain = [predictable{origin:object{type:sensor, id:SensorName}, kind:SenseName, domain:SenseDomain, by:SensorCA}]
% effector experience domain = [predictable{origin:object{type:effector, id:EffectorName}, kind:Action, domain:count, by:EffectorCA), ...]
% Make a prediction for each predictable
random_predictions_from_predictables(CA, Predictables, Predictions) :-
    log(info, predict, "Making random predictions from domain ~p", [Predictables]),    
    setof(Prediction,
          (member(Predictable, Predictables),
          predictable{origin:Origin, kind:Kind, by:UmweltCA} :< Predictable,
          % The value of a predictable
          random_domain_value(Predictable.domain, Value),
          % A random prediction is done with full, blind confidence because no evidence exists to attenuate it
          Prediction = prediction{origin:Origin, kind:Kind, value:Value, confidence:1.0, by: CA, for:[UmweltCA]}
          ),
          Predictions).

% Translate expected observations into predictions.
observations_as_predictions(CA, Observations, Predictions) :-
    findall(Prediction, (member(Observation, Observations), observation_as_prediction(CA, Observation, Prediction)), Predictions).
        
% Turn an observation into a prediction
observation_as_prediction(CA, Observation, Prediction) :-
    observation{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by:CA, of:UmweltCAs} :< Observation,
    Prediction = prediction{origin:Origin, kind:Kind, value:Value, confidence:Confidence, by: CA, for:UmweltCAs}.

% Resolve conflicting predictions.
% Two predictions conflict if they have the same name and are about the same object
% The one with greater confidence wins, else pick one randomly.
% When resolving conflicting predictions into one, combine who they are for.
resolve_conflicting_predictions([], Predictions, Predictions).
resolve_conflicting_predictions([Prediction | Rest], Acc, ResolvedPredictions) :-
    findall(OtherPrediction, (member(OtherPrediction, Rest), conflicting_predictions(Prediction, OtherPrediction)), ConflictingPredictions),
    resolve_conflicts([Prediction | ConflictingPredictions], Resolution),
    subtract(Rest, ConflictingPredictions, Others),
    resolve_conflicting_predictions(Others, [Resolution | Acc], ResolvedPredictions).

resolve_conflicts([Prediction], Prediction).

resolve_conflicts(ConflictingPredictions, Resolution) :-
    high_confidence_prediction(ConflictingPredictions, Prediction),
    setof(For, (member(P, ConflictingPredictions), For = P.for), AllFor),
    Resolution = Prediction.put([for = AllFor]).

% Predictions conflict if they have the same name (the name of the predicted experience)
% and object (what the prediction is about e.g. color, distance, luminance, count, more, trend)
conflicting_predictions(Prediction, OtherPrediction) :-
    prediction{origin:Origin, kind:Kind} :< Prediction,
    prediction{origin:Origin, kind:Kind} :< OtherPrediction.

% Select randomly a prediction with the highest confidence
high_confidence_prediction(Predictions, Prediction) :-
    random_permutation(Predictions, Permutation),
    highest_confidence_prediction(Permutation, Prediction).

highest_confidence_prediction([Candidate | Rest], Prediction) :-
   highest_confidence_prediction(Rest, Candidate, Prediction). 

highest_confidence_prediction([], Prediction, Prediction).

highest_confidence_prediction([Candidate | Rest], BestSoFar, Prediction) :-
    Candidate.confidence > BestSoFar.confidence ->
        highest_confidence_prediction(Rest, Candidate, Prediction) 
        ;
        highest_confidence_prediction(Rest, BestSoFar, Prediction).
  
predictions_sent_to_umwelt(CA, [Prediction | Rest]) :-
    For = Prediction.for, 
    forall(member(UmweltCA, For), prediction_sent_to_umwelt_ca(Prediction, UmweltCA, CA)),
    predictions_sent_to_umwelt(CA, Rest).

predictions_sent_to_umwelt(_, []).

prediction_sent_to_umwelt_ca(Prediction, UmweltCA, CA) :-
    message_sent(UmweltCA, prediction(Prediction), CA),
    log(info, predict, "~w sent prediction ~p to ~w", [CA, Prediction, UmweltCA]).
