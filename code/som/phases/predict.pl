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

% unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
% resolving WorkStatus to more(IntermediateState, WellbeingDeltas), or it can be done(EndState, WellbeingDeltas) as the last or only solution. 

% Make predictions and send them to the umwelt.
% This is always a single unit of work. Where there's indeterminacy, random choices are made.
unit_of_work(CA, State, done(NewState, WellbeingDeltas)) :-
    log(info, predict, "Predicting for CA ~w from ~p", [CA, State]),
    predictions(CA, State, Predictions),
    resolve_conflicting_predictions(Predictions, [], Predictions1),
    log(info, predict, "~w made predictions ~p", [CA, Predictions1]),
    predictions_sent_to_umwelt(CA, Predictions1),
    put_state(State, predictions_out, Predictions1, NewState),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase predict done for CA ~w with ~p", [CA, NewState]).

predictions(CA, State, Predictions) :-
    get_state(State, umwelt, Umwelt),
    get_state(State, observations, Observations),
    get_state(State, causal_theory, CausalTheory),
    predictions_from_observations(CausalTheory, Observations, Umwelt, Predictions),
    log(info, predict, "~w predicts ~p", [CA, Predictions]).

% No observations yet. Guess randomly from experience domains.
predictions_from_observations(_, [], Umwelt, Predictions) :-
    !,
    umwelt_random_predictions(Umwelt, [], Predictions).

% No causal theory yet; predicting previous observations.
predictions_from_observations(none, Observations, _, Observations).

% Apply causal theory to predict the next observations.
predictions_from_observations(CausalTheory, Observations, _, Predictions) :-
    apply_causal_theory(CausalTheory, Observations, Predictions).

umwelt_random_predictions([], Acc, Acc).

umwelt_random_predictions([Child | Rest], Acc, UmweltPredictions) :-
    log(info, predict, "Making random predictions for ~w", [Child]),
    ca_random_predictions(Child, Predictions),
    append(Acc, Predictions, Acc1),
    umwelt_random_predictions(Rest, Acc1, UmweltPredictions).

ca_random_predictions(CA, Predictions) :-
    query_answered(CA, experience_domain, Predictables),
    random_predictions_from_predictables(Predictables, Predictions).

% TODO
apply_causal_theory(_, Observations, Observations).

% sensor experience domain = [predictable{name:SensorName, object:SenseName, domain:SenseDomain, by:SensorCA}]
% effector experience domain = [predictable{name:EffectorName, object:Action, domain:boolean, by:EffectorCA), ...]
% Make a prediction for each predictable
random_predictions_from_predictables(Predictables, Predictions) :-
    log(info, predict, "Making random predictions from domain ~p", [Predictables]),    
    setof(Prediction,
          (member(Predictable, Predictables),
          predictable{name:Name, object:Object, by:CA} :< Predictable,
          % The value of a predictable
          random_domain_value(Predictable.domain, Value, Confidence),
          Prediction = prediction{name:Name, object:Object, value:Value, confidence:Confidence, for:[CA]}
          ),
          Predictions).

% Resolve conflicting predictions.
% Two predictions conflict if they have the same name and are about the same object
% The one with greater confidence wins, else pick one randomly.
% When resolving two predictions into one, combine who they are for.
resolve_conflicting_predictions([], Predictions, Predictions).
resolve_conflicting_predictions([Prediction | Rest], Acc, ResolvedPredictions) :-
    findall(OtherPrediction, (member(OtherPrediction, Rest), conflicting_predictions(Prediction, OtherPrediction)), ConflictingPredictions),
    resolve_conflicts([Prediction | ConflictingPredictions], Eliminated, Resolution),
    subtract(Rest, Eliminated, Others),
    resolve_conflicting_predictions(Others, [Resolution | Acc], ResolvedPredictions).

resolve_conflicts([Prediction], [], Prediction).

resolve_conflicts(Predictions, Eliminated, Resolution) :-
    choose_high_confidence_prediction(Predictions, Prediction),
    delete(Predictions, Prediction, Eliminated),
    setof(For, (member(P, Predictions), For = P.for), AllFor),
    Resolution = Prediction.put([for = AllFor]).

% Predictions conflict if they have the same name (the name of the predicted experience)
% and object (what the prediction is about e.g. color, distance, luminance, count, more, coincide, trend)
conflicting_predictions(Prediction, OtherPrediction) :-
    prediction{name: Name, object: Object} :< Prediction,
    prediction{name: Name, object: Object} :< OtherPrediction.

% Select randomly a prediction with the highest confidence
choose_high_confidence_prediction(Predictions, Prediction) :-
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
    forall(member(UmweltCA, For), prediction_sent_to_ca(CA, UmweltCA, Prediction)),
    predictions_sent_to_umwelt(CA, Rest).

predictions_sent_to_umwelt(_, []).

prediction_sent_to_ca(CA, UmweltCA, Prediction) :-
    message_sent(UmweltCA, prediction(Prediction), CA),
    log(info, predict, "~w sent prediction ~p to ~w", [CA, Prediction, UmweltCA]).
