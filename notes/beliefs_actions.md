# Beliefs, sensings and actions

Which GM is this:

    this_gm(GM). % asserted once
    this_round(Round). % asserted with each a new round

## Context

The pengine associated with a GM receives assertions for that GM.

    this_gm(GM). % asserted once at creation of the pengine
    this_round(Round). % asserted/replaced with each a new round
    in_round(Round, Data). % asserted during a round

where Data is either:

* a Belief held by the GM,
* a Prediction made or received by the GM
* a PredictionError made or received by the GM

## Representing beliefs

Generative Models (GMs) hold `belief`s. A belief is equivalent to a unary predicate, .e.g. `closer_to_obstacle(self)` or to a binary predicate, e.g. `distance _to_obstacle(self, far)`. Detectors also hold beliefs , e.g. `distance(25)` or `color(blue)` or `orientation(channel_1, 45)`,but is represented as `belief/2` and `belief/3`

    belief(Conjecture, about(Object)) % asserted "unary" belief by the GM - of this_gm(GM) 
    belief(Conjecture, about(Object, ValueOrObject)) % asserted "binary" belief by the GM 

Conjecture names the type of belief. It is an atom either defined by the Generative Model (GM), e.g. `closer_to_obstacle`, or created by Karma, e.g. `c1`

Object names who/what the belief is about. It is either defined by the GM, e.g. `self` or `other` or `channel_1`, or created by Karma to represent a latent object in the environment, e.g. `o2`. The value can be a number or an object name.

Predictions are about beliefs. Prediction errors are about predictions.

    prediction(Belief, GoalOrOpinion, ByGM) % goal or opinion predicted belief by a named GM

    prediction_error(Prediction, CorrectedBelief, ByGM) % contrarian belief reported by named GM about a prediction, with unified Conjecture and Object in the prediction belief and actual belief

 They are, from the perspective of a GM, inferred to be either sent or received.

    sent_prediction(prediction(Belief, GoalOrOpinion, ByGM)) :- this_gm(GM), byGM == GM.

    received_prediction(prediction(Belief, GoalOrOpinion, ByGM)) :- this_gm(GM), byGM \== GM.

    sent_prediction_error(prediction_error(Prediction, CorrectedBelief, ByGM)) :- this_gm(GM), byGM == GM.

    received_prediction_error(prediction_error(Prediction, CorrectedBelief, ByGM)) :- this_gm(GM), byGM \== GM.

Perceptions are inferred from sent/received predictions and prediction errors (from the same round):

    subject(prediction(belief(Conjecture, Object), _, _), [Conjecture, Object]).
    subject(prediction(belief(Conjecture, Object, _), _, _), [Conjecture, Object]).
    subject(prediction_error(Prediction, _, ), [Conjecture, Object]) :- subject(Prediction, [Conjecture, Object]).
    
    believed(prediction(Belief, _, _), Belief).
    believed(prediction_error(_, CorrectedBelief, _), CorrectedBelief).

    contradicted(Prediction) :- sent_prediction(Prediction), 
                                received_prediction_error(PredictionError), 
                                believed(Prediction, PredictedBelief),
                                believed(PredictionError, CorrectedBelief),
                                subject(PredictedBelief) == subject(CorrectedBelief), !.

    perception(Belief) :- sent_prediction(Prediction), not contradicted(Prediction), believed(Prediction, Belief).
    perception(Belief) :- received_prediction_error(PredictionError), believed(PredictionError, Belief).

Actions

    action(Intent, Parameters)
