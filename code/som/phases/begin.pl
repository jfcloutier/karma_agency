/*
* Begin a new timeframe by persisting recently received predictions long enough to match the latency of parent CAs.
*
*/

:- module(observe, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/ca_support)).

% Persist predictions received within the approximate timeframe of a parent CA
unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
    get_state(State, predictions_in, PredictionsIn),
    findall(Prediction, (member(Prediction, PredictionsIn), is_prediction_persisted(Prediction, State)), PersistedPredictionsIn),
    StateDeltas = [predictions_in:PersistedPredictionsIn],
    wellbeing:empty_wellbeing(WellbeingDelta).
    log(info, begin, "Phase begin done for CA ~w with state deltas ~p and wellbeing delta ~p", [CA, StateDeltas, WellbeingDelta]).

% A prediction received persists long enough to cover the duration of a parent's timeframe.
is_prediction_persisted(Prediction, State) :-
    get_state(State, timeframe_count, TimeframeCount),
    prediction_persistence_duration(State, Duration),
    WhenReceived = Prediction.when_received,
    (TimeframeCount - WhenReceived)  =< Duration.

% The duration is in multiples of the CA's own latency
% i.e. approximately how many of the CA's timeframes fit with a parent CA's timeframe
prediction_persistence_duration(State, Duration) :-
    get_state(State, latency, Latency),
    get_state(State, level, Level),
    ParentLevel is Level + 1,
    latency(ParentLevel, ParentLatency),
    Duration is round(ParentLatency / Latency).
