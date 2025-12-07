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

% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState) or done(EndState) as last solution. 

% Make predictions and send them to the umwelt.
% This is always a single unit of work. Where there's indeterminacy, random choices are made.
unit_of_work(CA, State, done(NewState)) :-
    log(info, predict, "Predicting for CA ~w from ~p", [CA, State]),
    predictions(State, Predictions),
    % All predictions sent to the entire umwelt
    predictions_sent_to_umwelt(State, Predictions),
    timeframe_updated(State, [predictions_out-Predictions], NewState),
    log(info, predict, "Phase predict done for CA ~w with ~p", [CA, NewState]).

predictions(State, Predictions) :-
    get_state(State, umwelt, Umwelt),
    get_state(State, timeframe, Timeframe),
    get_state(State, causal_theory, CausalTheory),
    predictions_from_observations(CausalTheory, Timeframe.observations, Umwelt, Predictions),
    log(info, predict, "~@ predicts ~p", [self, Predictions]).

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
    log(info, predict, "Making random predictions"),
    ca_random_predictions(Child, Predictions),
    grow_predictions(Predictions, Acc, Acc1),
    umwelt_random_predictions(Rest, Acc1, UmweltPredictions).

ca_random_predictions(CA, Predictions) :-
    query_answered(CA, experience_domain, ExperienceDomain),
    random_predictions_from_domain(ExperienceDomain, Predictions).

% TODO
apply_causal_theory(_, Observations, Observations).

% sensor experience domain = [predictable{name:SenseName, object:SensorName, value:SenseDomain}]
% effector experience domain = [predictable{name:Action, object:EffectorName, value:boolean), ...]
random_predictions_from_domain(ExperienceDomain, Predictions) :-
    log(info, predict, "Making random predictions from domain ~p", [ExperienceDomain]),    
    setof(Prediction,
          (member(Predictable, ExperienceDomain),
          predictable{name:Name, object:Object} :< Predictable,
          random_domain_value(Predictable.value, Value),
          Prediction = predicted{name:Name, object:Object, value:Value}
          ),
          Predictions).

% Add Prediction to Predictions, removing conflicting (randomly chosen) if any
% Two predictions conflict if they have the same name and are about the same object
grow_predictions([], Predictions, Predictions).
grow_predictions([CAPrediction | Rest], Acc, NewPredictions) :-
    member(OtherPrediction, Acc),
    conflicting_predictions(CAPrediction, OtherPrediction),
    !,
    (maybe ->
        delete(Acc, OtherPrediction, Acc1),
        Acc2 = [CAPrediction | Acc1]
        ;
        Acc2 = Acc
    ),
    grow_predictions(Rest, Acc2, NewPredictions).

grow_predictions([CAPrediction | Rest], Acc, NewPredictions) :-
    grow_predictions(Rest, [CAPrediction | Acc], NewPredictions).

conflicting_predictions(Prediction, OtherPrediction) :-
    predicted{name: Name, object: Object} :< Prediction,
    predicted{name: Name, object: Object} :< OtherPrediction.

predictions_sent_to_umwelt(State, Predictions) :-
    get_state(State, umwelt, Umwelt), 
    concurrent_forall(member(Child, Umwelt), predictions_sent_to_ca(Child, Predictions)).

predictions_sent_to_ca(CA, [Prediction | Rest]) :-
    message_sent(CA, prediction(Prediction)),
    log(info, predict, "Prediction ~p sent to ~w", [Prediction, CA]),
    predictions_sent_to_ca(CA, Rest).

predictions_sent_to_ca(_, []).