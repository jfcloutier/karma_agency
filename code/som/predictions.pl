/*
Utilities for predcitions.

% prediction{origin:Origin, kind:Kind, value:Value, weight:Weight, confidence:Confidence, by: CA, when_received: Age} -- when_received is optional

*/

:- module(predictions, [prediction_of_activation/4, predictions_sent_to_umwelt/3]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/ca_support)).

%! prediction_of_activation(++Directive, ++CA, ++Status, --Prediction)
prediction_of_activation(Directive, CA, Status, Prediction) :-
    goal_id(Directive, GoalId),
    Origin = object{type: goal, id: GoalId, evidence:[Directive]},
    Prediction = prediction{origin:Origin, kind:activation, value:Status, confidence:1.0, weight:4, by: CA}.

%! predictions_sent_to_umwelt(++CA, ++State, ++Predictions)
predictions_sent_to_umwelt(_, _, []).

predictions_sent_to_umwelt(CA, State, [Prediction | Rest]) :-
    get_state(State, umwelt, Umwelt),
    forall(member(UmweltCA, Umwelt), prediction_sent_to_umwelt_ca(Prediction, UmweltCA, CA)),
    predictions_sent_to_umwelt(CA, Umwelt, Rest).

prediction_sent_to_umwelt_ca(Prediction, UmweltCA, CA) :-
    message_sent(UmweltCA, prediction(Prediction), CA),
    log(info, predictions, "~w sent prediction ~p to ~w", [CA, Prediction, UmweltCA]).



