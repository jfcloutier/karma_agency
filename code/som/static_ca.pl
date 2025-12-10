
/*
Static CA support
*/

:- module(static_ca, []).

:- use_module(actors(actor_utils)).
:- use_module(agency(som/ca_support)).
:- use_module(utils(logger)).

% Prediction = prediction{name:Name, object:Object, value:Value}
% Experience = experience{name:Name, object:Object, value:Value, tolerance:Tolerance}
% PredictionError = prediction_error{prediction: Prediction, actual_value:Value}
% No state change for the moment
prediction_handled(Prediction, Parent, State, State) :-
    log(info, ca_support, "Handling prediction ~p in state ~p", [Prediction, State]),
    % The prediction must be about an experience else it is not relevant
	about_experience(Prediction, State, Experience),
    log(debug, ca_support, "Prediction ~p is about experience ~p of ~@", [Prediction, Experience, self]),
	(ca_support : same_experience_value(Prediction.value, Experience.value, Experience.tolerance) ->
		true;
		message_sent(Parent, prediction_error(prediction_error{prediction:Prediction, actual_value:Experience.value}))
	).	

about_experience(Prediction, State, Experience) :-
    prediction{name:Name, object:Object} :< Prediction,
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{name:Name, object:Object} :< Experience.
