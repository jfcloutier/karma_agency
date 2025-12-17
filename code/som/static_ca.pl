
/*
Static CA support
*/

:- module(static_ca, []).

:- use_module(actors(actor_utils)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som)).

:- use_module(utils(logger)).

% Prediction = prediction{name:Name, object:Object, value:Value, confidence:Confidence} - Confidence is between 0.0 and 1.0
% Experience = experience{name:Name, object:Object, value:Value, confidence:Confidence}
% PredictionError = prediction_error{prediction: Prediction, actual_value:Value, confidence:Confidence}
% No state change for the moment
prediction_handled(Prediction, Parent, State, State) :-
    log(info, static_ca, "~@ is handling prediction ~p in state ~p", [self, Prediction, State]),
    % The prediction must be about an experience else it is not relevant
	about_experience(Prediction, State, Experience),
    log(info, static_ca, "Prediction ~p is about experience ~p of ~@", [Prediction, Experience, self]),
	(ca_support : same_experience_value(Prediction.value, Experience.value) ->
		true;
        % Confidence is based on deviation from predicted value given value domain, if domain is a range
        domain(State, Domain),
        prediction_error_confidence(Prediction.value, Experience.value, Domain, Confidence),
        PredictionError = prediction_error{prediction:Prediction, actual_value:Experience.value, confidence:Confidence},
        log(info, static_ca, "~@ is sending prediction error ~p to ~w", [self, PredictionError, Parent]),
		message_sent(Parent, prediction_error(PredictionError))
	).	

about_experience(Prediction, State, Experience) :-
    prediction{name:Name, object:Object} :< Prediction,
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{name:Name, object:Object} :< Experience.

domain(State, Domain) :-
    get_state(State, sensor, Sensor) ->
    Domain = Sensor.capabilities.domain
    ;
    Domain = boolean.

% Assumed: The predicted and experienced value differ and always fall within their domain
prediction_error_confidence(PredictedValue, ExperiencedValue, Domain, Confidence) :-
    is_dict(Domain, range),
    range{from:From, to:To} :< Domain,
    Delta is abs(PredictedValue - ExperiencedValue),
    deviation(Delta, From, To, Deviation),
    Confidence is 1.0 - Deviation.   

% If the domain is not a range then the confidence in the prediction being in error is 100%
prediction_error_confidence(_, _, _, 1.0).

deviation(Delta, From, To, Deviation) :-
    Span is To - From,
    Deviation is min(1.0, (Delta ** 2) / Span).