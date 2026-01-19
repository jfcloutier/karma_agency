
/*
Static CA support
*/

:- module(static_ca, []).

:- use_module(actors(actor_utils)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som)).

:- use_module(utils(logger)).

%%% In static CA thread

% Prediction = prediction{origin:object{type:sensor, id:SensorName}, kind:SenseName, value:Value, confidence:Confidence, by: CA, for:CAs} - Confidence is between 0.0 and 1.0
% Experience = experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}
% PredictionError = prediction_error{prediction: Prediction, actual_value:Value, confidence:Confidence, by: CA}
% No state change for the moment
prediction_handled(Prediction, Parent, State, State) :-
    log(info, static_ca, "~@ is handling prediction ~p in state ~p", [self, Prediction, State]),
    % The prediction must be about an experience else it is not relevant
	about_experience(Prediction, State, Experience),
    !,
    log(info, static_ca, "Prediction ~p is about experience ~p of ~@", [Prediction, Experience, self]),
	(ca_support : same_experience_value(Prediction.value, Experience.value) ->
		true;
        % Confidence in the prediction error by static CAs is always 100%
        self(StaticCA),
        PredictionError = prediction_error{prediction:Prediction, actual_value:Experience.value, confidence:1.0, by:StaticCA},
        log(info, static_ca, "~@ is sending prediction error ~p to ~w", [self, PredictionError, Parent]),
		message_sent(Parent, prediction_error(PredictionError))
	).

prediction_handled(Prediction, Parent, State, State) :-
    self(StaticCA),
    PredictionError = prediction_error{prediction:Prediction, actual_value:unknown, confidence:1.0, by:StaticCA},
    log(info, static_ca, "~@ is sending prediction error ~p to ~w", [self, PredictionError, Parent]),
	message_sent(Parent, prediction_error(PredictionError)).

about_experience(Prediction, State, Experience) :-
    prediction{origin:Origin, kind:Kind} :< Prediction,
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{origin:Origin, kind:Kind} :< Experience.

% domain(State, Domain) :-
%     get_state(State, sensor, Sensor) ->
%     Domain = Sensor.capabilities.domain
%     ;
%     Domain = boolean.

% Assumed: The predicted and experienced value differ and always fall within their domain
% prediction_error_confidence(unknown, _, _, 1.0) :- !.
% prediction_error_confidence(_, unknown, _, 1.0) :- !.
% prediction_error_confidence(PredictedValue, ExperiencedValue, Domain, Confidence) :-
%     is_dict(Domain, range),
%     range{from:From, to:To} :< Domain,
%     Delta is abs(PredictedValue - ExperiencedValue),
%     deviation(Delta, From, To, Deviation),
%     Confidence is 1.0 - Deviation.   

% % If the domain is not a range then the confidence in the prediction being in error is 100%
% prediction_error_confidence(_, _, _, 1.0).

% deviation(Delta, From, To, Deviation) :-
%     Span is To - From,
%     Deviation is min(1.0, (Delta ** 2) / Span).