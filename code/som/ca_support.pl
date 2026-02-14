/*
Cognition Actor support library.
*/

:- module(ca_support, [from_parent/2, wellbeing_transfered/3, well_enough/1, object_hash/2, value_hash/2, atomic_list_hash/2, average_confidence/2, merge_wellbeing/3, empty_prediction/2, is_empty_prediction/1, prediction_handled/3]).

:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(utils(logger)).
:- use_module(library(sha)).
:- use_module(library(apply)).

handled(message(Message, Source), State, State) :-
	log(debug, ca_support, "~@ is NOT handling message ~p from ~w", [self, Message, Source]).

handled(query(parents), State, Parents) :-
    get_state(State, parents, Parents).

% FOR DEBUGGING ONLY
handled(query(state), State, State).

handled(query(Query), State, unknown) :-
    log(debug, ca_support, "~@ is NOT handling query ~p given state ~p", [self, Query, State]).

% Remove from parents if applicable
handled(event(ca_terminated, _, Source), State, NewState) :-
    get_state(State, parents, Parents),
    member(Source, Parents),
    subtract(Parents, [Source], Parents1),
    put_state(State, parents, Parents1, NewState).

handled(event(Topic, Payload, Source), State, State) :-
    log(debug, ca_support, "~@ is NOT handling event ~p with ~p from ~w", [self, Topic, Payload, Source]).
    
from_parent(Source, State) :-
    get_state(State, parents, Parents),
    member(Source, Parents).

wellbeing_transfered(State, WellbeingTransfer, NewState) :-
    get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	option(integrity(Integrity), Wellbeing),
	option(engagement(Engagement), Wellbeing),
	option(fullness(FullnessTransfer), WellbeingTransfer),
	option(integrity(IntegrityTransfer), WellbeingTransfer),
	option(engagement(EngagementTransfer), WellbeingTransfer),
    Fullness1 is min(100, Fullness + FullnessTransfer),
    Integrity1 is min(100, Integrity + IntegrityTransfer),
    Engagement1 is min(100, Engagement + EngagementTransfer),
	put_state(State, wellbeing, wellbeing{fullness:Fullness1, integrity:Integrity1, engagement:Engagement1}, NewState).

% Well enough to do something if both fullness and integrity are > 0
well_enough(State) :-
	get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	Fullness > 0,
	option(integrity(Integrity), Wellbeing),
	Integrity > 0,
    log(debug, ca_support, "~@ is well enough", [self]).

object_hash(Object, Hash) :-
    is_dict(Object, object),
    object{type:Type, id:Id} :< Object,
    atomic_list_concat([Type, Id], ":", Data),
    sha_hash(Data, Sha, []),
    hash_atom(Sha, Hash),
    !.

object_hash(Object, _) :-
    log(error, ca_support, "@@@ object_hash of ~p", [Object]),
    throw("BAD HASH").

value_hash(Value, Hash) :-
    is_dict(value, object) ->
        object_hash(Value, Hash)
        ;
        number(Value) ->
            number_string(Value, String),
            value_hash(String, Hash)
            ;
            sha_hash(Value, Sha, []),
            hash_atom(Sha, Hash).

atomic_list_hash(List, Hash) :-
    atomic_list_concat(List, Data),
    sha_hash(Data, Sha, []),
    hash_atom(Sha, Hash).

average_confidence([], 0).
average_confidence(DictsWithConfidence, AverageConfidence) :-
    length(DictsWithConfidence, N),
    N > 0,
    findall(Confidence, (member(WithConfidence, DictsWithConfidence), get_dict(confidence, WithConfidence, Confidence)), Confidences),
    sum_list(Confidences,Sum),
    AverageConfidence is Sum / N.

merge_wellbeing(State, WellbeingDeltas, NewState) :-
	log(debug, dynamic_ca, "Merge wellbeing deltas ~p into state", [WellbeingDeltas]),
	get_state(State, wellbeing, Wellbeing),
	apply_wellbeing_deltas(Wellbeing, WellbeingDeltas, Wellbeing1),
	put_state(State, wellbeing, Wellbeing1, NewState).

apply_wellbeing_deltas(Wellbeing, WellbeingDeltas, NewWellbeing) :-
	Fullness is max(Wellbeing.fullness + WellbeingDeltas.fullness, 0),
	Integrity is max(Wellbeing.integrity + WellbeingDeltas.integrity, 0),
	Engagement is max(Wellbeing.engagement + WellbeingDeltas.engagement, 0),
	NewWellbeing = wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement}.

empty_prediction(CA, Prediction) :-
    Prediction = prediction{origin:unknown, kind:unknown, value:unknown, confidence:0, priority:0, by:CA}.

% Whether this is an empty prediction
is_empty_prediction(Prediction) :-
	prediction{origin:unknown, kind:unknown, value:unknown} :< Prediction.

prediction_about_experience(Prediction, State, Experience) :-
    prediction{origin:Origin, kind:Kind} :< Prediction,
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{origin:Origin, kind:Kind} :< Experience.

% Prediction = prediction{origin:object{type:sensor, id:SensorName}, kind:SenseName, value:Value, priority:Priority, confidence:Confidence, by: CA} - Confidence is between 0.0 and 1.0
% Experience = experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}
% PredictionError = prediction_error{prediction: Prediction, actual_value:Value, confidence:Confidence, by: CA}
% No state change for the moment
prediction_handled(Prediction, State, State) :-
	is_empty_prediction(Prediction),
    log(info, ca_support, "~@ is handling empty prediction", [self]),
	!,
	get_state(State, experiences, Experiences),
	findall(PredictionError, 
		    (member(Experience, Experiences), prediction_error_from_experience(Experience, Prediction, PredictionError)), 
		    PredictionErrors),
	prediction_errors_sent(PredictionErrors, Prediction.by).

prediction_handled(Prediction, State, State) :-
    log(info, ca_support, "~@ is handling prediction ~p", [self, Prediction]),
	self(CA),
	prediction_about_experience(Prediction, State, Experience),
    log(info, ca_support, "Prediction ~p is about experience ~p of ~@", [Prediction, Experience, self]),
	!,
	(same_experience_value(Experience.value, Prediction.value) ->
		true
		;
		PredictionError = prediction_error{prediction:Prediction, actual_value:Experience.value, confidence:Experience.confidence, by:CA},
		prediction_error_sent(PredictionError, Prediction.by)
	).

prediction_handled(Prediction, State, State) :-
    self(CA),
    % Can't confirm or invalidate a prediction. Confidence is 0.
    PredictionError = prediction_error{prediction:Prediction, actual_value:unknown, confidence:0.0, by:CA},
	prediction_error_sent(PredictionError, Prediction.by).

same_experience_value(Value, Value) :-
    Value \== unknown.

prediction_error_from_experience(Experience, Prediction, PredictionError) :-
	self(CA),
	experience{origin:Origin, kind:Kind, value: Value, confidence:Confidence} :< Experience,
	% fill in prediction in case it is empty
	FilledPrediction = Prediction.put([origin=Origin, kind=Kind]),
	PredictionError = prediction_error{prediction:FilledPrediction, actual_value:Value, confidence:Confidence, by: CA}.

prediction_errors_sent([], _).

% Best effort at sending prediction errors back to the CA where the predictions came from.
prediction_errors_sent([PredictionError | Rest], Parent) :-
	prediction_error_sent(PredictionError, Parent),
	prediction_errors_sent(Rest, Parent).

prediction_error_sent(PredictionError, Parent) :-
	log(info, ca_support, "~@ is sending prediction error ~p to ~w", [self, PredictionError, Parent]),
	message_sent(Parent, prediction_error(PredictionError)).

prediction_error_sent(_, _).



