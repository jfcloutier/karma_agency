/*
Cognition Actor support library.
*/

:- module(ca_support, [latency/2, from_parent/2, wellbeing_transfered/3, well_enough/1, object_hash/2, value_hash/2, atomic_list_hash/2, average_confidence/2, merge_phase_deltas/5, 
                       merge_wellbeing/3, empty_prediction/2, is_empty_prediction/1, prediction_handled/3, simple_count/2, inc_simple_count/2, dec_simple_count/2,
					   is_effector/1, is_sensor/1,
					   compare_activation_status/3]).

:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(utils(logger)).
:- use_module(library(sha)).
:- use_module(library(apply)).

%! latency(+Level, -Latency) is det
% The time in seconds allocated to a cognition actor to complete a time frame given its level in the SOM
latency(Level, Latency) :-
	% Latency is max(0.1, 2 ** (Level - 1) / 2).
	Latency is max(0.2, 4 ** (Level - 1) / 2).

% For a CA, two plans are the same if they have the same directives in whatever order
agency_state_sorter(=, Plan1, Plan2) :-
	is_dict(Plan1, plan),
	is_dict(Plan2, plan),
	same_length(Plan1.directives, Plan2.directives), 
	predsort(agency_state_sorter, Plan1.directives, Directives1),
	predsort(agency_state_sorter, Plan2.directives, Directives2),
	all_same_goals(Directives1, Directives2), !.

agency_state_sorter(=, Observation1, Observation2) :-
	is_dict(Observation1, observation),
	is_dict(Observation2, observation),
	Observation1.id == Observation2.id, !.

agency_state_sorter(=, Prediction1, Prediction2) :-
	is_dict(Prediction1, prediction),
	is_dict(Prediction2, prediction),
	prediction{origin:Origin, kind:Kind, value:Value} :< Prediction1,
	prediction{origin:Origin, kind:Kind, value:Value} :< Prediction2,
	!.

agency_state_sorter(Delta, E1, E2) :-
	compare(Delta, E1, E2).
	
all_same_goals([], []).
all_same_goals([Goal1 | Rest1], [Goal2 | Rest2]) :-
	Goal1.id == Goal2.id,
	all_same_goals(Rest1, Rest2).

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

% Merge wellbeing deltas and properties changed by the phase
merge_phase_deltas(StateDeltas, WellbeingDelta, ProducedProperties, State, NewState) :-
	merge_phase_state_properties(ProducedProperties, StateDeltas, State, State1),
	merge_wellbeing(State1, WellbeingDelta, NewState).
	
merge_phase_state_properties([], _, State, State).

merge_phase_state_properties([Property | Rest], StateDeltas, State, NewState) :-
	Option =.. [Property, PropertyValue],
	(option(Option, StateDeltas) ->
	    (is_list(PropertyValue) ->
			% Duplicates are removed
			acc_state(State, Property, PropertyValue, ca_support:agency_state_sorter, State1)
			;
			put_state(State, Property, PropertyValue, State1)
	    )
		;
		State1 = State),
	merge_phase_state_properties(Rest, StateDeltas, State1, NewState).


merge_wellbeing(State, WellbeingDelta, NewState) :-
	log(debug, dynamic_ca, "Merge wellbeing deltas ~p into state", [WellbeingDelta]),
	get_state(State, wellbeing, Wellbeing),
	apply_wellbeing_delta(Wellbeing, WellbeingDelta, Wellbeing1),
	put_state(State, wellbeing, Wellbeing1, NewState).

apply_wellbeing_delta(Wellbeing, WellbeingDelta, NewWellbeing) :-
	Fullness is max(Wellbeing.fullness + WellbeingDelta.fullness, 0),
	Integrity is max(Wellbeing.integrity + WellbeingDelta.integrity, 0),
	Engagement is max(Wellbeing.engagement + WellbeingDelta.engagement, 0),
	NewWellbeing = wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement}.

empty_prediction(CA, Prediction) :-
    Prediction = prediction{origin:unknown, kind:unknown, value:unknown, confidence:0, weight:0, by:CA}.

% Whether this is an empty prediction
is_empty_prediction(Prediction) :-
	prediction{origin:unknown, kind:unknown, value:unknown} :< Prediction.

% A prediction is about an experience if they are of the same kind and about the same object
prediction_about_experience(Prediction, State, Experience) :-
    prediction{origin:Origin, kind:Kind} :< Prediction,
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{origin:Origin, kind:Kind} :< Experience.

% Prediction = prediction{origin:object{type:sensor, id:SensorName}, kind:SenseName, value:Value, weight:Weight, confidence:Confidence, by: CA} - Confidence is between 0.0 and 1.0
% Experience = experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}
% PredictionError = prediction_error{prediction: Prediction, actual_value:Value, confidence:Confidence, by: CA}
% No state change for the moment
prediction_handled(Prediction, State, State1) :-
	is_empty_prediction(Prediction),
    log(info, ca_support, "~@ is handling empty prediction", [self]),
	!,
	get_state(State, experiences, Experiences),
	findall(PredictionError, 
		    (member(Experience, Experiences), prediction_error_from_experience(Experience, Prediction, PredictionError)), 
		    PredictionErrors),
	update_wellbeing(sending_prediction_errors(PredictionErrors), State, State1),
	prediction_errors_sent(PredictionErrors, Prediction.by).

prediction_handled(Prediction, State, State1) :-
    log(info, ca_support, "~@ is handling prediction ~p", [self, Prediction]),
	self(CA),
	prediction_about_experience(Prediction, State, Experience),
    log(info, ca_support, "Prediction ~p is about experience ~p of ~@", [Prediction, Experience, self]),
	!,
	(same_experience_value(Experience.value, Prediction.value) ->
		true
		;
		PredictionError = prediction_error{prediction:Prediction, actual_value:Experience.value, confidence:Experience.confidence, by:CA},
		update_wellbeing(sending_prediction_errors([PredictionError]), State, State1),
		prediction_error_sent(PredictionError, Prediction.by)
	).

% A prediction that is not understood causes a prediction error with unknown actual value and with zero confidence
% This is needed so a CA can check if a prediction is meaningless to the entire umwelt (i.e. they all respond with an unknown actual value)
prediction_handled(Prediction, State, State) :-
    self(CA),
    % Can't confirm or invalidate a prediction. Confidence is 0.
    PredictionError = prediction_error{prediction:Prediction, actual_value:unknown, confidence:0.0, by:CA},
	% No change to wellbeing since zero-value engagement
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

% Count from 1 to 3 and then many. Else counting fails.
simple_count(N, Count) :-
    N > 0,
    (N > 3 -> Count = many ; Count = N).
simple_count(N, 0) :-
	N =< 0.

inc_simple_count(many, many).
inc_simple_count(SimpleCount, Count) :-
    number(SimpleCount),
    Count1 is SimpleCount + 1,
    simple_count(Count1, Count).

dec_simple_count(0, 0).
dec_simple_count(many, 3).
dec_simple_count(SimpleCount, Count) :-
    number(SimpleCount),
    Count is SimpleCount - 1.

is_effector(CA) :-
	atomic_list_concat([effector | _], ':', CA).

is_sensor(CA) :-
	atomic_list_concat([sensor | _], ':', CA).

update_wellbeing(sending_prediction_errors(PredictionErrors), State, State1) :-
	length(PredictionErrors, N),
	% TODO - Get engagement points from settings
	Delta is N * 0.01,
	get_state(State, wellbeing, Wellbeing),
	UpdatedWellbeing = Wellbeing.add_engagement(Delta),
	put_state(State, wellbeing, UpdatedWellbeing, State1).

% Compare a status to an other status.
% e.g. relevant < planned, and executed > relevant
compare_activation_status(Order, Status, OtherStatus) :-
	Statuses = [relevant, planned, executed],
	nth1(Index, Statuses, Status),
	nth1(OtherIndex, Statuses, OtherStatus),
	compare(Order, Index, OtherIndex).
