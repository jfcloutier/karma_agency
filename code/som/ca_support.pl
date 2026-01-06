/*
Cognition Actor support library.
*/

:- module(ca_support, [from_parent/2, wellbeing_transfered/3, well_enough/1, object_hash/2, value_hash/2, atomic_list_hash/2, average_confidence/2, merge_wellbeing/3]).

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

same_experience_value(Value, Value) :-
    Value \== unknown.

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
    hash_atom(Sha, Hash).

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



