:- module(type_signature, [min_type_signature/2]).

:- use_module(domains).

min_type_signature(Sequence, type_signature(ObjectTypes, Objects, PredicateTypes)) :-
    setof(object_type(ObjectType), Object^sequence_mentions_object(Sequence, object(ObjectType, Object)), ObjectTypes),
    setof(object(ObjectType, Object), ObjectType^sequence_mentions_object(Sequence, object(ObjectType, Object)), Objects),
    setof(PredicateType, sequence_implies_predicate_types(Sequence,  PredicateType), PredicateTypes).

sequence_mentions_object([State | _], Mention) :-
    state_mentions_object(State, Mention).
    
sequence_mentions_object([_ | OtherStates], Mention) :-
    sequence_mentions_object(OtherStates, Mention).

state_mentions_object([Observation | _], Mention) :-
    Observation =.. [_, _, Args, _],
    memberchk(Mention, Args).

sequence_implies_predicate_types([State | _], PredicateType) :-
    state_implies_predicate_type(State, PredicateType).

sequence_implies_predicate_types([_ | OtherStates], PredicateType) :-
    sequence_implies_predicate_types(OtherStates, PredicateType).

state_implies_predicate_type([Observation | _], PredicateType) :-
    Observation =.. [_, Predicate, Args, _],
    implied_predicate_type(Predicate, Args, PredicateType).

state_implies_predicate_type([_ | OtherObservations], PredicateType) :-
    state_implies_predicate_type(OtherObservations, PredicateType).

implied_predicate_type(Predicate, Args, PredicateType) :-
    types_from_args(Args, Types),
    PredicateType =.. [predicate, Predicate, Types].

types_from_args([], []).
types_from_args([Arg | OtherArgs], [Type | OtherTypes]) :-
    type_from_arg(Arg, Type),
    types_from_args(OtherArgs, OtherTypes).

type_from_arg(object(Type, _), object_type(Type)) :- !.
type_from_arg(Value, Domain) :-
    domain_of(Value, Domain).


domain_of(Value, value_type(DomainName)) :-
    domain_is(DomainName, DomainValues),
    memberchk(Value, DomainValues), !.

% cd('sandbox/prototypes/apperception').
% [leds_observations, domains, sequence, type_signature].
% sequence(leds_observations, Sequence), min_type_signature(Sequence, TypeSignature).