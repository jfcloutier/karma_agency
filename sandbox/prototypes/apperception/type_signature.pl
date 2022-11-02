:- module(type_signature, [min_type_signature/3]).

min_type_signature(Sequence, Domains, TypeSignature) :-
    setof(object_type(ObjectType), Object^sequence_mentions_object(Sequence, object(ObjectType, Object)), ObjectTypes),
    setof(object(ObjectType, Object), ObjectType^sequence_mentions_object(Sequence, object(ObjectType, Object)), Objects),
    setof(PredicateType, Domains^sequence_implies_predicates_type(Sequence, Domains, PredicateType), PredicateTypes),
    TypeSignature =.. [type_signature, ObjectTypes, Objects, PredicateTypes].

sequence_mentions_object([State | _], Mention) :-
    state_mentions_object(State, Mention).
    
sequence_mentions_object([_ | OtherStates], Mention) :-
    sequence_mentions_object(OtherStates, Mention).

state_mentions_object([Observation | _], Mention) :-
    Observation =.. [_, _, Args, _],
    memberchk(Mention, Args).

sequence_implies_predicates_type([State | _], Domains, PredicateType) :-
    state_implies_predicate_type(State, Domains, PredicateType).

sequence_implies_predicates_type([_ | OtherStates], Domains, PredicateType) :-
    sequence_implies_predicates_type(OtherStates, Domains, PredicateType).

state_implies_predicate_type([Observation | _], Domains, PredicateType) :-
    Observation =.. [_, Predicate, Args, _],
    implied_predicate_type(Predicate, Args, Domains, PredicateType).

state_implies_predicate_type([_ | OtherObservations], Domains, PredicateType) :-
    state_implies_predicate_type(OtherObservations, Domains, PredicateType).

implied_predicate_type(Predicate, Args, Domains, PredicateType) :-
    types_from_args(Args, Domains, Types),
    PredicateType =.. [predicate, Predicate, Types].

types_from_args([], _, []).
types_from_args([Arg | OtherArgs], Domains, [Type | OtherTypes]) :-
    type_from_arg(Arg, Domains, Type),
    types_from_args(OtherArgs, Domains, OtherTypes).

type_from_arg(object(Type, _), _, object(Type)).
type_from_arg(Value, Domains, Domain) :-
    domain_of(Value, Domains, Domain).


domain_of(Value, [Domain | _], domain(Domain)) :-
    memberchk(Value, Domain), !.

domain_of(Value, [_ | OtherDomains], Domain) :-
    domain_of(Value, OtherDomains, Domain).

% cd('sandbox/prototypes/apperception').
% [leds_observations, sequence, type_signature].
% sequence(leds_observations, Sequence), min_type_signature(Sequence, [[true, false]], TypeSignature).