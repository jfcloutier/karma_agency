% Templates of mostly increasing complexity are generated on demand:
%   template(type_signature(TS), theory_bounds(MaxStaticRules, MaxCausalRules, MaxAtoms))
% Each template specifies a set of theories.

%% Limits --->* SignatureExtensionTuple --->* Template (ExtendedSignature + implied MaxTheoryComplexity) --->* Theory
%%
%% Limits impose max object types, max objects and max predicate types on tuples tuple(NumObjectTypes, NumObjects, NumPredicateTypes).
%% Produce all tuples, offered in semi-random order, favoring frugality.
%% limit the number of templates generated per tuple so that any one tuple does not hog template generation
%% For each tuple, generate templates.

%% Since it is not expected that there will be enough time to search each template for its implied theories,
%% the sequence of templates generated on demand is randomized, but favoring simple templates first.


:- module(templates, [theory_template/3]).

:- use_module(library(chr)).
:- use_module(domains).

:- dynamic template_count/1, max_templates/1.

theory_template(MinTypeSignature, SignatureExtensionLimits, Template) :-
    % implied
    expand_limits(SignatureExtensionLimits, ExpandedLimits),
    scramble_signature(MinTypeSignature, ScrambledMinTypeSignature),
    % generated
    signature_extension_tuple(ExpandedLimits, SignatureExtensionTuple),
    reset_max_templates(MinTypeSignature, SignatureExtensionTuple),
    % generated
    extended_type_signature(ScrambledMinTypeSignature, SignatureExtensionTuple, ExtendedTypeSignature),
    % implied
    theory_complexity_bounds(ExtendedTypeSignature, TheoryComplexityBounds),
    Template = template(ExtendedTypeSignature, TheoryComplexityBounds),
    increment_template_count().

max_templates_reached() :-
    template_count(Count),
    max_templates(Max),
    Count == Max,
    format('MAX ~p TEMPLATES EXCEEDED!~n', [Max]).

reset_max_templates(MinTypeSignature, SignatureExtensionTuple) :-
    allow_max_templates(MinTypeSignature, SignatureExtensionTuple, Max),
    retractall(max_templates(_)),
    assert(max_templates(Max)),
    retractall(template_count(_)),
    assert(template_count(0)),
    format('MAX ~p TEMPLATES ALLOWED!~n', [Max]).

increment_template_count() :-
    template_count(Count),
    Inc is Count + 1,
    retractall(template_count(_)),
    assert(template_count(Inc)),
    format('COUNT IS NOW ~p~n', [Inc]).

allow_max_templates(type_signature(ObjectTypes, Objects, Predicates), 
                    tuple(NumObjectTypes, NumObjects, NumPredicateTypes), 
                    Max) :-
    length(ObjectTypes, MinObjectTypes),
    length(Objects,MinObjects),
    length(Predicates, MinPredicates),
    ObjectTypesCount is MinObjectTypes + NumObjectTypes,
    ObjectsCount is MinObjects + NumObjects,
    PredicatesCount is MinPredicates + NumPredicateTypes,
    Max is ObjectTypesCount * ObjectsCount * PredicatesCount.

expand_limits(N, limits(num_object_types(N), num_objects(N), num_predicate_types(N))) :-
    integer(N), !.
expand_limits(Limits, Limits).

scramble_signature(type_signature(ObjectTypes, Objects, PredicateTypes), type_signature(ScrambledObjectTypes, ScrambledObjects, ScrambledPredicateTypes)) :-
    random_permutation(ObjectTypes, ScrambledObjectTypes),
    random_permutation(Objects, ScrambledObjects),
    random_permutation(PredicateTypes, ScrambledPredicateTypes),
    !.

signature_extension_tuple(Limits, Tuple) :-
    findall(RatedTuple, rated_tuple(Limits, RatedTuple), RatedTuples),
    sort(1, @=<, RatedTuples, Tuples),
    !,
    member(indexed_tuple(_, NumObjectTypes, NumObjects, NumPredicateTypes), Tuples),
    Tuple = tuple(NumObjectTypes, NumObjects, NumPredicateTypes),
    format('****TUPLE ~p~n', [Tuple]).


rated_tuple(limits(num_object_types(MaxObjectTypes), num_objects(MaxObjects), num_predicate_types(MaxPredicateTypes)), 
            indexed_tuple(RandomOrder, NumObjectTypes, NumObjects, NumPredicateTypes)) :-
    between(0, MaxObjectTypes, NumObjectTypes),
    between(0, MaxObjects, NumObjects),
    between(0, MaxPredicateTypes, NumPredicateTypes),
    random_order(NumObjectTypes, NumObjects, NumPredicateTypes, RandomOrder).


random_order(NumObjectTypes, NumObjects, NumPredicateTypes, RandomOrder) :-
    random_between(1, 2, Random),
    !,
    Frugality is NumObjectTypes + NumObjects + NumPredicateTypes,
    RandomOrder is Random * Frugality.

extended_type_signature(type_signature(ObjectTypes, Objects, PredicateTypes), 
                        tuple(NumObjectTypes, NumObjects, NumPredicateTypes), 
                        type_signature(ExtendedObjectTypes, ExtendedObjects, ExtendedPredicateTypes)) :-
    extended_object_types(ObjectTypes, NumObjectTypes, ExtendedObjectTypes),
    extended_objects(Objects, ExtendedObjectTypes, NumObjects, ExtendedObjects),
    extended_predicate_types(PredicateTypes, ExtendedObjectTypes, NumPredicateTypes, ExtendedPredicateTypes).

extended_object_types(ObjectTypes, N, ExtendedObjectTypes) :-
    max_templates_reached() -> fail ; extended_object_types_(ObjectTypes, N, ExtendedObjectTypes).

extended_object_types_(ObjectTypes, 0, ObjectTypes).
extended_object_types_(ObjectTypes, N, ExtendedObjectTypes) :-
    N > 0,
    new_object_type(ObjectTypes, NewObjectType),
    N1 is N - 1,
    extended_object_types([NewObjectType | ObjectTypes], N1, ExtendedObjectTypes).

new_object_type(ObjectTypes, object_type(NewTypeName)) :-
    max_index(ObjectTypes, type_, 1, 0, Index),
    Index1 is Index + 1,
    atom_concat(type_, Index1, NewTypeName).

extended_objects(Objects, ObjectTypes, N, ExtendedObjects) :-
    max_templates_reached() -> fail ; extended_objects_(Objects, ObjectTypes, N, ExtendedObjects).

extended_objects_(Objects, _, 0, Objects).
extended_objects_(Objects, ObjectTypes, N, ExtendedObjects) :-
    N > 0,
    new_object(Objects, ObjectTypes, NewObject),
    N1 is N - 1,
    extended_objects([NewObject | Objects], ObjectTypes, N1, ExtendedObjects).

new_object(Objects, ObjectTypes, object(ObjectType, ObjectName)) :-
    member(object_type(ObjectType), ObjectTypes),
    max_index(Objects, object_, 2, 0, Index),
    Index1 is Index + 1,
    atom_concat(object_, Index1, ObjectName).

extended_predicate_types(PredicateTypes, ObjectTypes, N, ExtendedPredicateTypes) :-
    max_templates_reached() -> fail ; extended_predicate_types_(PredicateTypes, ObjectTypes, N, ExtendedPredicateTypes).

extended_predicate_types_(PredicateTypes, _, 0, PredicateTypes).
extended_predicate_types_(PredicateTypes, ObjectTypes, N, ExtendedPredicateTypes) :-
    N > 0,
    new_predicate_type(PredicateTypes, ObjectTypes, NewPredicateType),
    N1 is N - 1,
    extended_predicate_types([NewPredicateType | PredicateTypes], ObjectTypes, N1, ExtendedPredicateTypes).

new_predicate_type(PredicateTypes, ObjectTypes, predicate(PredicateName, ArgumentTypes)) :-
    make_argument_types(ObjectTypes, ArgumentTypes),
    max_index(PredicateTypes, pred_, 1, 0, Index),
    Index1 is Index + 1,
    atom_concat(pred_, Index1, PredicateName).

% [predicate(distance_from, [object_type(wall), value_type(proximity)]), 
%  predicate(on, [object_type(led), value_type(boolean)])]
make_argument_types(ObjectTypes, [ArgumentType1, ArgumentType2]) :-
    bagof(DomainType, Domain^domain_is(DomainType, Domain), DomainTypes),
    make_argument_type(ObjectTypes, ArgumentType1),
    make_argument_type(ObjectTypes, DomainTypes, ArgumentType2).

make_argument_type(ObjectTypes, ObjectType) :-
    member(ObjectType, ObjectTypes).

make_argument_type(ObjectTypes, _, ObjectType) :-
    make_argument_type(ObjectTypes, ObjectType).
make_argument_type(_, DomainTypes, value_type(DomainType)) :-
    member(DomainType, DomainTypes).

max_index([], _, _, MaxIndex, MaxIndex).
max_index([Term | Others], Prefix, Position, Max, MaxIndex) :-
    Term =.. List,
    nth0(Position, List, IndexedAtom),
    atom_concat(Prefix, IndexAtom, IndexedAtom),
    atom_string(IndexAtom, IndexString),
    number_string(Index, IndexString),
    Max1 is max(Index, Max),
    !,
    max_index(Others, Prefix, Position, Max1, MaxIndex).

max_index([_ | Others], Prefix, Position, Max, MaxIndex) :-
    max_index(Others, Prefix, Position, Max, MaxIndex).

theory_complexity_bounds(type_signature(ObjectTypes, Objects, PredicateTypes), 
                         theory_bounds(max_static_rules(MaxStatic), max_causal_rules(MaxCausal), max_atoms(MaxAtoms))) :-
    length(ObjectTypes, ObjectTypesCount),
    length(Objects, ObjectsCount),
    length(PredicateTypes, PredicateTypesCount),
    Count is ObjectTypesCount + ObjectsCount + PredicateTypesCount,
    MaxStatic is Count * 2,
    MaxCausal is Count * 2,
    MaxAtoms is Count * Count.

% cd('sandbox/prototypes/apperception').
% [leds_observations, sequence, type_signature, domains, templates].
% sequence(leds_observations, Sequence), min_type_signature(Sequence, TypeSignature), theory_template(TypeSignature, limits(num_object_types(1), num_objects(1), num_predicate_types(2)), Template).
