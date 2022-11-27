% Templates of increasing complexity are generated on demand with fair coverage.
% template(type_signature(TS), max_static_rules(MSR), max_causal_rules(MCR), max_atoms_in_rule(MAR), frugality(Integer))
% Each template specifies a set of theories.
:- module(templates, [templates/3]).

:- use_module(domains).

templates(MinTypeSignature, Limit, Templates) :-
    findall(rated_template(Template, Frugality), (template(MinTypeSignature, Limit, Template), frugality(Template, Frugality)), Templates).

template(MinTypeSignature, Limit, Template) :-
    extended_type_signature(MinTypeSignature, Limit, ExtendedTypeSignature),
    complexity_limits(MaxStaticRules, MaxCausalRules, MaxAtomsPerRule),
    Template = template(ExtendedTypeSignature, max_static_rules(MaxStaticRules), max_causal_rules(MaxCausalRules), max_atoms_per_rule(MaxAtomsPerRule)).

extended_type_signature(type_signature(ObjectTypes, Objects, PredicateTypes), Limit, ExtendedTypeSignature) :-
    extended_object_types(ObjectTypes, Limit, ExtendedObjectTypes),
    extended_objects(Objects, ExtendedObjectTypes, Limit, ExtendedObjects),
    extended_predicate_types(PredicateTypes, ExtendedObjectTypes, Limit, ExtendedPredicateTypes),
    ExtendedTypeSignature = type_signature(ExtendedObjectTypes, ExtendedObjects, ExtendedPredicateTypes).

extended_object_types(ObjectTypes, Limit, ExtendedObjectTypes) :-
    between(0, Limit, Count),
    extended_object_types_(ObjectTypes, Count, ExtendedObjectTypes).

extended_object_types_(ObjectTypes, 0, ObjectTypes).
extended_object_types_(ObjectTypes, N, ExtendedObjectTypes) :-
    N > 0,
    new_object_type(ObjectTypes, NewObjectType),
    N1 is N - 1,
    extended_object_types_([NewObjectType | ObjectTypes], N1, ExtendedObjectTypes).

new_object_type(ObjectTypes, object_type(NewTypeName)) :-
    max_index(ObjectTypes, type_, 1, 0, Index),
    Index1 is Index + 1,
    atom_concat(type_, Index1, NewTypeName).

extended_objects(Objects, ObjectTypes, Limit, ExtendedObjects) :-
    between(0, Limit, Count),
    extended_objects_(Objects, ObjectTypes, Count, ExtendedObjects).

extended_objects_(Objects, _, 0, Objects).
extended_objects_(Objects, ObjectTypes, N, ExtendedObjects) :-
    N > 0,
    new_object(Objects, ObjectTypes, NewObject),
    N1 is N - 1,
    extended_objects_([NewObject | Objects], ObjectTypes, N1, ExtendedObjects).

new_object(Objects, ObjectTypes, object(ObjectType, ObjectName)) :-
    member(object_type(ObjectType), ObjectTypes),
    max_index(Objects, object_, 2, 0, Index),
    Index1 is Index + 1,
    atom_concat(object_, Index1, ObjectName).

extended_predicate_types(PredicateTypes, ObjectTypes, Limit, ExtendedPredicateTypes) :-
    between(0, Limit, Count),
    extended_predicate_types_(PredicateTypes, ObjectTypes, Count, ExtendedPredicateTypes).

extended_predicate_types_(PredicateTypes, _, 0, PredicateTypes).
extended_predicate_types_(PredicateTypes, ObjectTypes, N, ExtendedPredicateTypes) :-
    N > 0,
    new_predicate_type(PredicateTypes, ObjectTypes, NewPredicateType),
    N1 is N - 1,
    extended_predicate_types_([NewPredicateType, PredicateTypes], ObjectTypes, N1, ExtendedPredicateTypes).

new_predicate_type(PredicateTypes, ObjectTypes, predicate(PredicateName, ArgumentTypes)) :-
    make_argument_types(ObjectTypes, ArgumentTypes),
    max_index(PredicateTypes, pred_, 1, 0, Index),
    Index1 is Index + 1,
    atom_concat(pred_, Index1, PredicateName).

% [predicate(distance_from, [object_type(wall), value_type(proximity)]), 
%  predicate(on, [object_type(led), value_type(boolean)])]
make_argument_types(ObjectTypes, [ArgumentType1, ArgumentType2]) :-
    make_argument_type(ObjectTypes, ArgumentType1),
    bagof(DomainType, domain_is(DomainType, _), DomainTypes),
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


complexity_limits(MaxStaticRules, MaxCausalRules, MaxAtomsPerRule).

% TODO
frugality(_, 0).


% cd('sandbox/prototypes/apperception').
% [leds_observations, sequence, type_signature, templates].
% sequence(leds_observations, Sequence), min_type_signature(Sequence, TypeSignature), templates(TypeSignature, 4, Templates), length(Templates, Count).