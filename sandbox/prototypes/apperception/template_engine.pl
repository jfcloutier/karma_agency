% An engine for generating on demand theory templates of mostly increasing complexity.

% Each template specifies a search space theories explaining a sequence of observations.
% Since it is not expected that there will be enough time to search each template for its implied theories,
% the sequence of templates generated on demand is randomized, but favoring simple templates first.

%% Limits --->* SignatureExtensionTuple --->* Template (ExtendedSignature + implied MaxTheoryComplexity) --->* Theory
%%
%% Limits impose max object types, max objects and max predicate types on tuples tuple(NumObjectTypes, NumObjects, NumPredicateTypes).
%% Produce all tuples, offered in semi-random order, favoring frugality.
%% limit the number of templates generated per tuple so that any one tuple does not hog template generation
%% For each tuple, generate templates.


/*
cd('sandbox/prototypes/apperception').
[leds_observations, sequence, type_signature, domains, global, template_engine].
sequence(leds_observations, Sequence), 
min_type_signature(Sequence, MinTypeSignature), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:2},
create_theory_template_engine(MinTypeSignature, MaxSignatureExtension, TheoryTemplateEngine),
engine_next(TheoryTemplateEngine, Template1),
engine_next(TheoryTemplateEngine, Template2),
engine_destroy(TheoryTemplateEngine).
*/

:- module(template_engine, [create_theory_template_engine/3]).

:- use_module(global).
:- use_module(library(chr)).
:- use_module(domains).

create_theory_template_engine(MinTypeSignature, MaxSignatureExtension, TheoryTemplateEngine) :-
    init_template_counter(),
    engine_create(Template, theory_template(MinTypeSignature, MaxSignatureExtension, Template), TheoryTemplateEngine).

% Add a value to the global variable to keep count of templates produced vs maximum allowed
init_template_counter() :-
    set_global(apperception, template_engine/max_templates, 0),
    set_global(apperception, template_engine/template_count, 0).

theory_template(MinTypeSignature, MaxSignatureExtension, Template) :-
    scramble_signature(MinTypeSignature, ScrambledMinTypeSignature),
    % generated
    signature_extension_tuple(MaxSignatureExtension, SignatureExtensionTuple),
    reset_template_counter(MinTypeSignature, SignatureExtensionTuple),
    % generated
    extended_type_signature(ScrambledMinTypeSignature, SignatureExtensionTuple, ExtendedTypeSignature),
    % implied
    theory_complexity_bounds(ExtendedTypeSignature, TheoryLimits),
    Template = template{type_signature:ExtendedTypeSignature, limits:TheoryLimits},
    increment_template_count().

reset_template_counter(MinTypeSignature, SignatureExtensionTuple) :-
    allow_max_templates(MinTypeSignature, SignatureExtensionTuple, Max),
    set_global(apperception, template_engine/max_templates, Max),
    set_global(apperception, template_engine/template_count, 0),
    format('MAX ~p TEMPLATES ALLOWED~n', [Max]).

max_template_count_reached() :-
    get_global(apperception, template_engine/max_templates, Max),
    get_global(apperception, template_engine/template_count, Max),
    format('MAX ~p TEMPLATES EXCEEDED!~n', [Max]).

increment_template_count() :-
    get_global(apperception, template_engine/template_count, Count),
    Inc is Count + 1,
    set_global(apperception, template_engine/template_count, Inc),
    format('COUNT IS NOW ~p~n', [Inc]).

allow_max_templates(TypeSignature,
                    tuple(NumObjectTypes, NumObjects, NumPredicateTypes), 
                    Max) :-
    length(TypeSignature.object_types, MinObjectTypes),
    length(TypeSignature.objects,MinObjects),
    length(TypeSignature.predicate_types, MinPredicates),
    ObjectTypesCount is MinObjectTypes + NumObjectTypes,
    ObjectsCount is MinObjects + NumObjects,
    PredicatesCount is MinPredicates + NumPredicateTypes,
    Max is ObjectTypesCount * ObjectsCount * PredicatesCount.

scramble_signature(TypeSignature, ScrambledTypeSignature) :-
    random_permutation(TypeSignature.object_types, ScrambledObjectTypes),
    random_permutation(TypeSignature.objects, ScrambledObjects),
    random_permutation(TypeSignature.predicate_types, ScrambledPredicateTypes),
    !,
    ScrambledTypeSignature = type_signature{object_types:ScrambledObjectTypes, objects:ScrambledObjects, predicate_types:ScrambledPredicateTypes}.

signature_extension_tuple(MaxSignatureExtension, Tuple) :-
    findall(RatedTuple, rated_tuple(MaxSignatureExtension, RatedTuple), RatedTuples),
    sort(1, @=<, RatedTuples, Tuples),
    !,
    member(indexed_tuple(_, NumObjectTypes, NumObjects, NumPredicateTypes), Tuples),
    Tuple = tuple(NumObjectTypes, NumObjects, NumPredicateTypes),
    format('****TUPLE ~p~n', [Tuple]).

rated_tuple(max_extension{max_object_types:MaxObjectTypes, max_objects:MaxObjects, max_predicate_types:MaxPredicateTypes}, IndexedTuple) :-
    between(0, MaxObjectTypes, NumObjectTypes),
    between(0, MaxObjects, NumObjects),
    between(0, MaxPredicateTypes, NumPredicateTypes),
    random_order(NumObjectTypes, NumObjects, NumPredicateTypes, RandomOrder),
    IndexedTuple = indexed_tuple(RandomOrder, NumObjectTypes, NumObjects, NumPredicateTypes).

random_order(NumObjectTypes, NumObjects, NumPredicateTypes, RandomOrder) :-
    random_between(1, 2, Random),
    !,
    Frugality is NumObjectTypes + NumObjects + NumPredicateTypes,
    RandomOrder is Random * Frugality.

extended_type_signature(TypeSignature,
                        tuple(NumObjectTypes, NumObjects, NumPredicateTypes),
                        ExtendedTypeSignature) :-
    extended_object_types(TypeSignature.object_types, NumObjectTypes, ExtendedObjectTypes),
    extended_objects(TypeSignature.objects, ExtendedObjectTypes, NumObjects, ExtendedObjects),
    extended_predicate_types(TypeSignature.predicate_types, ExtendedObjectTypes, NumPredicateTypes, ExtendedPredicateTypes),
    typed_variables(ExtendedObjects, ExtendedObjectTypes, TypedVariables),
    ExtendedTypeSignature = type_signature{object_types:ExtendedObjectTypes, objects:ExtendedObjects, predicate_types:ExtendedPredicateTypes, typed_variables:TypedVariables}.

extended_object_types(ObjectTypes, N, ExtendedObjectTypes) :-
    max_template_count_reached() -> fail ; extended_object_types_(ObjectTypes, N, ExtendedObjectTypes).

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
    max_template_count_reached() -> fail ; extended_objects_(Objects, ObjectTypes, N, ExtendedObjects).

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
    max_template_count_reached() -> fail ; extended_predicate_types_(PredicateTypes, ObjectTypes, N, ExtendedPredicateTypes).

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

% [variables(led, 2), variables(object1, 1)]
typed_variables(Objects, ObjectTypes, TypedVariables) :-
    typed_variables_(Objects, ObjectTypes, [], TypedVariables).

typed_variables_(_, [], TypedVariables, TypedVariables).
typed_variables_(Objects, [object_type(ObjectType) | OtherObjectTypes], Acc, TypedVariables) :-
    findall(ObjectName, member(object(ObjectType, ObjectName), Objects), ObjectNames),
    length(ObjectNames, Count),
    typed_variables_(Objects, OtherObjectTypes, [variables(ObjectType, Count) | Acc], TypedVariables).

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

theory_complexity_bounds(TypeSignature, Limits) :-
    length(TypeSignature.object_types, ObjectTypesCount),
    length(TypeSignature.objects, ObjectsCount),
    length(TypeSignature.predicate_types, PredicateTypesCount),
    Count is ObjectTypesCount + ObjectsCount + PredicateTypesCount,
    MaxRules is Count * 2,
    MaxElements is Count * Count,
    Limits = limits{max_rules:MaxRules, max_elements:MaxElements}.
