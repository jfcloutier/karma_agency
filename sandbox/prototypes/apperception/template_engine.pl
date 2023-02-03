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
[logger, leds_observations, sequence, type_signature, domains, global, template_engine].
sequence(leds_observations, Sequence), 
min_type_signature(Sequence, MinTypeSignature), 
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:2},
create_theory_template_engine(MinTypeSignature, MaxSignatureExtension, TheoryTemplateEngine),
engine_next(TheoryTemplateEngine, Template1),
engine_next(TheoryTemplateEngine, Template2),
engine_destroy(TheoryTemplateEngine).
*/

:- module(template_engine, [create_theory_template_engine/3]).

:- use_module(logger).
:- use_module(global).
:- use_module(type_signature).
:- use_module(domains).

%% Create an engine that produces theory templates on request
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
    log(info, template_engine, 'MAX ~p TEMPLATES ALLOWED', [Max]).

increment_template_count() :-
    get_global(apperception, template_engine/template_count, Count),
    Inc is Count + 1,
    set_global(apperception, template_engine/template_count, Inc),
    log(info, template_engine, 'COUNT IS NOW ~p', [Inc]).

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
    log(info, template_engine, '****TUPLE ~p', [Tuple]).

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

theory_complexity_bounds(TypeSignature, Limits) :-
    length(TypeSignature.object_types, ObjectTypesCount),
    length(TypeSignature.objects, ObjectsCount),
    length(TypeSignature.predicate_types, PredicateTypesCount),
    Count is ObjectTypesCount + ObjectsCount + PredicateTypesCount,
    MaxRules is Count * 2,
    MaxElements is 10 * Count * Count,
    MaxTheoryTime is MaxElements * 0.01,
    Limits = limits{max_rules: MaxRules, max_elements: MaxElements, max_theory_time: MaxTheoryTime}.
