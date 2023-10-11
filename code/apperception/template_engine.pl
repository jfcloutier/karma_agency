% An engine for generating on demand theory templates of mostly increasing complexity.

% Each template specifies a search space for theories explaining a sequence of observations.
% Since it is not expected that there will be enough time to search each template for its implied theories,
% the sequence of templates generated on demand is randomized, but favoring simple templates first.

%% (vocabulary extension) Limits --->* SignatureExtensionTuple --->* Template (ExtendedSignature + implied MaxTheoryComplexity) --->* Theory
%%
%% Limits impose max object types, max objects and max predicate types on tuples tuple(NumObjectTypes, NumObjects, NumPredicateTypes).
%% Produce all tuples, offered in semi-random order, favoring frugality.
%% Limit the number of templates generated per tuple so that any one tuple does not hog template generation
%% For each tuple, generate templates.


/*
[load].
[code(logger), tests(apperception/leds_observations), tests(apperception/eca_observations), apperception(sequence), apperception(type_signature), apperception(domains), apperception(template_engine)].
sequence(eca_observations, Sequence), 
min_type_signature(Sequence, MinTypeSignature), 
predicates_observed_to_vary(MinTypeSignature, Sequence, VaryingPredicateNames),
MaxSignatureExtension = max_extension{max_object_types:1, max_objects:1, max_predicate_types:2},
create_theory_template_engine(MinTypeSignature, VaryingPredicateName, MaxSignatureExtension, TheoryTemplateEngine),
engine_next(TheoryTemplateEngine, Template1),
engine_next(TheoryTemplateEngine, Template2),
engine_destroy(TheoryTemplateEngine).
*/

:- module(template_engine, [create_theory_template_engine/4]).

:- use_module(code(logger)).
:- use_module(apperception(type_signature)).
:- use_module(apperception(domains)).

%% Create an engine that produces theory templates on request
create_theory_template_engine(MinTypeSignature, VaryingPredicateNames, MaxSignatureExtension, TheoryTemplateEngine) :-
    log(info, template_engine, 'Creating template engine'),
    engine_create(Template, theory_template(MinTypeSignature, VaryingPredicateNames, MaxSignatureExtension, Template), TheoryTemplateEngine).

%% For testing
% theory_template(_, _, Template) :-
%     between(30, 50, N),
%     Template = template{limits:limits{max_elements:N,max_causal_rules:1,max_static_rules:1, max_theory_time:300}, varying_predicate_names:[on],
%                         type_signature:type_signature{object_types:[object_type(led)],objects:[object(led,object_1),object(led,b),object(led,a)],predicate_types:[predicate(on,[object_type(led),value_type(boolean)]),predicate(pred_1,[object_type(led),object_type(led)])],typed_variables:[variables(led,3)]},
%                         min_type_signature: type_signature{object_types:[object_type(led)], objects:[object(led, a), object(led, b)], predicate_types:[predicate(on, [object_type(led), value_type(boolean)])]}}.

theory_template(MinTypeSignature, VaryingPredicateNames, MaxSignatureExtension, Template) :-
    scramble_signature(MinTypeSignature, ScrambledMinTypeSignature),
    % generated
    signature_extension_tuple(MaxSignatureExtension, SignatureExtensionTuple),
    allow_max_templates(MinTypeSignature, SignatureExtensionTuple, Max),
    % generated
    extended_type_signature(ScrambledMinTypeSignature, SignatureExtensionTuple, ExtendedTypeSignature),
    % implied
    theory_complexity_bounds(ExtendedTypeSignature, TheoryLimits),
    log(info, template_engine, 'Template created with extension ~p and type signature ~p', [SignatureExtensionTuple, ExtendedTypeSignature]),
    Template = template{type_signature:ExtendedTypeSignature, min_type_signature:MinTypeSignature, varying_predicate_names:VaryingPredicateNames,
                        limits:TheoryLimits, tuple:SignatureExtensionTuple, max_tuple_templates: Max}.

allow_max_templates(MinTypeSignature, Tuple, Max) :-
    tuple(NumObjectTypes, NumObjects, NumPredicateTypes) = Tuple,
    length(MinTypeSignature.object_types, MinObjectTypes),
    length(MinTypeSignature.objects,MinObjects),
    length(MinTypeSignature.predicate_types, MinPredicates),
    ObjectTypesCount is MinObjectTypes + NumObjectTypes,
    ObjectsCount is MinObjects + NumObjects,
    PredicatesCount is MinPredicates + NumPredicateTypes,
    Max is ObjectTypesCount * ObjectsCount * PredicatesCount,
    log(warn, template_engine, 'Max ~p templates allowed for tuple ~p', [Max, Tuple]).

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
    length(TypeSignature.objects, ObjectsCount),
    length(TypeSignature.predicate_types, PredicateTypesCount),
    % Max number of rules per theory - there might be no static rule but there must always be at least one causal rule
    UpperMaxRules is round(PredicateTypesCount * 1.5),
    between(1, UpperMaxRules, MaxCausalRules),
    between(0, UpperMaxRules, MaxStaticRules),
    % The maximum number of predicates in the rule body of a theory (different from Evans' paper where it is the number of symbols in a rule)
    MaxElements is ObjectsCount * PredicateTypesCount,
    % Maximum number of seconds spent searching for theories in the template
    MaxTheoryTime is round(MaxElements ** 1.5),
    % MaxTheoryTime is MaxElements * 2, % * 0.5,
    Limits = limits{max_causal_rules: MaxCausalRules, max_static_rules: MaxStaticRules, max_elements: MaxElements, max_theory_time: MaxTheoryTime},
    log(note, template_engine, 'Template limits ~p', [Limits]).
