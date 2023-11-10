% An engine for generating on demand theory templates of mostly increasing complexity.

% Each template specifies a search space for theories explaining a sequence of observations.
% Since it is not expected that there will be enough time to search each template for its implied theories,
% the sequence of templates generated on demand is randomized, but favoring simple templates first.

%% (vocabulary extension) Limits --->* SignatureExtensionRegion --->* Template (ExtendedSignature + implied MaxTheoryComplexity) --->* Theory
%%
%% Limits impose max object types, max objects and max predicate types on regions region(NumObjectTypes, NumObjects, NumPredicateTypes).
%% Produce all regions, offered in semi-random order, favoring frugality.
%% Limit the number of templates generated per region so that any one region does not hog template generation
%% For each region, generate templates.


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
:- use_module(library(uuid)).

%% Create an engine that produces theory templates on request
create_theory_template_engine(MinTypeSignature, VaryingPredicateNames, MaxSignatureExtension, TheoryTemplateEngine) :-
    log(info, template_engine, 'Creating template engine'),
    engine_create(Template, theory_template(MinTypeSignature, VaryingPredicateNames, MaxSignatureExtension, Template), TheoryTemplateEngine).

%% For testing
% theory_template(_, _, _, Template) :-
%     between(30, 50, N),
%     Template = template{id: 'abc1234', limits:limits{max_elements:N,max_causal_rules:1,max_static_rules:1, max_search_time:300}, varying_predicate_names:[on],
%                         type_signature:type_signature{object_types:[object_type(led)],objects:[object(led,object_1),object(led,b),object(led,a)],predicate_types:[predicate(on,[object_type(led),value_type(boolean)]),predicate(pred_1,[object_type(led),object_type(led)])],typed_variables:[variables(led,3)]},
%                         min_type_signature: type_signature{object_types:[object_type(led)], objects:[object(led, a), object(led, b)], predicate_types:[predicate(on, [object_type(led), value_type(boolean)])]}}.
   
%% For testing


%  theory_template(_, _, MaxSignatureExtension, Template) :-
%      TypeSignature = type_signature{object_types:[object_type(cell)],
%                                                       objects:[object(cell,c1),object(cell,c2),object(cell,c3),object(cell,c4),object(cell,c5),object(cell,c6),object(cell,c7),object(cell,c8),object(cell,c9),object(cell,c10),object(cell,c11)],
%                                                       predicate_types:[predicate(on,[object_type(cell),value_type(boolean)]),predicate(right_of,[object_type(cell),object_type(cell)])],
%                                                       typed_variables:[variables(cell,3)]},
%     signature_extension_region(MaxSignatureExtension, SignatureExtensionRegion),
%     allow_max_templates(TypeSignature, SignatureExtensionRegion, Max),
%     Template = template{id: 'abc1234', 
%             type_signature: TypeSignature,
%             min_type_signature: TypeSignature,
%             varying_predicate_names:[on],
%             limits:limits{max_elements:5,max_causal_rules:2,max_static_rules:0, max_search_time:30}, 
%             region: SignatureExtensionRegion, 
%             max_region_templates: Max}.


theory_template(MinTypeSignature, VaryingPredicateNames, MaxSignatureExtension, Template) :-
    scramble_signature(MinTypeSignature, ScrambledMinTypeSignature),
    % generated
    signature_extension_region(MaxSignatureExtension, SignatureExtensionRegion),
    allow_max_templates(MinTypeSignature, SignatureExtensionRegion, Max),
    % generated
    extended_type_signature(ScrambledMinTypeSignature, SignatureExtensionRegion, ExtendedTypeSignature),
    % implied
    theory_complexity_bounds(ExtendedTypeSignature, TheoryLimits),
    log(info, template_engine, 'Template created with extension ~p and type signature ~p', [SignatureExtensionRegion, ExtendedTypeSignature]),
    uuid(Id),
    log(note, template_engine, 'Template ~p has ~p', [Id, TheoryLimits]),
    Template = template{id:Id, type_signature:ExtendedTypeSignature, min_type_signature:MinTypeSignature, varying_predicate_names:VaryingPredicateNames,
                        limits:TheoryLimits, region:SignatureExtensionRegion, max_region_templates: Max}.

allow_max_templates(MinTypeSignature, Region, Max) :-
    region(NumObjectTypes, NumObjects, NumPredicateTypes) = Region,
    length(MinTypeSignature.object_types, MinObjectTypes),
    length(MinTypeSignature.objects,MinObjects),
    length(MinTypeSignature.predicate_types, MinPredicates),
    ObjectTypesCount is MinObjectTypes + NumObjectTypes,
    ObjectsCount is MinObjects + NumObjects,
    PredicatesCount is MinPredicates + NumPredicateTypes,
    Max is ObjectTypesCount * ObjectsCount * PredicatesCount,
    log(warn, template_engine, 'Max ~p templates allowed for region ~p', [Max, Region]).

scramble_signature(TypeSignature, ScrambledTypeSignature) :-
    random_permutation(TypeSignature.object_types, ScrambledObjectTypes),
    random_permutation(TypeSignature.objects, ScrambledObjects),
    random_permutation(TypeSignature.predicate_types, ScrambledPredicateTypes),
    !,
    ScrambledTypeSignature = type_signature{object_types:ScrambledObjectTypes, objects:ScrambledObjects, predicate_types:ScrambledPredicateTypes}.

signature_extension_region(MaxSignatureExtension, Region) :-
    findall(RatedRegion, rated_region(MaxSignatureExtension, RatedRegion), RatedRegions),
    sort(1, @=<, RatedRegions, Regions),
    !,
    member(indexed_region(_, NumObjectTypes, NumObjects, NumPredicateTypes), Regions),
    Region = region(NumObjectTypes, NumObjects, NumPredicateTypes),
    log(info, template_engine, '****REGION ~p', [Region]).

rated_region(max_extension{max_object_types:MaxObjectTypes, max_objects:MaxObjects, max_predicate_types:MaxPredicateTypes}, IndexedRegion) :-
    between(0, MaxObjectTypes, NumObjectTypes),
    between(0, MaxObjects, NumObjects),
    between(0, MaxPredicateTypes, NumPredicateTypes),
    random_order(NumObjectTypes, NumObjects, NumPredicateTypes, RandomOrder),
    IndexedRegion = indexed_region(RandomOrder, NumObjectTypes, NumObjects, NumPredicateTypes).

% Some randomness in the oredring but keep a progression from simpler
random_order(NumObjectTypes, NumObjects, NumPredicateTypes, RandomOrder) :-
    random_between(1, 3, Random),
    !,
    Frugality is NumObjectTypes + NumObjects + NumPredicateTypes,
    RandomOrder is Random + Frugality.

% Come up with reasonable limits on the size, complexity and search time for theories in this template
theory_complexity_bounds(TypeSignature, Limits) :-
    length(TypeSignature.objects, ObjectsCount),
    length(TypeSignature.predicate_types, PredicateTypesCount),
    % Let's say that the max number of rules is twice the number of predicate types
    UpperMaxRules is round(PredicateTypesCount * 2),
    % Max number of rules per theory - there might be no static rule but there must always be at least one causal rule
    between(1, UpperMaxRules, MaxCausalRules),
    between(0, UpperMaxRules, MaxStaticRules),
    % The maximum number of predicates in the rule body of a theory (different from Evans' paper where it is the number of symbols in a rule)
    % is a function of the number of objects and predicate types
    MaxElements is (ObjectsCount / PredicateTypesCount) + PredicateTypesCount,
    % Maximum number of seconds spent searching for theories in the template grows geometrically with the max complexity of rules
    % MaxSearchTime is round(2 * (MaxElements + (2.2 ** MaxElements))),
    MaxSearchTime is round(MaxElements + (2 ** MaxElements)),
    round(MaxElements, RoundedMaxElements),
    Limits = limits{max_causal_rules: MaxCausalRules, max_static_rules: MaxStaticRules, max_elements: RoundedMaxElements, max_search_time: MaxSearchTime}.
  
