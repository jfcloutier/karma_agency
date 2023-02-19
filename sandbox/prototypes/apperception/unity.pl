:- module(unity, [
        conceptual_unity/2, 
        spatial_unity/2, 
        static_unity/4, 
        breaks_static_constraints/3,
        too_many_relations/2,
        facts_repeat/2,
        factual_contradiction/2]).

:- use_module(logger).
:- use_module(global).
:- use_module(rules_engine).
:- use_module(type_signature).

% Check that the static constraints achiev conceptual unity over relation predicates.
% Property predicates are implicitly unified via domain exclusion.
conceptual_unity(StaticConstraints, TypeSignature) :-
    log(info, unity, 'Checking conceptual unity'),
    all_binary_predicate_names(TypeSignature, AllBinaryPredicateNames),
    \+ (member(BinaryPredicateName, AllBinaryPredicateNames), \+ static_constraints_cover(StaticConstraints, BinaryPredicateName)).

% Facts are spatially unified if all objects are inter-related.
spatial_unity([], _) :- fail, !.
spatial_unity(Facts, TypeSignature) :-
    log(info, unity, 'Checking spatial unity'),
    \+ unrelated_objects(Facts, TypeSignature).

% Verify that facts consistent with the static rules and constraints,
% i.e. compute the closure of the facts under the static rules
% without causing a contradiction or breaking a static constraint.
static_unity([], _, _, _) :- fail,!.
static_unity(Facts, StaticRules, StaticConstraints, TypeSignature) :-
    log(info, unity, 'Checking static unity'),
    get_global(apperception, uuid, Module),
    setup_call_cleanup(
            clear_facts_and_rules(Module, TypeSignature.predicate_types), 
            static_unity_(Facts, StaticRules, StaticConstraints, TypeSignature, Module),
            clear_facts_and_rules(Module, TypeSignature.predicate_types)
    ),!.

% The facts breaks static constraints given a type signature.
% Only one constraint need to be broken.
breaks_static_constraints(Facts, StaticConstraints, TypeSignature) :-
    member(StaticConstraint, StaticConstraints),
    broken_static_constraint(StaticConstraint, Facts, TypeSignature),
    log(info, unity, 'Broken static constraint ~p in facts ~p', [StaticConstraint, Facts]).

% Repeated property on the same object irrespective of domain value,
% Or repeated relation between identical pair of objects. 
facts_repeat(Condition, OtherCondition) :-
    Condition =.. [PredName, ObjectName, Arg],
    OtherCondition  =.. [PredName, ObjectName, OtherArg],
    (Arg == OtherArg; is_domain_value(_, Arg), is_domain_value(_, OtherArg)).

static_constraints_cover(StaticConstraints, PredicateName) :-
    member(StaticConstraint, StaticConstraints),
    static_constraint_about(StaticConstraint, PredicateName), !.

static_constraint_about(one_related(PredicateName), PredicateName).
static_constraint_about(one_relation(PredicateNames), PredicateName) :-
    memberchk(PredicateName, PredicateNames).


% Some pair of objects from the type signature aren't related, directly or indirectly, in the  initial conditions.
unrelated_objects(Facts, TypeSignature) :-
    object_name_pair(TypeSignature.objects, ObjectName1-ObjectName2),
    \+ related(ObjectName1, ObjectName2, TypeSignature, Facts),
    log(debug, unity, 'Unrelated objects ~p and ~p in facts ~p!', [ObjectName1, ObjectName2, Facts]).

% Some pair of objects from the type signature
object_name_pair(TypeSignatureObjects, ObjectName1-ObjectName2) :-
    select(object(_, ObjectName1), TypeSignatureObjects, RemainingSignatureObjects),
    member(object(_, ObjectName2), RemainingSignatureObjects).

% Two objects are directly related a set of facts
related(ObjectName1, ObjectName2, Facts) :-
    member(Fact, Facts),
    Fact =.. [_ | ObjectNames],
    memberchk(ObjectName1, ObjectNames),
    memberchk(ObjectName2, ObjectNames).

% Two objects are indirectly related in a set of facts
related(ObjectName1, ObjectName2, TypeSignature, Facts) :-
    select(Fact, Facts, OtherFacts),
    Fact =.. [PredicateName | ObjectNames],
    % It's a relation between two objects
    member(predicate(PredicateName, [object_type(_), object_type(_)]), TypeSignature.predicate_types),
    select(ObjectName1, ObjectNames, [OtherObjectName]),
    related(OtherObjectName, ObjectName2, OtherFacts).

static_unity_(Facts, StaticRules, StaticConstraints, TypeSignature, Module) :-
     clear_facts_and_rules(Module, TypeSignature.predicate_types),
     assert_facts(Module, Facts),
     assert_rules(Module, StaticRules),
     save_module(Module),
     apply_rules(Module, StaticRules, Facts),
     merge_consistent(Facts, Facts, AugmentedFacts),
     !,
    (  (length(Facts, L),
        length(AugmentedFacts, L)
       ) ->
        \+ breaks_static_constraints(AugmentedFacts, StaticConstraints, TypeSignature)
        ;
        static_unity_(AugmentedFacts, StaticRules, StaticConstraints, TypeSignature, Module)
    ).

% Merge removing duplicates. Fail if attempting to merge contradictory facts.    
merge_consistent(Facts, [], Facts).
merge_consistent(Facts, [Fact | OtherAnswers], MergedFacts) :-
    \+ (member(Condition, Facts), factual_contradiction(Condition, Fact)),
    (memberchk(Fact, Facts) ->
        merge_consistent(Facts, OtherAnswers, MergedFacts)
        ;
        merge_consistent([Fact | Facts], OtherAnswers, MergedFacts)
    ).

factual_contradiction(Condition, Fact) :-
    Condition =.. [PredicateName, ObjectName, Arg],
    Fact =.. [PredicateName, ObjectName, Arg1],
    Arg \== Arg1,
    is_domain_value(_, Arg), !.

% There is a pair of objects to which the constraint applies and none of the "exactly one" relation is there, or more than one.
broken_static_constraint(StaticConstraint, Facts, TypeSignature) :-
    missing_relation(StaticConstraint, Facts, TypeSignature);
    too_many_relations(StaticConstraint, Facts).

missing_relation(one_relation(PredicateNames), Facts, TypeSignature) :-
   % There is a pair of objects in the type signature
   select(object(ObjectType1, ObjectName1), TypeSignature.objects, RemainingSignatureObjects),
   member(object(ObjectType2, ObjectName2), RemainingSignatureObjects),
   % for which one of the "exactly one" relations applies (the others apply as well or else they wouldn't be in the same constraint)
   member(PredicateName, PredicateNames),
   member(predicate(PredicateName, [object_type(ObjectType1), object_type(ObjectType2)]), TypeSignature.predicate_types),
   % And no relation exists in the initial conditions for this pair of objects which name is ones given in the constraint
   \+ (member(PName, PredicateNames),
      member(Fact, Facts),
      Fact =.. [PName, ObjectName1, ObjectName2]
      ),
   log(debug, unity, 'Zero one_relation(~p) between ~p and ~p in facts ~p', [PredicateNames, ObjectName1, ObjectName2, Facts]).

% If there is an object to which the one-related predicate applies, there must be one such condition and only one.
missing_relation(one_related(PredicateName), Facts, TypeSignature) :-
    % There is an object that can be related to another by the predicate named in the constraint
    member(object(ObjectType, ObjectName), TypeSignature.objects),
    member(predicate(PredicateName, [object_type(ObjectType), _]), TypeSignature.predicate_types),
    % such that
    % if there is no such a relation to another object in the facts
    (
     (member(Fact, Facts),
      Fact =.. [PredicateName, ObjectName, _]) ->
        fail
     ;
     % If no such relation for that object is found, the constraint is broken
     log(debug, unity, 'Zero one_related(~p) for object ~p of ~p in facts ~p', [PredicateName, ObjectName, TypeSignature.objects, Facts]),
     true
     ).

% Check all static constraints over the facts for too many constrained releations
too_many_relations([StaticConstraint | _], Facts) :-
    too_many_relations(StaticConstraint, Facts),!.

too_many_relations([_ | OtherStaticConstraints], Facts) :-
    too_many_relations(OtherStaticConstraints, Facts).

% More than one condition on the same objects from the set of mutually exclusive predicates
too_many_relations(one_relation(ConstrainedPredicateNames), Facts) :-
    % There is a relation named in the constraint between two objects
    select(ConstrainedPredicateName, ConstrainedPredicateNames, OtherConstrainedPredicateNames),
    select(Fact, Facts, OtherFacts),
    Fact =.. [ConstrainedPredicateName, ObjectName1, ObjectName2],
    % And there is another fact with a mutually exclusive predicate on these two objects
    member(OtherFact, OtherFacts),
    member(OtherConstrainedPredicateName, OtherConstrainedPredicateNames),
    OtherFact =.. [OtherConstrainedPredicateName, ObjectName1, ObjectName2],
    log(debug, unity, 'Multiple one_relation(~p) in facts ~p', [ConstrainedPredicateNames, Facts]).

% An object is related to multiple objects via a singular relation
too_many_relations(one_related(PredicateName), Facts) :-
    % There is an object that can be related to another by the predicate named in the constraint
    select(Fact, Facts, OtherFacts),
    Fact =.. [PredicateName, ObjectName, _],
    member(OtherFact, OtherFacts),
    OtherFact =.. [PredicateName, ObjectName, _],
    log(debug, unity, 'Multiple one_related(~p) from ~p in facts ~p', [PredicateName, ObjectName, Facts]).



