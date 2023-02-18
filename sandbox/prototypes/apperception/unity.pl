:- module(unity, [
        conceptual_unity/2, 
        spatial_unity/2, 
        static_unity/4, 
        breaks_static_constraints/3,
        too_many_relations/3,
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

% A round si spatially unified if all objects are inter-related.
spatial_unity([], _) :- fail, !.
spatial_unity(Round, TypeSignature) :-
    log(info, unity, 'Checking spatial unity'),
    \+ unrelated_objects(Round, TypeSignature).

% Verify that the round is consistent with the static rules and constraints,
% i.e. compute the closure of the round under the static rules
% without causing a contradiction or breaking a static constraint.
static_unity([], _, _, _) :- fail,!.
static_unity(Round, StaticRules, StaticConstraints, TypeSignature) :-
    log(info, unity, 'Checking static unity'),
    get_global(apperception, uuid, Module),
    setup_call_cleanup(
            clear(Module, TypeSignature.predicate_types), 
            static_unity_(Round, StaticRules, StaticConstraints, TypeSignature, Module),
            clear(Module, TypeSignature.predicate_types)
    ),!.

% The facts in a round breaks static constraints given a type signature.
% Only one constraint need to be broken.
breaks_static_constraints(Round, StaticConstraints, TypeSignature) :-
    member(StaticConstraint, StaticConstraints),
    broken_static_constraint(StaticConstraint, Round, TypeSignature),
    log(info, unity, 'Broken static constraint ~p in round ~p', [StaticConstraint, Round]).

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
unrelated_objects(Round, TypeSignature) :-
    object_name_pair(TypeSignature.objects, ObjectName1-ObjectName2),
    \+ related(ObjectName1, ObjectName2, TypeSignature, Round),
    log(debug, unity, 'Unrelated objects ~p and ~p in round ~p!', [ObjectName1, ObjectName2, Round]).

% Some pair of objects from the type signature
object_name_pair(TypeSignatureObjects, ObjectName1-ObjectName2) :-
    select(object(_, ObjectName1), TypeSignatureObjects, RemainingSignatureObjects),
    member(object(_, ObjectName2), RemainingSignatureObjects).

% Two objects are directly related in one initial condition
related(ObjectName1, ObjectName2, Round) :-
    member(Fact, Round),
    Fact =.. [_ | ObjectNames],
    memberchk(ObjectName1, ObjectNames),
    memberchk(ObjectName2, ObjectNames).

% Two objects are indirectly related in the initial conditions
related(ObjectName1, ObjectName2, TypeSignature, Round) :-
    select(Fact, Round, OtherRound),
    Fact =.. [PredicateName | ObjectNames],
    % It's a relation between two objects
    member(predicate(PredicateName, [object_type(_), object_type(_)]), TypeSignature.predicate_types),
    select(ObjectName1, ObjectNames, [OtherObjectName]),
    related(OtherObjectName, ObjectName2, OtherRound).

static_unity_(Round, StaticRules, StaticConstraints, TypeSignature, Module) :-
     clear(Module, TypeSignature.predicate_types),
     assert_facts(Module, Round),
     assert_rules(Module, StaticRules),
     save_module(Module),
     apply_rules(Module, StaticRules, Facts),
     merge_consistent(Round, Facts, AugmentedRound),
     !,
    (  (length(Round, L),
        length(AugmentedRound, L)
       ) ->
        \+ breaks_static_constraints(AugmentedRound, StaticConstraints, TypeSignature)
        ;
        static_unity_(AugmentedRound, StaticRules, StaticConstraints, TypeSignature, Module)
    ).

% Merge removing duplicates. Fail if attempting to merge contradictory facts.    
merge_consistent(Round, [], Round).
merge_consistent(Round, [Fact | OtherAnswers], MergedRound) :-
    \+ (member(Condition, Round), factual_contradiction(Condition, Fact)),
    (memberchk(Fact, Round) ->
        merge_consistent(Round, OtherAnswers, MergedRound)
        ;
        merge_consistent([Fact | Round], OtherAnswers, MergedRound)
    ).

factual_contradiction(Condition, Fact) :-
    Condition =.. [PredicateName, ObjectName, Arg],
    Fact =.. [PredicateName, ObjectName, Arg1],
    Arg \== Arg1,
    is_domain_value(_, Arg), !.

% There is a pair of objects to which the constraint applies and none of the "exactly one" relation is there, or more than one.
broken_static_constraint(StaticConstraint, Round, TypeSignature) :-
    missing_relation(StaticConstraint, Round, TypeSignature);
    too_many_relations(StaticConstraint, Round, TypeSignature).

missing_relation(one_relation(PredicateNames), Round, TypeSignature) :-
   % There is a pair of objects in the type signature
   select(object(ObjectType1, ObjectName1), TypeSignature.objects, RemainingSignatureObjects),
   member(object(ObjectType2, ObjectName2), RemainingSignatureObjects),
   % for which one of the "exactly one" relations applies (the others apply as well or else they wouldn't be in the same constraint)
   member(PredicateName, PredicateNames),
   member(predicate(PredicateName, [object_type(ObjectType1), object_type(ObjectType2)]), TypeSignature.predicate_types),
   % And no relation exists in the initial conditions for this pair of objects which name is ones given in the constraint
   \+ (member(PName, PredicateNames),
      member(Fact, Round),
      Fact =.. [PName, ObjectName1, ObjectName2]
      ),
   log(debug, unity, 'Zero one_relation(~p) between ~p and ~p in round ~p', [PredicateNames, ObjectName1, ObjectName2, Round]).

% If there is an object to which the one-related predicate applies, there must be one such condition and only one.
missing_relation(one_related(PredicateName), Round, TypeSignature) :-
    % There is an object that can be related to another by the predicate named in the constraint
    member(object(ObjectType, ObjectName), TypeSignature.objects),
    member(predicate(PredicateName, [object_type(ObjectType), _]), TypeSignature.predicate_types),
    % such that
    % if there is no such a relation to another object in the round
    (
     (member(Fact, Round),
      Fact =.. [PredicateName, ObjectName, _]) ->
        fail
     ;
     % If no such relation for that object is found, the constraint is broken
     log(debug, unity, 'Zero one_related(~p) for object ~p of ~p in round ~p', [PredicateName, ObjectName, TypeSignature.objects, Round]),
     true
     ).

% More than one condition on the same objects from the set of mutually exclusive predicates
too_many_relations(one_relation(PredicateNames), Round, _) :-
    % There is a relation named in the constraint between two objects
    select(Fact, Round, OtherFacts),
    Fact =.. [PredicateName, ObjectName1, ObjectName2],
    select(PredicateName, PredicateNames, OtherPredicateNames),
    % And there is another condition also named in the constraint between these two objects
    member(OtherFact, OtherFacts),
    OtherFact =.. [OtherPredicateName, ObjectName1, ObjectName2],
    memberchk(OtherPredicateName, OtherPredicateNames),
    log(debug, unity, 'Multiple one_relation(~p) from ~p in round ~p', [PredicateNames, ObjectName1, Round]).

% If there is an object to which the one-related predicate applies, there must not be more than one.
too_many_relations(one_related(PredicateName), Round, TypeSignature) :-
    % There is an object that can be related to another by the predicate named in the constraint
    member(object(ObjectType, ObjectName), TypeSignature.objects),
    member(predicate(PredicateName, [object_type(ObjectType), _]), TypeSignature.predicate_types),
    % such that
    % if there is such a relation to another object in the initial conditions, a second such relation to yet another object breaks the constraint
    (
     (select(Fact, Round, OtherFacts),
      Fact =.. [PredicateName, ObjectName, _]) ->
        (member(OtherFact, OtherFacts),
         OtherFact =.. [PredicateName, ObjectName, _],
         log(debug, unity, 'Multiple one_related(~p) from ~p in round ~p', [PredicateName, ObjectName, Round])
        )
     ;
     fail
     ).


