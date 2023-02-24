:- module(theory_engine, [create_theory_engine/2]).

:- use_module(library(lists)).
:- use_module(logger).
:- use_module(type_signature).
:- use_module(domains).
:- use_module(global).
:- use_module(unity).
:- use_module(rules_engine).


% An engine that generates valid theories on demand,
% given a minimal type signature (from the sequence) and a template (which sets the scope of the search space).

/*
cd('sandbox/prototypes/apperception').
[logger, type_signature, domains, global, unity, rules_engine, theory_engine].
ObjectTypes = [led],
PredicateTypes = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(behind, [object_type(led),  object_type(led)]) ], 
Objects = [object(led, light1), object(led, light2)],
TypedVariables = [variables(led, 2)], 
TypeSignature = type_signature{object_types:ObjectTypes, predicate_types:PredicateTypes, objects:Objects, typed_variables:TypedVariables},
Limits = limits{max_rules:8, max_elements:60, max_theory_time: 1000},
Template = template{type_signature:TypeSignature, limits:Limits},

create_theory_engine(Template, TheoryEngine),
engine_next(TheoryEngine, Theory1),
engine_next(TheoryEngine, Theory2),
engine_destroy(TheoryEngine).
*/

% Create an engine that produces theories.
create_theory_engine(Template, TheoryEngine) :-
    engine_create(Theory, theory(Template, Theory), TheoryEngine), !.

% template(type_signature: TypeSignature, max_rules: MaxRules, max_elements: MaxElements)
% type_signature(objects: Objects, predicate_types: PredicateTypes, typed_variables: TypedVariables)
theory(Template, Theory) :-
    uuid(UUID),
    set_global(apperception, uuid, UUID),
    reset_deadline(Template.limits.max_theory_time),
    reset_visited_constraints,
    static_constraints(Template.type_signature, StaticConstraints),
    log(info, theory_engine, 'Static constraints ~p', [StaticConstraints]),
    static_rules(Template, StaticConstraints, StaticRules),
    log(info, theory_engine, 'Static rules ~p', [StaticRules]),
    causal_rules(Template, StaticConstraints, CausalRules),
    log(info, theory_engine, 'Causal rules ~p', [CausalRules]),
    initial_conditions(Template.type_signature, StaticConstraints, CausalRules, StaticRules, InitialConditions),
    log(info, theory_engine, 'Initial conditions ~p', [InitialConditions]),
    Theory = theory{static_rules:StaticRules, causal_rules:CausalRules, static_constraints:StaticConstraints, initial_conditions:InitialConditions, rating: 0},
    reset_deadline(Template.limits.max_theory_time).

reset_deadline(MaxTime) :-
    get_time(Now),
    Deadline is Now + MaxTime,
    set_global(apperception, theory_engine/deadline, Deadline).

%  Throw an exception if too much time has been spent trying to compose a theory
check_deadline :-
    get_time(Now),
    get_global(apperception, theory_engine/deadline, Deadline),
    Now > Deadline -> 
        log(warn, theory_engine, 'TIME EXPIRED for this theory'),
        throw(error(time_expired, context(theory_engine, Deadline)))
    ; 
    true.

% Reset visited constraints to empty
reset_visited_constraints :-
    set_global(apperception, theory_engine/visited_constraints, []).

% Unary constraints are implicit in value domains (an led's "on" property can not be both true and false at the same time).
% A binary constraint defines a set of predicates on objects X and Y, such that exactly one of a set of binary relations Relation(X,Y) must be true at any time.
%   one_relation([pred1, pred2, pred3]).
% A uniqueness constraint states that for an object X, there is exactly one object Y such that r(X, Y).
%   one_related(pred1). 
static_constraints(TypeSignature, StaticConstraints) :-
    log(debug, theory_engine, 'Making static constraints'),
    check_deadline,
    all_binary_predicate_names(TypeSignature, AllBinaryPredicateNames),
    reset_visited_constraints,
    maybe_add_static_constraint(one_related, AllBinaryPredicateNames, [], StaticConstraints1),
    maybe_add_static_constraint(one_relation, AllBinaryPredicateNames, StaticConstraints1, StaticConstraints),
    valid_static_constraints(StaticConstraints, TypeSignature).

% Static rules
% Grow a set of static rules given a set of typed predicates and of typed variables such that
% * There is at least one rule
% * They do no exceed the limits set (max number of rules, max number of elements for the rule set)
% * No rule repeats another
% * No rule contradicts another
% * Rules can not contradict static constraints.
% Rules are constructed and compared as rule pairs, i.e., Head-Body pairs.
static_rules(Template, StaticConstraints, RulePairs) :-
    log(debug, theory_engine, 'Making static rules'),
    check_deadline,
    make_static_rule(Template.type_signature, Head-BodyPredicates),
    valid_static_rule(Head-BodyPredicates, StaticConstraints),
    valid_static_rules([Head-BodyPredicates], StaticConstraints),
    maybe_add_static_rule(Template, StaticConstraints, [Head-BodyPredicates], RulePairs).

% Rules that produce the changes in the next state from the current one (frame axiom)
% A predicate in head describes state at time t+1, the body describes state at time t.
% Rules must not exceed limits nor repeat themselves nor define non-change
causal_rules(Template, StaticConstraints, RulePairs) :-  
    log(debug, theory_engine, 'Making causal rules'),
    check_deadline,
    make_rule(Template.type_signature, Head-BodyPredicates),
    valid_causal_rule(Head-BodyPredicates, StaticConstraints),
    valid_causal_rules([Head-BodyPredicates]),
    NextifiedHead =.. [next, Head],
    maybe_add_causal_rule(Template, StaticConstraints, [NextifiedHead-BodyPredicates], RulePairs).

%% CONSTRUCTING VALID INITIAL CONDITIONS
initial_conditions(TypeSignature, StaticConstraints, CausalRules, StaticRules, UnifiedInitialConditions) :-
    log(debug, theory_engine, 'Making initial conditions'),
    check_deadline,
    reset_visited_initial_conditions,
    !,
    satisfy_causal_rule(CausalRules, TypeSignature, CausedConditions),
    % For every object, give a value to every applicable property
    add_initial_properties(TypeSignature.objects, TypeSignature.predicate_types, CausedConditions, WithInitialProperties),
    % For each pairing of objects, add one or more relations without breaking static constraints
    add_initial_relations(TypeSignature, StaticConstraints, WithInitialProperties, InitialConditions),
    % Make sure we have not produced these initial conditions already
    new_initial_conditions(InitialConditions),
    % Verify these new initial conditions are valid
    unified_initial_conditions(InitialConditions, TypeSignature, StaticConstraints, StaticRules, UnifiedInitialConditions).

new_initial_conditions(InitialConditions) :-
    not_already_visited(InitialConditions, theory_engine/visited_initial_conditions).

% Conceptual unity: Each (binary) predicate appears in a static constraint
% Do not repeat previously visited static constraints
valid_static_constraints(StaticConstraints, TypeSignature) :-
    conceptual_unity(StaticConstraints, TypeSignature),
    not_already_visited(StaticConstraints, theory_engine/visited_constraints).

not_already_visited(Solution, Path) :-
    get_global(apperception, Path, AllVisited),
    \+ repeats_visited(Solution, AllVisited),
    set_global(apperception, Path, [Solution | AllVisited]).

repeats_visited(Solution, AllVisited) :-
    member(VisitedSolution, AllVisited),
    length(Solution, L),
    length(VisitedSolution, L),
    subset(Solution, VisitedSolution).

maybe_add_static_constraint(_, _, StaticConstraints, StaticConstraints).

maybe_add_static_constraint(Kind, AllBinaryPredicateNames, Acc, StaticConstraints) :-
    static_constraint(Kind, AllBinaryPredicateNames, StaticConstraint),
    valid_static_constraint(StaticConstraint, Acc),
    maybe_add_static_constraint(Kind, AllBinaryPredicateNames, [StaticConstraint | Acc], StaticConstraints).

% Uniqueness constraint:  For any X, there is one and only one Y such that r(X, Y).
static_constraint(one_related, AllBinaryPredicateNames, StaticConstraint) :-
    member(BinaryPredicateName, AllBinaryPredicateNames),
    StaticConstraint =.. [one_related, BinaryPredicateName].

% Exclusion constraint: There must one and only one relation r1(X, Y), r2(X, Y), r3(X, Y)
static_constraint(one_relation, AllBinaryPredicateNames, StaticConstraint) :-
    sublist(BinaryPredicateNames, AllBinaryPredicateNames),
    length(BinaryPredicateNames, Length),
    Length > 1,
    StaticConstraint =.. [one_relation, BinaryPredicateNames].

valid_static_constraint(StaticConstraint, OtherStaticConstraints) :-
    \+ redundant_static_constraints([StaticConstraint | OtherStaticConstraints]).

redundant_static_constraints([Constraint | OtherConstraints]) :-
    member(OtherConstraint, OtherConstraints),
    (subsumed_static_constraint(Constraint, OtherConstraint) -> true 
    ; 
    redundant_static_constraints(OtherConstraints)).

subsumed_static_constraint(one_relation(PredicateNames), one_relation(OtherPredicateNames)) :-
    subset(PredicateNames, OtherPredicateNames) ; subset(OtherPredicateNames, PredicateNames).

subsumed_static_constraint(one_related(PredicateName), one_related(PredicateName)).


% Sublist of a list (order is preserved)
sublist([], []).
sublist(Sub, [_| Rest]) :-
    sublist(Sub, Rest).
sublist([X | Others], [X | Rest]) :-
    sublist(Others, Rest).


% Make a rule made from predicates, objects and variables
% TypeSignature.predicate_types = [predicate(on, [object_type(led), value_type(boolean), ...]
% TypeSignature.typed_variables = [variables(led, 2), variables(object1, 1), ...]
make_rule(TypeSignature, Head-BodyPredicates) :-
    make_distinct_variables(TypeSignature.typed_variables, DistinctVars),
    make_head(TypeSignature.predicate_types, DistinctVars, Head),
    make_body_predicates(TypeSignature.predicate_types, DistinctVars, Head, BodyPredicates).
    

maybe_add_causal_rule(_, _, RulePairs, RulePairs).
maybe_add_causal_rule(Template, _, RulePairs, RulePairs) :-
    rules_at_limits(RulePairs, Template.limits), !.

maybe_add_causal_rule(Template, StaticConstraints, Acc, RulePairs) :-
    make_rule(Template.type_signature, Head-BodyPredicates),
    valid_causal_rule(Head-BodyPredicates, StaticConstraints),
    % Nextifying the rule
    NextifiedHead =.. [next, Head],
    valid_causal_rules([NextifiedHead-BodyPredicates | Acc]),!,
    maybe_add_causal_rule(Template, StaticConstraints, [NextifiedHead-BodyPredicates | Acc], RulePairs).

% Checking rule before it is nextified
valid_causal_rule(Head-BodyPredicates, StaticConstraints) :-
    \+ idempotent_causal_rule(Head-BodyPredicates),
    \+ too_many_relations(StaticConstraints, BodyPredicates).

% An idempotent causal rule is is one where the head is found in the body -> nothing changes
idempotent_causal_rule(Head-BodyPredicates) :-
    memberchk_equal(Head, BodyPredicates), !.

% Checking nextified rules
valid_causal_rules(RulePairs) :-
     numerize_vars_in_rule_pairs(RulePairs, RulePairsWithNumberVars),
    % No repeated rules
    \+ repeated_rules(RulePairsWithNumberVars).

make_static_rule(TypeSignature, HoldingHead-BodyPredicates) :-
    make_rule(TypeSignature, Head-BodyPredicates),
    HoldingHead =.. [holds, Head].

maybe_add_static_rule(_, _, RulePairs, RulePairs).
maybe_add_static_rule(Template, _, RulePairs, RulePairs) :-
    rules_at_limits(RulePairs, Template.limits), !.

maybe_add_static_rule(Template, StaticConstraints, Acc, RulePairs) :-
    make_static_rule(Template.type_signature, Head-BodyPredicates),
    valid_static_rule(Head-BodyPredicates, StaticConstraints),
    valid_static_rules([Head-BodyPredicates | Acc], StaticConstraints),!,
    maybe_add_static_rule(Template, StaticConstraints, [Head-BodyPredicates | Acc], RulePairs).

valid_static_rule(Head-Body, StaticConstraints)  :-
    \+ recursive_static_rule(Head-Body),
    \+ too_many_relations(StaticConstraints, Body).

recursive_static_rule(HoldingHead-BodyPredicates) :-
    HoldingHead =.. [holds, Head],
    copy_term_nat(Head, CopiedHead),
    member(BodyPredicate, BodyPredicates),
    copy_term_nat(BodyPredicate, CopiedBodyPredicate),
    CopiedHead =@= CopiedBodyPredicate.

valid_static_rules(RulePairs, StaticConstraints) :-
    % numerize vars in each copied rule pair before comparing them to one another
    % to avoid head1(X, Y) :- pred1(X, Y) being seen as equivalent to head1(X, Y) :- pred1(Y, X) etc.
    numerize_vars_in_rule_pairs(RulePairs, RulePairsWithNumberVars),
    \+ repeated_rules(RulePairsWithNumberVars),
    % No two rules that contradict one another
    \+ contradicting_static_rules(RulePairsWithNumberVars),
    % No rule contradicts a static constraint
    \+ contradicted_static_contraint(RulePairsWithNumberVars, StaticConstraints),
    % use un-numerized pairs b/c testing is based on unify-ability
    \+ recursion_in_static_rules(RulePairs).

rules_at_limits(RulePairs, Limits) :-
    length(RulePairs, NumberOfRules),
    NumberOfRules >= Limits.max_rules,
    log(info, theory_engine, 'Exceeding max rules of ~p: ~p', [NumberOfRules, RulePairs]),
    !.

rules_at_limits(RulePairs, Limits) :-
    count_atoms(RulePairs, NumberOfAtoms),
    NumberOfAtoms >= Limits.max_elements, 
    log(info, theory_engine, 'Exceeding atoms limit of ~p: ~p', [NumberOfAtoms, RulePairs]),
    !.

contradicted_static_contraint(RulePairs, StaticConstraints) :-
    member(Rule, RulePairs),
    member(StaticConstraint, StaticConstraints),
    static_rule_contradicts_constraint(Rule, StaticConstraint),
    !.

% There are at least two binary predicates in the body with names in PredicateNames and relating the same vars
static_rule_contradicts_constraint(Head-BodyPredicates, one_relation(PredicateNames)) :-
    select(Pred1, BodyPredicates, OtherBodyPredicates),
    Pred1 =.. [PredName1, Var1, Var2],
    var_number(Var1, _),
    var_number(Var2 , _),
    member(Pred2, OtherBodyPredicates),
    Pred2 =.. [PredName2, Var1, Var2],
    subset([PredName1, PredName2], PredicateNames),
    log(debug, theory_engine, 'Static rule ~p contradicts static constraint ~p', [Head-BodyPredicates, one_relation(PredicateNames)]).


% An atom, here, is a singular, non-compound term.
count_atoms(Var, 1) :-
    var(Var), !.
count_atoms([], 0) :- !.
count_atoms([Term | OtherTerms], Count) :-
    !,
    count_atoms(Term, N1),
    count_atoms(OtherTerms, N2),
    Count is N1 + N2.
count_atoms(Term, Count) :-
    compound(Term),
    !,
    Term =.. SubTerms,
    count_atoms(SubTerms, Count).
count_atoms(_, 1).

% numbervar variables in each rule pair so that they can be compared by the order in which they appear.
numerize_vars_in_rule_pairs([], []).
numerize_vars_in_rule_pairs([HeadPredicate-BodyPredicates | OtherRulePairs], [HeadPredicate1-BodyPredicates1 | OtherRulePairs1]) :-
    numerize_vars([HeadPredicate | BodyPredicates], [HeadPredicate1 |BodyPredicates1]),
    numerize_vars_in_rule_pairs(OtherRulePairs, OtherRulePairs1).

numerize_vars(Predicates, PredicatesWithNumerizedVars) :-
    copy_term_nat(Predicates, PredicatesWithNumerizedVars),
    numbervars(PredicatesWithNumerizedVars, 1, _).

repeated_rules(RulePairs) :- 
    select(RulePair, RulePairs, OtherRulePairs), 
    member(OtherRulePair, OtherRulePairs), 
    rule_repeats(RulePair, OtherRulePair).

rule_repeats(Head-BodyPredicates, OtherHead-OtherBodyPredicates) :-
    equivalent_predicates(Head, OtherHead),
    equivalent_bodies(BodyPredicates, OtherBodyPredicates),
    log(debug, theory_engine, 'Repeated rule ~p', [Head-BodyPredicates]).

equivalent_bodies(BodyPredicates, OtherBodyPredicates) :-
    subsumed_conjunctions(BodyPredicates, OtherBodyPredicates) -> true ; subsumed_conjunctions(OtherBodyPredicates, BodyPredicates).

% True if each predicate in a body is equivalent to another predicate in the other body
subsumed_conjunctions([], _).
subsumed_conjunctions([Predicate | Rest], OtherPredicates) :-
    member(OtherPredicate, OtherPredicates),
    (equivalent_predicates(Predicate, OtherPredicate) -> subsumed_conjunctions(Rest, OtherPredicates)).

% A recusion exists when the head of a static rule can unify with a predicate in the body of another static rule.
recursion_in_static_rules(RulePairs) :-
    select(HoldingHead-_, RulePairs, OtherRulePairs),
    HoldingHead =.. [holds, Head],
    member(_-OtherBody, OtherRulePairs),
    member(OtherBodyPredicate, OtherBody),
    Head =@= OtherBodyPredicate,
    log(debug, theory_engine, 'Recursion on ~p in static rules ~p', [HoldingHead, RulePairs]).

% Reset visited initial conditions to empty
reset_visited_initial_conditions :-
    set_global(apperception, theory_engine/visited_initial_conditions, []).

% Ensure the initial conditions trigger one of the causal rules
satisfy_causal_rule(CausalRules, TypeSignature, CausedConditions) :-
    member(_-BodyPredicates, CausalRules),
    copy_term(BodyPredicates, CopiedBodyPredicates),
    satisfy_causal_rule_(CopiedBodyPredicates, TypeSignature, [], CausedConditions).

satisfy_causal_rule_([], _, CausedConditions, CausedConditions).
satisfy_causal_rule_([BodyPredicate | BodyPredicates], TypeSignature, Acc, CausedConditions) :-
    (ground(BodyPredicate) ; do_ground_predicate(BodyPredicate, TypeSignature)),
    satisfy_causal_rule_(BodyPredicates, TypeSignature, [BodyPredicate | Acc], CausedConditions).

do_ground_predicate(BodyPredicate, TypeSignature) :-
    BodyPredicate =.. [PredicateName | Args],
    do_ground_args(Args, 1, PredicateName, TypeSignature).

do_ground_args([], _, _, _).
do_ground_args([Arg | OtherArgs], Index, PredicateName, TypeSignature) :-
    do_ground_arg(Arg, Index, PredicateName, TypeSignature),
    NextIndex is Index + 1,
    do_ground_args(OtherArgs, NextIndex, PredicateName, TypeSignature).

do_ground_arg(Arg, _, _, _) :- ground(Arg), !.

do_ground_arg(Arg, Index, PredicateName, TypeSignature) :-
    member(predicate(PredicateName, ArgTypes), TypeSignature.predicate_types),
    nth1(Index, ArgTypes, object_type(ObjectType)),
    member(object(ObjectType, ObjectName), TypeSignature.objects),
    Arg = ObjectName.

% For each object, for each applicable property, make one with some domain value.
add_initial_properties(Objects, PredicateTypes, CausedConditions, WithProperties) :-
    add_initial_properties_(Objects, PredicateTypes, CausedConditions, WithProperties).

add_initial_properties_([], _, WithProperties, WithProperties).

add_initial_properties_([Object | OtherObjects], PredicateTypes, Acc, WithProperties) :-
    add_object_properties(Object, PredicateTypes, Acc, Acc1),
    add_initial_properties_(OtherObjects, PredicateTypes, Acc1, WithProperties).

add_object_properties(_, [], Properties, Properties).

add_object_properties(object(ObjectType, ObjectName), 
                       [predicate(PredicateName, [object_type(ObjectType), value_type(Domain)]) | OtherPredicateTypes],
                       Acc,
                       Properties) :-
    domain_is(Domain, Values),
    member(Value, Values),
    Property =.. [PredicateName, ObjectName, Value],
    \+ memberchk(Property, Acc),
    \+ (member(Fact, Acc), factual_contradiction(Property, Fact)),
    !,
    add_object_properties(object(ObjectType, ObjectName), OtherPredicateTypes, [Property | Acc], Properties).

add_object_properties(Object, [_ | OtherPredicateTypes], Acc, Properties) :-
    add_object_properties(Object, OtherPredicateTypes, Acc, Properties).

% For each pairing of objects, add one or more relations without breaking static constraints
add_initial_relations(TypeSignature, StaticConstraints, Acc, InitialConditions) :-
    object_pairs(TypeSignature.objects, ObjectPairs),
    add_object_relations(ObjectPairs, TypeSignature, StaticConstraints, Acc, InitialConditions).

object_pairs(Objects, ObjectPairs) :-
    object_pairs_(Objects, [], ObjectPairs).
    
object_pairs_([], ObjectPairs, ObjectPairs).
object_pairs_([_], ObjectPairs, ObjectPairs).
object_pairs_([Object | OtherObjects], Acc, ObjectPairs) :-
    findall([Object-OtherObject, OtherObject-Object], member(OtherObject, OtherObjects), PairsList),
    flatten([PairsList, Acc], Acc1),
    object_pairs_(OtherObjects, Acc1, ObjectPairs), 
    !.

add_object_relations([], _, _, InitialConditions, InitialConditions).
add_object_relations(ObjectPairs, TypeSignature, StaticConstraints, Acc, InitialConditions) :-
    select(ObjectPair, ObjectPairs, OtherObjectPairs),
    add_object_pair_relations(ObjectPair, TypeSignature, StaticConstraints, Acc, Acc1),
    add_object_relations(OtherObjectPairs, TypeSignature, StaticConstraints, Acc1, InitialConditions).

add_object_pair_relations(ObjectPair, TypeSignature, StaticConstraints, Acc, InitialConditions) :-
    add_required_pair_relations(ObjectPair, StaticConstraints, TypeSignature.predicate_types, Acc, Acc1),
    maybe_add_object_pair_relations(ObjectPair, TypeSignature.predicate_types, StaticConstraints, TypeSignature, Acc1, InitialConditions).

add_required_pair_relations(_, [], _, Acc, Acc).

% Add one relation of the mutually exclusive relations per object pair 
add_required_pair_relations(ObjectPair, 
                           [one_relation(ConstrainedPredicateNames) | OtherStaticConstraints], 
                           PredicateTypes, 
                           Acc, 
                           Acc1) :-
    member(PredicateName, ConstrainedPredicateNames),
    make_pair_relation(ObjectPair, PredicateName, PredicateTypes, Relation),
    add_required_pair_relations(ObjectPair, 
                           OtherStaticConstraints, 
                           PredicateTypes, 
                           [Relation | Acc],
                           Acc1).

% Make sure that the first paired object has the unique relation with only one other object.
add_required_pair_relations(ObjectPair, 
                                [one_related(PredicateName) | OtherStaticConstraints], 
                                PredicateTypes, 
                                Acc, 
                                Acc1) :-
    object(_, ObjectName1)-_ = ObjectPair,
    PriorRelation =.. [PredicateName, ObjectName1, _],
    (member(PriorRelation, Acc) ->
        add_required_pair_relations(ObjectPair, OtherStaticConstraints, PredicateTypes, Acc, Acc1)
        ;
        make_pair_relation(ObjectPair, PredicateName, PredicateTypes, Relation),
        add_required_pair_relations(ObjectPair, OtherStaticConstraints, PredicateTypes, [Relation | Acc], Acc1)
    ).

make_pair_relation(ObjectPair, PredicateName, PredicateTypes, Relation) :-
    object(ObjectType1, ObjectName1)-object(ObjectType2, ObjectName2) = ObjectPair,
    member(predicate(PredicateName, [object_type(ObjectType1), object_type(ObjectType2)]), PredicateTypes),
    Relation =.. [PredicateName, ObjectName1, ObjectName2].

maybe_add_object_pair_relations(_, [], _, _, InitialConditions, InitialConditions).

maybe_add_object_pair_relations(object(ObjectType1, ObjectName1)-object(ObjectType2, ObjectName2), 
                       [predicate(PredicateName, [object_type(ObjectType1), object_type(ObjectType2)]) | OtherPredicateTypes], 
                       StaticConstraints,
                       TypeSignature,
                       Acc, 
                       InitialConditions) :-
                       Relation =.. [PredicateName, ObjectName1, ObjectName2],
                       \+ memberchk(Relation, Acc),
                       % Don't break the at-most-one aspect of a one-relation or one-related constraint by adding the new relation
                       \+ too_many_relations(StaticConstraints, [Relation | Acc]),
                       maybe_add_object_pair_relations(object(ObjectType1, ObjectName1)-object(ObjectType2, ObjectName2), OtherPredicateTypes, StaticConstraints, TypeSignature, [Relation | Acc], InitialConditions).

maybe_add_object_pair_relations(ObjectPair, [_ | OtherPredicateTypes], StaticConstraints, TypeSignature, Acc, InitialConditions) :-
    maybe_add_object_pair_relations(ObjectPair, OtherPredicateTypes, StaticConstraints, TypeSignature, Acc, InitialConditions).

% Check that no static constraint is broken.
% Check that all objects are represented and spatial unity is achieved.
% Unify intial conditions by applying static rules to closure, without creating inconsistencies or breaking static rules
unified_initial_conditions(InitialConditions, TypeSignature, StaticConstraints, StaticRules, UnifiedConditions) :-
  check_deadline,
  get_global(apperception, uuid, Module),
  \+ unrepresented_object(InitialConditions, TypeSignature.objects),
  spatial_unity(InitialConditions, TypeSignature),
  static_closure(InitialConditions, StaticRules, TypeSignature, Module, UnifiedConditions),
  \+ breaks_static_constraints(UnifiedConditions, StaticConstraints, TypeSignature),
  !.

% One object in the type signature does not appear in a condition
unrepresented_object(Round, Objects) :-
    member(object(_, ObjectName), Objects),
    \+ object_referenced(ObjectName, Round),
    log(debug, theory_engine, 'Unrepresented object ~p in round ~p', [ObjectName, Round]).
    
% An object is referenced by name somewhere in the initial conditions
object_referenced(ObjectName, Round) :-
    member(Fact, Round),
    Fact =.. [_ | ObjectNames],
    member(ObjectName, ObjectNames).

%% Utilities

equivalent_predicates(Predicate, OtherPredicate) :- 
    Predicate =.. [PredicateName | Args],
    OtherPredicate =.. [PredicateName | OtherArgs],
    equivalent_args(Args, OtherArgs).

% Assumes that all vars have been numerized per rule pair
equivalent_args([], []).
equivalent_args([Arg | Rest], [OtherArg | OtherRest]) :-
    var_number(Arg, _),
    var_number(OtherArg, _),
    !,
    equivalent_args(Rest, OtherRest).
equivalent_args([Arg | Rest], [Arg | OtherRest]) :-
    equivalent_args(Rest, OtherRest).


% There exists two static rules that contradict one another
contradicting_static_rules([Head-Body | OtherRulePairs]) :- 
    member(OtherHead-OtherBody, OtherRulePairs),
    (contradicting_heads(Head-Body, OtherHead-OtherBody) -> 
        log(debug, theory_engine, 'Contradicting rules!'),
        true; 
        contradicting_bodies(Head-Body, OtherHead-OtherBody)
    ).

% Two rules contradict "at the head" one another if they have contradicting heads and equivalent bodies
contradicting_heads(HoldingHead-Body, OtherHoldingHead-OtherBody) :-
    HoldingHead =.. [holds, Head],
    OtherHoldingHead =.. [holds, OtherHead],
    contradicts(Head, OtherHead),
    equivalent_bodies(Body, OtherBody).

% Two rules contradict "at the body" one another if they have equivalent heads and contradicting bodies
contradicting_bodies(Head-Body, OtherHead-OtherBody) :-
    equivalent_predicates(Head, OtherHead),
    contradicting_predicates(Body, OtherBody).

% There exists two predicates in a list of predicates that contradict one another
contradicting_predicates([Predicate | Rest], OtherPredicates) :-
    contradiction_in([Predicate | OtherPredicates]) -> true ; contradicting_predicates(Rest, OtherPredicates).

make_head(PredicateTypes, DistinctVars, Head) :-
    member(PredicateType, PredicateTypes),
    make_rule_predicate(PredicateType, DistinctVars, Head).

% Make a list of at least one mutually valid body predicates
make_body_predicates(PredicateTypes, DistinctVars, Head, BodyPredicates) :-
    make_body_predicate(PredicateTypes, DistinctVars, Head, [], BodyPredicate),
    maybe_add_body_predicates(PredicateTypes, DistinctVars, Head, [BodyPredicate], BodyPredicates),
    valid_body_predicates(BodyPredicates, Head).

% TODO - the var in the head and its use in the body must be consistently typed
make_body_predicate(PredicateTypes, DistinctVars, Head, BodyPredicates, BodyPredicate) :-
    member(PredicateType, PredicateTypes),
    make_rule_predicate(PredicateType, DistinctVars, BodyPredicate),
    valid_body_predicate(BodyPredicate, Head, BodyPredicates).

% Don't repeat the head predicate in the body or any body predicate
% Don't contradict other predicates
valid_body_predicate(BodyPredicate, Head, BodyPredicates) :-
    numerize_vars([Head, BodyPredicate | BodyPredicates], PredicatesWithNumerizedVars),
    all_different(PredicatesWithNumerizedVars),
    \+ contradiction_in(PredicatesWithNumerizedVars).

all_different([]).
all_different([Term | Rest]) :-
    \+ memberchk_equal(Term, Rest),
    all_different(Rest).

contradiction_in([Predicate | Rest]) :-
    member(OtherPredicate, Rest),
    contradicts(Predicate, OtherPredicate), !.
contradiction_in([_ | Rest]) :- 
    contradiction_in(Rest).

% Same name, same corresponding vars, different correponding values
contradicts(BodyPredicate, Head) :-
    BodyPredicate =.. [Name | Args1],
    Head =.. [Name | Args2],
    contradicting_args(Args1, Args2).

contradicting_args([Arg1 | Rest1], [Arg2 | Rest2]) :-
   var_number(Arg1, _),
   var_number(Arg2, _),
   !,
   (equivalent_vars(Arg1, Arg2) -> contradicting_args(Rest1, Rest2) ; fail).
contradicting_args([Arg1 | _], [Arg2 | _]) :-
   Arg1 \== Arg2, !.
contradicting_args([_ | Rest1], [_ | Rest2]) :-
    contradicting_args(Rest1, Rest2).

equivalent_vars(Var1, Var2) :-
    var_number(Var1, N),
    var_number(Var2, N).

% Together, the body predicates cover all variables in the head.
% All variables are related in the head OR body
valid_body_predicates(BodyPredicates, Head) :-
    \+ singleton_var_exists([Head | BodyPredicates]),
    all_vars_related([Head | BodyPredicates]).

singleton_var_exists(Predicates) :-
    collect_vars(Predicates, Vars),
    member(Var, Vars),
    predicates_with_var(Var, Predicates, PredicatesWithVar),
    length(PredicatesWithVar, Count),
    Count == 1, !.

predicates_with_var(_, [], []).
predicates_with_var(Var, [Predicate | OtherPredicates], PredicatesWithVar) :-
    Predicate =.. [_ |Args],
    (
     memberchk_equal(Var, Args) -> predicates_with_var(Var, OtherPredicates, OtherPredicatesWithVar), PredicatesWithVar = [Predicate | OtherPredicatesWithVar] 
     ;
     predicates_with_var(Var, OtherPredicates, PredicatesWithVar)
     ).

collect_vars([], []).
collect_vars([Predicate | OtherPredicates], Vars) :-
    Predicate =.. [_ | Args],
    select_vars(Args, VarArgs),
    collect_vars(OtherPredicates, OtherVars),
    % Don't use append - it replaces vars
    collate(VarArgs, OtherVars, Vars).

% Collate without duplicates
collate([], List, List).
collate([Term | Rest], List, Others) :-
    memberchk_equal(Term, List), !,
    collate(Rest, List, Others).
collate([Term | Rest], List, [Term | Others]) :-
    collate(Rest, List, Others).

% Select without duplicates
select_vars([], []).
select_vars([Term | Rest], [Term | Vars]) :-
    var(Term),
    \+ memberchk_equal(Term, Rest),
    !,
    select_vars(Rest, Vars).
select_vars([_ | Rest], Vars) :-
    select_vars(Rest, Vars).

memberchk_equal(_, []) :- !, fail.
memberchk_equal(_, List) :- var(List), !, fail.
memberchk_equal(Term, [El | Rest]) :-
    Term == El -> true ; memberchk_equal(Term, Rest).

different_predicate_from(Predicate, OtherPredicate) :-
    Predicate =.. [Name | Args],
    OtherPredicate =.. [OtherName, OtherArgs],
    (Name == OtherName -> different_args(Args, OtherArgs); true).

different_args([], []).
different_args([Arg | Rest], [OtherArg | OtherRest]) :-
    different_arg(Arg, OtherArg),
    different_args(Rest, OtherRest).

different_arg(Arg, OtherArg) :-
    var_number(Arg, _),
    var_number(OtherArg, _),
    !,
    \+ equivalent_vars(Arg, OtherArg).
different_arg(_, _).

maybe_add_body_predicates(_, _, _, CurrentBodyPredicates, CurrentBodyPredicates).

maybe_add_body_predicates(PredicateTypes, DistinctVars, Head, CurrentBodyPredicates, BodyPredicates) :-
    make_body_predicate(PredicateTypes, DistinctVars, Head, CurrentBodyPredicates, BodyPredicate),
    valid_body_predicates([BodyPredicate | CurrentBodyPredicates], Head),
    maybe_add_body_predicates(PredicateTypes, DistinctVars, Head, [BodyPredicate | CurrentBodyPredicates], BodyPredicates).

% Don't repeat a var in the args of a predicate
make_rule_predicate(predicate(Name, TypedArgs), TypedVars, RulePredicate) :-
    make_args(TypedArgs, TypedVars, Args),
    RulePredicate =.. [Name | Args].

make_args([], _, []).

make_args([TypedArg | OtherTypedArgs], TypedVars, [Arg | OtherArgs]) :-
    make_arg(TypedArg, TypedVars, UnusedTypedVars, Arg),
    make_args(OtherTypedArgs, UnusedTypedVars, OtherArgs).

make_arg(object_type(Type), TypedVars, UnusedTypedVars, Arg) :-
    take_typed_var(Type, TypedVars, UnusedTypedVars, Arg).

make_arg(value_type(Domain), TypedVars, TypedVars, Arg) :-
    domain_is(Domain,Values),
    member(Arg, Values).
    
take_typed_var(Type, [vars(Type, Vars) | OtherTypedVars], [vars(Type, UnusedVars) | OtherTypedVars], Arg) :-
    select(Arg, Vars, UnusedVars).

take_typed_var(Type, [TypedVars | OtherTypedVars], [TypedVars | UnusedVars], Arg) :-
    take_typed_var(Type, OtherTypedVars, UnusedVars, Arg).

% count each type.
% Create a list of count variables for each type
% Return pairs count-variables
make_distinct_variables(TypedVariables, DistinctVars) :-
    bagof(vars(Type, Vars), N^(member(variables(Type, N), TypedVariables), distinct_vars(N, Vars)), DistinctVars).

distinct_vars(N, Vars) :-
    length(Vars, N),
    distinguish_all(Vars).

distinguish_all([]).
distinguish_all([Var | Rest]) :-
    distinguish_from(Var, Rest),
    distinguish_all(Rest).

distinguish_from(_, []).
distinguish_from(Var, [Other | Rest]) :-
    % constraint: Var and Other can never unify
    dif(Var, Other),
    distinguish_from(Var, Rest).

all_vars_related(Predicates) :-
    collect_vars(Predicates, Vars),
    \+ unrelated_vars(Vars, Predicates).

unrelated_vars(Vars, Predicates) :-
    member(Var1, Vars),
    member(Var2, Vars),
    Var1 \== Var2,
    \+ vars_related(Var1, Var2, Predicates).

% Two different vars are directly related within a single predicate
vars_related(Var1, Var2, Predicates) :-
    member(Predicate, Predicates),
    Predicate =.. [_ | Args],
    select_vars(Args, VarArgs),
    (directly_related_vars(Var1, Var2, VarArgs) -> true; indirectly_related_vars(Var1, Var2, VarArgs, Predicates)).

directly_related_vars(Var1, Var2, VarArgs) :-
    memberchk_equal(Var1, VarArgs),
    memberchk_equal(Var2, VarArgs).

% Two different vars are indirectly related across multiple predicates
indirectly_related_vars(Var1, Var2, VarArgs, Predicates) :-
    length(VarArgs, 2),
    memberchk_equal(Var1, VarArgs),
    member(OtherVar, VarArgs),
    OtherVar \== Var1,
    vars_related(OtherVar, Var2, Predicates), !.
