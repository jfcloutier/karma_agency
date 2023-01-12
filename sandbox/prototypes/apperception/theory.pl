:-module(theory, [theory/3]).

:- use_module(library(lists)).
:- use_module(type_signature).
:- use_module(domains).

% Generates a valid theory given a template.

/*
cd('sandbox/prototypes/apperception').
[leds_observations, type_signature, domains, sequence, theory].
MinPredicateTypes = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]) ], 
MinTypeSignature = type_signature{predicate_types:MinPredicateTypes, typed_variables:TypedVariables},
PredicateTypes = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]) ], 
TypedVariables = [variables(led, 2), variables(object1, 1)], 
TypeSignature = type_signature{predicate_types:PredicateTypes, typed_variables:TypedVariables},
Limits = limits{max_rules:3, max_elements:60},
Template = template{type_signature:TypeSignature, limits:Limits},
sequence(leds_observations, Sequence),
theory(MinTypeSignature, Template, Theory).
*/

% TODO: Use engines?

% template(type_signature: TypeSignature, max_rules: MaxRules, max_elements: MaxElements)
% type_signature(objects: Objects, predicate_types: PredicateTypes, typed_variables: TypedVariables)
theory(MinTypeSignature, Template, Theory) :-
    static_constraints(MinTypeSignature, Template.type_signature, StaticConstraints),
    static_rules(Template, StaticConstraints, StaticRules),
    causal_rules(Template, CausalRules),
    Theory = theory{static_rules:StaticRules, causal_rules:CausalRules, static_constraints:StaticConstraints}.

% Static rules
% Grow a set of static rules given a set of typed predicates and of typed variables such that
% * There is at least one rule
% * They do no exceed the limits set (max number of rules, max number of elements for the rule set)
% * No rule repeats another
% * No rule contradicts another
% * Rules can not contradict static constraints.
% Rules are constructed and compared as rule pairs, i.e., Head-Body pairs.
static_rules(Template, StaticConstraints, StaticRules) :-
    make_rule(Template.type_signature, Head-BodyPredicates),
    valid_static_rules([Head-BodyPredicates], Template.limits, StaticConstraints),
    maybe_add_static_rule(Template, StaticConstraints, [Head-BodyPredicates], RulePairs),
    pairs_rules(RulePairs, StaticRules).

% Rules that produce the changes in the next state from the current one (frame axiom)
% A predicate in head describes state at time t+1, the body describes state at time t.
% Rules must not exceed limits nor repeat themselves nor define non-change
causal_rules(Template, CausalRules) :-
    make_rule(Template.type_signature, Head-BodyPredicates),
    valid_causal_rules([Head-BodyPredicates], Template.limits),
    maybe_add_causal_rule(Template, [Head-BodyPredicates], RulePairs),
    pairs_rules(RulePairs, CausalRules).

% Unary constraints are implicit in value domains (an led's "on" property can not be both true and false at the same time).
% A binary constraint defines a set of predicates on objects X and Y, such that exactly one of a set of binary relations Relation(X,Y) must be true at any time.
%   one_relation([pred1, pred2, pred3]).
% A uniqueness constraint states that for an object X, there is exactly one object Y such that r(X, Y).
%   one_related(pred1). 
static_constraints(MinTypeSignature, TypeSignature, StaticConstraints) :-
    all_binary_predicate_names(TypeSignature, AllBinaryPredicateNames),
    nb_setval(visited_static_constraints, []),
    maybe_add_static_constraint(one_related, AllBinaryPredicateNames, [], StaticConstraints1),
    maybe_add_static_constraint(one_relation, AllBinaryPredicateNames, StaticConstraints1, StaticConstraints),
    valid_static_constraints(StaticConstraints, MinTypeSignature).

all_binary_predicate_names(TypeSignature, AllBinaryPredicateNames) :-
    PredicateTypes = TypeSignature.predicate_types,
    findall(BinaryPredicateName, binary_predicate_name(PredicateTypes, BinaryPredicateName), AllBinaryPredicateNames).

% Meta-constraint: Each (binary) predicate in the sequence appears in some static constraint
% Do not repeat previously visited static constraints
valid_static_constraints(StaticConstraints, MinTypeSignature) :-
    min_static_constraints_converage(StaticConstraints, MinTypeSignature),
    not_repetitive_static_constraints(StaticConstraints).

not_repetitive_static_constraints(StaticConstraints) :-
    nb_getval(visited_static_constraints, Visited),
    \+ repeats_visited_static_constraints(StaticConstraints, Visited),
    nb_setval(visited_static_constraints, [StaticConstraints | Visited]).

repeats_visited_static_constraints(StaticConstraints, Visited) :-
    member(VisitedStaticConstraints, Visited),
    length(StaticConstraints, L),
    length(VisitedStaticConstraints, L),
    subset(StaticConstraints, VisitedStaticConstraints).

min_static_constraints_converage(StaticConstraints, MinTypeSignature) :-
    all_binary_predicate_names(MinTypeSignature, AllBinaryPredicateNames),
    \+ (member(BinaryPredicateName, AllBinaryPredicateNames), \+ static_constraints_cover(StaticConstraints, BinaryPredicateName)).

static_constraints_cover(StaticConstraints, PredicateName) :-
    member(StaticConstraint, StaticConstraints),
    static_constraint_about(StaticConstraint, PredicateName), !.

static_constraint_about(one_related(PredicateName), PredicateName).
static_constraint_about(one_relation(PredicateNames), PredicateName) :-
    memberchk(PredicateName, PredicateNames).

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

binary_predicate_name(PredicateTypes, BinaryPredicateName) :-
    member(predicate(BinaryPredicateName, [object_type(_), object_type(_)]), PredicateTypes).

% Sublist of a list (order is preserved)
sublist([], []).
sublist(Sub, [_| Rest]) :-
    sublist(Sub, Rest).
sublist([X | Others], [X | Rest]) :-
    sublist(Others, Rest).

pairs_rules([], []).
pairs_rules([Head-BodyPredicates | OtherPairs], [Rule | OtherRules]) :-
    pair_rule(Head-BodyPredicates, Rule),
    pairs_rules(OtherPairs, OtherRules).

pair_rule(Head-BodyPredicates, Rule) :-
    and(BodyPredicates, Body),
    Rule =.. [:-, Head, Body].

% Make a rule made from predicates, objects and variables
% TypeSignature.predicate_types = [predicate(on, [object_type(led), value_type(boolean), ...]
% TypeSignature.typed_variables = [variables(led, 2), variables(object1, 1), ...]
make_rule(TypeSignature, Head-BodyPredicates) :-
    make_distinct_variables(TypeSignature.typed_variables, DistinctVars),
    make_head(TypeSignature.predicate_types, DistinctVars, Head),
    make_body_predicates(TypeSignature.predicate_types, DistinctVars, Head, BodyPredicates).
    
maybe_add_static_rule(_, _, RulePairs, RulePairs).

maybe_add_static_rule(Template, StaticConstraints, Acc, RulePairs) :-
    make_rule(Template.type_signature, Head-BodyPredicates),
    valid_static_rules([Head-BodyPredicates | Acc], Template.limits, StaticConstraints),!,
    maybe_add_static_rule(Template, StaticConstraints, [Head-BodyPredicates | Acc], RulePairs).

maybe_add_causal_rule(_, RulePairs, RulePairs).

maybe_add_causal_rule(Template, Acc, RulePairs) :-
    make_rule(Template.type_signature, Head-BodyPredicates),
    valid_causal_rules([Head-BodyPredicates | Acc], Template.limits),!,
    maybe_add_causal_rule(Template, [Head-BodyPredicates | Acc], RulePairs).

   
valid_static_rules(RulePairs, Limits, StaticConstraints) :-
    \+ rules_exceed_limits(RulePairs, Limits),
    % numerize vars in each copied rule pair before comparing them to one another
    % to avoid head1(X, Y) :- pred1(X, Y) being seen as equivalent to head1(X, Y) :- pred1(Y, X) etc.
    numerize_vars_in_rule_pairs(RulePairs, RulePairsWithNumberVars),
    \+ repeated_rules(RulePairsWithNumberVars),
    % No two rules that contradict one another
    \+ contradicting_rules(RulePairsWithNumberVars),
    % No rule contradicts a static constraint
    \+ contradicted_static_contraint(RulePairsWithNumberVars, StaticConstraints),
    % use un-numerized pairs b/c testing is based on unify-ability
    \+ recursion_in_rules(RulePairs).

valid_causal_rules(RulePairs, Limits) :-
    \+ rules_exceed_limits(RulePairs, Limits),
    % No idempotent rule (rules where the head is found in the body)
    \+ idempotent_causal_rules(RulePairs),
    numerize_vars_in_rule_pairs(RulePairs, RulePairsWithNumberVars),
    % No repeated rules
    \+ repeated_rules(RulePairsWithNumberVars).

rules_exceed_limits(RulePairs, Limits) :-
    length(RulePairs, NumberOfRules),
    NumberOfRules > Limits.max_rules, !.
    % format('Exceeding max rules of ~p~n', [MaxRules]).

rules_exceed_limits(RulePairs, Limits) :-
    count_atoms(RulePairs, NumberOfAtoms),
    NumberOfAtoms > Limits.max_elements, !.
    % format('Exceeding resource limit of ~p~n', [MaxElements]),

contradicted_static_contraint(RulePairs, StaticConstraints) :-
    member(Rule, RulePairs),
    member(StaticConstraint, StaticConstraints),
    static_rule_contradicts_constraint(Rule, StaticConstraint), !.

% There are at least two binary predicates in the body with names in PredicateNames and relating the same vars
static_rule_contradicts_constraint(_-BodyPredicates, one_relation(PredicateNames)) :-
    select(Pred1, BodyPredicates, OtherBodyPredicates),
    Pred1 =.. [PredName1, Var1, Var2],
    var_number(Var1, _),
    var_number(Var2 , _),
    member(Pred2, OtherBodyPredicates),
    Pred2 =.. [PredName2, Var1, Var2],
    subset([PredName1, PredName2], PredicateNames).


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

repeated_rules([RulePair | OtherRulePairs]) :- 
    member(OtherRulePair, OtherRulePairs),
    (rule_repeats(RulePair, OtherRulePair) -> true ; repeated_rules(OtherRulePairs)).

rule_repeats(Head-BodyPredicates, OtherHead-OtherBodyPredicates) :-
    equivalent_predicates(Head, OtherHead),
    equivalent_bodies(BodyPredicates, OtherBodyPredicates).

equivalent_bodies(BodyPredicates, OtherBodyPredicates) :-
    subsumed_conjunctions(BodyPredicates, OtherBodyPredicates) -> true ; subsumed_conjunctions(OtherBodyPredicates, BodyPredicates).

% True if each predicate in a body is equivalent to another predicate in the other body
subsumed_conjunctions([], _).
subsumed_conjunctions([Predicate | Rest], OtherPredicates) :-
    member(OtherPredicate, OtherPredicates),
    (equivalent_predicates(Predicate, OtherPredicate) -> subsumed_conjunctions(Rest, OtherPredicates)).

% A recusion exists when the head of a rule can unify with a predicate in the body of another rule.
recursion_in_rules(RulePairs) :-
    select(Head-_, RulePairs, OtherRulePairs),
    member(_-OtherBody, OtherRulePairs),
    member(OtherBodyPredicate, OtherBody),
    Head =@= OtherBodyPredicate.

% There are idempotent causal_rules is there is one where the head is found in the body -> nothing changes
idempotent_causal_rules(RulePairs) :-
    member(Head-BodyPredicates, RulePairs),
    memberchk_equal(Head, BodyPredicates), !.

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


% There exists two rules that contradict one another
contradicting_rules([Head-Body | OtherRulePairs]) :- 
    member(OtherHead-OtherBody, OtherRulePairs),
    (contradicting_heads(Head-Body, OtherHead-OtherBody) -> 
        true; 
        contradicting_bodies(Head-Body, OtherHead-OtherBody)
    ).

% Two rules contradict "at the head" one another if they have contradicting heads and equivalent bodies
contradicting_heads(Head-Body, OtherHead-OtherBody) :-
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

and([], []) :- true.
and([Goal], Goal) :- !.
and([Goal | Rest], Anded) :-
    and(Rest, Others),
    Anded =..  [(','), Goal, Others].
