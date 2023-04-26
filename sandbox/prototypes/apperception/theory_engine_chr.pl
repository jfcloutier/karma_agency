:- module(theory_engine_chr, [create_theory_engine/3]).

% An engine that generates valid theories on demand,
% given a minimal type signature (from the sequence) and a template (which sets the scope of the search space).

/*
cd('sandbox/prototypes/apperception').
[logger, type_signature, theory_engine_chr].
set_log_level(note).
ObjectTypes = [led],
PredicateTypes = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(behind, [object_type(led),  object_type(led)]) ],
Objects = [object(led, light1), object(led, light2)],
TypedVariables = [variables(led, 2)],
TypeSignature = type_signature{object_types:ObjectTypes, predicate_types:PredicateTypes, objects:Objects, typed_variables:TypedVariables},
Limits = limits{max_static_rules:1, max_causal_rules: 1, max_elements:15, max_theory_time: 300},
Template = template{type_signature:TypeSignature, limits:Limits},
theory_engine_chr:theory(Template).
*/

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(logger).
:- use_module(type_signature).
:- use_module(domains).
:- use_module(global).
:- use_module(unity).
:- use_module(rules_engine).

:- use_module(library(chr)).
% Constraints

:- chr_option(check_guard_bindings, on).

:- chr_constraint model_module(+), 
                  deadline(+),
                  max_static_rules(+),
                  max_causal_rules(+),
                  max_elements(+),
                  static_rules_count(+),
                  causal_rules_count(+),
                  elements_count(+),
                  max_body_predicates(+),
                  enough_body_predicates/0,
                  head_predicate(+),
                  body_predicate_count(+),
                  model_static_constraint(+), 
                  model_static_constraint_covers(+),
                  model_static_rule(+),
                  enough_static_rules/0, 
                  model_causal_rule(+), 
                  model_initial_condition(+).

% Keep the latest deadline
'update deadline' @ deadline(_) \ deadline(_)#passive <=> true.

% Complexity limits are singletons
max_static_rules(_) \ max_static_rules(_)#passive <=> true. 
max_causal_rules(_) \ max_causal_rules(_)#passive <=> true. 
max_elements(_) \ max_elements(_)#passive <=> true. 
max_body_predicates(_) \ max_body_predicates(_)#passive <=> true.

% Model static constraints
'adding static constraint fails after deadline' @ deadline(Deadline)#passive \ model_static_constraint(_)  <=> 
    after_deadline(Deadline) | fail.
'Add constraints in alpahbetical order' @ model_static_constraint(one_related(P1))#passive \ model_static_constraint(one_related(P2)) <=> 
    P2 @< P1 | fail.
'fail on repeated static constraint' @ model_static_constraint(C1)#passive \ model_static_constraint(C2) <=> 
    subsumed_static_constraint(C1 , C2) | fail.

% Static constraints conceptually unified?
'static constraint covers predicate' @ model_static_constraint(C)#passive \ model_static_constraint_covers(BinaryPredicateName) <=> 
    static_constraint_about(C, BinaryPredicateName) | true.
'no static constraint covers predicate' @ model_static_constraint_covers(_) <=> fail.

% Rule making
head_predicate(_) \ head_predicate(_)#passive <=> true.
'too many rule body predicates' @ body_predicate(_), max_body_predicates(Max), body_predicate_count(Count) ==> Count > Max | fail.
% TODO
% no predicate out of alphabetical order
% all different
% no contradiction
'enough rule body predicates' @ max_body_predicates(Max), body_predicate_count(Count) \ enough_body_predicates <=> Count == Max.
% TODO - collecting body predicates

% Model static rules
'adding static rule after deadline' @ deadline(Deadline)#passive \ model_static_rule(_)  <=> 
    after_deadline(Deadline) | fail.
'too many static rules' @ max_static_rules(Max), static_rules_count(Count) ==> Count > Max | fail.
'enough static rules' @ max_static_rules(Max), static_rules_count(Count) \ enough_static_rules <=> Count == Max.
'repeated static rule' @ model_static_rule(SR1)#passive \ model_static_rule(SR2) <=>
    same_rules(SR1, SR2) | fail.
'recursive static rule' @ model_static_rule(SR) <=> recursive_static_rule(SR) | fail.
'too many relations in static rule' @ model_static_constraint(SC)#passive \ model_static_rule(_-BodyPredicates) <=>
    too_many_relations(SC, BodyPredicates) | fail.
'contradicting static rules' @ model_static_rule(SR1)#passive \ model_static_rule(SR2) <=> contradicting_static_rules(SR1, SR2) | fail.
'static rules contradict a static constraint' @ model_static_constraint(SC)#passive \ model_static_rule(SR) <=> 
    static_rule_contradicts_constraint(SR, SC) | fail.
'static rules recurse' @ model_static_rule(SR1)#passive \ model_static_rule(SR2) <=> recursion_in_static_rules([SR1, SR2]) | fail.
'keep static rule, count it' @ model_static_rule(_) \ static_rules_count(Count) <=> Count1 is Count + 1, static_rules_count(Count1).
'keep static rule, start count' @ model_static_rule(_) ==> static_rules_count(1).

% Create an engine that produces theories with their traces.
create_theory_engine(Template, SequenceAsTrace, TheoryEngine) :-
    engine_create(Theory-Trace, theory(Template, SequenceAsTrace, Theory, Trace), TheoryEngine), !.

theory(Template) :-
    theory_limits(Template),
    named_model_module,
    static_constraints(Template.type_signature),
    % Do iterative deepening on rule size
    max_rule_body_sizes(Template, MaxStaticBodySize, MaxCausalBodySize),
    static_rules(Template, MaxStaticBodySize).
    % TODO - Causal rules and initial conditions

theory_limits(Template) :-
    theory_deadline(Template.limits.max_theory_time),
    max_static_rules(Template.limits.max_static_rules),
    max_causal_rules(Template.limits.max_causal_rules),
    max_elements(Template.limits.max_elements).

named_model_module :-
    uuid(UUID),
    model_module(UUID).

theory_deadline(MaxTime) :-
    get_time(Now),
    Deadline is Now + MaxTime,
    deadline(Deadline).

after_deadline(Deadline) :-
    get_time(Now),
    Now > Deadline.

% A number between 1 and the greatest possible number of predicates in any rule
max_rule_body_sizes(Template, MaxStaticBodySize, MaxCausalBodySize) :-
    aggregate(sum(Count), PredicateType, predicate_instance_count(PredicateType, Template, Count), Total),
    % "Worst case" is all unary predicates
    UpperLimit is div(Template.limits.max_elements, 6),
    UpperLimit1 is min(UpperLimit, Total),
    between(1, UpperLimit1, MaxStaticBodySize),
    between(1, UpperLimit1, MaxCausalBodySize),
    log(note, theory_engine, 'Max static body size = ~p, max causal body size = ~p', [MaxStaticBodySize, MaxCausalBodySize]).


%%% STATIC CONSTRAINTS

% Unary constraints are implicit in value domains (an led's "on" property can not be both true and false at the same time).
% A binary constraint defines a set of predicates on objects X and Y, such that exactly one of a set of binary relations Relation(X,Y) must be true at any time.
%   one_relation([pred1, pred2, pred3]).
% A uniqueness constraint states that for an object X, there is exactly one object Y such that r(X, Y).
%   one_related(pred1). 
static_constraints(TypeSignature) :-
    log(note, theory_engine, 'Making static constraints'),
    all_binary_predicate_names(TypeSignature, AllBinaryPredicateNames),
    maybe_add_static_constraint(one_related, AllBinaryPredicateNames),
    maybe_add_static_constraint(one_relation, AllBinaryPredicateNames),
    static_constraints_conceptually_unified(AllBinaryPredicateNames),
    log(note, theory_engine, 'Done making static constraints').

maybe_add_static_constraint(_, _).

maybe_add_static_constraint(Kind, AllBinaryPredicateNames) :-
    static_constraint(Kind, AllBinaryPredicateNames, StaticConstraint),
    model_static_constraint(StaticConstraint),
    maybe_add_static_constraint(Kind, AllBinaryPredicateNames).

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

% Validating static constraints

subsumed_static_constraint(one_relation(PredicateNames), one_relation(OtherPredicateNames)) :-
    subset(PredicateNames, OtherPredicateNames) ; subset(OtherPredicateNames, PredicateNames).

subsumed_static_constraint(one_related(PredicateName), one_related(PredicateName)).

static_constraints_conceptually_unified(AllBinaryPredicateNames) :-
    foreach(member(BinaryPredicateName, AllBinaryPredicateNames), model_static_constraint_covers(BinaryPredicateName)).

static_constraint_about(one_related(PredicateName), PredicateName).
static_constraint_about(one_relation(PredicateNames), PredicateName) :-
    memberchk(PredicateName, PredicateNames).

%%% STATIC RULES

static_rules(Template, MaxStaticBodySize) :-
    max_body_predicates(MaxStaticBodySize),
    static_rules(Template).

static_rules(Template) :-
    make_static_rule(Template, Head-BodyPredicates),
    model_static_rule(HoldingHead-BodyPredicates),
    static_rules(Template).
    
static_rules(_) :-
    enough_static_rules.

make_static_rule(Template, HoldingHead-BodyPredicates) :-
    TypeSignature = Template.type_signature,
    make_distinct_variables(TypeSignature, MaxBodySize, DistinctVars),
    make_head(TypeSignature.predicate_types, DistinctVars, Head),
    static_head_predicate(Head),
    HoldingHead =.. [holds, Head],
    make_body_predicates(Template, DistinctVars),
    collect_static_body_predicates(BodyPredicates).

% TODO
collect_static_body_predicates(BodyPredicates).

%%% VALIDATING STATIC RULES

contradicting_static_rules(Head-Body, OtherHead-OtherBody) :-
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

recursive_static_rule(HoldingHead-BodyPredicates) :-
    HoldingHead =.. [holds, Head],
    member(BodyPredicate, BodyPredicates),
    Head =@= BodyPredicate.

% A recusion exists when the head of a static rule can unify with a predicate in the body of another static rule.
recursion_in_static_rules(RulePairs) :-
    select(HoldingHead-_, RulePairs, OtherRulePairs),
    HoldingHead =.. [holds, Head],
    member(_-OtherBody, OtherRulePairs),
    member(OtherBodyPredicate, OtherBody),
    Head =@= OtherBodyPredicate,
    log(debug, theory_engine, 'Recursion on ~p in static rules ~p', [HoldingHead, RulePairs]).

%%% MAKING RULES

% count each type.
% Create a list of count variables for each type
% Return pairs count-variables
make_distinct_variables(TypeSignature, MaxBodySize, DistinctVars) :-
    bagof(vars(Type, Vars), 
          N^(member(variables(Type, Count), TypeSignature.typed_variables), 
             adjusted_typed_variables_count(Type, Count, TypeSignature.predicate_types, MaxBodySize, N), 
             distinct_vars(N, Vars)), 
          DistinctVars).

% adjusted_typed_variables_count(_, Count, _, _, Count).
% Keep the number of typed variables small
adjusted_typed_variables_count(Type, Count, PredicateTypes, MaxBodySize, AdjustedCount) :-
    (member(predicate(_, [object_type(Type), object_type(Type)]), PredicateTypes), Order = 2; Order = 1), !,
    AdjustedCount is min(Count, MaxBodySize * Order).

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

make_head(PredicateTypes, DistinctVars, Head) :-
    member(PredicateType, PredicateTypes),
    make_head_predicate(PredicateType, DistinctVars, Head).

make_head_predicate(predicate(Name, TypedArgs), DistinctVars, HeadPredicate) :-
    make_args(TypedArgs, DistinctVars, Args), 
    HeadPredicate =.. [Name | Args].

make_args([], _, []).

make_args([TypedArg | OtherTypedArgs], DistinctVars, [Arg | OtherArgs]) :-
    make_arg(TypedArg, DistinctVars, UnusedDistinctVars, Arg),
    make_args(OtherTypedArgs, UnusedDistinctVars, OtherArgs).

make_arg(object_type(Type), DistinctVars, UnusedDistinctVars, Arg) :-
    take_typed_var(Type, DistinctVars, UnusedDistinctVars, Arg).

make_arg(value_type(Domain), DistinctVars, DistinctVars, Arg) :-
    domain_is(Domain,Values),
    member(Arg, Values).
    
take_typed_var(Type, [vars(Type, Vars) | OtherTypedVars], [vars(Type, UnusedVars) | OtherTypedVars], Arg) :-
    select(Arg, Vars, UnusedVars).

take_typed_var(Type, [TypedVar | OtherTypedVars], [TypedVar | UnusedTypedVars], Arg) :-
    TypedVar = vars(OtherType, _),
    Type \== OtherType,
    take_typed_var(Type, OtherTypedVars, UnusedTypedVars, Arg).


% Make a list of at least one mutually valid body predicates
make_body_predicates(Template, DistinctVars) :-
    PredicateTypes = Template.type_signature.predicate_types,
    make_body_predicate(PredicateTypes, DistinctVars, BodyPredicate),
    make_body_predicates(Template, DistinctVars).   

make_body_predicates(Template, DistinctVars) :-
    enough_body_predicates.

make_body_predicate(PredicateTypes, DistinctVars) :-
    member(PredicateType, PredicateTypes),
    body_predicate(PredicateType, DistinctVars, BodyPredicate),
    body_predicate(BodyPredicate).

body_predicate(predicate(Name, TypedArgs), DistinctVars, RulePredicate) :-
    make_args(TypedArgs, DistinctVars, Args),
    RulePredicate =.. [Name | Args].

% Validating rule body predicates

out_of_order(predicate(PredicateTypeName, _), Predicates) :-
    member(Predicate, Predicates),
    Predicate =.. [PredicateName | _],
    compare((<), PredicateTypeName, PredicateName).





%%% Utilities

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

same_rules(Rule, OtherRule) :-
    numerize_vars_in_rule_pair(Rule, NumerizedRule),
    numerize_vars_in_rule_pair(OtherRule, NumerizedOtherRule),
    rule_repeats(NumerizedRule, NumerizedOtherRule).

numerize_vars_in_rule_pair(HeadPredicate-BodyPredicates, HeadPredicate1-BodyPredicates1) :-
    numerize_vars([HeadPredicate | BodyPredicates], [HeadPredicate1 |BodyPredicates1]).

numerize_vars(Predicates, PredicatesWithNumerizedVars) :-
    copy_term_nat(Predicates, PredicatesWithNumerizedVars),
    numbervars(PredicatesWithNumerizedVars, 1, _).

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

predicate_instance_count(PredicateType, Template, Count) :-
    member(PredicateType, Template.type_signature.predicate_types),
    PredicateType = predicate(_, ArgTypes),
    args_unification_count(ArgTypes, Template.type_signature.typed_variables, 0, 1, Count).

args_unification_count([], _, _, Acc, Acc).

args_unification_count([value_type(_) | OtherArgTypes], TypedVariables, NthArg, Acc, Count) :-
    args_unification_count(OtherArgTypes, TypedVariables, NthArg, Acc, Count).

args_unification_count([object_type(ObjectType) | OtherArgTypes], TypedVariables, NthArg, Acc, Count) :-
    member(variables(ObjectType, VarCount), TypedVariables),
    VarCount1 is VarCount - NthArg,
    Acc1 is Acc * VarCount1,
    NthArg1 is NthArg + 1,
    args_unification_count(OtherArgTypes, TypedVariables, NthArg1, Acc1, Count).

% Sublist of a list (order is preserved)
sublist([], []).
sublist(Sub, [_| Rest]) :-
    sublist(Sub, Rest).
sublist([X | Others], [X | Rest]) :-
    sublist(Others, Rest).