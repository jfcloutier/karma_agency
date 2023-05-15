:- module(theory_engine_chr, [create_theory_engine/3]).

% An engine that generates valid causal theories on demand,
% given a minimal type signature (implied by the sequence of observations) 
% and a template (which sets the scope of the search space).

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
% :- use_module(unity).

:- use_module(library(chr)).
% Constraints

:- chr_option(check_guard_bindings, on).

:- chr_constraint model_module(+), 
                  deadline(+),
                  max_rules(+, +),
                  max_elements(+),
                  rules_count(+, +),
                  elements_count(+),
                  max_body_predicates(+),
                  enough_body_predicates/0,
                  posited_head_predicate(+),
                  posited_body_predicate(+),
                  body_predicate_count(+),
                  collected_body_predicate(-),
                  posited_static_constraint(+), 
                  static_constraint_covers(+),
                  posited_rule(+, +),
                  enough_rules(+),
                  posited_fact(+, +),
                  related(+, +),
                  object_in_a_fact(+),
                  close_facts/0,
                  objects_related(+, +).

% Time and complexity limits are singletons
'update deadline' @ deadline(_) \ deadline(_)#passive <=> true.
max_rules(Kind, _) \ max_rules(Kind, _)#passive <=> true. 
max_elements(_) \ max_elements(_)#passive <=> true. 
max_body_predicates(_) \ max_body_predicates(_)#passive <=> true.

% Positing static constraints
'adding static constraint fails after deadline' @ deadline(Deadline)#passive \ posited_static_constraint(_)  <=> 
    after_deadline(Deadline) | fail.
'Add constraints in alpahbetical order' @ posited_static_constraint(one_related(P1))#passive \ posited_static_constraint(one_related(P2)) <=> 
    P2 @< P1 | fail.
'fail on repeated static constraint' @ posited_static_constraint(C1)#passive \ posited_static_constraint(C2) <=> 
    subsumed_static_constraint(C1 , C2) | fail.

% Validating static constraints
'static constraint covers predicate' @ posited_static_constraint(C)#passive \ static_constraint_covers(BinaryPredicateName) <=> 
    static_constraint_about(C, BinaryPredicateName) | true.
'no static constraint covers predicate' @ static_constraint_covers(_) <=> fail.

% Positing rule head and body predicates
'new rule head' @ posited_head_predicate(_) \ posited_head_predicate(_)#passive <=> true.
'enough body predicates' @ max_body_predicates(Max)#passive \ body_predicate_count(Count)#passive, enough_body_predicates <=> Count == Max | true.
'not enough body predicates' @ enough_body_predicates <=> fail.
'too many rule body predicates' @ posited_body_predicate(_), max_body_predicates(Max), body_predicate_count(Count) ==> Count > Max | fail.
'body predicates in alphabetical order' @ posited_body_predicate(P1)#passive \ posited_body_predicate(P2) <=> predicates_out_of_order(P1, P2) | fail.
'repeated body predicate' @  posited_body_predicate(P)#passive \ posited_body_predicate(P) <=> fail.
'contradiction among body predicates' @ posited_body_predicate(P1)#passive \ posited_body_predicate(P2) <=> contradicts(P1, P2) | fail.
'valid body predicate' @ posited_body_predicate(_) \ body_predicate_count(Count)#passive <=> Count1 is Count + 1, body_predicate_count(Count1).
'first valid body predicate' @ posited_body_predicate(_) ==> body_predicate_count(1).
'collecting body predicates' @ collected_body_predicate(Collected), posited_body_predicate(P)#passive <=> Collected = P.
'done collecting body predicates' @  collected_body_predicate(_) <=> true.

% Positing rules, static and causal
'adding rule after deadline' @ deadline(Deadline)#passive \ posited_rule(_, _)  <=> 
    after_deadline(Deadline) | fail.
'enough rules' @ max_rules(Kind, Max)#passive, rules_count(Kind, Count)#passive \ enough_rules(Kind) <=> Count == Max | true.
'not enough rules' @ enough_rules(_) <=> fail.
'keep rule, count it' @ posited_rule(Kind, _) \ rules_count(Kind, Count)#passive <=> Count1 is Count + 1, rules_count(Kind, Count1).
'keep rule, start count' @ posited_rule(Kind, _)  ==> rules_count(Kind, 1).

% Validating rules, static and causal
'repeated rule' @ posited_rule(Kind, R1)#passive \ posited_rule(Kind, R2) <=>
    rule_repeats(R1, R2) | fail.
'too many relations in rule' @ posited_static_constraint(SC)#passive \ posited_rule(_, _-BodyPredicates) <=>
    too_many_relations(SC, BodyPredicates) | fail.

% Validating static rules
'static rule head contradicted in body' @ posited_rule(static, Head-Body) <=> contradicted_head(Head, Body) | fail.
'recursive static rule' @ posited_rule(static, SR) <=> recursive_static_rule(SR) | fail.
'contradicting static rules' @ posited_rule(static, SR1)#passive \ posited_rule(static, SR2) <=> contradicting_static_rules(SR1, SR2) | fail.
'static rules contradict a static constraint' @ posited_static_constraint(SC)#passive \ posited_rule(static, SR) <=> 
    static_rule_contradicts_constraint(SR, SC) | fail.
'static rules recurse' @ posited_rule(static, SR1)#passive \ posited_rule(static, SR2) <=> recursion_in_static_rules([SR1, SR2]) | fail.

% Validating causal rules
'idempotent causal rule' @ posited_rule(causal, CR) <=> idempotent_causal_rule(CR) | fail.

% Initial conditions
'reflexive relation' @ posited_fact(_, [A, A]) <=> fail.
'no duplicate fact' @ posited_fact(F, As)#passive \ posited_fact(F, As) <=> true.
'contradictory facts' @ posited_fact(N, [O, V1])#passive \ posited_fact(N, [O, V2]) <=>  is_domain_value(V1), is_domain_value(V2)| fail.
'related objects' @ posited_fact(_, [O1, O2]) ==>  is_object(O2) | related(O1, O2).
'no duplicate related' @ related(O1, O2) \ related(O1, O2) <=> true.
'inverse related' @ related(O1, O2) ==> related(O2, O1).
'transitive related' @ related(O1, O2),  related(O2, O3) ==> related(O1, O3).

% Initial conditions unified?
'object in a fact' @ posited_fact(_, A)#passive \ object_in_a_fact(O) <=>  member(O, A) | true.
'object not in a fact' @ object_in_a_fact(_) <=> fail.
% TODO - Factual Closure
'objects related' @ related(O1, O2)#passive \ objects_related(O1, O2)  <=> true.
'objects not related' @ objects_related(_, _) <=> fail.

% Create an engine that produces theories with their traces.
create_theory_engine(Template, SequenceAsTrace, TheoryEngine) :-
    engine_create(Theory-Trace, theory(Template, SequenceAsTrace, Theory, Trace), TheoryEngine), !.

theory(Template) :-
    theory_limits(Template),
    static_constraints(Template.type_signature),
    % Do iterative deepening on rule size
    max_rule_body_sizes(Template, MaxStaticBodySize, MaxCausalBodySize),
    static_rules(Template, MaxStaticBodySize),
    causal_rules(Template, MaxCausalBodySize),
    initial_conditions(Template).
    % TODO - Collect theory

theory_limits(Template) :-
    theory_deadline(Template.limits.max_theory_time),
    max_rules(static, Template.limits.max_static_rules),
    max_rules(causal, Template.limits.max_causal_rules),
    max_elements(Template.limits.max_elements).

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
    maybe_posit_static_constraint(one_related, AllBinaryPredicateNames),
    maybe_posit_static_constraint(one_relation, AllBinaryPredicateNames),
    static_constraints_conceptually_unified(AllBinaryPredicateNames),
    log(note, theory_engine, 'Done making static constraints').

maybe_posit_static_constraint(_, _).

maybe_posit_static_constraint(Kind, AllBinaryPredicateNames) :-
    static_constraint(Kind, AllBinaryPredicateNames, StaticConstraint),
    posited_static_constraint(StaticConstraint),
    maybe_posit_static_constraint(Kind, AllBinaryPredicateNames).

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
    foreach(member(BinaryPredicateName, AllBinaryPredicateNames), static_constraint_covers(BinaryPredicateName)).

static_constraint_about(one_related(PredicateName), PredicateName).
static_constraint_about(one_relation(PredicateNames), PredicateName) :-
    memberchk(PredicateName, PredicateNames).

%%% STATIC RULES

static_rules(Template, MaxBodySize) :-
    max_body_predicates(MaxBodySize),
    distinct_typed_variables(Template.type_signature.typed_variables, [], DistinctVars), !,
    posit_static_rules(Template, DistinctVars).

posit_static_rules(_, _) :-
    enough_rules(static), !.

posit_static_rules(Template, DistinctVars) :-
    rule_from_template(Template, DistinctVars, Head-BodyPredicates),
    posited_rule(static, Head-BodyPredicates),
    posit_static_rules(Template, DistinctVars).

%%% VALIDATING RULES

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
    Fact =.. [fact, ConstrainedPredicateName, [ObjectName1, ObjectName2]],
    % And there is another fact with a mutually exclusive predicate on these two objects
    member(OtherFact, OtherFacts),
    member(OtherConstrainedPredicateName, OtherConstrainedPredicateNames),
    OtherFact =.. [OtherConstrainedPredicateName, ObjectName1, ObjectName2],
    log(debug, unity, 'Multiple one_relation(~p) in facts ~p', [ConstrainedPredicateNames, Facts]).

% An object is related to multiple objects via a singular relation
too_many_relations(one_related(PredicateName), Facts) :-
    % There is an object that can be related to another by the predicate named in the constraint
    select(Fact, Facts, OtherFacts),
    Fact =.. [fact, PredicateName, [ObjectName, _]],
    member(OtherFact, OtherFacts),
    OtherFact =.. [PredicateName, ObjectName, _],
    log(debug, unity, 'Multiple one_related(~p) from ~p in facts ~p', [PredicateName, ObjectName, Facts]).

    
%%% VALIDATING STATIC RULES

contradicting_static_rules(Head-Body, OtherHead-OtherBody) :-
    (contradicting_heads(Head-Body, OtherHead-OtherBody) -> 
        log(debug, theory_engine, 'Contradicting rules!'),
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

% There are at least two binary predicates in the body with names in PredicateNames and relating the same vars
static_rule_contradicts_constraint(Head-BodyPredicates, one_relation(PredicateNames)) :-
    select(Pred1, BodyPredicates, OtherBodyPredicates),
    Pred1 =.. [PredName1, Var1, Var2],
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
    select(Head-_, RulePairs, OtherRulePairs),
    member(_-OtherBody, OtherRulePairs),
    member(OtherBodyPredicate, OtherBody),
    Head =@= OtherBodyPredicate,
    log(debug, theory_engine, 'Recursion on ~p in static rules ~p', [Head, RulePairs]).

% One of the body predicates contradicts the head predicate
contradicted_head(HeadPredicate, BodyPredicates) :-
    member(BodyPredicate, BodyPredicates),
    contradicts(BodyPredicate, HeadPredicate).

%%% CAUSAL RULES

causal_rules(Template, MaxBodySize) :-
    max_body_predicates(MaxBodySize),
    distinct_typed_variables(Template.type_signature.typed_variables, [], DistinctVars), !,
    posit_causal_rules(Template, DistinctVars).

posit_causal_rules(_, _) :-
    enough_rules(causal), !.

posit_causal_rules(Template, DistinctVars) :-
    rule_from_template(Template, DistinctVars, Head-BodyPredicates),
    NextHead =.. [next, Head],
    posited_rule(causal, NextHead-BodyPredicates),
    posit_causal_rules(Template, DistinctVars).

%%% VALIDATING CAUSAL RULES

idempotent_causal_rule(next(Head)-BodyPredicates) :-
    memberchk_equal(Head, BodyPredicates).

%%% MAKING RULES

rule_from_template(Template, DistinctVars, Head-BodyPredicates) :-
    TypeSignature = Template.type_signature,
    rule_predicate(TypeSignature.predicate_types, DistinctVars, Head),
    posited_head_predicate(Head),
    posit_body_predicates(Template, DistinctVars),
    collect_body_predicates(BodyPredicates).

collect_body_predicates([BodyPredicate | OtherBodyPredicates]) :-
    collected_body_predicate(BodyPredicate), 
    nonvar(BodyPredicate), !,
    collect_body_predicates(OtherBodyPredicates).

collect_body_predicates([]).

distinct_typed_variables([], DistinctVars, DistinctVars).
distinct_typed_variables(TypedVariables, Acc, DistinctVars) :-
    select(variables(Type, Count), TypedVariables, Rest),
    distinct_vars(Count, Vars),
    distinct_typed_variables(Rest, [vars(Type, Vars) | Acc], DistinctVars).

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

make_head_predicate(predicate(Name, TypedArgs), DistinctVars, HeadPredicate) :-
    make_args(TypedArgs, DistinctVars, Args), !, 
    HeadPredicate =.. [fact, Name , Args].

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
posit_body_predicates(_, _) :-
    enough_body_predicates, !.

posit_body_predicates(Template, DistinctVars) :-
    PredicateTypes = Template.type_signature.predicate_types,
    posit_body_predicate(PredicateTypes, DistinctVars),
    posit_body_predicates(Template, DistinctVars).   

posit_body_predicate(PredicateTypes, DistinctVars) :-
    rule_predicate(PredicateTypes, DistinctVars, BodyPredicate),
    posited_body_predicate(BodyPredicate).

rule_predicate(PredicateTypes, DistinctVars, RulePredicate) :-
    member(predicate(Name, TypedArgs), PredicateTypes),
    make_args(TypedArgs, DistinctVars, Args),
    RulePredicate =.. [fact, Name , Args].

%% Validating rule body predicates

predicates_out_of_order(fact(PredicateTypeName1, _), fact(PredicateTypeName2, _)) :-
    compare((<), PredicateTypeName2, PredicateTypeName1).

%%% INITIAL CONDITIONS

initial_conditions(Template) :-
    log(info, theory_engine, 'Making initial conditions'),
    posit_initial_conditions(Template).

% Posit facts to the initial conditions until they are unified
% TODO - conditions that fail the unified_facts assumption
%      1. Transitively unrelated objects
%      2. Contradictory facts under static closure
%      
posit_initial_conditions(Template) :-
    facts_unified(Template.type_signature), !.

posit_initial_conditions(Template) :-
    posit_fact(Template),
    posit_initial_conditions(Template).

posit_fact(Template) :-
   member(predicate(PredicateName, ArgTypes), Template.type_signature.predicate_types),
   ground_args(ArgTypes, Template.type_signature.objects, Args),
   posited_fact(PredicateName, Args).

ground_args([], _, []).
ground_args([ArgType | OtherArgTypes], Objects, [Arg | OtherArgs]) :-
    ground_arg(ArgType, Objects, Arg),
    ground_args(OtherArgTypes, Objects, OtherArgs).

ground_arg(object_type(ObjectType), Objects, ObjectName) :-
    member(object(ObjectType, ObjectName), Objects).

ground_arg(value_type(Domain), _, Value) :-
    domain_is(Domain, Values),
    member(Value, Values).

% Validating initial conditions

is_domain_value(Value) :-
    is_domain_value(_, Value).

facts_unified(TypeSignature) :-
    % All objects are represented
    \+ unrepresented_object(TypeSignature.objects),
    % The static rules are applied, possibly positing new facts, without causing contradictions
    close_facts,
    % All objects are related directly or indirectly by the closed facts
    \+ unrelated_objects(TypeSignature).

unrepresented_object(Objects) :-
    member(object(_, ObjectName), Objects),
    \+ object_in_a_fact(ObjectName),
    log(info, theory_engine, 'Unrepresented object ~p in facts', [ObjectName]).

unrelated_objects(TypeSignature) :-
    object_name_pair(TypeSignature.objects, ObjectName1-ObjectName2),
    \+ objects_related(ObjectName1, ObjectName2),
    log(debug, unity, 'Unrelated objects ~p and ~p in facts!', [ObjectName1, ObjectName2]).

related_in_fact(Fact, ObjectName, OtherObjectName) :-
    Fact =.. [_ | Args], member(ObjectName, Args), member(OtherObjectName, Args).

is_object(ObjectName, TypedObjects) :-
    member(object(_, ObjectName), TypedObjects).

% Some pair of objects from the type signature
object_name_pair(TypeSignatureObjects, ObjectName1-ObjectName2) :-
    select(object(_, ObjectName1), TypeSignatureObjects, RemainingSignatureObjects),
    member(object(_, ObjectName2), RemainingSignatureObjects).
 

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
    BodyPredicate =.. [fact, Name , Args1],
    Head =.. [fact, Name , Args2],
    contradicting_args(Args1, Args2).

contradicting_args([Arg1 | Rest1], [Arg2 | Rest2]) :-
   var(Arg1),
   var(Arg2), !,
   (Arg1 == Arg2 -> contradicting_args(Rest1, Rest2); fail).
contradicting_args([Arg1 | _], [Arg2 | _]) :-
    nonvar(Arg1),
    nonvar(Arg2),
    Arg1 \== Arg2, !.
contradicting_args([_ | Rest1], [_ | Rest2]) :-
    contradicting_args(Rest1, Rest2).

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

%% Utilities

equivalent_predicates(Predicate, OtherPredicate) :- 
    Predicate =.. [fact, PredicateName , Args],
    OtherPredicate =.. [fact, PredicateName , OtherArgs],
    equivalent_args(Args, OtherArgs).

equivalent_args([], []).
equivalent_args([Arg | Rest], [OtherArg | OtherRest]) :-
    var(Arg),
    var(OtherArg),
    !,
    equivalent_args(Rest, OtherRest).
equivalent_args([Arg | Rest], [Arg | OtherRest]) :-
    equivalent_args(Rest, OtherRest).


% Sublist of a list (order is preserved)
sublist([], []).
sublist(Sub, [_| Rest]) :-
    sublist(Sub, Rest).
sublist([X | Others], [X | Rest]) :-
    sublist(Others, Rest).

memberchk_equal(_, []) :- !, fail.
memberchk_equal(_, List) :- var(List), !, fail.
memberchk_equal(Term, [El | Rest]) :-
    Term == El -> true ; memberchk_equal(Term, Rest).
