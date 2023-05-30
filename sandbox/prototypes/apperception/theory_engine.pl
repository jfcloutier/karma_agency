:- module(theory_engine, [create_theory_engine/2]).

% An engine that generates valid causal theories on demand,
% given a minimal type signature (implied by the sequence of observations) 
% and a template (which sets the scope of the search space).

/*
cd('sandbox/prototypes/apperception').
[logger, global, type_signature, theory_engine_chr].
set_log_level(info).
ObjectTypes = [led],
PredicateTypes = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(behind, [object_type(led),  object_type(led)]) ],
Objects = [object(led, light1), object(led, light2)],
TypedVariables = [variables(led, 2)],
TypeSignature = type_signature{object_types:ObjectTypes, predicate_types:PredicateTypes, objects:Objects, typed_variables:TypedVariables},
Limits = limits{max_static_rules:1, max_causal_rules: 1, max_elements:15, max_theory_time: 300},
Template = template{type_signature:TypeSignature, limits:Limits},
theory_engine:theory(Template, Theory, Trace).
*/

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(logger).
:- use_module(domains).
:- use_module(global).
:- use_module(library(chr)).

% Constraints

:- chr_option(check_guard_bindings, on).

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type round == int.
:- chr_type rule_kind ---> static ; causal.
:- chr_type object == any.
:- chr_type value == any.
:- chr_type predicate_name == any.
% :- chr_type arg ---> object ; value.
:- chr_type fact ---> fact(predicate_name, list(any)). % I want it to be list(arg) but the CHR compiler complains
:- chr_type static_constraint ---> one_relation(list(any)) ; one_related(any).
:- chr_constraint deadline(+any),
                  max_rules(+rule_kind, +),
                  max_elements(+int),
                  rules_count(+rule_kind, +int),
                  elements_count(+int),
                  max_body_predicates(+int),
                  enough_body_predicates/0,
                  posited_head_predicate(+fact),
                  posited_body_predicate(+fact),
                  body_predicate_count(+int),
                  collected_body_predicate(-fact),
                  posited_static_constraint(+static_constraint), 
                  static_constraint_covers(+predicate_name),
                  posited_rule(+rule_kind, +fact, +list(fact)),
                  enough_rules(+rule_kind),
                  round(+round),
                  bump_round(-round),
                  posited_fact(+fact),
                  posited_fact(+fact, +round),
                  related(+object, +object, +round),
                  object_in_a_fact(+object),
                  relevant_rule(+rule_kind),
                  fact_about(+any),
                  applying_rules(+rule_kind),
                  done_applying_rules/0,
                  evaluating_rule(+rule_kind, +fact, +list(fact)),
                  objects_related(+object, +object),
                  repeated_round/0,
                  facts_in_both_rounds(+round, +round),
                  repeated_fact(+fact),
                  count_repeated_facts/0,
                  repeated_count(+int),
                  extract_repeated_count(-int),
                  counting_facts/0,
                  count_facts/0,
                  counted_fact(+fact),
                  fact_count(+int),
                  extract_fact_count(-int),
                  remove_last_round/0,
                  remove_round(+round),
                  carry_over_unclosable_from(+round),
                  static_rule_about(+predicate_name),
                  carry_over_composable_from(+round),
                  redundant_fact_in_this_round(+fact),
                  contradicted_in_this_round(+fact),
                  breaks_static_constraint_in_this_round(+fact),
                  many_rounds/0,
                  inferred_fact(+rule_kind, +fact, +round), % for debugging
                  last_round(-round),
                  extract_fact(-fact, +round),
                  extract_static_constraint(-static_constraint),
                  extract_rule(+rule_kind, -fact, -list(fact)).

% Time and complexity limits are singletons
'update deadline' @ deadline(_) \ deadline(_)#passive <=> true.
max_rules(Kind, _) \ max_rules(Kind, _)#passive <=> true. 
max_elements(_) \ max_elements(_)#passive <=> true. 
max_body_predicates(_) \ max_body_predicates(_)#passive <=> true.

% Positing static constraints
'static constraint after deadline' @ deadline(Deadline) \ posited_static_constraint(_)  <=> 
     after_deadline(Deadline) | fail.
'constraints not in alpahbetical order' @ posited_static_constraint(one_related(P1))#passive \ posited_static_constraint(one_related(P2)) <=> 
    P2 @< P1 | fail.
'duplicate static constraint' @ posited_static_constraint(SC1)#passive \ posited_static_constraint(SC2) <=> 
    subsumed_static_constraint(SC1 , SC2) | fail.

% Validating static constraints
'static constraint covers predicate' @ posited_static_constraint(SC) \ static_constraint_covers(BinaryPredicateName) <=> 
    static_constraint_about(SC, BinaryPredicateName) | true.
'no static constraint covers predicate' @ static_constraint_covers(_) <=> fail.

% Positing rule head and body predicates
'one head predicate' @ posited_head_predicate(_) \ posited_head_predicate(_)#passive <=> true.
'set body predicate count to zero' @ posited_head_predicate(_) ==> body_predicate_count(0).
'enough body predicates' @ max_body_predicates(Max) \ enough_body_predicates,  body_predicate_count(Count)#passive <=> Count == Max | true.
'not enough body predicates' @ enough_body_predicates <=> fail.
'body predicates in alphabetical order' @ posited_body_predicate(P1)#passive \ posited_body_predicate(P2) <=> predicates_out_of_order(P1, P2) | fail.
'repeated body predicate' @  posited_body_predicate(P)#passive \ posited_body_predicate(P) <=> fail.
'contradiction among body predicates' @ posited_body_predicate(P1)#passive \ posited_body_predicate(P2) <=> contradicts(P1, P2) | fail.
'body predicate breaks static contraint' @ posited_static_constraint(SC), posited_body_predicate(P1)#passive \ posited_body_predicate(P2) <=> breaks_static_constraint(SC, P1, P2) | fail.
'another body predicate' @ posited_body_predicate(_) \ body_predicate_count(Count)#passive <=> Count1 is Count + 1, body_predicate_count(Count1).
'collecting body predicates' @ collected_body_predicate(Collected), posited_body_predicate(P) <=> Collected = P.
'done collecting body predicates' @  collected_body_predicate(_) <=> true.

% Positing rules, static and causal
 'adding rule after deadline' @ deadline(Deadline) \ posited_rule(_, _, _)  <=> 
    after_deadline(Deadline) | fail.
'enough rules' @ max_rules(Kind, Max) \ enough_rules(Kind), rules_count(Kind, Count)  <=> Count == Max | true.
'not enough rules' @ enough_rules(_) <=> fail.

% Validating rules, static or causal
'repeated rule' @ posited_rule(Kind, H1, B1)#passive \ posited_rule(Kind, H2, B2) <=>
    rule_repeats(H1-B1, H2-B2) | fail.
'ungroundable head' @ posited_rule(_, H, B) <=> ungroundable_head(H-B) | fail.

% Validating static rules
'static rule head contradicted in body' @ posited_rule(static, Head, Body) <=> contradicted_head(Head, Body) | fail.
'recursive static rule' @ posited_rule(static, H, B) <=> recursive_static_rule(H-B) | fail.
'contradicting static rules' @ posited_rule(static, H1, B1)#passive \ posited_rule(static, H2, B2) <=> contradicting_static_rules(H1-B1, H2-B2) | fail.
'static rules contradict a static constraint' @ posited_static_constraint(SC)#passive \ posited_rule(static, H, B) <=> 
    static_rule_contradicts_constraint(H-B, SC) | fail.
'static rules recurse' @ posited_rule(static, H1, B1)#passive \ posited_rule(static, H2, B2) <=> recursion_in_static_rules([H1-B1, H2-B2]) | fail.

% Validating causal rules
'idempotent causal rule' @ posited_rule(causal, H, B) <=> idempotent_causal_rule(H-B) | fail.

% Keeping and counting a rule
'keep rule, count it' @ posited_rule(Kind, _, _ ) \ rules_count(Kind, Count)#passive <=> Count1 is Count + 1, rules_count(Kind, Count1).
'keep rule, start count' @ posited_rule(Kind, _, _)  ==> rules_count(Kind, 1).

%%% Rounds
'new round replaces previous round' @ round(_) \ round(_)#passive  <=> true.

%%% Initial conditions
'adding fact after deadline' @ deadline(Deadline) \ posited_fact(_)  <=> 
    after_deadline(Deadline) | fail.
'posited fact in current round' @ round(Round)#passive \ posited_fact(F) <=> posited_fact(F, Round).
'reflexive relation' @ posited_fact(fact(_, [A, A]), _) <=> fail.
'duplicate fact' @ posited_fact(fact(N, As), Round)#passive \ posited_fact(fact(N, As), Round) <=> false.
'contradictory fact' @ posited_fact(fact(N, [O, V1]), Round)#passive \ posited_fact(fact(N, [O, V2]), Round) <=>  is_domain_value(V1), is_domain_value(V2) | fail.
'fact breaks static contraint' @ posited_static_constraint(SC)#passive, posited_fact(fact(N1, As1), Round)#passive \ posited_fact(fact(N2, As2), Round)  <=> breaks_static_constraint(SC, fact(N1, As1), fact(N2, As2)) | fail.

% Accumulating object relations
'remove duplicate relations' @ related(O1, O2, Round) \ related(O1, O2, Round) <=> true. 
'related objects' @ posited_fact(fact(_, [O1, O2]), Round) ==>  is_object(O1), is_object(O2) | related(O1, O2, Round).
'no duplicate related' @ related(O1, O2, Round) \ related(O1, O2, Round) <=> true.
'inverse related' @ related(O1, O2, Round) ==> related(O2, O1, Round).
'transitive related' @ related(O1, O2, Round),  related(O2, O3, Round) ==> related(O1, O3, Round).

% Initial conditions unified?
'object in a fact' @ round(Round), posited_fact(fact(_, A), Round) \ object_in_a_fact(O) <=>  member(O, A) | true.
'object not in a fact' @ object_in_a_fact(_) <=> fail.
'relevant rule' @ posited_rule(Kind, H, B) \ relevant_rule(Kind) <=> supported_rule(H-B) | true.
'no relevant rule' @ relevant_rule(_) <=> fail.
'fact about something' @ round(Round), posited_fact(fact(N,_), Round) \ fact_about(N) <=> true.
'no fact about something' @ fact_about(_) <=> fail.

% Applying rules
'inferred known fact' @ posited_fact(Fact, Round)#passive \ inferred_fact(_, Fact, Round) <=> true.
'inferred new fact' @ inferred_fact(_, Fact, Round) ==> posited_fact(Fact, Round).
'applying rules' @ posited_rule(Kind, Head, Body), applying_rules(Kind) ==> evaluating_rule(Kind, Head, Body).
'applying rules started' @ applying_rules(_) <=> true.
'fact from static rule' @ round(Round) \ evaluating_rule(static, Head, []) <=> ground(Head) | inferred_fact(static, Head, Round).
'fact from causal' @ round(Round) \ evaluating_rule(causal, Head, []) <=> ground(Head) | NextRound is Round + 1, inferred_fact(causal, Head, NextRound).
'reducing a rule' @ round(Round), posited_fact(fact(Name, GroundArgs), Round), evaluating_rule(Kind, Head, [fact(Name, Args) | Rest]) 
                                                                            ==> can_unify(GroundArgs, Args) | 
                                                                            copy_term_nat([Head, Args, Rest], [Head1, Args1, Rest1]), Args1 = GroundArgs, evaluating_rule(Kind, Head1, Rest1).
'cleanup evaluating rules' @ done_applying_rules \ evaluating_rule(_,_,_) <=> true.
'done evaluating rules' @ done_applying_rules <=> true.

% Spatial unity
'are objects related?' @ round(Round), related(O1, O2, Round)#passive \ objects_related(O1, O2)  <=> true.
'objects are not related' @ objects_related(_, _) <=> fail.

%%% Trace
% Repeated round
'last round repeats a prior one' @ round(Round) \ repeated_round <=> Round > 1, repeated_round(Round) | true.
'last round repeats - not' @ repeated_round <=> fail.

% counting facts and repeated facts

% rounds repeating facts
'a fact in both rounds' @ posited_fact(Fact, Round), posited_fact(Fact, OtherRound), facts_in_both_rounds(Round, OtherRound) ==> repeated_fact(Fact).
'done finding repeated facts' @ facts_in_both_rounds(_, _) <=> true.
'count repeated facts' @ count_repeated_facts \ repeated_count(Count), repeated_fact(_) <=> Count1 is Count + 1 | repeated_count(Count1).
'done counting repeated' @ repeated_count(_) \ count_repeated_facts <=> true.
'start count of repeated facts' @ count_repeated_facts ==> repeated_count(0).
'extract repeated count' @ extract_repeated_count(C), repeated_count(Count) <=> C = Count.
% counting facts
'mark facts for counting' @ counting_facts, round(Round), posited_fact(F, Round) ==> counted_fact(F).
'count facts' @ count_facts \ fact_count(Count), counted_fact(_) <=> Count1 is Count + 1 | fact_count(Count1).
'done counting facts' @ fact_count(_) \ count_facts <=> true.
'start count of facts' @ count_facts ==> fact_count(0).
'extract fact count' @ extract_fact_count(C), fact_count(Count) <=> C = Count.

% Removing a round
'remove last round' @ remove_last_round, round(Round)#passive <=> remove_round(Round).
'remove facts from last round' @ remove_round(Round) \ posited_fact(_, Round)#passive <=> true.
'remove related from last round' @ remove_round(Round) \ related(_, _, Round)#passive <=> true.
'done removing round' @ remove_round(RemovedRound) <=> LastRound is RemovedRound - 1, round(LastRound).
% Bump round
'bump round' @ bump_round(PreviousRound), round(Round) <=> NextRound is Round + 1, PreviousRound = Round, round(NextRound).
% Carry over unclosable facts from previous round
'carry over unclosable' @ carry_over_unclosable_from(PreviousRound), round(Round), posited_fact(F, PreviousRound) ==>  
                                                                        fact_from_previous_round_unclosable(F), fact_from_previous_round_composable(F) | posited_fact(F, Round).
'done carrying over unclosable' @ carry_over_unclosable_from(_) <=> true.
'static rule about' @ posited_rule(static, fact(Name, _), _) \ static_rule_about(Name) <=> true.
'no static rule about' @ static_rule_about(_) <=> false.

% Carry over composable facts from previous round
'carry over composable' @ carry_over_composable_from(PreviousRound), round(Round), posited_fact(F, PreviousRound) ==>  
                                                                        fact_from_previous_round_composable(F) | posited_fact(F, Round).
'done composing' @ carry_over_composable_from(_) <=> true.

'redundant fact' @ round(Round), posited_fact(fact(N, As), Round) \ redundant_fact_in_this_round(fact(N, As)) <=> true.
'not redundant fact' @ redundant_fact_in_this_round(_) <=> fail.
'contradicted in round' @ round(Round), posited_fact(fact(N, [O, V2]), Round) \ contradicted_in_this_round(fact(N, [O, V1])) <=>  is_domain_value(V1), is_domain_value(V2), V1 \== V2 | true.
'not contradicted in round' @ contradicted_in_this_round(_) <=> fail.

'breaks static contraint in next round' @ breaks_static_constraint_in_this_round(fact(N1, As1)), posited_static_constraint(SC)#passive, round(Round)#passive \ posited_fact(fact(N2, As2), Round)  <=> breaks_static_constraint(SC, fact(N1, As1), fact(N2, As2)) | true.
'does not break static contraint in next round' @ breaks_static_constraint_in_this_round(_) <=> fail.

% Validate trace
'trace has many rounds' @ round(Round) \ many_rounds <=> Round > 1 | true.
'trace does not have many rounds' @ many_rounds <=> fail.

% Extracting trace
'last round is' @ round(Round) \ last_round(R) <=> Round = R.
'extracting a fact from a round' @ extract_fact(F, R), posited_fact(Fact, Round) <=> R == Round | Fact = F.
'done extracting facts from a round' @ extract_fact(_,_) <=> fail.

% Extracting static constraints and rules
'extracting a static constraint' @ extract_static_constraint(Sc), posited_static_constraint(StaticConstraint) <=> Sc = StaticConstraint.
'done extracting static constraints' @ extract_static_constraint(_) <=> fail.
'extracting a rule' @ extract_rule(K, H, B), posited_rule(Kind, Head, Body) <=> K == Kind | Head = H, Body = B.
'done extracting rules' @ extract_rule(_, _, _) <=> fail.

% Create an engine that produces theories with their traces.
create_theory_engine(Template, TheoryEngine) :-
    engine_create(Theory-Trace, theory(Template, Theory, Trace), TheoryEngine), !.

theory(Template, Theory, Trace) :-
    theory_limits(Template),
    static_constraints(Template.type_signature),
    % Do iterative deepening on rule sizes
    max_rule_body_sizes(Template, MaxStaticBodySize, MaxCausalBodySize),
    rules(static, Template, MaxStaticBodySize),
    rules(causal, Template, MaxCausalBodySize),
    % Only try 10 sets of initial conditions in a row if getting no trace
    catch(
        (
        reset_counter(theory_engine/no_trace, 10),
        initial_conditions(Template),
        trace(Trace),
        extract_theory(Theory, Trace)
        ),
        error(no_trace, _),
        (
            log(info, theory_engine, 'CAN"T GET A TRACE!!!'),
            fail
        )
    ).

theory_limits(Template) :-
    theory_deadline(Template.limits.max_theory_time),
    max_rules(static, Template.limits.max_static_rules),
    max_rules(causal, Template.limits.max_causal_rules),
    max_elements(Template.limits.max_elements).

theory_deadline(MaxTime) :-
    get_time(Now),
    Deadline is Now + MaxTime,
    deadline(Deadline).

% Throw error when deadline is exceeded
after_deadline(Deadline) :-
    get_time(Now),
    Now > Deadline,
    throw(error(time_expired, context(theory_engine, Deadline))).

% A number between 1 and the greatest possible number of predicates in any rule
max_rule_body_sizes(Template, MaxStaticBodySize, MaxCausalBodySize) :-
    aggregate(sum(Count), PredicateType, predicate_instance_count(PredicateType, Template, Count), Total),
    % "Worst case" is all unary predicates
    UpperLimit is div(Template.limits.max_elements, 10),
    UpperLimit1 is min(UpperLimit, Total),
    choose_pair_in_range(1, UpperLimit1, MaxStaticBodySize-MaxCausalBodySize),
    % between(1, UpperLimit1, MaxStaticBodySize),
    % between(1, UpperLimit1, MaxCausalBodySize),
    log(note, theory_engine, 'Max static body size = ~p, max causal body size = ~p', [MaxStaticBodySize, MaxCausalBodySize]).

%%% STATIC CONSTRAINTS

% Unary constraints are implicit in value domains (an led's "on" property can not be both true and false at the same time).
% A binary constraint defines a set of predicates on objects X and Y, such that exactly one of a set of binary relations Relation(X,Y) must be true at any time.
%   one_relation([pred1, pred2, pred3]).
% A uniqueness constraint states that for an object X, there is exactly one object Y such that r(X, Y).
%   one_related(pred1). 
static_constraints(TypeSignature) :-
    log(info, theory_engine, 'Making static constraints'),
    all_binary_predicate_names(TypeSignature, AllBinaryPredicateNames),
    maybe_posit_static_constraint(one_related, AllBinaryPredicateNames),
    maybe_posit_static_constraint(one_relation, AllBinaryPredicateNames),
    static_constraints_conceptually_unified(AllBinaryPredicateNames),
    log(info, theory_engine, 'Done making static constraints').

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

%%% POSITING RULES

rules(Kind, Template, MaxBodySize) :-
    log(info, theory_engine, 'Making ~p rules', [Kind]),
    max_body_predicates(MaxBodySize),
    distinct_typed_variables(Template.type_signature.typed_variables, [], DistinctVars), !,
    posit_rules(Kind, Template, DistinctVars),
    log(info, theory_engine, 'Done making ~p rules', [Kind]).

posit_rules(Kind, _, _) :-
    enough_rules(Kind), 
    !.

posit_rules(Kind, Template, DistinctVars) :-
    rule_from_template(Template, DistinctVars, Head-BodyPredicates),
    posited_rule(Kind, Head, BodyPredicates),
    posit_rules(Kind, Template, DistinctVars).

%%% VALIDATING RULES

% The head of a rule is ungroundable if it has a variable that never appears in the body of the rule
ungroundable_head(fact(_, HeadArgs)-Body) :-
    member(HeadArg, HeadArgs),
    var(HeadArg),
    \+ (member(fact(_, BodyArgs), Body),
       memberchk_equal(HeadArg, BodyArgs)
    ).

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

recursive_static_rule(Head-BodyPredicates) :-
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

%%% VALIDATING CAUSAL RULES

idempotent_causal_rule(Head-BodyPredicates) :-
    memberchk_equal(Head, BodyPredicates).

%%% MAKING RULES

rule_from_template(Template, DistinctVars, Head-BodyPredicates) :-
    TypeSignature = Template.type_signature,
    posit_head_predicate(TypeSignature.predicate_types, DistinctVars, Head),
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

% Make a list of at least one mutually valid body predicates
posit_head_predicate(PredicateTypes, DistinctVars, HeadPredicate) :-
    member_rand(predicate(Name, TypedArgs), PredicateTypes),
    make_head_args(TypedArgs, DistinctVars, Args), 
    HeadPredicate =.. [fact, Name , Args],
    posited_head_predicate(HeadPredicate).

make_head_args([], _, []).

make_head_args([TypedArg | OtherTypedArgs], DistinctVars, [Arg | OtherArgs]) :-
    make_head_arg(TypedArg, DistinctVars, UnusedDistinctVars, Arg),
    make_head_args(OtherTypedArgs, UnusedDistinctVars, OtherArgs).

make_head_arg(object_type(Type), DistinctVars, UnusedDistinctVars, Arg) :-
    % Commit to the first var taken to avoid permutations of variables in the head.
    take_typed_var(Type, DistinctVars, UnusedDistinctVars, Arg), !.

make_head_arg(value_type(Domain), DistinctVars, DistinctVars, Arg) :-
    domain_is(Domain,Values),
    member_rand(Arg, Values).

posit_body_predicates(_, _) :-
    enough_body_predicates, !.

posit_body_predicates(Template, DistinctVars) :-
    PredicateTypes = Template.type_signature.predicate_types,
    posit_body_predicate(PredicateTypes, DistinctVars),
    posit_body_predicates(Template, DistinctVars).   

posit_body_predicate(PredicateTypes, DistinctVars) :-
    body_predicate(PredicateTypes, DistinctVars, BodyPredicate),
    posited_body_predicate(BodyPredicate).


body_predicate(PredicateTypes, DistinctVars, BodyPredicate) :-
    member_rand(predicate(Name, TypedArgs), PredicateTypes),
    make_body_args(TypedArgs, DistinctVars, Args),
    BodyPredicate =.. [fact, Name , Args].

make_body_args([], _, []).

make_body_args([TypedArg | OtherTypedArgs], DistinctVars, [Arg | OtherArgs]) :-
    make_body_arg(TypedArg, DistinctVars, UnusedDistinctVars, Arg),
    make_body_args(OtherTypedArgs, UnusedDistinctVars, OtherArgs).

make_body_arg(object_type(Type), DistinctVars, UnusedDistinctVars, Arg) :-
    take_typed_var(Type, DistinctVars, UnusedDistinctVars, Arg).

make_body_arg(value_type(Domain), DistinctVars, DistinctVars, Arg) :-
    domain_is(Domain,Values),
    member_rand(Arg, Values).

take_typed_var(Type, [vars(Type, Vars) | OtherTypedVars], [vars(Type, UnusedVars) | OtherTypedVars], Arg) :-
    select(Arg, Vars, UnusedVars).

take_typed_var(Type, [TypedVar | OtherTypedVars], [TypedVar | UnusedTypedVars], Arg) :-
    TypedVar = vars(OtherType, _),
    Type \== OtherType,
    take_typed_var(Type, OtherTypedVars, UnusedTypedVars, Arg).

%% Validating rule body predicates

predicates_out_of_order(fact(PredicateTypeName1, _), fact(PredicateTypeName2, _)) :-
    compare((<), PredicateTypeName2, PredicateTypeName1).

% Two mutually exclusive relations between the same objects or distinct variables
breaks_static_constraint(one_relation(ConstrainedPredicateNames), fact(Name, [A1, A2]), fact(OtherName, [A3, A4])) :-
    select(Name, ConstrainedPredicateNames, Rest),
    member(OtherName, Rest),
    A1 =@= A3,
    A2 =@= A4.

% A unitary relation links an object (or distinct variable) to more than one objects (or distinct variables)
breaks_static_constraint(one_related(Name), fact(Name, [A1, A2]), fact(Name, [A3, A4])) :-
    A1 =@= A3,
    A2 \=@= A4.


%%% INITIAL CONDITIONS

initial_conditions(Template) :-
    log(info, theory_engine, 'Making initial conditions'),
    round(1),
    posit_initial_conditions(Template),
    log(info, theory_engine, 'Done making initial conditions').

% Posit facts to the initial conditions until they are unified
% conditions that fail the unified_facts assumption
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
   posited_fact(fact(PredicateName, Args)).

ground_args([], _, []).
ground_args([ArgType | OtherArgTypes], Objects, [Arg | OtherArgs]) :-
    ground_arg(ArgType, Objects, Arg),
    ground_args(OtherArgTypes, Objects, OtherArgs).

ground_arg(object_type(ObjectType), Objects, ObjectName) :-
    member(object(ObjectType, ObjectName), Objects).

ground_arg(value_type(Domain), _, Value) :-
    domain_is(Domain, Values),
    member_rand(Value, Values).

% Validating initial conditions

is_domain_value(Value) :-
    is_domain_value(_, Value).

facts_unified(TypeSignature) :-
    % All objects are represented
    \+ unrepresented_object(TypeSignature.objects),
    % The initial conditions are relevant to at least one static rule
    relevant_rule(static),
    % The static rules are applied, possibly positing new facts, without causing contradictions (else closure fails)
    apply_rules(static),
    % All objects are related directly or indirectly by the closed facts
    \+ unrelated_objects(TypeSignature),
    % The closed initial conditions are relevant to at least one causal rule
    relevant_rule(causal).

% Apply static/causal rules to add facts in the current/next round inferred the rules
% Fails if contradictory or constraint-breaking facts would be added to a round.
apply_rules(Kind) :-
    applying_rules(Kind),
    done_applying_rules.

unrepresented_object(Objects) :-
    member(object(_, ObjectName), Objects),
    \+ object_in_a_fact(ObjectName),
    log(info, theory_engine, 'Unrepresented object ~p in facts', [ObjectName]).

% Are there unrelated objects in the current round?
unrelated_objects(TypeSignature) :-
    object_name_pair(TypeSignature.objects, ObjectName1-ObjectName2),
    \+ objects_related(ObjectName1, ObjectName2),
    log(debug, unity, 'Unrelated objects ~p and ~p in facts!', [ObjectName1, ObjectName2]).

related_in_fact(Fact, ObjectName, OtherObjectName) :-
    Fact =.. [_ | Args], member(ObjectName, Args), member(OtherObjectName, Args).

is_object(Name) :-
    \+ is_domain_value(Name).

% Some pair of objects from the type signature
object_name_pair(TypeSignatureObjects, ObjectName1-ObjectName2) :-
    select(object(_, ObjectName1), TypeSignatureObjects, RemainingSignatureObjects),
    member(object(_, ObjectName2), RemainingSignatureObjects).

% A rule is supported if all of its body predicates have potentially matching facts (none of its body predicates are not supported by facts)
supported_rule(_-Body) :-
    \+ unsupported_body_predicate(Body).

unsupported_body_predicate(Body) :-
    member(fact(PredicateName, _), Body),
    \+ fact_about(PredicateName).

% Making a trace from a set of initial conditions.
% It fails if the trace has only one round (the initial conditions).
% TODO - move the count down to number of initial conditions traced per rule set.
trace(Trace) :-
   log(info, theory_engine, 'Making trace'),
    make_trace,
    % Fail if making a trace did not produce rounds beyond initial conditions
    many_rounds, !,
    % restart the count of no trace in a row
    reset_counter(theory_engine/no_trace, 10),
    extract_trace(Trace).

% Throw an error if too many failed attempts at building a trace from initial conditions
% We don't want to try all possible initial conditions before giving up
trace(_) :-
    (count_down(theory_engine/no_trace) ->
        % backtrack and try another set of initial conditions
        fail
        ;
        % count down over - give up
        log(info, theory_engine, 'Done making trace'),
        throw(error(no_trace, context(theory_engine, trace)))
    ).

make_trace :-
     make_next_round,
     !,
    (repeated_round -> remove_last_round ; make_trace).

make_trace.

% Can fail
make_next_round :-
    apply_rules(causal),
    bump_round(PreviousRound),
    % carry over composable facts that can not be inferred via static rules
    carry_over_unclosable_from(PreviousRound),
    apply_rules(static),
    carry_over_composable_from(PreviousRound).

fact_from_previous_round_composable(Fact) :-
    \+ redundant_fact_in_this_round(Fact),
    \+ contradicted_in_this_round(Fact),
    \+ breaks_static_constraint_in_this_round(Fact).

fact_from_previous_round_unclosable(fact(PredicateName, _)) :-
    \+ static_rule_about(PredicateName).

% There is a prior round such that all facts in the round are repeated in the other round (i.e no fact in this round is unrepeated in the other round)
repeated_round(Round) :-
    PriorRound is Round - 1,
    between(1, PriorRound, OtherRound),
    \+ unrepeated_facts(Round, OtherRound).

% There is one or more facts in a round not repeated in some other round if
% the number of facts is in the round is different from the number of repeated facts.
unrepeated_facts(Round, OtherRound) :-
    facts_in_both_rounds(Round, OtherRound),
    count_repeated_facts,
    counting_facts,
    count_facts,
    extract_repeated_count(RepeatedCount),
    extract_fact_count(FactCount),
    RepeatedCount \== FactCount.

extract_trace(Trace) :-
    last_round(Last),
    extract_trace(1-Last, Trace).

extract_trace(N-Last, [Round | OtherRounds]) :-
    N =< Last,
    extract_facts(N, Round), !,
    N1 is N + 1,
    extract_trace(N1-Last, OtherRounds).

extract_trace(_, []).

extract_facts(N, [Predicate | Others]) :-
    extract_fact(fact(Name, Args), N),
    Predicate =.. [Name | Args],
    extract_facts(N, Others).

extract_facts(_, []).

%%% Extracting the theory

extract_theory(Theory, [InitialConditions | _]) :-
    extract_static_constraints(StaticConstraints),
    extract_rules(static, StaticRules),
    extract_rules(causal, CausalRules),
    Theory = theory{static_rules:StaticRules, causal_rules:CausalRules, static_constraints:StaticConstraints, initial_conditions:InitialConditions, rating: 0, found_time: 0}.

extract_static_constraints([StaticConstraint | Others]) :-
    extract_static_constraint(StaticConstraint), !,
    extract_static_constraints(Others).

extract_static_constraints([]).

extract_rules(Kind, [Head1-Body1 | Others]) :-
    extract_rule(Kind, Head, Body), !,
    defact([Head | Body], [Head1 | Body1]),
    extract_rules(Others).

extract_rules([]).

defact([], []).
defact([fact(Name, Args) | OtherFacts], [Predicate | OtherPredicates]) :-
    Predicate =.. [Name | Args],
    defact(OtherFacts, OtherPredicates).

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

can_unify(GroundArgs, Args) :-
    unifiable(GroundArgs, Args, _).

say(What, About) :-
    log(info, theory_engine, What, About).    

choose_pair_in_range(_, _, 2-2).

% choose_pair_in_range(Low, High, E1-E2) :-
%     bagof(N1-N2, (between(Low, High, N1), between(Low, High, N2)), Pairs),
%     predsort(ord_pair, Pairs, Sorted),
%     member(E1-E2, Sorted).

ord_pair(Comparison, X1-Y1, X2-Y2) :-
    S1 is X1 + Y1,
    S2 is X2 + Y2,
    (S1 =< S2 -> Comparison = '<' ; Comparison = '>').

% Get member of a randomly permuted list
member_rand(X, List) :-
    random_permutation(List, RandList),
    member(X, RandList).
