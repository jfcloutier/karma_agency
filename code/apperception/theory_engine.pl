:- module(theory_engine, [create_theory_engine/4]).

% An engine that generates valid causal theories on demand,
% given a minimal type signature (implied by the sequence of observations) 
% and a template (which sets the scope of the search space).

/*
[load].
[code(logger), code(global), apperception(type_signature), apperception(theory_engine)].
[tests(apperception/leds_observations), tests(apperception/eca_observations)].
set_log_level(info).
ObjectTypes = [object_type(led)],
PredicateTypes = [predicate(on, [object_type(led), value_type(boolean)])],
Objects = [object(led, a), object(led, b)],
TypedVariables = [variables(led, 3)],
MinTypeSignature = type_signature{object_types:ObjectTypes, objects:Objects, predicate_types:PredicateTypes},
TypeSignature = type_signature{object_types:ObjectTypes, predicate_types:[predicate(pred_1, [object_type(led),  object_type(led)]) | PredicateTypes], objects:[object(led, object_1) | Objects], typed_variables:TypedVariables},
Limits = limits{max_static_rules:1, max_causal_rules: 1, max_elements:15, max_search_time:3},
sequence(leds_observations, Sequence),
apperception_engine:sequence_as_trace(Sequence, SequenceAsTrace),
Template = template{type_signature:TypeSignature, varying_predicate_names:[on], min_type_signature:MinTypeSignature, limits:Limits},
theory_engine:theory(Template, SequenceAsTrace, 0, Theory, Trace).
*/

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module(code(logger)).
:- use_module(apperception(domains)).
:- use_module(apperception(initial_conditions_primer)).
:- use_module(code(global)).
:- use_module(library(chr)).

% Constraints

:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type round == int.
:- chr_type rule_kind ---> static ; causal.
:- chr_type object == any.
:- chr_type value == any.
:- chr_type predicate_name == any.
% :- chr_type arg ---> object ; value.
:- chr_type fact ---> fact(predicate_name, list(any)). % I want it to be list(arg) but the CHR compiler complains
:- chr_type static_constraint ---> one_relation(list(any)) ; one_related(any).
:- chr_constraint deadline(+any, +any),
                  max_rules(+rule_kind, +),
                  empty_rule_set(+rule_kind),
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
                  read_constraint(+list(static_constraint), -static_constraint),
                  posited_rule(+rule_kind, +fact, +list(fact)),
                  enough_rules(+rule_kind),
                  round(+round),
                  bump_round(-round),
                  primed_fact(+fact),
                  is_primed(+fact),
                  posited_fact(+fact),
                  posited_fact(+fact, +round),
                  object_in_a_fact(+object),
                  predicate_applied(+predicate_name, +object),
                  predicate_caused(+predicate_name),
                  predicate_causing(+predicate_name),
                  relevant_rule(+rule_kind),
                  predicate_type_used_in_rule(+rule_kind, +predicate_name),
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
                  rule_about(+rule_kind, +predicate_name),
                  carry_over_composable_from(+round),
                  redundant_fact_in_this_round(+fact),
                  contradicted_in_this_round(+fact),
                  breaks_static_constraint_in_this_round(+fact),
                  many_rounds/0,
                  inferred_fact(+rule_kind, +fact, +round), % for debugging
                  last_round(-round),
                  extract_fact(-fact, +round),
                  extract_static_constraint(-static_constraint),
                  extract_rule(+rule_kind, -fact, -list(fact)),
                  % UNUSED
                  end_time(+any, +float),
                  time_ended(+any, +float, +any).

:- chr_option(check_guard_bindings, on).
% Setting optimize to full and debug to off causes an is/2 instantiation error.
:- chr_option(optimize, off).
:- chr_option(debug, on).

% Time and complexity limits are singletons
'update deadline' @ deadline(_, _) \ deadline(_, _)#passive <=> true.
max_rules(Kind, _) \ max_rules(Kind, _)#passive <=> true.
max_body_predicates(_) \ max_body_predicates(_)#passive <=> true.

% UNUSED
'replace timer' @ end_time(T, _) \ end_time(T, _)#passive <=> true.
'timer done' @ end_time(T, Time1)#passive, time_ended(T, Time2, B) <=> Time2 > Time1, B = true | true.
'timer not done' @ time_ended(_, _, B) <=> B = false | true.

% Positing static constraints
'static constraint after deadline' @ deadline(Deadline, MaxTime) \ posited_static_constraint(_)  <=> 
     after_deadline(Deadline, MaxTime) | fail.
'constraints not in alpahbetical order' @ posited_static_constraint(one_related(P1))#passive \ posited_static_constraint(one_related(P2)) <=> 
    P2 @< P1 | fail.
'duplicate static constraint' @ posited_static_constraint(SC1)#passive \ posited_static_constraint(SC2) <=> 
    subsumed_static_constraint(SC1 , SC2) | fail.

% Validating static constraints
'static constraint covers predicate' @ posited_static_constraint(SC) \ static_constraint_covers(BinaryPredicateName) <=> 
    static_constraint_about(SC, BinaryPredicateName) | true.
'no static constraint covers predicate' @ static_constraint_covers(_) <=> fail.

% Reading static constraints
'read a constraint' @ posited_static_constraint(SC) \ read_constraint(AlreadyRead, Read) <=> \+ member(SC, AlreadyRead) | Read = SC.
'done reading constraints' @ read_constraint(_, _) <=> fail.

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
'adding rule after deadline' @ deadline(Deadline, MaxTime) \ posited_rule(_, _, _)  <=> 
    after_deadline(Deadline, MaxTime) | fail.
'when no rule is enough' @ max_rules(Kind, 0) \ enough_rules(Kind) <=> true.
'enough rules' @ max_rules(Kind, Max) \ enough_rules(Kind), rules_count(Kind, Count)  <=> Count == Max | true.
'not enough rules' @ enough_rules(_) <=> fail.

% Checking if rule set is empty
'empty rule set' @ max_rules(Kind, 0) \ empty_rule_set(Kind) <=> true.
'not empty rule set' @ empty_rule_set(_) <=> fail.

% Validating rules, static or causal
'repeated rule' @ posited_rule(Kind, H1, B1)#passive \ posited_rule(Kind, H2, B2) <=>
    rule_repeats(H1-B1, H2-B2) | fail.
'ungroundable head' @ posited_rule(_, H, B) <=> ungroundable_head(H-B) | fail.
'unrelated vars' @ posited_rule(_, H, B) <=> unrelated_vars(H-B) | fail.
'predicate used in rule' @ posited_rule(Kind, H, B) \ predicate_type_used_in_rule(Kind, Name) <=>  memberchk(fact(Name, _), [H | B]) | true.
'predicate not used in rule' @ predicate_type_used_in_rule(_,_) <=> fail.

% Validating static rules
'static rule head contradicted in body' @ posited_rule(static, Head, Body) <=> contradicted_head(Head, Body) | fail.
'recursive static rule' @ posited_rule(static, H, B) <=> recursive_static_rule(H-B) | fail.
'contradicting static rules' @ posited_rule(static, H1, B1)#passive \ posited_rule(static, H2, B2) <=> contradicting_static_rules(H1-B1, H2-B2) | fail.
'static rules contradict a static constraint' @ posited_static_constraint(SC)#passive \ posited_rule(static, H, B) <=> 
    static_rule_contradicts_constraint(H-B, SC) | fail.
'static rules recurse' @ posited_rule(static, H1, B1)#passive \ posited_rule(static, H2, B2) <=> recursion_in_static_rules([H1-B1, H2-B2]) | fail.

% Validating causal rules
'idempotent causal rule' @ posited_rule(causal, H, B) <=> idempotent_causal_rule(H-B) | fail.
'predicate caused' @ posited_rule(causal, fact(N, _), _) \ predicate_caused(N) <=> true.
'predicate not caused' @ predicate_caused(_) <=> false.
'predicate causing' @ posited_rule(causal, _, B) \ predicate_causing(N) <=> memberchk(fact(N, _), B) | true.
'predicate not causing' @ predicate_causing(_) <=> false.


% Keeping and counting a rule
'keep higher count' @ rules_count(Kind, C2) \ rules_count(Kind, C1) <=> C2 >= C1 | true. 
'keep rule, count it' @ posited_rule(Kind, _, _ ) \ rules_count(Kind, Count)#passive <=> Count1 is Count + 1, rules_count(Kind, Count1).
'keep rule, start count' @ posited_rule(Kind, _, _)  ==> rules_count(Kind, 1).

%%% Rounds
'new round replaces previous round' @ round(_) \ round(_)#passive  <=> true.

%%% Facts in rounds
'adding fact after deadline' @ deadline(Deadline, MaxTime) \ posited_fact(_)  <=> 
    after_deadline(Deadline, MaxTime) | fail.
'posited fact in current round' @ round(Round)#passive \ posited_fact(F) <=> posited_fact(F, Round).
'reflexive relation' @ posited_fact(fact(_, [A, A]), _) <=> fail.
'duplicate fact' @ posited_fact(fact(N, As), Round)#passive \ posited_fact(fact(N, As), Round) <=> fail.
'contradictory fact' @ posited_fact(fact(N, [O, V1]), Round)#passive \ posited_fact(fact(N, [O, V2]), Round) <=>  is_domain_value(V1), is_domain_value(V2) | fail.
'fact breaks static contraint' @ posited_static_constraint(SC)#passive, posited_fact(fact(N1, As1), Round)#passive \ posited_fact(fact(N2, As2), Round)  <=> breaks_static_constraint(SC, fact(N1, As1), fact(N2, As2)) | fail.

%%% Initial conditions (round 1)
'primed fact' @ primed_fact(F) ==> posited_fact(F).
'is primed fact' @ primed_fact(F)#passive \ is_primed(F) <=> true.
'is not primed fact' @ is_primed(_) <=> fail.
'fact posited in wrong order' @ posited_fact(F1, 1)#passive \ posited_fact(F2, 1) <=> facts_out_of_order(F1, F2) | fail.

% Facts unified in a round?
'object in a fact' @ round(Round), posited_fact(fact(_, A), Round) \ object_in_a_fact(O) <=>  member(O, A) | true.
'object not in a fact' @ object_in_a_fact(_) <=> fail.
'predicate applied to object' @ round(Round), posited_fact(fact(N, A), Round) \ predicate_applied(N, O) <=> member(O, A) | true.
'predicate not applied to object' @ predicate_applied(_,_) <=> false.
'relevant rule' @ posited_rule(Kind, H, B) \ relevant_rule(Kind) <=> supported_rule(H-B) | true.
'no relevant rule' @ relevant_rule(_) <=> fail.
'fact about something' @ round(Round), posited_fact(fact(N,_), Round) \ fact_about(N) <=> true.
'no fact about something' @ fact_about(_) <=> fail.

% Applying rules
'inferred known fact' @ posited_fact(Fact, Round)#passive \ inferred_fact(_, Fact, Round) <=> true.
'inferred new fact' @ inferred_fact(_, Fact, Round) ==> posited_fact(Fact, Round).
'applying rules' @ posited_rule(Kind, Head, Body), applying_rules(Kind) ==> evaluating_rule(Kind, Head, Body).
'applying rules started' @ applying_rules(_) <=> true.
'fact from static rule' @ round(Round) \ evaluating_rule(static, Head, []) <=> 
    ground(Head) | inferred_fact(static, Head, Round).
'fact from causal' @ round(Round) \ evaluating_rule(causal, Head, []) <=>
    ground(Head) | NextRound is Round + 1, inferred_fact(causal, Head, NextRound).
'reducing a rule' @ round(Round), posited_fact(fact(Name, GroundArgs), Round), 
                    evaluating_rule(Kind, Head, [fact(Name, Args) | Rest])   ==> 
    can_unify(GroundArgs, Args) | 
    copy_term_nat([Head, Args, Rest], [Head1, Args1, Rest1]), 
    Args1 = GroundArgs, evaluating_rule(Kind, Head1, Rest1).
'cleanup evaluating rules' @ done_applying_rules \ evaluating_rule(_,_,_) <=> true.
'done evaluating rules' @ done_applying_rules <=> true.

% Are objects related in a round?
'objects related?' @ round(Round), posited_fact(fact(_, [O1, O2]), Round) \ objects_related(O1, O2) <=> true.
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
'done removing round' @ remove_round(RemovedRound) <=> LastRound is RemovedRound - 1, round(LastRound).
% Bump round
'bump round' @ bump_round(PreviousRound), round(Round) <=> NextRound is Round + 1, PreviousRound = Round, round(NextRound).
% Carry over unclosable facts from previous round
'carry over unclosable' @ carry_over_unclosable_from(PreviousRound), round(Round), posited_fact(F, PreviousRound) ==>  
                                                                        fact_from_previous_round_unclosable(F), fact_from_previous_round_composable(F) | posited_fact(F, Round).
'done carrying over unclosable' @ carry_over_unclosable_from(_) <=> true.
'rule about' @ posited_rule(Kind, fact(Name, _), _) \ rule_about(Kind, Name) <=> true.
'no rule about' @ rule_about(_, _) <=> false.

% Carry over composable facts from previous round
'carry over composable' @ carry_over_composable_from(PreviousRound), round(Round), posited_fact(F, PreviousRound) ==>  
                                                                        fact_from_previous_round_composable(F) | posited_fact(F, Round).
'done composing' @ carry_over_composable_from(_) <=> true.

'redundant fact' @ round(Round), posited_fact(fact(N, As), Round) \ redundant_fact_in_this_round(fact(N, As)) <=> true.
'not redundant fact' @ redundant_fact_in_this_round(_) <=> fail.
'contradicted in round' @ round(Round), posited_fact(fact(N, [O, V2]), Round) \ contradicted_in_this_round(fact(N, [O, V1])) <=>  is_domain_value(V1), is_domain_value(V2), V1 \== V2 | true.
'not contradicted in round' @ contradicted_in_this_round(_) <=> fail.

'breaks static contraint in this round' @  posited_static_constraint(SC)#passive, round(Round)#passive, posited_fact(fact(N2, As2), Round) \ breaks_static_constraint_in_this_round(fact(N1, As1)) <=> breaks_static_constraint(SC, fact(N1, As1), fact(N2, As2)) | true.
'does not break static contraint in this round' @ breaks_static_constraint_in_this_round(_) <=> fail.

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
create_theory_engine(Template, SequenceAsTrace, Iteration, TheoryEngine) :-
    engine_create(Theory-Trace, theory(Template, SequenceAsTrace, Iteration, Theory, Trace), TheoryEngine), !.

theory(Template, SequenceAsTrace, Iteration, Theory, Trace) :-
    found_theory(false),
    theory_limits(Template, Iteration),
    static_constraints(Template.type_signature),
    % Do iterative deepening on rule sizes favoring simplicity
    rules(static, Template),
    rules(causal, Template),
    initial_conditions(Template, SequenceAsTrace),
    length(SequenceAsTrace, SequenceLength),
    trace(Trace, SequenceLength),
    theory_with_trace(Theory, Trace),
    found_theory(true).

% Set a non-backtrackable global for the thread that a theory was found.
found_theory(Found) :-
    set_global(theory_engine, theory_was_found, Found).

% Was a theory ever found in this thread?
theory_found :- 
    get_global(theory_engine, theory_was_found, true).
                
theory_limits(Template, Iteration) :-
    iteration_template_deadline(Template, Iteration, Deadline),
    template_deadline(Deadline),
    max_rules(static, Template.limits.max_static_rules),
    max_rules(causal, Template.limits.max_causal_rules).

% Increase deadline by 50% with each iteration
iteration_template_deadline(Template, Iteration, Deadline) :-
    MaxTime = Template.limits.max_search_time,
    Deadline is round(MaxTime +(MaxTime * Iteration / 2)),
    log(note, apperception_engine, 'Max time for template ~p is ~p', [Template.id, Deadline]).

% Maximum time allocated to search the template for theories
template_deadline(MaxTime) :-
    get_time(Now),
    Deadline is Now + MaxTime,
    deadline(Deadline, MaxTime).

% Throw an error if 10 percent toward deadline and no theory was ever found in this thread
after_deadline(Deadline, MaxTime) :-
    \+ theory_found,
    get_time(Now),
    TimeLeft is Deadline - Now,
    TimeLeft < MaxTime * 0.9,
    TimeSpent is round(MaxTime - TimeLeft),
    log(note, theory_engine, 'Too much time spent without finding a theory in this template: ~p out of ~p secs', [TimeSpent, MaxTime]),
    throw(error(template_search_time_expired, context(theory_engine, 'too long without finding any theory'))).

% Throw error when template deadline is exceeded - stop searcing for theories in the template
after_deadline(Deadline, _) :-
    get_time(Now),
    Now > Deadline,
    log(note, theory_engine, 'Time expired searching for theories in this template'),
    throw(error(template_search_time_expired, context(theory_engine, 'template deadline reached'))).

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
    member_rand(BinaryPredicateName, AllBinaryPredicateNames),
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

rules(Kind, Template) :-
    max_body_size(static, Template, MaxBodySize),
    log(info, theory_engine, 'Making ~p rules with max body size ~p', [Kind, MaxBodySize]),    
    posit_rules(Kind, Template, MaxBodySize, 0),
    log(info, theory_engine, 'Done making ~p rules', [Kind]).

max_body_size(static, Template, Size) :-
    Size = Template.limits.max_static_rule_body_size.

max_body_size(causal, Template, Size) :-
    Size = Template.limits.max_causak_rule_body_size.

% There is enough rules and there are no unused predicates in them.
posit_rules(Kind, Template, _, _) :-
    enough_rules(Kind),
    valid_rule_set(Kind, Template).

% There are not enough rules and more can be added
posit_rules(Kind, Template, MaxBodySize, RuleCount) :-
    \+ enough_rules(Kind),
    % Build a rule that doe not exceed max body size
    min_body_size(Template, RuleCount, MaxBodySize, MinBodySize),
    between(MinBodySize, MaxBodySize, BodySize),
    max_body_predicates(BodySize),
    distinct_typed_variables(Template.type_signature.typed_variables, BodySize, [], DistinctVars),
    rule_from_template(Template, DistinctVars, Head-BodyPredicates),
    posited_rule(Kind, Head, BodyPredicates),
    RuleCount1 is RuleCount + 1,
    posit_rules(Kind, Template, MaxBodySize, RuleCount1).

% The first rule must be of at least max body size
min_body_size(Template, RuleCount, MaxBodySize, MinBodySize) :-
    RuleCount == 0 -> 
        MinBodySize = MaxBodySize
        ;
        length(Template.type_signature.predicate_types, MinBodySize).

% reverse_between(Low, Low, Low).
% reverse_between(Low, High, High) :-
%     Low < High.
% reverse_between(Low, High, N) :-
%     Low < High,
%     High1 is High - 1,
%     reverse_between(Low, High1, N).

%%% VALIDATING A RULE

% The head of a rule is ungroundable if it has a variable that never appears in the body of the rule
ungroundable_head(fact(_, HeadArgs)-Body) :-
    member(HeadArg, HeadArgs),
    var(HeadArg),
    \+ (member(fact(_, BodyArgs), Body),
       memberchk_equal(HeadArg, BodyArgs)
    ).

% A var is not connected to some other var in the rule
unrelated_vars(Head-Body) :-    
    all_vars([Head | Body], [], AllVars),
    all_var_pairs([Head | Body], [], VarPairs),
    select(Var1, AllVars, Rest),
    member(Var2, Rest),
    \+ vars_connected(Var1, Var2, VarPairs).

% There exists a predicate (by name) not used in a given kind of rules
unused_predicate_type(Kind, Template) :-
    all_predicate_names(Template.type_signature, PredicateNames),
    member(PredicateName, PredicateNames),
    \+ predicate_type_used_in_rule(Kind, PredicateName),
    log(info, theory_engine, 'No predicate ~p is being used in ~p rules', [PredicateName, Kind]).

% The set of variables in predicates
all_vars([], AllVars, AllVars).
all_vars([fact(_, Args) | OtherPredicates], Acc, AllVars) :-
    var_args(Args, Acc, VarArgs),
    all_vars(OtherPredicates, VarArgs, AllVars).

% The set of vars in a predicate's arguments
var_args([], VarArgs, VarArgs).
var_args([Arg | OtherArgs], Acc, VarArgs) :-
    var(Arg),
    \+ memberchk_equal(Arg, Acc),
    !,
    var_args(OtherArgs,[Arg | Acc], VarArgs).
var_args([_ | Args], Acc, VarArgs) :-
    var_args(Args, Acc, VarArgs).

% Al directed pairs directly connecting variables in a rule
all_var_pairs([], VarPairs, VarPairs).
all_var_pairs([fact(_, [Arg1, Arg2]) | OtherPredicates], Acc, VarPairs) :-
    var(Arg1),
    var(Arg2),
    \+ memberchk_equal(Arg1-Arg2, Acc),
    !,
    all_var_pairs(OtherPredicates, [Arg1-Arg2 | Acc], VarPairs).
all_var_pairs([_ | Predicates], Acc, VarPairs) :- all_var_pairs(Predicates, Acc, VarPairs).

% Whether two variable are directly or indirectly connected, in any direction.
vars_connected(Var1, Var2, Pairs) :-
  vars_directly_connected(Var1, Var2, Pairs), !.

vars_connected(Var1, Var2, Pairs) :-
  vars_indirectly_connected(Var1, Var2, Pairs).

vars_directly_connected(Var1, Var2, [V1-V2 | _]) :-
    (Var1 == V1,
    Var2 == V2
    ;
    Var1 == V2,
    Var2 == V1
    ),
    !.

vars_directly_connected(Var1, Var2, [_ | Pairs]) :-
  vars_directly_connected(Var1, Var2, Pairs), !.

vars_indirectly_connected(Var1, Var2, Pairs) :-
  select(Var1-Var3, Pairs, Rest),
  Var2 \== Var3,
  vars_connected(Var3, Var2, Rest), 
  !.

vars_indirectly_connected(Var1, Var2, Pairs) :-
  select(Var3-Var1, Pairs, Rest),
  Var2 \== Var3,
  vars_connected(Var3, Var2, Rest).

%%% VALIDATING A STATIC RULE

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

%%% VALIDATING A CAUSAL RULE

idempotent_causal_rule(Head-BodyPredicates) :-
    memberchk_equal(Head, BodyPredicates).

%%% VALIDATING A RULE SET

% An empty rule set is always valid
valid_rule_set(Kind, _) :-
    empty_rule_set(Kind),
    !.

% A static rule set is valid if there are no unused predicate types
valid_rule_set(static, Template) :-
    \+ unused_predicate_type(static, Template).

% A causal rule set is valid if there are no unused predicate types,
% all predicates observed to vary are in the head of a rule and in the body of a rule.
valid_rule_set(causal, Template) :-
    \+ unused_predicate_type(causal, Template),
    \+ uncaused_varying_predicate(Template),
    \+ uncausing_varying_predicate(Template).

uncaused_varying_predicate(Template) :-
    member(PredicateName, Template.varying_predicate_names),
    \+ predicate_caused(PredicateName),
    log(debug, theory_engine, 'Uncaused predicate "~p" in rules', [PredicateName]).

uncausing_varying_predicate(Template) :-
    member(PredicateName, Template.varying_predicate_names),
    \+ predicate_causing(PredicateName),
    log(debug, theory_engine, 'Uncausing predicate "~p" in rules', [PredicateName]).

%%% MAKING RULES

rule_from_template(Template, DistinctVars, Head-BodyPredicates) :-
    posit_head_predicate(Template, DistinctVars, Head),
    posit_body_predicates(Template, DistinctVars),
    collect_body_predicates(BodyPredicates).

collect_body_predicates([BodyPredicate | OtherBodyPredicates]) :-
    collected_body_predicate(BodyPredicate), 
    nonvar(BodyPredicate), !,
    collect_body_predicates(OtherBodyPredicates).

collect_body_predicates([]).

distinct_typed_variables([], _, DistinctVars, DistinctVars).
% Don't create more variables of a type than the body size can accomodate
distinct_typed_variables(TypedVariables, MaxBodySize, Acc, DistinctVars) :-
    select(variables(Type, Count), TypedVariables, Rest),
    MaxVarCount is MaxBodySize + 1,
    VarCount is min(Count, MaxVarCount),
    distinct_vars(VarCount, Vars),
    !,
    distinct_typed_variables(Rest, MaxBodySize, [vars(Type, Vars) | Acc], DistinctVars).

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

% Make a predicate for the rule's head, preferring predicates made from the type signature of the observations
posit_head_predicate(Template, DistinctVars, HeadPredicate) :-
    all_predicate_names(Template.type_signature, AllPredicateNames),
    all_predicate_names(Template.min_type_signature, PreferredPredicateNames),
    member_rand(Name, PreferredPredicateNames, AllPredicateNames),
    member(predicate(Name, TypedArgs), Template.type_signature.predicate_types),
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
    select_rand(Arg, Vars, UnusedVars).

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

initial_conditions(Template, SequenceAsTrace) :-
    log(info, theory_engine, 'Making initial conditions'),
    round(1),
     % Prime the intial conditions (apply heuristics to accelerate the search for unified conditions)
    posit_from_observations(SequenceAsTrace),
    posit_from_unobserved(Template.type_signature, SequenceAsTrace),
    % Complete the initial conditions so that they are unified
    posit_other_initial_conditions(Template),
    log(info, theory_engine, 'Done making initial conditions').

posit_from_observations(SequenceAsTrace) :-
    facts_from_observations(SequenceAsTrace, Facts),
    posit_facts(Facts).

posit_from_unobserved(TypeSignature, SequenceAsTrace) :-
    read_constraints([], Constraints),
    facts_from_unobserved(TypeSignature, SequenceAsTrace, Constraints, Facts),
    posit_facts(Facts).

posit_facts([]).
posit_facts([Fact | Others]) :-
    primed_fact(Fact),
    !,
    log(debug, theory_engine, 'Posited observed fact ~p', [Fact]),
    posit_facts(Others).

% Get posited constraints
read_constraints(Collected, Constraints) :-
    read_constraint(Collected, ReadConstraint),
    !,
    read_constraints([ReadConstraint | Collected], Constraints).
read_constraints(Constraints, Constraints).

% Posit facts to the primed initial conditions until they are unified
% conditions that fail the unified_facts assumption
%      1. Transitively unrelated objects
%      2. Contradictory facts under static closure
%      3. Unrepresented objects
%      4. Under-described objects (a predicate applicable to an object is not applied)

posit_other_initial_conditions(Template) :-
    facts_unified(Template.type_signature, Template.limits.max_static_rules), !,
    log(info, theory_engine, 'Initial conditions are unified').

posit_other_initial_conditions(Template) :-
    posit_fact(Template),
    posit_other_initial_conditions(Template).

posit_fact(Template) :-
   member_rand(predicate(PredicateName, ArgTypes), Template.type_signature.predicate_types),
   ground_args(ArgTypes, Template.type_signature.objects, Args),
   Fact = fact(PredicateName, Args),
   posited_fact(Fact),
   log(debug, theory_engine, 'Posited ~p', [Fact]).

facts_out_of_order(Fact, OtherFact) :-
    (is_primed(Fact) ; is_primed(OtherFact)),
    !,
    fail.

facts_out_of_order(fact(PredicateTypeName1, _), fact(PredicateTypeName2, _)) :-
    compare((<), PredicateTypeName2, PredicateTypeName1), !.

facts_out_of_order(fact(PredicateTypeName, [Arg1, _]), fact(PredicateTypeName, [Arg2, _])) :-
    compare((<), Arg2, Arg1).

ground_args([], _, []).
ground_args([ArgType | OtherArgTypes], Objects, [Arg | OtherArgs]) :-
    ground_arg(ArgType, Objects, Arg),
    ground_args(OtherArgTypes, Objects, OtherArgs).

ground_arg(object_type(ObjectType), Objects, ObjectName) :-
    member_rand(object(ObjectType, ObjectName), Objects).

ground_arg(value_type(Domain), _, Value) :-
    domain_is(Domain, Values),
    member_rand(Value, Values).

% Validating initial conditions

is_domain_value(Value) :-
    is_domain_value(_, Value).

facts_unified(TypeSignature, MaxStaticRules) :-
    % All objects are represented
    \+ unrepresented_object(TypeSignature.objects),
    % No under-described object
    \+ underdescribed_object(TypeSignature),
    % The initial conditions are relevant to at least one static rule - if there  are static rules
    (MaxStaticRules > 0 -> 
        relevant_rule(static),
        % The static rules are applied, possibly positing new facts, without causing contradictions (else closure fails)
        apply_rules(static)
        ; 
        true),
    % All objects are related directly or indirectly by the closed facts
    \+ unrelated_objects(TypeSignature),
    % The closed initial conditions are relevant to at least one causal rule
    relevant_rule(causal).

% Apply static/causal rules to add facts in the current/next round inferred the rules
% Fails if contradictory or constraint-breaking facts would be added to a round.
apply_rules(Kind) :-
    applying_rules(Kind),
    done_applying_rules.

% One the given objects is not represented
unrepresented_object(Objects) :-
    member(object(_, ObjectName), Objects),
    \+ object_in_a_fact(ObjectName),
    log(debug, theory_engine, 'Unrepresented object ~p in facts', [ObjectName]).

% A predicate applicable to an object is not applied to it
underdescribed_object(TypeSignature) :-
    member(object(ObjectType, ObjectName), TypeSignature.objects),
    applicable_predicate_name(TypeSignature, ObjectType, PredicateName),
    \+ predicate_applied(PredicateName, ObjectName),
    log(debug, theory_engine, 'Under-described object ~p in facts', [ObjectName]).

% There are unrelated objects in the current round
unrelated_objects(TypeSignature) :-
    all_object_names(TypeSignature.objects, AllObjectNames),
    \+ (
        object_name_pair(AllObjectNames, ObjectName1-ObjectName2),
        delete(AllObjectNames, ObjectName1, Remaining1),
        delete(Remaining1, ObjectName2, Remaining2),
        objects_transitively_related(ObjectName1, ObjectName2, Remaining2)
    ),
    log(debug, unity, 'Unrelated objects ~p and ~p in facts!', [ObjectName1, ObjectName2]).

objects_transitively_related(ObjectName1, ObjectName2, _) :-
    objects_directly_related(ObjectName1, ObjectName2), !.

objects_transitively_related(SourceObjectName, TargetObjectName, ObjectNames) :-
    % Looking for intermediaries, one directly related to source and one directly related to target (can be the same)
    % And then going from there.
    member(ObjectName1, ObjectNames),
    objects_directly_related(SourceObjectName, ObjectName1),
    select(ObjectName2, ObjectNames, Remaining1),
    objects_directly_related(ObjectName2, TargetObjectName),
    delete(Remaining1, ObjectName2, Remaining2),
    objects_transitively_related(ObjectName1, ObjectName2, Remaining2),
    !,
    log(info, theory_engine, 'objects_tranisitively_related(~p, ~p)', [SourceObjectName, TargetObjectName]).

objects_directly_related(ObjectName, ObjectName) :- !.

 objects_directly_related(ObjectName1, ObjectName2) :-
    objects_related(ObjectName1, ObjectName2), !.

objects_directly_related(ObjectName1, ObjectName2) :-
    objects_related(ObjectName2, ObjectName1), !.

is_object(Name) :-
    \+ is_domain_value(Name).

% All object names in the type signature
all_object_names(TypeSignatureObjects, AllObjectNames) :-
    setof(ObjectName, member(object(_, ObjectName), TypeSignatureObjects), AllObjectNames).

% Some pair of objects
object_name_pair(ObjectNames, ObjectName1-ObjectName2) :-
    select(ObjectName1, ObjectNames, Remaining),
    member(ObjectName2, Remaining).

% A rule is supported if all of its body predicates have potentially matching facts (none of its body predicates are not supported by facts)
supported_rule(_-Body) :-
    \+ unsupported_body_predicate(Body).

unsupported_body_predicate(Body) :-
    member(fact(PredicateName, _), Body),
    \+ fact_about(PredicateName).

% Making a trace from a set of initial conditions.
% Its length (number of rounds) is not to exceed twice  that of the initial conditions.
% It fails if the trace has only one round (the initial conditions).

trace(Trace, SequenceLength) :-
   log(info, theory_engine, 'Making trace no longer than ~p', [SequenceLength]),
    % Grow a trace until it repeats
    grow_trace(SequenceLength),
    % Fail if making a trace did not produce rounds beyond initial conditions
    many_rounds, !,
    extract_trace(Trace),
    log(info, theory_engine, 'Made trace ~p', [Trace]).

grow_trace(SequenceLength) :-
     make_next_round,
     !,
    (done_growing_trace(SequenceLength) -> remove_last_round ; grow_trace(SequenceLength)).

grow_trace(_).

done_growing_trace(SequenceLength) :-
    too_many_rounds(SequenceLength) ; repeated_round.

too_many_rounds(SequenceLength) :-
    MaxRounds is SequenceLength, % * 2,
    last_round(LastRound),
    LastRound > MaxRounds.

% Can fail
make_next_round :-
    % Start populating the next round by adding those caused by the current round
    apply_rules(causal),
    % Move to the next round
    bump_round(PreviousRound),
    log(info, theory_engine, 'Made round ~p', [PreviousRound]),
    % carry over composable facts that can not be inferred via static rules
    % so that static rules have all the facts they need to infer facts in the new round
    carry_over_unclosable_from(PreviousRound),
    apply_rules(static),
    % carry over all remaining composable facts now that the static rules have been applied
    carry_over_composable_from(PreviousRound).

% A fact that can not be statically inferred or caused (it had to be primed) is always composable, if it is not redundant.
fact_from_previous_round_composable(Fact) :-
   constant_fact(Fact),
   \+ redundant_fact_in_this_round(Fact),
   !.

fact_from_previous_round_composable(Fact) :-
    \+ redundant_fact_in_this_round(Fact),
    \+ contradicted_in_this_round(Fact),
    \+ breaks_static_constraint_in_this_round(Fact).

fact_from_previous_round_unclosable(fact(PredicateName, _)) :-
    \+ rule_about(static, PredicateName).

constant_fact(Fact) :-
    fact(PredicateName, _) = Fact,
    \+ rule_about(causal, PredicateName), 
    \+ rule_about(static, PredicateName).

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

theory_with_trace(Theory, [InitialConditions | _]) :-
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
    extract_rules(Kind, Others).

extract_rules(_, []).

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


% Get member of a randomly permuted list
member_rand(X, List) :-
    random_permutation(List, RandList),
    member(X, RandList).

% Get member of a randomly permutation list, with preferred elements upfront
member_rand(X, Preferred, All) :-
    subtract(All, Preferred, Rest),
    random_permutation(Preferred, Preferred1),
    random_permutation(Rest, Rest1),
    append(Preferred1, Rest1, List),
    member(X, List).

% Select randomly from a list
select_rand(X, List, Rest) :-
    random_permutation(List, RandList),
    select(X, RandList, Rest).

%%%% UNUSED

start_timer_to_trace :-
    start_timer(trace, 1000).

times_up_to_trace :-
    times_up(trace).

% Starts a timer (in msecs)
start_timer(Timer, Duration) :-
    get_time(T),
    Deadline is T + (Duration / 1000),
    end_time(Timer, Deadline),
     log(debug, theory_engine, 'Started timer ~p for ~p', [Timer, Duration]).

% Succeeds if the timer is done
times_up(Timer) :- 
    get_time(T),
    time_ended(Timer, T, true),
    log(debug, theory_engine, 'Times up for ~p', [Timer]).
