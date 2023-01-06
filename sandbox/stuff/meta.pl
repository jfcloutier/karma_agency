:-module(meta, [causal_rule/3, all_rules/2, causal_rules/5, count_atoms/2]).

% TODO: Use engines?

:- use_module(library(lists)).

domain_is(boolean, [true, false]).
domain_is(color, [red, green, blue]).

all_rules(TypedPredicates, TypedVariables) :-
    retractall(on(_,_)),
    retractall(next_to(_,_)),
    retractall(light(_,_)),
    forall(causal_rule(TypedPredicates, TypedVariables, Rule), assertz(Rule)),
    tell('listings/on.pl'), listing(on(_,_)), told,
    tell('listings/next_to.pl'), listing(next_to(_,_)), told,
    tell('listings/light.pl'),listing(light(_,_)), told.

% Causal rules
% Grow a set of causal rules given a set of typed predicates and of typed variables such that
% * There is at least one rule
% * They do no exceed the resources allocated (max number of causal rules, max number of atoms)
% * No rule repeats another
% * No rule contradicts another

causal_rules(TypedPredicates, TypedVariables, MaxCausal, MaxAtoms, CausalRules) :-
    causal_rule(TypedPredicates, TypedVariables, CausalRule),
    valid_causal_rules([CausalRule], MaxCausal, MaxAtoms),
    maybe_add_causal_rule(TypedPredicates, TypedVariables, MaxCausal, MaxAtoms, [CausalRule], CausalRules).

maybe_add_causal_rule(_, _, _, _, CausalRules, CausalRules).

maybe_add_causal_rule(TypedPredicates, TypedVariables, MaxCausal, MaxAtoms, Acc, CausalRules) :-
    causal_rule(TypedPredicates, TypedVariables, CausalRule),
    valid_causal_rules([CausalRule | Acc], MaxCausal, MaxAtoms),
    maybe_add_causal_rule(TypedPredicates, TypedVariables, MaxCausal, MaxAtoms, [CausalRule | Acc], CausalRules).
   
valid_causal_rules(CausalRules, MaxCausal, MaxAtoms) :-
    \+ rules_exceed_resources(CausalRules, MaxCausal, MaxAtoms),
    \+ repeated_rule(CausalRules),
    \+ contradicting_rules(CausalRules).

rules_exceed_resources(Rules, MaxRules, _) :-
    length(Rules, NumberOfRules),
    NumberOfRules > MaxRules, !,
    format('Exceeding max rules of ~p~n', [MaxRules]).

rules_exceed_resources(Rules, _, MaxAtoms) :-
    count_atoms(Rules, NumberOfAtoms),
    NumberOfAtoms > MaxAtoms,
    format('Exceeding resource limit of ~p~n', [MaxAtoms]),
    !.

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

% TODO: This does not work
% Rules = [(on(_A, true):-next_to(_A, _B), on(_B, true)), (on(_C, true):-next_to(_C, _D), on(_D, true)), (on(_E, true):-next_to(_E, _F), on(_F, true))],
repeated_rule([Rule | OtherRules]) :- 
    member(OtherRule, OtherRules),
    rule_repeats(Rule, OtherRule).

rule_repeats(Rule, OtherRule) :-
    Rule =.. [:-, Head, Body],
    OtherRule =.. [:-, OtherHead, OtherBody],
    equivalent_predicates(Head, OtherHead),
    equivalent_bodies(Body, OtherBody).

equivalent_predicates(Predicate, OtherPredicate) :- 
    Predicate =.. [PredicateName | Args],
    OtherPredicate =.. [PredicateName | OtherArgs],
    equivalent_args(Args, OtherArgs).

equivalent_args([], []).
equivalent_args([Arg | Rest], [OtherArg | OtherRest]) :-
    var(Arg),
    var(OtherArg),
    !,
    equivalent_args(Rest, OtherRest).
equivalent_args([Arg | Rest], [Arg | OtherRest]) :-
    equivalent_args(Rest, OtherRest).

equivalent_bodies(Body, OtherBody) :-
    subsumed_conjunctions(Body, OtherBody) -> true ; subsumed_conjunctions(OtherBody, Body).

% True if all predicates in a body are equivalent to predicates in the other body
subsumed_conjunctions([], []).
subsumed_conjunctions([Predicate | Rest], [OtherPredicate | OtherRest]) :-
    equivalent_predicates(Predicate, OtherPredicate),
    subsumed_conjunctions(Rest, OtherRest).

% There exists two rules that contradict one another
% TODO: This is too broad b/c head1(X, Y) :- pred1(X, Y) contradicts head1(X, Y) :- pred1(Y, X) 
% even though they don't. Maybe first consistently rename the vars to constants.
contradicting_rules([Rule | OtherRules]) :- 
    Rule =.. [:-, Head, Body],
    member(OtherRule, OtherRules),
    OtherRule =.. [:-, OtherHead, OtherBody],
    equivalent_predicates(Head, OtherHead),
    contradicting_predicates(Body, OtherBody).

% There exists two predicates that contradict one another
contradicting_predicates([Predicate | Rest], [OtherPredicate | OtherRest]) :-
    contradiction_in([Predicate | OtherPredicate]) -> true ; contradicting_predicates(Rest, OtherRest).

% Assert a causal rule made from predicates, objects and variables
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean), ...]
% TypedVariables = [variables(led, 2), variables(object1, 1), ...]
causal_rule(TypedPredicates, TypedVariables, Rule) :-
    make_distinct_variables(TypedVariables, TypedVars),
    make_head(TypedPredicates, TypedVars, Head),
    make_body(TypedPredicates, TypedVars, Head, Body),
    Rule =.. [:-, Head, Body].
    
make_head(TypedPredicates, TypedVars, Head) :-
    member(TypedPredicate, TypedPredicates),
    make_rule_predicate(TypedPredicate, TypedVars, Head).

% Make sure that all vars used in the head also appear in the body
% Do not repeat a predicate in the head + body.
% Don't allow contradictions wthin head + body.
make_body(TypedPredicates, TypedVars, Head, Body) :-
    make_body_predicate(TypedPredicates, TypedVars, Head, [], BodyPredicate),
    maybe_add_body_predicates(TypedPredicates, TypedVars, Head, [BodyPredicate], BodyPredicates),
    valid_body_predicates(BodyPredicates, Head),
    and(BodyPredicates, Body).

make_body_predicate(TypedPredicates, TypedVars, Head, BodyPredicates, BodyPredicate) :-
    member(TypedPredicate, TypedPredicates),
    make_rule_predicate(TypedPredicate, TypedVars, BodyPredicate),
    valid_body_predicate(BodyPredicate, Head, BodyPredicates).

% Don't repeat the head predicate in the body or any body predicate
% Don't contradict other predicates
valid_body_predicate(BodyPredicate, Head, BodyPredicates) :-
    all_different([Head, BodyPredicate | BodyPredicates]),
    \+ contradiction_in([Head, BodyPredicate | BodyPredicates]).

all_different([]).
all_different([Term | Rest]) :-
    \+ memberchk_eq(Term, Rest),
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
   var(Arg1),
   var(Arg2),
   (Arg1 == Arg2 -> contradicting_args(Rest1, Rest2) ; fail).
contradicting_args([Arg1 | _], [Arg2 | _]) :-
    nonvar(Arg1),
    nonvar(Arg2),
   Arg1 \== Arg2.
contradicting_args([_ | Rest1], [_ | Rest2]) :-
    contradicting_args(Rest1, Rest2).


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
     memberchk_eq(Var, Args) -> predicates_with_var(Var, OtherPredicates, OtherPredicatesWithVar), PredicatesWithVar = [Predicate | OtherPredicatesWithVar] 
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
    memberchk_eq(Term, List), !,
    collate(Rest, List, Others).
collate([Term | Rest], List, [Term | Others]) :-
    collate(Rest, List, Others).

% Select without duplicates
select_vars([], []).
select_vars([Term | Rest], [Term | Vars]) :-
    var(Term),
    \+ memberchk_eq(Term, Rest),
    !,
    select_vars(Rest, Vars).
select_vars([_ | Rest], Vars) :-
    select_vars(Rest, Vars).

memberchk_eq(_, []) :- !, fail.
memberchk_eq(_, List) :- var(List), !, fail.
memberchk_eq(Term, [El | Rest]) :-
    Term == El -> true ; memberchk_eq(Term, Rest).

different_predicate_from(Predicate, OtherPredicate) :-
    Predicate =.. [Name | Args],
    OtherPredicate =.. [OtherName, OtherArgs],
    (Name == OtherName -> different_args(Args, OtherArgs); true).

different_args([], []).
different_args([Arg | Rest], [OtherArg | OtherRest]) :-
    different_arg(Arg, OtherArg),
    different_args(Rest, OtherRest).

different_arg(Arg, OtherArg) :-
    var(Arg),
    var(OtherArg),
    !,
    Arg \== OtherArg.
different_arg(_, _).

maybe_add_body_predicates(_, _, _, CurrentBodyPredicates, CurrentBodyPredicates).

maybe_add_body_predicates(TypedPredicates, TypedVars, Head, CurrentBodyPredicates, BodyPredicates) :-
    make_body_predicate(TypedPredicates, TypedVars, Head, CurrentBodyPredicates, BodyPredicate),
    valid_body_predicates([BodyPredicate | CurrentBodyPredicates], Head),
    maybe_add_body_predicates(TypedPredicates, TypedVars, Head, [BodyPredicate | CurrentBodyPredicates], BodyPredicates).

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
make_distinct_variables(Variables, TypedVars) :-
    bagof(vars(Type, Vars), N^(member(variables(Type, N), Variables), distinct_vars(N, Vars)), TypedVars).

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
    memberchk_eq(Var1, VarArgs),
    memberchk_eq(Var2, VarArgs).

% Two different vars are indirectly related across multiple predicates
indirectly_related_vars(Var1, Var2, VarArgs, Predicates) :-
    length(VarArgs, 2),
    memberchk_eq(Var1, VarArgs),
    member(OtherVar, VarArgs),
    OtherVar \== Var1,
    vars_related(OtherVar, Var2, Predicates), !.

and([], []) :- true.
and([Goal], Goal) :- !.
and([Goal | Rest], Anded) :-
    and(Rest, Others),
    Anded =..  [(','), Goal, Others].


% cd('sandbox/stuff').
% [meta].
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], causal_rule(TypedPredicates, TypedVariables, Rule).
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], all_rules(TypedPredicates, TypedVariables).
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], causal_rules(TypedPredicates, TypedVariables, 2, 20, Rules).
