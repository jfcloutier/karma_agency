:-module(meta, [static_rules/5]).

% TODO: Use engines?

:- use_module(library(lists)).

domain_is(boolean, [true, false]).
domain_is(color, [red, green, blue]).

% Static rules
% Grow a set of static rules given a set of typed predicates and of typed variables such that
% * There is at least one rule
% * They do no exceed the limits set (max number of static rules, max number of atoms)
% * No rule repeats another
% * No rule contradicts another

static_rules(TypedPredicates, TypedVariables, MaxStatic, MaxAtoms, Rules) :-
    static_rule(TypedPredicates, TypedVariables, Head-BodyPredicates),
    valid_static_rules([Head-BodyPredicates], MaxStatic, MaxAtoms),
    maybe_add_static_rule(TypedPredicates, TypedVariables, MaxStatic, MaxAtoms, [Head-BodyPredicates], RulePairs),
    pairs_rules(RulePairs, Rules).

pairs_rules([], []).
pairs_rules([Head-BodyPredicates | OtherPairs], [Rule | OtherRules]) :-
    pair_rule(Head-BodyPredicates, Rule),
    pairs_rules(OtherPairs, OtherRules).

pair_rule(Head-BodyPredicates, Rule) :-
    and(BodyPredicates, Body),
    Rule =.. [:-, Head, Body].

maybe_add_static_rule(_, _, _, _, RulePairs, RulePairs).

maybe_add_static_rule(TypedPredicates, TypedVariables, MaxStatic, MaxAtoms, Acc, RulePairs) :-
    static_rule(TypedPredicates, TypedVariables, Head-BodyPredicates),
    valid_static_rules([Head-BodyPredicates | Acc], MaxStatic, MaxAtoms),!,
    maybe_add_static_rule(TypedPredicates, TypedVariables, MaxStatic, MaxAtoms, [Head-BodyPredicates | Acc], RulePairs).
   
valid_static_rules(RulePairs, MaxStatic, MaxAtoms) :-
    \+ rules_exceed_limits(RulePairs, MaxStatic, MaxAtoms),
    % numerize vars in each copied rule pair before comparing them to one another
    % to avoid head1(X, Y) :- pred1(X, Y) being seen as equivalent to head1(X, Y) :- pred1(Y, X) etc.
    numerize_vars_in_rule_pairs(RulePairs, RulePairsWithNumberVars),
    \+ repeated_rules(RulePairsWithNumberVars),
    \+ contradicting_rules(RulePairsWithNumberVars),
    \+ recursion_in_rules(RulePairs).

rules_exceed_limits(RulePairs, MaxRules, _) :-
    length(RulePairs, NumberOfRules),
    NumberOfRules > MaxRules, !.
    % format('Exceeding max rules of ~p~n', [MaxRules]).

rules_exceed_limits(RulePairs, _, MaxAtoms) :-
    count_atoms(RulePairs, NumberOfAtoms),
    NumberOfAtoms > MaxAtoms, !.
    % format('Exceeding resource limit of ~p~n', [MaxAtoms]),

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


equivalent_predicates(Predicate, OtherPredicate) :- 
    Predicate =.. [PredicateName | Args],
    OtherPredicate =.. [PredicateName | OtherArgs],
    equivalent_args(Args, OtherArgs).

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

contradicting_heads(Head-Body, OtherHead-OtherBody) :-
    contradicts(Head, OtherHead),
    equivalent_bodies(Body, OtherBody).

contradicting_bodies(Head-Body, OtherHead-OtherBody) :-
    equivalent_predicates(Head, OtherHead),
    contradicting_predicates(Body, OtherBody).

% There exists two predicates that contradict one another
contradicting_predicates([Predicate | Rest], OtherPredicates) :-
    contradiction_in([Predicate | OtherPredicates]) -> true ; contradicting_predicates(Rest, OtherPredicates).

% Assert a static rule made from predicates, objects and variables
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean), ...]
% TypedVariables = [variables(led, 2), variables(object1, 1), ...]
static_rule(TypedPredicates, TypedVariables, Head-BodyPredicates) :-
    make_distinct_variables(TypedVariables, TypedVars),
    make_head(TypedPredicates, TypedVars, Head),
    make_body_predicates(TypedPredicates, TypedVars, Head, BodyPredicates).
    
make_head(TypedPredicates, TypedVars, Head) :-
    member(TypedPredicate, TypedPredicates),
    make_rule_predicate(TypedPredicate, TypedVars, Head).

% Make sure that all vars used in the head also appear in the body
% Do not repeat a predicate in the head + body.
% Don't allow contradictions wthin head + body.
make_body_predicates(TypedPredicates, TypedVars, Head, BodyPredicates) :-
    make_body_predicate(TypedPredicates, TypedVars, Head, [], BodyPredicate),
    maybe_add_body_predicates(TypedPredicates, TypedVars, Head, [BodyPredicate], BodyPredicates),
    valid_body_predicates(BodyPredicates, Head).

make_body_predicate(TypedPredicates, TypedVars, Head, BodyPredicates, BodyPredicate) :-
    member(TypedPredicate, TypedPredicates),
    make_rule_predicate(TypedPredicate, TypedVars, BodyPredicate),
    valid_body_predicate(BodyPredicate, Head, BodyPredicates).

% Don't repeat the head predicate in the body or any body predicate
% Don't contradict other predicates
valid_body_predicate(BodyPredicate, Head, BodyPredicates) :-
    numerize_vars([Head, BodyPredicate | BodyPredicates], PredicatesWithNumerizedVars),
    all_different(PredicatesWithNumerizedVars),
    \+ contradiction_in(PredicatesWithNumerizedVars).

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
    var_number(Arg, _),
    var_number(OtherArg, _),
    !,
    \+ equivalent_vars(Arg, OtherArg).
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
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], static_rules(TypedPredicates, TypedVariables, 2, 20, Rules).
