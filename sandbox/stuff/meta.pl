:-module(meta, [causal_rule/3, all_rules/2]).

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
   Arg1 == Arg2 -> contradicting_args(Rest1, Rest2) ; fail.
contradicting_args([Arg1 | _], [Arg2 | _]) :-
    nonvar(Arg1),
    nonvar(Arg2),
   Arg1 \== Arg2.
contradicting_args([_ | Rest1], [_ | Rest2]) :-
    contradicting_args(Rest1, Rest2).


% Together, the body predicates cover all variables in the head.
% All variables are related in the head OR body
valid_body_predicates(BodyPredicates, Head) :-
    all_head_vars_in_body(Head, BodyPredicates),
    all_vars_related([Head | BodyPredicates]).

all_head_vars_in_body(Head, BodyPredicates) :-
    collect_vars([Head], HeadVars),
    collect_vars(BodyPredicates, BodyVars),
    all_members(HeadVars, BodyVars).

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

all_members([], _).
all_members([Item | Rest], List) :-
    memberchk_eq(Item, List),
    all_members(Rest, List).

memberchk_eq(_, []) :- !, fail.
memberchk_eq(_, List) :- var(List), !, fail.
memberchk_eq(Term, [El | Rest]) :-
    Term == El -> true ; memberchk_eq(Term, Rest).

different_predicate_from(Predicate, OtherPredicate) :-
    Predicate =.. [Name | Args],
    OtherPredicate =.. [OtherName, OtherArgs],
    Name == OtherName -> different_args(Args, OtherArgs); true.

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
    \+ unrelated_pair(Vars, Predicates).

unrelated_pair(Vars, Predicates) :-
    member(Var1, Vars),
    member(Var2, Vars),
    Var1 \== Var2,
    \+ related(Var1, Var2, Predicates).

related(Var1, Var2, Predicates) :-
    member(Predicate, Predicates),
    Predicate =.. [_ | Args],
    select_vars(Args, VarArgs),
    memberchk_eq(Var1, VarArgs),
    memberchk_eq(Var2, VarArgs), !.
related(Var1, Var2, Predicates) :-
    member(Predicate, Predicates),
    Predicate =.. [_ | Args],
    select_vars(Args, VarArgs),
    length(VarArgs, 2),
    memberchk_eq(Var1, VarArgs),
    member(OtherVar, VarArgs),
    OtherVar \== Var1,
    related(OtherVar, Var2), !.

and([], []) :- true.
and([Goal], Goal) :- !.
and([Goal | Rest], Anded) :-
    and(Rest, Others),
    Anded =..  [(','), Goal, Others].


% cd('sandbox/stuff').
% [meta].
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], causal_rule(TypedPredicates, TypedVariables, Rule).
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(next_to, [object_type(led),  object_type(led)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], all_rules(TypedPredicates, TypedVariables).