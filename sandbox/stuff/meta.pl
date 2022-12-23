:-module(meta, [implied_by/0, causal_rule/3]).

:- dynamic(ok/1).

:- use_module(library(lists)).

known(b).
believed_by(me, b).
approved_by(other, b).

domain_is(boolean, [true, false]).
domain_is(color, [red, green, blue]).

implied_by() :-
    retractall(ok(_)),
    Head =.. [ok, X],
    and([known(X), believed_by(Y, X), approved_by(Z, X), dif(Y, Z)], Body),
    Clause =.. [:-, Head, Body],
    assertz(Clause),
    listing(ok/1).

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
% Use all variables in the body
% Do not repeat the head in the body
make_body(TypedPredicates, TypedVars, Head, tbd).

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
    % constraint
    dif(Var, Other),
    distinguish_from(Var, Rest).

and([], _) :- true.
and([Goal], Goal).
and([Goal | Rest], Anded) :-
    and(Rest, Others),
    Anded =..  [(','), Goal, Others].


% cd('sandbox/stuff').
% [meta].
% implied_by().
% call(meta:ok(X)).
%
% make_distinct_variables([variables(led, 2), variables(grid, 2)], Vars).
% make_distinct_variables([variables(led, 2), variables(grid, 2)], [vars(led, [V1, V2]), vars(grid, [V3])]), V1 = a, V2 = b.
%
% TypedPredicates = [predicate(on, [object_type(led), value_type(boolean)]), predicate(light, [object_type(led), value_type(color)]) ], TypedVariables = [variables(led, 2), variables(object1, 1)], causal_rule(TypedPredicates, TypedVariables, Rule).