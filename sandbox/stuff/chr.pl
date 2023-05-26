:- use_module(library(chr)).

/*
cd('sandbox/stuff').
[chr].

?- sample(1), sample(2), sample(3), bigger_samples, remove_samples.

?- fact(has_father, [will, jf]), fact(has_mother, [will, liz]),
   fact(has_father, [momo, jf]), fact(has_mother, [momo, liz]),
   rule(fact(parent, [X1, Y1]), [fact(has_father, [Y1, X1])]),
   rule(fact(parent, [X2, Y2]), [fact(has_mother, [Y2, X2])]),
   rule(fact(sibling, [X3, Y3]), [fact(parent, [Z3, X3]),fact(parent, [Z3, Y3])]),
   solve.
 
*/

:- chr_constraint sample(+), big_sample(+), remove_samples/0, bigger_samples/0, 
                  rule/2, fact/2, resolve/0, evaluate/2, cleanup/0.

%% For all
remove_samples \ sample(_)#passive <=> true.
remove_samples <=> true.

bigger_samples, sample(N)#passive ==> N1 is N * 10, big_sample(N1).
bigger_samples <=> true.

%% Mini Datalog
fact(Name, Args) \ fact(Name, Args) <=> true.
fact(sibling, [X, X]) <=> true.
fact(sibling, [X, Y]) \ fact(sibling, [Y, X]) <=> true.
rule(Head, Body), resolve ==> evaluate(Head, Body).
resolve <=> true.
evaluate(Head, []) <=> ground(Head) | Head.
fact(Name, GroundArgs),  evaluate(Head, [fact(Name, Args) | Rest]) ==>
    can_unify(GroundArgs, Args) | 
    copy_term([Head, Args, Rest], [Head1, Args1, Rest1]), 
    Args1 = GroundArgs, evaluate(Head1, Rest1).
cleanup \ evaluate(_, _) <=> true.
cleanup <=> true.  

solve :- resolve, cleanup.

can_unify(GroundArgs, Args) :-
    unifiable(GroundArgs, Args, _).
