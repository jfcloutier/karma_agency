:- module(extract,
          [ test/4
          ]).

:- use_module(library(chr)).

:- chr_option(check_guard_bindings, on).
:- chr_type list(T) ---> [] ; [T|list(T)].
:- chr_type boolean ---> true ; false.
:- chr_constraint   element(+, +), 
                    elements(+, +list(any)), 
                    extract(+, -list(any)), 
                    assumed(+, +, -boolean),
                    count(+, ?int),
                    counted(+, +list(any), ?int),
                    clear_all.

element(Kind, X) \ element(Kind, X) <=> true.
elements(Kind, L), element(Kind, X)#passive <=> elements(Kind, [X|L]).
extract(Kind, _) ==> elements(Kind, []).
extract(Kind, All)#passive, elements(Kind, Elements) <=> All = Elements.

element(Kind, X)#passive\assumed(Kind, X, Assumed) <=> Assumed = true.
assumed(_, _, Assumed) <=> Assumed = false.

count(Kind, _) ==> counted(Kind, [], 0).
element(Kind, X)#passive \ counted(Kind, Counted, Count) <=> \+ memberchk(X, Counted) | Count1 is Count+1, counted(Kind, [X|Counted], Count1).
count(Kind, Answer), counted(Kind, _, Count) <=> Answer = Count.

clear_all \ element(_,_) <=> true.
clear_all <=> true.

test(Kind, is_assumed(Kind, Element, Assumed), counting(Kind, Count), All) :-
    element(int, 1),
    element(int, 2),
    element(int, 3),
    element(int, 1),
    element(float, 3.14),
    assumed(Kind, Element, Assumed),
    count(Kind, Count),
    extract(Kind, All),
    clear_all.

% test(int, is_assumed(int, 3, Assumed), counting(int, Count), All).