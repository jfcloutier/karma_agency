:- module(happy, [professor/1, happy/1]).

:- use_module(library(chr)).

:- chr_constraint professor/1, rich/1, has_good_students/1.

professor(X) \ professor(X) <=> true.

professor(X), rich(X) ==> fail.

happy(X) :- rich(X).

happy(X) :- professor(X), has_good_students(X).
