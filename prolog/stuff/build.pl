:- module(build, [build/0]).

:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint(put_in_place/2).

put_in_place(P0, Time0), put_in_place(P1, Time1) ==> supports(P0, P1), Time0 > Time1 | fail.

build :- put_in_place(soil, 0), parts(Parts), build_it(Parts, 1).

build_it([], _).

build_it(Parts, Time) :-
    select(P, Parts, RestParts),
    put_in_place(P, Time),
    Time1 is Time + 1,
    build_it(RestParts, Time1).

parts([gable, c1, c2, f1, f2]).

supports(soil,f1).
supports(f1, f2).
supports(f2, c1).
supports(f2, c2).
supports(c1, gable).
supports(c2, gable).