/*
Tests of wellbeing dict functions.

[load].
[load_tests].
run_tests(wellbeing).
*/

:- use_module(utils(logger)).
:- use_module(agency(som/wellbeing)).

:- set_log_level(info).

:- begin_tests(wellbeing).

test(sub_wellbeing) :-
    Wellbeing = wellbeing{fullness:1.0, integrity:1.0, engagement:1.0},
    Wellbeing1 = Wellbeing.sub(wellbeing{fullness:0.5, integrity: 0.0, engagement:2.0}),
    assertion(Wellbeing1.fullness == 0.5),
    assertion(Wellbeing1.integrity == 1.0),
    assertion(Wellbeing1.engagement == 0.0).

test(div_wellbeing) :-
    Wellbeing = wellbeing{fullness:1.0, integrity:0.5, engagement:0.0},
    Wellbeing1 = Wellbeing.div(2),
    assertion(Wellbeing1.fullness == 0.5),
    assertion(Wellbeing1.integrity == 0.25),
    assertion(Wellbeing1.engagement == 0.0).

test(add_wellbeing) :-
    Wellbeing = wellbeing{fullness:1.0, integrity:0.5, engagement:0.0},
    Wellbeing1 = Wellbeing.add(wellbeing{fullness:0.5, integrity: 0.0, engagement:2.0}),
    assertion(Wellbeing1.fullness == 1.0),
    assertion(Wellbeing1.integrity == 0.5),
    assertion(Wellbeing1.engagement == 1.0).

:-end_tests(wellbeing).
