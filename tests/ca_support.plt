/*
Tests of CA support.

run_tests(ca_support).
*/

:- begin_tests(ca_support, []).

:- use_module(agency(som/ca_support)).
:- use_module(utils(logger)).

:- set_log_level(info).

test(comparing) :-
    assertion(\+ is_grossly_more_than(2, 3)),
    assertion(is_grossly_more_than(3, 0)),
    assertion(is_grossly_more_than(10, -10)),
    assertion(\+ is_grossly_more_than(16, 12)),
    assertion(is_grossly_more_than(16, 11)).

