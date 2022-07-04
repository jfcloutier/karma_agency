:- module(timed_gen, [test/1]).

:- use_module(library(random)).
:- use_module(timed_search).


test(Best) :-
    randseq(30, 1000, S),
    search(timed_gen:gen(_, S), timed_gen:rate_value(_, _, 2), 5, Best).

gen(Value, S) :-
    member(Value, S), % choice point for solutions
    format('Solution ~d~n', [Value]),
    sleep(1).

rate_value(Value, Value, Factor) :-
    0 is Value mod Factor, !.

rate_value(_, 0, _).
