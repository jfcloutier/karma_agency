:- module(timed_gen, []).

:- use_module(library(random)).
:- use_module(apperception(timed_search)).


% Searching for the largest even number in a large sequence within time limit.
find_big_even(Low, High, Secs, Best) :-
    randseq(Low, High, S),
    search(timed_gen:gen(_, S), timed_gen:rate_value(_, _, 2), Secs, Best).

gen(Value, S) :-
    member(Value, S), % choice point for solutions
    format('Candidate ~d~n', [Value]),
    sleep(1).

% First arg is the value being rated, second arg is the rating
rate_value(Value, Value, Factor) :-
    0 is Value mod Factor, !.

rate_value(_, 0, _).
