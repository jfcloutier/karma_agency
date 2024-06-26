:- module(timed_gen, []).

:- use_module(library(random)).
:- use_module(apperception(timed_search)).


% Searching for the largest even number in a series within time limit.
% If no even number is found in time, any odd number found will do.
find_big_even(Low, High, Secs, Best) :-
    randseq(Low, High, S),
    search(timed_gen:gen(_, S), timed_gen:rate_value(_, _, 2), Secs, Best).

gen(Value, S) :-
    member(Value, S), % choice point for solutions
    format('Candidate ~d~n', [Value]),
    % Make finding a solution artifically time-consuming
    sleep(0.5).

% First arg is the value being rated, second arg is the rating
% The solution is worth itself if divisible by Factor, else worth 0
rate_value(Value, Value, Factor) :-
    0 is Value mod Factor, !.

rate_value(_, 0, _).
