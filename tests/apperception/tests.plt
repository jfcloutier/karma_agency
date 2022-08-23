:- begin_tests(apperception).

:- use_module(tests('apperception/timed_gen')).

test(timed_search) :-
    TimeLimit is 5,
    get_time(StartTime),
    timed_gen:find_big_even(20, 1000, TimeLimit, Best),
    get_time(EndTime),
    TimeSpent is EndTime - StartTime,
    assertion(0 is Best mod 2),
    assertion(TimeSpent =< TimeLimit + 1).

:- end_tests(apperception).
