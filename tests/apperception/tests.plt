
:- begin_tests(apperception).

:- use_module(tests('apperception/timed_gen')).
:- use_module(tests('apperception/remembered_db')).

:- use_module(apperception(sequence)).

test(timed_search) :-
    TimeLimit is 5,
    get_time(StartTime),
    timed_gen:find_big_even(20, 1000, TimeLimit, Best),
    get_time(EndTime),
    TimeSpent is EndTime - StartTime,
    format("Found best solution ~p before time expired~n", [Best]),
    assertion(Best >= 20),
    assertion(Best =< 1000),
    assertion(TimeSpent =< TimeLimit + 1).

test(sequencing) :-
    enacted_sensory_sequence(remembered_db, gm_1, Sequence),
    format("Looking for sub-sequences of ~p~n~n", [Sequence]),
    forall(
        sub_sequence(SubSequence, Sequence),
        (assertion(check_sub_sequence(SubSequence, Sequence)),
         format("Found sub-sequence ~p~n", [SubSequence])
        )
    ).

check_sub_sequence([], _).

check_sub_sequence([Item | Rest], Sequence) :-
    member(Item, Sequence),
    check_sub_sequence(Rest, Sequence).
    

:- end_tests(apperception).
