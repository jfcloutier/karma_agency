
:- begin_tests(apperception).

:- use_module(tests('apperception/timed_gen')).
:- use_module(tests('apperception/remembered_db')).

:- use_module(apperception(experience)).

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
    enacted_sensory_experience(remembered_db, gm_1, Sequence),
    format("Looking for episodes of ~p~n~n", [Sequence]),
    forall(
        episode(Episode, Sequence),
        (assertion(check_episode(Episode, Sequence)),
         format("Found episode ~p~n", [Episode])
        )
    ).

check_episode([], _).

check_episode([Item | Rest], Sequence) :-
    member(Item, Sequence),
    check_episode(Rest, Sequence).
    

:- end_tests(apperception).
