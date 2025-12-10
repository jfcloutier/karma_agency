/*
Tests of test helper functions

run_tests(test_helper).
*/

:- begin_tests(test_helper, []).

:- use_module(test_helper).
:- use_module(utils(logger)).

:- set_log_level(info).

test(get_matching_dict_message) :-
    thread_self(Self),
    thread_create(
        (thread_self(Thread),
        thread_send_message(Self, message(prediction(prediction{name:sensor1, object:distance, value:10}), Thread))
        ), 
        Thread),
    get_matching_message(prediction{value:Value}, message(prediction(X), _)),
    log(info, test_helper, "Got message with match ~p", [X]),
    assertion(Value == 10).

test(not_matching_dict_message) :-
    thread_self(Self),
    thread_create(
        (thread_self(Thread),
        thread_send_message(Self, message(prediction(prediction{name:sensor1, object:distance, value:10}), Thread))
        ), 
        Thread),
    \+ get_matching_message(prediction_error{}, message(prediction(_), _), 0.1).

test(get_matching_list_message) :-
    thread_self(Self),
    thread_create(
        (thread_self(Thread),
        thread_send_message(Self, message(choice(2)))
        ), 
        Thread),
    get_matching_message([1,2,3,4], message(choice(X))),
    log(info, test_helper, "Got message with match ~p", [X]),
    assertion(X == 2).

test(not_matching_list_message) :-
    thread_self(Self),
    thread_create(
        (thread_self(Thread),
        thread_send_message(Self, message(choice(2)))
        ), 
        Thread),
    \+ get_matching_message([6,7,8], message(choice(_)), 0.1).

test(get_matching_other_message) :-
    thread_self(Self),
    thread_create(
        (thread_self(Thread),
        thread_send_message(Self, message(options([1,2,3])))
        ), 
        Thread),
    get_matching_message(options(_), message(X)),
    log(info, test_helper, "Got message with match ~p", [X]),
    assertion(X == options([1,2,3])).

test(not_matching_other_message) :-
    thread_self(Self),
    thread_create(
        (thread_self(Thread),
        thread_send_message(Self, message(options([1,2,3])))
        ), 
        Thread),
    \+ get_matching_message(something(_), message(_), 0.1).

:- end_tests(test_helper).