/*
[load].
['tests/global.plt'].
run_tests(global).
*/

:- begin_tests(global).

:- use_module(code(global)).

test(global_get_set) :-
    set_global(test, a/b, 1),
    get_global(test, a/b, Value),
    assertion(Value == 1),
    delete_global(test),
    catch(get_global(test, a/b, _), error(E,_), true),
    assertion(E == existence_error(variable,test)).

test(global_count_down) :-
    reset_counter(test, 3),
    count_down(test),
    count_down(test),
    assertion(\+ count_down(test)).

test(global_no_counter) :-
    reset_counter(test, 3),
    assertion( \+ count_down(test1)).

:- end_tests(global).
