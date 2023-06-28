:- module(global, [get_global/3, set_global/3, delete_global/1, count_down/1, reset_counter/2]).

% Facilities for creating, updating and deleting non-backtrackable variables containing dictionaries.
/*
[load].
[code(global)].
set_global(test, a/b, 1),
get_global(test, a/b, Value),
delete_global(test).

reset_counter(test, 3).
(count_down(test) -> format('2') ; format('OVER')).
(count_down(test) -> format('1') ; format('OVER')).
(count_down(test) -> format('0') ; format('OVER')).
(count_down(test) -> format('-1') ; format('OVER')).
*/
set_global(Name, Path, Value) :-
    (nb_current(Name, Dict) ->
        true
        ; dict_create(Dict, Name, [])),
    Updated = Dict.put(Path, Value),
    nb_setval(Name, Updated).

get_global(Name, Path, Value) :-
    nb_getval(Name, Dict),
    Value = Dict.get(Path).

delete_global(Name) :-
    nb_delete(Name).

reset_counter(Path, Count) :- 
    set_global(counters, Path, Count).

count_down(Path) :-
    get_global(counters, Path, Value),
    Value1 is Value - 1,
    Value1 > 0,
    set_global(counters, Path, Value1).
