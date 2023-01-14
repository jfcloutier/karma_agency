:- module(global, [get_global/3, set_global/3, delete_global/1]).

% Facilities for creating, updating and deleting non-backtrackable variables containing dictionaries.
/*
cd('sandbox/prototypes/apperception').
[global].
set_global(test, a/b, 1),
get_global(test, a/b, Value),
delete_global(test).

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