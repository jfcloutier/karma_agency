:- module(module_ops,
          [ ensure_competency_module/3,
            save_module/1
          ]).

ensure_competency_module(GM, Competency, Module) :-
    competency_module(GM, Competency, Module),
    (   current_module(Module)
    ;   create_module(Module),
        save_module(Module)
    ).

competency_module(GM, Competency, Module) :-
    concat(GM, '_', Prefix),
    concat(Prefix, Competency, Name),
    atom_string(Module, Name).

create_module(Module) :-
    assert(Module:created),
    set_prolog_flag(Module:unknown, warning).

save_module(Module) :-
    atom_string(Module, Name),
    concat(Name, '.pl', File),
    tell(File),
    listing(Module:_),
    told, !.