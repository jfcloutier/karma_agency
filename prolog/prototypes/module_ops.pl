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

% See /usr/lib/swi-prolog/library/listing.pl
% current_predicate(_, prior_types:Pred), strip_module(Pred, _Module, Head), functor(Head, Name, Arity), nth_clause(Pred, ClauseIndex, Reference), clause(Head, Body, Reference).
% Module = vocabulary, current_predicate(_, Module:Pred), \+ predicate_property(Module:Pred, imported_from(_)), strip_module(Pred, _Module, Head), functor(Head, Name, Arity), setof(Body, clause(Head, Body), Bodies).
% Module = vocabulary, current_predicate(_, Module:Pred), \+ predicate_property(Module:Pred, imported_from(_)), strip_module(Pred, _Module, Head), functor(Head, Name, Arity), clause(Head, Body).
