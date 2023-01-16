:- module(rules_engine, [clear_rules/2, clear_facts/2, assert_rules/2, assert_facts/2, save_modules/1, answer_query/3]).

% Dynamic module from where rules and facts are added/removed,
% and new facts are inferred by running the rules.

/*
[rules_engine].
RulePairs = [head(X, Y)-[body(X),body(Y)]],
uuid(Module),
clear_rules(Module, RulePairs),
% assert_rules(Module, RulePairs),
save_module(Module).
*/

% Clear all rules from the dynamic module
clear_rules(Module, RulePairs) :-
    forall(member(Head-_, RulePairs), retract_rules(Module, Head)).

retract_rules(Module, RuleHead) :-
    copy_term_nat(RuleHead, CopiedHead),
    ClauseHead =.. [:, Module, CopiedHead],
    retractall(ClauseHead).

% Assert the rules in it.
assert_rules(Module, RulePairs) :-
    forall(member(Head-Body, RulePairs), assert_rule(Module, Head-Body)),
    module_db(Module, DBModule),
    add_import_module(Module, DBModule, start).

assert_rule(Module, Head-Body) :-
    make_dynamic(Module, Head),
    pair_rule(Head-Body, Rule),
    ModuleRule =.. [:, Module, Rule],
    assertz(ModuleRule).

make_dynamic(Module, Term) :-
   descriptor(Module, Term, Descriptor),
   dynamic(Descriptor).

descriptor(Module, Term, Descriptor) :-
    Term =.. [PredicateName | Args],
    length(Args, L),
    LocalDescriptor =.. [/, PredicateName, L],
    Descriptor =.. [:, Module, LocalDescriptor].

clear_facts(Module, PredicateTypes) :-
    module_db(Module, ModuleDB),
    forall(member(PredicateType, PredicateTypes), retract_facts(ModuleDB, PredicateType)).

module_db(Module, DBModule) :-
    atom_concat(Module, '_DB', DBModule).

retract_facts(ModuleDB, predicate(PredicateName, Args)) :-
    length(Args, L),
    length(Vars, L),
    Term =.. [PredicateName | Vars],
    DBModuleTerm =.. [:, ModuleDB, Term],
    retractall(DBModuleTerm).
    
% Assert facts into the module
assert_facts(Module, Facts) :-
    forall(member(Fact, Facts), assert_fact(Module, Fact)).

assert_fact(Module, Fact) :-
    module_db(Module, DBModule),
    make_dynamic(DBModule, Fact),
    DBModuleFact =.. [:, DBModule, Fact],
    assertz(DBModuleFact).

answer_query(Module, Query, Answers) :-
    ModuleQuery =.. [:, Module, Query],
    findall(Query, ModuleQuery, Answers).

save_modules(Module) :-
    module_db(Module, DBModule),
    save_module(Module),
    save_module(DBModule).

save_module(Atom) :-
    atom_string(Atom, Name),
    concat(Name, '.pl', File),
    tell(File),
    format(":- module(~p, []).~n", [Atom]),
    ((import_module(Atom, Imported), Imported \== user) ->
        format("~n:- add_import_module(~p, ~p, end)~n", [Atom, Imported])
        ; true
    ),
    listing(Atom:_),
    told, !.

pairs_rules([], []).
pairs_rules([Head-BodyPredicates | OtherPairs], [Rule | OtherRules]) :-
    pair_rule(Head-BodyPredicates, Rule),
    pairs_rules(OtherPairs, OtherRules).

pair_rule(Head-BodyPredicates, Rule) :-
    and(BodyPredicates, Body),
    Rule =.. [:-, Head, Body].

and([], []) :- true.
and([Goal], Goal) :- !.
and([Goal | Rest], Anded) :-
    and(Rest, Others),
    Anded =..  [(','), Goal, Others].
