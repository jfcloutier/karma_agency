:- module(rules_engine, [clear/2, assert_rules/2, assert_facts/2, save_module/1, apply_rules/3]).

:- use_module(logger).

% Dynamic module from where rules and facts are added/removed,
% and new facts are inferred by running the rules.

/*
[logger, rules_engine].
RulePairs = [next_to(X, Y)-[on(X, true),on(Y, false)], on(X, true)-[light(X, red)]],
uuid(Module),
PredicateTypes = [predicate(on, [object_type(led), value_type(boolean)]), 
		  predicate(light, [object_type(led), value_type(color)]), 
		  predicate(next_to, [object_type(led), object_type(led)])
		  ],
Facts = [on(b, false), light(a, red), light(b, green)],
clear(Module, PredicateTypes),
assert_rules(Module, RulePairs),
assert_facts(Module, Facts),
answer_query(Module, next_to(X, Y), Answers),
save_module(Module).
*/

% Clear all rules and facts from the dynamic module
% TODO - Find a way to remove a dynamic module entirely
clear(Module, PredicateTypes) :-
    forall(member(PredicateType, PredicateTypes), retract_predicates(Module, PredicateType)).

% Assert the rules in it.
assert_rules(Module, RulePairs) :-
    forall(member(Head-Body, RulePairs), assert_rule(Module, Head-Body)).

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

retract_predicates(Module, predicate(PredicateName, Args)) :-
    length(Args, L),
    length(Vars, L),
    Term =.. [PredicateName | Vars],
    ModuleTerm =.. [:, Module, Term],
    retractall(ModuleTerm).
    
% Assert facts into the module
assert_facts(Module, Facts) :-
    forall(member(Fact, Facts), assert_fact(Module, Fact)).

assert_fact(Module, Fact) :-
    make_dynamic(Module, Fact),
    ModuleFact =.. [:, Module, Fact],
    asserta(ModuleFact).

apply_rules(Module, Rules, Results) :-
    apply_rules_(Module, Rules, [], Results).

apply_rules_(_, [], Results, Results).
apply_rules_(Module, [Head-_ | OtherRules], Acc, Results) :-
    answer_query(Module, Head, Facts),
    append(Facts, Acc, Acc1),
    apply_rules_(Module, OtherRules, Acc1, Results).

answer_query(Module, Query, Answers) :-
    copy_term(Query, CopiedQuery),
    ModuleQuery =.. [:, Module, CopiedQuery],
    (setof(CopiedQuery, ModuleQuery, Answers) ->
      true
      ; Answers = []).

save_module(Name) :-
    log_level(debug),
    mkdir('DELETE_ME'),
    atomic_list_concat(['DELETE_ME/', Name, '.pl'], File),
    tell(File),
    format(":- module(~p, []).~n", [Name]),
    listing(Name:_),
    told, !.

save_module(_).

mkdir(Dir) :-
    exists_directory(Dir) -> true; make_directory(Dir).

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
