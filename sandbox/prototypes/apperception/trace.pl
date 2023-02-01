:- module(trace, [make_trace/4]).


:- use_module(unity).
:- use_module(rules_engine).

/*
cd('sandbox/prototypes/apperception').
[trace, unity, rules_engine].
*/

% Starting from initial conditions as round(0) of the trace, apply the theory to construct round(1), etc. until a round repeats a prior round.
make_trace(Theory, TypeSignature, Trace, Module) :-
    setup_call_cleanup(
        clear(Module, TypeSignature.predicate_types),
        (expand_trace([Theory.initial_conditions], Theory, TypeSignature, Module, ReverseTrace),
         reverse(ReverseTrace, Trace)
        ),
        clear(Module, TypeSignature.predicate_types)
    ).

% Deterministic and always terminating trace expansion.
% Can fail.
expand_trace(Trace, Theory, TypeSignature, Module, ExpandedTrace) :-
    [Round | _] = Trace,
    next_round(Round, Theory, TypeSignature, Module, NextRound),
    (round_in_trace(NextRound, Trace) ->
        ExpandedTrace = Trace
        ;
        expand_trace([NextRound | Trace], Theory, TypeSignature, Module, ExpandedTrace)
    ).

next_round(Round, Theory, TypeSignature, Module, NextRound) :-
    make_round(Round, Theory, TypeSignature, Module, NextRound),
    spatial_unity(NextRound).

% Apply causal rules to a round to produce a caused facts.
% Apply static rules to the new round to add implied facts.
% Carry over all facts from current round that do not introduce contradictions 
% or break and static constraints.
make_round(Round, Theory, TypeSignature, Module, NextRound) :-
    apply_causal_rules_on_facts(Theory.causal_rules, Round, TypeSignature.predicate_types, Module, CausedFacts),
    apply_static_rules_on_facts(Theory.static_rules, CausedFacts, TypeSignature.predicate_types, Module, FullCausedFacts),
    carry_over_composable(Round, FullCausedFacts, Theory, NextRound).

apply_static_rules_on_facts(Rules, Facts, PredicateTypes, Module, Caused) :-
    clear(Module, PredicateTypes),
    assert_rules(Module, Rules),
    assert_facts(Module, Facts),
    apply_rules(Module, Rules, all, Caused).

% TODO - causal rules can be recursive!!
apply_causal_rules_on_facts(Rules, Facts, PredicateTypes, Module, Caused) :-
    clear(Module, PredicateTypes),
    assert_rules(Module, Rules),
    assert_facts(Module, Facts),
    apply_rules(Module, Rules, one, Caused).


% Add as many prior facts into caused facts without introducing a contradiction or breaking a static constraint.
% carry_over_composable(PriorFacts, CausedFacts, Theory, Module, ComposedFacts) :-
carry_over_composable([], ComposedFacts, _, _, ComposedFacts).
carry_over_composable([PriorFact | OtherPriorFacts], CausedFacts, Theory, Module, ComposedFacts) :-
    \+ (member(CausedFact, CausedFacts), facts_repeat(PriorFact, CausedFact)),
    \+ (member(CausedFact, CausedFacts), factual_contradiction(PriorFact, CausedFact)),
    \+ breaks_static_constraints([PriorFact | CausedFacts], Theory.static_constraints, Theory.type_signature),
    carry_over_composable(OtherPriorFacts, [PriorFact | CausedFacts], Theory, Module, ComposedFacts).

% Succeeds if can find another round in the trace that's a permutation of it.
round_in_trace(Round, Trace) :-
    findnsols(1, OtherRound, (member(OtherRound, Trace), permutation(Round, OtherRound)), _), !.
 