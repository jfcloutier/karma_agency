:- module(trace, [make_trace/4, sequence_as_trace/2, apply_causal_rules_on_facts/5]).

:- use_module(logger).
:- use_module(unity).
:- use_module(rules_engine).

/*
cd('sandbox/prototypes/apperception').
[logger, trace, unity, rules_engine].
*/

% Starting from initial conditions as round(0) of the trace, apply the theory to construct round(1), etc. until a round repeats a prior round.
make_trace(Theory, TypeSignature, Trace, Module) :-
    log(info, trace, 'Making trace applying ~p from initial conditions ~p', [Module, Theory.initial_conditions]),
     setup_call_cleanup(
        clear_facts_and_rules(Module, TypeSignature.predicate_types),
        (expand_trace([Theory.initial_conditions], Theory, TypeSignature, Module, ReverseTrace),
         reverse(ReverseTrace, Trace)
        ),
        clear_facts_and_rules(Module, TypeSignature.predicate_types)
    ).

% Deterministic and always terminating trace expansion.
% Can fail.
expand_trace(Trace, Theory, TypeSignature, Module, ExpandedTrace) :-
    [Round | _] = Trace,
    next_round(Round, Theory, TypeSignature, Module, NextRound),
    !,
    (round_in_trace(NextRound, Trace) ->
        log(debug, trace, 'Next round ~p is already in the trace ~p', [NextRound, Trace]),
        ExpandedTrace = Trace
        ;
        expand_trace([NextRound | Trace], Theory, TypeSignature, Module, ExpandedTrace)
    ).

% Apply causal rules to a round to produce a caused facts.
% Apply static rules to the new round to add implied facts.
% Carry over all facts from current round that do not introduce contradictions 
% Verify that the new round does not break static constraints and is spatially unified.
next_round(Round, Theory, TypeSignature, Module, NextRound) :-
    log(debug, trace, 'Making next round'),
    constant_facts(Round, Theory, ConstantFacts),
    apply_causal_rules_on_facts(Theory.causal_rules, Round, TypeSignature.predicate_types, Module, CausedFacts),
    append(ConstantFacts, CausedFacts, FactsToClose),
    static_closure(FactsToClose, Theory.static_rules, TypeSignature, Module, ClosedFacts),
    carry_over_composable(Round, ClosedFacts, Theory, TypeSignature, NextRound),
    \+ breaks_static_constraints(NextRound, Theory.static_constraints, TypeSignature),
    spatial_unity(NextRound, TypeSignature),
    log(debug, trace, 'Next round ~p', [NextRound]).

constant_facts(Facts, Theory, ConstantFacts) :-
    setof(Fact, (member(Fact, Facts), \+ inferrable(Fact, Theory)), ConstantFacts),
    !.

constant_facts(_, _, []).

inferrable(Fact, Theory) :-
    inferrable_from(Fact, Theory.causal_rules).

inferrable(Fact, Theory) :-
    inferrable_from(Fact, Theory.static_rules).

inferrable_from(Fact, [Head-_ | _]) :-
    Head =.. [_, InferredFact],
    Fact =.. [PredicateName | _],
    InferredFact =.. [PredicateName | _].

inferrable_from(Fact, [_ | OtherRulePairs]) :-
    inferrable_from(Fact, OtherRulePairs).

% Apply each causal rule in turn, accumulating caused facts.
% Raises invalid_causal_rules if contradictory facts are produced.
apply_causal_rules_on_facts(Rules, Facts, PredicateTypes, Module, Caused) :-
    apply_causal_rules_on_facts_(Rules, Facts, PredicateTypes, Module, [], Caused).

apply_causal_rules_on_facts_([], _, _, _, Caused, Caused) :- !.

apply_causal_rules_on_facts_([Rule | OtherRules], Facts, PredicateTypes, Module, Acc, Caused) :-
    apply_rules_on_facts([Rule], Facts, PredicateTypes, Module, Answers),
    unwrap_facts(Answers, next, CausedByRule),
    append(CausedByRule, Acc, Acc1),
    list_to_set(Acc1, Acc2),
    !,
    (facts_consistent(Acc2) ->
        apply_causal_rules_on_facts_(OtherRules, Facts, PredicateTypes, Module, Acc2, Caused)
        ;
        throw(error(invalid_causal_rules, context(trace, Acc2)))).

apply_rules_on_facts(Rules, Facts, PredicateTypes, Module, Caused) :-
    clear_facts_and_rules(Module, PredicateTypes),
    assert_rules(Module, Rules),
    assert_facts(Module, Facts),
    save_module(Module),
    apply_rules(Module, Rules, Caused).

% Add as many prior facts into caused facts without introducing a property contradiction or breaking a too-many-relations static constraint.
carry_over_composable([], ComposedFacts, _, _, ComposedFacts).
carry_over_composable([PriorFact | OtherPriorFacts], CausedFacts, Theory, TypeSignature, ComposedFacts) :-
    log(debug, trace, 'Trying to compose prior ~p into caused ~p', [PriorFact, CausedFacts]),
    \+ (member(CausedFact, CausedFacts), facts_repeat(PriorFact, CausedFact)),
    \+ (member(CausedFact, CausedFacts), factual_contradiction(PriorFact, CausedFact)),
    \+ too_many_relations(Theory.static_constraints, [PriorFact | CausedFacts]),
     !,
    log(debug, trace, 'Composed prior ~p into caused ~p', [PriorFact, CausedFacts]),
    carry_over_composable(OtherPriorFacts, [PriorFact | CausedFacts], Theory, TypeSignature, ComposedFacts).
carry_over_composable([_ | OtherPriorFacts], CausedFacts, Theory, TypeSignature, ComposedFacts) :-
    carry_over_composable(OtherPriorFacts, CausedFacts, Theory, TypeSignature, ComposedFacts).

% Succeeds if can find another round in the trace that's a permutation of it.
round_in_trace(Round, [TraceRound | OtherTraceRounds]) :- 
    permutation(Round, TraceRound)
    ;
    round_in_trace(Round, OtherTraceRounds).

% Covert a sequence to a trace so they can be compared
sequence_as_trace(Sequence, Trace) :-
    sequence_as_trace_(Sequence, [], Trace).

sequence_as_trace_([], ReversedTrace, Trace) :-
    reverse(ReversedTrace, Trace).

sequence_as_trace_([State | OtherStates], Acc, Trace) :-
    state_as_round(State, Round),
    sequence_as_trace_(OtherStates, [Round | Acc], Trace).

% Convert a trace round to a state in a sequence of observations
state_as_round(State, Round) :-
    state_as_round_(State, [], Round).

state_as_round_([], Round, Round).

state_as_round_([Observation | OtherObservations], Acc, Round) :-
    Observation =.. [sensed, PredicateName, ObservationArgs, _],
    !,
    convert_observation_args(ObservationArgs, Args),
    Fact =.. [PredicateName | Args],
    state_as_round_(OtherObservations, [Fact | Acc], Round).

state_as_round_([_ | OtherObservations], Acc, Round) :-
    state_as_round_(OtherObservations, Acc, Round).

convert_observation_args(ObservationArgs, Args) :-
    convert_observation_args_(ObservationArgs, [], Args).

 convert_observation_args_([], ReversedArgs, Args) :-
    reverse(ReversedArgs, Args).

convert_observation_args_([ObservationArg | OtherObservationArgs], Acc, Args) :-
    convert_observation_arg(ObservationArg, Arg),
    convert_observation_args_(OtherObservationArgs, [Arg | Acc], Args).

convert_observation_arg(ObservationArg, ObjectName) :-
    ObservationArg =.. [object, _, ObjectName], !.

convert_observation_arg(Arg, Arg).
