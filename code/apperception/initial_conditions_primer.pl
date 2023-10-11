% Pattern-based heuristics that avoid "combinatorial traps" by priming initial conditions.

:- module(initial_conditions_primer, [facts_from_observations/2, facts_from_unobserved/4]).

% Initialize initial conditions from an observed state within the observed sequence.
facts_from_observations(SequenceAsTrace, Facts) :-
    random_permutation(SequenceAsTrace, RandSequenceAsTrace),
    member(State, RandSequenceAsTrace),
    sort(0, @<, State, OrderedState),
    log(info, theory_engine, 'Priming initial conditions with observed ~p', [OrderedState]),
    observations(OrderedState, [], Facts).

observations([], Acc, Acc).
observations([Observation | Others], Acc, Facts) :-
    observed_fact(Observation, Fact),
    observations(Others, [Fact | Acc], Facts).

observed_fact(Observation, Fact) :-
    Observation =.. [PredicateName | Args],
    Fact = fact(PredicateName, Args).

% Premptively add a coherent set of constrained, unobserved relations 
% to the initial conditions to speed up getting to unified status.
facts_from_unobserved(TypeSignature, SequenceAsTrace, Constraints, Facts) :-
    reflexive_one_related_latent(TypeSignature, SequenceAsTrace, Constraints, PredicateName),
    !,
    latent_related_objects(TypeSignature, PredicateName, Facts).

facts_from_unobserved(_,_,_, []).

% There is a latent relation between object of the same type
reflexive_one_related_latent(TypeSignature, SequenceAsTrace, Constraints, ConstraintedPredicateName) :-
    member(one_related(ConstraintedPredicateName), Constraints),
    is_reflexive_relation(ConstraintedPredicateName, TypeSignature),
    is_unobserved_predicate(ConstraintedPredicateName, SequenceAsTrace).

% This is the name of a predicate relating objects of the smae type
is_reflexive_relation(PredicateName, TypeSignature) :-
    member(predicate(PredicateName, [object_type(ObjectType), object_type(ObjectType)]), TypeSignature.predicate_types).

% Whether a predicate is latent
% [[on(c9, false), on(c8, false), on(c7, false),...],[...],...]
is_unobserved_predicate(PredicateName, SequenceAsTrace) :-
    flatten(SequenceAsTrace, AllStates),
    \+ (member(Observation, AllStates), Observation =.. [PredicateName | _]).


% Create a circular chain of relations over all applicable objects
latent_related_objects(TypeSignature, PredicateName, Facts) :-
    member(predicate(PredicateName, [object_type(ObjectType), object_type(ObjectType)]), TypeSignature.predicate_types),
    random_permutation(TypeSignature.objects, AllObjects),
    findall(ObjectName, member(object(ObjectType, ObjectName), AllObjects), ChainObjects),
    [First, _ | _] = ChainObjects,
    last(ChainObjects, Last),
    chained_related_pairs(ChainObjects, [Last-First], ObjectPairs),
    relations_to_facts(PredicateName, ObjectPairs, [], Facts),
    log(info, theory_engine, 'Priming initial conditions with unobserved relations ~p', [Facts]).

chained_related_pairs([],ObjectPairs, ObjectPairs).
chained_related_pairs([_],ObjectPairs, ObjectPairs).
chained_related_pairs([From, To | Rest], Acc, ObjectPairs) :-
    chained_related_pairs([To | Rest], [From-To |  Acc], ObjectPairs).

relations_to_facts(_, [], Facts, Facts).
relations_to_facts(PredicateName, [FromObject-ToObject | OtherPairs], Acc, Facts) :-
    Fact = fact(PredicateName, [FromObject, ToObject]),
    relations_to_facts(PredicateName, OtherPairs, [Fact | Acc], Facts).


