:- module(rating, [rate_theory/4, count_elements/2]).

:- use_module(code(logger)).

% Rate how well the trace covers the sequence and how simple the theory is.
rate_theory(Theory, SequenceAsTrace, Trace, RatedTheory) :-
    rate_coverage(Trace, SequenceAsTrace, CoverageRating),
    rate_cost(Theory, Cost),
    Rating = CoverageRating-Cost,
    log(info, apperception_engine, 'Theory coverage ~p, cost ~p', [CoverageRating, Cost]),
    put_dict(rating, Theory, Rating, RatedTheory).

% Find the best coverage and rate it as a percentage.
% Consider the Trace as circular.
% Cover the Sequence from beginning with the Trace, starting in turn with each round in the Trace, and score.
% Keep the maximum score
rate_coverage(Trace, SequenceAsTrace, CoverageRating) :-
    findall(CoverageRating, rate_shifted_coverage(Trace, SequenceAsTrace, CoverageRating), Ratings),
    max_member(CoverageRating, Ratings).

rate_shifted_coverage(Trace, SequenceAsTrace, CoverageRating):-
    length(Trace, L),
    between(1, L, Start),
    rate_coverage_starting_at(Trace, Start, SequenceAsTrace, CoverageRating).

% Match sequence rouns with trace rounds from a starting point in the trace.
% Rate coverage per round pair
% Average rating over all pairs
rate_coverage_starting_at(Trace, Start, SequenceAsTrace, CoverageRating) :-
    pair_rounds(Trace, Start, SequenceAsTrace, [], RoundPairs),
    findall(Rating, (member(TraceRound-SequenceRound, RoundPairs), rate_round_pair(TraceRound-SequenceRound, Rating)), Ratings),
    sum_list(Ratings, Sum),
    length(Ratings, L),
    (L > 0 -> CoverageRating is div(Sum, L); CoverageRating = 0).

pair_rounds([], _, _, _ , []).
pair_rounds(_, _, [], RoundPairs, RoundPairs).
pair_rounds(Trace, Index, [SequenceRound | OtherSequenceRounds], Acc, RoundPairs) :-
    trace_round_at(Trace, Index, NextIndex, TraceRound),
    pair_rounds(Trace, NextIndex, OtherSequenceRounds, [TraceRound-SequenceRound | Acc], RoundPairs).

% Consider the trace circular
trace_round_at(Trace, Index, NextIndex, TraceRound) :-
    length(Trace, L),
    (Index =< L ->
        nth1(Index, Trace, TraceRound),
        NextIndex is Index + 1
        ;
        nth1(1, Trace, TraceRound),
        NextIndex = 2
    ).

% Rate percent coverage of a sequence round by a trace round
rate_round_pair(TraceRound-SequenceRound, Rating) :-
    intersection(TraceRound, SequenceRound, CommonFacts),
    length(SequenceRound, ObservedLength),
    length(CommonFacts, CommonLength),
    (ObservedLength == 0 -> Coverage = 1; Coverage is CommonLength / ObservedLength),
    Percent is Coverage * 100,
    round(Percent, Rating).
    

% Rate cost of the theory
rate_cost(Theory, Cost) :-
    count_elements(Theory.static_rules, StaticRuleElements),
    count_elements(Theory.causal_rules, CausalRuleElements),
    count_elements(Theory.static_constraints, ConstraintElements),
    Cost is StaticRuleElements + CausalRuleElements + ConstraintElements.

% Count the number of elements in a term
count_elements(Var, 1) :-
  var(Var), !.
count_elements([], 0) :- !.
count_elements([Head | Tail], Count) :-
    count_elements(Head, C1),
    count_elements(Tail, C2),
    Count is C1 + C2, !.
count_elements(Term, Count) :-
    Term =.. [_ | Args], !,
    count_elements(Args, C1),
    Count is C1 + 1, !.
count_elements(_, 1).
