:- module(sequence, [enacted_sensory_sequence/3, episode/2]).

:- use_module(library(lists)).

enacted_sensory_sequence(MemoryModule, GM, Sequence) :-
    memorized_sequence(MemoryModule, GM, [sensed, enacted, felt], Sequence).

memorized_sequence(MemoryModule, GM, Predicates, Sequence) :-
    setof(memory(Round, What), memorized(MemoryModule, GM, Predicates, Round, What), UnsortedMemories),
    !,
    % Sort memories on group
    sort(1, @=<, UnsortedMemories, SortedMemories),
    sequence(SortedMemories, Sequence).

memorized_sequence(_, _, _, []).

memorized(MemoryModule, GM, Predicates, Round, What) :-
    RememberedGoal =.. [remembered, GM, Round, What],
    call(MemoryModule:RememberedGoal),
    What =.. [Predicate | _] ,
    member(Predicate, Predicates).

sequence(Memories, Sequence) :- 
    sequence_(Memories, [], MemoriesByRound),
    memory_sequence(MemoriesByRound, Sequence).

sequence_([], Sequence, Sequence).

sequence_([memory(Round, What) | OtherMemories], Acc, Sequence) :-
    grouped_by_round(What, Round, Acc, Acc1),
    !,
    sequence_(OtherMemories, Acc1, Sequence).

%% Reverse insert grouped on rounds
grouped_by_round(What, Round, [], [[Round, [What]]]).

grouped_by_round(What, Round, [[Round, Whats] | Rest], [[Round, [What | Whats]] | Rest]).

grouped_by_round(What, Round, [[OtherRound, Whats] | Rest], [[Round, [What]], [OtherRound, Whats] | Rest]) :-
    Round > OtherRound.

grouped_by_round(What, Round, [[OtherRound, Whats] | Rest], [[OtherRound, Whats] | List]) :-
    Round < OtherRound,
    grouped_by_round(What, Round, Rest, List).

memory_sequence(MemoriesByRound, Sequence) :-
    memory_sequence_(MemoriesByRound, nil, [], Sequence).

memory_sequence_([], _, Sequence, Sequence).

memory_sequence_([[Round, Whats] | Rest], nil, Acc, Sequence) :-
    !,
    memory_sequence_(Rest, Round, [Whats | Acc], Sequence).

memory_sequence_([[Round, Whats] | Rest], PriorRound, Acc, Sequence) :-
    PriorRound - Round > 1,
    !,
    NextRound is PriorRound -1,
    memory_sequence_([[Round, Whats] | Rest], NextRound, [[] | Acc], Sequence).

memory_sequence_([[Round, Whats] | Rest], PriorRound, Acc, Sequence) :-
    Round is PriorRound - 1,
    memory_sequence_(Rest, Round, [Whats | Acc], Sequence).

% Find first the longer, more recent sub-sequences that do not begin or end with empty rounds.
episode(Episode, Sequence) :-
    bagof(measured(L, SS), (episode1(SS, Sequence), length(SS, L)), MeasuredEpisodes),
    sort(1, @>=, MeasuredEpisodes, Sorted),
    member(measured(_, Episode), Sorted).

% Non-empty sub sequences of decreasing lengths for each ending state,
% starting with the last state as ending state.
episode1(Episode, Sequence) :-
    reverse(Sequence, ReversedSequence),
    phrase(subseq(ReversedEpisode), ReversedSequence),
    reverse(ReversedEpisode, Episode),
    acceptable(Episode).

% A sub-sequence is acceptable if does not start nor end with an empty round.
acceptable([[] | _]) :- !, false.
acceptable(Episode) :- last(Episode, []), !, false.
acceptable(_).

subseq(S) --> ..., non_empty_seq(S), ... .

... --> [] | [_], ... .

non_empty_seq([X |T]) --> [X], non_empty_seq(T).
non_empty_seq([X]) --> [X].


