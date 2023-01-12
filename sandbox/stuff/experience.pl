:- module(experience, [enacted_sensory_experience/3, episode/2]).

% OBSOLETE

:- use_module(library(lists)).

% Return a complete, uninterrupted experience of what was enacted or sensed per round.
% For example, [[sensed(color(floor),is(brown),1.0),felt(pain(0.0)),felt(hunger(0.3)),felt(ennui(0.0))],[sensed(color(floor),is(green),0.5),enacted(go_forward(big,1.0))],[],[felt(pain(0.2)),felt(hunger(0.4)),enacted(turn_right(small))]]
enacted_sensory_experience(MemoryDBModule, GM, Experience) :-
    memorized_experience(MemoryDBModule, GM, [sensed, enacted, felt], Experience).

memorized_experience(MemoryDBModule, GM, Predicates, Experience) :-
    setof(memory(RoundIndex, What), memorized(MemoryDBModule, GM, Predicates, RoundIndex, What), UnsortedMemories),
    !,
    % Sort memories by round index
    sort(1, @=<, UnsortedMemories, SortedByRoundMemories),
    experience(SortedByRoundMemories, Experience).

memorized_experience(_, _, _, []).

memorized(MemoryDBModule, GM, Predicates, RoundIndex, What) :-
    RememberedGoal =.. [remembered, GM, RoundIndex, What],
    call(MemoryDBModule:RememberedGoal),
    What =.. [Predicate | _] ,
    member(Predicate, Predicates).

experience(Memories, Experience) :- 
    experience_(Memories, [], MemoriesGroupedByRound),
    memory_experience(MemoriesGroupedByRound, Experience).

experience_([], Experience, Experience).

experience_([memory(RoundIndex, What) | OtherMemories], Acc, Experience) :-
    grouped_by_round(What, RoundIndex, Acc, Acc1),
    !,
    experience_(OtherMemories, Acc1, Experience).

%% Reverse insert grouped on rounds
grouped_by_round(What, RoundIndex, [], [[RoundIndex, [What]]]).

grouped_by_round(What, RoundIndex, [[RoundIndex, Whats] | Rest], [[RoundIndex, [What | Whats]] | Rest]).

grouped_by_round(What, RoundIndex, [[OtherRoundIndex, Whats] | Rest], [[RoundIndex, [What]], [OtherRoundIndex, Whats] | Rest]) :-
    RoundIndex > OtherRoundIndex.

grouped_by_round(What, RoundIndex, [[OtherRoundIndex, Whats] | Rest], [[OtherRoundIndex, Whats] | List]) :-
    RoundIndex < OtherRoundIndex,
    grouped_by_round(What, RoundIndex, Rest, List).

memory_experience(MemoriesByRoundIndex, Experience) :-
    memory_experience_(MemoriesByRoundIndex, nil, [], Experience).

% Collect what's remembered (drop the round index), still grouped by round but as an uninterrupted experience
memory_experience_([], _, Experience, Experience).

memory_experience_([[RoundIndex, Whats] | Rest], nil, Acc, Experience) :-
    !,
    memory_experience_(Rest, RoundIndex, [Whats | Acc], Experience).

memory_experience_([[RoundIndex, Whats] | Rest], PriorRoundIndex, Acc, Experience) :-
    PriorRoundIndex - RoundIndex > 1,
    !,
    NextRoundIndex is PriorRoundIndex -1,
    memory_experience_([[RoundIndex, Whats] | Rest], NextRoundIndex, [[] | Acc], Experience).

memory_experience_([[RoundIndex, Whats] | Rest], PriorRoundIndex, Acc, Experience) :-
    RoundIndex is PriorRoundIndex - 1,
    memory_experience_(Rest, RoundIndex, [Whats | Acc], Experience).

% Find an episode that does not begin or end with empty rounds, prioritizing on longer, more recent ones.
episode(Episode, Experience) :-
    bagof(measured(L, SS), (episode1(SS, Experience), length(SS, L)), MeasuredEpisodes),
    sort(1, @>=, MeasuredEpisodes, Sorted),
    member(measured(_, Episode), Sorted).

% Non-empty episodes of decreasing lengths for each ending state,
% starting with the last state as ending state.
episode1(Episode, Experience) :-
    reverse(Experience, ReversedExperience),
    phrase(subseq(ReversedEpisode), ReversedExperience),
    reverse(ReversedEpisode, Episode),
    acceptable(Episode).

% An episode is acceptable if does not start nor end with an empty round.
acceptable([[] | _]) :- !, false.
acceptable(Episode) :- last(Episode, []), !, false.
acceptable(_).

subseq(S) --> ..., non_empty_seq(S), ... .

... --> [] | [_], ... .

non_empty_seq([X |T]) --> [X], non_empty_seq(T).
non_empty_seq([X]) --> [X].


