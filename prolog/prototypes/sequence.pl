:- module(sequence, [enacted_sensory_sequence/2]).

:- use_module(remembered_db).

enacted_sensory_sequence(GM, Sequence) :-
    memorized_sequence( GM, [sensed, enacted], Sequence).

memorized_sequence(GM, Predicates, Sequence) :-
    setof(memory(Round, What), memorized(GM, Predicates, Round, What), UnsortedMemories),
    !,
    % Sort memories on group
    sort(1, @=<, UnsortedMemories, SortedMemories),   
    sequence(SortedMemories, Sequence).

memorized_sequence(_,_,[]).

memorized(GM, Predicates, Round, What) :-
    remembered(Round, GM, What),
    What =.. [Predicate | _] ,
    member(Predicate, Predicates).

sequence(Memories, Sequence) :- 
    sequence_(Memories, [], MemoriesByRound),
    memory_sequence(MemoriesByRound, Sequence).

sequence_([], Sequence, Sequence).

sequence_([memory(Round, What) | OtherMemories], Acc, Sequence) :-
    grouped_by_round(What, Round, Acc, Acc1),
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
    NextRound is PriorRound -1,
    memory_sequence_([[Round, Whats] | Rest], NextRound, [[] | Acc], Sequence).

memory_sequence_([[Round, Whats] | Rest], PriorRound, Acc, Sequence) :-
    Round is PriorRound - 1,
    memory_sequence_(Rest, Round, [Whats | Acc], Sequence).
