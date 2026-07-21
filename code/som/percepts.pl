/*
Utilities for percepts

percept{origin:Object, kind:Kind, value:Value, confidence:Confidence, feeling:Feeling, by:CA}
*/

:- module(percepts, [percept_intensity/2,percepts_sorted_by_intensity/2, is_percept_impactable/1, activation_percept/4]).

:- use_module(agency(som/ca_support)).

% The intensity of a percept is its absolute feeling scaled by the confidence in the percept
percept_intensity(Percept, Intensity) :-
     Intensity is abs(Percept.feeling) * Percept.confidence.

% Percepts are sorted by their intensity from highest to lowest, with randomization of ties
percepts_sorted_by_intensity(Percepts, SortedPercepts) :-
     random_permutation(Percepts, PermutedPercepts),  
     map_list_to_pairs(percept_intensity, PermutedPercepts, Pairs),
     keysort(Pairs, AscSortedPairs),
     % We want descending order from most intense
     reverse(AscSortedPairs, SortedPairs),
     pairs_values(SortedPairs, SortedPercepts).

% Only synthetic percepts can be impacted. Sensory and executive percepts are not; they are what they are.
is_percept_impactable(Percept) :-
     memberchk(Percept.kind, [count, more, trend]).

activation_percept(Goal, Status, CA, ActivationPercept) :-
     goal_id(Goal, GoalId),
     Origin = object{type:goal, id:GoalId},
     ActivationPercept = percept{origin:Origin, kind:activation, value:Status, confidence:1.0, by:CA}.
