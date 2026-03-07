/*
Utilities for experiences

% experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, feeling:Feeling, by:CA}
*/

:- module(experiences, [experience_intensity/2, experiences_sorted_by_intensity/2, is_experience_impactable/1]).

% The intensity of an experience is its absolute feeling experience scaled by the confidence in the experience
experience_intensity(Experience, Intensity) :-
     Intensity is abs(Experience.feeling) * Experience.confidence.

% Experiences are sorted by their intensity from highest to lowest, with randomization of ties
experiences_sorted_by_intensity(Experiences, SortedExperiences) :-
     random_permutation(Experiences, PermutedExperiences),  
     map_list_to_pairs(experience_intensity, PermutedExperiences, Pairs),
     keysort(Pairs, AscSortedPairs),
     % We want descending order from most intense
     reverse(AscSortedPairs, SortedPairs),
     pairs_values(SortedPairs, SortedExperiences).

% Only synthetic experiences can be impacted. Sensory and executive experiences are not; they are what they are.
is_experience_impactable(Experience) :-
     memberchk(Experience.kind, [count, more, trend, unchanged]).