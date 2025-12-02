/*
Functions on wellbeing dicts.
*/

:- module(wellbeing, []).

% Remove a wellbeing from another
W.sub(D) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is max(W.fullness - D.fullness, 0.0),
    Integrity is max(W.integrity - D.integrity, 0.0),
    Engagement is max(W.engagement - D.engagement, 0.0).


% Divide wellbeing by a factor
W.div(F)  := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    F > 0,
    Fullness is W.fullness / F,
    Integrity is W.integrity / F,
    Engagement is W.engagement / F.

% Combine two wellbeings
W.add(D) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is min(W.fullness + D.fullness, 1.0),
    Integrity is min(W.integrity + D.integrity, 1.0),
    Engagement is min(W.engagement + D.engagement, 1.0).
