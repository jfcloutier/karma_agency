/*
Functions on wellbeing dicts.
*/

:- module(wellbeing, []).

dimension(Dimension) :-
    member(Dimension, [fullness, integrity, engagement]).

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

% Negate wellbeing 
W.neg()  := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is W.fullness * -1,
    Integrity is W.integrity * -1,
    Engagement is W.engagement * -1.

empty_wellbeing(wellbeing{fullness:0, integrity:0, engagement:0}).
