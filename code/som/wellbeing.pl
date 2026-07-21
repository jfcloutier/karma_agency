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

% Calculate the wellbeing delta from a given wellbeing
W.delta(D)  := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is W.fullness - D.fullness,
    Integrity is W.integrity - D.integrity,
    Engagement is W.engagement - D.engagement.

% Divide wellbeing by a factor
W.div(F)  := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    F > 0,
    Fullness is W.fullness / F,
    Integrity is W.integrity / F,
    Engagement is W.engagement / F.

% Combine two wellbeings - max at 1.0
W.add(D) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is min(W.fullness + D.fullness, 1.0),
    Integrity is min(W.integrity + D.integrity, 1.0),
    Engagement is min(W.engagement + D.engagement, 1.0).

% Combine two wellbeings - no max
W.sum(D) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is W.fullness + D.fullness,
    Integrity is W.integrity + D.integrity,
    Engagement is W.engagement + D.engagement.

W.add_fullness(Amount) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is min((W.fullness + Amount), 1.0),
    Integrity = W.integrity,
    Engagement = W.engagement.

W.add_integrity(Amount) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness = W.fullness,
    Integrity is min((W.integrity + Amount), 1.0),
    Engagement = W.engagement.

W.add_engagement(Amount) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness = W.fullness,
    Integrity = W.integrity,
    Engagement is min((W.engagement) + Amount, 1.0).

W.subtract_fullness(Amount) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is max((W.fullness - Amount), 0.0),
    Integrity = W.integrity,
    Engagement = W.engagement.

W.subtract_integrity(Amount) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness = W.fullness,
    Integrity is max((W.integrity - Amount), 0.0),
    Engagement = W.engagement.

W.subtract_engagement(Amount) := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness = W.fullness,
    Integrity = W.integrity,
    Engagement is max((W.engagement - Amount), 0.0).

% Negate wellbeing 
W.neg()  := wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is W.fullness * -1,
    Integrity is W.integrity * -1,
    Engagement is W.engagement * -1.

W.max(N) :=  wellbeing{fullness:Fullness, integrity:Integrity, engagement:Engagement} :-
    Fullness is max(W.fullness, N),
    Integrity is max(W.integrity, N),
    Engagement is max(W.engagement, N).


    

empty_wellbeing(wellbeing{fullness:0, integrity:0, engagement:0}).
