/*
About value domains.

Kinds of domains: boolean, percent, enumerated list, range{from:Int1,to:Int2}
*/

:- module(domain, [random_domain_value/3]).

random_domain_value(boolean, Value, 0.5) :-
    random_member(Value, [true, false]).

random_domain_value(percent, Value, 0.01) :-
    random_between(0, 100, Value).

random_domain_value(range{from:From, to:To}, Value, Confidence) :-
    random_between(From, To, Value),
    Confidence is 1 / (To - From).

random_domain_value(List, Value, Confidence) :-
    is_list(List),
    random_member(Value, List),
    length(List, Length),
    Confidence is 1 / Length.

