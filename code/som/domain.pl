/*
About value domains.

Kinds of domains: boolean, percent, enumerated list, range{from:Int1,to:Int2}
*/

:- module(domain, [random_domain_value/2]).

random_domain_value(boolean, Value) :-
    random_member(Value, [true, false]).

random_domain_value(count, Value) :-
    random_member(Value, [1,2,3]).

random_domain_value(percent, Value) :-
    random_between(0, 100, Value).

random_domain_value(range{from:From, to:To}, Value) :-
    random_between(From, To, Value).

random_domain_value(List, Value) :-
    is_list(List),
    random_member(Value, List).

