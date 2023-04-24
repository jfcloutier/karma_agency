:- module(domains, [domain_is/2, is_domain_value/2]).

domain_is(boolean, [true, false]).
domain_is(proximity, [very_close, close, far, very_far]).
domain_is(color, [red, green, blue]).

is_domain_value(Domain, Value) :-
    domain_is(Domain, Values),
    member(Value, Values).