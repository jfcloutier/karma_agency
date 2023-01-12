:- module(domains, [domain_is/2]).

domain_is(boolean, [true, false]).
domain_is(proximity, [very_close, close, far, very_far]).
domain_is(color, [red, green, blue]).