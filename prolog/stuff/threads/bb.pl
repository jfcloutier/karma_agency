:- module(bb, [add_event/1, remove_events/1]).

:- dynamic event/2.

add_event(Event) :-
    is_entry(Event),
    assertz(Event).

remove_events(Pattern) :-
    is_entry(Pattern),
    retractall(Pattern).

is_entry(Term) :-
    Term =..[event | _].


