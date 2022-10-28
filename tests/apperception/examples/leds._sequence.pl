:- module(leds_sequence, [sensed/3]).

% sensed(What, About, When)
sensed(off, [a], 1).
sensed(on, [b], 1).
sensed(on, [a], 2).
sensed(off, [b], 2).
sensed(on, [a], 3).
sensed(on, [b], 3).
sensed(on, [b], 4).
sensed(on, [a], 5).
sensed(off, [b], 5).
sensed(on, [a], 6).
sensed(on, [b], 6).
sensed(off, [a], 7).
sensed(on, [b], 7).
sensed(on, [a], 8).