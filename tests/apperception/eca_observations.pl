%%% Elementatry Cellular Automaton
/*
Rules (Left, Center Right)
    L, C, R -> ~C
    L, C, ~R -> C
    L, ~C, R -> C
    L, ~C, ~R -> ~C
    ~L, C, R -> C
    ~L, C, ~R -> C
    ~L, ~C, R -> C
    ~L, ~C, ~R -> ~C
*/

:- module(eca_observations, []).

% sensed(What, About, When)
sensed(on, [object(cell, c1), false], 1).
sensed(on, [object(cell, c2), false], 1).
sensed(on, [object(cell, c3), false], 1).
sensed(on, [object(cell, c4), false], 1).
sensed(on, [object(cell, c5), false], 1).
sensed(on, [object(cell, c6), true], 1).
sensed(on, [object(cell, c7), false], 1).
sensed(on, [object(cell, c8), false], 1).
sensed(on, [object(cell, c9), false], 1).
sensed(on, [object(cell, c10), false], 1).
sensed(on, [object(cell, c11), false], 1).
%
sensed(on, [object(cell, c1), false], 2).
sensed(on, [object(cell, c2), false], 2).
sensed(on, [object(cell, c3), false], 2).
sensed(on, [object(cell, c4), false], 2).
sensed(on, [object(cell, c5), true], 2).
sensed(on, [object(cell, c6), true], 2).
sensed(on, [object(cell, c7), false], 2).
sensed(on, [object(cell, c8), false], 2).
sensed(on, [object(cell, c9), false], 2).
sensed(on, [object(cell, c10), false], 2).
sensed(on, [object(cell, c11), false], 2).
%
sensed(on, [object(cell, c1), false], 3).
sensed(on, [object(cell, c2), false], 3).
sensed(on, [object(cell, c3), false], 3).
sensed(on, [object(cell, c4), true], 3).
sensed(on, [object(cell, c5), true], 3).
sensed(on, [object(cell, c6), true], 3).
sensed(on, [object(cell, c7), false], 3).
sensed(on, [object(cell, c8), false], 3).
sensed(on, [object(cell, c9), false], 3).
sensed(on, [object(cell, c10), false], 3).
sensed(on, [object(cell, c11), false], 3).
%
sensed(on, [object(cell, c1), false], 4).
sensed(on, [object(cell, c2), false], 4).
sensed(on, [object(cell, c3), true], 4).
sensed(on, [object(cell, c4), true], 4).
sensed(on, [object(cell, c5), false], 4).
sensed(on, [object(cell, c6), true], 4).
sensed(on, [object(cell, c7), false], 4).
sensed(on, [object(cell, c8), false], 4).
sensed(on, [object(cell, c9), false], 4).
sensed(on, [object(cell, c10), false], 4).
sensed(on, [object(cell, c11), false], 4).
%
sensed(on, [object(cell, c1), false], 5).
sensed(on, [object(cell, c2), true], 5).
sensed(on, [object(cell, c3), true], 5).
sensed(on, [object(cell, c4), true], 5).
sensed(on, [object(cell, c5), true], 5).
sensed(on, [object(cell, c6), true], 5).
sensed(on, [object(cell, c7), false], 5).
sensed(on, [object(cell, c8), false], 5).
sensed(on, [object(cell, c9), false], 5).
sensed(on, [object(cell, c10), false], 5).
sensed(on, [object(cell, c11), false], 5).
%
sensed(on, [object(cell, c1), true], 6).
sensed(on, [object(cell, c2), true], 6).
sensed(on, [object(cell, c3), false], 6).
sensed(on, [object(cell, c4), false], 6).
sensed(on, [object(cell, c5), false], 6).
sensed(on, [object(cell, c6), true], 6).
sensed(on, [object(cell, c7), false], 6).
sensed(on, [object(cell, c8), false], 6).
sensed(on, [object(cell, c9), false], 6).
sensed(on, [object(cell, c10), false], 6).
sensed(on, [object(cell, c11), false], 6).
%
sensed(on, [object(cell, c1), true], 7).
sensed(on, [object(cell, c2), true], 7).
sensed(on, [object(cell, c3), false], 7).
sensed(on, [object(cell, c4), false], 7).
sensed(on, [object(cell, c5), true], 7).
sensed(on, [object(cell, c6), true], 7).
sensed(on, [object(cell, c7), false], 7).
sensed(on, [object(cell, c8), false], 7).
sensed(on, [object(cell, c9), false], 7).
sensed(on, [object(cell, c10), false], 7).
sensed(on, [object(cell, c11), true], 7).
%
sensed(on, [object(cell, c1), false], 8).
sensed(on, [object(cell, c2), true], 8).
sensed(on, [object(cell, c3), false], 8).
sensed(on, [object(cell, c4), true], 8).
sensed(on, [object(cell, c5), true], 8).
sensed(on, [object(cell, c6), true], 8).
sensed(on, [object(cell, c7), false], 8).
sensed(on, [object(cell, c8), false], 8).
sensed(on, [object(cell, c9), false], 8).
sensed(on, [object(cell, c10), true], 8).
sensed(on, [object(cell, c11), true], 8).
%
sensed(on, [object(cell, c1), true], 9).
sensed(on, [object(cell, c2), true], 9).
sensed(on, [object(cell, c3), true], 9).
sensed(on, [object(cell, c4), true], 9).
sensed(on, [object(cell, c5), false], 9).
sensed(on, [object(cell, c6), true], 9).
sensed(on, [object(cell, c7), false], 9).
sensed(on, [object(cell, c8), false], 9).
sensed(on, [object(cell, c9), true], 9).
sensed(on, [object(cell, c10), true], 9).
sensed(on, [object(cell, c11), true], 9).
%
sensed(on, [object(cell, c1), false], 10).
sensed(on, [object(cell, c2), false], 10).
sensed(on, [object(cell, c3), false], 10).
sensed(on, [object(cell, c4), true], 10).
sensed(on, [object(cell, c5), true], 10).
sensed(on, [object(cell, c6), true], 10).
sensed(on, [object(cell, c7), false], 10).
sensed(on, [object(cell, c8), true], 10).
sensed(on, [object(cell, c9), true], 10).
sensed(on, [object(cell, c10), false], 10).
sensed(on, [object(cell, c11), false], 10).

