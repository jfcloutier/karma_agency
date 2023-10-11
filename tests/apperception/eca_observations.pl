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

sensed(right_of, [object(cell, c1), object(cell, c11)], 1).
sensed(right_of, [object(cell, c2), object(cell, c1)],  1).
sensed(right_of, [object(cell, c3), object(cell, c2)],  1).
sensed(right_of, [object(cell, c4), object(cell, c3)],  1).
sensed(right_of, [object(cell, c5), object(cell, c4)],  1).
sensed(right_of, [object(cell, c6), object(cell, c5)],  1).
sensed(right_of, [object(cell, c7), object(cell, c6)],  1).
sensed(right_of, [object(cell, c8), object(cell, c7)],  1).
sensed(right_of, [object(cell, c9), object(cell, c8)],  1).
sensed(right_of, [object(cell, c10), object(cell, c9)], 1).
sensed(right_of, [object(cell, c11), object(cell, c10)],1).

sensed(right_of, [object(cell, c1), object(cell, c11)], 2).
sensed(right_of, [object(cell, c2), object(cell, c1)],  2).
sensed(right_of, [object(cell, c3), object(cell, c2)],  2).
sensed(right_of, [object(cell, c4), object(cell, c3)],  2).
sensed(right_of, [object(cell, c5), object(cell, c4)],  2).
sensed(right_of, [object(cell, c6), object(cell, c5)],  2).
sensed(right_of, [object(cell, c7), object(cell, c6)],  2).
sensed(right_of, [object(cell, c8), object(cell, c7)],  2).
sensed(right_of, [object(cell, c9), object(cell, c8)],  2).
sensed(right_of, [object(cell, c10), object(cell, c9)], 2).
sensed(right_of, [object(cell, c11), object(cell, c10)],2).

sensed(right_of, [object(cell, c1), object(cell, c11)], 3).
sensed(right_of, [object(cell, c2), object(cell, c1)],  3).
sensed(right_of, [object(cell, c3), object(cell, c2)],  3).
sensed(right_of, [object(cell, c4), object(cell, c3)],  3).
sensed(right_of, [object(cell, c5), object(cell, c4)],  3).
sensed(right_of, [object(cell, c6), object(cell, c5)],  3).
sensed(right_of, [object(cell, c7), object(cell, c6)],  3).
sensed(right_of, [object(cell, c8), object(cell, c7)],  3).
sensed(right_of, [object(cell, c9), object(cell, c8)],  3).
sensed(right_of, [object(cell, c10), object(cell, c9)], 3).
sensed(right_of, [object(cell, c11), object(cell, c10)],3).

sensed(right_of, [object(cell, c1), object(cell, c11)], 4).
sensed(right_of, [object(cell, c2), object(cell, c1)],  4).
sensed(right_of, [object(cell, c3), object(cell, c2)],  4).
sensed(right_of, [object(cell, c4), object(cell, c3)],  4).
sensed(right_of, [object(cell, c5), object(cell, c4)],  4).
sensed(right_of, [object(cell, c6), object(cell, c5)],  4).
sensed(right_of, [object(cell, c7), object(cell, c6)],  4).
sensed(right_of, [object(cell, c8), object(cell, c7)],  4).
sensed(right_of, [object(cell, c9), object(cell, c8)],  4).
sensed(right_of, [object(cell, c10), object(cell, c9)], 4).
sensed(right_of, [object(cell, c11), object(cell, c10)],4).

sensed(right_of, [object(cell, c1), object(cell, c11)], 5).
sensed(right_of, [object(cell, c2), object(cell, c1)],  5).
sensed(right_of, [object(cell, c3), object(cell, c2)],  5).
sensed(right_of, [object(cell, c4), object(cell, c3)],  5).
sensed(right_of, [object(cell, c5), object(cell, c4)],  5).
sensed(right_of, [object(cell, c6), object(cell, c5)],  5).
sensed(right_of, [object(cell, c7), object(cell, c6)],  5).
sensed(right_of, [object(cell, c8), object(cell, c7)],  5).
sensed(right_of, [object(cell, c9), object(cell, c8)],  5).
sensed(right_of, [object(cell, c10), object(cell, c9)], 5).
sensed(right_of, [object(cell, c11), object(cell, c10)],5).

sensed(right_of, [object(cell, c1), object(cell, c11)], 6).
sensed(right_of, [object(cell, c2), object(cell, c1)],  6).
sensed(right_of, [object(cell, c3), object(cell, c2)],  6).
sensed(right_of, [object(cell, c4), object(cell, c3)],  6).
sensed(right_of, [object(cell, c5), object(cell, c4)],  6).
sensed(right_of, [object(cell, c6), object(cell, c5)],  6).
sensed(right_of, [object(cell, c7), object(cell, c6)],  6).
sensed(right_of, [object(cell, c8), object(cell, c7)],  6).
sensed(right_of, [object(cell, c9), object(cell, c8)],  6).
sensed(right_of, [object(cell, c10), object(cell, c9)], 6).
sensed(right_of, [object(cell, c11), object(cell, c10)],6).

sensed(right_of, [object(cell, c1), object(cell, c11)], 7).
sensed(right_of, [object(cell, c2), object(cell, c1)],  7).
sensed(right_of, [object(cell, c3), object(cell, c2)],  7).
sensed(right_of, [object(cell, c4), object(cell, c3)],  7).
sensed(right_of, [object(cell, c5), object(cell, c4)],  7).
sensed(right_of, [object(cell, c6), object(cell, c5)],  7).
sensed(right_of, [object(cell, c7), object(cell, c6)],  7).
sensed(right_of, [object(cell, c8), object(cell, c7)],  7).
sensed(right_of, [object(cell, c9), object(cell, c8)],  7).
sensed(right_of, [object(cell, c10), object(cell, c9)], 7).
sensed(right_of, [object(cell, c11), object(cell, c10)],7).

sensed(right_of, [object(cell, c1), object(cell, c11)], 8).
sensed(right_of, [object(cell, c2), object(cell, c1)],  8).
sensed(right_of, [object(cell, c3), object(cell, c2)],  8).
sensed(right_of, [object(cell, c4), object(cell, c3)],  8).
sensed(right_of, [object(cell, c5), object(cell, c4)],  8).
sensed(right_of, [object(cell, c6), object(cell, c5)],  8).
sensed(right_of, [object(cell, c7), object(cell, c6)],  8).
sensed(right_of, [object(cell, c8), object(cell, c7)],  8).
sensed(right_of, [object(cell, c9), object(cell, c8)],  8).
sensed(right_of, [object(cell, c10), object(cell, c9)], 8).
sensed(right_of, [object(cell, c11), object(cell, c10)],8).

sensed(right_of, [object(cell, c1), object(cell, c11)], 9).
sensed(right_of, [object(cell, c2), object(cell, c1)],  9).
sensed(right_of, [object(cell, c3), object(cell, c2)],  9).
sensed(right_of, [object(cell, c4), object(cell, c3)],  9).
sensed(right_of, [object(cell, c5), object(cell, c4)],  9).
sensed(right_of, [object(cell, c6), object(cell, c5)],  9).
sensed(right_of, [object(cell, c7), object(cell, c6)],  9).
sensed(right_of, [object(cell, c8), object(cell, c7)],  9).
sensed(right_of, [object(cell, c9), object(cell, c8)],  9).
sensed(right_of, [object(cell, c10), object(cell, c9)], 9).
sensed(right_of, [object(cell, c11), object(cell, c10)],9).

sensed(right_of, [object(cell, c1), object(cell, c11)], 10).
sensed(right_of, [object(cell, c2), object(cell, c1)],  10).
sensed(right_of, [object(cell, c3), object(cell, c2)],  10).
sensed(right_of, [object(cell, c4), object(cell, c3)],  10).
sensed(right_of, [object(cell, c5), object(cell, c4)],  10).
sensed(right_of, [object(cell, c6), object(cell, c5)],  10).
sensed(right_of, [object(cell, c7), object(cell, c6)],  10).
sensed(right_of, [object(cell, c8), object(cell, c7)],  10).
sensed(right_of, [object(cell, c9), object(cell, c8)],  10).
sensed(right_of, [object(cell, c10), object(cell, c9)], 10).
sensed(right_of, [object(cell, c11), object(cell, c10)],10).

