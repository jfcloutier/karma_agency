:- module(remembered_db,
          [ remembered/3
          ]).

% remembered(Round, Agent, GM, Memory).
remembered(1, gm_1, sensed(color(floor), is(brown))).
remembered(4, gm_1, enacted(turn_right(small))).
remembered(2, gm_1, sensed(color(floor), is(green))).
remembered(2, gm_1, enacted(go_forward(big))).
remembered(2, gm_2, believed(b1(self), is(true))).

remembered(10, gm_2, believed(b1(self), is(true))).
remembered(10, gm_2, sensed(b2(self), is(false))).
remembered(10, gm_2, enacted(turn_right(small))).