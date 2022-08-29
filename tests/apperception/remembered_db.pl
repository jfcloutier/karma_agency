:- module(remembered_db, []).

% remembered(GM, Round, Memory).
remembered(gm_1, 1, sensed(color(floor), is(brown), 1.0)).
remembered(gm_1, 4, enacted(turn_right(small))).
remembered(gm_1, 2, sensed(color(floor), is(green), 0.5)).
remembered(gm_1, 2, enacted(go_forward(big,1.0))).
remembered(gm_2, 2, believed(b1(self), is(true), 0.25)).

remembered( gm_2, 10, believed(b1(self), is(true), 1)).
remembered( gm_2, 10, sensed(b2(self), is(false), 0.5)).
remembered( gm_2, 10, enacted(turn_right(big))).