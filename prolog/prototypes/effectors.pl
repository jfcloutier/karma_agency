:- module(effectors, [effector/2, can_effect/3]).

effector(left_motor, motor).
effector(right_motor, motor).

can_effect(motor, run_positive).
can_effect(motor, run_negative).
