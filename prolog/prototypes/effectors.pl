:- module(effectors, [effector/2, effect/3]).

effector(left_motor, motor).
effector(right_motor, motor).

effect(motor, run_positive).
effect(motor, run_negative).
