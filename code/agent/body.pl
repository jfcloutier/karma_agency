/*
The two-way interface between the SOM and the robot's effectors and actuators.
*/

:- module(body, []).

:- use_module(actor_model(supervisor)).

start(Parent) :-
    start_supervisor(Parent).
    
start_supervisor(Parent) :-
    start_supervisor(Parent, body) :-
    supervisor:start_child(Parent, supervisor, body, [restart(transient)]).