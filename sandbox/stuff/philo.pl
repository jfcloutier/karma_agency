:- module(philos, [dinner/1]).

:- use_module(library(chr)).
:- use_module(library(lists)).

% What will be abduced (assumed), subject to integrity constraints
:- chr_constraint using_chopstick/3, eating/3.

% Integrity constraint:
% Two philosophers can not use the same chopstick at the same time!
using_chopstick(P1, C, T), using_chopstick(P2, C, T) ==> P1 \== P2 | fail.

% Facts:
% Four philosophers can be seated at a table. 
% They share their chopsticks (this is pre-COVID)
philosopher(plato, chopstick_1, chopstick_2).
philosopher(socrates, chopstick_2, chopstick_3).
philosopher(kant, chopstick_3, chopstick_4).
philosopher(kierkegaard, chopstick_4, chopstick_1).

% Choice: Deductive rule with four possible resolutions.
% A time slot for eating starts at hour 0, 1, 2, or 3 and lasts one hour. 
time_slot(T1, T2) :- member(T1, [0,1,2,3]), T2 is T1 + 1.

% Problem to be solved:
% Sit a given philosopher for dinner, 
% in some time slot,
% eating for an hour,
% using their chopsticks.
dinner(P) :-
    % Fact - the philosopher P uses chopsticks C1 and C2
    philosopher(P, C1, C2),
    % Choice - during one of 4 time slots TBD
    time_slot(T1, T2),
    % ASSUMING they can use their chopsticks
    using_chopstick(P, C1, T1),
    using_chopstick(P, C2, T1),
    % and thus are eating
    eating(P, T1, T2).

% dinner(plato), dinner(socrates), dinner(kant), dinner(kierkegaard).
