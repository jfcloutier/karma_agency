:- module(build,
          [ build_temple/0
          ]).

:- use_module(library(chr)).
:- use_module(library(lists)).

:- chr_constraint put_in_place(+any, +natural).

% Constraint: It is wrong to put in place a part before another that supports it.
out_of_order@put_in_place(P0, Time0), put_in_place(P1, Time1) ==> supports(P0, P1), Time0 > Time1 | fail.

% Build me a temple!
build_temple :-
    % Assume there's a ground
    put_in_place(soil, 0),
    %Gather the parts
    parts(Parts),
    % Then start building it
    build_it(Parts, 1).

% We're done building the temple when we've run out of parts
build_it([], _).

% To build a temple from parts at time Time...
build_it(Parts, Time) :-
    % pick a part, any part, leaving the rest of the parts
    select(P, Parts, RestParts),
    % assume you can put it in place
    put_in_place(P, Time),
    % advance the clock
    Time1 is Time+1,
    % build the temple with the rest of the parts
    build_it(RestParts, Time1).

% Here is the list of parts.
parts([gable, c1, c2, f1, f2]).

% What part supports what other part
supports(soil, f1).
supports(f1, f2).
supports(f2, c1).
supports(f2, c2).
supports(c1, gable).
supports(c2, gable).

% ['sandbox/stuff/build'].
% build_temple.