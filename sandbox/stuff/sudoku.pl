:- module(sudoku, [sudoku/1]).

:- use_module(library(clpfd)).

% Given a 9x9 matrix of variables and integers, solve the sudoku
sudoku(Rows) :-
    % Verify the list of lists is a 9x9 matrix
    length(Rows, 9), maplist(same_length(Rows), Rows),
    % Flatten lists, constrain each element to be in 1..9
    append(Rows, Vs), Vs ins 1..9,
    % Constrain each row to unify with distinct integers
    maplist(all_distinct, Rows),
    % Get the columns in the matrix
    transpose(Rows, Columns),
    % Constrain each column to unify with distinct integers
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    % Constrain each 3x3 block, from each of 3 consecutive rows, to be distinct
    distinct_blocks(As, Bs, Cs),
    distinct_blocks(Ds, Es, Fs),
    distinct_blocks(Gs, Hs, Is),
    % Assign values to variables from their domains (in 1..9) such that they satisfy all accumulated contraints (can backtrack)
    label(Vs).

% We're done recursively constraining blocks in groups of rows
distinct_blocks([], [], []).
% Pick 3 consecutive cells from the same columns from 3 rows
distinct_blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    % Constrain all cell to unify with distinct integers
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    % Recurse for the remaining blocks
    distinct_blocks(Ns1,Ns2,Ns3).

count(P, N) :- 
    problem(P, Rows), 
    setof(Rows, sudoku(Rows), Solutions),
    length(Solutions, N).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).

problem(2, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,_,_,_,_]]).

%% World's hardest Sudoku problem
problem(3, [[8,_,_,_,_,_,_,_,_],
            [_,_,3,6,_,_,_,_,_],
            [_,7,_,_,9,_,2,_,_],
            [_,5,_,_,_,7,_,_,_],
            [_,_,_,_,4,5,7,_,_],
            [_,_,_,1,_,_,_,3,_],
            [_,_,1,_,_,_,_,6,8],
            [_,_,8,5,_,_,_,1,_],
            [_,9,_,_,_,_,4,_,_]]).

ordered(_, []).
ordered(_, [_]).
ordered(Order, [A, B | Rest]) :- 
    zcompare(Order, B, A),
    ordered(Order, [B | Rest]).

print_rows([]) :- !.
print_rows([Row | Rest]) :-
    writeln(Row),
    print_rows(Rest).