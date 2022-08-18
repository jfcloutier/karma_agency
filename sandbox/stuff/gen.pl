% Can't generate and accumulate constraints while backtracking.
% The CHR store is on the execution stack...
:- module(gen,
          [ test/1
          ]).

:- use_module(library(chr)).
:- use_module(library(random)).
:- use_module(library(aggregate)).

:- chr_option(check_guard_bindings, on).
:- chr_constraint item(+int), extract(?), max_time(+), get_max_time(-).

item(X)\item(Y)<=>X>=Y| true.
extract(X), item(Y)<=>X=Y.
extract(_)<=>true.

% Timeboxing
max_time(_)\max_time(_)#passive<=>true.
max_time(T)\get_max_time(M)<=>T=M.

test(Max) :-
    timebox(5),
    randseq(20, 1000, S),
    aggregate(bag(Item), solution(S, Item), Items),
    length(Items, L),
    format('Found ~d items', [L]),
    assume_items(Items),
    extract(Max).
    
solution(S, Item) :-
    member(Item, S), % choice point for solutions
    format('Solution ~d~n', [Item]),
    (time_expired() ->   ! ; sleep(1)).

% Add constraints without backtracking. So no forall, findall etc.
assume_items([]).
assume_items([Item|Rest]) :-
    item(Item),
    assume_items(Rest).

timebox(Seconds) :-
    get_time(Now),
    T is Now+Seconds,
    max_time(T).

time_expired() :-
    get_max_time(T),
    get_time(Current),
    Current>T, !,
    writeln('TIME EXPIRED ').

% test(Max).