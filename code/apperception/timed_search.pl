:- module(timed_search, [search/4, check_time/0]).

:- use_module(library(chr)).
:- use_module(library(clpfd)).
:- use_module(library(aggregate)).

:- chr_constraint candidate/2, best_candidate/1,  max_time/1, get_max_time/1.

% Only keep the best rated candidate predictor
candidate(_Candidate1, Rating1) \ candidate(_Candidate2, Rating2) <=> Rating1 #>= Rating2 | true.

% Candidate extraction from the store
best_candidate(P2), candidate(P1, _)#passive <=> P1 = P2.

% The first argument of the candidate-generating goal is a candidate solution
% The first arg of the rated is the value being rated, the second arg is the rating
search(Module:Goal, Module:Rater, Seconds, Answer) :-
   timebox(Seconds),
   Goal =.. [_, Result | _],
   TimedGoal =.. [timed, Module:Goal],
   aggregate(bag(Result), TimedGoal, Answers),
   select_answer(Answers, Module:Rater, Answer).

timed(Goal) :-
    call(Goal),
    (time_expired() -> ! ; true).

select_answer(Answers, Module:Rater, Answer) :-
    submit_candidates(Answers, Module:Rater),
    best_candidate(Answer).

submit_candidates([], _).
submit_candidates([Answer | Others], Module:Rater) :-
    Rater =.. [Predicate, _, _ | Params],
    Evaluator =.. [Predicate, Answer, Rating | Params],
    call(Module:Evaluator),
    candidate(Answer, Rating),
    submit_candidates(Others, Module:Rater).

% Timeboxing
max_time(_) \ max_time(_)#passive <=> true.
max_time(Max) \ get_max_time(T) <=> Max = T.

timebox(Seconds) :-
    get_time(Now),
    T is Now+Seconds,
    max_time(T).

time_expired() :-
    get_max_time(T),
    get_time(Current),
    Current>T, !,
    writeln('TIME EXPIRED ').

check_time() :-
    time_expired() -> ! ; true.
