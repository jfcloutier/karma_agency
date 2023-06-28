:- module(logger, [log/3, log/4, log/5, set_log_level/1, log_level/1, ignore_log_topic/1, reset_logging/0]).

/*
[load].
[code(logger)].

reset_logging,
log(debug, test, 'This is a debug test'),
log(debug, test, 'This is a debug test ~p', [1]),
log(warn, test, 'This is a warn test'),
log(warn, test, 'This is a warn test ~p', [1]),
logger:level(Level),
logger:ignored(AllIgnored).

reset_logging,
set_log_level(info),
log(debug, test, 'This is a debug test'),
log(debug, test, 'This is a debug test ~p', [1]),
log(info, test, 'This is an info test'),
log(info, test, 'This is an info test ~p', [1]),
log(warn, test, 'This is a warn test'),
log(warn, test, 'This is a warn test ~p', [1]),
logger:level(Level),
logger:ignored(AllIgnored).

reset_logging,
set_log_level(error),
log(debug, test, 'This is a debug test'),
log(debug, test, 'This is a debug test ~p', [1]),
log(warn, test, 'This is a warn test'),
log(warn, test, 'This is a warn test ~p', [1]),
logger:level(Level),
logger:ignored(AllIgnored).

reset_logging,
set_log_level(info),
ignore_log_topic(test1).
log(debug, test, 'This is a debug test'),
log(debug, test1, 'This is a debug test1 ~p', [bla]),
log(info, test, 'This is an info test'),
log(info, test1, 'This is an info test1 ~p', [bla]),
log(warn, test, 'This is a warn test'),
log(warn, test1, 'This is a warn test1 ~p', [bla]),
logger:level(Level),
logger:ignored(AllIgnored).

set_log_level(info),
ignore_log_topic(test1).
reset_logging,
log(debug, test, 'This is a debug test'),
log(debug, test1, 'This is a debug test1 ~p', [bla]),
log(info, test, 'This is an info test'),
log(info, test1, 'This is an info test1 ~p', [bla]),
log(warn, test, 'This is a warn test'),
log(warn, test1, 'This is a warn test1 ~p', [bla]),
logger:level(Level),
logger:ignored(AllIgnored).
*/

:- dynamic(level/1).
:- dynamic(ignored/1).

levels([debug, info, warn, note, error]).

level(note).
ignored([]).

log_level(Level) :-
    level(Level).

set_log_level(Level) :-
    retractall(level(_)),
    asserta(level(Level)).

ignore_log_topic(Topic) :-
    ignored(AllIgnored),
    (memberchk(Topic, AllIgnored) -> NowIgnored = AllIgnored ; NowIgnored = [Topic | AllIgnored]),
    retractall(ignored(_)),
    asserta(ignored(NowIgnored)).

reset_logging :-
    retractall(level(_)),
    retractall(ignored(_)),
    asserta(level(note)),
    asserta(ignored([])).

log(Level, Topic, Message, Params, sleep(Secs)) :-
    log(Level, Topic, Message, Params),
    sleep(Secs), !.

log(Level, Topic, Message, sleep(Secs)) :-
    log(Level, Topic, Message),
    sleep(Secs), !.

log(Level, Topic, Message, Params) :-
    level_covered(Level),
    topic_covered(Topic),
    add_meta(Level, Topic, Message, Params, Line, ParamsPlus),
    format(Line, ParamsPlus), !.

log(_, _, _, _).

log(Level, Topic, Message) :-
    level_covered(Level),
    topic_covered(Topic),
    add_meta(Level, Topic, Message, [], Line, Params),
    format(Line, Params), !.

log(_, _, _).

add_meta(Level, Topic, Message, Params, Line, [Time, Level, Topic | Params]) :-
    string_concat(Message, '~n', Message1),
    time_now(Time),
    string_concat('~p ~p [~p] ', Message1, Line), !.

level_covered(Level) :-
    levels(Levels),
    level(MinLevel),
    nth1(Index, Levels, Level, _),
    nth1(MinIndex, Levels, MinLevel, _),
    Index >= MinIndex, !.

topic_covered(Topic) :-
    ignored(IgnoredTopics),
    \+ memberchk(Topic, IgnoredTopics), !.

time_now(Time) :-
    get_time(Timestamp),
    format_time(atom(Time), '%H:%M:%S.%f', Timestamp).


