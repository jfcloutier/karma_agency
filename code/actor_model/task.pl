
/*
Support for delayed async tasks and async tasks with completion logic.
*/
:- module(task, [do_and_then/2, do_after/2]).



:- use_module(actor_model(actor_utils)).
:- use_module(code(logger)).

do_and_then(Task, Completion) :-
    log(debug, task, 'Doing task ~p and then ~p', [Task, Completion]),
	thread_create(Task, Id, [detached(true), at_exit(Completion)]),
    log(debug, task, 'Task ~w completed doing ~p', [Id, Task]).

do_after(Task, DelaySecs) :-
    log(debug, task, 'Doing task ~p and after ~w seconds delay', [Task, DelaySecs]),
    sleep(DelaySecs),
	thread_create(Task, Id, [detached(true)]),
    log(debug, task, 'Task ~w completed doing ~p after ~w seconds', [Id, Task, DelaySecs]).
 