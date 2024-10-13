:- module(task, [do_and_then/2]).

:- [load].

:- use_module(actor_model(actor_utils)).
:- use_module(code(logger)).

do_and_then(Task, Completion) :-
    log(debug, task, 'Task ~w doing ~p and then ~p', [Id, Task, Completion]),
	thread_create(Task, Id, [detached(true), at_exit(Completion)]),
    log(debug, task, 'Task ~w completed doing ~p', [Id, Task]).