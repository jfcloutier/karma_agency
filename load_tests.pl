:- [
    'tests/sensor_ca.plt',
    'tests/effector_ca.plt',
    'tests/dynamic_ca.plt'
].

init_static_som :-
	agent : started('localhost:4000', false).

init_som :-
    agent : started('localhost:4000', true).

terminate_som :-
	supervisor : stopped(agency, 60).

get_message(Term) :-
    get_message(Term, 5).

get_message(Term, Timeout) :-
    thread_self(Name),
    thread_get_message(Name, Term, [timeout(Timeout)]).