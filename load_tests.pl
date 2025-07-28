:- [
    'tests/sensor_ca.plt',
    'tests/effector_ca.plt'
].

init_som :-
	agent : started('localhost:4000').

terminate_som :-
	supervisor : stopped(agency, 60).

get_message(Term) :-
    get_message(Term, 5).

get_message(Term, Timeout) :-
    thread_self(Name),
    thread_get_message(Name, Term, [timeout(Timeout)]).