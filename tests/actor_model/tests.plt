%% ['tests/actor_model/tests.plt'].
%% run_tests.

:- begin_tests(actor_model).

:- use_module(actor_model(worker)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).

:- use_module(tests('actor_model/bob')).
:- use_module(tests('actor_model/alice')).


test(supervisor) :-
    supervisor:start(top),
        assertion(is_thread(top)),
    supervisor:start_child(top, pubsub, [restart(transient)]),
        assertion(is_thread(pubsub)),
    start_bob(top),
        assertion(is_thread(bob)),
    start_alice(top),
        assertion(is_thread(alice)),
    pubsub:publish(party, [alice, bob]),
        sleep(1),
    stop_bob,
    supervisor:kill_child(top, worker, bob),
    stop_alice,
    supervisor:kill_child(top, pubsub), 
    supervisor:stop(top),
        % TODO - needs a solution to needing to sleep after supervisor:stop 
        sleep(1),
        assertion(\+ is_thread(top)),
        assertion(\+ is_thread(pubsub)),
        assertion(\+ is_thread(bob)),
        assertion(\+ is_thread(alice)).
 
 test(supervisors, [nondet]) :-
    supervisor:start(top),
    supervisor:start_child(top, pubsub, [restart(transient)]),
    supervisor:start_child(top, supervisor, bottom, [restart(transient)]),
    start_bob(bottom),
    start_alice(bottom),
    pubsub:publish(party, [alice, bob]),
        sleep(1),
    supervisor:stop(top),
        % TODO - needs a solution
        sleep(1),
        assertion(\+ is_thread(pubsub)),
        assertion(\+ is_thread(bob)),
        assertion(\+ is_thread(alice)),
        assertion(\+ is_thread(bottom)),
        assertion(\+ is_thread(top)).

:- end_tests(actor_model).