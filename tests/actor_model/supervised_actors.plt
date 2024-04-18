
:- begin_tests(actor_model).

:- use_module(actor_model(worker)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

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
    publish(party, [alice, bob]),
        % Give time for workers to respond to published messages
        sleep(1),
    stop_bob,
    % Wait for restart of permanent bob
    wait_for_actor(bob),
        assertion(is_thread(bob)),
    supervisor:kill_child(top, worker, bob),
    stop_alice,
    supervisor:kill_child(top, pubsub), 
    supervisor:stop(top),
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
    publish(party, [alice, bob]),
        % Give time for workers to respond to published messages
        sleep(1),
    supervisor:stop(top),
        assertion(\+ is_thread(pubsub)),
        assertion(\+ is_thread(bob)),
        assertion(\+ is_thread(alice)),
        assertion(\+ is_thread(bottom)),
        assertion(\+ is_thread(top)).

:- end_tests(actor_model).