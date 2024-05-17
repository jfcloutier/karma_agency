
/*
[load].
[code(logger)].
set_log_level(debug).
['tests/actor_model/actor_model.plt'].
run_tests(actor_model).
*/

:- begin_tests(actor_model).

:- use_module(actor_model(worker)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

:- use_module(bob).
:- use_module(alice).

test(supervised_pubsub) :-
    supervisor:start(top),
    assertion(is_thread(top)),
    supervisor:start_pubsub(top),
    assertion(is_thread(pubsub)),
    supervisor:stop(top),
    assertion(\+ is_thread(top)),
    assertion(\+ is_thread(pubsub)).

test(supervised_static_worker) :-
    Children = [worker(bob, [topics([party])]), pubsub],
    supervisor:start(top, [children(Children)]),
    assertion(is_thread(top)),
    assertion(is_thread(pubsub)),
    assertion(is_thread(bob)),
    supervisor:stop(top),
    assertion(\+ is_thread(top)),
    assertion(\+ is_thread(bob)),
    assertion(\+ is_thread(pubsub)).

test(supervised_dynamic_worker) :-
    supervisor:start(top),
    assertion(is_thread(top)),
    supervisor:start_worker_child(top, alice, alice, []),
    assertion(is_thread(alice)),
    supervisor:stop(top),
    assertion(\+ is_thread(top)),
    assertion(\+ is_thread(alice)).

test(supervised_static_supervisor) :-
    Children = [supervisor(bottom, [])],
    supervisor:start(top, [children(Children)]),
    assertion(is_thread(top)),
    assertion(is_thread(bottom)),
    supervisor:stop(top),
    assertion(\+ is_thread(top)),
    assertion(\+ is_thread(bottom)).

test(supervised_dynamic_supervisor) :-
    supervisor:start(top),
    assertion(is_thread(top)),
    supervisor:start_supervisor_child(top, bottom, []),
    assertion(is_thread(bottom)),
    supervisor:stop(top),
    assertion(\+ is_thread(top)),
    assertion(\+ is_thread(bottom)).

test(supervisor_with_static_children) :-
    supervisor:start(top, [children([pubsub])]),
    assertion(is_thread(top)),
    assertion(is_thread(pubsub)),
    
    Children = [
        worker(bob,  [topics([party, police]), init([mood(bored)]),  restart(permanent)]),
        worker(alice, alice,  [topics([party, police]), init([mood(peaceful)]), restart(transient)])
        ],
    supervisor:start_supervisor_child(top, bottom, [children(Children)]),
        assertion(is_thread(bob)),
        assertion(is_thread(alice)),
    % Querying
    assertion(send_query(bob, mood, bored)),
    assertion(send_query(alice, mood, peaceful)),

    % Eventing
    publish(party, [alice, bob]),
        % Give time for workers to respond to published messages
        sleep(1),
        % Checking state changes from event
        assertion(\+ send_query(bob, mood, bored)),
        assertion(\+ send_query(peter, mood, bored)),
        assertion(send_query(bob, mood, panicking)),

     % Unsubscribing
       worker:unsubscribe(bob, party),
     % Checking restart
     worker:stop(bob),
     % Wait for restart of permanent bob
     wait_for_actor(bob),
        assertion(is_thread(bob)),
    % Stopping
    worker:stop(alice),
        % alice is transient
        assertion(\+ is_thread(alice)),
    supervisor:kill_child(bottom, worker, bob),
        sleep(1),
        assertion(\+ is_thread(bob)),
        supervisor:stop(top),
            sleep(1),
            assertion(\+ is_thread(pubsub)),
            assertion(\+ is_thread(bottom)),
            assertion(\+ is_thread(top)).

test(restarting_supervised_supervisors) :-
    % Starting supervisor (top) with a static child supervisor (middle)
    Children = [pubsub, supervisor(middle, [restart(permanent)])],
    supervisor:start(top, [children(Children)]),
    assertion(is_thread(top)),
    assertion(is_thread(middle)),

    % Start dynamic supervisor (bottom) as child of supervisor middle
    supervisor:start_supervisor_child(middle, bottom, [restart(permanent)]),
    assertion(is_thread(bottom)),

    % Stopping permanent supervisor bottom -a dynamic child of middle- restarts bottom
    supervisor:stop(bottom),
    sleep(1),
    assertion(is_thread(bottom)),

    % Stop the middle supervisor does not restart its dynamic children: bottom is not restarted (even though it is permanent)
    supervisor:stop(middle),
    sleep(1),
    assertion(is_thread(middle)),
    assertion(\+ is_thread(bottom)),

    % Stop the top supervisor and thus every descendant.
    supervisor:stop(top),
    sleep(1),
    assertion(\+ is_thread(top)),
    assertion(\+ is_thread(middle)).

:- end_tests(actor_model).