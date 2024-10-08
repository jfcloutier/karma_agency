
/*
[load].
[code(logger)].
['tests/actor_model/actor_model.plt'].
set_log_level(debug).
run_tests(actor_model:subscribing_unsubscribing).
*/

:- begin_tests(actor_model).

:- use_module(code(logger)).
:- use_module(actor_model(worker)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

:- use_module(bob).
:- use_module(alice).

test(supervisor) :-
	supervisor : start(top), 
	assertion(
		is_thread(top)), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(top)).

test(supervised_static_pubsub) :-
	Children = [pubsub], 
	supervisor : start(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(pubsub)).

test(supervised_dynamic_pubsub) :-
	supervisor : start(top), 
	assertion(
		is_thread(top)), 
	supervisor : start_pubsub(top), 
	assertion(
		is_thread(pubsub)), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(pubsub)).

test(stop_supervisor_with_static_children) :-
	Children = [worker(bob, 
		[topics([party])]), pubsub], 
	supervisor : start(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(bob_clock)), 
	supervisor : children(top, Answer), 
	assertion(
		member(
			child(worker, bob), Answer)), 
	assertion(
		member(
			child(worker, pubsub), Answer)), 
	supervisor : stop(top), % No need to wait since it mas signalled
	
	assertion(
		 \+ is_thread(bob)), 
	assertion(
		 \+ is_thread(bob_clock)), 
	assertion(
		 \+ is_thread(pubsub)), 
	assertion(
		 \+ is_thread(top)).

test(exit_supervisor_with_static_children) :-
	Children = [worker(bob, 
		[topics([party])]), pubsub], 
	supervisor : start(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(bob_clock)), 
	supervisor : children(top, Answer), 
	assertion(
		member(
			child(worker, bob), Answer)), 
	assertion(
		member(
			child(worker, pubsub), Answer)), 
	supervisor : exit(top), % Must wait since this is messaged
	
	sleep(2), 
	assertion(
		 \+ is_thread(bob)), 
	assertion(
		 \+ is_thread(bob_clock)), 
	assertion(
		 \+ is_thread(pubsub)), 
	assertion(
		 \+ is_thread(top)).

test(supervised_dynamic_worker) :-
	supervisor : start(top), 
	assertion(
		is_thread(top)), 
	supervisor : start_worker_child(top, alice, alice, []), 
	assertion(
		is_thread(alice)), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(alice)).

test(supervised_static_supervisor) :-
	Children = [supervisor(bottom, [])], 
	supervisor : start(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(bottom)), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(bottom)).

test(supervised_dynamic_supervisor) :-
	supervisor : start(top), 
	assertion(
		is_thread(top)), 
	supervisor : start_supervisor_child(top, bottom, []), 
	assertion(
		is_thread(bottom)), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(bottom)).

test(supervised_actor_restart) :-
	Children = [pubsub, 
	worker(bob, 
		[topics([]), 
		init(
			[mood(bored)]), 
		restart(permanent)]), 
	worker(alice, 
		[topics([]), 
		init(
			[mood(peaceful)]), 
		restart(transient)])], 
	supervisor : start(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(alice)), % Checking restart
		
	exit_actor(bob), % Wait for restart of permanent bob
	
	wait_for_actor(bob), 
	assertion(
		is_thread(bob)), % Stopping
		
	exit_actor(alice), % alice is transient
	
	wait_for_actor_stopped(alice), 
	assertion(
		 \+ is_thread(alice)), 
	supervisor : stop_child(bottom, worker, bob), 
	assertion(
		 \+ is_thread(bob)), 
	supervisor : stop(top), 
	sleep(1), 
	assertion(
		 \+ is_thread(alice)), 
	assertion(
		 \+ is_thread(bob)), 
	assertion(
		 \+ is_thread(pubsub)), 
	assertion(
		 \+ is_thread(top)).

test(restarting_supervised_supervisors) :-
	% Starting supervisor (top) with a static child supervisor (middle)

	Children = [supervisor(middle, 
		[restart(permanent)])], 
	supervisor : start(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(middle)), % Start dynamic supervisor (bottom) as child of supervisor middle
		
	supervisor : start_supervisor_child(middle, bottom, 
		[restart(permanent)]), 
	assertion(
		is_thread(bottom)), % Exiting permanent supervisor bottom -a dynamic child of middle- restarts bottom
		
	supervisor : exit(bottom), 
	sleep(1), 
	assertion(
		is_thread(bottom)), % Stop the middle supervisor does not restart its dynamic children: bottom are not restarted (even though it is permanent)
		
	supervisor : stop(middle), 
	sleep(1), 
	assertion(
		 \+ is_thread(middle)), 
	assertion(
		 \+ is_thread(bottom)), % Stop the top supervisor and thus every descendant.
			
	supervisor : stop(top), 
	sleep(1), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(middle)).

test(subscribing_unsubscribing) :-
	supervisor : start(top, 
		[children([pubsub, 
			worker(bob, 
				[topics([party, police]), 
				init(
					[mood(bored)]), 
				restart(transient)]), 
			worker(alice, alice, 
				[topics([]), 
				init(
					[mood(peaceful)]), 
				restart(transient)])])]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(alice)), 
	unsubscribe(bob, party), 
	subscribe_all(alice, [party, police]), 
	unsubscribe_all(alice), 
	sleep(1), 
	supervisor : stop(top), 
	assertion(
		 \+ is_thread(pubsub)), 
	assertion(
		 \+ is_thread(alice)), 
	assertion(
		 \+ is_thread(bob)), 
	assertion(
		 \+ is_thread(top)).

test(communicating_with_supervised_static_children) :-
	supervisor : start(top, 
		[children([pubsub])]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	Children = [worker(bob, 
		[topics([party, police]), 
		init(
			[mood(bored)]), 
		restart(permanent)]), 
	worker(alice, alice, 
		[topics([party, police]), 
		init(
			[mood(peaceful)]), 
		restart(transient)])], 
	supervisor : start_supervisor_child(top, bottom, 
		[children(Children)]), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(alice)), % Querying
		
	assertion(
		send_query(bob, mood, bored)), 
	assertion(
		send_query(alice, mood, peaceful)), % Eventing
		
	publish(party, [alice, bob]), % Give time for workers to respond to published messages
	
	sleep(1), % Checking state changes from event
	
	assertion(
		 \+ send_query(bob, mood, bored)), 
	assertion(
		 \+ send_query(peter, mood, bored)), 
	assertion(
		send_query(bob, mood, panicking)), % Unsubscribing
		
	unsubscribe(bob, party), 
	supervisor : stop(top), 
	sleep(1), 
	assertion(
		 \+ is_thread(pubsub)), 
	assertion(
		 \+ is_thread(alice)), 
	assertion(
		 \+ is_thread(bob)), 
	assertion(
		 \+ is_thread(bottom)), 
	assertion(
		 \+ is_thread(top)).

:- end_tests(actor_model).