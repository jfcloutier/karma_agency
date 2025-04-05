
/*
[load].
[code(logger)].
['tests/actor_model/actor_model.plt'].
set_log_level(debug).
run_tests(actor_model:subscribing_unsubscribing).
*/
[load].

:- begin_tests(actor_model).

:- use_module(code(logger)).
:- use_module(actor_model(worker)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

:- use_module(bob).
:- use_module(alice).

test(supervisor) :-
	supervisor : started(top), 
	assertion(
		is_thread(top)), 
	supervisor : stopped(top), 
	assertion(
		 \+ is_thread(top)).

test(supervised_static_pubsub) :-
	Children = [pubsub], 
	supervisor : started(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	supervisor : stopped(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(pubsub)).

test(supervised_dynamic_pubsub) :-
	supervisor : started(top), 
	assertion(
		is_thread(top)), 
	supervisor : pubsub_started(top), 
	assertion(
		is_thread(pubsub)), 
	supervisor : stopped(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(pubsub)).

test(stop_supervisor_with_static_children) :-
	Children = [worker(bob, 
		[topics([party])]), pubsub], 
	supervisor : started(top, 
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
	supervisor : stopped(top), % No need to wait since it mas signalled
	
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
	supervisor : started(top, 
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
	supervisor : started(top), 
	assertion(
		is_thread(top)), 
	supervisor : worker_child_started(top, alice, alice, []), 
	assertion(
		is_thread(alice)), 
	supervisor : stopped(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(alice)).

test(supervised_static_supervisor) :-
	Children = [supervisor(bottom, [])], 
	supervisor : started(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(bottom)), 
	supervisor : stopped(top), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(bottom)).

test(supervised_dynamic_supervisor) :-
	supervisor : started(top), 
	assertion(
		is_thread(top)), 
	supervisor : supervisor_child_started(top, bottom, []), 
	assertion(
		is_thread(bottom)), 
	supervisor : stopped(top), 
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
		restarted(permanent)]), 
	worker(alice, 
		[topics([]), 
		init(
			[mood(peaceful)]), 
		restarted(transient)])], 
	supervisor : started(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(alice)), % Checking restart
		
	actor_exited(bob), % Wait for restart of permanent bob
	
	actor_ready(bob), 
	assertion(
		is_thread(bob)), % Stopping
		
	actor_exited(alice), % alice is transient
	
	actor_stopped(alice), 
	assertion(
		 \+ is_thread(alice)), 
	supervisor : child_stopped(bottom, worker, bob), 
	assertion(
		 \+ is_thread(bob)), 
	supervisor : stopped(top), 
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
		[restarted(permanent)])], 
	supervisor : started(top, 
		[children(Children)]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(middle)), % Start dynamic supervisor (bottom) as child of supervisor middle
		
	supervisor : supervisor_child_started(middle, bottom, 
		[restarted(permanent)]), 
	assertion(
		is_thread(bottom)), % Exiting permanent supervisor bottom -a dynamic child of middle- restarts bottom
		
	supervisor : exit(bottom), 
	sleep(1), 
	assertion(
		is_thread(bottom)), % Stop the middle supervisor does not restart its dynamic children: bottom are not restarted (even though it is permanent)
		
	supervisor : stopped(middle), 
	sleep(1), 
	assertion(
		 \+ is_thread(middle)), 
	assertion(
		 \+ is_thread(bottom)), % Stop the top supervisor and thus every descendant.
			
	supervisor : stopped(top), 
	sleep(1), 
	assertion(
		 \+ is_thread(top)), 
	assertion(
		 \+ is_thread(middle)).

test(subscribing_unsubscribing) :-
	supervisor : started(top, 
		[children([pubsub, 
			worker(bob, 
				[topics([party, police]), 
				init(
					[mood(bored)]), 
				restarted(transient)]), 
			worker(alice, alice, 
				[topics([]), 
				init(
					[mood(peaceful)]), 
				restarted(transient)])])]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(alice)), 
	unsubscribed(bob, party), 
	all_subscribed(alice, [party, police]), 
	all_unsubscribed(alice), 
	sleep(1), 
	supervisor : stopped(top), 
	assertion(
		 \+ is_thread(pubsub)), 
	assertion(
		 \+ is_thread(alice)), 
	assertion(
		 \+ is_thread(bob)), 
	assertion(
		 \+ is_thread(top)).

test(communicating_with_supervised_static_children) :-
	supervisor : started(top, 
		[children([pubsub])]), 
	assertion(
		is_thread(top)), 
	assertion(
		is_thread(pubsub)), 
	Children = [worker(bob, 
		[topics([party, police]), 
		init(
			[mood(bored)]), 
		restarted(permanent)]), 
	worker(alice, alice, 
		[topics([party, police]), 
		init(
			[mood(peaceful)]), 
		restarted(transient)])], 
	supervisor : supervisor_child_started(top, bottom, 
		[children(Children)]), 
	assertion(
		is_thread(bob)), 
	assertion(
		is_thread(alice)), % Querying
		
	assertion(
		query_sent(bob, mood, bored)), 
	assertion(
		query_sent(alice, mood, peaceful)), % Eventing
		
	publish(party, [alice, bob]), % Give time for workers to respond to published messages
	
	sleep(1), % Checking state changes from event
	
	assertion(
		 \+ query_sent(bob, mood, bored)), 
	assertion(
		 \+ query_sent(peter, mood, bored)), 
	assertion(
		query_sent(bob, mood, panicking)), % Unsubscribing
		
	unsubscribed(bob, party), 
	supervisor : stopped(top), 
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