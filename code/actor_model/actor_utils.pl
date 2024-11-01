:- module(actor_utils, [
    self/1, 
    start_actor/2, start_actor/3, exit_actor/1,
    wait_for_actor/1, wait_for_actor_stopped/1, wait_for_actor_stopped/2, 
    send/2, send_at_interval/5, send_control/2, send_control/3, send_message/1, send_message/2, send_query/2, send_query/3, send_query/4,
    empty_state/1, get_state/3, put_state/3, put_state/4,
    pick_some/2]).

:- [load].

:- use_module(code(logger)).
:- use_module(timer).

self(Name) :-
	thread_self(Name).

start_actor(Name, Goal) :-
	start_actor(Name, Goal, []).

start_actor(Name, Goal, Options) :-
	FullOptions = [alias(Name)|Options], 
	thread_create(Goal, _, FullOptions).

exit_actor(Name) :-
	log(debug, actor_model, 'About to exit actor ~w', [Name]), 
	send(Name, 
		control(exit)).

empty_state(state{}).

get_state(State, Key, Value) :-
	is_dict(State, state), 
	get_dict(Key, State, Value).

% Adding/overwriting pairs to a dictionary.

put_state(State, Pairs, NewState) :-
	dict_pairs(State, Tag, StatePairs), 
	merge_pairs(StatePairs, Pairs, MergedPairs), 
	dict_pairs(NewState, Tag, MergedPairs).

put_state(State, Key, Value, NewState) :-
	is_dict(State, state), 
	put_dict(Key, State, Value, NewState).

% Pick a random subset of a list, at least one but no more than half. Deterministic.
pick_some([], []).
pick_some([E], [E]).

pick_some(List, SubList) :-
	sort(List, Set),
	random_permutation(Set, PermutedList), 
	length(Set, L), 
	Max is max(1, round(L / 2)),
	random_between(1, Max, N), 
	take(N, PermutedList, SubList).

take(_, [], []).
take(0, _, []).

take(N, _, []) :-
	N < 0.

take(N, [El|Rest], [El|Others]) :-
	N > 0, N1 is N - 1, 
	take(N1, Rest, Others).

merge_pairs(Pairs, InPairs, MergedPairs) :-
	append(InPairs, Pairs, AllPairs), 
	sort(1,  @>= , AllPairs, SortedAllPairs), 
	group_pairs_by_key(SortedAllPairs, Grouped), 
	pairs_from_groups(Grouped, MergedPairs).

pairs_from_groups([], []).

pairs_from_groups([Key-[Value|_]|Rest], [Key-Value|OtherPairs]) :-
	pairs_from_groups(Rest, OtherPairs).

% Tell a thread to process a message in priority by signalling it

signal(Name, Module, Signal) :-
	log(debug, actor_model, 'Signaling ~w ~w with ~p', [Module, Name, Signal]), 
	is_thread(Name)
	 ->
			(Goal =.. [ : , Module, 
		process_signal(Signal)], 
		thread_signal(Name, Goal));
	log(debug, actor_model, 'Can not signal non-existent thread ~w', [Name]).

% Signal a control message

send_control(Module, Control) :-
	Module : name(Name), 
	send_control(Name, Module, Control).

send_control(Name, Module, Control) :-
	catch(
		signal(Name, Module, 
			control(Control)), Exception, 
		(
			log(warn, actor_model, "Failed to send signal ~p to ~w (~w): ~p", [Control, Name, Module, Exception]), true)).

% Undecorated message sent with best effort.

send(Name, Message) :-
	catch(
		(
			log(debug, actor_model, '~@ is sending ~p to ~w', [self, Message, Name]), 
			thread_send_message(Name, Message)), Exception, 
		(
			log(warn, actor_model, "Failed to send message ~p to ~w: ~p", [Message, Name, Exception]), true)).

% Send undecorated message periodically

send_at_interval(Target, Tag, Message, Delay, Timer) :-
	self(Name), 
	atomic_list_concat([Name, Tag], "_", Timer), 
	timer : start(Timer, 
		thread_send_message(Target, Message), Delay).

% Send self a message

send_message(Message) :-
	thread_self(Name), 
	send_message(Name, Message).

% Send a semantic message

send_message(Name, Message) :-
	thread_self(From), 
	send(Name, 
		message(Message, From)).

% Send self a query

send_query(Question, Answer) :-
	thread_self(Name), 
	send_query(Name, Question, Answer).

% Send a semantic query and wait for an answer

send_query(Name, Question, Answer) :-
	send_query(Name, Question, 5, Answer).

send_query(Name, Question, Timeout, Answer) :-
	log(info, actor_model, '~@ is sending query ~p to ~w', [self, Question, Name]), 
	catch(
		(
			thread_self(From), 
			thread_send_message(Name, 
				query(Question, From)), % Fails (quietly) is a matching message is not received in time
				
			log(debug, actor_model, '~w is waiting for response(Answer, ~p, ~w), timeout is ~w', [From, Question, Name, Timeout]), 
			thread_get_message(From, 
				response(Answer, Question, Name), 
				[timeout(Timeout)]), 
			log(info, actor_model, 'Got answer ~p from ~w to query ~p', [Answer, Name, Question])), Exception, 
		(
			log(warn, actor_model, "Failed to query ~w about ~p: ~p~n", [Name, Question, Exception]), fail)).

% Wait 20s for a actor thread to be alive

wait_for_actor(Name) :-
	wait_for_actor(Name, 20).

wait_for_actor(Name, 0) :-
	log(warn, actor_model, "Failed waiting for actor thread ~w~n", [Name]), !, fail.

wait_for_actor(Name, CountDown) :-
	is_thread(Name)
	 ->
			log(debug, actor_model, 'Actor ~w is running', [Name]), true;
	(
		log(debug, actor_model, 'Waiting for actor thread ~w (~w)', [Name, CountDown]), 
		sleep(0.25), AttemptsLeft is CountDown - 1, 
		wait_for_actor(Name, AttemptsLeft)).

% Wait 15s (60 * 0.25s) for an actor thread to be stopped

wait_for_actor_stopped(Name) :-
	wait_for_actor_stopped(Name, 60).

wait_for_actor_stopped(Name, 0) :-
	log(warn, actor_model, 'Failed waiting for actor thread ~w to be stopped', [Name]), !, fail.

wait_for_actor_stopped(Name, CountDown) :-
	 \+ is_thread(Name) ->
		true;
	(
		log(debug, actor_model, 'Waiting for actor thread stopped ~w (~w)~n', [Name, CountDown]), 
		sleep(0.25), AttemptsLeft is CountDown - 1, 
		wait_for_actor_stopped(Name, AttemptsLeft)).