:- module(actor_utils, [
    self/1, 
    start_actor/2, start_actor/3, 
    wait_for_actor/1, wait_for_actor_stopped/1, wait_for_actor_stopped/2, 
    send/2, send_at_interval/5, send_control/1, send_control/2, send_message/1, send_message/2, send_query/2, send_query/3, send_query/4,
    empty_state/1, get_state/3, put_state/3, put_state/4]).

:- use_module(code(logger)).
:- use_module(timer).
    
self(Name) :-
    thread_self(Name).

start_actor(Name, Goal) :-
    start_actor(Name, Goal, []).

start_actor(Name, Goal, Options) :-
    FullOptions = [alias(Name) | Options],
    thread_create(Goal, _, FullOptions).

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

merge_pairs(Pairs, InPairs, MergedPairs) :-
    append(InPairs, Pairs, AllPairs),
    sort(1, @>=, AllPairs, SortedAllPairs),
    group_pairs_by_key(SortedAllPairs, Grouped),
    pairs_from_groups(Grouped, MergedPairs).

pairs_from_groups([], []).
pairs_from_groups([Key-[Value | _] | Rest], [Key-Value | OtherPairs]) :-
    pairs_from_groups(Rest, OtherPairs).

% Undecorated message
send(Name, Message) :-
    catch(
        (
            log(debug, actor_model, 'Sending ~p to ~w', [Message, Name]),
            thread_send_message(Name, Message)
        ), 
        Exception, 
        (log(warn, actor_model, "Failed to send message ~p to ~w: ~p~n", [Message, Name, Exception]), fail)
        ).

% Send undecorated message periodically
send_at_interval(Target, Tag, Message, Delay, Timer) :-
    self(Name),
    atomic_list_concat([Name, Tag], "_", Timer),
    timer:start(Timer, thread_send_message(Target, Message), Delay).

send_message(Message) :-
    thread_self(Name),
    send_message(Name, Message).

% Semantic message
send_message(Name, Message) :-
    thread_self(From),
    send(Name, message(Message, From)).

send_query(Question, Answer) :-
    thread_self(Name),
    send_query(Name, Question, Answer).

send_query(Name, Question, Answer) :-
    send_query(Name, Question, 5, Answer).

send_query(Name, Question, Timeout, Answer) :- 
    log(debug, actor_model, 'Sending query ~p to ~w', [Question, Name]),
    catch(
            (
                thread_self(From),
                thread_send_message(Name, query(Question, From)), 
                % Fails (quietly) is a matching message is not received in time
                log(debug, actor_model, '~w is waiting for response(Answer, ~p, ~w), timeout is ~w', [From, Question, Name, Timeout]),
                thread_get_message(From, response(Answer, Question, Name), [timeout(Timeout)]),
                log(debug, actor_model, 'Got answer ~p from ~w to query ~p', [Answer, Name, Question])
            ),
            Exception, 
            (log(warn, actor_model, "Failed to query ~w about ~p: ~p~n", [Name, Question, Exception]), fail)
        ).

% Control message
send_control(Control) :-
    thread_self(Name),
    send_control(Name, Control).

send_control(Name, Control) :-
    send(Name, control(Control)).

% Send at interval

wait_for_actor(Name) :-
    wait_for_actor(Name, 20).

wait_for_actor(Name, 0) :-
    log(warn, actor_model, "Failed waiting for actor thread ~w~n", [Name]),!,
    fail.

wait_for_actor(Name, CountDown) :-
    is_thread(Name) -> 
        log(debug, actor_model, 'Actor ~w is running', [Name]),
        true
    ; 
        (log(debug, actor_model, 'Waiting for actor thread ~w (~w)', [Name, CountDown]),
         sleep(0.25), 
         AttemptsLeft is CountDown - 1,
         wait_for_actor(Name, AttemptsLeft)).

wait_for_actor_stopped(Name) :-
    wait_for_actor_stopped(Name, 60).

wait_for_actor_stopped(Name, 0) :-
    log(warn, actor_model, 'Failed waiting for actor thread stopped ~w', [Name]),!,
    fail.

wait_for_actor_stopped(Name, CountDown) :-
    \+ is_thread(Name) -> 
        true
    ; 
        (log(debug, actor_model, 'Waiting for actor thread stopped ~w (~w)~n', [Name, CountDown]),
            sleep(0.25), 
            AttemptsLeft is CountDown - 1,
            wait_for_actor_stopped(Name, AttemptsLeft)).