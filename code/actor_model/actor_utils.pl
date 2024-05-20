:- module(actor_utils, [self/0, self/1, start_actor/2, start_actor/3, send/2, send_control/1, send_control/2, send_message/1, send_message/2, send_query/2, send_query/3, send_query/4, wait_for_actor/1, wait_for_actor_stopped/1, wait_for_actor_stopped/2, empty_state/1, get_state/3, put_state/4]).

:- use_module(code(logger)).

self :-
    self(Name),
    print(Name).
    
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

put_state(State, Key, Value, NewState) :-
    is_dict(State, state),
    put_dict(Key, State, Value, NewState).

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

send_message(Message) :-
    thread_self(Name),
    send_message(Name, Message).

% Semantic message
send_message(Name, Message) :-
    thread_self(From),
    log(debug, actor_model, 'Sending message(~p, ~w) to ~w', [Message, From, Name]),
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