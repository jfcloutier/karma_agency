:- module(thread_utils, [start_thread/2, start_thread/3, send_message/2, wait_for_thread/1, wait_for_thread_stopped/1]).

start_thread(Name, Goal) :-
    start_thread(Name, Goal, []).

start_thread(Name, Goal, Options) :-
    FullOptions = [alias(Name) | Options],
    thread_create(Goal, _, FullOptions).

send_message(Name, Message) :-
    catch(thread_send_message(Name, Message), Exception, format("Failed to send message ~p to ~w: ~p~n", [Message, Name, Exception])).

wait_for_thread(Name) :-
    wait_for_thread(Name, 5).

wait_for_thread(Name, 0) :-
    format("Failed waiting for thread ~w~n", [Name]),!,
    fail.

wait_for_thread(Name, CountDown) :-
    is_thread(Name) -> 
        true
    ; 
        (format("Waiting for thread ~w (~w)~n", [Name, CountDown]),
         sleep(1), 
         AttemptsLeft is CountDown - 1,
         wait_for_thread(Name, AttemptsLeft)).

wait_for_thread_stopped(Name) :-
    wait_for_thread_stopped(Name, 5).

wait_for_thread_stopped(Name, 0) :-
    format("Failed waiting for thread stopped ~w~n", [Name]),!,
    fail.

wait_for_thread_stopped(Name, CountDown) :-
    is_thread(Name) -> 
        true
    ; 
        (format("Waiting for thread stopped ~w (~w)~n", [Name, CountDown]),
            sleep(1), 
            AttemptsLeft is CountDown - 1,
            wait_for_thread_stopped(Name, AttemptsLeft)).