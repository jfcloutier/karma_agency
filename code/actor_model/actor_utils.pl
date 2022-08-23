:- module(actor_utils, [start_actor/2, start_actor/3, send_message/2, wait_for_actor/1, wait_for_actor_stopped/1]).

start_actor(Name, Goal) :-
    start_actor(Name, Goal, []).

start_actor(Name, Goal, Options) :-
    FullOptions = [alias(Name) | Options],
    thread_create(Goal, _, FullOptions).

send_message(Name, Message) :-
    catch(thread_send_message(Name, Message), Exception, format("Failed to send message ~p to ~w: ~p~n", [Message, Name, Exception])).

wait_for_actor(Name) :-
    wait_for_actor(Name, 5).

wait_for_actor(Name, 0) :-
    format("Failed waiting for actor thread ~w~n", [Name]),!,
    fail.

wait_for_actor(Name, CountDown) :-
    is_thread(Name) -> 
        true
    ; 
        (format("Waiting for actor thread ~w (~w)~n", [Name, CountDown]),
         sleep(1), 
         AttemptsLeft is CountDown - 1,
         wait_for_actor(Name, AttemptsLeft)).

wait_for_actor_stopped(Name) :-
    wait_for_actor_stopped(Name, 5).

wait_for_actor_stopped(Name, 0) :-
    format("Failed waiting for actor thread stopped ~w~n", [Name]),!,
    fail.

wait_for_actor_stopped(Name, CountDown) :-
    \+ is_thread(Name) -> 
        true
    ; 
        (format("Waiting for actor thread stopped ~w (~w)~n", [Name, CountDown]),
            sleep(1), 
            AttemptsLeft is CountDown - 1,
            wait_for_actor_stopped(Name, AttemptsLeft)).