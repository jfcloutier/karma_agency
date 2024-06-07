/*
Timer creates threads to periodically call goals until stopped.
*/

:- module(timer, []).

:- use_module(code(logger)).

start(Timer, Goal, Delay) :-
    log(info, timer, 'Timer ~w will be executing ~p every ~w seconds', [Timer, Goal, Delay]),
    thread_create(run(Delay, Goal), _, [alias(Timer), detached(true)]).

stop(Timer) :-
    log(info, timer, 'Stopping timer ~w', [Timer]),
    thread_signal(Timer, exit_timer).

% Runs in a thread

run(Delay, Goal) :-
    sleep(Delay),
    catch(
        (
            log(debug, timer, 'Timer ~@ is executing ~p after waiting ~w seconds', [self, Goal, Delay]),
            call(Goal)
        ), 
        Exception, 
        (log(warn, timer, 'Failed to execute ~p: ~p', [Goal, Exception]), true)
        ),
    run(Delay, Goal).

exit_timer :-
    thread_self(Timer),
    log(info, timer, 'Exiting timer ~w', [Timer]),
    thread_exit(true).
    