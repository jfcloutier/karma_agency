:- module(act, [executing_for/1]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).

:- thread_local phase_ending().

phase_timeout() :-
    remember_one(phase_ending()).

executing_for(CA, State) :-
    execution(State, NewState),
    message_sent(CA, end_of_phase(NewState)).

% DO SOMETHING THAT ALTERS THE STATE OF THE CA AND OF OF OTHERS
% Use engine to generate policies until time is up
execution(State, State).