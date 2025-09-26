/*
A phase during a time frame of a dynamic cognition actor.
A phase is executed in its own thread.
*/

:- module(phase, [next_phase/2, phase_transition/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/phase/observe)).
:- use_module(agency(som/phase/believe)).
:- use_module(agency(som/phase/act)).
:- use_module(agency(som/phase/assess)).
:- use_module(agency(som/phase/age)).

:- thread_local phase/1, timer/1.

% Phase transitions
next_phase(initiating, observe).
next_phase(observe, believe).
next_phase(believe, act).
next_phase(act, assess).
next_phase(assess, age).
next_phase(age, observe).

phase_timebox(observe, 0.3).
phase_timebox(believe, 0.3).
phase_timebox(act, 0.3).

%%%% In CA thread

% A thread is started with the execution of the phase as goal
% So that the CA's main thread can conitnue to receive messages etc. while the phase is being executed
phase_transition(State, NewState) :- 
	self(Name),
    get_state(State, phase, EndingPhase),
	next_phase(EndedPhase, Phase),
	thread_create(phase_executed(Phase, Name, State), ThreadId, [detached(true)]),
    timebox_phase(Phase, State, PhaseThreadId, Timer),
    remember_one(timer(Timer)),
    put_state(State, [phase - Phase, phase_thread - PhaseThreadId], NewState).

timebox_phase(Phase, State, PhaseThreadId, Timer) :-
    get_state(State, latency, Latency),
    (phase_timeout(Phase, State, Delay) ->
        call_later(PhaseThreadId, 
            phase_ender, 
            message_sent(PhaseThreadId, end_phase(Phase)),
            Delay,
            Timer
        ) 
        ; 
        true).

phase_timeout(Phase, State, Delay) :-
    phase_timebox(Phase, Fraction),
    get_state(State, latency, Latency),
    Delay is Latency * Fraction.

%%%% IN THREAD

% Run in the phase's thread until done or interrupted by time's out.
% The last action is to send Name a message with the updated state
phase_executed(Phase, CA, State) :-
    remember_one(phase(Phase)),
    Phase:executing_for(CA, State).

% Signalled end_phase goal from timer
end_phase(Phase) :-
    phase(Phase) ->
    Phase:phase_timeout()
    ;
    true.
