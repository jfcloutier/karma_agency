/*
A phase during a time frame of a dynamic cognition actor.
A phase is executed in its own thread.

A dynamic CA (dCA) interacts with other CAs one timeframe after another.
In each timeframe the dCA goes through a sequence of phases.
During a phase, the dCA continues to receive and process events/queries/messages, updating its main state.
A phase branches off the dCA's state and updates it while it sends events/messages/queries to CAs.
A phase ends when either its work is done or its alloted time (if limited) is expired.
At the end of a phase, the dCA is told of the ended phase, its branched off state is merged back into the dCA's main state for the next phase to work from.
At the end of the sequence of phases, the dCA is told of it, ends the current timeframe, and starts a new one (unless it commits apoptosis).

The sequence of phases is

* begin         - the timeframe begins - update wellbeing from diffusion
* predict       - make predictions from current observations 
* observe       - merge predictions and prediction errors into new observations, elevate umwelt observations, remove obsolete observations
* experience    - integrate updated history of observations into new experiences, remove obsolete experiences, (re)assign value
* plan          - formulate and prioritize goals (formulated and received), select a goal, construct a plan and emit it
* act           - confirm plan feasibility, execute it, remember the goal and plan for later assessment
* assess        - evaluate causal theory and request new one if unsatisfactory, grant past plans affordance status if their goals were achieved
* conclude      - the timeframe concludes - update wellbeing measures and emit wellbeing changes
*/

:- module(phase, [current_phase/2, next_phase/2, phase_transition/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(timer)).
:- use_module(agency(som/phases/begin)).
:- use_module(agency(som/phases/predict)).
:- use_module(agency(som/phases/observe)).
:- use_module(agency(som/phases/experience)).
:- use_module(agency(som/phases/plan)).
:- use_module(agency(som/phases/act)).
:- use_module(agency(som/phases/assess)).
:- use_module(agency(som/phases/conclude)).


% phase_status(Phase, Status) - Status is running or stopped
:- thread_local phase_status/2.
% phase_time_limit(Phase, Time) - Time limit before a phase is stopped
:- thread_local phase_time_limit/2.

% Phase transitions
% Timeframe started (goes right to begin)
next_phase(initiating, begin).
% Phase transitions within a timeframe where each phase does units of work until done or time is up
next_phase(begin, predict).
next_phase(predict, observe).
next_phase(observe, experience).
next_phase(experience, plan).
next_phase(plan, act).
next_phase(act, assess).
next_phase(assess, conclude).

% Timeboxing of phases as fraction of the latency of a timeframe
phase_timebox(observe, 0.2).
phase_timebox(experience, 0.2).
phase_timebox(plan, 0.5).
phase_timebox(_, 0.1).

%%%% IN dCA THREAD

% A thread is started with the execution of the phase as goal
% So that the dCA's main thread can continue to receive messages etc. while the phase is being executed
phase_transition(State, NewState) :- 
	self(CA),
    log(info, phase, "Phase transition of CA ~w in state ~p", [CA, State]),
    current_phase(State, EndedPhase),
	next_phase(EndedPhase, Phase),
    log(info, phase, "Phase transition of CA ~w from ~w to ~w", [CA, EndedPhase, Phase]),
    update_timeframe(State, [phase - Phase], NewState),
    % Start the phase thread. It then waits for go.
	thread_create(phase:started(Phase, CA, NewState), _, [detached(true)]).

current_phase(State, Phase) :-
    get_state(State, timeframe, Timeframe),
    Phase = Timeframe.phase.

% Update the timeframe of a CA state
update_timeframe(State, Pairs, NewState) :-
    get_state(State, timeframe, Timeframe),
    put_state(Timeframe, Pairs, NewTimeframe),
    put_state(State, timeframe, NewTimeframe, NewState).

%%%% IN PHASE THREAD

% Run in the phase's thread until done or status is stopped.
% The last action is to send Name a message with the updated state
started(Phase, CA, State) :-
    get_state(State, latency, Latency),
    timebox_phase_thread(Phase, Latency),
    Goal =.. [:, Phase, unit_of_work(CA, State, WorkStatus)],
    log(info, phase, "Starting phase ~w (~@) for CA ~w with goal ~p", [Phase, self, CA, Goal]),
    engine_create(WorkStatus, Goal, WorkEngine),
    remember_one(phase_status(Phase, running)),
    executing_phase(Phase, WorkEngine, CA, State).

% Timebox a phase but only if it is meant to be
timebox_phase_thread(Phase, Latency) :-
    phase_timeout(Phase, Latency, Delay),
    get_time(Now),
    Deadline is Now + Delay,
    log(info, phase, "Setting end of phase ~w of ~@ to ~w seconds from now", [Phase, self, Delay]),
    remember_one(phase_time_limit(Phase, Deadline)).

executing_phase(Phase, WorkEngine, CA, PhaseState) :-
    phase_ended(Phase),
    !,
    log(info, phase, "Phase ~w for CA ~p was stopped", [Phase, CA]),
    engine_destroy(WorkEngine),
    message_sent(CA, end_of_phase(Phase, PhaseState)).

executing_phase(Phase, WorkEngine, CA, _) :-
    log(info, phase, "Doing unit of work in phase ~w for CA ~p", [Phase, CA]),
    work_engine_cranked(WorkEngine, WorkStatus),
    log(info, phase, "Unit of work in phase ~w for CA ~w done with ~p", [Phase, CA, WorkStatus]),
    work_status_handled(Phase, CA, WorkEngine, WorkStatus).

work_status_handled(Phase, CA, WorkEngine, done(EndState)) :-
    remember_one(phase_status(Phase, stopped)),
    executing_phase(Phase, WorkEngine, CA, EndState).

work_status_handled(Phase, CA, WorkEngine, more(IntermediateState)) :-
    executing_phase(Phase, WorkEngine, CA, IntermediateState).

work_status_handled(Phase, CA, _, WorkStatus) :-
    log(info, phase, "@@@ Not handling work status for phase ~w of CA ~w: ~p", [Phase, CA, WorkStatus]),
    fail.

work_engine_cranked(WorkEngine, WorkStatus) :-
    engine_next_reified(WorkEngine, Next),
    interpret_unit_of_work(Next, WorkStatus).

interpret_unit_of_work(the(WorkStatus), WorkStatus).
interpret_unit_of_work(no, _) :-
    log(warn, phase, "No work status from engine"),
    throw("No engine answer").
interpret_unit_of_work(exception(Error), _) :-
    log(warn, phase, "Exception from engine. Got ~p", [Error]),
    throw(Error).
    
phase_ended(Phase) :-
    phase_status(Phase, stopped), !.

phase_ended(Phase) :-
    get_time(Now),
    phase_time_limit(Phase, TimeLimit),
    Now > TimeLimit.

remember_one(Term) :-
    Term =.. [Head | Args],
    length(Args, Arity),
    length(Any, Arity),
    Retracted =.. [Head | Any],
    % Because using abolish/1 erases the thread_local property
    retractall(Retracted),
    assertz(Term).

phase_timeout(Phase, Latency, Delay) :-
    phase_timebox(Phase, Fraction),
    Delay is Latency * Fraction.
