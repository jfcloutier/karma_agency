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

:- module(phase, [next_phase/2, phase_consumes_produces/3, phase_transition/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(timer)).
:- use_module(agency(som/phases/begin)).
:- use_module(agency(som/phases/predict)).
:- use_module(agency(som/phases/observe)).
:- use_module(agency(som/phases/experience)).
:- use_module(agency(som/phases/plan)).
:- use_module(agency(som/phases/act)).
:- use_module(agency(som/phases/assess)).
:- use_module(agency(som/phases/conclude)).

% phase_time_limit(Time) - Time limit before a phase is stopped
:- thread_local phase_time_limit/1.

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

% The state properties consumes (empties) and produces (replaces)
phase_consumes_produces(initiating, [], []).
phase_consumes_produces(begin, [], []).
phase_consumes_produces(predict, [], [predictions_out]).
phase_consumes_produces(observe, [predictions_out, prediction_errors], [observations]).
phase_consumes_produces(experience, [], [experiences]).
phase_consumes_produces(plan, [], []).
phase_consumes_produces(act, [], []).
phase_consumes_produces(assess, [], [alive]).
phase_consumes_produces(conclude, [], []).


% Timeboxing of phases as fraction of the latency of a timeframe
phase_timebox(predict, 0.3).
phase_timebox(observe, 0.1).
phase_timebox(experience, 0.2).
phase_timebox(plan, 0.4).
phase_timebox(_, 0).

%%%% IN dCA THREAD

% A thread is started with the execution of the phase as goal
% So that the dCA's main thread can continue to receive messages etc. while the phase is being executed
phase_transition(State, NewState) :- 
	self(CA),
    transition_states(CA, State, PhaseState, NewState),
    log(info, phase, "Phase transition of CA ~w with state ~p", [CA, NewState]),
    % Start the phase thread with the transitioned-to state
    thread_create(phase:started(CA, PhaseState), _, [detached(true)]).

% The phase state does not consume properties, the new state does. 
transition_states(CA, State, PhaseState, NewCAState) :-
    EndedPhase = State.phase,
    next_phase(EndedPhase, Phase),
    put_state(State, phase, Phase, PhaseState),
    log(info, phase, "Phase transition of CA ~w from ~w to ~w", [CA, EndedPhase, Phase]),
    phase_consumes_produces(Phase, Consumes, _),
    consume_state_properties(PhaseState, Consumes, NewCAState).

% Note - This assumes that a consumed property is being reset to an empty list
consume_state_properties(State, [], State).
consume_state_properties(State, [Property | Rest], NewState) :-
    put_state(State, Property, [], State1),
    consume_state_properties(State1, Rest, NewState).

%%%% IN PHASE THREAD

% Run in the phase's thread until done or status is stopped.
% A phase is run in its own thread so that the CA thread can receive messages while the phase is executing.
% The last action is to send the CA a message with the updated state.
started(CA, State) :-
    get_state(State, latency, Latency),
    timebox_phase(Phase, Latency),
    % unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
    % resolving WorkStatus to more(IntermediateState, Changed), or it can be done(EndState, Changed) as the last or only solution.
    % where Changed are the names of the state properties
    Goal =.. [:, Phase, unit_of_work(CA, State, WorkStatus)],
    Phase = State.phase,
    engine_create(WorkStatus, Goal, WorkEngine),
    run_phase(CA, Phase, WorkEngine).

% Timebox a phase but only if it is meant to be
timebox_phase(Phase, Latency) :-
    phase_max_time(Phase, Latency, Delay),
    get_time(Now),
    Deadline is Now + Delay,
    log(info, phase, "Setting end of phase ~w of ~@ to ~w seconds from now", [Phase, self, Delay]),
    remember_one(phase_time_limit(Deadline)).
    
run_phase(CA, Phase, WorkEngine) :-
    log(info, phase, "Doing unit of work in phase ~w for CA ~p", [Phase, CA]),
    work_engine_cranked(WorkEngine, WorkStatus),
    log(info, phase, "Unit of work in phase ~w for CA ~w done with ~p", [Phase, CA, WorkStatus]),
    work_status_handled(WorkStatus, WorkEngine, CA).

work_status_handled(done(EndState, WellbeingDeltas), WorkEngine, CA) :-
    phase_done(CA, WorkEngine, EndState, WellbeingDeltas).

work_status_handled(more(IntermediateState, WellbeingDeltas), WorkEngine, CA) :-
    phase_timeout() -> 
        phase_done(CA, WorkEngine, IntermediateState, WellbeingDeltas)
        ;
        Phase = IntermediateState.phase,
        run_phase(CA, Phase, WorkEngine).

work_status_handled(WorkStatus, _, _) :-
    log(info, phase, "@@@ Not handling work status of ~@: ~p", [self, WorkStatus]),
    fail.

run_the_clock(Phase) :-
    get_time(Now),
    phase_time_limit(TimeLimit),
    Now < TimeLimit,
    Sleep is TimeLimit - Now,
    log(info, phase, "Running the clock for phase ~w for ~w seconds", [Phase, Sleep]),
    sleep(Sleep).

run_the_clock(_).

phase_done(CA, WorkEngine, PhaseEndState, WellbeingDeltas) :-
    Phase = PhaseEndState.phase,
    log(info, phase, "Phase ~w for CA ~p done with end state ~p and wellbeing deltas ~p", [Phase, CA, PhaseEndState, WellbeingDeltas]),
    engine_destroy(WorkEngine),
    run_the_clock(Phase),
    % The engine sends the CA that started it a message that the phase is done
    message_sent(CA, phase_done(PhaseEndState, WellbeingDeltas)),
    % This event is currently only used in tests
    published(end_of_phase, [phase=Phase, state=PhaseEndState], CA).

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

phase_timeout() :-
    get_time(Now),
    phase_time_limit(TimeLimit),
    Now > TimeLimit.

% TODO - put in prolog_utils
remember_one(Term) :-
    Term =.. [Head | Args],
    length(Args, Arity),
    length(Any, Arity),
    Retracted =.. [Head | Any],
    % Because using abolish/1 erases the thread_local property
    retractall(Retracted),
    assertz(Term).

phase_max_time(Phase, Latency, Delay) :-
    phase_timebox(Phase, Fraction),
    Delay is Latency * Fraction.
