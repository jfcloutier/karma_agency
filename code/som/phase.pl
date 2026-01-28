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

* predict       - make predictions from current observations 
* observe       - merge predictions and prediction errors into new observations
* experience    - integrate current and past observations into terminated, updated and new experiences
* feel          - assign a normative value (from worst to best feeling) to each experience
* plan          - formulate and prioritize goals (formulated and received), select a goal, construct a plan and emit it
* act           - confirm plan feasibility, execute it, remember the goal and plan for later assessment
* assess        - evaluate causal theory and request new one if unsatisfactory, grant past plans affordance status if their goals were achieved
* bind          - update and diffuse wellbeing (create cognitive glue)
*/

:- module(phase, [next_phase/2, phase_consumes_produces/3, phase_transition/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(timer)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/phases/predict)).
:- use_module(agency(som/phases/observe)).
:- use_module(agency(som/phases/experience)).
:- use_module(agency(som/phases/feel)).
:- use_module(agency(som/phases/plan)).
:- use_module(agency(som/phases/act)).
:- use_module(agency(som/phases/assess)).
:- use_module(agency(som/phases/bind)).

% phase_time_limit(Time) - Time limit before a phase is stopped
:- thread_local phase_time_limit/1.

% Phase transitions
% Timeframe started (goes right to begin)
next_phase(initiating, predict).
% Phase transitions within a timeframe where each phase does units of work until done or time is up
next_phase(predict, observe).
next_phase(observe, experience).
next_phase(experience, feel).
next_phase(feel, plan).
next_phase(plan, act).
next_phase(act, assess).
next_phase(assess, bind).

% The state properties consumes (empties) and produces (replaces)
phase_consumes_produces(initiating, [], []).
phase_consumes_produces(predict, [], [predictions_out]).
phase_consumes_produces(observe, [predictions_out, prediction_errors], [observations]).
phase_consumes_produces(experience, [], [experiences]).
phase_consumes_produces(feel, [experiences], [feeling, experiences]).
phase_consumes_produces(plan, [], []). % TODO
phase_consumes_produces(act, [], []).  % TODO
phase_consumes_produces(assess, [], [alive]).
phase_consumes_produces(bind, [], []).


% Timeboxing of phases as fraction of the latency of a timeframe

% Allow time for umwelt CAs to respond with prediction errors
phase_timebox(predict, 0.3).
% Limit the time allocated to producing an open-ended stream of experiences
phase_timebox(experience, 0.1).
% Allow time for umwelt CAs to respond to the plan's directives
phase_timebox(plan, 0.6).
phase_timebox(_, 0).

%%%% IN dCA THREAD

% A thread is started with the execution of the phase as goal
% So that the dCA's main thread can continue to receive messages etc. while the phase is being executed
phase_transition(State, NewState) :- 
	self(CA),
    transition_states(CA, State, PhaseState, NewState),
    % Start the phase thread with the transitioned-to state
    thread_create(phase:started(CA, PhaseState), _, [detached(true)]).

% The phase state does not consume properties, the new state does. 
transition_states(CA, State, PhaseState, NewCAState) :-
    EndedPhase = State.phase,
    next_phase(EndedPhase, Phase),
    put_state(State, phase, Phase, PhaseState),
    get_state(State, timeframe_count, TimeframeCount),
    log(info, phase, "Phase transition of CA ~w from ~w to ~w (~w)", [CA, EndedPhase, Phase, TimeframeCount]),
    phase_consumes_produces(Phase, Consumes, _),
    consume_state_properties(PhaseState, Consumes, NewCAState).

% Note - This assumes that a consumed property is being reset to an empty list
consume_state_properties(State, [], State).
consume_state_properties(State, [Property | Rest], NewState) :-
    put_state(State, Property, [], State1),
    consume_state_properties(State1, Rest, NewState).

%%%% IN PHASE THREAD

% Run in the phase's thread until the phase is done, because it ran out of work or it timed out.
% A phase is run in its own thread so that the CA thread can receive messages while the phase is executing.
% The last action is to send the CA a message with the updated state.
started(CA, State) :-
    get_state(State, latency, Latency),
    Phase = State.phase,
    timebox_phase(Phase, Latency),
    work(CA, State, Phase).

% Timebox a phase but only if it is meant to be
timebox_phase(Phase, Latency) :-
    phase_max_time(Phase, Latency, Delay),
    get_time(Now),
    Deadline is Now + Delay,
    log(info, phase, "Setting end of phase ~w of ~w seconds from now", [Phase, Delay]),
    remember_one(phase_time_limit(Deadline)).

% Maybe do something before engaging into units of work
% unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
% resolving WorkStatus to more(IntermediatePhaseState, WellbeingDeltas), or it can be done(EndPhaseState, WellbeingDeltas) as the last or only solution.
work(CA, State, Phase) :-
    Phase:before_work(CA, State, InitialPhaseState),
    Goal =.. [:, Phase, unit_of_work(CA, InitialPhaseState, WorkStatus)],
    engine_create(WorkStatus, Goal, WorkEngine),
    get_state(State, timeframe_count, TimeframeCount),
    run_phase(CA, Phase, TimeframeCount, WorkEngine, InitialPhaseState).
    
run_phase(CA, Phase, TimeframeCount, WorkEngine, AccState) :-
    log(info, phase, "Doing unit of work in phase ~w of CA ~p (~w)", [Phase, CA, TimeframeCount]),
    work_engine_cranked(WorkEngine, WorkStatus),
    log(info, phase, "Unit of work in phase ~w of CA ~w completed (~w)", [Phase, CA, TimeframeCount]),
    work_status_handled(WorkStatus, WorkEngine, CA, Phase, AccState).

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

work_status_handled(done(StateDeltas, WellbeingDeltas), WorkEngine, CA, Phase, AccState) :-
    updated_state_from_work(AccState, StateDeltas, WellbeingDeltas, UpdatedState),
    phase_done(CA, Phase, UpdatedState, WellbeingDeltas, WorkEngine).

work_status_handled(more(StateDeltas, WellbeingDeltas), WorkEngine, CA, Phase, AccState) :-
    phase_timeout() -> 
        phase_done(CA, Phase, AccState, WellbeingDeltas, WorkEngine)
        ;
        get_state(AccState, timeframe_count, TimeframeCount),
        updated_state_from_work(AccState, StateDeltas, WellbeingDeltas, UpdatedState),
        run_phase(CA, Phase, TimeframeCount, WorkEngine, UpdatedState).

work_status_handled(WorkStatus, _, _, _, _) :-
    log(info, phase, "@@@ Not handling work status of ~@: ~p", [self, WorkStatus]),
    fail.

updated_state_from_work(State, StateDeltas, WellbeingDeltas, UpdatedState) :-
    update_from_state_deltas(State, StateDeltas, State1),
    merge_wellbeing(State1, WellbeingDeltas, UpdatedState).

update_from_state_deltas(State, [], State).

update_from_state_deltas(State, [Delta | Rest], UpdatedState) :-
    Property-Value = Delta,
    (is_list(Value) ->
        acc_state(State, Property, Value, State1)
        ;
        put_state(State, Property, Value, State1)
    ),
    update_from_state_deltas(State1, Rest, UpdatedState).

% Last unit of work is done. Perhaps do some after-work, run the clock, and let the dynamic CA (and others) know.
phase_done(CA, Phase, PhaseDoneState, WellbeingDeltas, WorkEngine) :-
    engine_destroy(WorkEngine),
    Phase:after_work(CA, PhaseDoneState, FinalPhaseState),
    get_state(PhaseDoneState, timeframe_count, TimeframeCount),    
    log(info, phase, "Phase ~w for CA ~p done with wellbeing deltas ~p (~w)", [Phase, CA, WellbeingDeltas, TimeframeCount]),
    run_the_clock(Phase),
    % The engine sends the CA that started it a message that the phase is done
    message_sent(CA, phase_done(FinalPhaseState, WellbeingDeltas)),
    % This event is currently only used in tests
    published(end_of_phase, [phase=Phase, state=FinalPhaseState], CA).

run_the_clock(Phase) :-
    get_time(Now),
    phase_time_limit(TimeLimit),
    Now < TimeLimit,
    Sleep is TimeLimit - Now,
    log(info, phase, "Running the clock for phase ~w for ~w seconds", [Phase, Sleep]),
    sleep(Sleep).

run_the_clock(_).

phase_timeout() :-
    get_time(Now),
    phase_time_limit(TimeLimit),
    Now > TimeLimit.

phase_max_time(Phase, Latency, Delay) :-
    phase_timebox(Phase, Fraction),
    Delay is Latency * Fraction.

% TODO - put in prolog_utils
remember_one(Term) :-
    Term =.. [Head | Args],
    length(Args, Arity),
    length(Any, Arity),
    Retracted =.. [Head | Any],
    % Because using abolish/1 erases the thread_local property
    retractall(Retracted),
    assertz(Term).
