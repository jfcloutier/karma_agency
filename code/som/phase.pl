/*
A phase is a step within a timeframe of a dynamic cognition actor.
A phase is executed in its own thread. The phase thread communicates progress and completions with the CA's thread via messages.

A dynamic CA (dCA) interacts with other CAs one timeframe after another.
In each timeframe the dCA goes through a sequence of phases.
During a phase, the dCA continues to receive and process events/queries/messages (from other CAs and from its phases), updating its main state.
A phase branches off the dCA's state. Elements consumed by the phase are emptied form the dCA's state.
A phase's unit of work, when done, updates elements of the dCA state. During work, a phase may send events/messages/queries to CAs, including the one it transitions.
A phase ends when either its work is done or its alloted time (if limited) is expired.
At the end of a phase, the dCA is told of the ended phase, its final production (select state properties) is merged into the dCA's state for the next phase to work from.
At the end of the sequence of phases, the dCA is told of it, ends the current timeframe, and starts a new one (unless it commits apoptosis).

The sequence of phases is

* begin         - persist recently received predictions
* predict       - send predictions from current observations 
* observe       - merge predictions and prediction errors into new observations
* experience    - integrate current and past observations into terminated, updated and new experiences
* feel          - assign a normative value (from worst to best feeling) to each experience
* act           - determine intent, make plans for the intent and for directives received, and execute plans, always taking urgency into account
* assess        - evaluate the current causal theory and request a new one if unsatisfactory, abandon the intent if stale, score plans when goals are achieved, diffuse wellbeing, decide to live or die
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
:- use_module(agency(som/phases/act)).
:- use_module(agency(som/phases/assess)).

% phase_time_limit(Time) - Time limit before a phase is stopped
:- thread_local phase_time_limit/1.

% Phase transitions
% Timeframe starts at initiating
next_phase(initiating, begin).
% Phase transitions within a timeframe where each phase does units of work until done or time is up
next_phase(begin, predict).
next_phase(predict, observe).
next_phase(observe, experience).
next_phase(experience, feel).
next_phase(feel, act).
next_phase(act, assess).
% No next phase after assess - a new timeframe is started if the CA is still alive

/*
The list-valued state properties consumed and produced by each phase:
  * If a property is consumed then it is available to the phase but emptied from the CA state on entering the phase
  * If a property is produced by a phase then it is accumulated into CA state from work by the phase
  * A property that is both consumed and produced by a phase has the phase replace it with new content

The single-valued state properties are never consumed but can be updated by a phase if declared as produced.
*/

% Do nothing - this is only a jumping point to the `begin` phase
phase_consumes_produces(initiating, [], []).
% Begin the timeframe by persisting recently received predictions.
phase_consumes_produces(begin, [predictions_in], [predictions_in]).
% Predictions are sent out, replacing prior ones
phase_consumes_produces(predict, [predictions_out], [predictions_out]).
% New observations are produced from consuming predictions sent and prediction errors received, and they replace old observations.
% Predictions received persist for a few lifecycles.
phase_consumes_produces(observe, [prediction_errors, observations], [observations]).
% Prior experiences and updated and new experiences produced from the latest observations.
phase_consumes_produces(experience, [experiences], [experiences]).
% An overall feeling is computed and experiences are replaced by felt experiences
phase_consumes_produces(feel, [experiences], [feeling, experiences]).
% Available umwelt actions are updated, intent maybe created, plans are built
phase_consumes_produces(act, [umwelt_actions], [umwelt_actions, plans, intent]).
% Alive status is updated, plans and intent may be abandoned, executed plans become affordances, affordances are scored and possibly dropped
phase_consumes_produces(assess, [plans, affordances], [alive, intent, plans, affordances]).
% Before cycling back to `initiating`, the memorable part of the state (experiences, feeling, affordances) is saved as the latest past timeframe.
% Otherwise, the returned-to `initiating` phase is engaged with the state at the end of the just-completed `assess` phase

% Timeboxing of phases as fraction of the latency of a timeframe

% Allow time for umwelt CAs to respond with prediction errors
phase_timebox(predict, 0.3).
% Limit the time allocated to producing an open-ended stream of experiences
phase_timebox(experience, 0.1).
% Allow time for umwelt CAs to act
phase_timebox(act, 0.6).
phase_timebox(_, 0).

%%%% IN dCA THREAD

% A thread is started with the execution of the phase as goal
% So that the dCA's main thread can continue to receive messages etc. while the phase is being executed
phase_transition(State, NewState) :- 
	self(CA),
    transition_states(CA, State, PhaseState, NewState),
    % Start the phase thread with the transitioned-to state. It is detached so that the system automatically reclaims resources on termination.
    thread_create(phase:started(CA, PhaseState), _, [detached(true)]).

% On a phase transition, the CA loses consumed properties while the phase retains them and can work from them to produce fresh updates. 
transition_states(CA, State, PhaseState, NewCAState) :-
    EndedPhase = State.phase,
    next_phase(EndedPhase, Phase),
    put_state(State, phase, Phase, PhaseState),
    get_state(State, timeframe_count, TimeframeCount),
    phase_consumes_produces(Phase, Consumes, _),
    % The new CA state has the current phase set and the consumed properties emptied
    consume_state_properties(PhaseState, Consumes, NewCAState),
    log(info, phase, "Phase transition of CA ~w from ~w to ~w with intial ~p (~w)", [CA, EndedPhase, Phase, PhaseState, TimeframeCount]).

% Note - This assumes that a consumed property is being reset to an empty list
consume_state_properties(State, [], State).
consume_state_properties(State, [Property | Rest], NewState) :-
    put_state(State, Property, [], State1),
    consume_state_properties(State1, Rest, NewState).

%%%% IN PHASE THREAD

% Run in the phase's thread until the phase is done, either because it ran out of work or it timed out.
% A phase is run in its own thread so that the CA thread can receive messages for other CAs (and from the phase thread) while the phase is executing.
% The last action is to send the CA a phase_done message with the updated state and wellbeing.
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
    remember_one(phase_time_limit(Deadline)),
    log(info, phase, "Setting end of phase ~w with latency ~w to ~w seconds from now ~w to deadline ~w", [Phase, Latency, Delay, Now, Deadline]).

% Maybe do something before engaging into units of work
% unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
% resolving WorkStatus to more(IntermediatePhaseState, WellbeingDelta), or it can be done(EndPhaseState, WellbeingDelta) as the last or only solution.
work(CA, State, Phase) :-
    Phase:before_work(CA, State, StateDeltas, WellbeingDelta),
    % Report the preliminary work that was done before engaging the work engine
    work_reported(CA, Phase, before, StateDeltas, WellbeingDelta),
    % Use the result of the before-work to update the state passed to the phase
    phase_consumes_produces(Phase, _, ProducedProperties),
    log(info, phase, "(~@) Phase ~w producing ~p BEFORE WORK with state deltas ~p", [self, Phase, ProducedProperties, StateDeltas]),
    merge_phase_deltas(StateDeltas, WellbeingDelta, ProducedProperties, State, InitialPhaseState),
    Goal =.. [:, Phase, unit_of_work(CA, InitialPhaseState, WorkStatus)],
    log(debug, phase, "Creating engine for phase ~p on ~p", [Phase, Goal]),
    engine_create(WorkStatus, Goal, WorkEngine),
    get_state(State, timeframe_count, TimeframeCount),
    % Run the phase, one work unit or the whole phase until done, producing state and wellbeing deltas
    run_phase(CA, Phase, TimeframeCount, WorkEngine).
    
run_phase(CA, Phase, TimeframeCount, WorkEngine) :-
    log(info, phase, "(~w) Running phase ~w (~w)", [CA, Phase, TimeframeCount]),
    work_engine_cranked(WorkEngine, WorkStatus),
    log(info, phase, "(~w) Unit of work in phase ~w completed with status ~p (~w)", [CA, Phase, WorkStatus, TimeframeCount]),
    work_status_handled(WorkStatus, WorkEngine, CA, Phase, TimeframeCount).

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

work_status_handled(done(StateDeltas, WellbeingDelta), WorkEngine, CA, Phase, TimeframeCount) :-
    phase_done(CA, Phase, TimeframeCount, StateDeltas, WellbeingDelta, WorkEngine).

work_status_handled(more(StateDeltas, WellbeingDelta), WorkEngine, CA, Phase, TimeframeCount) :-
    (phase_timeout(Phase) -> 
        phase_done(CA, Phase, TimeframeCount, StateDeltas, WellbeingDelta, WorkEngine)
        ;
        work_reported(CA, Phase, more, StateDeltas, WellbeingDelta),
        run_phase(CA, Phase, TimeframeCount, WorkEngine)).

work_status_handled(WorkStatus, _, _, _, _) :-
    log(info, phase, "@@@ Not handling work status of ~@: ~p", [self, WorkStatus]),
    fail.

% Last unit of work is done. Perhaps do some after-work, run the clock, and let the dynamic CA (and others) know.
phase_done(CA, Phase, TimeframeCount, StateDeltas, WellbeingDelta, WorkEngine) :-
    engine_destroy(WorkEngine),  
    log(info, phase, "Phase ~w for CA ~p done with deltas ~p and ~p (~w)", [Phase, CA, StateDeltas, WellbeingDelta, TimeframeCount]),
    % Run out the clock if work ended before time expired - to allow receipt of messages caused by work done, before tranistioning to the next phase
    run_the_clock(Phase),
    % The engine sends the CA that started it a message that the phase is done
    message_sent(CA, phase_done(Phase, StateDeltas, WellbeingDelta)),
    % This event is currently only used in tests
    published(end_of_phase, [phase=Phase, state_deltas=StateDeltas, wellbeing_delta=WellbeingDelta], CA).

work_reported(CA, Phase, before, StateDeltas, WellbeingDelta) :-
    message_sent(CA, phase_progressed(Phase, StateDeltas, WellbeingDelta)),
    % This event is currently only used in tests
    published(phase_before_work, [phase=Phase, state_deltas=StateDeltas, wellbeing_delta=WellbeingDelta], CA).

work_reported(CA, Phase, more, StateDeltas, WellbeingDelta) :-
    message_sent(CA, phase_progressed(Phase, StateDeltas, WellbeingDelta)),
    % This event is currently only used in tests
    published(phase_progressed, [phase=Phase, state_deltas=StateDeltas, wellbeing_delta=WellbeingDelta], CA).

run_the_clock(Phase) :-
    get_time(Now),
    phase_time_limit(TimeLimit),
    Now < TimeLimit,
    Sleep is TimeLimit - Now,
    log(info, phase, "Running the clock for phase ~w for ~w seconds", [Phase, Sleep]),
    sleep(Sleep).

run_the_clock(_).

phase_timeout(Phase) :-
    get_time(Now),
    phase_time_limit(TimeLimit),
    Now > TimeLimit,
    log(info, phase, "Phase ~w TIMED OUT (now ~w exceeded time limit ~w)", [Phase, Now, TimeLimit]).

phase_max_time(Phase, Latency, Delay) :-
    phase_timebox(Phase, Fraction),
    Delay is Latency * Fraction.

remember_one(Term) :-
    Term =.. [Head | Args],
    length(Args, Arity),
    length(Any, Arity),
    Retracted =.. [Head | Any],
    % Because using abolish/1 erases the thread_local property
    retractall(Retracted),
    assertz(Term).
