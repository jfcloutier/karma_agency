/*
Abandon intent if 
  * it is no longer relevant (the experience to be impacted is gone)
  * it is stalled (not executing/executed for N timeframes)
  * then drop any plan for it and let umwelt know the intent is abandoned

Abandon a directive if
  * it is no longer relevant
  * then drop any plan for it and let the parents know that the directive cannot be executed

Abandon a plan if
  * it is stalled 

Assess goal achievement
  * for each executed goal, assess whether it was achieved

Score plans
  * over all timeframes, check if goal states went from executed to achieved (have targeted experience impacts been realized?)
  * if achieved, score associated executed plans
    * the closer in time to goal achievement, the higher the (correlation) score

Evaluate causal theory
  * If none and there's enough history (timeframe count > N), request one from the Apperception Engine
* If too many prediction errors from applying the current causal theory, request a new one (hold on to the old ones)

Diffuse wellbeing
  * broadcast wellbeing status (parents and umwelt are listeners)
  * decide how much wellbeing to transfer to which parents and umwelt CAs
    * from reviewing wellbeing status events received
    * and own wellbeing reserves + depletion rates (they modulate generosity)
  * send messages transfering wellbeing to needy parents and/or umwelt CAs
    * clear received wellbeing status events

Trigger a life event, or none
  * ask SOM to trigger the next life event: apoptosis, replication, division or none
  * wait for the SOM to realize it, unless none
    * apoptosis distributes fullness to parents and umwelt
    * replication/division divides fullness equally, integrity is copied, engagement starts full for new CAs
*/

% plan{id: ID, goal_id: GoalID, directives: [goal{...} | Action, ...], status:Status, score: Score, timeframe_index: Index}
% goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id:IntentId, intent_level: Level, timeframe_index: Index}
% experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, feeling:Feeling, by:CA}
% target{origin: Origin, kind: Kind, value: Value}
% object{type:synthetic, id:Id, evidence: [ObservationId, ...]}

:- module(assess, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/goal_states)).
:- use_module(agency(som/goals)).
:- use_module(agency(som/ca_support)).

% No work done before units of work
before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).


% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDelta) or done(StateDeltas, WellbeingDelta) as last solution. 
unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
  wellbeing:empty_wellbeing(EmptyWellbeingDelta),
  assess_acting(CA, State, [], EmptyWellbeingDelta, StateDeltas1, WellbeingDelta1),
  evaluate_causal_theory(CA, State, StateDeltas1, WellbeingDelta1, StateDeltas2, WellbeingDelta2),
  diffuse_wellbeing(CA, State, StateDeltas2, WellbeingDelta2, StateDeltas3, WellbeingDelta3),
  life_event(CA, State, StateDeltas3, WellbeingDelta3, StateDeltas, WellbeingDelta),
  log(info, assess, "Phase assess ended for CA ~w", [CA]).

assess_acting(CA, State, StateDeltas, WellbeingDelta, NewStateDeltas, NewWellbeingDelta) :-
  maybe_abandon_intent(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1),
  maybe_abandon_directives(CA, State, StateDeltas1, WellbeingDelta1, StateDeltas2, WellbeingDelta2),
  maybe_abandon_plans(CA, State, StateDeltas2, WellbeingDelta2, StateDeltas3, WellbeingDelta3),
  assess_goals(CA, State, StateDeltas3, WellbeingDelta3, StateDeltas4, WellbeingDelta4),
  score_plans(CA, State, StateDeltas4, WellbeingDelta4, NewStateDeltas, NewWellbeingDelta).

maybe_abandon_intent(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta) :-
  state_plus_deltas(State, StateDeltas, WellbeingDelta, State1),
  get_state(State, intent, Intent),
  (retain_intent(Intent, State1) ->
    StateDeltas1 = StateDeltas
    ;
    abandon_intent(Intent, State1, Plans),
    forget_goal_state(Intent, State, UpdatedGoalStates),
    merge_options([intent = none, plans = Plans, goal_states = UpdatedGoalStates], StateDeltas, StateDeltas1)
  ).

% Abandon all directives that are no longer relevant (they are about terminated experiences)
% The umwelt is not told of the CA's abandoned directives.
% The umwelt orphaned goal states and directives will eventually be deleted when the originating intent completes or is abandoned.
maybe_abandon_directives(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, EmptyWellbeingDelta) :-
  state_plus_deltas(State, StateDeltas, WellbeingDelta, State1),
  wellbeing:empty_wellbeing(EmptyWellbeingDelta),
  get_state(State1, goal_states, GoalStates),
  findall(GoalState, 
          (member(GoalState, GoalStates), is_irrelevant_directive_goal_state(GoalState, State1)),
          DirectiveGoalStates),
  abandon_directives(DirectiveGoalStates, State1, RemainingPlans, RemainingGoalStates),
  merge_options([goal_states = RemainingGoalStates, plans = RemainingPlans], StateDeltas, StateDeltas1).

% Quietly abandon stalled plans.
maybe_abandon_plans(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1) :-
  state_plus_deltas(State, StateDeltas, WellbeingDelta, State1),
  stalled_plans(State, StalledPlans),
  get_state(State1, plans, Plans),
  subtract(Plans, StalledPlans, RemainingPlans),
  merge_options([plans = RemainingPlans], StateDeltas, StateDeltas1).

% For each executed goal state in this timeframe,
% mark as achieved if targeted experience was impacted as intended.
assess_goals(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1) :-
  state_plus_deltas(State, StateDeltas, WellbeingDelta, State1),
  get_state(State, goal_states, GoalStates),
  maybe_mark_goal_states_achieved(GoalStates, State, UpdatedGoalStates),
  merge_options([goal_states = UpdatedGoalStates], StateDeltas, StateDeltas1).

% For each executed plan in this or prior timeframes,
% if the associated goal state status is achieved in this timeframe,
%   give it a score (in its timeframe) proportional to the timeframes distance,
score_plans(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1) :-
  state_plus_deltas(State, StateDeltas, WellbeingDelta, State1),
  % TODO
  merge_options([], StateDeltas, StateDeltas1).
  
evaluate_causal_theory(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1) :-
  state_plus_deltas(State, StateDeltas, WellbeingDelta, State1),
  % TODO
  merge_options([], StateDeltas, StateDeltas1).

  
diffuse_wellbeing(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1).
  
life_event(CA, State, StateDeltas, WellbeingDelta, StateDeltas1, WellbeingDelta1).

retain_intent(none, _) :- !.

retain_intent(Intent, State) :-
  \+ intent_stalled(Intent, State),
  goal_relevant(Intent, State).

% An intent is stalled if it has been waiting execution for too long.
intent_stalled(Intent, State) :-
  get_state(State, timeframe_count, TimeframeCount),
  goal_state_of(Intent, State, GoalState),
  \+ memberchk(GoalState.status, [executing, executed, achieved]),
  Age is TimeframeCount - Intent.timeframe_index,
  % TODO - get constant from settings
  Age > 5.

stalled_plans(State, StalledPlans) :-
  get_state(State, plans, Plans),
  findall(StalledPlan, (member(StalledPlan, Plans), plan_stalled(StalledPlan, State)), StalledPlans).

% A plan is stalled if it has been waiting execution for too long.
plan_stalled(Plan, State) :-
  get_state(State, timeframe_count, TimeframeCount),
  \+ memberchk(Plan.status, [executing, executed]),
  Age is TimeframeCount - Plan.timeframe_index,
  % TODO - get constant from settings
  Age > 5.

% The intent is still relevant if the CA still has the exact same experience the intent targets.
% Even if an experience is not the same but is compatible with the intent's target, (e.g. a count of observations with one more observation)
% we nonetheless want to scrap the intent so it gets recreated; the differences can affect impact planning. 
goal_relevant(Goal, State) :-
  target{origin:Origin, kind:Kind, value:Value} :< Goal.target,
  get_state(State, experiences, Experiences),
  member(Experience, Experiences),
  experience{origin:Origin, kind:Kind, value:Value} :< Experience.

% Drop plan, if neither executing or executed, for the intent and announce that the intent is abandoned.
abandon_intent(Intent, State, UpdatedPlans) :-
  get_state(State, plans, Plans),
  (standby_plan_for_intent(Intent, State, Plan) ->
    subtract(Plans, [Plan], UpdatedPlans)
    ;
    UpdatedPlans
  ),
  published(abandoned([intent_id=Intent.id])).

standby_plan_for_intent(Intent, State, Plan) :-
  get_state(State, plans, Plans),
  member(Plan, Plans),
  plan{goal_id:Intent.id, status:Status} :< Plan,
  \+ memberchk(Status, [executed, executing]).

is_irrelevant_directive_goal_state(GoalState, State) :-
  \+ is_intent(GoalState.goal), 
  GoalState.status == planning, 
  \+ goal_relevant(GoalState.goal, State).

forget_goal_state(Goal, State, UpdatedGoalStates) :-
  get_state(State, goal_states, GoalStates),
  member(GoalState, GoalStates),
  GoalState.goal.id == Goal.id,
  subtract(GoalStates, [GoalState], UpdatedGoalStates), !.

forget_goal_state(_, State, GoalStates) :-
  get_state(State, goal_states, GoalStates).

% Forget the directives' goal states and plans
% The umwelt is not informed (we don't know if it got the same directive from different parents).
% The umwelt orphaned goal states and plans will be deleted when the originating intent is completed or abandoned.
% Remember that plan execution is triggered transitively by the CA with the originating intent.
abandon_directives([], State, RemainingPlans, RemainingGoalStates) :-
  get_state(State, plans, RemainingPlans),
  get_state(State, goal_states, RemainingGoalStates).

abandon_directives([GoalState | Rest], State, RemainingPlans, RemainingGoalStates) :-
  abandon_directives(Rest, State, RemainingPlans1, RemainingGoalStates1),
  (goal_state_plan(GoalState, State, Plan) ->
    subtract(RemainingPlans1, [Plan], RemainingPlans)
    ;
    RemainingPlans = RemainingPlans1
  ),
  subtract(RemainingGoalStates1, GoalState, RemainingGoalStates).

goal_state_plan(GoalState, State, Plan) :-
  get_state(State, plans, Plans),
  member(Plan, Plans),
  Plan.goal_id = GoalState.goal.id.

maybe_mark_goal_states_achieved([], _, []).

maybe_mark_goal_states_achieved([GoalState | Rest], State, [UpdatedGoalState | OtherUpdatedGoalStates]) :- 
  Goal = GoalState.goal,
  ((GoalState.status == executed, goal_achieved(Goal, State)) ->
    UpdatedGoalState = GoalState.put(status, achieved)
    ;
    UpdatedGoalState = GoalState
  ),
  maybe_mark_goal_states_achieved(Rest, State, OtherUpdatedGoalStates).

goal_achieved(Goal, State) :-
  goal{target:Target, impact:Impact} :< Goal,
  goal_impacted(Target, Impact, State).

% target{origin: Origin, kind: Kind, value: Value}
% Impact = terminate | persist | create
% TODO
goal_impacted(Target, Impact, State).


state_plus_deltas(State, StateDeltas, WellbeingDelta, State1) :-
  phase_consumes_produces(assess, _, ProducedProperties),
  merge_phase_deltas(StateDeltas, WellbeingDelta, ProducedProperties, State, State1).

% unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
%     staying_alive(State, Alive),
%     StateDeltas = [alive=Alive],
%     wellbeing:empty_wellbeing(WellbeingDelta),
%     log(info, assess, "Phase assess ended for CA ~w", [CA]).

staying_alive(State, Alive) :-
    get_state(State, timeframe_count, Count),
    get_state(State, settings, Settings),
    option(max_timeframes(Max), Settings, infinity),
    ((Max \== infinity, Count >= Max) -> Alive = false ; Alive = true).