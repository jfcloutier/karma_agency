/**
Possibly create/replace intent, advance prioritized goal_states,
find plans for intent and for received directives, progress the execution of plans.

State properties produced by this phase but not consumed upon entering it:

intent - intent created or replaced
plans - new plans found
goal_states - records on the progress of goals

Plan creation involves:

% experience{origin:Object, kind:unchanged, value:Count, confidence: Confidence, by:CA}
% synthetic_object([Observation.id], Object) => Object = object{type:synthetic, id:ObjectId, evidence:ObservationIds}
% plan{id: ID, goal_id: GoalID, directives: [goal{...} | Action, ...], status:Status, score: Score}
% goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id: IntentId, intent_level: Level}
% target{origin: Origin, kind: Kind, value: Value}
% object{type:synthetic, id:Id, evidence: [ObservationId, ...]}
% observation{id:Id, origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA}

**/

:- module(act, []).

:- use_module(utils(logger)).
:- use_module(utils(tools)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/goals)).
:- use_module(agency(som/goal_states)).
:- use_module(agency(som/plans)).
:- use_module(agency(som/experiences)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/dynamic_ca)).
:- use_module(agency(som/ca_support)).

% Computing umwelt actions before individual units of work
before_work(_, State, [umwelt_actions = UmweltActions], WellbeingDeltas) :-
    umwelt_actions(State.umwelt, UmweltActions),
    wellbeing:empty_wellbeing(WellbeingDeltas).

% A unit of work consists in identifying the most important goal state to advance and then advancing it, possibly to a dead end.
% The work is done if there is no goal state left to advance.
% The unit of work must always be carried out on the latest goal states, which may have changed prior to entering this phase or before each unit of work.
unit_of_work(CA, State, more(StateDeltas, WellbeingDeltas)) :-
    repeat,
    % query_answered/3 is deterministic
    query_answered(CA, goal_states, GoalStates),
    query_answered(CA, plans, Plans),
    % Are we sure the CA has updated its goal states from the last unit of work?
    % - Yes since the query always follows the work_progressed message sent after the previous unit of work.
    log(info, act, "~w is acting on goal states ~p and plans ~p", [CA, GoalStates, Plans]),
    acted(CA, State, GoalStates, Plans, StateDeltas, WellbeingDeltas).

unit_of_work(_, _, done([], WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).

% Find all actions supported by effector CAs currently in the CA's umwelt
umwelt_actions(Umwelt, Actions) :-
    findall(ActionDomain, (member(UmweltCA, Umwelt), is_effector(UmweltCA), query_answered(UmweltCA, action_domain, ActionDomain)), Actions1),
    flatten(Actions1, Actions2),
    sort(Actions2, Actions),
    log(info, act, "All umwelt actions: ~p", [Actions]).

% Succeeds with creating an intent and/or advancing the top goal (might cause plans to be found), or it fails if no goal state can be advanced.
% ASSUMPTION: Goals progress monotonically (no backsliding) until they are executed or have reached a deadend.
% State deltas include intent, plans and goal_states

% The CA acts by creating a missing intent to advance (we only need to check goal states for evidence of an existing and possibly progressing intent)
% A later work unit would find a plan for it whenever the intent becomes the most urgent goal to advance.
acted(CA, State, GoalStates, _, [intent=Intent, goal_states=[IntentGoalState]], WellbeingDeltas) :-
    \+ intent_in_progress(GoalStates, _),
    % TODO - No wellbeing deltas for now
    wellbeing:empty_wellbeing(WellbeingDeltas),
    intent_created(CA, State, Intent, IntentGoalState).

% Select the most urgent goal that can be advanced and advance it
acted(CA, State, GoalStates, Plans, [goal_states = AdvancedGoalStates, plans = NewPlans], WellbeingDeltas) :-
    intent_in_progress(GoalStates, _),
    % choice point
    urgent_goal_state(GoalStates, GoalState),
    log(info, act, "The urgent goal state is ~p", [GoalState]),
    % can fail if the goal state is not advanced
    goal_state_advanced_from(GoalState.status, GoalState, CA, State, GoalStates, Plans, AdvancedGoalStates, NewPlans),
    log(info, act, "Advanced goal state ~p to ~p with new plans ~p", [GoalState, AdvancedGoalStates, NewPlans]),
    !,
    % TODO - No wellbeing deltas for now
    wellbeing:empty_wellbeing(WellbeingDeltas).

acted(_, _, _, _, [], WellbeingDeltas) :-
    log(info, act, "Nothing to act on"),
    sleep(0.1),
    wellbeing:empty_wellbeing(WellbeingDeltas).

% goal_state{goal: Goal, status: Status, messages: [GoalMessage, ...]}
% goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id: IntentId, intent_level: Level}
% The CA has a progressing intent if there's a goal state where the goal id is the intent id,
% and its status indicates progress.
intent_in_progress(GoalStates, Goal) :-
    member(GoalState, GoalStates),
    Goal = GoalState.goal,
    % An intent is a goal that is its own intent (it is not subjugated to an ancestor CA's intent)
    Goal.intent_id = Goal.id,
    goal_state_is_advancing(GoalState),
    !.

% An intent is created by wanting to impact the most felt experience that can be impacted and planned for
% An intent is the only goal a CA creates on its own; its other goals are given to it as directives by its parent CAs.
intent_created(CA, State, Intent, IntentGoalState) :-
    dynamic_ca:level_from_name(CA, Level),
    experience_to_impact(State, Experience),
    goal_from_experience(Experience, Level, Goal),
    % An intent is a goal whose intent_id and id are the same - the goal is self-serving
    Intent = Goal.put(intent_id, Goal.id),
    % The intent is created with goal state status of `planning`, meaning the next step is to find a plan for the intent
    IntentGoalState = goal_state{goal: Intent, status: planning, messages: []},
    !,
    log(info, act, "Intent created ~p with goal state ~p", [Intent, IntentGoalState]).

% Find the experience starting with the most intensely felt (absolute feeling scaled by confidence).
% When there is a tie, choose one randomly.
% Non-deterministic
experience_to_impact(State, Experience) :-
    findall(Exp, (member(Exp, State.experiences), is_experience_impactable(Exp)), ImpactableExperiences),
    experiences_sorted_by_intensity(ImpactableExperiences, SortedExperiences),
    % Choose an experience starting from the most intense
    member(Experience, SortedExperiences),
    log(info, act, "Experience to impact is ~p", [Experience]).

% Create a goal from an experience to impact
% Determine what kind of impact based on whether the experience is good vs bad
goal_from_experience(Experience, IntentLevel, Goal) :-
    experience{origin:Object, kind:Kind, value:Value} :< Experience,
    % The target of a goal is the experienced property/relation that was synthesized fomr observations (of umwelt experiences)
    Target = target{origin:Object, kind:Kind, value:Value},
    desired_impact(Experience, Impact),
    % The goal's priority is the intensity of the felt experience
    experience_intensity(Experience, Priority),
    % Create a goal without intent id
    GoalWithoutId = goal{target: Target, impact: Impact, priority: Priority, intent_level: IntentLevel},
    goal_with_id(GoalWithoutId, Goal),
    log(info, act, "The goal from ~p is ~p", [Experience, Goal]).

% Persist a good or neutral experience, and terminate a bad one.
desired_impact(Experience, Impact) :-
    Experience.feeling < 0 ->
        Impact = terminate
        ;
        Impact = persist.

% Progress the goal state, which may lead to a new plan.
% Fails if the goal state cannot be advanced.

% A received directive is to be done.
% The goal state is advanced by the CA determining if it can seek or not to achieve it.
goal_state_advanced_from(todo, GoalState, _, State, _, _, [UpdatedGoalState], []) :-
    can_seek_goal(GoalState.goal, State) ->
        published(can_seek, [directive=GoalState.goal]),
        UpdatedGoalState = GoalState.put(status, can_seek)
        ;
        published(cannot_seek, [directive=GoalState.goal]),
        UpdatedGoalState = GoalState.put(status, cannot_seek).

% A goal state is moved to planning because the CA is asked by a parent CA to find a plan to achieve the goal, 
% or the goal state is planning from the start because the goal is the intent of the CA.
% A goal stays in planning while a plan for it is being searched.
% If there is already a possible plan for the goal and,
%   if all the plan's directives can_execute (infered from messages received by the CA from its umwelt), then advance the goal state to can_execute and publish this
%   if any of the plan's directives cannot_execute (inferred from messages), mark the plan as cannot_execute (keep it to avoid duplication of effort, don't publish since there might be an alternate plan)
%   if executability is still TBD, fail since the goal can not be advanced at the moment (it is in limbo)
% If there is an executed plan, move the goal state to executed (should already be there anyway), 
%   advance the goal state to executed and publish it for the planned-for directive 
% If there is no executed or potentially executable plan yet for the directive/intent, find one
%       if no plan can be found, 
%           advance the goal state to cannot_execute and publish this for the planned-for directive
% Finding a plan
%   Find a sequence of directives targeting observations (of experiences in the umwelt) that, if executed, might achieve the goal.
%   The plan can be an affordance (a plan executed in a prior timeframe) or be newly created (with status unknown).
%   When found, send a todo to the umwelt with all the directives in the plan.

% The plan is somehow already executed. Advance the goal state status to executed and let parents know if they didn't already.
goal_state_advanced_from(planning, GoalState, _, _, _, Plans, [UpdatedGoalState], []) :-
    known_plan_for_goal_state(Plans, GoalState, Plan),
    Plan.status == executed,
    !,
    UpdatedGoalState = GoalState.put(status, executed),
    published(executed, [directive_id(Plan.goal_id)]).

% The plan is ready to execute
goal_state_advanced_from(planning, GoalState, _, _, GoalStates, Plans, [UpdatedGoalState], [UpdatedPlan]) :-
    known_plan_for_goal_state(Plans, GoalState, Plan),
    Plan.status == possible,
    all_plan_directives_can_execute(Plan, GoalStates),
    !,
    UpdatedPlan = Plan.put(status, can_execute),
    UpdatedGoalState = GoalState.put(status, can_execute),
    published(can_execute, [directive_id(Plan.goal_id)]).

% A plan thought possible can not be executed in this timeframe
goal_state_advanced_from(planning, GoalState, _, _, GoalStates, Plans, [UpdatedGoalState], [UpdatedPlan]) :-
    known_plan_for_goal_state(Plans, GoalState, Plan),
    Plan.status == possible,
    member(Directive, Plan.directives),
    directive_has_any_status(Directive, GoalStates, [cannot_execute]),
    !,
    UpdatedPlan = Plan.put(status, cannot_execute),
    UpdatedGoalState = GoalState.put(status, cannot_execute).

% The plan for this goal state has status unknown or possible and can't be advanced. It will remain so until it is executing. Advancing the goal state thus fails.
goal_state_advanced_from(planning, GoalState, _, _, _, Plans, [], []) :-
    known_plan_for_goal_state(Plans, GoalState, Plan),
    memberchk(Plan.status, [unknown, possible]),
    !,
    fail.

% An executed plan is found from a prior timeframe (an affordance) for the stated goal, 
% or a new plan is found.
% Broadcast the plan's directives to the umwelt.
goal_state_advanced_from(planning, GoalState, _, State, _, Plans, [], [Plan]) :-
    (affordance(GoalState, State, Plan) ->
        true
        ;
        new_plan(GoalState, State, Plans, Plan)
    ),
    !,
    published(todo, [directives(Plan.directives)]).

% Progress is made in progressing the execution of the goal's plan by having each of its directives executed in turn
% Find the plan for the goal with goal state
% Find the first directive that is can_execute and publish `execute` for the directive.
% Fail otherwise.
goal_state_advanced_from(executing, GoalState, CA, _, GoalStates, Plans, [UpdatedGoalState], []) :-
    known_plan_for_goal_state(Plans, GoalState, Plan),
    plan_directive_with_goal_state_status(Plan, GoalStates, can_execute, GS, Directive), 
    published(execute, [directive_id(Directive.id)]),
    UpdatedGoalState = GS.put(messages, [message{about:execute, source:CA} | GS.messages]).

known_plan_for_goal_state(Plans, GoalState, Plan) :-
    member(Plan, Plans),
    Plan.goal_id == GoalState.goal.id.

plan_directive_with_goal_state_status(Plan, GoalStates, Status, GS, Directive) :-
    member(Directive, Plan.directives),
    member(GS, GoalStates),
    GS.goal.id == Directive.id,
    GS.status == Status.

% Whether there is an experience (irrespective of the experienced property/relation's value) that matches the target of the goal
can_seek_goal(Goal, State) :-
    target{origin:Object, kind:Kind} :< Goal.target,
    member(Experience, State.experiences),
    experience{origin:Object, kind:Kind} :< Experience.

% All plan directives can execute or have already been executed
all_plan_directives_can_execute(Plan, GoalStates) :-
    forall(member(Directive, Plan.directives), directive_has_any_status(Directive, GoalStates, [can_execute, executed])).

directive_has_any_status(Directive, GoalStates, Statuses) :-
    member(GoalState, GoalStates),
    GoalState.goal.id == Directive.id,
    memberchk(GoalState.status, Statuses).

% An affordance is a plan from a prior timeframe for the current goal in this timeframe.
% The higher the score, the more likely the affordance will be reused.
% If the same plan is scored in multiple timeframes, the latest score is used.
% Going back in time(frames), accumulate the plans for the given goal, ignoring older duplicates
% For each matching plan, starting with the most recent, roll the dice on whether to reuse it or not
%   The higher its score, the more likely it will be picked
% Stop after one is picked or fail if none are.
affordance(GoalState, State, Plan) :-
    scored_plans_for_goal(State.timeframes, GoalState.goal, ScoredPlans),
    !,
    reused_scored_plan(ScoredPlans, Plan),
    log(info, act, "Reusing affordance ~p from ~p", [Plan, ScoredPlans]).

scored_plans_for_goal(Timeframes, Goal, ScoredPlans) :-
    matching_scored_plans(Timeframes, Goal, [], Plans),
    map_list_to_pairs(plan_score, Plans, Pairs),
    keysort(Pairs, ScoredPlans1),
    % Highest score first
    reverse(ScoredPlans1, ScoredPlans).

matching_scored_plans([], _, Plans, Plans).
matching_scored_plans([Timeframe | Rest], Goal, Acc, Plans) :-
    matching_scored_plan(Timeframe.plans, Goal, Plan),
    \+ (member(AccPlan, Acc), same_directives(Plan.directives, AccPlan.directives)),
    !,
    matching_scored_plans(Rest, Goal, [Plan | Acc], Plans).

matching_scored_plans([_ | Rest], Goal, Acc, Plans) :-
    matching_scored_plans(Rest, Goal, Acc, Plans).

matching_scored_plan(Timeframe, Goal, Plan) :-
    member(Plan, Timeframe.plans),
    Plan.score \= 0,
    % Two goals with the same id are semantically the same goal
    Plan.goal.id == Goal.id.

% Same directives irrespective of intent in the same order?
same_directives([], []).

same_directives([Goal1 | Rest1], [Goal2 | Rest2]) :-
    Goal1.id == Goal2.id,
    same_directives(Rest1, Rest2).

% A plan score is between 0.0 and 1.0
% Roll a loaded dice (between 0.0 and 1.2) so that 20% of the time, even a perfect plan is not the one picked
reused_scored_plan([Score-Plan | _], Plan) :-
    random(R),
    (R * 1.2) < Score,
    !.

reused_scored_plan([_ | Rest], Plan) :-
    reused_scored_plan(Rest, Plan).

% Look for a new plan that could achieve the give goal referenced in its state.
% To be new, a must not repeat any plan in the timeframe.
% A new plan starts with status `unknown`. The CA's umwelt will tell the CA if it is `possible` or `cannot_execute`.
% goal_state{goal: Goal, status: Status, received:Boolean messages: [GoalMessage, ...]}
% goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id:IntentId, intent_level: Level}
% target{origin: Origin, kind: Kind, value: Value}
% Impact = terminate | persist | create
% create - requires causal theory
new_plan(GoalState, State, Plans, NewPlan) :-
    log(info, act, "Making new plan for ~p", [GoalState]),
    goal{target:Target, impact:Impact} :< GoalState.goal,
    plan_for_goal(Target.kind, Impact, GoalState.goal, State, NewPlan),
    \+ (member(CurrentPlan, Plans), same_plans(CurrentPlan, NewPlan)),
    log(info, act, "New plan ~p for goal state ~p", [NewPlan, GoalState]).

% The CA needs a plan for the goal to persist an `unchanged` experience it has (represented by the Target property or relation).
% The goal is either a parent's directive or the CA's own intent.
% The plan's sole directive will be to persist the evidence for that experience of the CA (the evidence is an umwelt experience that was observed as unchanging),
% so that the CA's `unchanged` experience still exists in the next timeframe (albeit with its value incremented by one timeframe count).
plan_for_goal(unchanged, persist, Goal, State, NewPlan) :-
    % What umwelt experience was observed not to change
    [ObservationId] = Goal.target.origin.evidence,
    observation_from_id(ObservationId, State, Observation),
    % The CA wants the observed umwelt experience to persist, with the same priority and intent context as the goal the directive is meant to achieve
    directives_from_observation(Observation, persist, Goal, State, Directive),
    plan(Goal.id, [Directive], NewPlan).

% The CA needs a plan to terminate an 'unchanged` experience it has.
% This can be done by terminating the evidence for that experience (an umwelt experience observed not to change) in a number of ways:
% The observed experience terminates if any experience of the same kind with the same origin has a different value
% or if no such experience is found (irrespective of value).
plan_for_goal(unchanged, terminate, Goal, State, NewPlan) :-
    [ObservationId] = Goal.target.origin.evidence,
    observation_from_id(ObservationId, State, Observation),
    directives_from_observation(Observation, terminate, Goal, State, Directive),
    plan(Goal.id, [Directive], NewPlan).

% The CA needs a plan for the goal to persist a `count` experience
% This can be done by persisting the full evidence for that experience (kin umwelt experiences observed and counted)
plan_for_goal(count, persist, Goal, State, NewPlan) :-
    ObservationIds = Goal.target.origin.evidence,
    directives_to_impact_all_observations(ObservationIds, Goal, persist, State, Directives),
    plan(Goal.id, Directives, NewPlan).

% The CA needs a plan for the goal to persist a `count` experience
% This can be done by terminating any portion of the evidence for that experience (kin umwelt experiences observed and counted)
plan_for_goal(count, terminate, Goal, State, NewPlan) :-
    ObservationIds = Goal.target.origin.evidence,
    members(SomeObservationIds, ObservationIds),
    directives_to_impact_all_observations(SomeObservationIds, Goal, terminate, State, Directives),
    plan(Goal.id, Directives, NewPlan).

% The CA needs a plan for the goal to persist a `more` experience
% A `more` experience is a relation where the origin's evidence outnumbers the value object's evidence.
% Persisting can be done by persisting all origin evidence/observations and terminate 0 or more of value object evidence/observations
plan_for_goal(more, persist, Goal, State, NewPlan) :-
    MoreObservationIds = Goal.target.origin.evidence,
    FewerObservationIds = Goal.target.value.evidence,
    directives_to_impact_all_observations(MoreObservationIds, Goal, persist, State, Directives1),
    members_or_none(SomeObservationIds, FewerObservationIds),
    directives_to_impact_all_observations(SomeObservationIds, Goal, terminate, State, Directives2),
    append(Directives1, Directives2, AllDirectives),
    random_permutation(AllDirectives, Directives),
    plan(Goal.id, Directives, NewPlan).
    
% The CA needs a plan for the goal to terminate a `more` experience
% A `more` experience is a relation where the origin's evidence outnumbers the value object's evidence.
% Terminating can be done by persisting all value evidence/observations and terminating enough origin object evidence/observations so there aren't more.
plan_for_goal(more, terminate, Goal, State, NewPlan) :-
    MoreObservationIds = Goal.target.origin.evidence,
    FewerObservationIds = Goal.target.value.evidence,
    length(MoreObservationIds, MoreCount),
    length(FewerObservationIds, FewerCount),
    MinTerminate is MoreCount - FewerCount,
    random_between(MinTerminate, MoreCount, TerminateCount),
    random_permutation(MoreObservationIds, PermutedMoreObservationIds),
    take(TerminateCount, PermutedMoreObservationIds, SomeObservationIds),
    directives_to_impact_all_observations(SomeObservationIds, Goal, terminate, State, Directives),
    plan(Goal.id, Directives, NewPlan).

% When the CA needs a plan to persist a `trend` experience.
%   If the trend value is up, we want a next observation with a value greater than the latest Observation.
%   If the trend value is down, we want a next observation with a value lesser than the latest Observation.
%   If the trend value is ended, we want a next observation with the same value as that of the latest Observation.
% When the CA needs a plan to terminate a `trend` experience.
%   If the trend value is up, we want a next observation with a value lesser or equal to that of the latest Observation.
%   If the trend value is down, we want a next observation with a value greater or equal to that of the latest Observation.
%   If the trend value is ended, we want a next observation with a value different from that of the latest Observation.
plan_for_goal(trend, Impact, Goal, State, NewPlan) :-
    TrendValue = Goal.target.value,
    [_, LatestObservation] = Goal.target.origin.evidence,
    plan_for_trend_goal(Impact, TrendValue, LatestObservation, Goal, State, NewPlan).
    
% We want a next observation with a value greater than that of the latest Observation
plan_for_trend_goal(Impact, Trend, LatestObservation, Goal, State, NewPlan) :-
    member(Impact-Trend, [persist-up, terminate-down, terminate-ended]),
    Value = Goal.target.value,
    inc_simple_count(Value, IncValue),
    NextObservation = LatestObservation.put(value, IncValue),
    IncValue \= Value,
    directives_from_observation(NextObservation, create, Goal, State, Directives),
    plan(Goal.id, Directives, NewPlan).

% We want a next observation with a value lesser than that of the latest Observation
plan_for_trend_goal(Impact, Trend, LatestObservation, Goal, State, NewPlan) :-
    member(Impact-Trend, [persist-down, terminate-up, terminate-ended]),
    Value = Goal.target.value,
    dec_simple_count(Value, DecValue),
    DecValue \= Value,
    NextObservation = LatestObservation.put(value, DecValue),
    directives_from_observation(NextObservation, create, Goal, State, Directives),
    plan(Goal.id, Directives, NewPlan).

% We want a next observation with the same value as that of the latest Observation
plan_for_trend_goal(Impact, Trend, LatestObservation, Goal, State, NewPlan) :-
    member(Impact-Trend, [persist-ended, terminate-up, terminate-down]),
    directives_from_observation(LatestObservation, persist, Goal, State, Directives),
    plan(Goal.id, Directives, NewPlan).

observation_from_id(ObservationId, State, Observation) :-
    member(Observation, State.observations),
    ObservationId == Observation.id.

directives_to_impact_all_observations([], _, _, _, []).
directives_to_impact_all_observations(ObservationIds, Goal, Impact, State, AllDirectives) :-
    log(info, act, "Finding directives to ~w observations ~p given goal ~p", [Impact, ObservationIds, Goal]),
    findall(Observation, (member(ObservationId, ObservationIds), observation_from_id(ObservationId, State, Observation)), Observations),
    log(info, act, "The observations to ~w are ~p", [Impact, Observations]),
    findall(Directives, (member(Observation, Observations), directives_from_observation(Observation, Impact, Goal, State, Directives)), AllDirectives1),
    flatten(AllDirectives1, AllDirectives),
    log(info, act, "The directives are ~p", [AllDirectives]).

% Directives from a synthetic observation
directives_from_observation(Observation, Impact, FromGoal, _, [Directive]) :-
    \+ is_sensory_observation(Observation),
    observation_target(Observation, Target),
    GoalWithoutId = goal{target: Target, impact: Impact, priority: FromGoal.priority, intent_id: FromGoal.intent_id, intent_level: FromGoal.intent_level},
    goal_with_id(GoalWithoutId, Directive).

% Comeup with directives/actions to impact a sensory observation, i.e. observation of a sensor CA experience
% i.e. observation{id:Id, origin:object{type:sensor, id:SensorName}, kind:SenseName, value:Value, confidence:Confidence, by:CA}
% Unless it uses an affordance (it does not since we are here), the CA does not know (yet) a sequence of effector actions
% that will produce the desired impact on the observed sensory experience. So it has to guess.
% From all actions supported by effector CAs in the CA's umwelt, generate a list of actions (0 or 1-3 of each) and randomly permute.
% Doing nothing is not an option.
directives_from_observation(Observation, _, _, State, Actions) :-
    is_sensory_observation(Observation),
    action_groups(State.umwelt_actions, ActionGroups),
    flatten(ActionGroups, Actions1),
    Actions1 \= [],
    random_permutation(Actions1, Actions).   

action_groups([], []).

action_groups([Action | Rest], [Group | OtherGroups]) :-
    action_group(Action, Group),
    action_groups(Rest, OtherGroups).

% 50% of the time, don't include the action
% Otherwise have it be repeated 1 to 3 times
action_group(Action, ActionGroup) :-
    random_between(0, 1, R),
    (R == 0 ->
        ActionGroup = []
        ;
        random_between(1, 3, N),
        replicate(Action, N, ActionGroup)
    ).
