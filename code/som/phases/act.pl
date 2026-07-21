/**
Execute plans that are predicted as executed, create an intent if missing, find plans for intent and for directives predicted as planned.

* Execute a plan when its goal activation is experienced as `planned`
 * immediately if the plan's goal is the intent
 * only if the plan's goal is a directive which activation is predicted as `executed`
* Create an intent
  * only if there is currently none
* Find a plan for the intent and for each directive predicted by a parent CA as `planned`
  * only if it has none
  * a plan is found by
    * selecting a goal-matching affordance
    * or building the plan from scratch

State properties produced by this phase but not consumed upon entering it:

intent - intent maybe created
plans - new plans may be found
experiences - goal activation experiences may be had

Acting involves:

% prediction{origin:Origin, kind:Kind, value:Value, weight:Weight, confidence:Confidence, by: CA, when_received: Age} - predictions received about the activations of received directives
% observation{id:Id, origin:Object, kind:Kind, value:Value, confidence:Confidence, by:CA} - observed activations of directives sent to the umwelt
% experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, feeling:Feeling, by:CA} - experienced activation status of intent and received directives
% plan{goal:goal{...}, directives:[goal{...} | command{...}, ...]} - plans found for intent and received directives
% goal{target: target{...}, impact: Impact, priority: Priority, intent_level: Level} - intent or directive
%   target{origin: object{...}, kind: Kind, value: Value}
%       Impact = terminate | persist | create
%       object{type:ObjectType, id:Id, evidence: [ObservationId, ...]} -- Only synthetic objects have evidence (a list of observation IDs)
%           ObjectType = sensor | synthetic | goal
% command{effector_ca:CA_ID, action:Action}

**/

:- module(act, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/plans)).
:- use_module(agency(som/experiences)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/goals)).
:- use_module(agency(som/predictions)).
:- use_module(agency(som/dynamic_ca)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(body)).

before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% Execute a plan,
% or create a missing intent,
% or find a required plan.
unit_of_work(CA, State, more(StateDeltas, WellbeingDelta)) :-
    plan_executed(CA,State, StateDeltas, WellbeingDelta)
    ;
    intent_created(CA,State, StateDeltas, WellbeingDelta)
    ;
    plan_found(CA, State, StateDeltas, WellbeingDelta).

% Nothing more to do
unit_of_work(_, _, done([], WellbeingDelta)) :-
    wellbeing:empty_wellbeing(WellbeingDelta).

% One plan is to be found if there is an intent and it has none, or a directive was predicted as `planned` by a parent CA.
%! plan_found(++CA, ++State, --StateDeltas, --WellbeingDelta)
plan_found(CA, State, [plans=[Plan]], WellbeingDelta) :-
    get_state(State, intent, Intent),
    Intent \= none,
    \+ plan_for_goal(Intent, State, _),
    plan_found_for_goal(Intent, CA, State, Plan, WellbeingDelta).

plan_found(CA, State, [plans=[Plan]], WellbeingDelta) :-
    is_directive_activation_predicted_as(Directive, State, planned),
     \+ plan_for_goal(Directive, State, _),
    plan_found_for_goal(Directive, CA, State, Plan, WellbeingDelta).

%% INTENT CREATION

%! intent_created(++CA, ++State, --StateDeltas, --WellbeingDelta)
intent_created(CA, State, StateDeltas, WellbeingDelta) :-
    get_state(State, intent, Intent),
    Intent \= none,
    new_intent(CA, State, Intent, StateDeltas),
    % TODO - No wellbeing deltas for now
    wellbeing:empty_wellbeing(WellbeingDelta).

% An intent is created by wanting to impact the most felt experience that can be impacted and planned for
% An intent is the only goal a CA creates on its own; its other goals are given to it as directives by its parent CAs.
% When creating an intent, don't repeat the previous intent; it was abandoned for a reason.
%! new_intent(++CA, ++State, --Intent, --StateDeltas)
new_intent(CA, State, Intent, [intent=Intent, experiences=[IntentActivationExperience]]) :-
    dynamic_ca:level_from_name(CA, Level),
    experience_to_impact(CA, State, Experience),
    get_state(State, age, Age),
    goal_from_experience(Experience, Level, Age, Intent),
    \+ is_intent_repeating(Intent, State),
    % The intent activation is immediately experienced as an activation with status `relevant` (just a marker, no operational significance).
    % In the next timeframe, a plan for it will be found because the intent has no plan yet.
    activation_experience(Intent, relevant, CA, IntentActivationExperience),
    !,
    log(info, act, "(~w) Intent ~p created", [CA, Intent]).

% Find the experience starting with the most intensely felt (absolute feeling scaled by confidence).
% When there is a tie, choose one randomly.
% Non-deterministic
experience_to_impact(CA, State, Experience) :-
    findall(Exp, (member(Exp, State.experiences), is_experience_impactable(Exp)), ImpactableExperiences),
    experiences_sorted_by_intensity(ImpactableExperiences, SortedExperiences),
    % Choose an experience starting from the most intense
    member(Experience, SortedExperiences),
    log(info, act, "(~w) Experience to impact is ~p", [CA, Experience]).

% Create a goal from an experience to impact
% Determine what kind of impact based on whether the experience is good vs bad
goal_from_experience(Experience, IntentLevel, Age, Goal) :-
    experience{origin:Object, kind:Kind, value:Value} :< Experience,
    % The target of a goal is the experienced property/relation that was synthesized from observations (of umwelt experiences)
    Target = target{origin:Object, kind:Kind, value:Value},
    desired_impact(Experience, Impact),
    % The goal's priority is the intensity of the felt experience
    experience_intensity(Experience, Priority),
    % Create a goal without intent id
    Goal = goal{target: Target, impact: Impact, priority: Priority, intent_level: IntentLevel, timeframe_index: Age}, % TODO
    log(info, act, "The goal from ~p is ~p", [Experience, Goal]).

% Persist a good or neutral experience, and terminate a bad one.
desired_impact(Experience, Impact) :-
    Experience.feeling < 0 ->
        Impact = terminate
        ;
        Impact = persist.

is_intent_repeating(Intent, State) :-
    get_state(State, timeframes, Timeframes),
    % iterate over timeframes from latest until an intent is found
    member(Timeframe, Timeframes),
    is_goal(Timeframe.intent),
    !,
    % Are they the same?
    same_goals(Intent, Timeframe.intent).

%% PLAN EXECUTION

% TODO
% If the intent has a plan and the intent activation is experienced as `planned`, execute it.
% If a directive is predicted as executed and is experienced as `planned`, execute it.
plan_executed(CA, State, StateDeltas, WellbeingDelta) :-
    intent_plan_executed(CA, State, StateDeltas, WellbeingDelta).

plan_executed(CA, State, StateDeltas, WellbeingDelta) :-
    directive_plan_executed(CA, State, StateDeltas, WellbeingDelta).

intent_plan_executed(CA, State, StateDeltas, WellbeingDelta) :-
    get_state(State, intent, Intent),
    is_goal(Intent),
    has_goal_activation_status(Intent, State, planned),
    plan_for_goal(Intent, State, Plan),
    plan_execution(Plan, Intent, CA, State, StateDeltas, WellbeingDelta).

has_goal_activation_status(Goal, State, Status) :-
    goal_id(Goal, GoalId),
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    experience{origin:Object, kind:activation, value:Status} :< Experience,
    object{type:goal, id:GoalId} :< Object.

plan_for_goal(Goal, State, Plan) :-
    get_state(State, plans, Plans),
    member(Plan, Plans),
    same_goals(Goal, Plan.goal).

% Find a prediction received that a directive's activation status is `executed`.
% Find the plan for the directive
% Verify that the directive's activation is experienced as `planned`
% Find a (sub)directive in the plan that is observed as `planned` and predict it as `executed`
directive_plan_executed(CA, State, [predictions_out=[Prediction]], WellbeingDelta) :-
    is_directive_activation_predicted_as(Directive, State, executed),
    plan_for_goal(Directive, State, Plan),
    has_goal_activation_status(Directive, State, planned),
    member(SubDirective, Plan.directives),
    is_directive_observed_as(SubDirective, State, planned),
    directive_predicted_as(SubDirective, CA, State, executed, Prediction),
    % TODO - decrease fullness
    wellbeing:empty_wellbeing(WellbeingDelta).

%! is_directive_activation_predicted_as(?Directive, ++State, ?Status)
is_directive_activation_predicted_as(Directive, State, Status) :-
    get_state(State, predictions_in, Predictions),
    member(Prediction, Predictions),
    prediction{origin:Object, kind:activation, value:Status} :< Prediction,
    object{type:goal, evidence:[Directive]} :< Object.

%! is_directive_observed_as(++Directive, ++State, ?Status)
is_directive_observed_as(Directive, State, Status) :-
    goal_id(Directive, GoalId),
    get_state(State, observations, Observations),
    member(Observation, Observations),
    observation{origin:Object, kind:activation, value:Status} :< Observation,
    object{type:goal, id:GoalId} :< Object.

%! directive_predicted_as(++Directive, ++State, ++Status, --Prediction)
% An activation prediction about a directive is sent to the CA's umwelt
directive_predicted_as(Directive, CA, State, Status, Prediction) :-
    get_state(State, umwel, Umwelt),
    prediction_of_activation(Directive, CA, Status, Prediction),
    predictions_sent_to_umwelt(CA, Umwelt, [Prediction]).

% If level == 1
% Progress is made by executing at once all commands in the goal's plan, if found
% Publish `executed` for the plan's goal
plan_execution(Plan, Goal, CA, _, [experiences=[ActivationExperience]], WellbeingDelta) :-
    dynamic_ca : level_from_name(CA, Level),
    Level == 1,
    !,
    forall(Command, (member(Command, Plan.directives), command_actuation_readied(Command))),
    actuations_executed_by_body(),
    forall(Command, (member(Command, Plan.directives), command_actuation_executed(Command))),
    activation_experience(Goal, executed, CA, ActivationExperience),
    % TODO - decrease fullness
    wellbeing:empty_wellbeing(WellbeingDelta).

% Get the body host from the agency supervisor
% Tell the body to execute all pending actions
actuations_executed_by_body() :-
	query_answered(agency, option(body_host), Host),
  	body : actions_executed(Host).

% Tell an effector CA to ready execution of its action
command_actuation_readied(Command) :-
    message_sent(Command.effector_ca, ready_actuation(Command)).

% Tell an effector CA to execute its action (it tells the body to get the action reeady for execution)
command_actuation_executed(Command) :-
    message_sent(Command.effector_ca, execute_actuation(Command)).

%%% FINDING A PLAN

% One plan to find, either an affordance or a new plan
% The affordance must be scored high enough to be chosen over buidling a new plan.
%! plan_found_for_goal(++Directive, ++CA, ++State, ?Plan) is semidet
plan_found_for_goal(Goal, CA, State, Plan, WellbeingDelta) :-
    (affordance_plan(Goal, CA, State, Plan),
    % Reusing an affordance is free
    wellbeing:empty_wellbeing(WellbeingDelta)
    ;
    new_plan(Goal, CA, State, Plan),
    % Building a plan costs fullness but increases engagement
    wellbeing_delta_from_building_plan(Plan, WellbeingDelta) 
    ),
    !.

affordance_plan(Goal, CA, State, Plan) :-
    candidate_affordances_for(Goal, State, Affordances),
    selected_affordance_plan(Affordances, Plan),
    log(info, act, "(~w) Affordance plan ~p selected for goal ~p", [CA, Plan, Goal]).

candidate_affordances_for(Goal, State, CandidateAffordances) :-
    get_state(State, affordances, Affordances),
    findall(Affordance, (member(Affordance, Affordances), is_candidate_affordance(Affordance, Goal)), CandidateAffordances).

% An affordance for a goal is a candidate if it has a score
is_candidate_affordance(Affordance, Goal) :-
    Score = Affordance.score,
    Score \= none,
    same_goals(Goal, Affordance.plan.goal).

plan_score(Affordance, Score) :-
    Score = Affordance.score.

% Use the highest scored plan as long as its score is > 0.5 (ties are randomized)
% TODO - adjust minimal score based on settings and wellbeing
selected_affordance_plan(Affordances, Plan) :-
    map_list_to_pairs(plan_score, Affordances, ScoredAffordances),
    % To randomize ties
    random_permutation(ScoredAffordances, ScoredAffordances1),
    keysort(ScoredAffordances1, SortedScoredAffordances),
    % Highest score first
    reverse(SortedScoredAffordances, [Score-Affordance | _]),
    Score > 0.5,
    Plan = Affordance.plan.

% Find a plan that is not a scored affordance
new_plan(Goal, CA, State, Plan) :-
    log(info, act, "(~w) Making new plan for ~p", [CA, Goal]),
    goal{target:Target, impact:Impact} :< Goal,
    plan_for_goal(Target.kind, Impact, Goal, State, Plan),
    \+ is_scored_affordance_plan(State, Plan),
    log(info, act, "(~w) New plan ~p for goal ~p", [CA, Plan, Goal]).

is_scored_affordance_plan(State, Plan) :-
    get_state(State, affordances, Affordances),
    member(Affordance, Affordances),
    Affordance.score \= none,
    same_plans(Affordance.plan, Plan).

% The CA needs a plan for the goal to persist a `count` experience
% This can be done by persisting the full evidence for that experience (kin umwelt experiences observed and counted)
plan_for_goal(count, persist, Goal, State, Plan) :-
    ObservationIds = Goal.target.origin.evidence,
    directives_to_impact_all_observations(ObservationIds, Goal, persist, State, Directives),
    Plan = plan{goal:Goal, directives:Directives}.

% The CA needs a plan for the goal to persist a `count` experience
% This can be done by terminating any portion of the evidence for that experience (kin umwelt experiences observed and counted)
plan_for_goal(count, terminate, Goal, State, Plan) :-
    ObservationIds = Goal.target.origin.evidence,
    members(SomeObservationIds, ObservationIds),
    directives_to_impact_all_observations(SomeObservationIds, Goal, terminate, State, Directives),
    Plan = plan{goal:Goal, directives:Directives}.

% The CA needs a plan for the goal to persist a `more` experience
% A `more` experience is a relation where the origin's evidence outnumbers the value object's evidence.
% Persisting can be done by persisting all origin evidence/observations and terminate 1 or more of value object evidence/observations
plan_for_goal(more, persist, Goal, State, Plan) :-
    MoreObservationIds = Goal.target.origin.evidence,
    FewerObservationIds = Goal.target.value.evidence,
    directives_to_impact_all_observations(MoreObservationIds, Goal, persist, State, Directives1),
    random_subset(FewerObservationIds, EvenFewerObservationIds),
    \+ length(EvenFewerObservationIds, 0),
    directives_to_impact_all_observations(EvenFewerObservationIds, Goal, terminate, State, Directives2),
    append(Directives1, Directives2, AllDirectives),
    random_permutation(AllDirectives, Directives),
    Plan = plan{goal:Goal, directives:Directives}.
    
% The CA needs a plan for the goal to terminate a `more` experience
% A `more` experience is a relation where the origin's evidence outnumbers the value object's evidence.
% Terminating can be done by persisting all value evidence/observations and terminating enough origin object evidence/observations so there aren't more.
plan_for_goal(more, terminate, Goal, State, Plan) :-
    MoreObservationIds = Goal.target.origin.evidence,
    FewerObservationIds = Goal.target.value.evidence,
    length(MoreObservationIds, MoreCount),
    length(FewerObservationIds, FewerCount),
    MinTerminate is MoreCount - FewerCount,
    random_between(MinTerminate, MoreCount, TerminateCount),
    random_take(TerminateCount, MoreObservationIds, SomeObservationIds),
    directives_to_impact_all_observations(SomeObservationIds, Goal, terminate, State, Directives),
    Plan = plan{goal:Goal, directives:Directives}.

% When the CA needs a plan to persist a `trend` experience.
%   If the trend value is up, we want a next observation with a value greater than the latest Observation.
%   If the trend value is down, we want a next observation with a value lesser than the latest Observation.
%   If the trend value is steady, we want a next observation with the same value as that of the latest Observation. <== TODO
% When the CA needs a plan to terminate a `trend` experience.
%   If the trend value is up, we want a next observation with a value lesser or equal to that of the latest Observation.
%   If the trend value is down, we want a next observation with a value greater or equal to that of the latest Observation.
%   If the trend value is steady, we want a next observation with a value different from that of the latest Observation. <== TODO
plan_for_goal(trend, Impact, Goal, State, Plan) :-
    TrendValue = Goal.target.value,
    [_, LatestObservation] = Goal.target.origin.evidence,
    plan_for_trend_goal(Impact, TrendValue, LatestObservation, Goal, State, Plan).
    
% We want a next observation with a value greater than that of the latest Observation
plan_for_trend_goal(Impact, Trend, LatestObservation, Goal, State, Plan) :-
    member(Impact-Trend, [persist-up, terminate-down, terminate-steady]),
    Value = Goal.target.value,
    inc_simple_count(Value, IncValue),
    NextObservation = LatestObservation.put(value, IncValue),
    IncValue \= Value,
    directives_from_observation(NextObservation, create, Goal, State, Directives),
    Plan = plan{goal:Goal, directives:Directives}.

% We want a next observation with a value lesser than that of the latest Observation
plan_for_trend_goal(Impact, Trend, LatestObservation, Goal, State, Plan) :-
    member(Impact-Trend, [persist-down, terminate-up, terminate-steady]),
    Value = Goal.target.value,
    dec_simple_count(Value, DecValue),
    DecValue \= Value,
    NextObservation = LatestObservation.put(value, DecValue),
    directives_from_observation(NextObservation, create, Goal, State, Directives),
    Plan = plan{goal:Goal, directives:Directives}.

% We want a next observation with the same value as that of the latest Observation
plan_for_trend_goal(Impact, Trend, LatestObservation, Goal, State, NewPlan) :-
    member(Impact-Trend, [terminate-up, terminate-down, persist-steady]),
    directives_from_observation(LatestObservation, persist, Goal, State, Directives),
    NewPlan = plan{goal:Goal, directives:Directives}.

directives_to_impact_all_observations([], _, _, _, []).
directives_to_impact_all_observations(ObservationIds, Goal, Impact, State, AllDirectives) :-
    log(info, act, "Finding directives to ~w observations ~p given goal ~p", [Impact, ObservationIds, Goal]),
    findall(Observation, (member(ObservationId, ObservationIds), observation_from_id(ObservationId, State, Observation)), Observations),
    log(info, act, "The observations to ~w are ~p", [Impact, Observations]),
    findall(Directives, (member(Observation, Observations), directives_from_observation(Observation, Impact, Goal, State, Directives)), AllDirectives1),
    flatten(AllDirectives1, AllDirectives),
    log(info, act, "The directives are ~p", [AllDirectives]).

observation_from_id(ObservationId, State, Observation) :-
    get_state(State, observations, Observations),
    member(Observation, Observations),
    ObservationId == Observation.id.

% Directives from a synthetic observation
directives_from_observation(Observation, Impact, FromGoal, _, [Directive]) :-
    \+ is_sensory_observation(Observation),
    observation_target(Observation, Target),
    Directive = goal{target: Target, impact: Impact, priority: FromGoal.priority, intent_level: FromGoal.intent_level}.

% Commands to impact a sensory observation, i.e. observation of a sensor CA experience.
% Without a scored affordance, the CA does not know (yet) a sequence of effector actions (a movement)
% that will produce the desired impact on the observed sensory experience. So it has to guess.
directives_from_observation(Observation, _, _, State, Commands) :-
    is_sensory_observation(Observation),
    get_state(State, umwelt_commands, UmweltCommands),
    movement(UmweltCommands, Movement),
    random_permutation(Movement, Commands).      

% From all commands supported by the CA's umwelt, generate a list of commands (0 or 1-3 of each) and randomly permute.
movement([], []).

movement([Command | Rest], [Group | OtherGroups]) :-
    command_group(Command, Group),
    movement(Rest, OtherGroups).

% 50% of the time, don't include the command
% Otherwise have it be repeated 1 to 3 times
command_group(Command, CommandGroup) :-
    random_between(0, 1, R),
    (R == 0 ->
        CommandGroup = []
        ;
        random_between(1, 3, N),
        replicate(Command, N, CommandGroup)
    ).

wellbeing_delta_from_building_plan(Plan, WellbeingDelta) :-
    wellbeing:empty_wellbeing(EmptyWellbeing),
    length(Plan.directives, N),
    % TODO - Get the fullness cost of a new plan from settings
    Delta is N * 0.01,
    % Planning is engaging
    WellbeingDelta1 = EmptyWellbeing.add_engagement(Delta),
    % Planning is exhausting
    WellbeingDelta = WellbeingDelta1.subtract_fullness(Delta).

