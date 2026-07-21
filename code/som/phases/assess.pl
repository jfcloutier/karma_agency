/*
In the `assess` phase, a CA

* moves executed plans to unscored affordances
* abandons its current intent (current self-assigned goal) if it is stale or stalled
* abandons plans if they are stale or stalled
* scores affordances (remembered executed plans) to reflect the likelihood of the plans being responsible for achieving their goals
* forgets old or useless affordances
* decides whether to get an initial or a replacement causal theory
* decides how much of its wellbeing fullness to diffuse to its entourage (umwelt and parents)
* asks the SOM to carry out the next life event, if any (apoptosis, replication, division, or simply going on)
*/

% plan{goal: goal{...}, directives: [goal{...} | Action, ...], age:Age}
% goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_level: Level}
% experience{origin:Object, kind:Kind, value:Value, confidence:Confidence, feeling:Feeling, by:CA}
% target{origin: Origin, kind: Kind, value: Value}
% object{type:synthetic, id:Id, evidence: [ObservationId, ...]}

:- module(assess, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(som/wellbeing)).
:- use_module(agency(som/phase)).
:- use_module(agency(som/ca_support)).
:- use_module(agency(som/observations)).
:- use_module(agency(som/affordances)).

% No work done before units of work
before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).


% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDelta) or done(StateDeltas, WellbeingDelta) as last solution. 
unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
  add_affordances(CA,State, StateDeltas0, WellbeingDelta0),
  assess_intent(CA, State, StateDeltas0, WellbeingDelta0, StateDeltas1, WellbeingDelta1),
  assess_plans(CA, State, StateDeltas1, WellbeingDelta1, StateDeltas2, WellbeingDelta2),
  assess_affordances(CA, State, StateDeltas2, WellbeingDelta2, StateDeltas3, WellbeingDelta3),
  assess_causal_theory(CA, State, StateDeltas3, WellbeingDelta3, StateDeltas4, WellbeingDelta4),
  healing(CA, State, WellbeingDelta4, WellbeingDelta5),
  diffuse_fullness(CA, State, StateDeltas4, WellbeingDelta5, StateDeltas5, WellbeingDelta6),
  next_life_event(CA, State, StateDeltas5, WellbeingDelta6, StateDeltas, WellbeingDelta),
  log(info, assess, "Phase assess ended for CA ~w", [CA]).

% First step.
% Add each executed plan as an unscored affordance (no duplicate) and forget it
add_affordances(_, State, StateDeltas, WellbeingDelta) :-
  wellbeing:empty_wellbeing(WellbeingDelta),
  executed_plans(State, ExecutedPlans),
  (length(ExecutedPlans, 0) ->
  get_state(State, plans, Plans),
    StateDeltas = [plans - Plans]
    ;
    get_state(State, plans, Plans),
    findall(Affordance, (member(Plan, ExecutedPlans), affordance(Plan, none, Affordance)), Affordances),
    subtract(Plans, ExecutedPlans, RemainingPlans),
    StateDeltas = [affordances - Affordances, plans - RemainingPlans]
  ).

executed_plans(State, ExecutedPlans) :-
  get_state(State, plans, Plans),
  findall(Plan, (member(Plan, Plans), is_plan_executed(Plan, State)), ExecutedPlans).

is_plan_executed(Plan, State) :-
  GoalId = Plan.goal.id,
  get_state(State, experiences, Experiences),
  member(Experience, Experiences),
  experience{origin:Object, kind:activation, value:executed} :< Experience,
  object{type:goal, id:GoalId} :< Object.

assess_intent(_, State, StateDeltas, WellbeingDelta, NewStateDeltas, WellbeingDelta) :-
  get_state(State, intent, Intent),
  (is_intent_retained(Intent, State) ->
    NewStateDeltas = StateDeltas
    ;
    merge_options(StateDeltas, [intent - none], NewStateDeltas)
  ).

is_intent_retained(none, _).

% Retain an intent to persist or terminate an experience if the experience exists
is_intent_retained(Intent, State) :-
  goal{impact:Impact} :< Intent,
  memberchk(Impact, [persist, terminate]),
  get_state(State, experiences, Experiences),
  member(Experience, Experiences),
  experience_matches_goal(Experience, Intent).

% Retain an intent to create an experience if it does not exist
is_intent_retained(Intent, State) :-
  goal{impact:create} :< Intent,
  get_state(State, experiences, Experiences),
  \+ (member(Experience, Experiences), experience_matches_goal(Experience, Intent)).

experience_matches_goal(Experience, Goal) :-
  experience{origin:Origin, kind:Kind, value:Value} :< Experience,
  target{origin:TargetOrigin, kind:Kind, value:TargetValue} :< Goal.target,
  matching_values(Kind, Value, TargetValue, State),
  matching_objects(Kind, Origin, TargetOrigin, State).

% If objects synthesized from a time series of observations are being matched, the ones from the experience must expand on the one from the target
matching_objects(trend, Object, TargetObject, _) :-
  expands_on(Object.evidence, TargetObject.evidence).

% If objects synthesized from `count` observations are being matched, what's counted must be commensurable
% If objects synthesized from sensory observations are being matched, readings must be from the sames sense
matching_objects(more, Object, TargetObject, State) :-
  [ObservationId] = Object.evidence,
  [TargetObservationId] = TargetObject.evidence,
  observation_from_id(ObservationId, State, Observation),
  observation_from_id(TargetObservationId, State, TargetObservation),
  matching_observations(more, Observation, TargetObservation).

% Objects in experiences other than `more` or `trend` match on equality
matching_objects(Kind, Object, Object, _) :-
  \+ memberchk(Kind, [more, trend]).

% Values of `more` experiences/targets are objects each synthesized from a numerically-valued observation (`count` or a sense reading)
matching_values(more, Object, TargetObject, State) :-
  matching_objects(more, Object, TargetObject, State).

% From any other kinds, values must be equal to match
matching_values(Kind, Value, Value, _) :-
  Kind \= mode.

matching_observations(more, Observation, TargetObservation) :-
  observation{kind:Count, evidence:Evidence} :< Observation,
  observation{kind:Count, evidence:TargetEvidence} :< TargetObservation,
  commensurable(Evidence, TargetEvidence).

matching_observations(more, Observation, TargetObservation) :-
  observation{kind:Kind, value:Value} :< Observation,
  Kind \= count,
  observation{kind:Kind, value:TargetValue} :< TargetObservation,
  number(Value),
  number(TargetValue).

commensurable(Evidence, TargetEvidence) :-
  (expands_on(Evidence, TargetEvidence) ; expands_on(TargetEvidence, Evidence)).

% Evidence (a set of observation ids) is a non-strict superset of other evidence.
expands_on(Evidence, OtherEvidence) :-
  forall(member(Id, Evidence), memberchk(Id, OtherEvidence)).

matching_observed_objects(Kind, Object, Object, _) :-
  \+ memberchk(Kind, [more, trend]).

assess_plans(_, State, StateDeltas, WellbeingDelta, NewStateDeltas, WellbeingDelta) :-
  option(plans(Plans), StateDeltas),
  stalled_plans(Plans, State, StalledPlans),\
  subtract(Plans, StalledPlans, RemainingPlans),
  (dropped_intent_plan(State, StateDeltas, IntentPlan) ->
    delete(RemainingPlans, IntentPlan, SurvivingPlans)
    ;
    SurvivingPlans = RemainingPlans
  ),
  % Merge old StateDeltas into new one (new overrides old)
  merge_options([plans = SurvivingPlans], StateDeltas, NewStateDeltas).

stalled_plans(Plans, State, StalledPlans) :-
  findall(StalledPlan, (member(StalledPlan, Plans), plan_stalled(StalledPlan, State)), StalledPlans).

% A plan is stalled if it has been waiting execution for too long.
plan_stalled(Plan, State) :-
  get_state(State, age, Age),
  \+ memberchk(Plan.status, [executing, executed]),
  Age is Age - Plan.age,
  % TODO - get constant from settings
  Age > 5.

dropped_intent_plan(State, StateDeltas, Plan) :-
  option(intent(none), StateDeltas),
  get_state(State, intent, Intent),
  Intent \= none,
  get_state(State, plans, Plans),
  member(Plan, Plans),
  Plan.goal == Intent.

% For each unscored affordance, determine if its plan's goal is currently achieved,
% then give it a score proportional to the time elapsed between execution and goal achievement.
assess_affordances(_, State, StateDeltas, WellbeingDelta, NewStateDeltas, WellbeingDelta) :-
  get_state(State, affordances, Affordances),
  findall(Affordance1, (member(Affordance, Affordances), maybe_scored_affordance(Affordance, State, Affordance1)), NewAffordances),
  % Merge old StateDeltas into new one (new overrides old)
  merge_options([affordances - NewAffordances], StateDeltas, NewStateDeltas).

maybe_scored_affordance(Affordance, State, Affordance1) :-
  is_affordance_scored(Affordance) ->
    Affordance1 = Affordance
    ;
    evaluated_affordance(Affordance, State, Affordance1).

% If the affordance plan's goal is currently achieved,
% give it a score proportional to the time elapsed between execution and goal achievement, else leave it unscored.
evaluated_affordance(UnscoredAffordance, State, Affordance) :-
  Goal = UnscoredAffordance.plan.goal,
  get_state(State, experiences, Experiences),
  is_goal_achieved(Goal, Goal.impact, Experiences) ->
    get_state(State, age, Age),
    PlanAge = UnscoredAffordance.plan.age,
    Score is 1.0 / max(1, Age - PlanAge),
    Affordance = UnscoredAffordance.put(score, Score)
    ;
    Affordance = UnscoredAffordance.

% The persist or create goal is achieved if it matches an experience
is_goal_achieved(Goal, Impact, Experiences) :-
  memberchk(Impact, [persist, create]),
  member(Experience, Experiences), 
  experience_matches_goal(Experience, Goal).

% The terminate goal is achieved if it does not match an experience
is_goal_achieved(Goal, terminate, Experiences) :-
    \+ (member(Experience, Experiences), experience_matches_goal(Experience, Goal)).
  
% TODO - Do nothing for now
assess_causal_theory(CA, State, StateDeltas, WellbeingDelta, StateDeltas, WellbeingDelta).

% If integrity is diminished and fullness is available for healing, use some of it to increase integrity (integrity gained == fullness lost)
healing(_, State, WellbeingDelta, NewWellbeingDelta) :-
  get_state(State, wellbeing, Wellbeing),
  apply_wellbeing_delta(Wellbeing, WellbeingDelta, CurrentWellbeing),
  integrity_gain(CurrentWellbeing, IntegrityGain),
  FullnessLoss is 0.0 - IntegrityGain, 
  NewWellbeingDelta = WellbeingDelta.add(wellbeing{fullness:FullnessLoss, integrity:IntegrityGain, engagement:0.0}).

% If integrity is lower than 1.0 and fullness can be spent, spend it to increase integrity.
% The goal is to avoid death from having exhausted either integrity or fullness.
% There is a maximum delta integrity (maximum rate of self-repair)
integrity_gain(CurrentWellbeing, IntegrityGain) :-
  MaximumHealing = 0.1, % TODO get from settings
  IntegrityDeficit  is 1.0 - CurrentWellbeing.integrity,
  MaxHealing is min(IntegrityDeficit, MaximumHealing),
  MinimumFullnessBeforeHealing = 0.2, % TODO get from settings
  FullnessAvailable is max(0, CurrentWellbeing.fullness - MinimumFullnessBeforeHealing),
  IntegrityGain is min(MaxHealing, FullnessAvailable).

% Incremental (i.e. partial), outward wellbeing fullness osmosis with direct entourage (parents and umwelt)
% Given the latest wellbeing status events received (wellbeings_in)
diffuse_fullness(_, State, StateDeltas, WellbeingDelta, StateDeltas, NewWellbeingDelta) :-
  get_state(State, wellbeing, Wellbeing),
  apply_wellbeing_delta(Wellbeing, WellbeingDelta, CurrentWellbeing),
  fullness_donations(CurrentWellbeing.fullness, State, FullnessDonations),
  fullness_donated(FullnessDonations, AllSentFullness),
  wellbeing_delta_from_donations(AllSentFullness, WellbeingDelta, NewWellbeingDelta).

% How much of the shareable wellbeing is aportioned to each needy entourage CA.
fullness_donations(Fullness, State, FullnessDonations) :-
  fullness_demands(Fullness, State, FullnessDemands),
  total_demand(FullnessDemands, TotalDemand),
  rationed_donations(FullnessDemands, TotalDemand, Fullness, FullnessDonations).

% Find the positive deltas (demand) between the CA's fullness and each received each wellbeing report (how much more fullness the CA has than an entourage CA).
fullness_demands(Fullness, State, FullnessDemands) :-
  get_state(State, wellbeings_in, WellbeingReports),
  findall(FullnessDemand,
    (member(WellbeingReport, WellbeingReports),
    WellbeingReport =.. [CA, CAWellbeing],   % A wellbeing report is an option CA(Wellbeing)
    FullnessDiff is max(0, Fullness - CAWellbeing.fullness), % constrained to fullness diff >= 0 - how much more fullness does the CA have (0 if not)
    FullnessDiff > 0,
    FullnessDemand = CA-FullnessDiff), % A demand is pair CA-FullnessDiff 
    FullnessDemands).

  total_demand(FullnessDemands, TotalDemand) :-
    pairs_values(FullnessDemands, FullnessDiffs),
    foldl(plus, FullnessDiffs, 0, TotalDemand).

% Calculate the ratio of how much fullness could be donated vs the aggregated demand.
donation_ratio(FullnessDemands, Fullness, DonationRatio) :-
  pairs_values(FullnessDemands, FullnessDiffs),
  foldl(plus, FullnessDiffs, 0, TotalDemand),
  (TotalDemand == 0 ->
    DonationRatio = 0
    ;
    Generosity = 0.5, % TODO in settings - 
    MaxToDonate is Generosity * Fullness,
    DonationRatio is min(1.0, MaxToDonate / TotalDemand)
  ).

% Apply the ratio ratio to each demand to produce the donations as CA-Fullness pairs.
rationed_donations(FullnessDemands, TotalDemand, Fullness, FullnessDonations) :-
  Generosity = 0.5, % TODO in settings - 
  DonationBudget is Generosity * Fullness,
  findall(FullnessDonation, 
    (member(EntourageCA-FullnessDiff, FullnessDemands),
    FullnessDonation is (FullnessDiff / TotalDemand) *  DonationBudget,
    FullnessDonation = EntourageCA-FullnessDonation
    ),
    FullnessDonations
    ).

% Messages are sent to entourage CAs with their donated share of wellbeing.
fullness_donated(FullnessDonations, AllSentFullness) :-
  findall(Fullness, (member(EntourageCA-Fullness, FullnessDonations), message_sent(EntourageCA, fullness_transfer(Fullness))), AllSentFullness).

% Update wellbeing from fullness donations
wellbeing_delta_from_donations(AllSentFullness, WellbeingDelta, NewWellbeingDelta) :-
  foldl(plus, AllSentFullness, 0, TotalSent),
  NewWellbeingDelta = WellbeingDelta.sub(wellbeing{fullness:TotalSent, integrity:0, engagement:0}).

% TODO
next_life_event(CA, State, StateDeltas, WellbeingDelta, NewStateDeltas, NewWellbeingDelta).


