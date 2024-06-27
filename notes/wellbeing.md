# Wellbeing

Wellbeing is a collection of agent-wide measures the agent strives to maximize. Wellbeing provides normativity and drives attention.

The measures are:

* Fullness: A measure of how much energy the agent has in store to pay for its metabolic and activity costs
* Integrity: A measure of the structural and metabolic health of the agent
* Engagement: A measure of how active the agent is at making sense of its environment

Wellbeing measures integrate information the Actors communicate about their metabolic draw and actions taken throughout their lifecycles.
The measures of wellbeing are centrally computed and broadcasted so that they permeate the collective of Cognition Actors (CAs) and Meta-Cognition Actors (M-CAs).

Wellbeing provides the context by which *normativity* is associated with the beliefs of CAs. CAs detect wellbeing trends and associate them with the beliefs they synthesize from current and past observation. Beliefs associated with positively trending wellbeing measures are "pleasant" beliefs. Beliefs associated with negatively trending wellbeing measures are "unpleasant" beliefs. Beliefs associated with neither are "neutral" beliefs.

Wellbeing drives attention: CAs intend actions to validate pleasant beliefs and actions to invalidate unplesant beliefs. Neutral beliefs are "left alone" by the CA, but may compose non-neutral beliefs of more abstract CAs.

Wellbeing also provides the motivation for M-CAs to cull or add CAs to manage overall metabolic expenditures or nudge up overall engagement.

Wellbeing is implemented by actors, one per measure. Each wellbeing actor listens to events broadcasted by CAs that are relevant to increasing or decreasing the wellbeing measure managed by the actor. Each wellbeing actor also broadcasts changes to its wellbeing measure. These broadcasts are listened to by all CAs and M-CAs.

## Fullness

The fullness wellbeing actor maintains a global energy budget to pay for the agent's metabolic and activity costs. It starts full.

An agent's collective of CAs is mortal; when there is no energy left, it dies.

Energy is replenished by the action of `eating` when the agent is positioned over food. It is spent globally whenever the agent recovers from lost integrity.

Energy is spent individually by each Cognition Actor when

* completing a time frame (baseline cost)
* remembering past states (additional cost per state beyond the current state)
* synthesizing a belief (each belief comes at a metabolic cost)
* obtaining a causal theory (the more time spent searching, the higher the cost of the theory)
* using a causal theory (the more complex the theory, the higher the cost per use)

At the completion of a time frame, the CA emits an account of energy spent. It is collected and integrated by the energy wellbeing actor.

CAs and meta-CAs listen to energy level events, broadcasted whenever the measure changes significantly.

In response,

* A CA may, as long as it does not break constraints,
  * forget a past state (oldest first)
  * forget a belief (neutral first)

* A M-CA may, as long as it does not break constraints,
  * remove a "low-value" CA

## Integrity

The integrity wellbeing actor maintains a global integrity level. Integrity is initially full. 

Integrity decrease because of harmful actions taken by the agent. Lost integrity is recovered with time but at an energy cost proportional to the amount of recovery.

Integrity is reduced whenever the agent's touch sensor is activated and whenever the agent executes the action of `eating` poison food (the agent may not know to distinguish perceptually between food and poison but its "metabolism" always does).

CAs listen to integrity level events, broadcasted whenever the measure drops significantly.

In response,

* A CA may raise the threshold at which it intends to impact good or bad beliefs (it less easily triggered into action when unhealthy)

## Engagement

The engament wellbeing actor keeps track of how engaged CAs are overall. Engagement is initially empty. 

Engagement goes up whenever a CA's belief is used in the synthesis of another CA's belief (it becomes relevant) and whenever an action it intends is executed (it acts in the world).

Engagement decreases whenever a CA's belief loses relevance (another CA's that was belief based on it is removed), or with the passing of time if no action is executed.

In response,

* A M-CA may duplicate a low-engagement CA (start a new CA with the same umwelt) in the hope that its sense-making trajectory will be different and lead to more relevance
* A CA may lower its threshold for validating beliefs, to the point where neutral beliefs are acted on (babbling)
