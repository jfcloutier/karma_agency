# Wellbeing

Wellbeing is a collection of measures the agent strives to maximize. Wellbeing imparts normativity to beliefs, motivates activity, and focuses attention.

The measures are:

* **Fullness**: A measure of how much energy the agent has in store, to pay for "metabolic" and activity costs
* **Integrity**: A measure of the "health" of the agent, helped by avoiding collisions and not "eating poison"
* **Engagement**: A measure of how much the agent is engaging its environment, by making sense of it and acting in it

The wellbeing measures are numbers, one for each measure. Each measure aggregates information communicated by the Cognition Actors about their metabolic draw, actions they take etc.

The measures of wellbeing are centrally computed and then broadcasted. They permeate the collective of Cognition Actors (CAs) and Meta-Cognition Actors (M-CAs - actors responsible for each curating a level of abstraction within a hierarchy of CAs).

Wellbeing provides the context by which *normativity* is associated with the beliefs of CAs. CAs detect wellbeing levels and trends and correlate these with beliefs the CAs synthesize from current and past observations. Beliefs associated with high wellbeing measures are "pleasant" beliefs. Beliefs associated with low wellbeing measures are "unpleasant" beliefs. Beliefs associated with neither are "neutral" beliefs.

Wellbeing drives attention: CAs intend actions to validate pleasant beliefs and actions to invalidate unplesant beliefs. Neutral beliefs are normally "left alone" by the CA but are kept as they may compose non-neutral beliefs of more abstract CAs.

Wellbeing also provides the motivation for M-CAs to cull or add CAs so as to manage overall energy expenditures or nudge up overall engagement.

## Fullness

The fullness Wellbeing Actor maintains a global energy budget. It starts full.

An agent's collective of CAs is mortal; when there is no energy left, it dies.

Energy is replenished by the action of `eating` but only when the agent is positioned over food (the agent may or may not know it is over food).

Energy is spent individually by each CA when

* completing a time frame (baseline metabolic cost)
* remembering past states (additional cost per state beyond the current state)
* synthesizing a belief (each belief comes at a metabolic cost)
* obtaining a causal theory (the more time spent searching, the higher the cost of finding the theory)
* using a causal theory (the more complex the theory, the higher the cost per use)

Energy is also spent whenever the agent progressively and automatically recovers from lost integrity.

At the completion of a time frame, the CA emits an account of energy spent in the time frame. Costs reported by CAs are collected and integrated by the energy Wellbeing Actor.

CAs and meta-CAs listen to energy level events, broadcasted to the collective of CAs and M-CAs whenever the measure changes significantly.

In response to low energy,

* A CA may, subject to maintaining constraint closure (more about that in another post),
  * forget a past state (oldest first)
  * forget a belief (neutral first)
  * look for a less complex causal theory
  * raise the threshold for validating/invalidating beliefs (don't waste energy on trivial matters!)

* A M-CA may, also subject to maintaining constraint closure,
  * remove a "low-value" CA (low-engagement, low predictive accuracy)

## Integrity

The integrity Wellbeing Actor maintains a global integrity level. Integrity is initially full.

Integrity decrease because of harmful actions taken by the agent. Lost integrity is recovered over time but at an energy cost proportional to the amount of recovery.

Integrity is reduced whenever

* the agent's touch sensor is activated, or
* whenever the agent executes the action of `eating` poison food (the agent may not know to distinguish perceptually between non-food, food and poison but its "metabolism" does).

CAs listen to integrity level events, broadcasted whenever the measure changes significantly.

In response to low integrity,

* A CA may raise the threshold at which it intends to impact good, or bad beliefs (it less easily triggered into action when unhealthy)

## Engagement

The engament Wellbeing Actor keeps track of how engaged CAs are overall. Engagement is initially empty.

Engagement goes up whenever

* a CA's belief is used in the synthesis of another CA's belief (it becomes relevant), and
* whenever an action it intends is executed (it acts in the world).

Engagement decreases whenever

* a CA's belief loses relevance (it was the basis of another CA's belief that was removed), or
* with the passing of time when no action is executed

In response to overall low engagement,

* a M-CA may add a new CA, even though the hierarchy level might already be crowded, in the hope that its sense-making trajectory will lead to relevance and to action-taking
* a CA may lower its threshold for validating beliefs, to the point where even neutral beliefs are acted on (leading to babbling)
