# Wellbeing

## About

Wellbeing is a collection of measures the agent strives to maximize. Wellbeing imparts normativity to beliefs, motivates activity, and focuses attention.

The measures are:

* **Fullness**: How much energy the agent has in store, to "pay for" cognitive effort, effector activation, and (pretend) maintenance and repair.
* **Integrity**: The "health" of the agent, helped by avoiding collisions and not "eating poison"
* **Engagement**: How much the agent is engaging its environment, both by making sense of it and by acting in it

The wellbeing measures are each a pair of numbers, a current value and a gradient.

The current value is an "how much" value computed from aggregating information communicated by the Cognition Actors about their computations, actions they take, bumps they suffer, food they "ingest" etc. The gradient value integrates how the current value is changing, that is, how sharply is the value going up or down.

The measures of wellbeing are centrally computed and then broadcasted to the "society of mind". They permeate the collective of Cognition Actors (CAs) and Meta-Cognition Actors (M-CAs).

Wellbeing provides the means by which *normativity* is associated with the beliefs of CAs. CAs are informed of current wellbeing levels and gradients, and correlate these with beliefs the CAs synthesize from current and past observations. Beliefs associated with high and static or with increasing wellbeing measures, are "pleasant" beliefs. Beliefs associated with low and static or with decreasing wellbeing measures are "unpleasant". Beliefs associated with neither are "neutral". Low and decreasing measures feel the worst whereas high and increasing measures feel the best.

Wellbeing drives attention: CAs intend actions to persist pleasant beliefs and actions to terminate unpleasant beliefs. Neutral beliefs are normally "left alone" by the CA but are kept as they may compose the non-neutral beliefs of more abstract CAs.

Wellbeing also provides the motivation for M-CAs to cull or add CAs so as to manage overall energy expenditure or nudge up overall engagement.

## Fullness

The fullness Wellbeing Actor maintains a global energy budget. It starts full.

An agent's collective of CAs is mortal. When there is no energy left, its mind dies (its Lego body is permanent).

Energy is replenished by the action of `eating` but only when the agent is positioned over food (the agent may or may not know it is over food).

Energy is spent individually by each CA when

* obtaining a causal theory (the more time spent searching, the higher the cost of finding the theory)
* synthesizing a belief (each belief comes at a cost)
* using a causal theory (the more complex the theory, the higher the cost per use)
* remembering past observations and beliefs (additional cost per remembered observation/belief)
* completing a time frame (baseline computational/metabolic cost)

Energy is also spent whenever the agent progressively and automatically recovers from lost integrity, i.e. repairs itself.

At the completion of a time frame, the CA emits an account of what it did in the time frame. Reports from CAs are collected and integrated by the fullness Wellbeing Actor into an updated, global value.

CAs and meta-CAs listen to fullness level events, broadcasted to the collective of CAs and M-CAs whenever the measure changes significantly.

In response to low and/or decreasing fullness,

A CA may:

* forget a past state (oldest first)
* forget a belief ignored by all parent CAs (neutral first)
* look for a less complex causal theory
* raise the threshold for persisting/terminating beliefs (don't waste energy on trivial matters!)

A M-CA may

* remove a "low-value" CA (low-engagement, low predictive accuracy) whose beliefs are ignored or redundant

## Integrity

The integrity Wellbeing Actor maintains a global integrity level. Integrity is initially full.

Integrity decreases because of harmful actions taken by the agent.

Lost integrity is recovered over time but at an energy cost proportional to the amount of recovery.

Integrity is reduced whenever

* the agent's touch sensor is activated, or
* the agent executes the action of `eating` over poison food (the agent may not know to distinguish perceptually between non-food, food and poison but its "metabolism" does).

CAs listen to integrity level events, broadcasted whenever the measure changes significantly.

In response to low integrity,

* A CA may raise the threshold at which it intends to impact its beliefs (it less easily triggered into action when unhealthy)

## Engagement

The engament wellbeing actor keeps track of how engaged the CAs are overall. Engagement is initially empty.

Engagement goes up whenever

* a CA's belief is used in the synthesis of another CA's belief (it becomes relevant), and
* whenever an action it intends is executed (it acts in the world).

Engagement decreases whenever

* a CA's belief loses relevance (it was the basis of another CA's belief that was removed), or
* with the passing of time when no action is executed

In response to overall low engagement,

* a M-CA may replace a CA in the hope that the new CA's sense-making trajectory will lead to relevance and to action-taking
* a CA may lower its threshold for persisting beliefs, to the point where even neutral beliefs are acted on (leading to babbling as a form of undirected engagement)
