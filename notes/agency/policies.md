# Policies

A Cognition Actor (CA) seeks to persist its pleasant beliefs and to terminate its unpleasant beliefs through action policies.

## Recap

A CA is part of a hierarchichal collective of CAs that animate an agent. Each CA has an umwelt composed of a small number of CAs from the level below. A CA synthesizes its beliefs from predicted observations, past and current, of the beliefs in its umwelt.

The beliefs of a CA exist in the context of wellbeing measures, current and trending. The wellbeing measures are fullness, integrity and engagement. They signal risks, present or absent, to the survivability of the entire collective of CAs. Beliefs associated with high risks are unpleasant. Beliefs associated with low risks are pleasant. Others are neutral. A CA acts on its umwelt in response to the pleasantness or unpleasantness of its current beliefs.

A CA operates one time frame after another. Each time frame corresponds to a "thick now". The time frame of a CA starts and stops independently of other CAs; time frames are not synchronized. CAs higher up in the collective's hierarchy have longer time frames. During its current time frame, a CA makes new observations by predicting the latest beliefs in its umwelt, reads the broadcasted global wellbeing measures, refreshes its set of beliefs by integrating the latest observations with past ones, and assigns up-to-date normative values (pleasantness/unpleasantness) to its updated beliefs.

At the end of the current time frame, a CA decides whether to act on its updated beliefs and, if so, how.

## Intents

A CA can at most *intend* to take actions to impact its beliefs; an intent is not guaranteed to be realized. After all, a CA participates in a collective and there is only one agent that the collective animates. Simultaneous intents to alter beliefs coming from multiple CAs must be resolved so that only one course of action, hopefully beneficial to all CAs, is carried out by the agent at any point in time.

An intent by a CA consists of

* a goal - the validation of a pleasant belief or the invalidation of an unpleasant belief held by the CA
* a priority - determined by the graded normativity of the belief targeted by the intent
* a policy - how to achieve the goal by validating/invalidating a set of observed beliefs held by the CA's umwelt CAs (and on which the targeted belief depends)

A policy is effectively a list of sub-goals to be delegated to the umwelt CAs. The policy's sub-goals are communicated all at once to the CA's umwelt at the end of the CA's current time frame.

A CA constructing an intent around a goal will first look up in past time frames for policies it has already tried, if any, to accomplish the goal. If it remembers one that was repeatedly successful, it will try it again out of habit, else it will construct a new one.

Repeated intents builds up habits whenever realizing an intent via a given policy is usually successful.

## Attention and action thresholds

The first step to composing an intent is to identify a belief to attend to (assuming a parent CA has not imposed one - more on this later).

Which belief is identified for action depends on the normativity of the belief and whether or not it is *sufficiently* pleasant or unpleasant (the normativity threshold) for the CA to pay attention to it.

The normativity threshold for action is set by the current wellbeing context which is shared by the entire collective. In a low fullness context (depleted energy stores), a CA raises the threshold for action (conserve energy!), whereas, when energy is plentiful and agent engagement is low, a CA lowers the threshold for action (try doing something, anything!). Only a belief with normativity above threshold is deemed worthy of action.

The CA selects which belief, if any, is *most worthy* of action in the current time frame. Invalidating an unpleasant belief it cares about takes precedence over validating a pleasant belief. Typically, the CA will intend to terminate the most unpleasant, action-worthy belief. Else it will intend to persist the most pleasant, action-worthy belief, or do neither if there is no action-worthy belief.

## Constructing a policy

Once a CA has identified (or been told of) a belief to persist or terminate, the CA must determine a policy to achieve this goal. The composition of the policy depends on the nature of the belief (abduction, count, trend or ending) and on the desired impact (validation/perpetuation or invalidation/termination).

It also depends on the *base of support* of the belief to be impacted; a belief is synthesized from observations (its base of support) which are themselves beliefs once or currently held in the CA's umwelt. To impact a belief a CA holds, the CA needs to impact supporting beliefs held by CAs in its umwelt.

Unless it only just got started, a CA will have a causal theory it then uses to predict the beliefs of its umwelt (i.e. incoming observations) as a result of taking or not taking certain actions. This can also be put to use when shaping a policy.

A causal theory has three kinds of rules

* Constraints: What is expected *not* to be observed simultaneously - e.g. not (A and B)
* Static rules: What is expected to be observed simultaneoulsy - e.g. if A and B and then also C
* Causal rules: What is expected to be observed given what was last observed - e.g. A and B causes C

(An additional but implied rule is that a previous observation survives as a current observation if it does not terminate any constraint or static rule.)

A belief is a synthesis of multiple observations, either made in the current time frame (abduction or count belief) or across past and current time frames (trend and ending beliefs). These observations constitute the base of support of the belief targeted for impact by the intent. To impact a belief (to persist or terminate it), the CA will want to impact all or some of the supporting observations. These observations being themselves beliefs held in the umwelt of the CA, one can see how acting becomes a recursive operation that will unfold across multiple layers of CAs and over multiple CA time frames.

The task of constructing a policy -in order to impact a belief held by the CA- consists in deciding which supporting beliefs (observed umwelt beliefs) to impact and how. There will likely be many possible combinations of supporting beliefs to impact and thus there could be many alternate policies to choose from. The causal theory, which models the generative process causing these observations, informs the CA in the construction of hopefully effective policies.

The CA could impact a supporting belief either *directly* or *indirectly*.

* Directly: By requesting its umwelt to alter the supporting belief itself
* Indirectly: By requesting its umwelt to impact one or more of the beliefs that are causally related to the supporting belief (contingent beliefs according to the CA's causal theory).

Let's says a CA wants to impact an observation A that supports a belief it intends to persist or terminate. It could do so directly but also indirectly by impacting observation B if

* A and B are mutually exclusive (according to the CA's causal theory)
* A and B must co-exist under some condition
* B causes A under some condition

How a CA could impact a held belief is further modulated by the kind of belief it is (abduction, count, trend or ending belief).

### Impacting an abduction belief

An *abduction* belief can not be impacted. This would be tantamount to neurosis. The abduced beliefs are unchangeable realit, until a new causal model is acquired that is without them.

### Impacting a count belief

A *count* belief encapsulates how many of a given kind of observed beliefs there are in the current time frame (for e.g., self is next to 2 objects)

To persist a count belief, a CA's policy would direct its umwelt CAs to persist *all of* the counted (observed) beliefs, directly or indirectly.

To terminate a count belief, a CA's policy would direct its umwelt CAs to terminate *any of* the counted beliefs, directly or indirectly, or to add a counted belief.

**From this point forward, "directly or indirectly" will be implied.**

### Impacting a trend belief

A *trend* belief captures how a given kind of observed belief is changing across the latest time frames of the CA, for example, "the distance to an obstacle keeps dminishing".

To persist a trend belief, a CA's policy would direct its umwelt to **further** the trend. To terminate a trend belief, a CA's policy would direct its umwelt to **disrupt** the trend. How this is to be accomplished depends on the nature of the trend (stable, up, down or unstable).

If the nature of the trend to impact is

* stable
  * to disrupt it, make the observed, trending belief unstable (or up or down) instead (i.e. make it take a new value)
  * to further it, keep the observed, trending belief stable (i.e. make it take the same value)
* unstable
  * to disrupt it, make the observed, trending belief stable instead
  * to further it, prevent the observed, trending belief from becoming stable
* up
  * to disrupt it, reduce or stabilize the values of the observed, trending belief
  * to further it, increase the values of the observed, trending belief
* down
  * to disrupt it, increase or stabilize the values of the observed, trending belief
  * to further it, decrease the values of the observed, trending belief

### Impacting an ending belief

An *ending* belief captures the fact that an observation from a previous time frame has disappeared from all time frames since.

To further an ending belief is simply to prevent the ended onservation from being made again (to keep it "ended"), whereas disrupting it is to make the observation again (to "un-end" it).

## Impacting a belief by executing a policy

A policy is a set of goals to be realized by a CA's umwelt in order to impact a belief held by the parent CA.

A goal is either

* an observed belief plus a desired impact (the umwelt is free to formulate an appropriate policy)
* an observed intent (a specific policy that was once observed as having been executed in the umwelt and then incorporated in the parent CA's causal model)

If the the CA is an *effector CA*, the goals it receives will simply be to activate effectors, like spinning a wheel forward or backward. Effector CAs are at the bottom level of the hierarchy of CAs and are indirectly and transitively in the umwelts of every CAs. A policy is thus ultimately realized via cumulated actions taken by effector CAs.

A CA executes a policy it formulated or received at the close of the current time frame. 

In the next time frame, the CA determines whether or not the policy, if executed, achieved the intended goal.

### Algorithm

Note: The algorithm implements something akin to two-phase commits on transactions with sub-transactions.

At the end of a time frame, a CA with no active goal (in play or executable)

* selects a goal -this becomes a "top" goal because it originated with the CA-
* and formulates a new policy or retrieves a named policy (as a conjunction of sub-goals, i.e. observation-impact pairs, to achieve the goal),
* and marks the goal as "in play",
* and emits the policy to its umwelt CAs together with its priority (a number proportional to how unpleasant/pleasant the belief to impact is).

A CA keeps a list of the policies (as a conjunction of goals)  it receives within its current timeframe.

At the end of its current timeframe, a CA:

* rejects irrelevant policies (none of the goals of the policy reference beliefs the CA holds or could hold),
* sorts the remaining policies by priority (highest first),
* accepts the highest priority policy remaining, if any,
* notifies originating CAs that it has rejected all other received policies,
* processes the accepted policy, if any

Note: A CA processes at most one policy at a time. A new time frame is started only once the accepted policy, if any, is rejected or executed. A CA's timeframe must thus be qualitatively longer than that of its umwelt CAs, to the point where, to an observer, it operates on a different time scale.

For each other goal in the accepted policy:

* If it is relevant to the CA
  * If the CA is an effector, the goal is marked as "executable" and the CA communicates to the CA of origin that the goal is "executable"
  * else, the goal is marked "in play" and the CA formulates a policy to realize it, etc.
* If it is not relevant to the CA, the goal is rejected and the originating CA is notified

A CA that emitted a policy to its umwelt CAs to achieve a goal (i.e. impact a belief it holds) keeps the goal in play:

* Until **any** goal in the emitted policy is reported as rejected by **all** umwelt CAs
  * The CA then communicates to the originating CA that the received goal, for which the CA formulated the rejected policy, was rejected, taking the goal out of play
* Or untll **all** goals in the policy are reported as executable by **any** umwelt CA
  * The CA then communicates to the originating CA, if any, that the received goal, for which the CA formulated the policy, is executable, and moves it from in play to executable

When the "top" goal is ultimately marked as "executable", the CA that formulated the policy for it now instructs its umwelt CAs to execute each (sub)goal in the policy.

When a CA receives the instruction to execute a goal it has in play

* If the CA is an effector CA that can execute the goal
  * the CA instructs the target effector to carry out the action-as-goal
  * and **immediately** reports it as executed to the originating CA (though the action may take a while to complete)
* Else, for each sub-goal in the policy it formulated to achieve the received goal
  * It instructs umwelts CAs to execute their policy for the sub-goal

Once all the (sub)goals in the policy a CA emitted are reported to it as "executed", the goal for which the policy was formulated by the CA is reported as executed to the originating CA, and it is no longer active.

When all goals of a CA are inactivated (rejected or executed), a new frame is started. For each goal that was executed, the goal with their associated policy is stored with what has now become the previous frame.

## Observing executed policies as intent beliefs

When a CA executes a policy it formulated, the execution of the policy becomes an *intent belief* in the next timeframe of the CA.

The intent belief is given a predicate name unique to its construction (the conjunction of goals). The predicate arguments are the targeted belief and the desired impact that motivated formulating the intent belief's policy.

The policy's construction is known to the CA but is hidden to observers (its parent CAs).

The intent belief, like any other belief of the CA, is observable by a parent CA, and thus can be incorporated into the parent CA's causal model.

The CA remembers named intent beliefs (and their unique policies) for as long as parent CAs references them in their causal models.

When a CA observes a new intent belief in its umwelt, it requests an updated causal model.

An executed policy is considered effective if it is picked up as a cause in a causal model of a parent CA.
