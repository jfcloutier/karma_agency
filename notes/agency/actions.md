# Actions

A Cognition Actor (CA) seeks to validate its pleasant beliefs and to invalidate its unpleasant beliefs through actions.

## Recap

A CA is part of a hierarchichal collective of CAs that animate an agent. Each CA has an umwelt composed of a small number of CAs from the level below. A CA synthesizes its beliefs from predicted observations, past and current, of the beliefs in its umwelt.

The beliefs of a CA exist in the context of wellbeing measures, current and trending. The wellbeing measures are fullness, integrity and engagement. They signal risks, present or absent, to the survivability of the entire collective of CAs. Beliefs associated with high risks are unpleasant. Beliefs associated with low risks are pleasant. Others are neutral. A CA acts on its umwelt in response to the pleasantness or unpleasantness of its current beliefs.

A CA operates one time frame after another. Each time frame corresponds to a "thick now". The time frame of a CA starts and stops independently of other CAs; time frames are not synchronized. CAs higher up in the collective's hierarchy have longer time frames. During its current time frame, a CA makes new observations by predicting the latest beliefs in its umwelt, reads the global wellbeing measures, refreshes its set of beliefs by integrating the latest observations with past ones, and assigns up-to-date normative values (pleasantness/unpleasantness) to its updated beliefs.

At the end of the current time frame, a CA decides whether to act on its updated beliefs and, if so, how.

## Intents

A CA can at most *intend* to take action; an intent is not guaranteed to be realized. After all, a CA participates in a collective and there is only one agent that the collective animates. Simultaneous intents to act coming from multiple CAs must be resolved so that only one course of action, hopefully beneficial to all CAs, is carried out by the agent at any point in time.

An intent (to act) by a CA consists of

* a goal - the validation of a pleasant belief or the invalidation of an unpleasant belief held by the CA
* a priority - determined by the graded normativity of the belief targeted by the intent
* a policy - a set of beliefs held by the CA's umwelt CAs (and on which the targeted belief depends) to be validated/furthered or invalidated/disrupted in order to achieve the goal

A policy is effectively a list of sub-goals to be delegated to the umwelt CAs. The policy's sub-goals are communicated all at once to the CA's umwelt at the end of the CA's current time frame.

Repeated intents builds up habits whenever realizing an intent via a given policy is largely successful.

## Attention and action thresholds

The first step to composing an intent is to identify a belief to attend to (assuming a parent CA has not imposed one - more on this later).

Which belief is identified for action depends on the normativity of the belief and whether or not it is *sufficiently* pleasant or unpleasant (the normativity threshold) for the CA to pay attention to it.

The normativity threshold for action is set by the current wellbeing context which is shared by the entire collective. In a low fullness context (depleted energy stores), a CA raises the threshold for action (conserve energy!), whereas, when energy is plentiful and agent engagement is low, a CA lowers the threshold for action (try doing something, anything!). Only a belief with normativity above threshold is deemed worthy of action.

The CA selects which belief, if any, is *most worthy* of action in the current time frame. Invalidating an unpleasant belief it cares about takes precedence over validating a pleasant belief. Typically, the CA will intend to invalidate the most unpleasant, action-worthy belief. Else it will intend to validate the most pleasant, action-worthy belief, or do neither if there is no action-worthy belief.

## Constructing a policy

Once a CA has identified (or been told of) of a belief to validate or invalidate, the CA must determine a policy to achieve this goal. The composition of the policy depends on the nature of the belief (abduction, count, trend or ending) and on the desired impact (validation/perpetuation or invalidation/termination).

It also depends on the *base of support* of the belief to be impacted; a belief is synthesized from observations (its base of support) which are themselves beliefs once or currently held in the CA's umwelt. To impact a belief a CA holds, the CA needs to impact supporting beliefs held by CAs in its umwelt.

Unless it only just got started, a CA will have a causal theory it uses to predict the beliefs of its umwelt(i.e. incoming observations). This can also be put to use when shaping a policy.

A causal theory has three kinds of rules

* Constraints: What is expected *not* to be observed simultaneously - e.g. not (A and B)
* Static rules: What is expected to be observed simultaneoulsy - e.g. A if B and C
* Causal rules: What is expected to be observed given what was last observed - e.g. A and B causes C

(An additional but implied rule is that a previous observation survives as a current observation if it does not invalidate any constraint or static rule.)

A belief is a synthesis of multiple observations, either made in the current time frame (abduction or count belief) or across past and current time frames (trend and ending beliefs). These observations constitute the base of support of the belief targeted for impact by the intent. To impact a belief (to validate or invalidate it), the CA will want to impact all or some of the supporting observations. These observations being themselves beliefs held in the umwelt of the CA, one can see how acting becomes a recursive operation that will unfold across multiple layers of CAs and over multiple CA time frames.

The task of constructing a policy -in order to impact a belief held by the CA- consists in deciding which supporting beliefs (observed umwelt beliefs) to impact and how. There will likely be many possible combinations of supporting beliefs to impact and thus there could be many alternate policies to choose from. The causal theory, which models the generative process causing these observations, informs the CA in the construction of hopefully effective policies.

The CA could impact a supporting belief either *directly* or *indirectly*.

* Directly: By requesting its umwelt to alter the supporting belief itself
* Indirectly: By requesting its umwelt to impact one or more of the beliefs that are causally related to the supporting belief (contingent beliefs according to the CA's causal theory).

Let's says a CA wants to impact an observation A that supports a belief it intends to validate or invalidate. It could do so directly but also indirectly by impacting observation B if

* A and B are mutually exclusive (according to the CA's causal theory)
* A and B must co-exist under some condition
* B causes A under some condition

How a CA could impact a held belief is further modulated by the kind of belief it is (abduction, count, trend or ending belief).

### Impacting an abduction belief

An *abduction* belief is an object, relationship or property that is *imagined* (i.e. not directly observed and thus made up) by the CA to formulate a causal theory that explains what it does directly observe.

To validate an abduction belief, a CA's policy would request its umwelt CAs to validate *all of* the supporting beliefs, directly or indirectly.

To invalidate an abduction belief, a CA's policy would request its umwelt CAs to invalidate *any of* the supporting beliefs, directly or indirectly.

### Impacting a count belief

A *count* belief encapsulates how many of a given kind of observed beliefs there are in the current time frame (for e.g., self is next to 2 objects)

To validate a count belief, a CA's policy would direct its umwelt CAs to validate *all of* the counted (observed) beliefs, directly or indirectly.

To invalidate a count belief, a CA's policy would direct its umwelt CAs to invalidate *any of* the counted beliefs, directly or indirectly.

**From this point forward, "directly or indirectly" will be implied.**

### Impacting a trend belief

A *trend* belief captures how a given kind of observed belief is changing across the latest time frames of the CA, for example, "the distance to an obstacle keeps dminishing".

To validate a trend belief, a CA's policy would direct its umwelt to **further** the trend. To invalidate a trend belief, a CA's policy would direct its umwelt to **disrupt** the trend. How this is to be accomplished depends on the nature of the trend (stable, up, down or unstable).

If the nature of the trend to impact is

* stable
  * to disrupt it, make the observed, trending belief unstable instead (i.e. make it take a new value)
  * to further it, keep the observed, trending belief stable (i.e. make it take the same value)
* unstable
  * to disrupt it, make the observed, trending belief stable instead
  * to further it, keep the observed, trending belief unstable
* up
  * to disrupt it, reduce or stabilize the values of the observed, trending belief
  * to further it, increase the values of the observed, trending belief
* down
  * to disrupt it, increase or stabilize the values of the observed, trending belief
  * to further it, decrease the values of the observed, trending belief

### Impacting an ending belief

An *ending* belief captures how long (i.e. how many of the CA's time frames) before a given kind of observation ends.

To further an ending belief is simply to prevent it from being held again (to keep it "ended"), whereas disrupting it is to hold the belief again (to "un-end" it).

## Receiving a goal

A CA can have at most one intent in play at any time. When it receives a goal (with associated priority) from a parent CA, the CA:

* rejects it if the CA already has an intent in play of equal or higher priority (the rejection is communicated to the originating parent CA)
* accepts it if the incoming goal's priority is higher;  the CA immediately terminates the intent in play with failure (also communicated to the originating parent CA)

A goal received by a CA from a parent CA is a request to validate/further/instantiate or invalidate/disrupt/terminate a belief it holds or could hold. There is no difference, other than its origin, between a goal received or autonomously set by a CA. In either case, the CA needs to wrap the goal into an intent by constructing or selecting a policy that is likely to achieve the goal.

It is the receiving CA's responsibility to select a policy to achieve the received goal. The parent CA does not micro-manage its umwelt; it delegates goals to them but lets them determine what policies to use to accomplish these goals.

If a CA receives multiple goals from the same parent CA at once, it accepts or rejects them all. If accepted, it then queues them and converts each one into an intent, and then attempts to realize each one in turn, one per time frame, by constructing and executing a policy for it. The intents waiting in queue are considered "in play", should more goals be received.

If at the end of its current time frame, a CA has no intent in play or has not received goals from parent CAs, then the CA may autonomously compose one from scratch based on its own, action-worthy pleasant or unpleasant beliefs.

## Executing a policy

Until its intent's policy is executed or rejected, a CA remembers that it has the intent "in play". The CA can not generate or attempt to realize another intent until the last one is rejected or completed in success or in faillure.

A CA constructing an intent around a goal will first look up in past time frames for policies it has already tried, if any, to accomplish the goal. If it remembers one that was repeatedly successful, it will try it again out of habit, else it will construct a new one.

A policy is a set of goals to be realized by the receiving CA's own umwelt, unless the CA is an *effector CA*, in which case goals received are simply to activate effectors, like spinning a wheel forward or backward. Effector CAs are at the bottom level of the hierarchy of CAs and are indirectly and transitively in the umwelts of every CAs. A policy is ultimately realized via cumulated actions taken by effector CAs.

A CA executes a policy as it closes its current time frame. In the next time frame, the CA determines whether or not the policy achieved the intended goal.

## Determining the success or failure of a policy

If any of the goals in the intent's policy was rejected by all umwelt CAs (i.e. none took it up), then the intent as a whole is marked as rejected.

Otherwise, after it has updated its beliefs in the current time frame, a CA looks up for the intent in play, if any, and checks if the intended goal was achieved given the evidence provided by the updated beliefs.

If unsuccessful (thus far), the CA may wait another time frame to assess success or failure, keeping the intent in play. Otherwise the CA records the resulting success or failure of the intent and marks it as "no longer in play".

A CA remembers past time frames and associated completed intents. Eventually old time frames will be forgotten but, until they are, completed intents and the effectiveness of their policies remain available to the CA.
