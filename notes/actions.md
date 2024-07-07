# Actions

A Cognition Actor (CA) seeks to validate pleasant beliefs and to invalidate unplesant beliefs by acting.

## Recap

A CA is part of a hierarchichal collective of CAs that animate a mobile agent. Each CA has an umwelt composed of a small number of CAs from the level below. A CA synthesizes its beliefs from observed supporting beliefs, past and current, of the CAs in its umwelt. The beliefs of a CA exist in the context of wellbeing measures, current and trending. Wellbeing measures (fullness, integrity and engagement) signal risks, present or absent, to the survivability of the entire collective of CAs. Beliefs associated with high risks are unpleasant. Beliefs associated with low risks are pleasant. Others are neutral. A CA acts on its umwelt in response to the pleasantness or unpleasantness of its beliefs.

A CA operates one time frame after another. Each time frame corresponds to a "thick now". Each CA starts and stops its current time frame independently of other CAs; time frames are not synchronized across CAs. CAs higher up in the collective's hierarchy have longer time frames. During its current time frame, a CA makes new observations by predicting the latest beliefs of its umwelt, reads the global wellbeing meaures, refreshes its set of beliefs from integrating the latest observations with past ones, and assigns up-to-date normative values (pleasantness/unpleasantness) to its updated beliefs.

At the end of the current time frame, a CA must decide whether to act on its updated beliefs and, if so, how.

## Intents

A CA can at most **intend** to take action; an intent is not guaranteed to be realized. After all, a CA participates in a collective of CAs and there is only one agent that the collective animates. Simultaneous intentions to act coming from multiple CAs must be resolved so that only one course of action, hopefully beneficial, is carried out by the agent at any point in time.

An intent (to act) consists of

* a goal - the validation of a pleasant belief or the invalidation of an unpleasant belief held by the CA
* a priority - determined by the normativity of the belief targeted by the intent
* a policy - a set of beliefs held by umwelt CAs (and on which the targeted belief depends) to be validated/furthered or invalidated/disrupted in order to achieve the goal

A policy is effectively a set of sub-goals to be delegated to the umwelt CAs to which they apply. The policy's sub-goals are communicated all at once to the CA's umwelt at the end of the present time frame.

Until its policy is executed, a CA remembers that it has an intent "in play". This terminates the present time frame and starts a new one. The CA can not generate another intent until the last one is completed, in success or in faillure (more on this later).

A CA can have at most one intent in play at any time. When it receives a policy goal (with associated intent priority) from a parent CA, the CA

* rejects it (fails it immediately) if the CA already has an intent in play of equal or higher priority
* accepts it if the incoming goal's priority is higher, and immediately terminates the intent in play with failure

Once a received (prioritized) goal (to validate/invalidate a belief held by the receiving CA and observed by the parent CA) is accepted, the CA turns it into an intent by formulating a policy for it. It is the receiving CA's responsibility to select a policy to achieve the received/imposed goal. The parent CA does not micro-manage its umwelt; it delegates goals to them but lets them determine the policies to accomplish them.

If a CA receives multiple goals from the same parent CA at once, it accepts or rejects them all. If accepted, it then queues them and converts each one into an intent, and then attempts to realize each one in turn, one per time frame. The intents waiting in queue are considered "in play", should more goals be received.

If at the end of its current time frame, a CA has no intent in play or has not received goals from parent CAs, then the CA may autonomously compose one from scratch based on its own pleasant or unpleasant beliefs.

## Attention and action thresholds

The first step to composing an intent is to identify a belief to attend to. Which belief is identified depends on the normativity of the belief and whether or not it is *sufficiently* pleasant or unpleasant (the normativity threshold) for the CA to pay attention to it.

The normativity threshold for action is set by the current wellbeing context, shared by the entire collective. In a low fullness context (depleted energy stores), a CA raises the threshold for action (conserve energy!), whereas, when energy is plentiful and agent engagement is low, a CA lowers the threshold for action (try doing something, anything!). Only a belief with normativity above threshold is deemed worthy of action.

The CA selects which belief, if any, is most worthy of action in the current time frame. Invalidating an unpleasant belief it cares about takes precedence over validating a pleasant belief it also cares about. Typically, the CA will intend to invalidate the most unpleasant, action-worthy belief. Else it will intend to validate the most pleasant, action-worthy belief, or do neither if there is no action-worthy belief.

## Constructing a policy

Once the CA has identified (or been told of) a belief to validate or invalidate, a CA must determine a policy that could achieve this goal. The composition of the policy depends on the nature of the belief (count, trend or ending) and on the desired impact (validation/perpetuation or invalidation/termination). It also depends on the *base of support* of the belief to be impacted.

A belief is synthesized from observations which are themselves beliefs once or currently held by the CA's umwelt. To impact a belief a CA holds, the CA needs to impact the supporting beliefs held by CAs in its umwelt.

Unless it only just got started, a CA will have a causal theory that enables it to predict incoming observations of its umwelt. This will be of use.

A causal theory has three kinds of rules

* constraints: what will not be observed simultaneously - e.g. not (A and B)
* static rules: what will be observed simultaneoulsy - e.g. A if B and C
* causal rules: what will be observed given what was last observed - e.g. A and B causes C

An implied rule is that a previous observation survives as a current observation if it does not invalidate any of the above rules (aka framing).

A belief is a synthesis of multiple observations, either made in the current time frame (count belief) or across past and current time frames (trend and ending beliefs). These observations constitute the base of support of the belief. To impact a belief (to validate or invalidate it), the CA will want to impact all or some of its supporting observations. These observations are themselves beliefs held in the umwelt of the CA. We can see how this is a recursive operation that will unfold across multiple layers of CAs and over multiple CA time frames.

The task of constructing a policy -in order to impact a belief held by the CA- consists in deciding which supporting beliefs (observations of umwelt beliefs) to impact and how. There will likely be many alternate policies to choose from. The causal theory, which models the generative process causing these observations, informs the CA in constructing hopeful effective policies.

Again, to impact a belief, a CA will want to impact the belief's base of support (observations from which the belief was synthesized).

The CA could impact a supporting observation (a belief held in the umwelt of the CA) either *directly* or *indirectly*.

* Directly: By requesting its umwelt to alter the observed belief itself
* Indirectly: By requesting its umwelt to impact another observation that is related to the supporting observed belief according to the CA's causal theory

Let's says the CA wants to impact observation A that supports a targeted belief. It could do so directly but also indirectly by impacting observation B if

* A and B are mutually exclusive (according to the CA's causal theory)
* A and B must co-exist under some condition
* B causes A under some condition

How a CA can impact a held belief is further modulated by the kind of belief it is (count, trend or ending belief).

### Impacting a count belief

A *count* belief encapsulates how many of a given kind of observed beliefs there are in the current time frame (for e.g., self is next to 2 objects)

To validate a count belief, a CA's policy would direct its umwelt CSs to validate *all of* the counted (observed) beliefs, directly or indirectly.

To invalidate a count belief, a CA's policy would direct its umwelt CAs to invalidate *any of* the counted beliefs, directly or indirectly.

**From this point forward, "directly or indirectly" will be implied.**

### Impacting a trend belief

A *trend* belief captures how a given kind of observed belief is changing across the latest time frames of the CA, for .e.g. the distance to an obstacle keeps dminishing.

To validate a trend belief, a CA's policy would direct its umwelt to **further** the trend. To invalidate a trend belief, a CA's policy would direct its umwelt to **disrupt** the trend. How this is to be accomplished depends on the nature of the trend (stable, up, down or unstable).

If the nature of the trend to impact is

* stable then
  * to disrupt it, make the observed, trending beliefs unstable instead (i.e. take a new value)
  * to further it, keep the observed, trending beliefs stable (i.e. take the same value)
* unstable then
  * to disrupt it, make the observed, trending beliefs stable instead
  * to further it, keep the observed, trending beliefs unstable
* up then
  * to disrupt it, reduce or stabilize the values of the observed, trending beliefs
  * to further it, increase the values of the observed, trending beliefs
* down then
  * to disrupt it, increase or stabilize the values of the observed, trending beliefs
  * to further it, decrease the values of the observed, trending beliefs

### Impacting an ending belief

An *ending* belief captures how long before a given kind of belief, observed in recent time frames, ends.

To further an ending belief is simply to prevent it from becoming true (to keep it "ended"), whereas disrupting it is to make the belief true again (to "un-end" it).

## Tranlsating a goal into an intent with a policy

It is left to the CAs to find policies that will accomplish goals they receive from their parent CAs, should they accept the goals.

A received goal is a request to validate/further/instantiate or invalidate/disrupt/terminate a belief they hold or could hold. There is no difference between a received goal and a goal set autonomously by a CA other than its origin. In both cases, a policy needs to be constructed/selected that is likely to achieve the goal.

## Executing a policy

## Evaluating the effectiveness of a policy


  * By furthering or interrupting (i.e. impacting) sub-beliefs via intents delegated to umwelt CAs
* Realizing an intent is recursive because a belief is a recursive structure of beliefs held by umwelt CAs
* An intent contains
  * a goal - the belief of the CA to validate or invalidate
  * a policy - which belief(s) of umwelt CAs to validate/invalidate
  * the initiating CA
  * a deadline (the end of the CA's time frame)
* Policy selection
  * What intents a CA emits to its umwelt depends on the CA's goal
  * Determining alternative ways to achieve a goal
    * identify the subject of the trend defining the belief
    * The subject of the trend is the head of one or more causal rules in the CA's causal theory
    * This entails various combination of causative sub-beliefs, from the bodies of the causal rules, that the CA may want to impact
    * A policy is a set of intents each with the goal of impacting a sub-belief (i.e. a belief in the umwelt)
  * Each alternative set of intents is a distinct policy that can be tried
    * How sub-beliefs are intended to be impacted depends on how the CA's belief is to be impacted
      * furthering all if the goal is to further the CA's belief
      * disrupting any if the goal is to disrupting the CA's belief
    * Since a belief is a detected trend, impacting it depends on the type of impact and the value of the trend
      * if the value of the trend to impact is
        * stable
          * disrupt -> intend to disrupt sub-beliefs
          * further -> intend to further sub-beliefs
        * unstable
          * disrupt -> intend to further sub-beliefs
          * further -> intend to disrupt sub-beliefs
        * fluctuant
          * disrupt -> intend to further sub-beliefs (stop fluctuation)
          * further -> intend to disrupt sub-beliefs (keep fluctuating)
        * up
          * disrupt -> intend to down sub-beliefs
          * further -> intend to up sub-beliefs
        * down
          * disrupt -> intend to up sub-beliefs
          * further -> intend to down sub-beliefs
* A CA receiving an intent from a parent CA will try alternative policies until the received intent is realized (or times up)
  * based on its causal theory which might describe alternate causes for the same outcome
  * Or on guesses if it has no theory yet (babbling)
* One policy execution at a time
  * A CA can not intend its own goal if it is executing a parent CA's policy
    * Lower-level CAs relinquish some autonomy for the greater good
* Learning affordances
  * A CA gets feedback as to whether the intents in an executed policy were realized
  * A CA remembers how successul each policy was at achieving a goal
    * A CA selects the more recently successful policies more often (it will also try others in case they might be better)
  * A CA emits a goal-free intent to observe what happens (babbles)
    * when given the opportunity
      * all wellbeing levels are high and the CA is not involved in a parent's intent
    * or when necessary
      * the CA lacks beliefs (because the