# Actions

A Cognition Actor (CA) seeks to validate pleasant beliefs and to invalidate unplesant beliefs by acting.

## Recap

A CA is part of a hierarchichal collective of CAs that animate a mobile agent. Each CA has an umwelt composed of a small number of CAs from the level below. A CA synthesizes its beliefs from observed supporting beliefs, past and current, of the CAs in its umwelt. The beliefs of a CA exist in the context of wellbeing measures, current and trending. Wellbeing measures (fullness, integrity and engagement) signal risks, present or absent, to the survivability of the entire collective of CAs. Beliefs associated with high risks are unpleasant. Beliefs associated with low risks are pleasant. Others are neutral. A CA acts on its umwelt in response to the (un)pleasantness of its beliefs.

A CA operates one time frame after another. Each time frame corresponds to a "thick now". Each CA starts and stops its current time frame independently of other CAs; time frames are not synchronized across CAs. CAs higher up in the collective's hierarchy have longer time frames. During its current time frame, a CA updates its observations of its umwelt, reads the global wellbeing meaures, refreshes its set of beliefs from observations latest and past, and assigns up-to-date normative values (pleasantness/unpleasantness) to its beliefs.

At the end of the current time frame, a CA must decide whether to act and, if so, how.

## Intents

A CA can at most **intend** to take action; an intent is not guaranteed to be realized. After all, a CA participates in a collective of CAs and there is only one agent that the collective animates. Simultaneous intentions to act coming from multiple CAs must be resolved so that only one course of action, hopefully beneficial, is carried out by the agent at any point in time.

An intent (to act) consists of

* a goal - the validation of a pleasant belief or the invalidation of an unpleasant belief held by the CA
* a priority - determined by the normativity of the belief targeted by the intent
* a policy - a set of beliefs held by umwelt CAs (and on which the targeted belief depends) to be validated/invalidated in order to achieve the goal

A policy is effectively a set of sub-goals to be delegated to the umwelt CAs to which they apply. The policy's sub-goals are communicated all at once to the CA's umwelt at the end of the present time frame.

The CA remembers that it has an intent "in play", and terminates the time frame to start a new one. The CA can not generate another intent until the last one is completed, in success or in faillure (more on this later).

A CA can have at most one intent in play at any time. When it receives a policy goal (with associated intent priority) from a parent CA, the CA

* rejects it (fails it immediately) if the CA already has an intent in play of equal or higher priority
* accepts it if the incoming goal's priority is higher, and immediately terminates the intent in play with failure

Once a received (prioritized) goal (to validate/invalidate a belief held by the receiving CA and observed by the parent CA) is accepted, the CA turns it into an intent by adding a policy to it. It is the receiving CA's responsibility to select a policy to achieve the received/imposed goal. The parent CA does not micro-manage its umwelt; it delegates goals to them but lets them determine the policies to accomplish them.

If a CA receives multiple goals from the same parent CA at once, it accepts or rejects them all. If accepted, it then queues them and converts each one into an intent, and then attempts to realize each one in turn, one per time frame. The waiting intents are considered "in play", should more goals be received.

If at the end of its current time frame, a CA has no intent in play or has not received goals from parent CAs, then the CA may autonomously compose one from scratch.

## Attention and action thresholds

The first step to composing an intent is to identify a belief to attend to. Which belief is identified depends on the normativity of the belief and whether or not it is sufficiently pleasant or unpleasant (the normativity threshold) for the CA to pay attention to it.

The normativity threshold for action is set by the current wellbeing context, shared by the entire collective. In a low fullness context (depleted energy stores), a CA raises the threshold for action (conserve energy!), whereas, when energy is plentiful and agent engagement is low, a CA lowers the threshold for action (try doing something, anything!). A belief with normativity above threshold is deemed worthy of action.

The CA selects which belief, if any, is most worthy of action in the current time frame. Invalidating an unpleasant belief it cares about takes precedence over validating a pleasant belief it also cares about. Typically, the CA will intend to invalidate the most unpleasant, action-worthy belief. Else it will intend to validate the most pleasant, action-worthy belief, or do neither if there is no action-worthy belief.

## Selecting a policy

Once the CA has identified a belief to validate or invalidate as a goal, a CA must determine a policy that could achieve this goal. The composition of the policy depends on the nature of the belief (count, trend or ending) and on the desired impact (validation/perpetuation or invalidation/termination). A belief is synthesized from observations, themselves beliefs once or currently held by the CA's umwelt. To impact a belief a CA holds, the CA needs to impact the supporting beliefs held by CAs in its umwelt.

### Impacting a count belief

A *count* belief encapsulates how many of a given kind of observed beliefs there are in the current time frame.

To validate a count belief, a CA's policy would direct its umwelt CSa to validate *all of* the counted (observed) beliefs.

To invalidate a count belief, a CA's policy would direct its umwelt CAs to invalidate *any of* the counted beliefs.

### Impacting a trend belief

A *trend* belief encapsulates how a given kind of observed belief is changing across the latest time frames of the CA.

To validate a trend belief, a CA's policy would direct its umwelt to **further** the trend. To invalidate a trend belief, a CA's policy would direct its umwelt to **disrupt** the trend. How this is to be accomplished depends on the nature of the trend (stable, up, down or unstable).

If the nature of the trend to impact is

* stable then
  * to disrupt it, make the observed, trending beliefs unstable instead
  * to further it, keep the observed, trending beliefs stable
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

## Interpreting a received goal

It is left to the umwelt CAs to find policies that will accomplish received goals.

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