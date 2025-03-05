# Policies

A Cognition Actor (CA) seeks to persist its pleasant beliefs and to terminate its unpleasant beliefs through the execution of policies.

## Recap

A CA is part of a hierarchichal collective of CAs that animate an agent. Each CA has an umwelt composed of a small number of CAs from the (abstraction) level below.
A CA synthesizes its beliefs from the predicted observations, past and current, of the beliefs in its umwelt.

The beliefs of a CA exist in the context of wellbeing measures, current and trending. The wellbeing measures are fullness, integrity and engagement.
They signal risks, present or absent, to the survivability of the entire collective of CAs. Beliefs associated with high/growing risks are unpleasant.
Beliefs associated with low/decreasing risks are pleasant. Others are neutral. A CA acts on its umwelt in response to the pleasantness or unpleasantness of its current beliefs.

A CA operates one timeframe after another. Each timeframe corresponds to a "thick now". The timeframe of a CA starts and stops independently of other CAs;
timeframes are not synchronized. CAs higher up in the collective's hierarchy have qualitatively longer timeframes. During its current timeframe, a CA observes the latest beliefs in its umwelt (via predictions and prediction errors), reads the broadcasted global wellbeing measures, refreshes its set of beliefs by integrating the latest observations
with past ones, and assigns up-to-date normative values (pleasantness/unpleasantness) to its updated beliefs.

At the end of the current timeframe, a CA decides whether to act on updated beliefs and, if so, which ones and how.

## Intents

A CA can at most *intend* to take action to impact its beliefs; an intent is not guaranteed to be realized. After all, a CA participates in a collective
and there is only one agent that the collective animates. Simultaneous intents to alter beliefs coming from multiple CAs must be resolved so that only one course of action,
the one with highest priority, is carried out by the agent at any point in time.

An intent by a CA consists of

* a goal - a targeted belief held by the CA and a desired impact on it, i.e. the persisting of a pleasant belief or the termination of an unpleasant belief
* a priority - determined by the graded normativity of the belief targeted by the intent
* a policy - how to achieve the goal by impacting a set of observed beliefs held by the umwelt CAs from which the targeted CA's belief was synthesized

A policy is a list of directives, to be communicated to the umwelt CAs. A directive is either a goal (a belief to impact) to be achieved by the umwelt however it chooses,
or it is a command, a named and previously executed policy known to the umwelt, to be executed again by the umwelt.

The policy's directives are communicated all at once by the CA to its umwelt at the end of the CA's current timeframe.
Keep in mind that, any point in time, multiple CAs may be intending to act.

## Attention and action thresholds

The first step to acting is to identify a belief to attend to, assuming a parent CA has not already directed the CA to impact one of its own choosing.

Only certain types of beliefs can be acted on and impacted, namely `count`, `comparison`, `trend` and `end`. The others, `abduction` and `attempt`, represent "facts" that can not be changed.

Which actionable belief is identified depends on its pleasantness/unpleasantness and whether or not it is *sufficiently* pleasant or unpleasant (activation threshold) for the CA to pay attention to it.

The activation threshold is set by the current wellbeing context which is shared by the entire collective. In a low fullness context
(depleted energy stores), a CA raises the threshold for action (conserve energy!), whereas, when energy is plentiful and agent engagement is low,
a CA lowers the threshold for action (try doing something, anything!). Only a belief with normativity (pleasantness/unpleasantness) above the dynamic threshold is deemed worthy of action.

The normativity of a belief is set as a combination of absolute wellbeing values and their gradient. A positive wellbeing value but one that has been falling sharply will yield unpleasantness, whereas a negative wellbeing value that is rising sharply will yield pleasantness.

The CA selects which belief, if any, is *most worthy* of action in the current timeframe. Invalidating an unpleasant belief it cares about takes precedence over validating a pleasant belief. Typically, the CA will intend to terminate the most unpleasant, action-worthy belief. Else it will intend to persist the most pleasant, action-worthy belief, or do do something random (babble) if there is no action-worthy belief.

## Constructing a policy

When a CA receives a policy from a parent CA, it determines which goals and commands are relevant to it; the others are likely relevant to sibling CAs participating in the same umwelt.

Commands are pre-constructed policies and need be executed only if known. Goals name beliefs and impacts and it's up to the receiving CA to construct a policy for each one that's relevant to the CA.

Once a CA has identified (or been told by a parent CA of) a belief to persist or terminate, the CA must determine a policy to achieve this goal.
The composition of the policy depends on the nature of the belief (`count`, `comparison`, `trend` or `end`) and on the desired impact (persist/terminate).

The nature of the belief and the desired impact determine the *base of support* of the belief to be impacted. A belief is synthesized from observations (its base of support) which are themselves beliefs once or currently held in the CA's umwelt. To impact a belief a CA holds, the CA needs to impact supporting beliefs observed to be held by CAs in its own umwelt.

Unless it only just got started, a CA will have a causal theory. The CA can use it to predict changes to the beliefs of its umwelt (i.e. incoming observations) as a result of executing
particular directives (goals or commands). This can be put to use when shaping a policy.

A causal theory has three kinds of rules

* Constraints: What is expected *not* to be observed simultaneously - e.g. not (A and B)
* Static rules: What is expected to be observed simultaneoulsy - e.g. if A and B and then also C
* Causal rules: What is expected to be observed given what was last observed - e.g. A and B causes C

(An additional but implied rule is that a previous observation survives as a current observation if it does not contradict any constraint or static rule.)

A CA's belief can be a synthesis of multiple observations (of beliefs in the CA's umwelt), either within the bounds of the current timeframe (`count` and `comparison` beliefs)
or integrating past and current timeframes (trend and end beliefs). A belief can also be a record of an executed policy (attempt) or an imagined property/relation (abduction).

These observations constitute the base of support of the belief targeted for impact. To impact a belief (to persist or terminate it), the CA will want to impact all or some of the supporting observations.

Observations of the CA being themselves beliefs held in the umwelt of the CA, one can see how acting becomes a recursive operation that will unfold across multiple layers of CAs and over multiple CA timeframes, until effector CAs become involved and actually effect change to the agent or its environment.

The task of constructing a policy -in order to impact a belief held by the CA- consists in deciding which supporting beliefs (observed umwelt beliefs) to impact and how.
There will likely be many possible combinations of supporting beliefs to impact and thus there could be many alternate policies to choose from.
The causal theory, which models the generative process causing these observations, informs the CA in the construction of hopefully effective policies.

To recap, the CA could impact a supporting belief either *by command* or *by goal* .

* By command: By naming a policy once executed by the umwelt to be executed again
* By goal: By requesting its umwelt to find a way to impact a belief it holds

Let's say a CA wants to impact an observation A that supports a belief it intends to persist or terminate. It could do so directly but also indirectly by impacting observation B if

* A and B are mutually exclusive (according to the CA's causal theory)
* A and B must co-exist under some condition
* B causes A under some condition

How a CA could impact a held belief is further modulated by the kind of belief it is. The only types of beliefs that can be impacted are count, trend and end beliefs.

An *abduction* belief can not be impacted. This would be tantamount to neurosis. A belief abduced by a causal model it posited as given, until a new causal model is acquired that is without it.

An *attempt* belief also can not be impacted for the same reason: It is a record of action already taken.

### Impacting a count belief

A *count* belief encapsulates how many of a given kind of observed beliefs there are in the current timeframe (for e.g., self is next to 2 objects)

To persist a count belief, a CA's policy would direct its umwelt CAs to persist *all of* the counted (observed) beliefs.

To terminate a count belief, a CA's policy would direct its umwelt CAs to terminate *any of* the counted beliefs, or to add a counted belief.

### Impacting a comparison belief

A *comparison* belief expresses that more of an object was counted than another.

To persist a comparison belief, a CA's policy would direct its umwelt CAs to persist both compared counts.

To terminate a count belief, a CA's policy would direct its umwelt CAs to reduce the first counts, or to grow the second count.

### Impacting a trend belief

A *trend* belief captures how a given kind of observed belief is changing across the latest timeframes of the CA, for example, "distance keeps dminishing" is a "down" trend.

To persist a trend belief, a CA's policy would direct its umwelt to **further** the trend. To terminate a trend belief, a CA's policy would direct its umwelt
to **disrupt** the trend. How this is to be accomplished depends on the nature of the trend (stable, up, down or unstable).

A `stable` trend captures an unchanging value for a given property of a given object.
An `unstable` trend captures a seemingly randomly changing value for a given property of a given object.
An `up` trend captures an upwardly changing ordinal value for a given property of a given object.
A `down` trend captures a downwardly changing ordinal value for a given property of a given object.

If the nature of the trend to impact is

* stable
  * to disrupt it, make the observed, trending belief unstable (or up or down) instead (i.e. make it take a different value)
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

### Impacting an end belief

An *end* belief captures the fact that an observation from a previous timeframe has disappeared from all timeframes since.

To further an end belief is simply to prevent the ended onservation from being made again (to keep it "ended"), whereas disrupting it is to make the observation again (to "un-end" it).

## Impacting a belief by executing a policy

A policy is a set of directives to be realized by a CA's umwelt in order to impact a belief held by the parent CA.

A directive is either

* a goal: An observed belief plus a desired impact (the umwelt is free to formulate an appropriate policy)
* a command: a named policy (previously built and executed in the umwelt and then incorporated in the parent CA's causal model)

If the the CA is an *effector CA*, the goals it receives will simply be to activate effectors, like spinning a wheel forward or backward.
Effector CAs are at the bottom level of the hierarchy of CAs and are indirectly and transitively in the umwelts of every CAs.
A policy is thus ultimately realized via cumulated actions taken by effector CAs.

A CA executes a policy it formulates or has received only at the close of its current timeframe.

### Algorithm for executing policies

Note: The algorithm implements something akin to two-phase commits on composite transactions (only once a policy in play is recursively marked as executable is it recursively executed).

At the end of a timeframe, a CA

* selects a goal -this becomes a "top" goal because it originated with the CA-
* and formulates a policy to achieve the goal,
* and marks the goal as "in play",
* and emits the policy to its umwelt CAs together with its priority (a number proportional to how unpleasant/pleasant the belief to impact is).

A CA keeps a list of the policies (as a conjunction of goals)  it receives within its current timeframe.

At the end of its current timeframe, a CA:

* rejects irrelevant policies (none of the goals of the policy reference beliefs the CA holds or could hold, none of the commands in the policy are known to the CA),
* sorts the remaining policies by priority (highest first),
* accepts the highest priority policy remaining, if any,
* notifies originating CAs of having rejected all other received policies,
* processes the accepted policy, if any

Note: A CA processes at most one policy at a time. A new timeframe is started only once the accepted policy, if any, is rejected or executed.
A CA's timeframe must thus be qualitatively longer than that of its umwelt CAs, to the point where, to an observer, it operates on a different time scale than its umwelt.

For each goal in the accepted policy:

* If it is relevant to the CA
  * If the CA is an effector, the goal is marked as "executable" and the CA communicates to the CA of origin that the goal is "executable"
  * else, the goal is marked "in play" and the CA formulates a policy to realize it (i.e. recursively executes the algorithm)
* If it is not relevant to the CA, the goal is rejected and the originating CA is notified

For each command in the accepted policy:

* If it is known to the CA, the command is marked as "executable" and the CA communicates to the CA of origin that the command is "executable"
* If it is not known to the CA, the command is rejected and the originating CA is notified

A CA that emitted a policy to its umwelt CAs to achieve a goal (i.e. impact a belief it holds) keeps the goal in play:

* Until **any** directive in the emitted policy is reported as rejected by **all** umwelt CAs
  * The CA then communicates to the originating CA that the received goal, for which the CA formulated the rejected policy, was rejected, taking the goal out of play
* Or untll **all** directives in the policy are reported as executable by **any** umwelt CA
  * The CA then communicates to the originating CA, if any, that the received goal, for which the CA formulated the policy, is executable, and moves it from in play to executable

If the "top" goal is marked as "executable", the CA instructs its umwelt CAs to execute each directive in the policy.
If the top goal's policy is rejected, the goal is taken out of play and nothing is done.

When a CA receives the instruction to execute a goal it has in play

* If the CA is an effector CA that can execute the goal
  * the CA instructs the target effector to carry out the action-as-goal
  * and **immediately** reports it as executed to the originating CA (though the action may take a while to complete)
* Else, for each directive in the policy it formulated to achieve the received goal
  * It randomly selects one umwelt CA ready to execute the directive (more than one umwelt CA might be ready, making the others redundant)
    * It instructs the selected umwelt CA
      * to execute the policy it built if the directive is a goal
      * to execute the named, pre-built policy if the directive is a command
    * It instructs any redundant umwelt CA to consider the directive as executed

Once all the directives in the policy a CA emitted are reported to it as "executed", the goal for which the policy was formulated by the CA is reported as executed
to the originating CA, and it is no longer active.

When all goals of a CA are inactivated (rejected or executed), a new frame is started.

For each goal that was executed, the goal with their associated policy is stored with what has now become the previous frame.

## Observing executed policies as attempt beliefs

When a CA executes a policy it formulated, the execution of the policy becomes an *attempt belief* held by the CA in its next timeframe.

The attempt belief, like any other belief of the CA, is observable by a parent CA, and, crucially, can be incorporated into the parent CA's causal model.

The policy's contents (a list of goals for the CA's umwelt) is known to the CA but is hidden to its observers (i.e. parent CAs).

The CA remembers named attempt beliefs (and their unique policies) for as long as parent CAs references them in their causal models.

When a CA observes a new (as yet unobserved) attempt belief in its umwelt, its causal model becomes obsolete and ought to be replaced.

An executed policy is considered effective if it is picked up (in the form of an attempt belief) as a cause in a causal model of a parent CA.
