# Cognition actor

A CA strives to become increasingly compentent at making sense of its umwelt and at altering its own beliefs.

A CA makes sense of its umwelt by predicting changes to it (i.e. predicting incoming observations)
and by deriving beliefs from past observations that then become observations available to parent CAs (higher-level CAs that have the CA in their umwelts).

A CA is effective at impacting its umwelt if it has reliable policies it can apply to validate pleasant beliefs or invalidate unpleasant beliefs.

The more competent a CA is, the more likely it is that its parents CAs will be competent and survive, and thus the more likely it is that the CA itself will survive since orphaned CAs are susceptible to being removed.

## Observing (its umwelt's beliefs)

* A CA predicts the beliefs of CAs in its umwelt
  * If the CA has no prior observations, it makes random, domain-bounded predictions about all beliefs in its umwelt
  * If the CA has no causal theory but has prior observations, it predicts that the latest observations are unchanged.
  * If the CA has a causal theory, it uses it to predict the next observations.
* A CA predicts the success of its policies
* If a prediction is met with a prediction error, the prediction error becomes the latest observation, else its the the prediction.
* A CA drops repeated states (state = observations in the same time frame) - i.e. if nothing changes, remembered time stands still

## Modeling (causality relating observations)

* To be competent, a CA needs to make sense of its observations via a causal theory of
  * how its umwelt changes (or not) on its own
  * how its umwelt changes (or not) by executed policies
* A CA requests a causal theory from the Apperception Engine when
  * it has no theory and a theory can be generated from its remembered states
  * it has a theory and its predictive capabilities have degraded since acquired such that it is no longer good enough
* A CA's causal theory integrates beliefs across its umwelt
  * co-occurrences of observations from static rules
    * if a static rule states "A whenever B and C:, then observing B and C at time T infers (deductively) observing A also at T
    * e.g if the proximity got smaller then the ambient light must also have gotten brighter
  * sequencing of observations from causal rules
    * if a causal rule states "A after B and C", then
      * observing A at time T entails (abductively) B and C at T-1
      * having observed B and C at T-1 entails (deductively) A at time T
    * e.g. the color becomes gray after the color was green
* Inferred observations (that are not also umwelt-affirmed observations) are marked as "inferred" to distinguish them
  * A new causal theory leads the CA to reconsider inferred observations across remembered states
  * A CA reconsidering its past is opaque to parent CAs
    * A CA never questions observations affirmed by its umwelt (even if umwelt CAs might reconsider their past beliefs)
      * Because the internal belief state of a CA is opaque to other CAs and is only revealed by
        * prediction errors
        * events signaling new belief predicates (names and domains) and new (latent) objects
          * the belief space of a CA never shrinks
* The CA applies the theory's causal rules to predict the next state so it can make predictions
  * from the execution of a policy selected to achieve an intent by the CA from the previous state
  * due to "spontaneous" changes in the umwelt
* The parent CAs constrain the vocabulary of a CA's causal theory, should the CA look for a better one
  * or else beliefs of parent CAs based on latent observations would become unfounded
  * the parent CAs can provide these constraints to the CA
* Up-down and down-up constraints
  * Umwelt CAs constrain the space of beliefs of their parent CAs
  * Parent CAs constrain the space of causal theories of the CAs in their umwelts

## Believing (detecting patterns of change in observations)

* A sensor CA has atomic beliefs obtained from Kalman-filtered measurements
* A CA gets its beliefs from detecting trends in the remembered observations of its umwelt
  * trends capture patterns of change over many states that are not be inferrable from the causal theory (state-to-state transitions)
    * e.g. the distance getting smaller is stable (trend about a trend)
* A CA has no belief until it detects trends
* Detecting trends
  * trends capture how observations most recently vary across more than two consecutive states
  * the subject of the trend (property or relation) must match the head of a causal rule of the CA's causal theory
    * i.e. obeservations on that subject are expected to vary across states
  * trends have values
    * up, down, stable, unstable, fluctuant
    * trends of trends can be stable, unstable and fluctuant
      * stable: e.g. the sub-trend is consistently up
      * unstable: e.g. the sub-trend is up then down then stable...
      * fluctuant: e.g. the sub-trend values alternate
  * trend values have durations
    * since time frame T and until and the trend value changes or the trend disappears
    * trends about the same subject (e.g. ambient brightness) do not overlap in time
    * trends are tolerant of "noise" - mostly up is up
  * values are assigned from
    * patterns of change in the number of objects related to a given object by a given relation (r(object, X) is the subject)
    * patterns of change in the value of a given object's property (p(object, V) is the subject)
    * property value domains are ordered if numerical
      * allowing for up/down trend values
* Synthesizing a belief from a detected trend
  * A synthetic property or relation names a detected trend
    * A synthetic relation between objects X and Y is a trend detected about an observed relation between X and Y
      * The observed relation (an umwelt CAs belief) may itself be a trending relation or an atomic relation
    * A synthetic property of object X is a trend detected about an observed property of X
      * The observed property (an umwelt CAs belief) may itself be a trending property or an atomic property
* Naming a synthetic belief
  * The CA assigns an agent-wide, forever unique predicate name to the belief
  * The CA remembers and hides the definition of the belief
* Exposing a synthetic belief
  * A CA exposes its beliefs (detected trends) so they are indistinguisable from atomic properties or relations
* Beliefs are (if transitively opened up) recursive structures (trends of trends ... of atomic properties/relations)
  * They are composed of simpler beliefs, down to atomic beliefs (held by sensor CAs)
  * This makes beliefs semantically transparent to an outside observer but obscure between CAs
* A CA's set of beliefs grows when
  * a new trend (whatever its value) is detected
    * changing the value of a trend does not add to the set of beliefs the CA makes available as observations to parent CAs
  * a CA's space of beliefs never shrinks
    * a trend never disappear
    * it can become boring/uninformative

## Acting (on beliefs)

* A CA seeks to validate a pleasant belief and invalidate an unplesant belief
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
      * all fitness levels are high and the CA is not involved in a parent's intent
    * or when necessary
      * the CA lacks beliefs (because the umwelt does not change) and thus affordances

## Attention

* A belief, being a trend, is associated with a fitness/pleasantness trend (better, worse, same, unclear)
  * A CA remembers the ambient fitness at each time frame
* A CA normally does not intend to validate/invalidate beliefs when fitness trends not far enough from neutral
* Higher-level beliefs are more likely to be associated with clear fitness trends than lower-level beliefs
  * This drives the SOM to add levels until clear fitness trends appear
  * Afforded control over fitness makes up fo resource consumption from adding levels to the SOM

## Regulating (to respond to fitness measures)

* The agent's fitness actors (fullness, integrity and engagement) each keep track of an agent fitness measure.
  * The agent starts life with highest fullness, highest integrity and lowest engagement (all initially broadcasted)
* When a measure changes, the fitness actor emits an event listened to by all CAs (and meta-CAs)
* In each time frame, a CA responds to the latest brodacasted measure while its high or low
* It responds to
  * low fullness by
    * getting rid of an ineffective policy (if any)
    * else getting rid of an irrelevant belief (if any)
    * else forgetting the oldest state (unless it supports held beliefs)
  * high fullness by
    * trying to upgrade the causal theory
  * low integrity by
    * ratcheting up pleasantness threshold for triggering intents (reduces likelihood to act)
  * high integrity or low engagement
    * ratcheting down pleasantness threshold for triggering intents (increases likelihood to act)
      * When threshold is 0, even neutral beliefs will be validated
  * high fullness and high integrity
    * babbling
