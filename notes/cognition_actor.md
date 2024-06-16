# Cognition actor

A CA strives to become competent at predicting its umwelt (merkwelt + wirkwelt) so it can be relevant to other CAs - and survive

## Observing (its umwelt's beliefs)

* A CA predicts the beliefs of CAs in its umwelt
  * If the CA has no prior observations, it makes random, domain-bounded predictions about all beliefs in its umwelt
  * If the CA has no model but has prior observations, it predicts that the latest observations are unchanged.
  * If the CA has a causal model, it uses it to predict the next observations.
* A CA predicts the success of its policies
* If a prediction is met with a prediction error, the prediction error becomes the latest observation, else the prediction does.
* A CA drops repeated states (state = observations in a time frame) - i.e. if nothing changes, remembered time stands still

## Modeling (causality between observations)

* To be competent, a CA needs to make sense of its observations via causal models of
  * how its umwelt changes (or not) on its own
  * how its umwelt changes (or not) by executed policies
* A CA requests a causal model from the Apperception Engine when
  * it has no model and a model can be generated from its remembered states
  * it has a model and its predictive capabilities have fallen by half or more since acquired
* A CA integrates beliefs across its umwelt using its causal model
  * co-occurrences of observations from static rules
    * e.g if the proximity got smaller then the ambient light must also have gotten brighter
  * sequencing of observations from causal rules
    * e.g. the color becomes gray after the color being green
* The CA gets a model's trace
  * to "observe" latent properties/relations given the current state
    * latent properties/relations
  * to predict
    * the consequences of the execution of a policy selected to achieve an intent by the CA
    * "spontaneous" changes in the umwelt

## Believing (detecting patterns of change in observations)

* A sensor CA has atomic beliefs obtained from measurements
* A CA gets its beliefs from observing its umwelt and doing
  * abduction
    * via its causal model abducting properties, relations and/or objects
    * the causal model anticipates with varying accuracy state-to-state changes
    * it introduces beliefs about latent reality posited from making sense of observing the umwelt
    * some latent reality might only become apparent at a sufficient level of abstraction
  * induction
    * by detecting trends in remembered states
      * trends capture patterns of change over mutliple states that may not be inferrable from the causal model
      * e.g. the distance getting smaller is stable (trend about a trend)
* Detecting trends
  * trends are properties about observations changing (or not) over time
  * trends have values
    * up, down, stable, unstable, fluctuant
    * trends of trends can be stable, unstable and fluctuant
      * stable: e.g. the sub-trend is consistently up
      * unstable: e.g. the sub-trend is up then down then stable...
      * fluctuant: e.g. the sub-trend is up then down then up then down...
  * trend values have durations
    * since time frame T and until and the trend value changes or the trend disappears
  * values are assigned from
    * patterns of change in the number of objects related to a given object
    * patterns of change in the value of a given object's property
    * property value domains are ordered if numerical
      * allowing for up/down trend values
* Synthesizing a belief from a detected trend
  * A property or relation defined by a detected trend
    * A synthetic relation between objects X and Y is a trend detected about an observed relation between X and Y
      * The observed relation (an umwelt CAs belief) may itself be
        * defined by an umwelt CA as a trending relation between X and Y
        * or an atomic relation between X and Y
    * A synthetic property of object X is a trend detected about an observed property of X
      * The observed property (an umwelt CAs belief) may itself be
        * defined by an umwelt CA as a trending property of X
        * or an atomic property of X
* Synthesizing a belief from posited latent reality
  * If an umwelt CA believes in the existence of a latent object/property/relation, then the CA must include it in its causal model
* Naming a synthetic belief
  * The CA assigns an agent-wide, forever unique predicate name to the belief
  * The CA remembers and hides the definition of the belief
    * as a detected trend (thick now)
    * as a latent object/property/relation
* Exposing a synthetic belief
  * A CA exposes its beliefs as objects/relations/properties but hides their definition/origin
  * To other CAs a synthetic belief looks the same as an atomic belief
* Beliefs are (if transitively opened up) recursive structures (trends of trends ... of atomic or abduced beliefs)
  * They are composed of simpler beliefs, down to atomic beliefs (held by sensor CAs)
  * This makes beliefs semantically transparent to an outside observer but obscure between CAs
* A CA's set of beliefs changes when
  * a new trend (whatever its value) is detected
  * a trend disappears when
    * its subject (an abduced object, property or relation or trend) is no longer observed
  * changing the value of a trend does not add to the set of beliefs

## Acting (on beliefs)

* A CA intends to validate a pleasant belief and invalidate an unplesant belief
* By furthering or interrupting (impacting) sub-beliefs via intents delegated to umwelt CAs
* Realizing and intent is recursive because a belief is a recursive structure of beliefs held by umwelt CAs
* An intent contains
  * a goal - the belief to validate or invalidate
  * a policy
  * the initiating CA
  * a deadline (the end of the CA's time frame)
* Policy selection
  * What intents a CA emits to its umwelt depends on the CA's goal (further or disrupt a belief it holds)
  * There might be alternative ways to achieve a goal (impacting a held belief) depending on the definition ofthe belief
  * Each alternative set of intents on the umwelt that could achieve a goal is a policy
    * If the belief is the effect of one or more causal rules
      * determining which rule(s) apply to the current state - i.e. which, if any, were operative
      * for each opertive rule
        * impacting the conjunction of cause beliefs
          * furthering all if the goal is to further the effected belief
          * disrupting any if the goal is to disrupting the effected belief
    * If the belief is a detected trend
      * direct the umwelt to impact the trend based on the value of the trend and the goal
        * stable
          * disrupt -> intend to disrupt the sub-belief
          * further -> intend to further the sub-belief
        * unstable
          * disrupt -> intend to further the sub-belief
          * further -> intend to disrupt the sub-belief
        * fluctuant
          * disrupt -> intend to further the sub-belief (stop fluctuation)
          * further -> intend to disrupt the sub-belief (keep fluctuating)
        * up
          * disrupt -> intend to down the sub-belief
          * further -> intend to up the sub-belief
        * down
          * disrupt -> intend to up the sub-belief
          * further -> intend to down the sub-belief
* A CA will try alternative intents until the received intent is realized (or times up)
  * based on its causal model which might describe alternate causes for the same outcome
  * Or it guesses if it has no model yet (babbling)
* One policy execution at a time
  * A CA can not execute its own policy if it is executing a higher CA's policy
    * Lower-level CAs relinquish some autonomy for the greater good
* Learning affordances
  * A CA gets feedback as to whether the delegated intents from its policy were realized
  * A CA detects remembers how successul a policy was at achieving a goal
    * A CA selects the more successful policies more often but not always
  * A CA holding atomic beliefs needs to discover what actions affect its beliefs

## Caring (about beliefs)

* A belief, being a trend, is associated with a fitness/pleasantness trend (better, worse, same, unclear)
* A CA does not attempt to validate/invalidate beliefs under a deviation-from-neutral threshold
* Higher-level beliefs are more likely to be associated with clear fitness trends than lower-level beliefs are
  * This drives the SOM to add levels until clear fitness trends appear
  * Afforded control over fitness makes up fo resource consumption from adding levels to the SOM

## Regulating (to respond to fitness risks)

* Low fullness
  * Get rid of remembered policies (least effective first)
  * Get rid of beliefs (least cared about first)
  * Forgetting states (oldest first)
* Low integrity
  * Set a higher pleasantness threshold triggering intentions (reduce likelihood to act)
* Low engagement
  * Set a lower pleasantness threshold triggering intentions (increase likelihood to act)
  * Refresh causal model to discover previously missed causes to act
  * When threshold is 0, even neutral beliefs will be validated (babbling)
