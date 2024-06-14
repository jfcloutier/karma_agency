/*
Cognition actor.

A CA strives to become competent at predicting its umwelt (merkwelt + wirkwelt) so it can be relevant to other CAs - and survive

* Observing (beliefs within umwelt)
    * A CA makes predictions about the beliefs of CAs in its umwelt
        * If the CA has no prior observations, it makes random, domain-bounded predictions about all beliefs in its umwelt
        * If the CA has no model but has prior observations, it predicts that the latest observations are unchanged.
        * If the CA has a causal model, it uses it to predict the next observations.
    * If a prediction is met with a prediction error, the prediction error becomes the latest observation, else the prediction does.
    * A CA receives as events all actions executed by the agent
        * and matches them to policies 
        * that then become observations in the current frame
    * A CA drops repeated states (state = observations in a time frame) - i.e. if nothing changes, remembered time stands still

* Modeling (causality between observations)
    * To be competent, a CA needs to make sense of its observations via causal models of
        * how its umwelt changes (or not) on its own
        * how its umwelt changes (or not) by executed policies
    * A CA requests a causal model from the Apperception Engine when
        * it has no model and has at least four remembered (thus distinct) states
        * it has a model and its predictive capabilities have fallen by half or more since acquired 
    * A CA integrates beliefs across its umwelt using its causal model
        * co-occurrences of observations from static rules
            * e.e.g if the proximity gets smaller then the ambient light is also getting briighter
        * sequencing of observations from causal rules
            * e.g. the color becomes gray after the color being green

* Believing (detecting patterns of change in observations)
    * A CA syntesizes beliefs by detecting trends in remembered states
        * e.g. the distance getting smaller is stable (trend about a trend)
    * Expressing trends
        * trends are properties about properties and relations observed over time
        * trends have values
            * up, down, stable, unstable, fluctuant
            * trends of trends can be stable, unstable and fluctuant
                * stable: e.g. the sub-trend is consistently up
                * unstable: e.g. the sub-trend is up then down then stable...
                * fluctuant: e.g. the sub-trend is up then down then up then down...
        * trend values have durations
            * since time frame T and until and the trend value changes or the trend disappears
        * values are assigned from
            * patterns of changes in the number of objects related to a given object
            * patterns of changes in the value of a given object's property
            * property value domains are ordered if numerical
                * allowing for up/down trend values
    * Beliefs are recursive structures (trends of trends)
        * They are composed of simpler beliefs, down to atomic beliefs (held by sensor CAs)
        * This makes beliefs semantically transparent
    * A CA's set of beliefs changes when
        * a new trend (whatever its value) is detected
        * a trend disappears when its subject (an abduced object, property or relation or trend) is no longer observed
        * changing the value of a trend does not add to the set of beliefs
            
* Acting (on beliefs)
  * A CA intends to validate a pleasant belief and invalidate an unplesant belief
    * By futhering or interrupting a belief (sub-intents are delegated to umwelt CAs)
    * Realizing and intent is recursive because a belief is a recursive structure of beliefs held by umwelt CAs
    * Lower-level CAs act (and expand energy they need to survive) for the greater good
  * A CA can not emit another intent until the last one is reported as executed or denied
  * A CA holding atomic beliefs needs to discover what actions affect each beliefs
    * It also gets feedback as to whether the execution of an intent delegated by a parent CA was successful or not
  * A CA will try alternative intents until the delegated intent is realized (or times up)
    * based on its causal model which might describe alternate causes for the same outcome
    * Or it guesses if it has no model yet (babbling)

* Caring (about beliefs)
    * A belief, being a trend, is associated with a fitness/pleasantness trend (better, worse, same, unclear)
    * A CA does not attempt to validate/invalidate beliefs under a deviation-from-neutral threshold
    * Higher-level beliefs are more likely to be associated with clear fitness trends than lower-level beliefs are
        * This drives the SOM to add levels until fitness trends appear
        * Afforded control over fitness makes up fo resource consumption from adding levels to the SOM

* Responding (to fitness risks)
    * Low fullness
        * Get rid of actions (least effective first)
        * Get rid of beliefs (least cared about first)
        * Forgetting states (oldest first)
    * Low integrity
        * Set a higher pleasantness threshold to have intentions (reduce likelihood to act)
    * Low engagement
        * Set a lower pleasantness threshold to have intentions (reduce likelihood to act)
        * Refresh causal model to discover previously missed causes to act
        * Even neutral beliefs will be validated (babbling)


*/

:- module(ca, []).