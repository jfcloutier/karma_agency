/*
Cognition actor.

A CA strives to become competent at predicting its umwelt (merkwelt + wirkwelt) so it can be relevant to other CAs - and survive

* Observing
    * A CA makes predictions about the beliefs of CAs in its umwelt
        * If the CA has no prior observations, it makes random, domain-bounded predictions about all beliefs in its umwelt
        * If the CA has no model but has prior observations, it predicts that the latest observations are unchanged.
        * If the CA has a causal model, it uses it to predict the next observations.
    * If a prediction is met with a prediction error, the prediction error becomes the latest observation, else the prediction does.
    * A CA receives as events all actions executed by the agent
        * and matches them to policies 
        * that then become observations in the current frame
    * A CA drops repeated states (state = observations in a time frame) - i.e. if nothing changes, remembered time stands still

* Modeling
    * To be competent, a CA needs to make sense of its observations via causal models of
        * how its umwelt changes (or not) on its own
        * how its umwelt changes (or not) by actions taken
    * A CA requests a causal model from the Apperception Engine when
        * it has no model and has at least four remembered (thus distinct) states
        * it has a model and its predictive capabilities have fallen by half or more since acquired 

* Acting
  * A CA intends to validate a pleasant belief and invalidate an unplesant belief
  * Futhering or interrupting a belief requires a plan of actions to be taken by umwelt CAs
    * A plan is recursive Because a belief is a recursive structure of beliefs held by umwelt CAs
  * A CA can not execute another plan until it is reported as executed or denied
  * A CA holding atomic beliefs needs to discover what actions affect each beliefs
    * It also gets feedback as to whether the execution of a plan it was part of was successful or not

* Believing
    * A CA's empirical beliefs are its latests observations
        * they are the beliefs held by its umwelt
    * A CA adds a set of synthetic beliefs by detecting trends, co-occurences and mutual exclusions
        * observed trends across remembered states (the distance getting smaller is stable)
        * observed co-occurrences (the distance getting smaller while the ambient light getting briighter)
        * observed mutual exclusions (the distance never being stable when action A is executed)
    * Observations by a CA are beliefs of umwelt CAs - beliefs based on observations are recursive belief structures
        * a belief can be a stable trend of mutual exclusion between two trends etc.
        * this makes beliefs semantically transparent (they are decomposable into simpler beliefs)
    * Expressing trends
        * trends are properties about properties and relations observed over time
        * trends have values
            * stable, up, down, chaotic, gone
        * trends have durations
            * since time frame T and until and the trend value changes
        * values are assigned from
            * patterns of changes in the number of objects related to a given object
            * patterns of changes in the value of a given object's property
            * property value domains are ordered if numerical
                * allowing for up/down trend values
    * A CA's set of beliefs changes when
        * a new trend or co-occurrence or mutual exclusion is detected
        * a trend is gone because its subject (an abduced object, property or relation) is no longer believed to exist
        * changing the value of a trend does not add to the set of beliefs
            * it changes an existing belief

* Caring
    * A belief is associated with an associated pleasantness trend (better, worse, same)
    * A CA does not attempt to validate/invalidate neutral beliefs

* Fitness responses
    * Low fullness
        * Get rid of actions (least effective first)
        * Get rid of beliefs (least cared about first)
        * Forgetting states (oldest first)
    * Low integrity
        * Fewer intents (reduce likelihood to emit intents)
    * Low engagement
        * More intents (increase likelihood to emit intents)


*/

:- module(ca, []).