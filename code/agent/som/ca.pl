/*
Cognition actor.

A CA strives to become competent at predicting its umwelt so it can be relevant to other CAs

* Observing
    * A CA makes predictions about the beliefs of CAs in its umwelt
        * If the CA has no prior observations, it makes random, domain-bounded predictions about all beliefs in its umwelt
        * If the CA has no model but has prior observations, it predicts the latest observations are unchanged.
        * If the CA has a causal model, it uses it to predict the next observations.
    * If a prediction is met with a prediction error, the prediction error becomes the latest observation, else the prediction does.
    * A CA receives as events of all actions executed by the agent, and matches them to policies that then become observations in the current frame
    * A CA drops repeated states (state = observations in a time frame) - i.e. if nothing happens, remembered time stands still

* Modeling
    * To be competent, a CA needs to make sense via causal models of
        * How its umwelt changes (or not) on its own
        * How its umwelt changes (or not) with actions taken
    * A CA requests a causal model from the Apperception Engine when
        * it has no model and has at least four states
        * it has a model and its predictive capabilities have fallen by half since acquired
        * a policy modified was executed since the model was 

* Acting
  * A CA builds and curates a set of policies
    * A policy is a sequence of actions
  * A CA discovers what policies effect what belief changes
        * By correlating from observations
  * A CA intends policies so as to validate pleasant beliefs and invalidate unplesant beliefs
    * An intent associates a policy with a sought belief change
    * The policy selected to effect a belief change is based on
      * evidence of effectiveness (the greater the evidence the higher the odds of selection)
  * A CA can not act between intending a policy and it being reported as executed or denied
  * A CA can not intend a policy subsumed by another (e.g. [action1] is subsumed by [action1, action2])
  * A CA starts with policies that are copies of all the policies in its umwelt
  * A CA synthesizes a new policy
    * how
        * by matching sequences of observed policies across frames with a consequent belief change (discovery)
        * by randomly joining policies (subsumed or not) into an unsubsume policy (babbling)
        * by randomly stripping a policy into an unsubsumed policy (babbling)
    * when
        * the number of policies < 2 * number of beliefs
  * A CA drops a synthesized policy if 
    * it is persistently ineffective (rarely achieves its intended goals)

* Believing

*/

:- module(ca, []).