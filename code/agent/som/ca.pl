/*
Cognition actor.

To survive, a CA strives to become competent so it can be relevant.

* A CA makes predictions about the beliefs of CAs in its umwelt
  * If the CA has no beliefs yet and no prior observations, it makes random predictions, within relevant value domains.
  * If the CA has no beliefs yet but has prior observations, it predicts the latest observations.
  * If the CA has beliefs, it uses its causal model to abduce umwelt beliefs and predict those.

* If a prediction is met with a prediction error, the prediction error becomes the latest observation, else the prediction does.

* To be competent, a CA needs to make sense of
  * How its umwelt changes (or not) without actions taken
  * How its umwelt changes (or not) with actions taken

* A CA babbles to accumulate observed actions and their possible consequences.

* Once a CA has accumulated enough states (per frame sets of observations)
*/

:- module(ca, []).