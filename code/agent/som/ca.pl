/*
Cognition actor (CA)

A CA strives to become increasingly compentent at making sense of its umwelt and at altering its own beliefs.

A CA makes sense of its umwelt by predicting changes to it (i.e. predicting incoming observations)
and by deriving beliefs from past observations, beliefs that then become observations available to parent CAs (higher-level CAs that have the CA in their umwelts).

A CA is effective at impacting its umwelt if it has reliable policies it can execute to validate pleasant beliefs or invalidate unpleasant beliefs.

The more competent a CA is, the more likely it is that its parents CAs will be competent and survive,
and thus the more likely it is that the CA itself will survive since orphaned CAs are susceptible to being removed.
*/

:- module(ca, []).