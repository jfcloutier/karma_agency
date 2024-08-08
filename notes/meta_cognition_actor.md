# Meta-cognition actors

* An agent is animated by a Society of Mind (SOM) composed of Cognition Actors (CAs) organized in a hierarchy and managed by MetaCognition Actors (meta-CAs).
* Metacognition actors drive the search for a  SOM configuration that keeps itself alive.
* A meta-cognition actor (meta-CA) adds and removes CAs at the hierarchical level of the SOM it manages (the level is made up of its "ward" CAs).
* A hierachy level L of the SOM is said to be complete if the umwelts of the level L CAs cumulatively contain all the CAs at level L - 1  (level L covers level L-1)
* Sensor and effector CAs form level 0 of the SOM, which is a prior and unchangeable level. Level 0 has no meta-CA.
* There is one meta-CA per level L > 0 of the SOM.
* Through the actions of meta-CAs, the SOM expands from the bottom (as extant CAs are found to be competent) and shrinks from the top (as top CAs are found to be incompetent).

## Lifecycle

* A meta-CA potentially takes action on its ward CAs once per time frame. The higher the level of the meta-CA, the longer its time frames.
* Whenever level L becomes complete, the level L meta-CA creates an L+1 meta-CA if none already exists
* Whenever level L is empty and level L-1 is incomplete, the level L meta-CA, if it exists, removes itself from the SOM

## Actions

* At the end of each of its time frame (many times the length of the time frames of its wards), a meta-CA at level L
  * attempts to add one "ward" CA" to its level, giving it an umwelt made of L-1 CAs
  * attempts to remove one incompetent ward CA from its level
  * may advise ward CAs at risk of removal to update their causal models (i.e. get their act together)
* A meta-CA at level L attempts to add a ward CA but only if level L is incomplete and level L - 1 is complete
  * When adding a CA, it selects a random subset of L-1 CAs to create a permament umwelt for the new CA, such that
    * the umwelt will contain at two or more CAs from level L - 1
    * the new umwelt increases the coverage of L-1 CAs (the number of them in other CAs' umwelts)
      * ward CAs are allowed to overlap in their umwelts
    * all effector CAs are transitively in the new CA's umwelt (the new CA is not restricted in the actions it can intend)
    * each umwelt CA is already found to be competent (incompetent CAs are of no use) or it is a sensor/effector CAs (they are inherently competent)
* A meta-CA at level L removes a ward CA if
  * it is not in the umwelt of a (higher-level) CA
  * it is demonstrably **incompetent** over the latest N frames (N = a preset grace period) because
    * its predictions were no better than random guesses
    * the effectiveness of its executed policies were no better than random
  * there are no other candidate CAs for removal that are more expensive
    * the expensiveness of a CA is computed in terms of
      * the complexity of the CA's causal theory
      * the memory load (how many observations and beliefs are in remembered frames)
* A meta-CA advises a ward CA to get a new causal theory if the latter was a candidate for removal but was not removed
  * the search for a new causal theory by the ward CA is constrained to have the same signature so as not to render obsolete the ward CA's current abductive beliefs

## Wellbeing

* When a critically low fullness signal is received, a meta-CA responds by
  * removing its most expensive **superfluous** ward CA (cutting the fat)
    * a level L CA is superfluous if none of its beliefs are used in the synthesis of beliefs held by a level L + 1 CA
  * refraining from adding a new CA (keeping down expenses)
