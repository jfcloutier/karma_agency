# Beliefs

> A Cognition Actor (CA) derives beliefs for the current time frame by abstracting observations from the current and past time frames.

A belief is an inference a CA makes on the basis of its observations past and present.
The beliefs are made observable to parent CAs (i.e. to the CAs in which umwelts they sit).
A CA observes the beliefs of CAs in its umwelt and nothing else.

Observations in a given time frame are considered synchronous, as are the beliefs derived in the time frame.

The beliefs of a CA are expressed for observation by obfuscating their derivation,
i.e. observed beliefs are opaque to the observer as to how they were inferred.

The "bottom" beliefs (not derived from other beliefs) are those of the sensor CAs.
Sensor CAs sit at the bottom of the hierarchy of CAs and have sensors as their umwelts (one sensor per sensor CA).
A sensor CA simply observes the value of its attached sensor and translates it into a belief.

There are five kinds of beliefs: **abduction**, **count**, **trend**, **end** and **attempt**.

Each belief is expressed as a `property` (linking an object and a value) or as a `relation` (linking two objects).

A property is expressed as `Property(ObjectType(Object), Value)` where

* `Property` is a property name
* `ObjectType` is `agent`, , `policy`, `goal`, `counted`, `trending`, or `ending`
* `Value` is a literal belonging to the property's domain (e.g. blue, up, true, 4, slowly, etc.)
* `Object` is the unique name of an agent, policy, or goal, or of what's trending, ending or being counted,
  * e.g. `self`, `ahsd34ahsdjh`
  * the identity (derivation) of the named object is known only to the CA that named it
  * note that objects are not necessarily physical "things" (only an agent is)

A relation is expressed as  `Relation(ObjectType(Object), ObjectType(Object))`, where

* `Relation` is a relation name
* others as above

## Abduction

> The translation of a sensor reading, or something unobserved needed to formulate a causal theory.

Abduction **conjures** up properties or relations to either translate a sensor reading or to posit occulted observations needed to causally explain direct observations.

* What
  * A relation or property
* Observed as-is (i.e. unobfuscated)

## Count

> How many relations of a given type and directionality an object has with other objects.

e.g. this policy was attempted twice to achieve this goal

* What
  * A property
* Observed as `count(counted(Object), Value)` where
  * `Object` is a unique name created from `relations(RelationName, Direction, ObjectType(ObjectInRelation))`
  * `Direction` is `to` or `from`
  * `Value` is a positive, non-zero integer

## Trend

> How the values of a property of an object are trending, as observed over time frames leading to the current frame.

e.g. luminance increases (trend)
e.g. the increase in luminance persists (trending trend)

* What
  * A property
* Observed as `trend(trending(Object), Value)` where
  * `Object` is a unique name created from `properties(PropertyName, ObjectType(Object))`
  * `Value` is `stable`, `unstable`, `up` or `down`

## End

> How long before something observed in past time frames ends in the current time frame.

e.g. increase in luminance ended suddenly

* What
  * A property
* Observed as `end(ending(Object), Value)` where
  * `Object` is a unique name created from
    * `property_ending(Object, Value)`
    * or `relation_ending(RelationName, ObjectType(Object), ObjectType(Object))`
  * `Value` is `suddenly` or `slowly`

## Attempt

> The **fact** that the CA executed a policy to achieve a goal.

e.g. attempted to stop distance getting smaller

* What
  * A relation
* Observed as `attempted(policy(Object1), goal(Object2))` where
  * `Object1` is a unique name created from `sub_goals([Goal, ...])`
  * `Object2` is a unique name created from `intent(belief(Object), Value)` where
    * `Value` is `persist` or `terminate`

## About naming of objects in beliefs

Names of objects are generated in such as way as to be unique to the semantics of an object.
Objects with the same structure and content will have the same name across CAs.
