# Beliefs

> A Cognition Actor (CA) synthesizes beliefs for the current time frame by abstracting observation from the current and past time frames.

The observations are of the beliefs of the CA's umwelt. They are taken as synchronous within each time frame.
A CA observes the beliefs of CAs in its umwelt and nothing else.

A belief is an inference a CA makes on the basis of its observations past and present.

The beliefs of a CA are observable by its parent CAs but not their derivation/construction,
i.e. observed beliefs are opaque to the observer as to how they were inferred.

Sensor CAs have sensors as umwelts (one sensor per sensor CA). A sensor CA simply observes the value of its assigned sensor
and synthesizes a belief by translating it into an abduced property.

There are five kinds of beliefs: **abductions**, **counts**, **trends**, **endings** and **attempts**.

## Abduction

> Relation or property created in order to formulate a causal theory or to describe a sensor reading

Abduction is the only mechanism by which non-synthetic beliefs (not derived from observed beliefs) are created.

* What
  * A relation or property
* Expressed as `Property(Type(Object), Value)` or `Relation(ObjectType(Object), ObjectType(Object))`, where
  * `Property` is a property name
  * `Relation` is a relation name
  * `ObjectType` is `agent`, `belief`, `policy` or `goal`
  * `Value` is a literal belonging to the property's domain (e.g. blue, up, true, 4, etc.)
  * `Object` is the unique name of the agent or of a policy, or it is an observed belief, or it is a goal,
    * e.g. `self`, `ahsd34ahsdjh`, `count(487cba2, 3)`, `goal(Belief, Impact)` - note that objects are not necessarily physical "things"
  * `Belief` is a belief as observed (i.e. opaque as to its derivation)
  * `Impact` is either `persist` or `terminate`

## Count

> Property expressing number of relations an object has with other objects, relations of a given type and directionality

e.g. this policy was attempted twice to achieve this goal

* What
  * The number of relations of a given type involving a given item
* Expressed as `count(belief(Object), Value)` where
  * `Object` is a unique name created from `relations_counted(RelationName, to/from, ObjectType(ObjectInFocus))`
  * `Value` is a positive, non-zero integer

## Trend

> Property expressing how an object is changing

e.g. luminance increases (trend)
e.g. the increase in luminance persists (trending trend)

* What
  * Property expressing how the values of a property of an object trend, as observed over frames leading to the current frame
* Expressed as `trend(belief(Object), Value)` where
  * `Object` is a unique name created from `property_trending(PropertyName, ObjectType(Object))`
  * `Value` is `stable`, `unstable`, `up` or `down`

## Ending

> Property expressing how long before something observed in past time frames ends in the current time frame

e.g. increase in luminance ended suddenly

* What
  * The valued property of a given object
  * Or a given relation between two objects
* Expressed as `ended(belief(Object), Value)` where
  * `Object` is a unique name created from
    * `property_ending(Object, Value)`
    * or `relation_ending(RelationName, ObjectType(Object), ObjectType(Object))`
  * `Value` is `suddenly` or `slowly`

## Attempt

> A relationship expressing a policy executed to achieve a goal

e.g. attempted to stop distance getting smaller

* What
  * A relationship between a policy and a goal
* Expressed as `attempted(policy(Object1), goal(Object2))` where
  * `Object1` is a unique name created from `sub_goals([Goal, ...])`
  * `Object2` is a unique name created from `intent(belief(Object), Value)` where
    * `Value` is `persist` or `terminate`

## About naming of objects in beliefs

Names of objects are generated in such as way as to be unique to the semantics of an object,
such that identical objects will have the same name across CAs.
