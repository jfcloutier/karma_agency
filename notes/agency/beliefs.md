# Beliefs

> A Cognition Actor (CA) synthesizes beliefs for the current frame by abstracting past and current states.

A state is a set of observations of the CA's umwelt that are taken as synchronous within one time frame.

A belief is an inference a CA makes on the basis of its observations past and present.

The beliefs of a CA are observable by its parent CAs but not their derivation/construction,
i.e. observed beliefs are opaque to the observer as to how they were inferred.

A CA observes the beliefs of CAs in its umwelt and nothing else.

Sensor CAs have sensors as umwelts (one sensor per sensor CA). A sensor CA simply observes the value of its assigned sensor
and synthesizes a belief by expressing it trivially as an abduced property.

There are five kinds of beliefs: **abductions**, **counts**, **trends**, **endings** and **attempts**.

## Abduction

> Relation or property created in order to formulate a causal theory or to describe a sensor reading

Abduction is the only mechanism by which non-synthetic beliefs (not inferred from observed beliefs) are created.

* What
  * A relation or property
* Expressed as `Property(Type(Item), Value)` or `Relation(ObjectType(Object), ObjectType(Object))`, where
  * `Property` is a property name
  * `Relation` is a relation name
  * `ObjectType` is `agent`, `belief`, `policy` or `goal`
  * `Value` is a literal belonging to the property's domain
  * `Object` is the unique name of the agent or a policy, or an observable belief, or a goal,
    * e.g. `self`, `ahsd34ahsdjh`, `count(487cba2, 3)`, `goal(Belief, Impact)`
  * `Belief` is a belief as expressed for observation
  * `Impact` is either `persist` or `terminate`

## Count

> Property expressing number of relations of a given type and directionality an object has with other objects

e.g. self is next to 2 things

* What
  * The number of relations of a given type involving a given item
* Expressed as `count(ObjectType(Object), Integer)` where
  * `Object` is a unique name created from `relations_counted(RelationName, to/from, ObjectType(ObjectInFocus))`

## Trend

> Property expressing how an object is changing

e.g. luminance increases (trend)
e.g. the increase in luminance persists (trending trend)

* What
  * The property of a given type of a given item is stable, unstable, up or down, as observed over frames leading to the current frame
* Expressed as `trend(Object, Value)` where `Object` is a unique name created from `property_trending(PropertyName, ObjectType(Item))`

## Ending

> Property expressing how long before something observed in past states ends in the current state

e.g. the floor last stopped being green after 5 time frames

* What
  * The valued property of a given item
  * Or a given relation between two items
* Expressed as `ending(Object, Value)` where `Object` is a unique name created from
  * `property_ending(ObservedObject, Value)`
  * `relations_ending(RelationName, ObjectType(Object1), ObjectType(Object2))`

## Attempt

> A relationship expressing a policy (a list of sub-goals) executed to achieve a goal to persist/terminate a belief of the CA

e.g. attempted to stop the distance to a thing getting smaller

* What
  * A relationship between a policy and a goal
* Expressed as `attempted(Policy, Goal)` where
  * Policy is the name of a remembered policy executed by the CA
  * Goal is `goal(Belief, Impact)`, where impact is either `persist` or `terminate`

## About naming of items in beliefs

Names are generated in such as way as to be unique to the structure and contents of the belief, and determined by it such that if two CAs synthesize smeantically identical beliefs, they will be named identically without requiring concertation between CAs.
