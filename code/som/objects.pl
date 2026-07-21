/*
Utilities for objects

An object is what an observation, experience, prediction or prediction error is about

object{type:Type, id:Id}
or
object{type:Type, id:Id, evidence: [ObservationId, ...]}

For sensor objects:

* Type is `sensor`
* Id is the sensor name

For synthetic objects:

  * Type is `synthetic`
  * Id is a function of the sorted observation IDs.
  * Evidence (optional) is the private set of Observations by the dCA from which a synthetic object was created by the dCA.

For goal objects:

  * Type is `goal`
  * Id is the goal's id which is a function of the goal's target and impact
  * Evidence is the listed Goal itself - this is needed to predict the status of a directive as yet unknown to an umwelt
*/

:- module(objects, [synthetic_object/2]).

:- use_module(agency(som/ca_support)).


% Note: Two objects of the same type about the same "thing" must have identical ids, irrespective of the CA that created the object.
synthetic_object(ObservationIds, Object) :-
    atomic_list_hash(ObservationIds, ObjectId),
    Object = object{type:synthetic, id:ObjectId, evidence:ObservationIds}.
