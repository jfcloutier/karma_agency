/*
The two-way interface between the SOM and the robot's effectors and actuators.
*/

:- module(body, [get_sensors/2, get_actuators/2]).

:- use_module(actor_model(supervisor)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).


start(Parent) :-
    start_supervisor(Parent).
    
start_supervisor(Parent) :-
    supervisor:start_child(Parent, supervisor, body, [restart(transient)]).

get_sensors(Host, Sensors) :-
    format(atom(Url), 'http://~w/api/sensors', [Host]),
    http_get(Url, Response, []),
    sensors_from_response(Response, Sensors).

sensors_from_response(json([sensors=JsonSensors]), Sensors) :-
    sensors_from_json(JsonSensors, Sensors).

sensors_from_json([], []).
sensors_from_json([JsonSensor | Others], [Sensor | OtherSensors]) :-
    sensor_from_json(JsonSensor, Sensor),
    sensors_from_json(Others, OtherSensors).

sensor_from_json(json(KVPairs), Sensor) :-
    Sensor = sensor{}.put(KVPairs).

get_actuators(Host, Actuators) :-
    format(atom(Url), 'http://~w/api/actuators', [Host]),
    http_get(Url, Response, []),
    actuators_from_response(Response, Actuators).

actuators_from_response(json([actuators=JsonActuators]), Actuators) :-
    actuators_from_json(JsonActuators, Actuators).

actuators_from_json([], []).
actuators_from_json([JsonActuator | Others], [Actuator | OtherActuators]) :-
    actuator_from_json(JsonActuator, Actuator),
    actuators_from_json(Others, OtherActuators).

actuator_from_json(json(KVPairs), Actuator) :-
    Actuator = actuator{}.put(KVPairs).