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
    api_url(Host, sensors, Url),
    http_get(Url, Response, []),
    devices_from_response(Response, sensor, Sensors).

get_actuators(Host, Actuators) :-
    api_url(Host, actuators, Url),
    http_get(Url, Response, []),
    devices_from_response(Response, actuator, Actuators).

api_url(Host, Query, Url) :-
    format(atom(Url), 'http://~w/api/~w', [Host, Query]).

devices_from_response(json([_=Json]), Tag, Devices) :-
    devices_from_json(Json, Tag, Devices).

devices_from_json([], _, []).
devices_from_json([JsonDevice | Others], Tag, [Device | OtherDevices]) :-
    device_from_json(JsonDevice, Tag, Device),
    devices_from_json(Others, Tag, OtherDevices).

device_from_json(json(KVPairs), Tag, Device) :-
    dict_create(Device, Tag, KVPairs).